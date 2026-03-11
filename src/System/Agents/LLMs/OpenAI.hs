{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Agents.LLMs.OpenAI (
  Tool(..),Trace(..),ToolCall(..),ToolCallFunction(..),ToolName(..), Model(..), ModelFlavor(..)
 , ApiKey(..)
 , parseFlavor
 , ApiBaseUrl(..)
 , SystemPrompt(..)
 , Response(..)
 , callLLMPayload
 , calculatePayloadBytes
 , parseLLMResponse
 , waitRateLimit
 , parseRateLimitDelay
 , ApiLimits(..)
 , WaitAction(..)
) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, ToJSON, Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LByteString
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Read as Text
import qualified Network.HTTP.Client as NetHttpClient
import Prod.Tracer (Tracer (..), contramap, runTracer)
import Text.Read (readMaybe)

import qualified System.Agents.HttpClient as HttpClient
import System.Agents.ToolSchema

-------------------------------------------------------------------------------
-- Trace with byte counts
-------------------------------------------------------------------------------

-- | Trace events for LLM calls, now including byte counts for tracking.
--
-- The constructors include byte counts for request/response payloads
-- to support cost transparency and debugging.
data Trace
    = CallChatCompletion !Aeson.Value !Int
      -- ^ Payload being sent to the LLM + byte count
    | GotChatCompletion !Aeson.Value !Int
      -- ^ Response received from the LLM + byte count
    | HttpClientTrace !HttpClient.Trace
      -- ^ Trace from the underlying HTTP client
    deriving (Show)

-- | Calculate byte size of a JSON Value for tracking purposes.
--
-- This encodes the value to JSON and returns the length of the
-- resulting ByteString. Useful for tracking payload sizes.
calculatePayloadBytes :: Aeson.Value -> Int
calculatePayloadBytes = fromIntegral . LByteString.length . Aeson.encode

-------------------------------------------------------------------------------
newtype ApiBaseUrl
    = ApiBaseUrl {getBaseUrl :: Text}
    deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

type Remaining = Int
type Reset = Text

extractRequestRateLimitFromResponse :: NetHttpClient.Response body -> (Maybe Remaining, Maybe Reset)
extractRequestRateLimitFromResponse rsp =
    let
        remaining = Prelude.lookup "x-ratelimit-remaining-requests" (NetHttpClient.responseHeaders rsp) >>= readMaybe . C8.unpack :: Maybe Int
        resetStr = Prelude.lookup "x-ratelimit-reset-requests" (NetHttpClient.responseHeaders rsp) >>= pure . Text.decodeUtf8
     in
        (remaining, resetStr)

extractTokenRateLimitFromResponse :: NetHttpClient.Response body -> (Maybe Remaining, Maybe Reset)
extractTokenRateLimitFromResponse rsp =
    let
        remaining = Prelude.lookup "x-ratelimit-remaining-tokens" (NetHttpClient.responseHeaders rsp) >>= readMaybe . C8.unpack :: Maybe Int
        resetStr = Prelude.lookup "x-ratelimit-reset-tokens" (NetHttpClient.responseHeaders rsp) >>= pure . Text.decodeUtf8
     in
        (remaining, resetStr)

data ApiLimits = ApiLimits
    { requestCushion :: Remaining
    , tokenCushion :: Remaining
    }

data WaitAction
    = WaitOnTokens Int
    | WaitOnRequests Int
    deriving (Show, Ord, Eq)

evalWaitAction :: ApiLimits -> NetHttpClient.Response a -> Maybe WaitAction
evalWaitAction lims rsp =
    let
        (r0, rst0) = extractRequestRateLimitFromResponse rsp
        (r1, rst1) = extractTokenRateLimitFromResponse rsp
        p0 = maybe False (< lims.requestCushion) r0
        p1 = maybe False (< lims.tokenCushion) r1
        delay0 = maybe (-1) parseRateLimitDelay rst0
        delay1 = maybe (-1) parseRateLimitDelay rst1
     in
        doWait p0 delay0 p1 delay1
  where
    doWait True d0 True d1
        | d1 > d0 = Just $ WaitOnTokens d1
        | otherwise = Just $ WaitOnRequests d0
    doWait True d0 False _ = Just $ WaitOnRequests d0
    doWait False _ True d1 = Just $ WaitOnTokens d1
    doWait False _ False _ = Nothing

{- | Parses the ratelimit reset header syntaxes.
https://community.openai.com/t/what-is-new-field-in-rate-limits-x-ratelimit-reset-tokens-usage-based/541210
I've not found a grammar for the format so I made up examples found, data seen, and hallucinated.
-}
parseRateLimitDelay :: Text -> Int
parseRateLimitDelay t0 =
    let
        (d, t1) = getDays t0
        (h, t2) = getHours t1
        (m, t3) = getMin t2
        (ms, _) = getMsec t3
        (s, _) = getSec t3
        secs = if ms > 0 then 1 else s
     in
        secs + 60 * (m + 60 * (h + 24 * d))
  where
    getDays, getHours, getMin, getSec :: Text -> (Int, Text)
    getDays = breakInt "d"
    getHours = breakInt "h"
    -- special handling of `ms` confusing minutes
    getMin t =
        let (val, rest) = breakInt "m" t
         in if "s" `Text.isPrefixOf` rest then (0, t) else (val, rest)
    getSec t =
        let (val, rest) = breakFloat "s" t
         in (ceiling val, rest)
    getMsec = breakInt "ms"

    -- special handling of sub-seconds: we'll wait for a whole second anyway,
    -- so we round-up the value during parsing
    breakInt :: Text -> Text -> (Int, Text)
    breakInt char txt =
        let (val, rest) = Text.breakOn char txt
         in if rest == ""
                then (0, txt)
                else (readDecimalOrZero val, Text.drop (Text.length char) rest)

    breakFloat :: Text -> Text -> (Float, Text)
    breakFloat char txt =
        let (val, rest) = Text.breakOn char txt
         in if rest == ""
                then (0, txt)
                else (readFloatOrZero val, Text.drop (Text.length char) rest)

    readDecimalOrZero :: Text -> Int
    readDecimalOrZero txt =
        case Text.decimal txt of
            Right (v, _) -> v
            Left _ -> 0

    readFloatOrZero :: Text -> Float
    readFloatOrZero txt =
        case Text.rational txt of
            Right (v, _) -> v
            Left _ -> 0

waitRateLimit :: ApiLimits -> (WaitAction -> IO ()) -> Tracer IO Trace
waitRateLimit lims onWait = Tracer go
  where
    go :: Trace -> IO ()
    go (HttpClientTrace (HttpClient.RunRequest _ rsp)) = do
        case evalWaitAction lims rsp of
            Nothing -> pure ()
            (Just w@(WaitOnTokens n)) -> do
                onWait w
                threadDelay (1000000 * n)
            (Just w@(WaitOnRequests n)) -> do
                onWait w
                threadDelay (1000000 * n)
    go _ = pure ()

-------------------------------------------------------------------------------
newtype ToolName = ToolName {getToolName :: Text}
    deriving (Show, Eq, Ord)

data Tool = Tool
    { toolName :: ToolName
    , toolDescription :: Text
    , toolParamProperties :: [ParamProperty]
    }
    deriving (Show)

instance ToJSON Tool where
    toJSON t =
        Aeson.object
            [ "type" .= ("function" :: Text)
            , "function"
                .= Aeson.object
                    [ "name" .= t.toolName.getToolName
                    , "description" .= t.toolDescription
                    , "parameters" .= Aeson.object (jsonSchema toplevelProperty)
                    ]
            ]
      where
        toplevelProperty :: ParamProperty
        toplevelProperty =
            ParamProperty
                { propertyKey = t.toolName.getToolName
                , propertyType = ObjectParamType t.toolParamProperties
                , propertyDescription = t.toolDescription
                , propertyRequired = True
                }

data ToolResponse
    = ToolNotFound
    | TextToolResponse (NonEmpty Text)
    | ToolFailure Text
    deriving (Show, Eq, Ord)

newtype SystemPrompt = SystemPrompt {getSystemPrompt :: Text}
    deriving (Show, Eq, Ord, IsString)

newtype ApiKey = ApiKey {revealApiKey :: ByteString}

data ModelFlavor
    = OpenAIv1
    | MistralV1
    | KimiV1
    | ClaudeV1
    deriving (Show, Eq, Ord)

data Model = Model
    { modelFlavor :: ModelFlavor
    , modelBaseUrl :: ApiBaseUrl
    , modelName :: Text
    , modelSystemPrompt :: SystemPrompt
    }
    deriving (Show, Eq, Ord)

parseFlavor :: Text -> Maybe ModelFlavor
parseFlavor "kimiv1" = Just KimiV1
parseFlavor "KimiV1" = Just KimiV1
parseFlavor "OpenAIv1" = Just OpenAIv1
parseFlavor "MistralV1" = Just MistralV1
parseFlavor "openai-v1" = Just OpenAIv1
parseFlavor "mistral-v1" = Just MistralV1
parseFlavor "claude-v1" = Just ClaudeV1
parseFlavor "ClaudeV1" = Just ClaudeV1
parseFlavor _ = Nothing


callLLMPayload ::
    (ToJSON payload) =>
    Tracer IO Trace ->
    HttpClient.Runtime ->
    ApiBaseUrl ->
    payload ->
    IO (Either String Value)
callLLMPayload tracer rt (ApiBaseUrl baseUrl) payload = do
    let payloadVal = Aeson.toJSON payload
    let requestBytes = calculatePayloadBytes payloadVal
    runTracer tracer (CallChatCompletion payloadVal requestBytes)
    httpRsp <- rt.post (contramap HttpClientTrace tracer) (baseUrl <> "/chat/completions") (Just payloadVal)
    case httpRsp of
        Left (HttpClient.SomeError err) -> do
            pure $ Left err
        Right rsp -> do
            case HttpClient.decodeBody (HttpClient.J rsp) of
                Nothing -> pure $ Left "json decode body error"
                Just body -> do
                    let responseBytes = calculatePayloadBytes body
                    runTracer tracer (GotChatCompletion body responseBytes)
                    pure $ Right body

data ToolCallFunction
    = ToolCallFunction
    { toolCallFunctionName :: ToolName
    , toolCallFunctionArgsUnparsed :: Text
    , toolCallFunctionArgs :: Maybe Aeson.Value
    }
    deriving (Show)

instance FromJSON ToolCallFunction where
    parseJSON =
        Aeson.withObject "ToolCallFunction" $ \v -> do
            argsStr <- v .: "arguments"
            let argVal = Aeson.decode' (LByteString.fromStrict $ Text.encodeUtf8 argsStr) :: Maybe Aeson.Value
            ToolCallFunction
                <$> (ToolName <$> v .: "name")
                <*> pure argsStr
                <*> pure argVal

data ToolCall
    = ToolCall
    { rawToolCall :: Aeson.Object
    , toolCallId :: Text
    , toolCallType :: Maybe Text
    , toolCallFunction :: ToolCallFunction
    }
    deriving (Show)

instance FromJSON ToolCall where
    parseJSON =
        Aeson.withObject "ToolCall" $ \v ->
            ToolCall
                <$> pure v
                <*> v .: "id"
                <*> v .:? "type"
                <*> v .: "function"

data Response
    = Response
    { rawResponse :: Aeson.Object
    , chosenMessage :: Aeson.Object
    , finishReason :: Maybe Text
    , rspContent :: Maybe Text
    , rspToolCalls :: Maybe [ToolCall]
    , rspReasoningContent :: Maybe Text
    }
    deriving (Show)

instance FromJSON Response where
    parseJSON =
        Aeson.withObject "Response" $ \v -> do
            choices <- v .: "choices" :: Aeson.Parser (NonEmpty Aeson.Object)
            firstChoice <- (NonEmpty.head choices) .: "message"
            Response
                <$> pure v
                <*> pure firstChoice
                <*> firstChoice .:? "finish_reason"
                <*> firstChoice .: "content"
                <*> firstChoice .:? "tool_calls"
                <*> firstChoice .:? "reasoning_content"

parseLLMResponse :: Value -> Aeson.Parser Response
parseLLMResponse v = Aeson.parseJSON v

