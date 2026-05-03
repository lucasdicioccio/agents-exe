{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.LLMs.OpenAI (
    Tool (..),
    Trace (..),
    OpenAIToolCall (..),
    ToolCallFunction (..),
    ToolName (..),
    Model (..),
    ModelFlavor (..),
    ApiKey (..),
    parseFlavor,
    ApiBaseUrl (..),
    SystemPrompt (..),
    Response (..),
    TokenUsage (..),
    callLLMPayload,
    calculatePayloadBytes,
    parseLLMResponse,
    waitRateLimit,
    parseRateLimitDelay,
    ApiLimits (..),
    WaitAction (..),
) where
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, ToJSON, Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
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
import qualified Network.HTTP.Types.Status as HttpStatus
import Prod.Tracer (Tracer (..), contramap, runTracer)
import Text.Read (readMaybe)

import qualified System.Agents.HttpClient as HttpClient
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..), jsonSchema)
import System.Agents.Tools.ParamTier (defaultParamTier)
-------------------------------------------------------------------------------

{- | Token usage breakdown from LLM API response.

Different providers use different field names and structures.
This type captures the common fields across providers while
preserving the raw usage object for provider-specific fields.

Supported providers:
- OpenAI: prompt_tokens, completion_tokens, total_tokens, cached_tokens in prompt_tokens_details
- Kimi/Moonshot: Same as OpenAI, cached_tokens at top level
- Claude: input_tokens, output_tokens, thinking_tokens for extended thinking
- Mistral: Same as OpenAI
-}
data TokenUsage = TokenUsage
    { tokenPromptTokens :: Int
    -- ^ Tokens in the prompt/context (input)
    , tokenCompletionTokens :: Int
    -- ^ Tokens in the completion response (output)
    , tokenTotalTokens :: Int
    -- ^ Total tokens used
    , tokenCachedTokens :: Maybe Int
    -- ^ Cached tokens (Kimi/OpenAI context caching)
    , tokenThinkingTokens :: Maybe Int
    -- ^ Reasoning/thinking tokens (Claude extended thinking)
    , tokenRawUsage :: Aeson.Object
    -- ^ Raw usage object for provider-specific fields
    }
    deriving (Show, Eq, Ord)

instance FromJSON TokenUsage where
    parseJSON = parseUsage

instance ToJSON TokenUsage where
    toJSON usage =
        Aeson.Object $
            KeyMap.fromList
                [ "prompt_tokens" .= usage.tokenPromptTokens
                , "completion_tokens" .= usage.tokenCompletionTokens
                , "total_tokens" .= usage.tokenTotalTokens
                ]
                `KeyMap.union` usage.tokenRawUsage

{- | Parse usage object handling different provider formats.
Handles field name variations across OpenAI, Claude, Kimi, and Mistral.
-}
parseUsage :: Aeson.Value -> Aeson.Parser TokenUsage
parseUsage = Aeson.withObject "Usage" $ \v -> do
    -- Handle different field names across providers
    -- OpenAI/Kimi/Mistral: prompt_tokens
    -- Claude: input_tokens
    prompt <- v .: "prompt_tokens" <|> v .: "input_tokens" <|> pure 0

    -- OpenAI/Kimi/Mistral: completion_tokens
    -- Claude: output_tokens
    completion <- v .: "completion_tokens" <|> v .: "output_tokens" <|> pure 0

    -- Total tokens - calculate if not provided
    total <- v .:? "total_tokens" >>= maybe (pure (prompt + completion)) pure

    -- Cached tokens - can be at top level (Kimi) or in details object (OpenAI)
    cached <-
        v .:? "cached_tokens"
            <|> ( v .:? "prompt_tokens_details" >>= \case
                    Just (Object d') -> d' .:? "cached_tokens"
                    _ -> pure Nothing
                )

    -- Thinking/reasoning tokens (Claude extended thinking)
    thinking <- v .:? "reasoning_tokens" <|> v .:? "thinking_tokens"

    TokenUsage prompt completion total cached thinking <$> pure v

-------------------------------------------------------------------------------
-- Trace with byte counts and token usage
-------------------------------------------------------------------------------

{- | Trace events for LLM calls, now including byte counts and token usage for tracking.

The constructors include:
- Byte counts for request/response payloads to support cost transparency and debugging
- Optional estimated tokens for requests (approximate, based on 4 chars/token)
- Optional actual token usage from provider responses
-}
data Trace
    = -- | Payload being sent to the LLM + byte count + estimated tokens
      CallChatCompletion !Aeson.Value !Int !(Maybe Int)
    | -- | Response received from the LLM + byte count + actual token usage
      GotChatCompletion !Aeson.Value !Int !(Maybe TokenUsage)
    | -- | Trace from the underlying HTTP client
      HttpClientTrace !HttpClient.Trace
    | -- | Moonshot/Kimi overloaded error backoff: retry attempt and delay in seconds
      OverloadedBackoff !Int !Int
    deriving (Show)

{- | Calculate byte size of a JSON Value for tracking purposes.

This encodes the value to JSON and returns the length of the
resulting ByteString. Useful for tracking payload sizes.
-}
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
newtype Tool = Tool {getTool :: ToolDescription}
    deriving (Show)

instance ToJSON Tool where
    toJSON (Tool t) =
        Aeson.object
            [ "type" .= ("function" :: Text)
            , "function"
                .= Aeson.object
                    [ "name" .= t.toolDescriptionName.getToolName
                    , "description" .= t.toolDescriptionText
                    , "parameters" .= Aeson.object (jsonSchema toplevelProperty)
                    ]
            ]
      where
        toplevelProperty :: ParamProperty
        toplevelProperty =
            ParamProperty
                { propertyKey = t.toolDescriptionName.getToolName
                , propertyType = ObjectParamType t.toolDescriptionParamProperties
                , propertyDescription = t.toolDescriptionText
                , propertyRequired = True
                , propertyTier = defaultParamTier
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

-------------------------------------------------------------------------------
-- Moonshot/Kimi overloaded error handling
-------------------------------------------------------------------------------

{- | Error response body from Moonshot/Kimi when overloaded.
Example:
  {"error":{"message":"The engine is currently overloaded, please try again later","type":"engine_overloaded_error"}}
-}
data OverloadedError = OverloadedError
    { overloadedMessage :: Text
    , overloadedType :: Text
    }
    deriving (Show)

instance FromJSON OverloadedError where
    parseJSON = Aeson.withObject "OverloadedError" $ \v -> do
        err <- v .: "error"
        OverloadedError
            <$> err .: "message"
            <*> err .: "type"

-- | Check if an error response indicates the engine is overloaded
isOverloadedError :: LByteString.ByteString -> Bool
isOverloadedError body =
    case Aeson.decode body of
        Just (OverloadedError msg errType) ->
            Text.isInfixOf "overloaded" (Text.toLower msg) || errType == "engine_overloaded_error"
        Nothing -> False

-- | Backoff delays in seconds for overloaded errors: 1min, 9min, 20min
overloadedBackoffDelays :: [Int]
overloadedBackoffDelays = [60, 540, 1200] -- 1min, 9min, 20min

-------------------------------------------------------------------------------
-- Retry logic for LLM calls with overloaded error handling
-------------------------------------------------------------------------------

-- | Extract token usage from a JSON response body if present.
extractTokenUsage :: Aeson.Value -> Maybe TokenUsage
extractTokenUsage body =
    case body of
        Aeson.Object obj ->
            case KeyMap.lookup "usage" obj of
                Just usageVal -> Aeson.parseMaybe parseUsage usageVal
                Nothing -> Nothing
        _ -> Nothing

callLLMPayload ::
    (ToJSON payload) =>
    Tracer IO Trace ->
    HttpClient.Runtime ->
    ApiBaseUrl ->
    payload ->
    IO (Either String Value)
callLLMPayload tracer rt baseUrl payload =
    callWithRetry 0
  where
    callWithRetry :: Int -> IO (Either String Value)
    callWithRetry attempt = do
        result <- makeRequest
        case result of
            Left err -> pure $ Left err
            Right (status, bodyVal)
                | status == HttpStatus.status429 && isOverloadedError (Aeson.encode bodyVal) ->
                    handleOverloaded attempt
                | HttpStatus.statusIsSuccessful status -> pure $ Right bodyVal
                | otherwise -> pure $ Left $ "HTTP error: " ++ show status

    makeRequest :: IO (Either String (HttpStatus.Status, Aeson.Value))
    makeRequest = do
        let payloadVal = Aeson.toJSON payload
        let requestBytes = calculatePayloadBytes payloadVal
        -- Estimate tokens: roughly 4 characters per token
        let estimatedTokens = Just (requestBytes `div` 4)
        runTracer tracer (CallChatCompletion payloadVal requestBytes estimatedTokens)
        httpRsp <- rt.post (contramap HttpClientTrace tracer) (baseUrl.getBaseUrl <> "/chat/completions") (Just payloadVal)
        case httpRsp of
            Left (HttpClient.SomeError err) -> pure $ Left err
            Right rsp -> do
                let status = NetHttpClient.responseStatus rsp
                case HttpClient.decodeBody (HttpClient.J rsp) of
                    Nothing -> pure $ Left "json decode body error"
                    Just body -> do
                        let responseBytes = calculatePayloadBytes body
                        -- Parse token usage from response if available
                        let mTokenUsage = extractTokenUsage body
                        runTracer tracer (GotChatCompletion body responseBytes mTokenUsage)
                        pure $ Right (status, body)

    handleOverloaded :: Int -> IO (Either String Value)
    handleOverloaded attempt =
        case drop attempted overloadedBackoffDelays of
            (delay : _) -> do
                runTracer tracer (OverloadedBackoff (attempt + 1) delay)
                threadDelay (delay * 1000000)
                callWithRetry (attempt + 1)
            [] -> pure $ Left "Moonshot/Kimi overloaded: max retry attempts exceeded"
      where
        attempted = attempt

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

data OpenAIToolCall
    = OpenAIToolCall
    { rawToolCall :: Aeson.Object
    , toolCallId :: Text
    , toolCallType :: Maybe Text
    , toolCallFunction :: ToolCallFunction
    }
    deriving (Show)

instance FromJSON OpenAIToolCall where
    parseJSON =
        Aeson.withObject "OpenAIToolCall" $ \v ->
            OpenAIToolCall
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
    , rspToolCalls :: Maybe [OpenAIToolCall]
    , rspReasoningContent :: Maybe Text
    , rspTokenUsage :: Maybe TokenUsage
    -- ^ Parsed token usage from the 'usage' field
    }
    deriving (Show)

instance FromJSON Response where
    parseJSON =
        Aeson.withObject "Response" $ \v -> do
            choices <- v .: "choices" :: Aeson.Parser (NonEmpty Aeson.Object)
            firstChoice <- (NonEmpty.head choices) .: "message"
            -- Parse usage field if present
            mUsage <- v .:? "usage" >>= mapM parseUsage
            Response
                <$> pure v
                <*> pure firstChoice
                <*> firstChoice .:? "finish_reason"
                <*> firstChoice .: "content"
                <*> firstChoice .:? "tool_calls"
                <*> firstChoice .:? "reasoning_content"
                <*> pure mUsage

parseLLMResponse :: Value -> Aeson.Parser Response
parseLLMResponse v = Aeson.parseJSON v
