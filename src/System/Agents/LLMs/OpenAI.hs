{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Agents.LLMs.OpenAI where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, ToJSON, Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bits (xor, shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LByteString
import Data.Char (ord)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.Monoid (Last (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Read as Text
import qualified Network.HTTP.Client as NetHttpClient
import Prod.Tracer (Tracer (..), contramap, runTracer)
import Text.Read (readMaybe)
import Numeric (showHex)

import qualified System.Agents.HttpClient as HttpClient
import System.Agents.ToolSchema

-------------------------------------------------------------------------------
data Trace
    = CallChatCompletion !Aeson.Value
    | GotChatCompletion !Aeson.Value
    | HttpClientTrace !HttpClient.Trace
    deriving (Show)

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

-- | Maximum length for a tool name (OpenAI limit)
maxToolNameLength :: Int
maxToolNameLength = 64

-- | Length of the hash prefix to use when truncating tool names
hashPrefixLength :: Int
hashPrefixLength = 8

-- | Compute a simple hash of a Text value.
-- Uses a variant of djb2 algorithm to produce a 32-bit hash.
textHash :: Text -> Text
textHash txt =
    let h = Text.foldl' hashStep 5381 txt
        hashStep hash char = ((hash `shiftL` 5) + hash) `xor` ord char
        -- Convert to hex, padding to 8 characters
        hex = Text.pack $ showHex (fromIntegral h :: Word) ""
    in Text.take hashPrefixLength (hex <> "00000000")

-- | Caps a tool name to 64 characters.
-- If the name exceeds 64 chars, the head is replaced with a hash prefix
-- and the description is updated to include the original tool name.
capToolName :: ToolName -> ToolName
capToolName tn =
    let originalName = tn.getToolName
        nameLen = Text.length originalName
    in if nameLen <= maxToolNameLength
        then tn
        else
            let -- Calculate how much of the tail we can keep
                 suffixLength = maxToolNameLength - hashPrefixLength - 1  -- -1 for the hyphen
                 suffix = Text.drop (nameLen - suffixLength) originalName
                 newName = textHash originalName <> "-" <> suffix
            in ToolName newName

adaptTool :: Tool -> Tool
adaptTool tool =
  let
    newToolName = capToolName tool.toolName
    newDesc = tool.toolDescription <> "\n(tool: " <> tool.toolName.getToolName <> ")"
  in if newToolName == tool.toolName
     then tool
     else tool { toolName = newToolName, toolDescription = newDesc }

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
                t.toolName.getToolName
                (ObjectParamType t.toolParamProperties)
                t.toolDescription

systemMessage :: Text -> Aeson.Value
systemMessage txt =
    Aeson.object
        [ "role" .= ("system" :: Text)
        , "content" .= txt
        ]

userPromptMessage :: Text -> Aeson.Value
userPromptMessage txt =
    Aeson.object
        [ "role" .= ("user" :: Text)
        , "content" .= txt
        ]

data ToolResponse
    = ToolNotFound
    | TextToolResponse (NonEmpty Text)
    | ToolFailure Text
    deriving (Show, Eq, Ord)

toolResponseMessages :: ToolCall -> ToolResponse -> Aeson.Value
toolResponseMessages tc ToolNotFound =
    Aeson.object
        [ "role" .= ("tool" :: Text)
        , "tool_call_id" .= tc.toolCallId
        , "content" .= ("the requested tool was not found" :: Text)
        ]
toolResponseMessages tc (ToolFailure rsp) =
    Aeson.object
        [ "role" .= ("tool" :: Text)
        , "tool_call_id" .= tc.toolCallId
        , "content" .= rsp
        ]
toolResponseMessages tc (TextToolResponse rsps) =
    Aeson.object
        [ "role" .= ("tool" :: Text)
        , "tool_call_id" .= tc.toolCallId
        , "content" .= NonEmpty.toList (fmap formatText rsps)
        ]
  where
    formatText txt =
        Aeson.object
            [ "type" .= ("text" :: Text)
            , "text" .= txt
            , "annotations" .= ([] :: [Text])
            ]

data HistoryItem
    = PromptAnswered (Maybe Text) Response
    | ToolCalled (ToolCall, ToolResponse)
    deriving (Show)

type History = Seq HistoryItem

lastAnswer :: History -> Last Response
lastAnswer hist =
    foldMap f $ hist
  where
    f :: HistoryItem -> Last Response
    f (PromptAnswered _ r) = Last $ Just r
    f (ToolCalled _) = Last $ Nothing

lastAnswerMaybe :: History -> Maybe Response
lastAnswerMaybe = getLast . lastAnswer

withLastAnswer :: a -> (Response -> a) -> History -> a
withLastAnswer v f =
    maybe v f . lastAnswerMaybe

makeMessage :: HistoryItem -> [Aeson.Value]
makeMessage (ToolCalled (t, rs)) = [toolResponseMessages t rs]
makeMessage (PromptAnswered prompt r) =
    Maybe.catMaybes
        [ fmap userPromptMessage prompt
        , Just $ Aeson.Object r.chosenMessage
        ]

newtype SystemPrompt = SystemPrompt {getSystemPrompt :: Text}
    deriving (Show, Eq, Ord, IsString)

makeMessages :: SystemPrompt -> History -> Maybe Text -> [Aeson.Value]
makeMessages sysPrompt hist prompt =
    mconcat [initialHistory, recordedHistory, lastMessage]
  where
    initialHistory =
        Maybe.catMaybes
            [ Just $ systemMessage sysPrompt.getSystemPrompt
            ]
    recordedHistory =
        foldMap makeMessage hist
    lastMessage =
        Maybe.catMaybes
            [fmap userPromptMessage prompt]

newtype ApiKey = ApiKey {revealApiKey :: ByteString}

makeApiKey :: ByteString -> ApiKey
makeApiKey = ApiKey

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

defaultFlavor :: ModelFlavor
defaultFlavor = OpenAIv1

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

renderPayload ::
    Model ->
    [Tool] ->
    History ->
    Maybe Text ->
    Aeson.Value
renderPayload model tools hist prompt =
    case model.modelFlavor of
        OpenAIv1 ->
            Aeson.object
                [ "model" .= model.modelName
                , "messages"
                    .= makeMessages model.modelSystemPrompt hist prompt
                , "tools" .= map adaptTool tools
                -- todo:
                -- allow to tune json format with something like
                -- "json_format" .= Aeson.object [ "type" .= ("json_object :: Text) ]
                ]
        MistralV1 ->
            Aeson.object
                [ "model" .= model.modelName
                , "messages"
                    .= makeMessages model.modelSystemPrompt hist prompt
                , "tools" .= map adaptTool tools
                , "tool_choice" .= ("any" :: Text)
                , "parallel_tool_calls" .= True
                ]
        KimiV1 ->
            Aeson.object
                [ "model" .= model.modelName
                , "messages"
                    .= makeMessages model.modelSystemPrompt hist (prompt <|> Just "ok")
                , "tools" .= map adaptTool tools
                -- todo:
                -- allow to tune json format with something like
                -- "json_format" .= Aeson.object [ "type" .= ("json_object :: Text) ]
                ]
        ClaudeV1 ->
            Aeson.object
                [ "model" .= model.modelName
                , "messages"
                    .= makeMessages model.modelSystemPrompt hist prompt
                , "tools" .= map adaptTool tools
                , "thinking"
                    .= Aeson.object
                        [ "type" .= ("enabled" :: Text)
                        , "budget_tokens" .= (10000 :: Int)
                        ]
                ]

callLLMPayload ::
    (ToJSON payload) =>
    Tracer IO Trace ->
    HttpClient.Runtime ->
    ApiBaseUrl ->
    payload ->
    IO (Either String Value)
callLLMPayload tracer rt (ApiBaseUrl baseUrl) payload = do
    let payloadVal = Aeson.toJSON payload
    runTracer tracer (CallChatCompletion payloadVal)
    httpRsp <- rt.post (contramap HttpClientTrace tracer) (baseUrl <> "/chat/completions") (Just payloadVal)
    case httpRsp of
        Left (HttpClient.SomeError err) -> do
            pure $ Left err
        Right rsp -> do
            case HttpClient.decodeBody (HttpClient.J rsp) of
                Nothing -> pure $ Left "json decode body error"
                Just body -> do
                    runTracer tracer (GotChatCompletion body)
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

finishedForToolCalls :: Response -> Bool
finishedForToolCalls r = r.finishReason == Just "tool_calls"

finishedForLength :: Response -> Bool
finishedForLength r = r.finishReason == Just "length"

finishedBecauseStopped :: Response -> Bool
finishedBecauseStopped r = r.finishReason == Just "stop"

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

locateResponseText :: History -> Maybe Text
locateResponseText hist = do
    rsp <-
        Maybe.listToMaybe $
            Maybe.mapMaybe viewResponse $
                toList $
                    Seq.reverse hist
    rsp.rspContent
  where
    viewResponse :: HistoryItem -> Maybe Response
    viewResponse (PromptAnswered _ rsp) = Just rsp
    viewResponse _ = Nothing

