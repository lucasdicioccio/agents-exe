{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Agents.LLMs.OpenAI where

import Data.Aeson (FromJSON, ToJSON, Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import Data.Monoid (Last (..))
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding as Text
import Data.Text.IO as Text
import Prod.Tracer (Tracer, contramap, runTracer)

import qualified System.Agents.HttpClient as HttpClient

-------------------------------------------------------------------------------
data Trace
    = CallChatCompletion !Aeson.Value
    | GotChatCompletion !Aeson.Value
    | HttpClientTrace !HttpClient.Trace
    deriving (Show)

-------------------------------------------------------------------------------
newtype ToolName = ToolName {getToolName :: Text}
    deriving (Show, Eq, Ord)

data ParamProperty = ParamProperty
    { propertyKey :: Text
    , propertyType :: Text
    , propertyDescription :: Text
    }
    deriving (Show)

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
                    , "parameters"
                        .= Aeson.object
                            [ "type" .= ("object" :: Text)
                            , "properties" .= HashMap.fromList (fmap toPair t.toolParamProperties)
                            , "additionalProperties" .= False
                            , "required" .= fmap propertyKey t.toolParamProperties
                            ]
                    ]
            , "strict" .= True
            ]
      where
        toPair :: ParamProperty -> (Text, Value)
        toPair p = (p.propertyKey, Aeson.object ["type" .= p.propertyType, "description" .= p.propertyDescription])

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

toolResponseMessages :: ToolCall -> Text -> Aeson.Value
toolResponseMessages tc txt =
    Aeson.object
        [ "role" .= ("tool" :: Text)
        , "tool_call_id" .= tc.toolCallId
        , "content" .= txt
        ]

data HistoryItem
    = PromptAnswered (Maybe Text) Response
    | ToolCalled (ToolCall, ByteString)

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
makeMessage (ToolCalled (t, bs)) = [toolResponseMessages t (Text.decodeUtf8 bs)]
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

data Model = Model
    { modelName :: Text
    , modelSystemPrompt :: SystemPrompt
    }
    deriving (Show, Eq, Ord)

gpt4Turbo :: SystemPrompt -> Model
gpt4Turbo = Model "gpt-4-turbo"

gpt4oMini :: SystemPrompt -> Model
gpt4oMini = Model "gpt-4o-mini"

simplePayload ::
    Model ->
    [Tool] ->
    History ->
    Maybe Text ->
    Aeson.Value
simplePayload model tools hist prompt =
    Aeson.object
        [ "model" .= model.modelName
        , "messages"
            .= makeMessages model.modelSystemPrompt hist prompt
        , "tools" .= tools
        -- todo:
        -- allow to tune json format with something like
        -- "json_format" .= Aeson.object [ "type" .= ("json_object :: Text) ]
        ]

callLLMPayload ::
    (ToJSON payload) =>
    Tracer IO Trace ->
    HttpClient.Runtime ->
    payload ->
    IO (Either String Value)
callLLMPayload tracer rt payload = do
    let payloadVal = Aeson.toJSON payload
    runTracer tracer (CallChatCompletion payloadVal)
    httpRsp <- rt.post (contramap HttpClientTrace tracer) "https://api.openai.com/v1/chat/completions" (Just payloadVal)
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
    , toolCallType :: Text
    , toolCallFunction :: ToolCallFunction
    }
    deriving (Show)

instance FromJSON ToolCall where
    parseJSON =
        Aeson.withObject "ToolCall" $ \v ->
            ToolCall
                <$> pure v
                <*> v .: "id"
                <*> v .: "type"
                <*> v .: "function"

data Response
    = Response
    { rawResponse :: Aeson.Object
    , chosenMessage :: Aeson.Object
    , finishReason :: Maybe Text
    , rspContent :: Maybe Text
    , rspToolCalls :: Maybe [ToolCall]
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

parseLLMResponse :: Value -> Aeson.Parser Response
parseLLMResponse v = Aeson.parseJSON v

printLastAnswer :: History -> IO ()
printLastAnswer hist = do
    let msg = rspContent =<< lastAnswerMaybe hist
    Text.putStrLn $ Maybe.fromMaybe "finished but with no response" msg
