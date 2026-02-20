{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.OneShot where

import Control.Exception (Exception, throwIO)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (traverse_)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist)

import System.Agents.AgentTree
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Runtime as Runtime
import System.Agents.Base (newConversationId,newStepId)
import System.Agents.Session.Base
import System.Agents.Session.Loop
import System.Agents.Session.Step
import System.Agents.Session.OpenAI
import System.Agents.ToolRegistration
import System.Agents.Tools
import System.Agents.Tools.Base
import System.Agents.ToolSchema

import qualified Data.Aeson.Key as AesonKey
import Prod.Tracer (Tracer (..), contramap)

mainPrintAgent :: Props -> IO ()
mainPrintAgent props = do
    withAgentTreeRuntime props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized _ -> pure ()

mainOneShotText :: Maybe FilePath -> Props -> Text -> IO ()
mainOneShotText mpath props query = do
    withAgentTreeRuntime props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized ai -> runOneShotAgent mpath ai.agentRuntime query

data SessionLoadingFailed = SessionLoadingFailed FilePath
  deriving (Show)
instance Exception SessionLoadingFailed


runOneShotAgent :: Maybe FilePath -> Runtime.Runtime -> Text -> IO ()
runOneShotAgent mpath rt query = do
    agent0 <- runtimeToAgent rt
    let agent = agentSetQuery (UserQuery query)
          $ maybe id agentStoreSession mpath
          $ fmap oneShotStep
          $ agent0
    result <- case mpath of
      Nothing -> do
        session0 <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId
        run agent session0
      Just path -> do
        msession0 <- readSession path
        case msession0 of
          Just session0 -> run agent session0
          Nothing -> throwIO (SessionLoadingFailed path)
    Text.putStrLn (getOneShotResult result)

-- | Stopping result type that carries the final response text.
newtype OneShotResult = OneShotResult  { getOneShotResult :: Text }

-- | Step function that stops when the LLM returns no tool calls.
oneShotStep :: (LlmTurnContent, Session) -> OneShotResult
oneShotStep (llmTurn,_) = OneShotResult $ extractResponseText llmTurn.llmResponse

-- | Converts a Runtime into an Agent that stops when no tool calls are present.
runtimeToAgent :: Runtime.Runtime -> IO (Agent (LlmTurnContent, Session))
runtimeToAgent rt = do
    let sPrompt = SystemPrompt rt.agentModel.modelSystemPrompt.getSystemPrompt
    let sTools = fmap toolRegistrationToSystemTool <$> rt.agentTools
    stepId <- newStepId
    convId <- newConversationId

    -- Create OpenAI completion config from runtime
    let completionConfig = OpenAICompletionConfig
            { cfgTracer =  contramap (Runtime.AgentTrace_Conversation rt.agentSlug rt.agentId convId . (Runtime.LLMTrace stepId)) rt.agentTracer
            , cfgRuntime = rt.agentAuthenticatedHttpClientRuntime
            , cfgBaseUrl = rt.agentModel.modelBaseUrl
            , cfgModelName = rt.agentModel.modelName
            , cfgModelFlavor = rt.agentModel.modelFlavor
            }
    let completeF = mkOpenAICompletion completionConfig

    pure $ Agent
        { step = naiveTilNoToolCallStep
        , sysPrompt = pure sPrompt
        , sysTools = sTools
        , usrQuery = pure Nothing
        , toolCall = executeToolCall rt.agentTools
        , complete = completeF
        }

-- | Extract text content from an LLM response.
extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse txt _) = Maybe.fromMaybe "" txt

-- | Execute a tool call using the runtime's registered tools.
executeToolCall :: IO [ToolRegistration] -> LlmToolCall -> IO UserToolResponse
executeToolCall registrations (LlmToolCall callVal) =
    -- Extract the tool call ID and function info from the LlmToolCall
    case parseLlmToolCall callVal of
        Nothing -> pure $ UserToolResponse $ Aeson.String "Failed to parse tool call"
        Just tc -> do
            regs <- registrations
            result <- llmCallTool regs tc
            pure $ callResultToUserToolResponse tc result

-- | Parse an LlmToolCall into OpenAI's ToolCall format.
parseLlmToolCall :: Aeson.Value -> Maybe OpenAI.ToolCall
parseLlmToolCall val =
    case Aeson.parseMaybe Aeson.parseJSON val of
        Just tc -> Just tc
        Nothing ->
            -- Try to extract from our LlmToolCall format
            case val of
                Aeson.Object obj ->
                    case (KeyMap.lookup "id" obj, KeyMap.lookup "function" obj) of
                        (Just (Aeson.String tid), Just funcVal) ->
                            Just $ OpenAI.ToolCall
                                { OpenAI.rawToolCall = obj
                                , OpenAI.toolCallId = tid
                                , OpenAI.toolCallType = KeyMap.lookup "type" obj >>= \v -> case v of Aeson.String t -> Just t; _ -> Nothing
                                , OpenAI.toolCallFunction = case Aeson.parseMaybe Aeson.parseJSON funcVal of
                                    Just f -> f
                                    Nothing -> OpenAI.ToolCallFunction (OpenAI.ToolName "") "" Nothing
                                }
                        _ -> Nothing
                _ -> Nothing

-- | Execute a single tool call against registered tools.
llmCallTool :: [ToolRegistration] -> OpenAI.ToolCall -> IO (CallResult OpenAI.ToolCall)
llmCallTool registrations call =
    let
        script =
            Maybe.listToMaybe $
                Maybe.mapMaybe (\r -> r.findTool call) registrations
        args = call.toolCallFunction.toolCallFunctionArgs
        spec = (,) <$> script <*> args
     in
        case spec of
            Nothing -> pure $ ToolNotFound call
            Just (t, v) -> do
                -- Use a dummy conversation ID for one-shot execution
                convId <- newConversationId
                ret <- t.toolRun (Tracer $ const $ pure ()) convId v
                pure $ mapCallResult (const call) ret

-- | Convert a CallResult to UserToolResponse.
callResultToUserToolResponse :: OpenAI.ToolCall -> CallResult OpenAI.ToolCall -> UserToolResponse
callResultToUserToolResponse _ result =
    case result of
        ToolNotFound _ ->
            UserToolResponse $ Aeson.String "Tool not found"
        BashToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ show err
        IOToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ show err
        McpToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.unlines ["tool-error", Text.pack $ show err]
        McpToolResult _ res ->
            UserToolResponse $ Aeson.toJSON res
        BlobToolSuccess _ v ->
            UserToolResponse $ Aeson.String $ Text.decodeUtf8 v

-- | Convert a ToolRegistration to a SystemTool for the Session agent.
toolRegistrationToSystemTool :: ToolRegistration -> SystemTool
toolRegistrationToSystemTool reg =
    let llmTool = reg.declareTool
        toolDefv1 = SystemToolDefinitionV1
            { name = llmTool.toolName.getToolName
            , llmName = llmTool.toolName.getToolName
            , description = llmTool.toolDescription
            , properties = llmTool.toolParamProperties
            , raw = Aeson.object
                [ "type" .= ("function" :: Text)
                , "function" .= Aeson.object
                    [ "name" .= llmTool.toolName.getToolName
                    , "description" .= llmTool.toolDescription
                    , "parameters" .= toolParamsToJson llmTool.toolParamProperties
                    ]
                ]
            }
     in SystemTool $ V1 toolDefv1

-- | Convert tool parameters to JSON schema.
toolParamsToJson :: [ParamProperty] -> Aeson.Value
toolParamsToJson props =
    Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= KeyMap.fromList (map paramPropertyToJson props)
        , "required" .= map propertyKey props
        , "additionalProperties" .= False
        ]
  where
    paramPropertyToJson :: ParamProperty -> (Aeson.Key, Aeson.Value)
    paramPropertyToJson p = (AesonKey.fromText p.propertyKey, paramTypeToJson p)
    paramTypeToJson :: ParamProperty -> Aeson.Value
    paramTypeToJson p =
        Aeson.object $
            [ "type" .= paramTypeToString p.propertyType
            , "description" .= p.propertyDescription
            ]
            ++ case p.propertyType of
                EnumParamType values -> ["enum" .= values]
                _ -> []

    paramTypeToString :: ParamType -> Text
    paramTypeToString NullParamType = "null"
    paramTypeToString StringParamType = "string"
    paramTypeToString BoolParamType = "boolean"
    paramTypeToString NumberParamType = "number"
    paramTypeToString (EnumParamType _) = "string"
    paramTypeToString (OpaqueParamType t) = t
    paramTypeToString (MultipleParamType t) = t
    paramTypeToString (ObjectParamType _) = "object"

readSession :: FilePath -> IO (Maybe Session)
readSession path = do
  fileExists <- doesFileExist path
  if fileExists
    then do
       dat <- BSL.readFile path
       pure $ Aeson.decode =<< lastLine dat
    else do
       sess <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId
       pure $ Just sess
  where
    lastLine :: LByteString.ByteString -> Maybe LByteString.ByteString
    lastLine dat = case BSL.lines dat of [] -> Nothing ; rows -> Just (last rows)

storeSession :: Session -> FilePath -> IO ()
storeSession sess path = do
  BSL.writeFile path (Aeson.encode sess <> "\n")

agentSetQuery :: forall r. UserQuery -> Agent r -> Agent r
agentSetQuery query agent =
    agent { usrQuery = pure (Just query) }

agentStoreSession :: forall r. FilePath -> Agent r -> Agent r
agentStoreSession path agent =
    agent { step = decorate agent.step }
  where
    decorate :: (Session -> IO (Action r)) -> (Session -> IO (Action r))
    decorate f = \sess -> storeSession sess path >> f sess

