{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.OneShot (
  runtimeToAgent,
  agentStoreSession,
  fileStoringCallback,
  mainPrintAgent,
  mainOneShotText
) where

import Control.Exception (Exception)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (traverse_)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import System.Agents.AgentTree
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Runtime as Runtime
import System.Agents.Base (newConversationId,newStepId,ConversationId, AgentId)
import System.Agents.Session.Base
import System.Agents.Session.Loop
import System.Agents.Session.Step
import System.Agents.Session.OpenAI
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration
import System.Agents.Tools
import System.Agents.Tools.Context (ToolExecutionContext, mkToolExecutionContext, CallStackEntry (..))
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))

import qualified Data.Aeson.Key as AesonKey
import Prod.Tracer (Tracer (..), contramap)

mainPrintAgent :: Props -> IO ()
mainPrintAgent props = do
    withAgentTreeRuntime props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized _ -> pure ()

-- | Configuration for one-shot execution with optional session persistence.
data OneShotConfig = OneShotConfig
  { onSessionProgress :: ConversationId -> OnSessionProgress
    -- ^ Callback for session progress updates (defaults to 'ignoreSessionProgress')
  , initialSession :: Maybe Session
    -- ^ Optional initial session to resume from
  , extraSavePath :: Maybe FilePath
    -- ^ Optional final session store path
  }

-- | Creates a configuration that optionally persists sessions to a file on top of the SessionStore.
fileStoringConfig :: SessionStore -> Maybe Session -> Maybe FilePath -> OneShotConfig
fileStoringConfig store mSession mPath = OneShotConfig
  { onSessionProgress = fileStoringCallback store
  , initialSession = mSession
  , extraSavePath = mPath
  }

-- | Run a one-shot agent with the given configuration.
runOneShotWithConfig :: SessionStore -> OneShotConfig -> ConversationId -> Runtime.Runtime -> Text -> IO OneShotResult
runOneShotWithConfig store config convId rt query = do
    agent0 <- runtimeToAgent store config.extraSavePath convId rt
    let agent = agentSetQuery (UserQuery query)
          $ agentWithSessionProgress (config.onSessionProgress convId)
          $ fmap oneShotStep
          $ agent0
    
    -- Create or use initial session with all required fields including sessionConversationId
    session0 <- case config.initialSession of
      Just s -> pure s
      Nothing -> Session [] <$> newSessionId <*> pure Nothing <*> newTurnId
    
    config.onSessionProgress convId (SessionStarted session0)
    result <- run convId agent session0
    config.onSessionProgress convId (SessionCompleted session0)
    pure result

-- | Legacy function: Run a one-shot agent with optional file-based session storage.
mainOneShotText :: SessionStore -> Maybe FilePath -> Maybe Session -> Props -> Text -> IO ()
mainOneShotText store mPath mSession props query = do
    convId <- newConversationId
    withAgentTreeRuntime props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized ai -> do
                let config = fileStoringConfig store mSession mPath
                result <- runOneShotWithConfig store config convId ai.agentRuntime query
                Text.putStrLn (getOneShotResult result)

data SessionLoadingFailed = SessionLoadingFailed FilePath
  deriving (Show)
instance Exception SessionLoadingFailed

-- | Stopping result type that carries the final response text.
newtype OneShotResult = OneShotResult  { getOneShotResult :: Text }

-- | Step function that stops when the LLM returns no tool calls.
oneShotStep :: (LlmTurnContent, Session) -> OneShotResult
oneShotStep (llmTurn,_) = OneShotResult $ extractResponseText llmTurn.llmResponse

-- | Converts a Runtime into an Agent that stops when no tool calls are present.
--
-- The agent is configured with the runtime's agent ID and the provided conversation ID.
-- These identifiers are used to construct the 'ToolExecutionContext' passed to tools
-- during execution, allowing tools to access session metadata.
runtimeToAgent :: SessionStore -> Maybe FilePath -> ConversationId -> Runtime.Runtime -> IO (Agent (LlmTurnContent, Session))
runtimeToAgent store mPath convId rt = do
    let sPrompt = SystemPrompt rt.agentModel.modelSystemPrompt.getSystemPrompt
    let sTools = fmap toolRegistrationToSystemTool <$> rt.agentTools
    stepId <- newStepId

    -- Create OpenAI completion config from runtime
    let completionConfig = OpenAICompletionConfig
            { cfgTracer =  contramap (Runtime.AgentTrace_Conversation rt.agentSlug rt.agentId convId . (Runtime.LLMTrace stepId)) rt.agentTracer
            , cfgRuntime = rt.agentAuthenticatedHttpClientRuntime
            , cfgBaseUrl = rt.agentModel.modelBaseUrl
            , cfgModelName = rt.agentModel.modelName
            , cfgModelFlavor = rt.agentModel.modelFlavor
            }
    let completeF = mkOpenAICompletion completionConfig

    pure $ 
      agentStoreSession store mPath convId $
        Agent
        { step = naiveTilNoToolCallStep
        , sysPrompt = pure sPrompt
        , sysTools = sTools
        , usrQuery = pure Nothing
        , toolCall = executeToolCall rt.agentId convId rt.agentTools
        , complete = completeF
        , contextConfig = defaultContextConfig
        }

-- | Extract text content from an LLM response.
extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse txt _thinking _) = Maybe.fromMaybe "" txt

-- | Execute a tool call using the runtime's registered tools.
--
-- Constructs a 'ToolExecutionContext' with the provided agent and session identifiers,
-- then executes the tool with this context. The context gives tools access to:
--
-- * 'ctxSessionId' - From the current session
-- * 'ctxConversationId' - The conversation ID passed from the runtime
-- * 'ctxTurnId' - From the current session
-- * 'ctxAgentId' - The agent ID from the runtime configuration
-- * 'ctxFullSession' - Populated according to 'ContextConfig' (not directly available here)
executeToolCall :: 
    AgentId              -- ^ Agent ID for context
    -> ConversationId    -- ^ Conversation ID for context
    -> IO [ToolRegistration] 
    -> ToolExecutionContext  -- ^ Context passed from runStepM (ignored, we construct fresh)
    -> LlmToolCall 
    -> IO UserToolResponse
executeToolCall agentId convId registrations _ctx (LlmToolCall callVal) =
    -- Extract the tool call ID and function info from the LlmToolCall
    case parseLlmToolCall callVal of
        Nothing -> pure $ UserToolResponse $ Aeson.String "Failed to parse tool call"
        Just tc -> do
            regs <- registrations
            -- Construct context for this tool execution
            -- We don't have access to the full session here, so we generate a minimal context
            -- with the identifiers we have. In the future, we could pass session through the
            -- Agent type or use a Reader pattern to access it here.
            sessId <- newSessionId
            tId <- newTurnId
            let toolCtx = mkToolExecutionContext
                    sessId
                    convId
                    tId
                    (Just agentId)
                    Nothing  -- No full session available at this point
                    [CallStackEntry "root" convId 0]  -- Root call stack entry
                    Nothing  -- No max recursion depth by default
            result <- llmCallTool regs toolCtx tc
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
--
-- The 'ToolExecutionContext' is now passed as a parameter, providing tools with
-- access to session metadata without exposing these details to the LLM.
llmCallTool :: 
    [ToolRegistration] 
    -> ToolExecutionContext  -- ^ Context containing session metadata for tools
    -> OpenAI.ToolCall 
    -> IO (CallResult OpenAI.ToolCall)
llmCallTool registrations ctx call =
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
                ret <- t.toolRun (Tracer $ const $ pure ()) ctx v
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
        OpenAPIToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        OpenAPIToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "OpenAPI tool error: " <> err
        PostgRESToolResult _ toolResult ->
            UserToolResponse $ Aeson.toJSON toolResult
        PostgRESToolError _ err ->
            UserToolResponse $ Aeson.String $ Text.pack $ "PostgREST tool error: " <> err

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

-- | Creates a callback that stores session progress to a file.
-- This is useful for creating an 'OnSessionProgress' handler that persists to disk.
fileStoringCallback :: SessionStore -> ConversationId -> OnSessionProgress
fileStoringCallback store convId progress =
    case progress of
        SessionUpdated sess -> SessionStore.storeSession store convId sess
        SessionCompleted sess -> SessionStore.storeSession store convId sess
        SessionStarted sess -> SessionStore.storeSession store convId sess
        SessionFailed sess _ -> SessionStore.storeSession store convId sess

-- | Creates a callback that stores session progress using a SessionStore.
sessionStoreCallback :: SessionStore -> ConversationId -> OnSessionProgress
sessionStoreCallback store convId progress =
    case progress of
        SessionUpdated sess -> storeSessionWithStore sess
        SessionCompleted sess -> storeSessionWithStore sess
        SessionStarted sess -> storeSessionWithStore sess
        SessionFailed sess _ -> storeSessionWithStore sess
  where
    storeSessionWithStore sess =
      SessionStore.storeSession store convId sess

-- | Creates a callback that stores session progress using an extra optional session-path.
-- This second is useful in OneShot command where the command-line drives the filename.
filepathStoreCallback :: Maybe FilePath -> OnSessionProgress
filepathStoreCallback Nothing _ = pure ()
filepathStoreCallback (Just path) progress =
    case progress of
        SessionUpdated sess -> go sess
        SessionCompleted sess -> go sess
        SessionStarted sess -> go sess
        SessionFailed sess _ -> go sess
  where
    go sess =
      SessionStore.storeSessionToFile sess path

agentSetQuery :: forall r. UserQuery -> Agent r -> Agent r
agentSetQuery query agent =
    agent { usrQuery = pure (Just query) }

-- | Wrap an agent to store sessions using a SessionStore.
-- The session is stored using the conversation ID from the session.
agentStoreSession :: forall r. SessionStore -> Maybe FilePath -> ConversationId -> Agent r -> Agent r
agentStoreSession store mPath convId agent =
    agentWithSessionProgress handleProgress agent
  where
    handleProgress x = do
      sessionStoreCallback store convId x
      filepathStoreCallback mPath x

-- | Wrap an agent to emit session progress events after each step.
agentWithSessionProgress :: forall r. OnSessionProgress -> Agent r -> Agent r
agentWithSessionProgress onProgress agent =
    agent { step = decorate agent.step }
  where
    decorate :: (Session -> IO (Action r)) -> (Session -> IO (Action r))
    decorate f = \sess -> do
        onProgress (SessionUpdated sess)
        f sess

