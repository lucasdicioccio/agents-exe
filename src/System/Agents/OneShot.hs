{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | One-shot execution of agents with OS compatibility layer.

This module provides single-conversation execution (batch mode) with support
for both the legacy Runtime interface and the new OS model via RuntimeBridge.

The primary functions ('mainOneShotText', 'runtimeToAgent') have been updated
to use OS-native types. 'mainOneShotText' now works directly with 'OSAgentTree'.
-}
module System.Agents.OneShot (
    -- * Types
    Trace (..),
    ThinkingOutput (..),

    -- * Main functions (OS-native)
    nodeToAgent,
    agentStoreSession,
    agentEvaluateActiveTools,
    fileStoringCallback,
    mainPrintAgent,
    mainOneShotText,
    mainOneShotTextWithThinking,

    -- * Utility functions
    parseModelFlavor,
) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Exception (Exception)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Prod.Tracer (Tracer (..), contramap)
import System.IO (stderr)

import System.Agents.AgentTree (
    LoadAgentResult (..),
    LoadedApiKeys,
    OSAgentNode (..),
    OSAgentTree (..),
    Props (..),
    withAgentTree,
 )
import System.Agents.Base (ConversationId, newConversationId)
import qualified System.Agents.Base as Base
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Base
import qualified System.Agents.Session.Compat as SessionCompat
import System.Agents.Session.Loop
import System.Agents.Session.OpenAI
import System.Agents.Session.Step (naiveTilNoToolCallStep)
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolRegistration
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..), ToolDescription (..), ToolName (..))
import System.Agents.Tools.Activation (Activation (..))
import System.Agents.Tools.Activation.Session (
    foldSession,
    isToolgroupActive,
    ToolboxSessionState (..),
    extractToolgroups,
    makeActivateTool,
    makeDeactivateTool,
    makeDiscoverTools,
 )
import System.Agents.Tools.ExecuteToolCall (executeLlmToolCall)

import qualified Data.Aeson.Key as AesonKey
import qualified System.Agents.ToolPortal as ToolPortal

data Trace
    = ToolRegistrationTrace !ToolRegistration.Trace
    | ToolPortalTrace !ToolPortal.Trace
    | OpenAITrace !OpenAI.Trace
    deriving (Show)

-- | Controls where thinking content should be output.
data ThinkingOutput
    = -- | Suppress thinking output (default)
      ThinkingNone
    | -- | Output thinking to stdout
      ThinkingStdout
    | -- | Output thinking to stderr
      ThinkingStderr
    deriving (Show, Eq, Ord)

mainPrintAgent :: Props -> IO ()
mainPrintAgent props = do
    withAgentTree props $ \x -> do
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
    , thinkingOutput :: ThinkingOutput
    -- ^ Where to output thinking content (defaults to 'ThinkingNone')
    }

-- | Creates a configuration that optionally persists sessions to a file on top of the SessionStore.
fileStoringConfig :: SessionStore -> Maybe Session -> Maybe FilePath -> OneShotConfig
fileStoringConfig store mSession mPath =
    OneShotConfig
        { onSessionProgress = fileStoringCallback store
        , initialSession = mSession
        , extraSavePath = mPath
        , thinkingOutput = ThinkingNone
        }

-- | Run a one-shot agent with the given configuration.
runOneShotWithConfig ::
    SessionStore ->
    OneShotConfig ->
    ConversationId ->
    -- | The tracer for logging
    Tracer IO Trace ->
    -- | API keys for creating HTTP runtime
    LoadedApiKeys ->
    -- | The agent node to execute
    OSAgentNode ->
    Text ->
    IO OneShotResult
runOneShotWithConfig store config convId tracer loadedApiKeys node query = do
    agent0 <- nodeToAgentWithThinking store config.extraSavePath config.thinkingOutput convId tracer loadedApiKeys node
    
    -- Apply dynamic tool filtering based on session activation state
    -- This allows tools to be enabled/disabled via meta_activate_tool/meta_deactivate_tool
    agent1 <- agentEvaluateActiveTools (osNodeTools node) agent0
    
    let agent =
            agentSetQuery (UserQuery query) $
                agentWithSessionProgress (config.onSessionProgress convId) $
                    agent1

    -- Create or use initial session with all required fields including sessionConversationId
    session0 <- case config.initialSession of
        Just s -> pure s
        Nothing -> Session [] <$> newSessionId <*> pure Nothing <*> newTurnId

    config.onSessionProgress convId (SessionStarted session0)
    (llmTurn, _) <- run convId agent session0
    config.onSessionProgress convId (SessionCompleted session0)
    pure $ OneShotResult $ extractResponseText llmTurn.llmResponse

{- | Run a one-shot agent with optional file-based session storage.

This function uses OS-native types for agent execution.
-}
mainOneShotText ::
    Tracer IO Trace ->
    SessionStore ->
    Maybe FilePath ->
    Maybe Session ->
    Props ->
    Text ->
    IO ()
mainOneShotText tracer store mPath mSession props query = do
    mainOneShotTextWithThinking tracer store mPath mSession ThinkingNone props query

-- | Run a one-shot agent with configurable thinking output.
mainOneShotTextWithThinking ::
    Tracer IO Trace ->
    SessionStore ->
    Maybe FilePath ->
    Maybe Session ->
    ThinkingOutput ->
    Props ->
    Text ->
    IO ()
mainOneShotTextWithThinking tracer store mPath mSession thinkingOut props query = do
    convId <- newConversationId
    withAgentTree props $ \x -> do
        case x of
            Errors errs -> traverse_ print errs
            Initialized tree -> do
                let config = (fileStoringConfig store mSession mPath){thinkingOutput = thinkingOut}
                let node = osTreeRoot tree
                OneShotResult result <- runOneShotWithConfig store config convId tracer (apiKeys props) node query
                Text.putStrLn result

data SessionLoadingFailed = SessionLoadingFailed FilePath
    deriving (Show)
instance Exception SessionLoadingFailed

-- | Stopping result type that carries the final response text.
newtype OneShotResult = OneShotResult Text

-- | Extract text content from an LLM response.
extractResponseText :: LlmResponse -> Text
extractResponseText (LlmResponse txt _thinking _ _) = Maybe.fromMaybe "" txt

-- | Parse flavor from text, defaulting to OpenAIv1 if not recognized.
parseModelFlavor :: Text -> OpenAI.ModelFlavor
parseModelFlavor txt = Maybe.fromMaybe OpenAI.OpenAIv1 $ OpenAI.parseFlavor txt

-- | Look up an API key by its ID from the loaded API keys.
lookupApiKey :: Text -> LoadedApiKeys -> Maybe OpenAI.ApiKey
lookupApiKey keyId keys = fmap snd $ listToMaybe $ filter ((== keyId) . fst) keys

{- | Converts an OSAgentNode into an Agent that stops when no tool calls are present.

The agent is configured with the node's agent ID and the provided conversation ID.
These identifiers are used to construct the 'ToolExecutionContext' passed to tools
during execution, allowing tools to access session metadata.

This function uses OS-native types for agent execution.
-}
nodeToAgent ::
    SessionStore ->
    Maybe FilePath ->
    ConversationId ->
    -- | The tracer for logging
    Tracer IO Trace ->
    -- | API keys for creating HTTP runtime
    LoadedApiKeys ->
    OSAgentNode ->
    IO (Agent (LlmTurnContent, Session))
nodeToAgent store mPath convId tracer loadedApiKeys node =
    nodeToAgentWithThinking store mPath ThinkingNone convId tracer loadedApiKeys node

-- | Converts an OSAgentNode into an Agent with configurable thinking output.
nodeToAgentWithThinking ::
    SessionStore ->
    Maybe FilePath ->
    ThinkingOutput ->
    ConversationId ->
    -- | The tracer for logging
    Tracer IO Trace ->
    -- | API keys for creating HTTP runtime
    LoadedApiKeys ->
    OSAgentNode ->
    IO (Agent (LlmTurnContent, Session))
nodeToAgentWithThinking store mPath thinkingOut convId tracer loadedApiKeys node = do
    let agentCfg = osNodeConfig node
    let sPrompt = SystemPrompt $ Text.unlines $ Base.systemPrompt agentCfg

    -- Read tools from the OS-native TVar
    allTools <- fmap toolRegistrationToSystemTool <$> readTVarIO (osNodeTools node)

    -- Get the API key for this agent and create HTTP runtime
    let apiKeyId = Base.apiKeyId agentCfg
    let mApiKey = lookupApiKey apiKeyId loadedApiKeys
    httpRuntime <- case mApiKey of
        Just apiKey -> HttpClient.newRuntime (HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey)
        Nothing -> HttpClient.newRuntime HttpClient.NoToken

    -- Create OpenAI completion config from node config
    -- Use node's agent slug and id for tracing
    let completionConfig =
            OpenAICompletionConfig
                { cfgTracer = contramap OpenAITrace tracer
                , cfgRuntime = httpRuntime
                , cfgBaseUrl = OpenAI.ApiBaseUrl $ Base.modelUrl agentCfg
                , cfgModelName = Base.modelName agentCfg
                , cfgModelFlavor = parseModelFlavor $ Base.flavor agentCfg
                }
    let completeF = mkOpenAICompletion completionConfig
    let tp = ToolPortal.makeToolPortal (contramap ToolPortalTrace tracer) (osNodeTools node)

    pure $
        agentStoreSession store mPath convId $
            Agent
                { step = \sess -> do
                    action <- naiveTilNoToolCallStep sess
                    -- Output thinking if present and configured
                    case action of
                        Stop (llmTurn, _) ->
                            case (thinkingOut, llmTurn.llmResponse.responseThinking) of
                                (ThinkingStdout, Just t) -> Text.putStrLn t
                                (ThinkingStderr, Just t) -> Text.hPutStrLn stderr t
                                _ -> pure ()
                        _ -> pure ()
                    pure action
                , sysPrompt = pure sPrompt
                , sysTools = pure allTools
                , usrQuery = pure Nothing
                , toolCall = executeLlmToolCall (contramap ToolRegistrationTrace tracer) (osNodeTools node) (SessionCompat.parseToolCallFromLlmToolCall, SessionCompat.callResultToUserToolResponse)
                , toolPortal = tp
                , complete = completeF
                , contextConfig = defaultContextConfig
                }

-- | Convert a ToolRegistration to a SystemTool for the Session agent.
toolRegistrationToSystemTool :: ToolRegistration -> SystemTool
toolRegistrationToSystemTool reg =
    let llmTool = reg.declareTool
        toolDefv1 =
            SystemToolDefinitionV1
                { name = llmTool.toolDescriptionName.getToolName
                , llmName = llmTool.toolDescriptionName.getToolName
                , description = llmTool.toolDescriptionText
                , properties = llmTool.toolDescriptionParamProperties
                , raw =
                    Aeson.object
                        [ "type" .= ("function" :: Text)
                        , "function"
                            .= Aeson.object
                                [ "name" .= llmTool.toolDescriptionName.getToolName
                                , "description" .= llmTool.toolDescriptionText
                                , "parameters" .= toolParamsToJson llmTool.toolDescriptionParamProperties
                                ]
                        ]
                }
     in SystemTool $ V1 toolDefv1

{- | Convert tool parameters to JSON schema.

Only properties with 'propertyRequired = True' are included in the 'required' array.
-}
toolParamsToJson :: [ParamProperty] -> Aeson.Value
toolParamsToJson props =
    Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= KeyMap.fromList (map paramPropertyToJson props)
        , "required" .= map propertyKey (filter propertyRequired props)
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

{- | Creates a callback that stores session progress to a file.
This is useful for creating an 'OnSessionProgress' handler that persists to disk.
-}
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

{- | Creates a callback that stores session progress using an extra optional session-path.
This second is useful in OneShot command where the command-line drives the filename.
-}
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
    agent{usrQuery = pure (Just query)}

{- | Wrap an agent to store sessions using a SessionStore.
The session is stored using the conversation ID from the session.
-}
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
    agent{step = decorate agent.step}
  where
    decorate :: (Session -> IO (Action r)) -> (Session -> IO (Action r))
    decorate f = \sess -> do
        onProgress (SessionUpdated sess)
        f sess

-- | A nil UUID for creating initial empty session references.
nilUUID :: UUID
nilUUID = UUID.fromWords 0 0 0 0

{- | Wrap an agent to dynamically evaluate and filter active tools based on session history.

This decorator:
1. Takes a TVar containing the available ToolRegistrations
2. Creates an IORef to track the current session state
3. Updates the IORef after each step (like onProgress)
4. Reads from the TVar and filters ToolRegistrations based on session activation state
5. Maps the filtered ToolRegistrations to SystemTools
6. Adds meta tools (meta_activate_tool, meta_deactivate_tool, meta_discover_tools) when toolgroups exist

Tools are filtered based on 'meta_activate_tool' and 'meta_deactivate_tool' calls
in the session history. The list of ToolRegistrations is read fresh from the TVar
on each access, allowing runtime changes to the available tools.

Tools with 'OnDemandActivated' activation are only visible when their toolgroup
is active. Tools with 'AlwaysActivated' or no activation are always visible.

Example usage:

@
agent <- nodeToAgent store mPath convId tracer loadedApiKeys node
dynamicAgent <- agentEvaluateActiveTools (osNodeTools node) agent
@
-}
agentEvaluateActiveTools :: forall r. TVar [ToolRegistration] -> Agent r -> IO (Agent r)
agentEvaluateActiveTools toolsTVar agent = do
    -- Create an IORef to track the current session
    -- Use nil UUIDs for initial empty session
    let emptySessionId = SessionId nilUUID
    let emptyTurnId = TurnId nilUUID
    let emptySession = Session [] emptySessionId Nothing emptyTurnId
    sessionRef <- newIORef emptySession
    
    -- Create the decorated agent
    pure $ agent
        { step = decorateStep sessionRef agent.step
        , sysTools = filterTools sessionRef toolsTVar
        }
  where
    -- | Decorates the step function to update the sessionRef after each step
    decorateStep :: IORef Session -> (Session -> IO (Action r)) -> (Session -> IO (Action r))
    decorateStep sessionRef stepFn = \sess -> do
        -- Update the session reference with the current session
        writeIORef sessionRef sess
        -- Call the original step function
        stepFn sess
    
    -- | Filters tools based on the current session activation state
    filterTools :: IORef Session -> TVar [ToolRegistration] -> IO [SystemTool]
    filterTools sessionRef tvar = do
        -- Get the current session state
        currentSession <- readIORef sessionRef
        
        -- Compute the activation state from session history
        let activationState = foldSession currentSession
        
        -- Read the ToolRegistrations from the TVar
        allToolRegs <- readTVarIO tvar
        
        -- Extract all unique toolgroups from the registrations
        let toolgroups = extractToolgroups allToolRegs
        print toolgroups
        
        -- Build meta tools if any toolgroups exist
        let metaTools = if null toolgroups
                        then []
                        else [makeActivateTool toolgroups, makeDeactivateTool toolgroups, makeDiscoverTools toolgroups]
        
        -- Filter ToolRegistrations based on activation state
        let activeToolRegs = filter (isToolRegActive activationState) allToolRegs
        
        -- Combine meta tools with active tool registrations, then map to SystemTools
        pure $ map toolRegistrationToSystemTool (metaTools ++ activeToolRegs)
    
    -- | Check if a ToolRegistration is active based on the activation state
    isToolRegActive :: ToolboxSessionState -> ToolRegistration -> Bool
    isToolRegActive activationState toolReg = 
        case toolReg.toolActivation of
            Nothing -> True  -- No activation control = always visible
            Just AlwaysActivated -> True  -- Always activated
            Just (OnDemandActivated toolgroup) -> 
                -- Check if the toolgroup is active in the session state
                isToolgroupActive activationState toolgroup
            Just (FirstNStepsActivated _ _) -> 
                -- For now, first-N steps activation is treated as always active
                -- Future: track step count in session state
                True

