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
    ThinkingOutput (..),

    -- * Main functions (OS-native)
    nodeToAgent,
    agentStoreSession,
    fileStoringCallback,
    mainPrintAgent,
    mainOneShotText,
    mainOneShotTextWithThinking,

    -- * Utility functions
    parseModelFlavor,
) where

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (Exception)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
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
import System.Agents.Base (ConversationId, newConversationId, newStepId)
import qualified System.Agents.Base as Base
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Runtime.Trace (ConversationTrace (..), Trace (..))
import System.Agents.Session.Base
import System.Agents.Session.Loop
import System.Agents.Session.OpenAI
import System.Agents.Session.Step (naiveTilNoToolCallStep)
import qualified System.Agents.Session.Compat as SessionCompat
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ToolName(..), ToolDescription(..), ParamProperty (..), ParamType (..))
import System.Agents.Tools.ExecuteToolCall (executeLlmToolCall)

import qualified Data.Aeson.Key as AesonKey
import qualified System.Agents.ToolPortal as ToolPortal

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
    let agent =
            agentSetQuery (UserQuery query) $
                agentWithSessionProgress (config.onSessionProgress convId) $
                    agent0

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
    SessionStore ->
    Maybe FilePath ->
    Maybe Session ->
    Props ->
    Text ->
    IO ()
mainOneShotText store mPath mSession props query = do
    mainOneShotTextWithThinking store mPath mSession ThinkingNone props query

-- | Run a one-shot agent with configurable thinking output.
mainOneShotTextWithThinking ::
    SessionStore ->
    Maybe FilePath ->
    Maybe Session ->
    ThinkingOutput ->
    Props ->
    Text ->
    IO ()
mainOneShotTextWithThinking store mPath mSession thinkingOut props query = do
    convId <- newConversationId
    -- Create a no-op tracer
    let tracer = Tracer $ const $ pure ()
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
extractResponseText (LlmResponse txt _thinking _) = Maybe.fromMaybe "" txt

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
    sTools <- fmap toolRegistrationToSystemTool <$> readTVarIO (osNodeTools node)

    stepId <- newStepId

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
                { cfgTracer = contramap (AgentTrace_Conversation (Base.slug agentCfg) (osNodeAgentId node) convId . (LLMTrace stepId)) tracer
                , cfgRuntime = httpRuntime
                , cfgBaseUrl = OpenAI.ApiBaseUrl $ Base.modelUrl agentCfg
                , cfgModelName = Base.modelName agentCfg
                , cfgModelFlavor = parseModelFlavor $ Base.flavor agentCfg
                }
    let completeF = mkOpenAICompletion completionConfig
    let tp = ToolPortal.makeToolPortal (contramap (ToolPortalTrace (Base.slug agentCfg)) tracer) (osNodeTools node)

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
                , sysTools = pure sTools
                , usrQuery = pure Nothing
                , toolCall = executeLlmToolCall (contramap (ToolTrace agentCfg.slug) tracer) (osNodeTools node) (SessionCompat.parseToolCallFromLlmToolCall, SessionCompat.callResultToUserToolResponse)
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

