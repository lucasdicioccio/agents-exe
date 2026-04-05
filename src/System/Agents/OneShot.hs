{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | One-shot execution of agents with OS-native implementation.

This module is now a compatibility wrapper around 'System.Agents.OS.Interfaces.OneShot'.
It re-exports the main functions for backward compatibility while using the
new OS-native ECS-based implementation internally.

For new code, consider using 'System.Agents.OS.Interfaces.OneShot' directly.
-}
module System.Agents.OneShot (
    -- * Types
    Trace (..),
    ThinkingOutput (..),

    -- * Main functions (OS-native)
    nodeToAgent,
    fileStoringCallback,
    mainPrintAgent,
    mainOneShotText,
    mainOneShotTextWithThinking,

    -- * Re-export from ProgressiveDisclosure
    agentEvaluateActiveTools,

    -- * Re-export from StoreSessionProgress
    agentStoreSession,
    agentWithSessionProgress,

    -- * Utility functions
    mapProgressiveDisclosureTrace,
    parseModelFlavor,
) where

import Control.Concurrent.STM (readTVarIO)
import Control.Exception (Exception)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
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
import System.Agents.Tools.ExecuteToolCall (executeLlmToolCall)

-- Re-export agentEvaluateActiveTools from the new module
import System.Agents.Combinators.ProgressiveDisclosure (agentEvaluateActiveTools)
import qualified System.Agents.Combinators.ProgressiveDisclosure as ProgressiveDisclosure

-- Re-export session storage combinators
import System.Agents.Combinators.StoreSessionProgress (
    agentStoreSession,
    agentWithSessionProgress,
 )

import qualified System.Agents.ToolPortal as ToolPortal

-- | Trace type for OneShot operations.
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

-- | Print agent information.
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
    agent1 <- agentEvaluateActiveTools (contramap mapProgressiveDisclosureTrace tracer) (osNodeTools node) agent0

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

-- | Map ProgressiveDisclosure trace to OneShot trace.
mapProgressiveDisclosureTrace :: ProgressiveDisclosure.Trace -> Trace
mapProgressiveDisclosureTrace (ProgressiveDisclosure.ToolRegistrationTrace t) = ToolRegistrationTrace t
mapProgressiveDisclosureTrace (ProgressiveDisclosure.ToolPortalTrace t) = ToolPortalTrace t

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

-- | Extract text from an LLM response.
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
                , toolCall =
                    executeLlmToolCall
                        (contramap ToolRegistrationTrace tracer)
                        (readTVarIO $ osNodeTools node)
                        (SessionCompat.parseToolCallFromLlmToolCall, SessionCompat.callResultToUserToolResponse)
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

agentSetQuery :: forall r. UserQuery -> Agent r -> Agent r
agentSetQuery query agent =
    agent{usrQuery = pure (Just query)}
