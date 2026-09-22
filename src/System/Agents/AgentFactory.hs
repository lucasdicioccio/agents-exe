{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The single place where a loaded agent node becomes a runnable 'Agent'.

Every front-end (one-shot @run@, the TUI, the durable @session@ commands, the
MCP server, and agents called as tools by other agents) builds its agents with
'buildAgent', so they all get the same behaviour:

1. prompt, tools, tool execution, and the LLM completion function;
2. the execution settings from the agent's JSON ('applyAgentDurableConfig');
3. the optional continuation store and tool cache from 'AgentDeps';
4. progressive disclosure of tools ('agentEvaluateActiveTools');
5. session persistence into the 'SessionSink' from 'AgentDeps', installed
   last so it does not depend on fields set afterwards.

The agent never writes to stdout or stderr: front-ends that print (for
example thinking output) decorate the result.
-}
module System.Agents.AgentFactory (
    -- * Building agents
    buildAgent,
    AgentRole (..),

    -- * Dependencies
    AgentDeps (..),
    defaultAgentDeps,
    fileAgentDeps,
    SessionSink (..),
    Completion,

    -- * Traces
    Trace (..),
    mapProgressiveDisclosureTrace,

    -- * Helpers
    toolRegistrationToSystemTool,
) where

import Control.Concurrent.STM (readTVarIO)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Prod.Tracer (Tracer, contramap)

import System.Agents.AgentTree (LoadedApiKeys, OSAgentNode (..))
import System.Agents.Base (ConversationId)
import qualified System.Agents.Base as Base
import System.Agents.Combinators.ProgressiveDisclosure (agentEvaluateActiveTools, toolRegistrationToSystemTool)
import qualified System.Agents.Combinators.ProgressiveDisclosure as ProgressiveDisclosure
import System.Agents.Combinators.StoreSessionProgress (SessionSink (..), agentPersistSession)
import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.AgentConfig (applyAgentDurableConfig)
import System.Agents.Session.Async (ContinuationStore)
import System.Agents.Session.Base
import qualified System.Agents.Session.Compat as SessionCompat
import System.Agents.Session.OpenAI (OpenAICompletionConfig (..), mkOpenAICompletion)
import System.Agents.Session.Step (naiveTilNoToolCallStep)
import System.Agents.SessionStore (SessionLabels (..), SessionStore, conversationIdToSessionId)
import qualified System.Agents.ToolPortal as ToolPortal
import qualified System.Agents.ToolRegistration as ToolRegistration
import System.Agents.Tools.Cache (ToolCache)
import System.Agents.Tools.Context (CallStackEntry (..))
import System.Agents.Tools.ExecuteToolCall (executeLlmToolCall)
import System.Agents.Tools.Params.Types (Params)

-------------------------------------------------------------------------------
-- Traces
-------------------------------------------------------------------------------

data Trace
    = ToolRegistrationTrace !ToolRegistration.Trace
    | ToolPortalTrace !ToolPortal.Trace
    | OpenAITrace !OpenAI.Trace
    deriving (Show)

mapProgressiveDisclosureTrace :: ProgressiveDisclosure.Trace -> Trace
mapProgressiveDisclosureTrace (ProgressiveDisclosure.ToolRegistrationTrace t) = ToolRegistrationTrace t
mapProgressiveDisclosureTrace (ProgressiveDisclosure.ToolPortalTrace t) = ToolPortalTrace t

-------------------------------------------------------------------------------
-- Dependencies
-------------------------------------------------------------------------------

-- | An LLM completion function.
type Completion = LlmCompletion -> IO (LlmResponse, [LlmToolCall])

-- | What agents need from their host, shared by an agent and its sub-agents.
data AgentDeps = AgentDeps
    { adApiKeys :: LoadedApiKeys
    , adSessionSink :: SessionSink
    -- ^ Where every agent built with these dependencies stores its sessions.
    , adContinuationStore :: Maybe ContinuationStore
    , adToolCache :: Maybe ToolCache
    , adCompletion :: Maybe (OSAgentNode -> Completion)
    {- ^ Replaces the LLM call, e.g. with a mock in tests. 'Nothing' calls the
    agent's configured OpenAI-compatible endpoint.
    -}
    , adOnTextDelta :: Maybe (Text.Text -> IO ())
    {- ^ Streams the LLM's answers, passing each piece of text here as it
    arrives. Ignored when 'adCompletion' replaces the LLM call.
    -}
    , adLiveParams :: IO Params
    {- ^ The current session/message-scope parameter overlay (§7,
    @todos/tool-partial-application.md@), read fresh every time the tool
    list is built: what decides, for this agent's own @whenUnbound: "expose"@
    bindings, whether their argument is currently bound (hidden) or not
    (visible). 'pure mempty' outside @agents-server@, where there is no
    notion of a session distinct from the process: an 'Expose'd argument then
    behaves like a process-scope-only binding, decided once from
    'osNodeParams'. @agents-server@'s 'System.Agents.Host.Runner' overrides
    this per session, backed by that session's own live parameter store.
    -}
    }

-- | Dependencies that store nothing.
defaultAgentDeps :: LoadedApiKeys -> AgentDeps
defaultAgentDeps keys =
    AgentDeps
        { adApiKeys = keys
        , adSessionSink = SinkNone
        , adContinuationStore = Nothing
        , adToolCache = Nothing
        , adCompletion = Nothing
        , adOnTextDelta = Nothing
        , adLiveParams = pure mempty
        }

-- | Dependencies that store sessions as files, as the CLI and TUI do.
fileAgentDeps :: SessionStore -> LoadedApiKeys -> AgentDeps
fileAgentDeps store keys = (defaultAgentDeps keys){adSessionSink = SinkFiles store}

-------------------------------------------------------------------------------
-- Building agents
-------------------------------------------------------------------------------

-- | Whether the agent runs a top-level conversation or is called by another agent.
data AgentRole
    = RootAgent
    | SubAgent
        { subParentConversation :: ConversationId
        -- ^ Conversation of the calling agent.
        , subCallStack :: [CallStackEntry]
        -- ^ Call stack including the entry for this sub-agent.
        }

{- | Build a runnable agent for a conversation.

The 'ConversationId' identifies the conversation for tool contexts and
file-based storage. A 'SinkBackend' keys sessions by their own 'SessionId',
and records the agent's slug and, for sub-agents, the parent session (the
parent's conversation ID read as a session ID).
-}
buildAgent ::
    Tracer IO Trace ->
    AgentDeps ->
    AgentRole ->
    ConversationId ->
    OSAgentNode ->
    IO (Agent (LlmTurnContent, Session))
buildAgent tracer deps role convId node = do
    let agentCfg = node.osNodeConfig
    let sPrompt = SystemPrompt $ Text.unlines $ Base.systemPrompt agentCfg
    sTools <- fmap toolRegistrationToSystemTool <$> readTVarIO node.osNodeTools
    resolvedParams <- readTVarIO node.osNodeParams
    exposeBindings <- readTVarIO node.osNodeExposeBindings
    completeF <- case deps.adCompletion of
        Just mkCompletion -> pure (mkCompletion node)
        Nothing -> openAICompletion tracer deps.adApiKeys deps.adOnTextDelta agentCfg
    let (callStack, parent) = case role of
            RootAgent -> ([CallStackEntry "root" convId 0], Nothing)
            SubAgent parentConv stack -> (stack, Just parentConv)
    let labels =
            SessionLabels
                { slAgent = Just (Base.slug agentCfg)
                , slParent = conversationIdToSessionId <$> parent
                , slOwner = Nothing
                }
    let base =
            Agent
                { step = naiveTilNoToolCallStep
                , sysPrompt = pure sPrompt
                , sysTools = pure sTools
                , usrQuery = pure Nothing
                , toolCall =
                    executeLlmToolCall
                        (contramap ToolRegistrationTrace tracer)
                        (readTVarIO node.osNodeTools)
                        (SessionCompat.parseToolCallFromLlmToolCall, SessionCompat.callResultToUserToolResponse)
                , toolPortal = ToolPortal.makeToolPortal (contramap ToolPortalTrace tracer) node.osNodeTools
                , complete = completeF
                , contextConfig = defaultContextConfig
                , ctxWorld = Nothing
                , ctxEventQueue = Nothing
                , ctxCallStack = callStack
                , ctxParentConversation = parent
                , ctxExecutionMode = Synchronous
                , ctxAsyncYieldStrategy = YieldWhenAllDone
                , ctxMaxConcurrency = Nothing
                , ctxAsyncCallTimeout = Nothing
                , ctxToolCache = deps.adToolCache
                , ctxToolCallPolicy = defaultToolCallPolicy
                , ctxToolExecutor = Nothing
                , ctxContinuationStore = deps.adContinuationStore
                , ctxDeploymentRunner = Nothing
                , ctxSessionBackend = sinkBackend deps.adSessionSink
                , ctxAsyncEngine = Nothing
                , ctxParams = resolvedParams
                , ctxInheritedBindings = []
                , ctxMailbox = Nothing
                , ctxMailRouter = Nothing
                , ctxSpawnSession = Nothing
                , ctxWatchSession = Nothing
                , ctxUnwatchSession = Nothing
                , ctxInterruptCompletions = False
                }
    disclosed <-
        agentEvaluateActiveTools
            (contramap mapProgressiveDisclosureTrace tracer)
            (fmap (`Map.union` resolvedParams) deps.adLiveParams)
            exposeBindings
            node.osNodeTools
            (applyAgentDurableConfig agentCfg base)
    pure $ agentPersistSession deps.adSessionSink labels convId disclosed
  where
    sinkBackend :: SessionSink -> Maybe SessionBackend
    sinkBackend (SinkBackend backend) = Just backend
    sinkBackend _ = Nothing

-- | The completion function for the agent's configured endpoint and API key.
openAICompletion :: Tracer IO Trace -> LoadedApiKeys -> Maybe (Text.Text -> IO ()) -> Base.Agent -> IO Completion
openAICompletion tracer keys onTextDelta agentCfg = do
    let mApiKey = snd <$> find ((== Base.apiKeyId agentCfg) . fst) keys
    httpRuntime <- case mApiKey of
        Just apiKey -> HttpClient.newRuntime (HttpClient.BearerToken $ Text.decodeUtf8 $ OpenAI.revealApiKey apiKey)
        Nothing -> HttpClient.newRuntime HttpClient.NoToken
    pure $
        mkOpenAICompletion
            OpenAICompletionConfig
                { cfgTracer = contramap OpenAITrace tracer
                , cfgRuntime = httpRuntime
                , cfgBaseUrl = OpenAI.ApiBaseUrl $ Base.modelUrl agentCfg
                , cfgModelName = Base.modelName agentCfg
                , cfgModelFlavor = Maybe.fromMaybe OpenAI.OpenAIv1 $ OpenAI.parseFlavor $ Base.flavor agentCfg
                , cfgOnTextDelta = onTextDelta
                }
