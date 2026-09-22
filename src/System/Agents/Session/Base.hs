{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.Session.Base (
    -- * Re-exported from Session.Types
    SessionId (..),
    TurnId (..),
    newSessionId,
    newTurnId,
    Session (..),
    newSessionFromPrompt,
    SessionStatus (..),
    sessionStatusOf,
    sessionStatusText,
    parseSessionStatus,
    isBlockedOnDeferredCalls,
    hasBackgroundCalls,
    backgroundCalls,
    DeferredCallView (..),
    pendingDeferredCalls,
    Turn (..),
    UserTurnContent (..),
    LlmTurnContent (..),
    PartialUserTurnContent (..),

    -- * Partial turn helpers
    partialCompletedResponses,
    partialToolMessages,
    partialPendingCalls,
    partialPendingContinuations,
    cacheKeyForTrackedCall,

    -- * Durable workflow primitives (re-exported from Session.Types)
    ToolCallId (..),
    newToolCallId,
    providerToolCallId,
    llmToolCallName,
    isFinalToolCallState,
    ToolCallState (..),
    ToolCallDisposition (..),
    IsolationSpec (..),
    Reason (..),
    Decorator (..),
    AppliedPolicy (..),
    TrackedToolCall (..),

    -- * Execution mode / continuation types
    ExecutionMode (..),
    ContinuationToken (..),
    newContinuationToken,
    CacheKey (..),
    AsyncYieldStrategy (..),

    -- * Session mailbox (re-exported from Session.Types / Session.Mailbox)
    Cursor,
    MessageId (..),
    newMessageId,
    messageIdText,
    Priority (..),
    Sender (..),
    ControlMsg (..),
    MailBody (..),
    Envelope (..),
    Outgoing (..),
    SendError (..),
    Receipt (..),
    Mailbox (..),
    newInMemoryMailbox,
    awaitMail,
    mailboxMaxUnread,
    MailStore (..),
    newDurableMailbox,
    MailboxInfo (..),
    MailRouter (..),
    newMailRouter,

    -- * Byte usage tracking
    StepByteUsage (..),
    calculateStepByteUsage,
    sessionTotalBytes,

    -- * Content types
    SystemPrompt (..),
    LlmResponse (..),
    LlmToolCall (..),
    UserQuery (..),
    UserToolResponse (..),

    -- * Tool definitions
    SystemTool (..),
    SystemToolDefinition (..),
    SystemToolDefinitionV1 (..),

    -- * Signal types (trajectory analysis)
    InteractionSignals (..),
    ExecutionSignals (..),
    EnvironmentSignals (..),
    TrajectorySignals (..),
    StepSignals (..),
    defaultInteractionSignals,
    defaultExecutionSignals,
    defaultEnvironmentSignals,
    defaultTrajectorySignals,

    -- * Session progress tracking
    SessionProgress (..),
    OnSessionProgress,
    ignoreSessionProgress,

    -- * Migration helpers
    migrateSessionV1ToV2,

    -- * Re-exports for convenience
    TokenUsage (..),

    -- * Async engine
    AsyncEngine (..),

    -- * Session backend (re-exported from SessionStore)
    SessionBackend (..),

    -- * Durable executor/policy/helpers (re-exported from Session.Durable)
    ToolExecutor (..),
    DeploymentRunner (..),
    IsolationError (..),
    IsolationEnvelope (..),
    IsolationResultEnvelope (..),
    IsolationResultStatus (..),
    ToolCallPolicy,
    defaultToolCallPolicy,
    inProcessExecutor,
    yieldingExecutor,
    cachingExecutor,
    cachedInProcessExecutor,
    composeExecutors,
    mkDurableExecutor,
    flattenDisposition,
    isolatedExecutor,
    AsyncToolResponse (..),
    Exec,
    ToolMiddleware,
    WrapperEnv (..),
    defaultWrapperEnv,
    interpretDecorator,
    applyDecorators,
    mkIsolationEnvelope,
    mkIsolationSuccessEnvelope,
    mkIsolationErrorEnvelope,
    parseIsolationResultEnvelope,
    parseIsolationResultEnvelopeLBS,
    localProcessRunner,
    dockerRunner,
    functionRunner,

    -- * Defined in this module
    MissingUserPrompt (..),
    LlmCompletion (..),
    Action (..),
    ContextConfig (..),
    defaultContextConfig,
    Agent (..),

    -- * Agent combinators
    withExecutionMode,
    withToolCache,
    withToolCallPolicy,
    withToolExecutor,
    withContinuationStore,
    withDeploymentRunner,
    withSessionBackend,
    withDurableWorkflows,
    withAsyncConfig,
    withDurableExecutor,
    withAsyncEngine,
    withAsyncYieldStrategy,
    withMailbox,
    withMailRouter,
    withSpawnSession,
    SpawnSession,
) where

import Control.Concurrent.STM (TQueue)

import System.Agents.Base (ConversationId)
import qualified System.Agents.OS.Conversation.ToolCalls as TCT
import System.Agents.OS.Core.World (World)
import System.Agents.OS.Events (OSEvent)
import System.Agents.Session.Async (ContinuationStore (..))
import System.Agents.Session.Async.Engine (AsyncEngine (..), mkAsyncEngine)
import System.Agents.Session.Mailbox (
    Mailbox (..),
    MailboxInfo (..),
    MailRouter (..),
    SpawnSession,
    newMailRouter,
    MailStore (..),
    awaitMail,
    mailboxMaxUnread,
    newDurableMailbox,
    newInMemoryMailbox,
 )
import System.Agents.Session.Durable (
    AsyncToolResponse (..),
    DeploymentRunner (..),
    Exec,
    IsolationEnvelope (..),
    IsolationError (..),
    IsolationResultEnvelope (..),
    IsolationResultStatus (..),
    ToolCallPolicy,
    ToolExecutor (..),
    ToolMiddleware,
    WrapperEnv (..),
    applyDecorators,
    cachingExecutor,
    cachedInProcessExecutor,
    composeExecutors,
    defaultToolCallPolicy,
    defaultWrapperEnv,
    dockerRunner,
    flattenDisposition,
    functionRunner,
    inProcessExecutor,
    interpretDecorator,
    isolatedExecutor,
    localProcessRunner,
    mkDurableExecutor,
    mkIsolationEnvelope,
    mkIsolationErrorEnvelope,
    mkIsolationSuccessEnvelope,
    parseIsolationResultEnvelope,
    parseIsolationResultEnvelopeLBS,
    yieldingExecutor,
 )
import System.Agents.SessionStore (SessionBackend (..))
import System.Agents.Tools.Cache (ToolCache (..))
import System.Agents.Tools.Context (CallStackEntry, ToolExecutionContext, ToolPortal)
import System.Agents.Tools.Bindings.Types (ScopedBinding)
import System.Agents.Tools.Params.Types (Params)
-- Re-export all session types from Session.Types for backward compatibility
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.Session.Types

-------------------------------------------------------------------------------
-- Action and Agent types
-------------------------------------------------------------------------------

data MissingUserPrompt = MissingUserPrompt
    { missingQuery :: Bool
    , missingToolCalls :: [LlmToolCall]
    }
    deriving (Show, Eq, Ord)

-- | LLM completion request with optional media attachments.
data LlmCompletion = LlmCompletion
    { completePrompt :: SystemPrompt
    , completeTools :: [SystemTool]
    , completeQuery :: Maybe UserQuery
    , completeToolResponses :: [(LlmToolCall, UserToolResponse)]
    , completeConversationHistory :: [Turn]
    , completeMedia :: [MediaAttachment]
    -- ^ Additional media attachments for multi-modal LLMs
    , completeSessionId :: Maybe SessionId
    -- ^ Optional session ID for prompt caching (used as prompt_cache_key)
    }
    deriving (Show, Eq, Ord)

data Action r
    = Stop r
    | AskUserPrompt MissingUserPrompt
    | AskLlmCompletion LlmCompletion
    | -- comfort/note fully-motivated below, is to evolve the agent so that th runner logic has a primitive to do so
      -- \* one advantage is it allows "pure" agents (i.e., dropping the need a IO in usrQuery et al.)
      -- \* could consider forking but that would require a joining function (r -> r -> r) to combine results, which prevents the functorial aspects
      -- \* could consider extensiblility so that agents come with their set of decisions, but the runloop then has to account for these
      Evolve (Agent r)
    deriving (Functor)

{- | Configuration for what to include in the tool execution context.

This allows fine-grained control over the context passed to tools,
enabling performance optimizations and privacy controls.

By default, minimal context is provided to avoid performance overhead.
-}
data ContextConfig = ContextConfig
    { includeFullSession :: Bool
    -- ^ Whether to include 'ctxFullSession'
    , includeAgentId :: Bool
    -- ^ Whether to include 'ctxAgentId'
    }
    deriving (Show, Eq)

{- | Default context configuration with minimal inclusion.

* 'includeFullSession' = False (sessions can grow large)
* 'includeAgentId' = True (lightweight and commonly useful)
-}
defaultContextConfig :: ContextConfig
defaultContextConfig = ContextConfig False True

{- | An agent is a decorated step function from a session step to an action that
may yield a result r or some delay.
Functions in its body.

Version 2 additions for async/resumable execution:
* 'ctxExecutionMode' - Controls sync vs async execution
* 'ctxToolCache' - Optional cache for tool results
* 'ctxToolCallPolicy' - Per-call execution policy
* 'ctxToolExecutor' - Optional pluggable tool executor
* 'ctxContinuationStore' - Optional durable continuation store
* 'ctxDeploymentRunner' - Optional isolated-deployment runner
* 'ctxSessionBackend' - Optional durable session storage backend
* 'ctxAsyncEngine' - Optional concurrent async execution engine
-}
data Agent r = Agent
    { step :: Session -> IO (Action r)
    , --
      sysPrompt :: IO SystemPrompt
    , sysTools :: IO [SystemTool]
    , usrQuery :: IO (Maybe UserQuery)
    , toolCall :: ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
    , toolPortal :: ToolPortal
    , --
      complete :: LlmCompletion -> IO (LlmResponse, [LlmToolCall])
    , --
      contextConfig :: ContextConfig
    -- ^ Configuration for what to include in tool execution context
    , --
      ctxWorld :: Maybe World
    {- ^ Optional OS World for ECS operations. When present, tools can
    insert entities and components into the OS. This enables subcall
    conversations to be visible in the TUI.
    -}
    , ctxEventQueue :: Maybe (TQueue OSEvent)
    {- ^ Optional event queue for OS event emission. When present, tools
    can emit events to notify the TUI of subcall lifecycle (start,
    progress, completion, failure).
    -}
    , ctxCallStack :: [CallStackEntry]
    {- ^ Call stack for tracking nested agent invocations. Root entry
    is at depth 0, and each nested call adds a new entry.
    -}
    , ctxParentConversation :: Maybe ConversationId
    {- ^ Optional parent conversation ID for subcalls. When present,
    indicates this agent is being used for a nested agent invocation,
    enabling proper lineage tracking in the OS.
    -}
    , ctxExecutionMode :: ExecutionMode
    {- ^ Execution mode: Synchronous (default) or Asynchronous.
    Async mode enables partial execution and session resumption.
    -}
    , ctxAsyncYieldStrategy :: AsyncYieldStrategy
    {- ^ Yield strategy for asynchronous tool execution.
    Defaults to 'YieldWhenAllDone' for backward compatibility.
    -}
    , ctxMaxConcurrency :: Maybe Int
    {- ^ Maximum number of concurrent async tool calls for the engine
    created on demand by asynchronous steps. 'Nothing' uses the default.
    -}
    , ctxAsyncCallTimeout :: Maybe Int
    {- ^ Seconds after which a background tool call is given up on and
    reported as failed. 'Nothing' lets calls run indefinitely.
    -}
    , ctxToolCache :: Maybe ToolCache
    {- ^ Optional tool cache for storing and retrieving tool results.
    Used in async mode to avoid re-executing cached tool calls.
    -}
    , ctxToolCallPolicy :: ToolCallPolicy
    {- ^ Per-call policy deciding sync / async / isolated / deferred.
    Defaults to 'defaultToolCallPolicy' (always RunSync).
    -}
    , ctxToolExecutor :: Maybe ToolExecutor
    {- ^ Optional pluggable tool executor. When present and execution
    mode is Asynchronous, the executor is used instead of the standard
    'toolCall' function. The loop selects sync vs async based on the
    policy disposition.
    -}
    , ctxContinuationStore :: Maybe ContinuationStore
    {- ^ Optional durable store for yielded continuations. When present,
    deferred tool calls are persisted so external workers can complete
    them.
    -}
    , ctxDeploymentRunner :: Maybe DeploymentRunner
    {- ^ Optional runner for isolated tool execution (Docker, subprocess,
    serverless). Used when the policy returns 'RunIsolated'.
    -}
    , ctxSessionBackend :: Maybe SessionBackend
    {- ^ Optional durable session storage backend. When present, progress
    callbacks and session persistence combinators store sessions via
    this backend, falling back to file storage when absent.
    -}
    , ctxAsyncEngine :: Maybe AsyncEngine
    {- ^ Optional concurrent async execution engine. When present,
    'RunAsync' calls are executed concurrently in background threads
    and their OS entity lifecycle is kept in sync.
    -}
    , ctxParams :: Params
    {- ^ This agent's parameters, resolved against process-level (and,
    later, session/message-level) values (see
    @todos/tool-partial-application.md@). Copied into every tool call's
    'ToolExecutionContext' by 'buildContext'.
    -}
    , ctxInheritedBindings :: [ScopedBinding]
    {- ^ Bindings an ancestor placed on one of this agent's own helpers, or
    on this agent itself (@todos/tool-partial-application.md@, §8.3).
    Copied into every tool call's 'ToolExecutionContext' by 'buildContext',
    same as 'ctxParams'.
    -}
    , ctxMailbox :: Maybe Mailbox
    {- ^ Optional mailbox (@todos/session-mailbox.md@, Phase 1). When
    present, the receive points in "System.Agents.Session.Step" fold unread
    mail into the user turn being built (R1), and block on it when the step
    has nothing else to do (R2). 'Nothing' keeps every existing code path
    (including 'usrQuery') unchanged.
    -}
    , ctxMailRouter :: Maybe MailRouter
    {- ^ Optional process-wide table of live mailboxes (@todos/session-mailbox.md@,
    Phase 4, D12). When present, 'System.Agents.Tools.SystemToolbox' capabilities
    like @send-message@ can address another session by id. Handed down to
    sub-agents the same way 'ctxWorld' \/ 'ctxEventQueue' are.
    -}
    , ctxSpawnSession :: Maybe SpawnSession
    {- ^ Optional @spawn-session@ hook (@todos/session-mailbox.md@, Phase 4,
    §5): given a helper's agent slug and an initial message, start it
    running as a detached child session (not call\/return -- it outlives
    the tool call that spawned it and answers by mail) and return its new
    'SessionId' at once. Front-end-specific (durable\/'createSessionAs'-like
    on the server; a new conversation in the TUI; a thread in @run@), so it
    is a hook rather than a shared implementation, the same shape as
    'ctxCancelToolCall' on 'ToolExecutionContext'. Handed down to sub-agents
    the same way 'ctxMailRouter' is.
    -}
    }
    deriving (Functor)

-------------------------------------------------------------------------------
-- Agent Combinators
-------------------------------------------------------------------------------

{- | Set the async yield strategy for an agent.

Example:

@
yieldingAgent = withAsyncYieldStrategy YieldOnAnyProgress baseAgent
@
-}
withAsyncYieldStrategy :: AsyncYieldStrategy -> Agent r -> Agent r
withAsyncYieldStrategy strategy agent = agent{ctxAsyncYieldStrategy = strategy}

{- | Install a mailbox on an agent (@todos/session-mailbox.md@, Phase 1).

Example:

@
mb <- newInMemoryMailbox
mailboxAgent = withMailbox mb baseAgent
@
-}
withMailbox :: Mailbox -> Agent r -> Agent r
withMailbox mb agent = agent{ctxMailbox = Just mb}

{- | Install a mail router on an agent (@todos/session-mailbox.md@, Phase 4).

Example:

@
router <- newMailRouter
routedAgent = withMailRouter router baseAgent
@
-}
withMailRouter :: MailRouter -> Agent r -> Agent r
withMailRouter router agent = agent{ctxMailRouter = Just router}

{- | Install a @spawn-session@ hook on an agent (@todos/session-mailbox.md@,
Phase 4).
-}
withSpawnSession :: SpawnSession -> Agent r -> Agent r
withSpawnSession spawn agent = agent{ctxSpawnSession = Just spawn}

{- | Set the execution mode for an agent.

Example:

@
asyncAgent = withExecutionMode Asynchronous baseAgent
@
-}
withExecutionMode :: ExecutionMode -> Agent r -> Agent r
withExecutionMode mode agent = agent{ctxExecutionMode = mode}

{- | Add a tool cache to an agent.

Example:

@
cache <- mkSqliteToolCache ".agents-cache.db"
cachedAgent = withToolCache agent cache
@
-}
withToolCache :: Agent r -> ToolCache -> Agent r
withToolCache agent cache = agent{ctxToolCache = Just cache}

{- | Set a custom tool-call policy.

Example:

@
policyAgent = withToolCallPolicy myPolicy agent
@
-}
withToolCallPolicy :: ToolCallPolicy -> Agent r -> Agent r
withToolCallPolicy policy agent = agent{ctxToolCallPolicy = policy}

{- | Set a custom tool executor.

Example:

@
execAgent = withToolExecutor customExecutor agent
@
-}
withToolExecutor :: ToolExecutor -> Agent r -> Agent r
withToolExecutor executor agent = agent{ctxToolExecutor = Just executor}

{- | Set a continuation store for durable async execution.

Example:

@
store <- mkSqliteContinuationStore conn
storedAgent = withContinuationStore store agent
@
-}
withContinuationStore :: ContinuationStore -> Agent r -> Agent r
withContinuationStore store agent = agent{ctxContinuationStore = Just store}

{- | Set a deployment runner for isolated tool execution.

Example:

@
runner <- dockerRunner "agents-exe/runner"
isoAgent = withDeploymentRunner runner agent
@
-}
withDeploymentRunner :: DeploymentRunner -> Agent r -> Agent r
withDeploymentRunner runner agent = agent{ctxDeploymentRunner = Just runner}

{- | Set a durable session storage backend on an agent.

Example:

@
backend <- mkSqliteSessionStore conn
durableAgent = withSessionBackend backend agent
@
-}
withSessionBackend :: SessionBackend -> Agent r -> Agent r
withSessionBackend backend agent = agent{ctxSessionBackend = Just backend}

{- | Set a durable session backend and continuation store in one call.

Convenience combinator for the common durable-workflow setup where both
session state and yielded continuations should be persisted.

Example:

@
backend  <- mkSqliteSessionStore conn
store    <- mkSqliteContinuationStore conn
durableAgent = withDurableWorkflows backend store agent
@
-}
withDurableWorkflows :: SessionBackend -> ContinuationStore -> Agent r -> Agent r
withDurableWorkflows backend store agent =
    agent
        { ctxSessionBackend = Just backend
        , ctxContinuationStore = Just store
        }

{- | Configure the asynchronous execution settings for an agent in one call.

Sets the execution mode, optional tool cache, and tool-call policy. This is
useful when switching an agent from the default synchronous mode to a
durable, policy-driven async mode.

Example:

@
asyncAgent = withAsyncConfig Asynchronous (Just cache) myPolicy agent
@
-}
withAsyncConfig :: ExecutionMode -> Maybe ToolCache -> ToolCallPolicy -> Agent r -> Agent r
withAsyncConfig mode mCache policy agent =
    agent
        { ctxExecutionMode = mode
        , ctxAsyncYieldStrategy = YieldWhenAllDone
        , ctxToolCache = mCache
        , ctxToolCallPolicy = policy
        }

{- | Configure a durable executor for an agent in one call.

This combines an optional tool cache and an optional deployment runner into
a 'ToolExecutor' and installs it on the agent. The resulting executor:

* Looks up results in the cache when a cache is provided.
* Delegates 'RunIsolated' calls to the deployment runner when a runner is
  provided.
* Falls back to the agent's native 'toolCall' function for everything else.

Example:

@
cache  <- mkSqliteToolCache ".cache.db"
runner <- dockerRunner "agents-exe/runner"
agent' = withDurableExecutor cache runner agent
@
-}
withDurableExecutor ::
    Maybe ToolCache ->
    Maybe DeploymentRunner ->
    Agent r ->
    Agent r
withDurableExecutor mCache mRunner agent =
    agent
        { ctxToolExecutor =
            Just $
                mkDurableExecutor
                    agent.ctxToolCallPolicy
                    mCache
                    mRunner
                    agent.toolCall
        }

{- | Install (or replace) a concurrent async engine on the agent.

The engine is created from the agent's OS 'World' and native tool-call
function. If the agent does not have a 'World', this combinator has no
effect.

Asynchronous steps install an engine on demand, but the loops only keep it
for the duration of one call. Install it up front when a session is paused
and resumed with the same agent, so resumed steps can still cancel calls
started earlier and share the concurrency limit.

Example:

@
asyncAgent <- withAsyncEngine 4 baseAgent
@
-}
withAsyncEngine :: Int -> Agent r -> IO (Agent r)
withAsyncEngine maxConcurrency agent =
    case agent.ctxWorld of
        Nothing -> pure agent
        Just world0 -> do
            -- Register the tool-call stores first so the engine and the
            -- agent share the same world value.
            world <- TCT.ensureToolCallComponentsIO world0
            engine <- mkAsyncEngine world (executeCallSync agent) maxConcurrency agent.ctxAsyncCallTimeout agent.ctxMailbox
            pure agent{ctxWorld = Just world, ctxAsyncEngine = Just engine}

{- | Execute a call synchronously through the agent's executor (or its
native 'toolCall'), applying the decorators its resolved policy carries.

Mirrors 'System.Agents.Session.Step.executeCall' (minus the
'DeploymentRunner' fallback, which 'ctxToolExecutor' already covers for
every agent built through 'withDurableExecutor'). It lives here, rather
than being shared with that module, because 'Step' imports this module and
a shared definition would make the two modules depend on each other.
-}
executeCallSync :: Agent r -> ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
executeCallSync agent ctx call = do
    response <- applyDecorators wrapperEnv decorators baseExec ctx call
    case response of
        ToolComplete result -> pure result
        ToolYield{} -> pure $ TextResponse "tool call yielded unexpectedly under a synchronous executor"
  where
    wrapperEnv = WrapperEnv{weCache = agent.ctxToolCache}
    (decorators, _base) = flattenDisposition (agent.ctxToolCallPolicy ctx call)
    baseExec ctx' call' = ToolComplete <$> baseExecSync ctx' call'
    baseExecSync ctx' call' =
        case agent.ctxToolExecutor of
            Just executor -> executor.execSync ctx' call'
            Nothing -> agent.toolCall ctx' call'

