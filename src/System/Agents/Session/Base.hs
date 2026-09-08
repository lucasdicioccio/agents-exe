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
    Turn (..),
    UserTurnContent (..),
    LlmTurnContent (..),
    PartialUserTurnContent (..),
    SystemPrompt (..),
    LlmResponse (..),
    LlmToolCall (..),
    UserQuery (..),
    UserToolResponse (..),
    SystemTool (..),
    SystemToolDefinition (..),
    SystemToolDefinitionV1 (..),
    SessionProgress (..),
    OnSessionProgress,
    ignoreSessionProgress,
    ExecutionMode (..),
    ContinuationToken (..),
    newContinuationToken,
    ContinuationStore (..),
    CacheKey (..),
    migrateSessionV1ToV2,

    -- * Durable workflow primitives (re-exported from Session.Types)
    ToolCallId (..),
    newToolCallId,
    ToolCallState (..),
    ToolCallDisposition (..),
    IsolationSpec (..),
    Reason (..),
    Decorator (..),
    AppliedPolicy (..),
    TrackedToolCall (..),

    -- * Partial turn helpers (re-exported from Session.Types)
    partialCompletedResponses,
    partialPendingCalls,
    partialPendingContinuations,

    -- * Durable executor/policy helpers (re-exported from Session.Durable)
    ToolExecutor (..),
    DeploymentRunner (..),
    IsolationError (..),
    ToolCallPolicy,
    defaultToolCallPolicy,
    inProcessExecutor,
    yieldingExecutor,
    cachingExecutor,
    isolatedExecutor,
    flattenDisposition,

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
) where

import Control.Concurrent.STM (TQueue)

import System.Agents.Base (ConversationId)
import System.Agents.OS.Core.World (World)
import System.Agents.OS.Events (OSEvent)
import System.Agents.Session.Async (ContinuationStore (..))
import System.Agents.Session.Durable (
    DeploymentRunner (..),
    IsolationError (..),
    ToolCallPolicy,
    ToolExecutor (..),
    cachingExecutor,
    defaultToolCallPolicy,
    flattenDisposition,
    inProcessExecutor,
    isolatedExecutor,
    yieldingExecutor,
 )
import System.Agents.Tools.Cache (ToolCache (..))
import System.Agents.Tools.Context (CallStackEntry, ToolExecutionContext, ToolPortal)
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
      -- \* one advantage is it allows "pure" agents (i.e., dropping the need for a IO in usrQuery et al.)
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
    }
    deriving (Functor)

-------------------------------------------------------------------------------
-- Agent Combinators
-------------------------------------------------------------------------------

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

