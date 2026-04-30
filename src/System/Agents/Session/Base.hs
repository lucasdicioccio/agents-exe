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
    CacheKey (..),
    migrateSessionV1ToV2,

    -- * Defined in this module
    MissingUserPrompt (..),
    LlmCompletion (..),
    Action (..),
    ContextConfig (..),
    defaultContextConfig,
    Agent (..),

    -- * Async execution helpers
    AsyncToolCallFn,
    defaultAsyncExecutor,

    -- * Agent combinators
    withExecutionMode,
    withToolCache,
    withAsyncToolCall,
) where

import Control.Concurrent.STM (TQueue)

import System.Agents.Base (ConversationId)
import System.Agents.OS.Core.World (World)
import System.Agents.OS.Events (OSEvent)
import System.Agents.Session.Async (AsyncToolResponse (..))
import System.Agents.Tools.Cache (ToolCache (..))
import System.Agents.Tools.Context (CallStackEntry, ToolExecutionContext, ToolPortal)

-- Re-export all session types from Session.Types for backward compatibility
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.Session.Types

-------------------------------------------------------------------------------
-- Async Execution Types
-------------------------------------------------------------------------------

{- | Type alias for async-aware tool execution function.

In async mode, tool calls can either complete immediately or yield
for external completion.
-}
type AsyncToolCallFn =
    ToolExecutionContext
    -> LlmToolCall
    -> IO AsyncToolResponse

{- | Default async executor that simply calls the synchronous tool executor.

This provides backward compatibility - when no special async handling
is needed, tools execute synchronously as before.
-}
defaultAsyncExecutor :: (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) -> AsyncToolCallFn
defaultAsyncExecutor syncExec ctx call = do
    result <- syncExec ctx call
    pure $ ToolComplete result

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
* 'ctxAsyncToolCall' - Async-aware tool executor
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
    , ctxAsyncToolCall :: Maybe AsyncToolCallFn
    {- ^ Optional async-aware tool executor. When present and execution
    mode is Asynchronous, this function is used instead of the
    standard toolCall function.
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

{- | Set a custom async tool call executor.

Example:

@
asyncAgent = withAsyncToolCall customAsyncExecutor agent
@
-}
withAsyncToolCall :: Agent r -> AsyncToolCallFn -> Agent r
withAsyncToolCall agent asyncFn = agent{ctxAsyncToolCall = Just asyncFn}

