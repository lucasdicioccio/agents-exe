{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Durable workflow primitives for agent sessions.

This module defines the policy and executor interfaces introduced in
Phase 1 of the durable-workflows plan. It separates *how* a tool call
is dispatched from *what* the session loop does with it:

* 'ToolCallPolicy' decides, per call, whether to run synchronously,
  asynchronously, in isolation, or defer.
* 'ToolExecutor' abstracts the actual execution mechanism.
* 'DeploymentRunner' abstracts isolated execution (Docker, subprocess,
  serverless).

The design goal is to make synchronous / asynchronous / isolated tool
execution transparent to the agent core.
-}
module System.Agents.Session.Durable (
    -- * Policy
    ToolCallPolicy,
    ToolCallDisposition (..),
    defaultToolCallPolicy,
    flattenDisposition,

    -- * Executor interface
    ToolExecutor (..),
    inProcessExecutor,
    yieldingExecutor,
    cachingExecutor,
    isolatedExecutor,

    -- * Isolated execution
    DeploymentRunner (..),
    IsolationError (..),
    IsolationSpec (..),
) where

import Data.Text (Text)
import Data.Time (getCurrentTime)

import System.Agents.Session.Async (AsyncToolResponse (..), newContinuationToken)
import System.Agents.Session.Types (
    Decorator (..),
    IsolationSpec (..),
    LlmToolCall (..),
    ToolCallDisposition (..),
    UserToolResponse (..),
 )
import System.Agents.Tools.Cache (CachedResult (..), ToolCache (..))
import qualified System.Agents.Tools.Cache as Cache
import System.Agents.Tools.Context (ToolExecutionContext)

-------------------------------------------------------------------------------
-- Policy
-------------------------------------------------------------------------------

{- | Pure decision primitive for a single tool call.

Given the execution context and the LLM-issued call, return a
'ToolCallDisposition' describing how the runtime should handle it.

The default policy ('defaultToolCallPolicy') runs everything synchronously
for backward compatibility.
-}
type ToolCallPolicy =
    ToolExecutionContext -> LlmToolCall -> ToolCallDisposition

-- | Default policy: execute every tool call synchronously in-process.
defaultToolCallPolicy :: ToolCallPolicy
defaultToolCallPolicy = const $ const RunSync

{- | Flatten a possibly-decorated disposition into its decorators and base.

Useful when serializing an 'AppliedPolicy' or when an executor wants to
handle decorators (timeouts, retries, labels) separately from the core
decision.
-}
flattenDisposition :: ToolCallDisposition -> ([Decorator], ToolCallDisposition)
flattenDisposition disp = case disp of
    Decorate decorators inner ->
        let (more, base) = flattenDisposition inner
         in (decorators ++ more, base)
    other -> ([], other)

-------------------------------------------------------------------------------
-- Executor interface
-------------------------------------------------------------------------------

{- | Pluggable tool-call executor.

Decouples *how* a tool call runs from the session loop. The loop
selects an executor based on the 'ToolCallDisposition' produced by the
policy.
-}
data ToolExecutor = ToolExecutor
    { execSync :: ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
    -- ^ Execute a call synchronously and return its result.
    , execAsync :: ToolExecutionContext -> LlmToolCall -> IO AsyncToolResponse
    -- ^ Execute a call in async mode. May complete immediately or yield.
    }

{- | In-process executor.

Runs the call using the provided synchronous function. The async
variant always returns 'ToolComplete'.
-}
inProcessExecutor ::
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    ToolExecutor
inProcessExecutor runSync =
    ToolExecutor
        { execSync = runSync
        , execAsync = \ctx call -> ToolComplete <$> runSync ctx call
        }

{- | Yielding executor.

Always returns 'ToolYield' with a fresh continuation token. Useful for
testing durable-workflow flows or for policies that want every call to
pause.
-}
yieldingExecutor :: ToolExecutor
yieldingExecutor =
    ToolExecutor
        { execSync = \_ _ -> pure $ TextResponse "yielding executor does not support sync execution"
        , execAsync = \_ _ -> do
            token <- newContinuationToken
            let key = Cache.CacheKey "yield" "yield"
            pure $ ToolYield token key
        }

{- | Caching executor wrapper.

Looks up calls in the cache before delegating to the underlying executor.
On cache miss, delegates and stores the result. The wrapper applies to
both 'execSync' and 'execAsync'.
-}
cachingExecutor :: ToolCache -> ToolExecutor -> ToolExecutor
cachingExecutor cache inner =
    ToolExecutor
        { execSync = \ctx call -> do
            let key = Cache.computeCacheKey call
            mCached <- cache.cacheLookup key
            case mCached of
                Just cached -> pure cached.crResult
                Nothing -> do
                    result <- inner.execSync ctx call
                    now <- getCurrentTime
                    cache.cacheStore key $ CachedResult result now Nothing
                    pure result
        , execAsync = \ctx call -> do
            let key = Cache.computeCacheKey call
            mCached <- cache.cacheLookup key
            case mCached of
                Just cached -> pure $ ToolComplete cached.crResult
                Nothing -> do
                    response <- inner.execAsync ctx call
                    case response of
                        ToolComplete result -> do
                            now <- getCurrentTime
                            cache.cacheStore key $ CachedResult result now Nothing
                            pure response
                        ToolYield{} -> pure response
        }

-------------------------------------------------------------------------------
-- Isolated execution
-------------------------------------------------------------------------------

{- | Error produced by an isolated execution runner.
-}
newtype IsolationError = IsolationError Text
    deriving (Show, Eq)

{- | Runner for tool calls outside the current process.

Abstracts over Docker, subprocess workers, and future serverless targets.
The runner receives the isolation specification and the original tool call
and must return either an error or a 'UserToolResponse'.
-}
data DeploymentRunner = DeploymentRunner
    { drName :: Text
    -- ^ Human-readable runner name (e.g., "docker", "local-process")
    , drExecute :: IsolationSpec -> LlmToolCall -> IO (Either IsolationError UserToolResponse)
    }

{- | Executor that delegates isolated calls to a 'DeploymentRunner'.

The provided policy is re-evaluated for each call to recover the
'IsolationSpec'. This keeps the 'ToolExecutor' interface unchanged while
still allowing per-call isolation decisions.
-}
isolatedExecutor :: ToolCallPolicy -> DeploymentRunner -> ToolExecutor -> ToolExecutor
isolatedExecutor policy runner inner =
    ToolExecutor
        { execSync = \ctx call -> dispatch ctx call inner.execSync
        , execAsync = \ctx call -> dispatchAsync ctx call
        }
  where
    dispatch ctx call fallback = do
        let (decorators, base) = flattenDisposition $ policy ctx call
        case base of
            RunIsolated spec -> runIsolated decorators spec ctx call
            _ -> fallback ctx call

    dispatchAsync ctx call = do
        let (decorators, base) = flattenDisposition $ policy ctx call
        case base of
            RunIsolated spec -> ToolComplete <$> runIsolated decorators spec ctx call
            _ -> inner.execAsync ctx call

    runIsolated _decorators spec _ctx call = do
        result <- drExecute runner spec call
        pure $ either (TextResponse . ("isolation error: " <>) . (\(IsolationError e) -> e)) id result

