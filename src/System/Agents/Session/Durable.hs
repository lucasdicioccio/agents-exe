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
    cachedInProcessExecutor,
    composeExecutors,
    mkDurableExecutor,

    -- * Isolated execution
    DeploymentRunner (..),
    IsolationError (..),
    IsolationSpec (..),

    -- * Envelope types and helpers (re-exported from Isolation)
    IsolationEnvelope (..),
    IsolationResultEnvelope (..),
    IsolationResultStatus (..),
    mkIsolationEnvelope,
    mkIsolationSuccessEnvelope,
    mkIsolationErrorEnvelope,
    parseIsolationResultEnvelope,
    parseIsolationResultEnvelopeLBS,

    -- * Concrete runners (re-exported from Isolation)
    localProcessRunner,
    dockerRunner,
    functionRunner,
) where

import Data.Maybe (listToMaybe)
import Data.Time (getCurrentTime)

import System.Agents.Session.Async (AsyncToolResponse (..), newContinuationToken)
import System.Agents.Session.Isolation (
    DeploymentRunner (..),
    IsolationEnvelope (..),
    IsolationError (..),
    IsolationResultEnvelope (..),
    IsolationResultStatus (..),
    dockerRunner,
    functionRunner,
    localProcessRunner,
    mkIsolationEnvelope,
    mkIsolationErrorEnvelope,
    mkIsolationSuccessEnvelope,
    parseIsolationResultEnvelope,
    parseIsolationResultEnvelopeLBS,
 )
import System.Agents.Session.Types (
    Decorator (..),
    IsolationSpec (..),
    LlmToolCall (..),
    ToolCallDisposition (..),
    UserToolResponse (..),
 )
import System.Agents.Tools.Cache (CachedResult (..), ToolCache (..))
import qualified System.Agents.Tools.Cache as Cache
import System.Agents.Tools.Context (ToolExecutionContext, contextSnapshot)

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

{- | In-process executor with caching.

Convenience wrapper equivalent to @cachingExecutor cache (inProcessExecutor runSync)@.
Useful when an agent wants to add a cache to its native 'toolCall' without
manually constructing the inner executor.
-}
cachedInProcessExecutor ::
    ToolCache ->
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    ToolExecutor
cachedInProcessExecutor cache runSync =
    cachingExecutor cache (inProcessExecutor runSync)

{- | Compose a list of conditional executors over a fallback executor.

The provided policy is evaluated to obtain the flattened disposition for
each call. The first predicate that matches the base disposition wins;
otherwise execution falls through to the provided fallback executor. This
makes it easy to assemble complex executors (e.g., "bash calls go to Docker,
everything else runs in-process") without writing a custom 'ToolExecutor' by
hand.

Example:

@
let isIsolated base = case base of RunIsolated _ -> True; _ -> False
composeExecutors policy
    [ (isIsolated, isolatedExecutor policy runner (inProcessExecutor runSync))
    ]
    (inProcessExecutor runSync)
@
-}
composeExecutors ::
    ToolCallPolicy ->
    [(ToolCallDisposition -> Bool, ToolExecutor)] ->
    ToolExecutor ->
    ToolExecutor
composeExecutors policy branches fallback =
    ToolExecutor
        { execSync = \ctx call -> do
            let (_decorators, base) = flattenDisposition (policy ctx call)
            case lookupBranch base of
                Just executor -> executor.execSync ctx call
                Nothing -> fallback.execSync ctx call
        , execAsync = \ctx call -> do
            let (_decorators, base) = flattenDisposition (policy ctx call)
            case lookupBranch base of
                Just executor -> executor.execAsync ctx call
                Nothing -> fallback.execAsync ctx call
        }
  where
    lookupBranch base =
        listToMaybe [executor | (pred', executor) <- branches, pred' base]

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

{- | Executor that delegates isolated calls to a 'DeploymentRunner'.

The provided policy is re-evaluated for each call to recover the
isolation specification. A fresh continuation token and an
'IsolationEnvelope' are built for every isolated call so that external
workers receive a stable, language-agnostic input document.
-}
isolatedExecutor :: ToolCallPolicy -> DeploymentRunner -> ToolExecutor -> ToolExecutor
isolatedExecutor policy runner inner =
    ToolExecutor
        { execSync = \ctx call -> dispatch ctx call inner.execSync
        , execAsync = \ctx call -> dispatchAsync ctx call
        }
  where
    dispatch ctx call fallback = do
        let (_decorators, base) = flattenDisposition $ policy ctx call
        case base of
            RunIsolated _spec -> runIsolated base ctx call
            _ -> fallback ctx call

    dispatchAsync ctx call = do
        let (_decorators, base) = flattenDisposition $ policy ctx call
        case base of
            RunIsolated _spec -> ToolComplete <$> runIsolated base ctx call
            _ -> inner.execAsync ctx call

    runIsolated disp ctx call = do
        token <- newContinuationToken
        let envelope = mkIsolationEnvelope token call (contextSnapshot ctx) disp Nothing
        result <- drExecute runner envelope
        pure $ either (TextResponse . ("isolation error: " <>) . (\(IsolationError e) -> e)) id result

-------------------------------------------------------------------------------
-- Durable executor construction
-------------------------------------------------------------------------------

{- | Build a durable executor from the agent's native tool-call function.

The resulting executor:

* Looks up results in the optional cache first.
* Delegates 'RunIsolated' calls to the optional deployment runner.
* Falls back to the native 'toolCall' for everything else.

This is the executor used by 'withDurableExecutor' and mirrors the default
logic in 'System.Agents.Session.Step.executeCall', but packaged as a reusable
'ToolExecutor'.
-}
mkDurableExecutor ::
    ToolCallPolicy ->
    Maybe ToolCache ->
    Maybe DeploymentRunner ->
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    ToolExecutor
mkDurableExecutor policy mCache mRunner runSync =
    let base = inProcessExecutor runSync
        cached = maybe base (\cache -> cachingExecutor cache base) mCache
        isolated = maybe cached (\runner -> isolatedExecutor policy runner cached) mRunner
     in isolated

