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

    -- * Decorators / wrappers (Phase 0 of the session-mailbox spec)
    AsyncToolResponse (..),
    Exec,
    ToolMiddleware,
    WrapperEnv (..),
    defaultWrapperEnv,
    interpretDecorator,
    applyDecorators,

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

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (getCurrentTime)
import System.Timeout (timeout)

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
    CacheKey (..),
    Decorator (..),
    IsolationSpec (..),
    LlmToolCall (..),
    ToolCallDisposition (..),
    UserToolResponse (..),
 )
import System.Agents.Tools.Cache (CachedResult (..), ToolCache (..))
import qualified System.Agents.Tools.Cache as Cache
import System.Agents.Tools.Context (ToolExecutionContext (..), contextSnapshot)

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
-- Decorators / wrappers
-------------------------------------------------------------------------------

{- | A tool execution, in the form every decorator wraps.

'AsyncToolResponse' rather than 'UserToolResponse' so that a future
decorator (a @before@ hook, Phase 5) can answer 'ToolYield' without
blocking the caller.
-}
type Exec = ToolExecutionContext -> LlmToolCall -> IO AsyncToolResponse

-- | A decorator, once interpreted, wraps an 'Exec' into another 'Exec'.
type ToolMiddleware = Exec -> Exec

{- | Environment available to 'interpretDecorator'.

Carries the pieces a decorator needs that are not visible on the call
itself: a tool cache for 'WithCache' to look up and store into.
-}
newtype WrapperEnv = WrapperEnv
    { weCache :: Maybe ToolCache
    -- ^ Cache backing 'WithCache'. Without one, 'WithCache' is a no-op.
    }

-- | A 'WrapperEnv' with no cache configured.
defaultWrapperEnv :: WrapperEnv
defaultWrapperEnv = WrapperEnv{weCache = Nothing}

{- | Interpret one 'Decorator' as 'ToolMiddleware'.

* 'WithTimeout' races the call against the timeout; on expiry it completes
  with a 'TextResponse' explaining the timeout rather than propagating an
  exception, so a timed-out call still looks like a normal tool result to
  the LLM.
* 'WithRetries' retries on any exception raised by the wrapped call, up to
  the given count, reporting each retry through 'ctxProgressCallback' when
  one is configured.
* 'WithCache' looks up 'weCache' by the decorator's explicit 'CacheKey'
  before running the call, and stores a completed result under that key.
  A yielded result is never cached (there is nothing to store yet).
* 'WithLabel' is metadata for observability; it does not change execution.
* 'WithTruncate' caps a completed text\/JSON result to at most the given
  number of bytes, noting the cut to the model. Media and mixed results are
  passed through unchanged.
-}
interpretDecorator :: WrapperEnv -> Decorator -> ToolMiddleware
interpretDecorator env dec next ctx call = case dec of
    WithLabel _label -> next ctx call
    WithTimeout seconds -> do
        result <- timeout (max 0 seconds * 1000000) (next ctx call)
        pure $ case result of
            Just response -> response
            Nothing ->
                ToolComplete $
                    TextResponse $
                        "tool call timed out after " <> Text.pack (show seconds) <> "s"
    WithRetries count -> retryLoop (max 0 count)
    -- 'Decorator's 'CacheKey' (Session.Types) and 'ToolCache's 'CacheKey'
    -- (Tools.Cache) are distinct, identically-shaped types; convert.
    WithCache key -> case env.weCache of
        Nothing -> next ctx call
        Just cache -> do
            let cacheKey = Cache.CacheKey key.ckToolName key.ckArgumentsHash
            mCached <- cache.cacheLookup cacheKey
            case mCached of
                Just cached -> pure $ ToolComplete cached.crResult
                Nothing -> do
                    response <- next ctx call
                    case response of
                        ToolComplete result -> do
                            now <- getCurrentTime
                            cache.cacheStore cacheKey $ CachedResult result now Nothing
                            pure response
                        ToolYield{} -> pure response
    WithTruncate maxBytes -> truncateResponse maxBytes <$> next ctx call
  where
    retryLoop attemptsLeft = do
        outcome <- try (next ctx call) :: IO (Either SomeException AsyncToolResponse)
        case outcome of
            Right response -> pure response
            Left err
                | attemptsLeft <= 0 ->
                    pure $ ToolComplete $ TextResponse $ "tool call failed: " <> Text.pack (show err)
                | otherwise -> do
                    reportRetry ctx attemptsLeft
                    retryLoop (attemptsLeft - 1)

-- | Report an in-progress retry through the context's progress callback, if any.
reportRetry :: ToolExecutionContext -> Int -> IO ()
reportRetry ctx attemptsLeft =
    case ctx.ctxProgressCallback of
        Nothing -> pure ()
        Just report ->
            report $
                Aeson.object
                    [ "event" Aeson..= ("retrying" :: Text)
                    , "attemptsLeft" Aeson..= attemptsLeft
                    ]

-- | Cap a completed response to at most 'maxBytes'; pass a yielded response through.
truncateResponse :: Int -> AsyncToolResponse -> AsyncToolResponse
truncateResponse maxBytes response = case response of
    ToolComplete result -> ToolComplete (truncateUserToolResponse maxBytes result)
    ToolYield{} -> response

-- | Cap 'TextResponse'\/'JsonResponse' payloads; leave media\/mixed untouched.
truncateUserToolResponse :: Int -> UserToolResponse -> UserToolResponse
truncateUserToolResponse maxBytes result = case result of
    TextResponse txt
        | BS.length (TE.encodeUtf8 txt) > maxBytes -> TextResponse (truncateText maxBytes txt)
        | otherwise -> result
    JsonResponse val ->
        let encoded = TE.decodeUtf8With TEE.lenientDecode $ LBS.toStrict $ Aeson.encode val
         in if BS.length (TE.encodeUtf8 encoded) > maxBytes
                then TextResponse (truncateText maxBytes encoded)
                else result
    other -> other

-- | Cut a 'Text' down to at most 'maxBytes' UTF-8 bytes, noting the cut.
truncateText :: Int -> Text -> Text
truncateText maxBytes txt =
    TE.decodeUtf8With TEE.lenientDecode (BS.take maxBytes (TE.encodeUtf8 txt))
        <> "\n[truncated: result exceeded "
        <> Text.pack (show maxBytes)
        <> " bytes]"

{- | Compose the given decorators, outermost first, around an 'Exec'.

@applyDecorators env [d1, d2] base@ behaves as @d1 (d2 base)@: the first
decorator in the list is the outermost, matching how 'flattenDisposition'
orders decorators from a (possibly nested) 'Decorate'.
-}
applyDecorators :: WrapperEnv -> [Decorator] -> Exec -> Exec
applyDecorators env decorators base = foldr (interpretDecorator env) base decorators

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

