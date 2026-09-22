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
    mkToolInvoker,

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

import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Exception (SomeException, evaluate, try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (diffUTCTime, getCurrentTime)
import System.Exit (ExitCode (..))
import System.IO (hClose, hSetBinaryMode)
import System.Process (StdStream (..), createProcess, proc, std_err, std_in, std_out, waitForProcess)
import System.Timeout (timeout)

import System.Agents.OS.Events (OSEvent (..))
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
    HookTarget (..),
    IsolationSpec (..),
    LlmToolCall (..),
    ToolCallDisposition (..),
    UserToolResponse (..),
    llmToolCallName,
    providerToolCallId,
 )
import System.Agents.Tools.Cache (CachedResult (..), ToolCache (..), extractToolInfo)
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
itself: a tool cache for 'WithCache' to look up and store into, and (Phase
5) a way to invoke a tool by name for a @'HookTool'@ target.
-}
data WrapperEnv = WrapperEnv
    { weCache :: Maybe ToolCache
    -- ^ Cache backing 'WithCache'. Without one, 'WithCache' is a no-op.
    , weInvokeTool :: Maybe (Text -> Aeson.Value -> IO UserToolResponse)
    {- ^ Invoke a tool registered on the agent by name, given its JSON
    arguments, for a @'HookTool'@ hook target. 'Nothing' means a
    'HookTool' hook always fails (and so, per D7, denies for 'before' /
    passes through for 'after').
    -}
    }

-- | A 'WrapperEnv' with no cache and no tool-invocation hook configured.
defaultWrapperEnv :: WrapperEnv
defaultWrapperEnv = WrapperEnv{weCache = Nothing, weInvokeTool = Nothing}

{- | Build a 'weInvokeTool' hook from an agent's raw tool-dispatch function
(its @toolCall@), wrapping a name and JSON arguments into the OpenAI-style
shape 'extractToolInfo' already reads. Deliberately bypasses 'executeCall'
(and so, the decorator pipeline): a hook target must not re-trigger the
wrappers that might be watching the tool it invokes.
-}
mkToolInvoker ::
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    ToolExecutionContext ->
    Text ->
    Aeson.Value ->
    IO UserToolResponse
mkToolInvoker dispatch ctx toolName args =
    dispatch ctx $
        LlmToolCall $
            Aeson.object ["function" Aeson..= Aeson.object ["name" Aeson..= toolName, "arguments" Aeson..= args]]

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
* 'WithBeforeHook' (Phase 5) runs a hook before the call: 'continue'
  (optionally with rewritten arguments), 'deny' (the call never runs),
  'defer' (@'ToolYield'@, the usual external-completion path), or 'answer'
  (short-circuit with a given result). Per D7, any hook failure (a crashing
  command, a non-zero exit, an unparseable response) denies, "fail closed",
  and is traced as an 'OSEvent_Error' on 'ctxEventQueue' when one exists.
* 'WithAfterHook' (Phase 5) runs a hook once the call completes (never for
  a 'ToolYield', since there is no result yet): 'continue' (optionally
  rewriting the result) or 'annotate' (append a note to it). Per D7, a
  hook failure passes the original result through unchanged, traced the
  same way.
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
    WithBeforeHook target -> do
        outcome <- runBeforeHook env target ctx call
        case outcome of
            BeforeContinue mRewrite -> next ctx (maybe call (`withRewrittenArguments` call) mRewrite)
            BeforeDeny message -> pure $ ToolComplete $ TextResponse message
            BeforeDefer _reason -> do
                token <- newContinuationToken
                pure $ ToolYield token (Cache.computeCacheKey call)
            BeforeAnswer result -> pure $ ToolComplete result
    WithAfterHook target -> do
        startedAt <- getCurrentTime
        response <- next ctx call
        case response of
            ToolYield{} -> pure response
            ToolComplete result -> do
                finishedAt <- getCurrentTime
                let durationMs = round (1000 * diffUTCTime finishedAt startedAt) :: Int
                ToolComplete <$> runAfterHook env target ctx call result durationMs
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

-------------------------------------------------------------------------------
-- Hooks (Phase 5 of the session-mailbox spec)
-------------------------------------------------------------------------------

-- | What a @before@ hook decided.
data BeforeOutcome
    = BeforeContinue (Maybe Aeson.Value)
    | BeforeDeny Text
    | BeforeDefer Text
    | BeforeAnswer UserToolResponse

-- | What an @after@ hook decided.
data AfterOutcome
    = AfterContinue (Maybe UserToolResponse)
    | AfterAnnotate Text

{- | Run a @before@ hook. Never throws: per D7, any failure (the target
crashing, a bad exit code, unparseable JSON) is reported as 'BeforeDeny',
"fail closed", and traced.
-}
runBeforeHook :: WrapperEnv -> HookTarget -> ToolExecutionContext -> LlmToolCall -> IO BeforeOutcome
runBeforeHook env target ctx call = do
    outcome <- try (dispatchHook env target ctx call Nothing Nothing) :: IO (Either SomeException Aeson.Value)
    case outcome of
        Left err -> do
            traceHookFailure ctx ("before hook failed, denying: " <> Text.pack (show err))
            pure $ BeforeDeny ("blocked by policy: " <> Text.pack (show err))
        Right responseJson -> case Aeson.Types.parseEither parseBeforeResponse responseJson of
            Left parseErr -> do
                traceHookFailure ctx ("before hook returned an unparseable response, denying: " <> Text.pack parseErr)
                pure $ BeforeDeny "blocked by policy: hook returned an invalid response"
            Right decoded -> pure decoded

{- | Run an @after@ hook. Never throws: per D7, any failure passes the
original result through unchanged, traced.
-}
runAfterHook ::
    WrapperEnv -> HookTarget -> ToolExecutionContext -> LlmToolCall -> UserToolResponse -> Int -> IO UserToolResponse
runAfterHook env target ctx call result durationMs = do
    outcome <- try (dispatchHook env target ctx call (Just result) (Just durationMs)) :: IO (Either SomeException Aeson.Value)
    case outcome of
        Left err -> do
            traceHookFailure ctx ("after hook failed, ignoring: " <> Text.pack (show err))
            pure result
        Right responseJson -> case Aeson.Types.parseEither parseAfterResponse responseJson of
            Left parseErr -> do
                traceHookFailure ctx ("after hook returned an unparseable response, ignoring: " <> Text.pack parseErr)
                pure result
            Right (AfterContinue mRewrite) -> pure $ fromMaybe result mRewrite
            Right (AfterAnnotate note) -> pure $ annotateResult note result

parseBeforeResponse :: Aeson.Value -> Aeson.Types.Parser BeforeOutcome
parseBeforeResponse = Aeson.withObject "BeforeHookResponse" $ \v -> do
    action <- v Aeson..: "action" :: Aeson.Types.Parser Text
    case action of
        "continue" -> BeforeContinue <$> v Aeson..:? "arguments"
        "deny" -> BeforeDeny <$> v Aeson..: "message"
        "defer" -> BeforeDefer <$> v Aeson..:? "reason" Aeson..!= ""
        "answer" -> BeforeAnswer <$> v Aeson..: "result"
        other -> fail ("unknown before-hook action: " <> Text.unpack other)

parseAfterResponse :: Aeson.Value -> Aeson.Types.Parser AfterOutcome
parseAfterResponse = Aeson.withObject "AfterHookResponse" $ \v -> do
    action <- v Aeson..: "action" :: Aeson.Types.Parser Text
    case action of
        "continue" -> AfterContinue <$> v Aeson..:? "result"
        "annotate" -> AfterAnnotate <$> v Aeson..: "text"
        other -> fail ("unknown after-hook action: " <> Text.unpack other)

{- | Append an @after@ hook's note to a result. 'TextResponse' and
'JsonResponse' carry it; media\/mixed results are passed through unchanged
(there is no plain-text slot to append to).
-}
annotateResult :: Text -> UserToolResponse -> UserToolResponse
annotateResult note result = case result of
    TextResponse txt -> TextResponse (txt <> "\n\n[hook note: " <> note <> "]")
    JsonResponse val -> JsonResponse (Aeson.object ["result" Aeson..= val, "hookNote" Aeson..= note])
    other -> other

{- | Build the hook input JSON: @{tool, arguments, session_id, tool_call_id,
agent}@ for @before@, plus @result@\/@status@\/@duration_ms@ for @after@
(§6). @status@ is always @"success"@: at this layer a failed call already
looks like an ordinary completed 'UserToolResponse' (e.g. 'WithRetries'
giving up), there is no separate failure signal to report.
-}
hookInputJson :: ToolExecutionContext -> LlmToolCall -> Maybe UserToolResponse -> Maybe Int -> Aeson.Value
hookInputJson ctx call mResult mDurationMs =
    Aeson.object $
        [ "tool" Aeson..= llmToolCallName call
        , "arguments" Aeson..= snd (extractToolInfo (let LlmToolCall v = call in v))
        , "session_id" Aeson..= ctx.ctxSessionId
        , "tool_call_id" Aeson..= fromMaybe "unknown" (providerToolCallId call)
        , "agent" Aeson..= ctx.ctxAgentId
        ]
            ++ maybe [] (\r -> ["result" Aeson..= r, "status" Aeson..= ("success" :: Text)]) mResult
            ++ maybe [] (\ms -> ["duration_ms" Aeson..= ms]) mDurationMs

-- | Dispatch a hook input to its target and return its raw JSON response.
dispatchHook ::
    WrapperEnv -> HookTarget -> ToolExecutionContext -> LlmToolCall -> Maybe UserToolResponse -> Maybe Int -> IO Aeson.Value
dispatchHook env target ctx call mResult mDurationMs = do
    let input = hookInputJson ctx call mResult mDurationMs
    case target of
        HookCommand path -> runHookCommand path input
        HookTool toolName -> case env.weInvokeTool of
            Nothing -> ioError $ userError "no tool-invocation hook configured for HookTool targets"
            Just invoke -> do
                response <- invoke toolName input
                case response of
                    JsonResponse v -> pure v
                    TextResponse t -> case Aeson.eitherDecodeStrict (TE.encodeUtf8 t) of
                        Left e -> ioError $ userError ("hook tool returned non-JSON text: " <> e)
                        Right v -> pure v
                    _ -> ioError $ userError "hook tool must return a text or JSON response"

{- | Run a @'HookCommand'@ subprocess: the input JSON on stdin, its JSON
response on stdout (the isolation-envelope conventions' stdin\/stdout/JSON
shape, though a hook's response is not itself an 'IsolationResultEnvelope',
so this does not reuse 'runExternalProcess'). A non-zero exit is a failure
(denied for @before@, per D7).
-}
runHookCommand :: FilePath -> Aeson.Value -> IO Aeson.Value
runHookCommand path input = do
    let cp = (proc path []){std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    (mStdin, mStdout, mStderr, ph) <- createProcess cp
    case (mStdin, mStdout, mStderr) of
        (Just hin, Just hout, Just herr) -> do
            hSetBinaryMode hin True
            hSetBinaryMode hout True
            hSetBinaryMode herr True
            LBS.hPut hin (Aeson.encode input)
            hClose hin
            out <- LBS.hGetContents hout
            err <- LBS.hGetContents herr
            _ <- evaluate (LBS.length out)
            _ <- evaluate (LBS.length err)
            exitCode <- waitForProcess ph
            case exitCode of
                ExitFailure code ->
                    ioError $
                        userError (path <> " exited with code " <> show code <> errSuffix err)
                ExitSuccess -> either (ioError . userError) pure (Aeson.eitherDecode out)
        _ -> ioError $ userError "failed to create process pipes"
  where
    errSuffix err =
        let e = Text.strip $ TE.decodeUtf8With TEE.lenientDecode $ LBS.toStrict err
         in if Text.null e then "" else ": " <> Text.unpack e

{- | Splice rewritten @arguments@ into an 'LlmToolCall', following the same
two JSON shapes 'extractToolInfo' reads from (OpenAI-style
@function.arguments@, or a native top-level @arguments@).
-}
withRewrittenArguments :: Aeson.Value -> LlmToolCall -> LlmToolCall
withRewrittenArguments newArgs (LlmToolCall val) = LlmToolCall $ case val of
    Aeson.Object obj -> case KeyMap.lookup "function" obj of
        Just (Aeson.Object funcObj) ->
            Aeson.Object $ KeyMap.insert "function" (Aeson.Object (KeyMap.insert "arguments" newArgs funcObj)) obj
        _ -> Aeson.Object (KeyMap.insert "arguments" newArgs obj)
    other -> other

-- | Report a hook failure through the context's event queue, if any.
traceHookFailure :: ToolExecutionContext -> Text -> IO ()
traceHookFailure ctx msg = case ctx.ctxEventQueue of
    Nothing -> pure ()
    Just q -> atomically $ writeTQueue q (OSEvent_Error msg)

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

