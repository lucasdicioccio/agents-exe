{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Concurrent async execution engine for tool calls.

This module provides the Phase 2 async engine described in
@todos/async-tool-calls.md@. It runs 'RunAsync' tool calls concurrently,
updates their OS entity lifecycle states, and supports structured progress
callbacks.

Key design points:

* Each 'RunAsync' call is promoted to a background 'Async' thread.
* Calls are started under a shared semaphore that enforces 'aeMaxConcurrency'
  across all batches created by the engine.
* The engine marks entities as 'TcExecuting' before running a call and
  'TcCompleted' / 'TcFailed' / 'TcCancelled' afterwards. A 'ProgressStarted'
  entry is also recorded when the call begins.
* A progress callback is injected into 'ToolExecutionContext' for each call;
  payloads emitted through it become 'ToolCallProgress' entries on the OS
  entity.
* A global registry inside 'AsyncEngine' lets 'cancelToolCall' find the
  batch that owns a running call. Completed calls are removed from the
  registry so that cancellation reports 'False' for already-final calls.
* A process-wide index of running calls records which engine owns each one,
  so 'cancelToolCallAnyEngine' can stop a call from an agent that does not
  hold that engine (e.g. a session resumed with a freshly built agent).

The engine is intentionally small and does not persist in-progress calls.
After a process restart, calls referenced in chat history that have no OS
entity are reported as @orphaned@ by the system capability layer
(Phase 3).

Every lifecycle step of a call (queued, started, progressed, completed,
failed, timed out, cancelled) is also reported to the engine's
'Prod.Tracer.Tracer' as an 'AsyncTrace', which carries the session and call
ids. This is the logging path; the 'ctxEmit' activity events are the one for
clients.
-}
module System.Agents.Session.Async.Engine (
    -- * Engine and batch types
    AsyncEngine (..),
    AsyncBatch (..),
    AsyncBatchUpdate (..),

    -- * Traces
    AsyncTrace (..),
    AsyncCallInfo (..),
    AsyncCallEvent (..),
    asyncTraceKind,
    asyncTraceFields,

    -- * Construction
    mkAsyncEngine,
    mkAsyncEngineSharing,
    newAsyncConcurrencyLimit,

    -- * Batch lifecycle
    startAsyncBatch,
    waitForProgress,
    waitForProgressTimeout,
    finalizeCompleted,
    cancelAsyncBatch,
    cancelToolCall,
    cancelToolCallAnyEngine,
    shutdownAsyncEngine,
) where

import Control.Concurrent (QSem, newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (Async, async, cancel, poll, waitAnyCatch)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeAsyncException, SomeException, bracket_, catch, displayException, finally, fromException, throwIO)
import Control.Monad (forM, forM_, void, when)
import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (isJust, isNothing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Prod.Tracer (Tracer, runTracer)
import System.Timeout (timeout)

import qualified System.Agents.OS.Conversation.ToolCalls as TCT
import System.Agents.OS.Conversation.Types (ProgressKind (..), ToolCallProgress (..), ToolCallState (tcStatus), ToolCallStatus (..))
import System.Agents.Base (ConversationId)
import System.Agents.OS.Core.Types (EntityId)
import System.Agents.OS.Core.World (World, getComponent)
import System.Agents.OS.Events (OSEmission (..), ToolCallActivity (..), ToolCallPhase (..))
import System.Agents.Session.Mailbox (Mailbox (..))
import System.Agents.Session.Types (
    LlmToolCall (..),
    SessionId,
    ToolCallId (..),
    TrackedToolCall (..),
    UserToolResponse (..),
    llmToolCallName,
    providerToolCallId,
 )
import qualified System.Agents.Session.Types as ST
import System.Agents.Tools.Context (ToolExecutionContext (..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

{- | The async execution engine.

Carries the OS 'World', the call executor, a concurrency limit, a shared
semaphore, and a registry of active batches keyed by 'ToolCallId'. The
registry is used by 'cancelToolCall' to locate the batch owning a running
call.
-}
data AsyncEngine = AsyncEngine
    { aeWorld :: World
    , aeExecutor :: ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
    , aeMaxConcurrency :: Int
    , aeCallTimeout :: Maybe Int
    -- ^ Seconds after which a call is given up on ('Nothing': never)
    , aeSemaphore :: QSem
    , aeRegistry :: TVar (Map ToolCallId AsyncBatch)
    , aeMailbox :: Maybe Mailbox
    {- ^ Optional mailbox (@todos/session-mailbox.md@, Phase 1). When
    present, every call's final result (completed or failed) is also
    posted as 'ST.ToolCallFinished' mail right after its OS entity is
    marked final, so the owning session's next receive point (R1/R2) sees
    it without polling. Best-effort: a full mailbox drops the notification,
    the OS entity state is still the source of truth for
    'get-tool-call-status' and late-result delivery.
    -}
    , aeTracer :: Tracer IO AsyncTrace
    -- ^ Receives one 'AsyncTrace' per lifecycle step of every call
    }

{- | A running batch of async calls.

The batch tracks the 'Async' handle for each call so that progress can be
polled and cancellation is possible.
-}
data AsyncBatch = AsyncBatch
    { abEngine :: AsyncEngine
    , abWorld :: World
    , abCalls :: TVar (Map ToolCallId AsyncCallHandle)
    }

{- | Handle for a single background call.
-}
data AsyncCallHandle = AsyncCallHandle
    { achTracked :: TrackedToolCall
    , achAsync :: Async UserToolResponse
    , achEmit :: ToolCallPhase -> IO ()
    -- ^ Publishes an activity event for this call (no-op without an event queue)
    , achTrace :: AsyncCallEvent -> IO ()
    -- ^ Reports a lifecycle step of this call to the engine's tracer
    }

{- | Result of waiting for progress in a batch.

'abuCompleted' contains calls that have finished since the last wait (or
since the batch started). 'abuStillRunning' lists calls that are still in
flight.
-}
data AsyncBatchUpdate = AsyncBatchUpdate
    { abuCompleted :: [(ToolCallId, UserToolResponse)]
    , abuStillRunning :: [ToolCallId]
    }
    deriving (Show, Eq)

{- | The engine that owns each call currently running in this process.

Engines are per agent, and an agent rebuilt for a resumed session gets a new
one, which knows nothing of the calls its predecessor started. Their threads
still run in this process, so this index is how they stay reachable. Entries
live exactly as long as the entries of the owner's 'aeRegistry'. Call ids
are UUIDs, so engines never collide.
-}
runningCallOwners :: TVar (Map ToolCallId AsyncEngine)
runningCallOwners = unsafePerformIO (newTVarIO Map.empty)
{-# NOINLINE runningCallOwners #-}

-- | Forget calls in both the engine's registry and the process-wide index.
forgetCalls :: AsyncEngine -> [ToolCallId] -> STM ()
forgetCalls engine callIds = do
    modifyTVar' (aeRegistry engine) $ \reg -> foldr Map.delete reg callIds
    modifyTVar' runningCallOwners $ \owners -> foldr Map.delete owners callIds

-------------------------------------------------------------------------------
-- Traces
-------------------------------------------------------------------------------

-- | A lifecycle step of one background tool call.
data AsyncTrace = AsyncCallTrace !AsyncCallInfo !AsyncCallEvent
    deriving (Show, Eq)

-- | Which call a trace is about.
data AsyncCallInfo = AsyncCallInfo
    { aciSessionId :: !SessionId
    -- ^ Session holding the call
    , aciConversationId :: !ConversationId
    -- ^ Conversation holding the call
    , aciToolCallId :: !ToolCallId
    -- ^ Session-level id of the call
    , aciProviderCallId :: !(Maybe Text)
    -- ^ Id the LLM provider gave the call, when known
    , aciToolName :: !Text
    -- ^ Name of the called tool
    }
    deriving (Show, Eq)

{- | What happened to the call. Durations are in milliseconds.

A call that ends is reported with exactly one of 'AsyncCallCompleted',
'AsyncCallFailed', 'AsyncCallTimedOut' or 'AsyncCallCancelled'.
-}
data AsyncCallEvent
    = -- | Accepted by the engine; it now waits for a concurrency slot
      AsyncCallQueued
    | -- | Got a slot and runs, after waiting this long for it
      AsyncCallStarted !Int
    | -- | The tool reported a progress payload
      AsyncCallProgressed !Value
    | -- | Returned a result after running this long
      AsyncCallCompleted !Int
    | -- | The executor threw, after running this long
      AsyncCallFailed !Int !Text
    | -- | Given up on after 'aeCallTimeout' seconds
      AsyncCallTimedOut !Int
    | -- | Stopped by 'cancelToolCall', 'cancelAsyncBatch' or 'shutdownAsyncEngine'
      AsyncCallCancelled
    deriving (Show, Eq)

{- | A short, stable name for the step, for log lines:
@tool_call.queued@, @tool_call.started@, @tool_call.progressed@,
@tool_call.completed@, @tool_call.failed@, @tool_call.timed_out@,
@tool_call.cancelled@.
-}
asyncTraceKind :: AsyncTrace -> Text
asyncTraceKind (AsyncCallTrace _ event) = case event of
    AsyncCallQueued -> "tool_call.queued"
    AsyncCallStarted _ -> "tool_call.started"
    AsyncCallProgressed _ -> "tool_call.progressed"
    AsyncCallCompleted _ -> "tool_call.completed"
    AsyncCallFailed _ _ -> "tool_call.failed"
    AsyncCallTimedOut _ -> "tool_call.timed_out"
    AsyncCallCancelled -> "tool_call.cancelled"

{- | The fields of a log line for the step: the ids of the session, the
conversation and the call, the tool name, and what the step carries
(@queued_ms@, @elapsed_ms@, @error@, @timeout_seconds@). A progress payload
is tool output of any size, so only its encoded size is given
(@payload_bytes@).
-}
asyncTraceFields :: AsyncTrace -> [Pair]
asyncTraceFields (AsyncCallTrace info event) =
    [ "session_id" .= info.aciSessionId
    , "conversation_id" .= info.aciConversationId
    , "tool_call_id" .= info.aciToolCallId
    , "tool" .= info.aciToolName
    ]
        <> maybe [] (\pid -> ["provider_call_id" .= pid]) info.aciProviderCallId
        <> eventFields
  where
    eventFields = case event of
        AsyncCallQueued -> []
        AsyncCallStarted queuedMs -> ["queued_ms" .= queuedMs]
        AsyncCallProgressed payload -> ["payload_bytes" .= LByteString.length (Aeson.encode payload)]
        AsyncCallCompleted elapsedMs -> ["elapsed_ms" .= elapsedMs]
        AsyncCallFailed elapsedMs err -> ["elapsed_ms" .= elapsedMs, "error" .= err]
        AsyncCallTimedOut seconds -> ["timeout_seconds" .= seconds]
        AsyncCallCancelled -> []

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

{- | Create a new async engine.

The executor is invoked in a background thread for each 'RunAsync' call. The
concurrency limit is enforced by a shared 'QSem'.
-}
mkAsyncEngine ::
    -- | Receives the lifecycle of every call ('Prod.Tracer.silent' for none)
    Tracer IO AsyncTrace ->
    World ->
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    Int ->
    -- | Seconds after which a call is given up on ('Nothing': never)
    Maybe Int ->
    -- | Mailbox to post 'ST.ToolCallFinished' mail to, if any
    Maybe Mailbox ->
    IO AsyncEngine
mkAsyncEngine tracer world executor maxConcurrency callTimeout mailbox = do
    sem <- newAsyncConcurrencyLimit maxConcurrency
    mkAsyncEngineSharing tracer sem world executor maxConcurrency callTimeout mailbox

{- | A concurrency limit that several engines can share.

Each agent creates its own engine (the executor is tied to the agent's
tools), so a host that runs many agents can cap them globally by creating
one limit and passing it to every engine.
-}
newAsyncConcurrencyLimit :: Int -> IO QSem
newAsyncConcurrencyLimit maxConcurrency = newQSem (max maxConcurrency 1)

-- | Like 'mkAsyncEngine', but with a semaphore shared with other engines.
mkAsyncEngineSharing ::
    Tracer IO AsyncTrace ->
    QSem ->
    World ->
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    Int ->
    Maybe Int ->
    Maybe Mailbox ->
    IO AsyncEngine
mkAsyncEngineSharing tracer sem world executor maxConcurrency callTimeout mailbox = do
    registry <- newTVarIO Map.empty
    pure $ AsyncEngine world executor maxConcurrency callTimeout sem registry mailbox tracer

-------------------------------------------------------------------------------
-- Batch lifecycle
-------------------------------------------------------------------------------

{- | Start executing a batch of tracked calls concurrently.

Each call must already have an OS entity id ('tcEntityId'); calls without
one cannot be tracked and are ignored, so callers should run them inline.
The supplied 'ToolExecutionContext' is used as a template: the engine
injects a progress callback tailored to each call.

The calls are registered in the engine's cancellation registry, then
started under the shared concurrency semaphore. Each call removes itself
from the registry when it finishes.
-}
startAsyncBatch ::
    AsyncEngine ->
    ToolExecutionContext ->
    [TrackedToolCall] ->
    IO AsyncBatch
startAsyncBatch engine baseCtx allCalls = do
    let calls = filter (isJust . tcEntityId) allCalls
    callMap <- newTVarIO Map.empty
    let batch = AsyncBatch engine (aeWorld engine) callMap
    -- Register this batch for every call it owns.
    atomically $ do
        modifyTVar' (aeRegistry engine) $ \reg ->
            foldr (\tc -> Map.insert (tcId tc) batch) reg calls
        modifyTVar' runningCallOwners $ \owners ->
            foldr (\tc -> Map.insert (tcId tc) engine) owners calls
    -- Start each call.
    forM_ calls $ \tc -> do
        handle <- startCall engine batch baseCtx tc
        atomically $ modifyTVar' callMap $ Map.insert (tcId tc) handle
    pure batch

{- | Block until at least one call in the batch makes progress.

"Progress" here means a call reached a final state (completed, failed, or
cancelled). When this returns, use 'finalizeCompleted' to collect the
finished calls.
-}
waitForProgress :: AsyncBatch -> IO AsyncBatchUpdate
waitForProgress = waitForProgressWithTimeout Nothing

{- | Block until at least one call finishes or the timeout (in milliseconds)
elapses.

If the timeout elapses first, the update reflects whatever calls have
completed up to that point.
-}
waitForProgressTimeout :: Int -> AsyncBatch -> IO AsyncBatchUpdate
waitForProgressTimeout ms = waitForProgressWithTimeout (Just ms)

waitForProgressWithTimeout :: Maybe Int -> AsyncBatch -> IO AsyncBatchUpdate
waitForProgressWithTimeout mTimeout batch = do
    handles <- Map.elems <$> readTVarIO (abCalls batch)
    case map achAsync handles of
        [] -> finalizeBatchUpdate batch
        asyncs -> do
            let waitAction = waitAnyCatch asyncs
            _ <- case mTimeout of
                Nothing -> Just <$> waitAction
                Just ms -> timeout (ms * 1000) waitAction
            finalizeBatchUpdate batch

{- | Collect all completed calls and remove them from the batch.

Returns the list of completed '(ToolCallId, UserToolResponse)' pairs. Calls
that are still running remain in the batch and can be collected later.
-}
finalizeCompleted :: AsyncBatch -> IO [(ToolCallId, UserToolResponse)]
finalizeCompleted batch = do
    update <- finalizeBatchUpdate batch
    pure $ abuCompleted update

{- | Cancel every still-running call in the batch.

Best-effort: asynchronous exceptions are sent to each background thread and
the corresponding OS entities are marked 'TcCancelled'.
-}
cancelAsyncBatch :: AsyncBatch -> IO ()
cancelAsyncBatch batch = do
    handles <- readTVarIO (abCalls batch)
    forM_ handles $ \h -> do
        -- A call that already ended and only waits to be collected has
        -- reported how it ended: it is not traced as cancelled as well.
        stillRunning <- isNothing <$> poll (achAsync h)
        cancel (achAsync h)
        case tcEntityId (achTracked h) of
            Just eid -> TCT.cancelToolCall (abWorld batch) eid
            Nothing -> pure ()
        achEmit h ToolCallCancelled
        when stillRunning $ achTrace h AsyncCallCancelled
    atomically $ do
        modifyTVar' (abCalls batch) $ const Map.empty
        removeFromRegistry batch (Map.keys handles)

{- | Cancel a single running call by its 'ToolCallId'.

Looks up the call in the engine registry, cancels its background thread
(waiting for it to terminate), and marks the OS entity as cancelled. A
successful cancellation also posts 'ST.ToolCallFinished' \/ 'ST.Failed' mail
(@todos/session-mailbox.md@ §4, "'CancelCalls': the engine's cancel;
results come back as 'ToolCallFinished' ... 'Failed'"), the same way a call
that fails on its own does (see 'startCall').
Returns 'True' if the call ended up cancelled, 'False' otherwise (unknown
to this engine, or it reached a final state before the cancellation).
-}
cancelToolCall :: AsyncEngine -> ToolCallId -> IO Bool
cancelToolCall engine callId = do
    mBatch <- Map.lookup callId <$> readTVarIO (aeRegistry engine)
    case mBatch of
        Nothing -> pure False
        Just batch -> do
            handles <- readTVarIO (abCalls batch)
            case Map.lookup callId handles of
                Nothing -> pure False
                Just h -> do
                    cancel (achAsync h)
                    atomically $ do
                        modifyTVar' (abCalls batch) $ Map.delete callId
                        forgetCalls engine [callId]
                    case tcEntityId (achTracked h) of
                        Just eid -> do
                            TCT.cancelToolCall (abWorld batch) eid
                            mState <- atomically $ getComponent @ToolCallState (abWorld batch) eid
                            let cancelled = fmap tcStatus mState == Just TcCancelled
                            when cancelled $ do
                                achEmit h ToolCallCancelled
                                achTrace h AsyncCallCancelled
                                notifyMailbox engine (achTracked h) ST.Failed (TextResponse "tool call cancelled")
                            pure cancelled
                        Nothing -> pure False

{- | Cancel a running call whichever engine of this process owns it.

The given engine (the calling agent's, if it has one) is asked first. If it
does not know the call, the engine that started it is looked up in the
process-wide index and asked instead. This is what makes @cancel-tool-call@
and 'System.Agents.Session.Types.CancelCalls' mail stop the thread of a call
started before the agent was rebuilt (a pause and resume in the same
process), rather than only mark its entity.

Returns 'True' if the call ended up cancelled. A call that no engine of this
process runs (already final, or started by a process that is gone) gives
'False'.
-}
cancelToolCallAnyEngine :: Maybe AsyncEngine -> ToolCallId -> IO Bool
cancelToolCallAnyEngine mEngine callId = do
    byOwn <- maybe (pure False) (`cancelToolCall` callId) mEngine
    if byOwn
        then pure True
        else do
            mOwner <- Map.lookup callId <$> readTVarIO runningCallOwners
            maybe (pure False) (`cancelToolCall` callId) mOwner

{- | Cancel every call the engine still knows about.

Called when the agent that owns the engine is done (or failed), so that
background calls and their subprocesses do not outlive it. Calls that
already finished are unaffected.
-}
shutdownAsyncEngine :: AsyncEngine -> IO ()
shutdownAsyncEngine engine = do
    batches <- readTVarIO (aeRegistry engine)
    -- Several calls map to the same batch; cancelling a batch twice is a no-op
    -- because the first cancellation empties it.
    forM_ (Map.elems batches) cancelAsyncBatch
    atomically $ forgetCalls engine (Map.keys batches)

-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------

-- | Start a single background call under the shared concurrency semaphore.
startCall ::
    AsyncEngine ->
    AsyncBatch ->
    ToolExecutionContext ->
    TrackedToolCall ->
    IO AsyncCallHandle
startCall engine _batch baseCtx tc = do
    let world = aeWorld engine
    eid <- requireEntityId tc
    let emit = emitActivity baseCtx tc
    let trace = runTracer (aeTracer engine) . AsyncCallTrace (callInfo baseCtx tc)
    let onProgress payload = do
            makeProgressCallback world eid payload
            emit (ToolCallProgressed payload)
            trace (AsyncCallProgressed payload)
    let ctx =
            baseCtx
                { ctxProgressCallback = Just onProgress
                , ctxRecordChildSession = Just (TCT.recordChildSession world eid)
                }
    queuedAt <- getCurrentTime
    trace AsyncCallQueued
    a <- async $ (`finally` unregister) $ do
        bracket_ (waitQSem sem) (signalQSem sem) $ do
            TCT.startToolCall world eid
            emitStartedProgress world eid
            emit ToolCallStarted
            startedAt <- getCurrentTime
            trace (AsyncCallStarted (millisBetween queuedAt startedAt))
            result <- runCallWithTimeout engine ctx (tcCall tc)
            elapsedMs <- millisBetween startedAt <$> getCurrentTime
            case result of
                Right response -> do
                    TCT.completeToolCall world eid (toJSON response)
                    notifyMailbox engine tc ST.Completed response
                    emit ToolCallCompleted
                    trace (AsyncCallCompleted elapsedMs)
                    pure response
                Left failure -> do
                    let err = callFailureText failure
                    let response = TextResponse $ "async tool call failed: " <> err
                    TCT.failToolCall world eid err
                    notifyMailbox engine tc ST.Failed response
                    emit (ToolCallFailed err)
                    trace $ case failure of
                        CallThrew _ -> AsyncCallFailed elapsedMs err
                        CallTimedOut seconds -> AsyncCallTimedOut seconds
                    pure response
    pure $ AsyncCallHandle tc a emit trace
  where
    sem = aeSemaphore engine
    unregister = atomically $ forgetCalls engine [tcId tc]

{- | Post a call's final result as mail, if the engine has a mailbox.

Best-effort and not literally the same STM transaction as the OS entity
update right above it in 'startCall' ('mbSend' on the in-memory mailbox
does its own 'getCurrentTime' \/ 'atomically'); nothing else can complete
the same call concurrently, so the two updates cannot observably race, but
a crash between them could still drop the notification. The OS entity
remains authoritative either way (poll \/ late-result delivery covers it).
-}
notifyMailbox :: AsyncEngine -> TrackedToolCall -> ST.ToolCallState -> UserToolResponse -> IO ()
notifyMailbox engine tc state response =
    case engine.aeMailbox of
        Nothing -> pure ()
        Just mb ->
            void $
                mb.mbSend
                    ST.Outgoing
                        { ST.outId = Nothing
                        , ST.outFrom = ST.FromToolCall (tcId tc)
                        , ST.outPriority = ST.Normal
                        , ST.outHops = 0
                        , ST.outBody = ST.ToolCallFinished (tcId tc) state response
                        }

-- | Why a call did not return a result.
data CallFailure
    = -- | The executor threw this exception
      CallThrew Text
    | -- | The call outlived 'aeCallTimeout' (in seconds)
      CallTimedOut Int

-- | The error recorded on the OS entity and sent to the model.
callFailureText :: CallFailure -> Text
callFailureText failure = case failure of
    CallThrew err -> err
    CallTimedOut seconds -> "async tool call timed out after " <> Text.pack (show seconds) <> "s"

{- | Run a call, giving up after 'aeCallTimeout' seconds.

A timed-out call is reported as failed. The executor is interrupted, so a
tool running a subprocess kills it (see
'System.Agents.Tools.Bash.runProcessReportingOutput').
-}
runCallWithTimeout ::
    AsyncEngine ->
    ToolExecutionContext ->
    LlmToolCall ->
    IO (Either CallFailure UserToolResponse)
runCallWithTimeout engine ctx call =
    case aeCallTimeout engine of
        Nothing -> runCall engine ctx call
        Just seconds -> do
            mResult <- timeout (max 1 seconds * 1000000) (runCall engine ctx call)
            pure $ case mResult of
                Just result -> result
                Nothing -> Left (CallTimedOut seconds)

-- | Run the executor and catch any synchronous exception. Asynchronous
-- exceptions (e.g. from 'cancel') are rethrown so cancellation stops the call.
runCall ::
    AsyncEngine ->
    ToolExecutionContext ->
    LlmToolCall ->
    IO (Either CallFailure UserToolResponse)
runCall engine ctx call =
    (Right <$> aeExecutor engine ctx call) `catch` handler
  where
    handler :: SomeException -> IO (Either CallFailure UserToolResponse)
    handler e = case fromException e of
        Just (_ :: SomeAsyncException) -> throwIO e
        Nothing -> pure $ Left (CallThrew (Text.pack $ displayException e))

-- | The ids and tool name a trace about this call carries.
callInfo :: ToolExecutionContext -> TrackedToolCall -> AsyncCallInfo
callInfo ctx tc =
    AsyncCallInfo
        { aciSessionId = ctxSessionId ctx
        , aciConversationId = ctxConversationId ctx
        , aciToolCallId = tcId tc
        , aciProviderCallId = providerToolCallId (tcCall tc)
        , aciToolName = llmToolCallName (tcCall tc)
        }

-- | Whole milliseconds from the first time to the second.
millisBetween :: UTCTime -> UTCTime -> Int
millisBetween from to = round (realToFrac (diffUTCTime to from) * 1000 :: Double)

-- | Publish an activity event through the context's 'ctxEmit' hook
-- (@todos/os-as-standalone-server.md@ G3), if present.
emitActivity :: ToolExecutionContext -> TrackedToolCall -> ToolCallPhase -> IO ()
emitActivity ctx tc phase = do
    now <- getCurrentTime
    let activity =
            ToolCallActivity
                { tcaSessionId = ctxSessionId ctx
                , tcaConversationId = ctxConversationId ctx
                , tcaToolCallId = tcId tc
                , tcaProviderCallId = providerToolCallId (tcCall tc)
                , tcaToolName = llmToolCallName (tcCall tc)
                , tcaPhase = phase
                , tcaAt = now
                }
    forM_ (ctxEmit ctx) $ \emitFn -> emitFn (EmitToolCallActivity activity)

-- | Build a progress callback that writes structured updates to the OS entity.
makeProgressCallback :: World -> EntityId -> Value -> IO ()
makeProgressCallback world eid payload = do
    now <- getCurrentTime
    let progress = ToolCallProgress now (ProgressPartial payload) payload
    TCT.addToolCallProgress world eid progress

-- | Emit the initial 'ProgressStarted' entry for a call.
emitStartedProgress :: World -> EntityId -> IO ()
emitStartedProgress world eid = do
    now <- getCurrentTime
    let payload = object ["started" .= True]
        progress = ToolCallProgress now ProgressStarted payload
    TCT.addToolCallProgress world eid progress

-- | Require that a tracked call has an associated OS entity.
requireEntityId :: TrackedToolCall -> IO EntityId
requireEntityId tc =
    case tcEntityId tc of
        Just eid -> pure eid
        Nothing ->
            error $
                "Async engine requires every tracked call to have an OS entity id: "
                    ++ show (tcId tc)

-- | Collect completed calls and produce a batch update, removing finished
-- calls from the running set and from the engine registry.
finalizeBatchUpdate :: AsyncBatch -> IO AsyncBatchUpdate
finalizeBatchUpdate batch = do
    handles <- readTVarIO (abCalls batch)
    (completed, stillRunning) <- partitionCompleted handles
    atomically $ do
        modifyTVar' (abCalls batch) $ const stillRunning
        removeFromRegistry batch (map fst completed)
    pure $ AsyncBatchUpdate completed (Map.keys stillRunning)
  where
    partitionCompleted handles = do
        tagged <- forM (Map.toList handles) $ \(callId, h) -> do
            mResult <- poll (achAsync h)
            pure $ case mResult of
                Just (Right response) -> Left (callId, response)
                Just (Left err) ->
                    let response = TextResponse $ "async exception: " <> Text.pack (displayException err)
                     in Left (callId, response)
                Nothing -> Right (callId, h)
        let completed = [pair | Left pair <- tagged]
        let running = Map.fromList [pair | Right pair <- tagged]
        pure (completed, running)

-- | Remove the given call ids from the engine registry for this batch.
removeFromRegistry :: AsyncBatch -> [ToolCallId] -> STM ()
removeFromRegistry batch = forgetCalls (abEngine batch)

