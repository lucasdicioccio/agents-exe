{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | End-to-end tests for asynchronous tool calls driven through the session
stepper: partial turns, placeholder responses, late result delivery,
cancellation, orphaned calls, and fallbacks.
-}
module AsyncToolCallsTests (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, flushTQueue)
import Control.Exception (IOException, onException, throwIO, try)
import Control.Monad (void)
import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import System.IO.Error (ioeGetErrorString)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Data.UUID (nil)
import Prod.Tracer (silent)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@=?), (@?=))

import qualified System.Agents.Base as Base
import System.Agents.OS.Conversation.ToolCalls (createToolCallEntity, recordChildSession, registerToolCallComponents)
import System.Agents.OS.Core.Types (EntityId (..))
import System.Agents.OS.Core.World (World, newWorld)
import System.Agents.OS.Events (OSEmission (..), ToolCallActivity (..), ToolCallPhase (..), newQueueEmitter)
import System.Agents.Session.Base
import System.Agents.SessionStore (readSessionFromFile, storeSessionToFile)
import System.Agents.SessionPrint (OrderPreference (..), PrintVisibility (..), SessionPrintOptions (..), formatSessionAsMarkdown)
import System.Agents.Session.Async.Engine (shutdownAsyncEngine)
import System.Agents.Session.Loop (BlockedOnDeferredCalls (..), isBlockedOnDeferredCalls, run, runAsyncKeepingAgent, runUntilBlocked)
import System.Agents.Session.Step (buildContext, naiveTilNoToolCallStep, pollRunningCall, runStepMAsync, runStepMSync)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (proc)
import System.Timeout (timeout)
import System.Agents.Tools.Bash (runProcessReportingOutput)
import System.Agents.TUI.ToolCallActivity (
    ToolCallView (..),
    applyToolCallActivity,
    describeToolCallView,
    pruneToolCallViews,
    sessionToolCallViews,
    summarizeProgress,
 )
import System.Agents.Tools.Context (ToolExecutionContext, ToolPortal, ToolResult (..))
import qualified System.Agents.Tools.Context as Ctx
import System.Agents.Tools.SystemToolbox.ToolCallStatus (
    cancelToolCallById,
    clampWaitSeconds,
    getToolCallStatus,
    listRunningToolCalls,
    waitForCallsOrMail,
 )
import System.Agents.Tools.SystemToolbox.Types (
    CancelToolCallParams (..),
    CancelToolCallResult (..),
    GetToolCallStatusParams (..),
    ListRunningToolCallsResult (..),
    RunningToolCallInfo (..),
    ToolCallStatusResult (..),
    WaitParams (..),
    WaitResult (..),
 )

tests :: TestTree
tests =
    testGroup
        "Async tool calls (end-to-end)"
        [ testCase "partial turn shows a placeholder and the late result is delivered once" partialTurnLateDelivery
        , testCase "cancel-tool-call interrupts a running background call" cancelInterruptsCall
        , testCase "running call without an OS entity resolves as orphaned" orphanedCallResolves
        , testCase "a call running at shutdown is orphaned after a reload" orphanedAcrossRestart
        , testCase "resuming a partial turn replaces it instead of stacking" resumeReplacesPartialTurn
        , testCase "RunAsync call that cannot be tracked runs inline" untrackableCallRunsInline
        , testCase "asynchronous agent without a World gets a private one" asyncWithoutWorld
        , testCase "ctxMaxConcurrency bounds concurrently running calls" maxConcurrencyBoundsCalls
        , testCase "engine publishes activity events on the event queue" engineEmitsActivity
        , testCase "a background call's completion is posted as ToolCallFinished mail" backgroundCompletionPostsMail
        , testCase "background results win the race against a pending user query" backgroundWinsUserQueryRace
        , testCase "a user query wins the race against running background calls" userQueryWinsRace
        , testGroup
            "TUI activity view"
            [ testCase "tracks latest phase and progress per call" activityViewTracksProgress
            , testCase "pruning keeps finished calls the session still shows running" activityViewPrune
            , testCase "summarizes progress payloads" activitySummaries
            ]
        , testCase "markdown export shows partial turn call status" markdownPartialTurn
        , testCase "runUntilBlocked returns a session blocked on deferred calls" runUntilBlockedOnDeferred
        , testCase "runUntilBlocked stops on a Control Pause sent mid-wait" runUntilBlockedStopsOnPauseMidWait
        , testCase "a failing run cancels background calls" runFailureCancelsBackgroundCalls
        , testCase "a call that outlives ctxAsyncCallTimeout fails" callTimesOut
        , testGroup
            "residual gaps"
            [ testCase "a result read with get-tool-call-status is not repeated by the late notice" statusReadNotRepeated
            , testCase "a result read in the turn that delivers it is not repeated" statusReadInSameTurnNotRepeated
            , testCase "with a mailbox, a late result reaches the LLM once" lateResultOnceWithMailbox
            , testCase "with a mailbox, a result read with get-tool-call-status is not repeated" statusReadNotRepeatedWithMailbox
            , testCase "with a mailbox, an attached call's result is not repeated as mail" attachedResultNotRepeatedAsMail
            , testCase "cancel-tool-call kills a call owned by another agent's engine" cancelReachesForeignEngine
            , testCase "run stops on a turn that only waits on deferred calls" runStopsOnDeferredOnly
            , testCase "a synchronous step does not repeat a result read with get-tool-call-status" statusReadNotRepeatedSyncStep
            , testCase "mail about a call this process has no entity for is still rendered" foreignToolCallMailRendered
            , testCase "runAsyncKeepingAgent hands back the engine that owns the running calls" runAsyncHandsBackEngine
            ]
        , testGroup
            "Phase 2: attach / detach"
            [ testCase "attachSeconds detaches a call after its deadline" attachSecondsDetaches
            , testCase "an interrupt envelope detaches attached calls (R3a)" interruptDetachesAttachedCalls
            , testCase "a normal envelope does not detach an attached call" normalMailDoesNotDetach
            , testCase "wait wakes on a named call becoming final" waitWakesOnNamedCall
            , testCase "wait wakes on any-call" waitWakesOnAnyCall
            , testCase "wait wakes on mail without consuming it" waitWakesOnMail
            , testCase "wait times out when nothing happens" waitTimesOut
            , testCase "clampWaitSeconds caps at maxWaitSeconds" clampWaitSecondsCapsRequest
            ]
        , testGroup
            "Phase 4: detached sub-agent calls expose their child session id"
            [ testCase "pollRunningCall copies a recorded child session id onto the tracked call" pollRunningCallCopiesChildSessionId
            , testCase "pollRunningCall leaves tcChildSessionId absent for an ordinary call" pollRunningCallLeavesChildSessionIdAbsent
            , testCase "a running placeholder includes childSessionId when known" placeholderIncludesChildSessionId
            , testCase "a running placeholder omits childSessionId for an ordinary call" placeholderOmitsChildSessionIdByDefault
            ]
        , testGroup
            "subprocess output"
            [ testCase "reports output lines while the process runs" processReportsOutput
            , testCase "cancelling the call terminates the process" processKilledOnCancel
            , testCase "cancelling the call kills processes the script started" processGroupKilledOnCancel
            ]
        ]

-------------------------------------------------------------------------------
-- Scenarios
-------------------------------------------------------------------------------

{- | Two async calls, one fast and one gated. With 'YieldOnAnyProgress' the
step yields a partial turn, the LLM sees a placeholder for the slow call,
and when the LLM ends its turn without tool calls the stepper waits for the
slow call and delivers its result in a user message.
-}
partialTurnLateDelivery :: Assertion
partialTurnLateDelivery = do
    world <- mkWorld
    gate <- newEmptyMVar
    completions <- newIORef []
    script <-
        newIORef
            [ [mkCall "call_fast" "fast", mkCall "call_slow" "slow"]
            , []
            , []
            ]
    let agent =
            (mkAgent world YieldOnAnyProgress (gatedToolCall gate))
                { complete = scriptedComplete script completions
                }
    let s0 = initialSession

    -- LLM issues both calls.
    (a1, s1) <- stepOk agent s0
    -- Both start; the fast one finishes, the slow one is still running.
    (a2, s2) <- stepOk a1 s1
    case s2.turns of
        (PartialUserTurn partial _ : _) ->
            map (\tc -> (providerToolCallId tc.tcCall, tc.tcState)) partial.pTrackedToolCalls
                @?= [(Just "call_fast", Completed), (Just "call_slow", Running)]
        _ -> assertFailure "expected a partial user turn"
    assertBool "engine should be kept on the agent" (isJustEngine a2)

    -- The LLM is asked with a placeholder for the slow call.
    (a3, s3) <- stepOk a2 s2
    firstPartial <- lastCompletion completions
    map (providerToolCallId . fst) firstPartial.completeToolResponses @?= [Just "call_fast", Just "call_slow"]
    case map snd firstPartial.completeToolResponses of
        [TextResponse "fast-result", JsonResponse (Object placeholder)] ->
            KeyMap.lookup "status" placeholder @?= Just (String "running")
        other -> assertFailure $ "unexpected tool responses: " <> show other

    -- The LLM ended its turn without tool calls: the stepper waits for the
    -- slow call (released shortly) and delivers its result.
    void $ forkIO $ threadDelay 50000 >> putMVar gate ()
    (a4, s4) <- stepOk a3 s3
    case s4.turns of
        (UserTurn content _ : _) -> do
            content.userToolResponses @?= []
            case content.userQuery of
                Just q -> do
                    assertBool "notice mentions the call id" ("call_slow" `Text.isInfixOf` q.queryText)
                    assertBool "notice includes the result" ("slow-result" `Text.isInfixOf` q.queryText)
                Nothing -> assertFailure "expected a notice with the late result"
        _ -> assertFailure "expected a user turn with the late result"
    length [() | PartialUserTurn _ _ <- s4.turns] @?= 1

    -- The LLM sees the notice; the partial turn keeps its placeholder.
    (a5, s5) <- stepOk a4 s4
    final <- lastCompletion completions
    case final.completeQuery of
        Just q -> assertBool "query carries the late result" ("slow-result" `Text.isInfixOf` q.queryText)
        Nothing -> assertFailure "expected the late-result query"
    allCompletions <- readIORef completions
    mapM_ assertToolMessagesPaired allCompletions

    -- Nothing is left in the background, so the session stops.
    (_, res6) <- runStepMAsync convId a5 s5
    case res6 of
        Left _ -> pure ()
        Right _ -> assertFailure "expected the session to stop"

{- | A call cancelled through the capability, addressed by its provider id,
has its thread interrupted and resolves as failed on the next step.
-}
cancelInterruptsCall :: Assertion
cancelInterruptsCall = do
    world <- mkWorld
    interrupted <- newIORef False
    finished <- newIORef False
    let slowCall _ _ = do
            (threadDelay 10000000 >> writeIORef finished True) `onException` writeIORef interrupted True
            pure $ TextResponse "should not happen"
    let agent = mkAgent world (YieldOnTimeout 20) slowCall
    let s0 = sessionWithCalls [mkCall "call_slow" "slow"]

    (a1, s1) <- stepOk agent s0
    case s1.turns of
        (PartialUserTurn partial _ : _) -> map tcState partial.pTrackedToolCalls @?= [Running]
        _ -> assertFailure "expected a partial user turn"

    let ctx = buildContext a1 s1 convId
    Right (ListRunningToolCallsResult running) <- listRunningToolCalls ctx
    map rtciToolCallId running @?= ["call_slow"]
    Right status <- getToolCallStatus ctx (statusParams "call_slow")
    tcsrToolName status @?= Just "slow"
    tcsrIsFinal status @?= False

    Right cancelled <- cancelToolCallById ctx (CancelToolCallParams "call_slow" Nothing)
    ccrCancelled cancelled @?= True
    readIORef interrupted >>= (@?= True)
    readIORef finished >>= (@?= False)
    Right after <- getToolCallStatus ctx (statusParams "call_slow")
    tcsrStatus after @?= "cancelled"

    -- The next step resolves the partial turn, then asks the LLM.
    (_, s2) <- stepOk a1 s1
    case s2.turns of
        (LlmTurn _ _ : UserTurn content _ : _) ->
            map snd content.userToolResponses @?= [TextResponse "async tool call was cancelled"]
        _ -> assertFailure "expected the cancelled call to finalize the user turn"

-- | A running call whose entity is gone (e.g. after a restart) is failed.
orphanedCallResolves :: Assertion
orphanedCallResolves = do
    world <- mkWorld
    let tracked =
            TrackedToolCall
                { tcId = ToolCallId UUID.nil
                , tcCall = mkCall "call_lost" "lost"
                , tcState = Running
                , tcResult = Nothing
                , tcContinuation = Nothing
                , tcPolicy = AppliedPolicy (RunAsync Nothing) Nothing
                , tcEntityId = Just (EntityId UUID.nil)
                , tcDeliveredLate = False
                }
    let partial = PartialUserTurnContent (SystemPrompt "test") [] Nothing [tracked] [] False
    let s0 = (sessionWithCalls [tracked.tcCall]){turns = [PartialUserTurn partial Nothing, llmTurnWith [tracked.tcCall]]}
    let agent = mkAgent world YieldWhenAllDone (\_ _ -> pure $ TextResponse "unused")
    (_, s1) <- stepOk agent s0
    case s1.turns of
        [LlmTurn _ _, UserTurn content _, LlmTurn _ _] ->
            case map snd content.userToolResponses of
                [TextResponse txt] -> assertBool "response explains the orphan" ("orphaned" `Text.isInfixOf` txt)
                other -> assertFailure $ "unexpected responses: " <> show other
        other -> assertFailure $ "unexpected turns: " <> show (length other)

{- | Simulates a restart: a session with a call still running is written to
disk, reloaded in a fresh 'World' (as a new process would), and the call is
reported as orphaned instead of hanging.
-}
orphanedAcrossRestart :: Assertion
orphanedAcrossRestart =
    withSystemTempDirectory "async-restart" $ \dir -> do
        gate <- newEmptyMVar
        world <- mkWorld
        let agent = mkAgent world (YieldOnTimeout 20) (gatedToolCall gate)
        (_, running) <- stepOk agent (sessionWithCalls [mkCall "call_slow" "slow"])
        [Running] @=? [tc.tcState | p <- partialTurns running, tc <- p.pTrackedToolCalls]

        -- The process goes away: the session survives, the world does not.
        storeSessionToFile running (dir </> "session.json")
        Just reloaded <- readSessionFromFile (dir </> "session.json")

        freshWorld <- mkWorld
        let restarted = mkAgent freshWorld (YieldOnTimeout 20) (gatedToolCall gate)
        -- The capability reports the call as final and orphaned.
        Right status <- getToolCallStatus (buildContext restarted reloaded convId) (statusParams "call_slow")
        tcsrStatus status @?= "orphaned"
        tcsrIsFinal status @?= True
        -- And the stepper resolves the turn instead of waiting forever.
        (_, s2) <- stepOk restarted reloaded
        case [c | UserTurn c _ <- s2.turns] of
            (content : _) -> case map snd content.userToolResponses of
                [TextResponse txt] -> assertBool "response explains the orphan" ("orphaned" `Text.isInfixOf` txt)
                other -> assertFailure $ "unexpected responses: " <> show other
            _ -> assertFailure "expected a user turn"
        putMVar gate ()

{- | A partial turn holding a deferred call and a running call is resumed
twice; each resume replaces the head turn.
-}
resumeReplacesPartialTurn :: Assertion
resumeReplacesPartialTurn = do
    world <- mkWorld
    gate <- newEmptyMVar
    let policy _ call
            | callName call == "defer_me" = Defer (Reason "external")
            | otherwise = RunAsync Nothing
    let agent = (mkAgent world (YieldOnTimeout 20) (gatedToolCall gate)){ctxToolCallPolicy = policy}
    let s0 = sessionWithCalls [mkCall "call_defer" "defer_me", mkCall "call_slow" "slow"]
    (a1, s1) <- stepOk agent s0
    (a2, s2) <- stepOk a1 s1
    putMVar gate ()
    (_, s3) <- stepOk a2 s2
    map (length . partialTurns) [s1, s2, s3] @?= [1, 1, 1]
    length s3.turns @?= 2
    case partialTurns s3 of
        [p] -> map tcState p.pTrackedToolCalls @?= [Deferred, Completed]
        _ -> assertFailure "expected one partial turn"

-- | A malformed call has no OS entity, so it runs inline instead of crashing.
untrackableCallRunsInline :: Assertion
untrackableCallRunsInline = do
    world <- mkWorld
    let agent = mkAgent world YieldWhenAllDone (\_ _ -> pure $ TextResponse "inline")
    let s0 = sessionWithCalls [LlmToolCall (String "not a tool call")]
    (_, s1) <- stepOk agent s0
    case s1.turns of
        (UserTurn content _ : _) -> map snd content.userToolResponses @?= [TextResponse "inline"]
        _ -> assertFailure "expected a full user turn"

-- | Async mode must not silently run inline just because no World was set.
asyncWithoutWorld :: Assertion
asyncWithoutWorld = do
    world <- mkWorld
    gate <- newEmptyMVar
    let agent = (mkAgent world (YieldOnTimeout 20) (gatedToolCall gate)){ctxWorld = Nothing}
    (a1, s1) <- stepOk agent (sessionWithCalls [mkCall "call_slow" "slow"])
    case s1.turns of
        (PartialUserTurn partial _ : _) -> map tcState partial.pTrackedToolCalls @?= [Running]
        _ -> assertFailure "expected a partial user turn"
    assertBool "world should be created" (isJust a1.ctxWorld)
    assertBool "engine should be created" (isJustEngine a1)
    putMVar gate ()
    Right done <- getToolCallStatus (buildContext a1 s1 convId) (GetToolCallStatusParams "call_slow" False True 5)
    tcsrStatus done @?= "completed"
    (_, s2) <- stepOk a1 s1
    case s2.turns of
        (LlmTurn _ _ : UserTurn content _ : _) ->
            map snd content.userToolResponses @?= [TextResponse "slow-result"]
        _ -> assertFailure "expected the call to complete"

-- | With a limit of one, calls issued together never overlap.
maxConcurrencyBoundsCalls :: Assertion
maxConcurrencyBoundsCalls = do
    world <- mkWorld
    active <- newIORef (0 :: Int)
    peak <- newIORef (0 :: Int)
    let tool _ _ = do
            n <- atomicModifyIORef' active (\x -> (x + 1, x + 1))
            atomicModifyIORef' peak (\p -> (max p n, ()))
            threadDelay 20000
            atomicModifyIORef' active (\x -> (x - 1, ()))
            pure $ TextResponse "done"
    let agent = (mkAgent world YieldWhenAllDone tool){ctxMaxConcurrency = Just 1}
    (_, s1) <- stepOk agent (sessionWithCalls [mkCall "call_a" "a", mkCall "call_b" "b", mkCall "call_c" "c"])
    case s1.turns of
        (UserTurn content _ : _) -> length content.userToolResponses @?= 3
        _ -> assertFailure "expected a full user turn"
    readIORef peak >>= (@?= 1)

-- | Start, progress, and completion reach the 'ctxEmit' queue emitter.
engineEmitsActivity :: Assertion
engineEmitsActivity = do
    world <- mkWorld
    (queue, emitter) <- newQueueEmitter
    let tool ctx _ = do
            mapM_ ($ String "halfway") (Ctx.ctxProgressCallback ctx)
            pure $ TextResponse "done"
    let agent = (mkAgent world YieldWhenAllDone tool){ctxEmit = Just emitter}
    _ <- stepOk agent (sessionWithCalls [mkCall "call_a" "a"])
    events <- atomically $ flushTQueue queue
    [(act.tcaProviderCallId, act.tcaToolName, act.tcaPhase) | EmitToolCallActivity act <- events]
        @?= [ (Just "call_a", "a", ToolCallStarted)
            , (Just "call_a", "a", ToolCallProgressed (String "halfway"))
            , (Just "call_a", "a", ToolCallCompleted)
            ]

{- | Per @todos/session-mailbox.md@ §3, the engine posts a background call's
final result to the owning session's mailbox as 'ToolCallFinished' mail.
-}
backgroundCompletionPostsMail :: Assertion
backgroundCompletionPostsMail = do
    world <- mkWorld
    mb <- newInMemoryMailbox
    let tool _ call = pure $ TextResponse (callName call <> "-result")
    let agent = (mkAgent world YieldWhenAllDone tool){ctxMailbox = Just mb}
    _ <- stepOk agent (sessionWithCalls [mkCall "call_a" "a"])
    mEnvelopes <- timeout (5 * 1000 * 1000) $ atomically $ awaitMail mb 0 (const True)
    case mEnvelopes of
        Nothing -> assertFailure "expected ToolCallFinished mail after the background call completed"
        Just [e] -> case e.envBody of
            ToolCallFinished _tcid state response -> do
                state @?= Completed
                response @?= TextResponse "a-result"
            other -> assertFailure $ "expected ToolCallFinished mail, got " <> show other
        Just other -> assertFailure $ "expected exactly one envelope, got " <> show (length other)

{- | While waiting for user input, a background call finishing delivers its
result without a user query.
-}
backgroundWinsUserQueryRace :: Assertion
backgroundWinsUserQueryRace = do
    (_, gate, a1, s1) <- sessionWithBackgroundCall
    never <- newEmptyMVar :: IO (MVar ())
    let agent = a1{usrQuery = readMVar never >> pure (Just (UserQuery "too late" [])), step = askInputWhenIdle}
    void $ forkIO $ threadDelay 20000 >> putMVar gate ()
    (_, s2) <- stepOk agent s1
    case s2.turns of
        (UserTurn content _ : _) -> case content.userQuery of
            Just q -> do
                assertBool "notice includes the result" ("slow-result" `Text.isInfixOf` q.queryText)
                assertBool "no user text" (not ("too late" `Text.isInfixOf` q.queryText))
            Nothing -> assertFailure "expected the late-result notice"
        _ -> assertFailure "expected a user turn"

-- | A user query arriving first is sent right away; the call keeps running.
userQueryWinsRace :: Assertion
userQueryWinsRace = do
    (_, gate, a1, s1) <- sessionWithBackgroundCall
    let agent = a1{usrQuery = pure (Just (UserQuery "hello" [])), step = askInputWhenIdle}
    (_, s2) <- stepOk agent s1
    case s2.turns of
        (UserTurn content _ : _) -> fmap (.queryText) content.userQuery @?= Just "hello"
        _ -> assertFailure "expected a user turn"
    [tc.tcState | p <- partialTurns s2, tc <- p.pTrackedToolCalls] @?= [Running]
    putMVar gate ()

activityViewTracksProgress :: Assertion
activityViewTracksProgress = do
    t <- getCurrentTime
    let act phase = activity t phase
        views =
            foldr
                applyToolCallActivity
                Map.empty
                (reverse [act ToolCallStarted, act (ToolCallProgressed (String "line 1")), act ToolCallCompleted])
    case Map.elems (sessionToolCallViews (SessionId nil) views) of
        [v] -> do
            v.tcvLatest.tcaPhase @?= ToolCallCompleted
            v.tcvLastProgress @?= Just (String "line 1")
            describeToolCallView v @?= "finished, result pending delivery"
        other -> assertFailure $ "unexpected views: " <> show other

activityViewPrune :: Assertion
activityViewPrune = do
    t <- getCurrentTime
    let done = applyToolCallActivity (activity t ToolCallCompleted) Map.empty
        runningCall = trackedCall Running
        stillRunning = (sessionWithCalls []){turns = [PartialUserTurn (PartialUserTurnContent (SystemPrompt "p") [] Nothing [runningCall] [] False) Nothing]}
        caughtUp = (sessionWithCalls []){turns = [PartialUserTurn (PartialUserTurnContent (SystemPrompt "p") [] Nothing [trackedCall Completed] [] False) Nothing]}
    Map.size (pruneToolCallViews stillRunning done) @?= 1
    Map.size (pruneToolCallViews caughtUp done) @?= 0
    let started = applyToolCallActivity (activity t ToolCallStarted) Map.empty
    Map.size (pruneToolCallViews caughtUp started) @?= 1

activitySummaries :: Assertion
activitySummaries = do
    summarizeProgress (String "a\nb\n") @?= "b"
    summarizeProgress (object ["line" .= ("out" :: Text), "n" .= (3 :: Int)]) @?= "out"
    summarizeProgress (object ["n" .= (3 :: Int)]) @?= "{\"n\":3}"
    Text.length (summarizeProgress (String (Text.replicate 200 "x"))) @?= 100

{- | A deferred call next to a slow async call: the loop waits for the async
call, then returns instead of spinning on the deferred one.
-}
runUntilBlockedOnDeferred :: Assertion
runUntilBlockedOnDeferred = do
    world <- mkWorld
    gate <- newEmptyMVar
    let policy _ call
            | callName call == "defer_me" = Defer (Reason "external")
            | otherwise = RunAsync Nothing
    let agent = (mkAgent world (YieldOnTimeout 20) (gatedToolCall gate)){ctxToolCallPolicy = policy}
    void $ forkIO $ threadDelay 50000 >> putMVar gate ()
    res <- timeout 5000000 $ runUntilBlocked convId agent (sessionWithCalls [mkCall "call_defer" "defer_me", mkCall "call_slow" "slow"])
    case res of
        Nothing -> assertFailure "runUntilBlocked did not return"
        Just (Left _) -> assertFailure "expected a blocked session"
        Just (Right sess) -> do
            assertBool "blocked on deferred calls" (isBlockedOnDeferredCalls sess)
            [map tcState p.pTrackedToolCalls | p <- take 1 (partialTurns sess)] @?= [[Deferred, Completed]]

-------------------------------------------------------------------------------
-- Residual gaps (todos/async-tool-calls-progress.md, "Known gaps")
-------------------------------------------------------------------------------

-- | Occurrences of the needle in what a completion sends the LLM for the
-- current turn: the user query and the tool messages.
occurrencesInCompletion :: Text -> LlmCompletion -> Int
occurrencesInCompletion needle completion =
    Text.count needle $
        maybe "" (.queryText) completion.completeQuery
            <> Text.pack (show (map snd completion.completeToolResponses))

-- | Occurrences over every user turn of a session, as the LLM was sent them.
occurrencesInUserTurns :: Text -> Session -> Int
occurrencesInUserTurns needle sess =
    sum
        [ Text.count needle (maybe "" (.queryText) q <> Text.pack (show (map snd rs)))
        | turn <- sess.turns
        , (q, rs) <- case turn of
            UserTurn c _ -> [(c.userQuery, c.userToolResponses)]
            PartialUserTurn c _ -> [(c.pUserQuery, partialToolMessages c)]
            LlmTurn _ _ -> []
        ]

waitForCompletionParams :: Text -> GetToolCallStatusParams
waitForCompletionParams tid = GetToolCallStatusParams tid True True 5

{- | The model fetched the final result with @get-tool-call-status@ before the
stepper delivered it: the late notice names the call but does not carry the
result a second time.
-}
statusReadNotRepeated :: Assertion
statusReadNotRepeated = do
    (_, gate, a1, s1) <- sessionWithBackgroundCall
    putMVar gate ()
    Right status <- getToolCallStatus (buildContext a1 s1 convId) (waitForCompletionParams "call_slow")
    tcsrIsFinal status @?= True
    (_, s2) <- stepOk a1 s1
    case s2.turns of
        (UserTurn content _ : _) -> case content.userQuery of
            Just q -> do
                assertBool "notice names the call" ("call_slow" `Text.isInfixOf` q.queryText)
                assertBool "notice does not repeat the result" (not ("slow-result" `Text.isInfixOf` q.queryText))
            Nothing -> assertFailure "expected a notice for the finished call"
        _ -> assertFailure "expected a user turn"
    -- Delivered: nothing is left in the background.
    hasBackgroundCalls s2 @?= False

{- | The model asks for the status in the very turn whose user message would
deliver the late result: the result is in the tool message only.
-}
statusReadInSameTurnNotRepeated :: Assertion
statusReadInSameTurnNotRepeated = do
    world <- mkWorld
    gate <- newEmptyMVar
    script <- newIORef [[mkCall "call_slow" "slow"], [mkCall "call_check" "check"], []]
    completions <- newIORef []
    let tool ctx call = case callName call of
            "check" ->
                getToolCallStatus ctx (waitForCompletionParams "call_slow") >>= \case
                    Right status -> pure $ JsonResponse (toJSON status)
                    Left err -> pure $ TextResponse (Text.pack (show err))
            _ -> gatedToolCall gate ctx call
    let agent = (mkAgent world (YieldOnTimeout 20) tool){complete = scriptedComplete script completions}
    (a1, s1) <- stepOk agent initialSession
    (a2, s2) <- stepOk a1 s1 -- starts the slow call; partial turn
    (a3, s3) <- stepOk a2 s2 -- LLM sees the placeholder, asks for the status
    -- The call is final before the turn starts, so the stepper could deliver it.
    putMVar gate ()
    waitUntilNoneRunning (buildContext a3 s3 convId)
    (a4, s4) <- stepOk a3 s3 -- runs the status call
    (_, s5) <- stepOk a4 s4 -- LLM reads it and ends its turn
    occurrencesInUserTurns "slow-result" s5 @?= 1
    hasBackgroundCalls s5 @?= False
    lastCompletion completions >>= \c -> occurrencesInCompletion "slow-result" c @?= 1

-- | Poll 'listRunningToolCalls' until no call is running (bounded).
waitUntilNoneRunning :: ToolExecutionContext -> IO ()
waitUntilNoneRunning ctx = go (200 :: Int)
  where
    go n =
        listRunningToolCalls ctx >>= \case
            Right (ListRunningToolCallsResult []) -> pure ()
            _ | n > 0 -> threadDelay 10000 >> go (n - 1)
            _ -> assertFailure "calls are still running"

{- | The engine also posts a finished call as mail. The stepper delivers the
result itself, so the mail must not carry it a second time.
-}
lateResultOnceWithMailbox :: Assertion
lateResultOnceWithMailbox = do
    (sess, completion) <- lateDeliveryWithMailbox False
    occurrencesInUserTurns "slow-result" sess @?= 1
    occurrencesInCompletion "slow-result" completion @?= 1

statusReadNotRepeatedWithMailbox :: Assertion
statusReadNotRepeatedWithMailbox = do
    (sess, completion) <- lateDeliveryWithMailbox True
    occurrencesInUserTurns "slow-result" sess @?= 0
    occurrencesInCompletion "slow-result" completion @?= 0
    occurrencesInCompletion "call_slow" completion @?= 1

{- | A background call finishes after the LLM ended its turn, in a session
with a mailbox. Returns the session and the completion that delivers it.
-}
lateDeliveryWithMailbox :: Bool -> IO (Session, LlmCompletion)
lateDeliveryWithMailbox readFirst = do
    world <- mkWorld
    mb <- newInMemoryMailbox
    gate <- newEmptyMVar
    script <- newIORef [[mkCall "call_slow" "slow"], [], []]
    completions <- newIORef []
    let agent =
            (mkAgent world (YieldOnTimeout 20) (gatedToolCall gate))
                { complete = scriptedComplete script completions
                , ctxMailbox = Just mb
                }
    (a1, s1) <- stepOk agent initialSession
    (a2, s2) <- stepOk a1 s1
    (a3, s3) <- stepOk a2 s2
    putMVar gate ()
    if readFirst
        then do
            Right status <- getToolCallStatus (buildContext a3 s3 convId) (waitForCompletionParams "call_slow")
            tcsrIsFinal status @?= True
        else pure ()
    -- The engine posts the mail right after the entity is final.
    _ <- timeout 5000000 $ atomically $ awaitMail mb s3.mailCursor (const True)
    (a4, s4) <- stepOk a3 s3
    (_, s5) <- stepOk a4 s4
    completion <- lastCompletion completions
    hasBackgroundCalls s5 @?= False
    pure (s5, completion)

{- | An attached call that finishes within its turn answers through its tool
message; the mail the engine posted for it adds nothing to the next turn.
-}
attachedResultNotRepeatedAsMail :: Assertion
attachedResultNotRepeatedAsMail = do
    world <- mkWorld
    mb <- newInMemoryMailbox
    script <- newIORef [[mkCall "call_a" "a"], [mkCall "call_b" "b"], []]
    completions <- newIORef []
    let tool _ call = pure $ TextResponse (callName call <> "-result")
    let agent =
            (mkAgent world YieldWhenAllDone tool)
                { complete = scriptedComplete script completions
                , ctxMailbox = Just mb
                }
    (a1, s1) <- stepOk agent initialSession
    (a2, s2) <- stepOk a1 s1 -- runs a
    _ <- timeout 5000000 $ atomically $ awaitMail mb s1.mailCursor (const True)
    (a3, s3) <- stepOk a2 s2 -- LLM asks for b
    (a4, s4) <- stepOk a3 s3 -- runs b: the mail about a is unread here
    (_, s5) <- stepOk a4 s4
    occurrencesInUserTurns "a-result" s5 @?= 1
    occurrencesInUserTurns "b-result" s5 @?= 1

{- | A call started by one agent's engine is cancelled through another agent
that shares the world but not the engine (a session resumed in the same
process with a freshly built agent): the thread is interrupted, not only the
entity marked.
-}
cancelReachesForeignEngine :: Assertion
cancelReachesForeignEngine = do
    world <- mkWorld
    interrupted <- newEmptyMVar
    let slowCall _ _ = do
            threadDelay 10000000 `onException` putMVar interrupted ()
            pure $ TextResponse "should not happen"
    let agent = mkAgent world (YieldOnTimeout 20) slowCall
    (_, s1) <- stepOk agent (sessionWithCalls [mkCall "call_slow" "slow"])
    [Running] @=? [tc.tcState | p <- partialTurns s1, tc <- p.pTrackedToolCalls]

    -- A fresh agent: same world, no engine.
    let resumed = mkAgent world (YieldOnTimeout 20) slowCall
    Right cancelled <- cancelToolCallById (buildContext resumed s1 convId) (CancelToolCallParams "call_slow" Nothing)
    ccrCancelled cancelled @?= True
    timeout 2000000 (readMVar interrupted) >>= \case
        Just () -> pure ()
        Nothing -> assertFailure "the call's thread kept running after cancel-tool-call"

{- | 'run' cannot return a session, so on a turn that only waits on deferred
calls it used to loop forever. It now stops with 'BlockedOnDeferredCalls'.
-}
runStopsOnDeferredOnly :: Assertion
runStopsOnDeferredOnly = do
    world <- mkWorld
    let agent =
            (mkAgent world YieldWhenAllDone (\_ _ -> pure $ TextResponse "unused"))
                { ctxToolCallPolicy = \_ _ -> Defer (Reason "external")
                }
    res <- timeout 3000000 $ try $ run convId agent (sessionWithCalls [mkCall "call_defer" "defer_me"])
    case res of
        Nothing -> assertFailure "run kept spinning on a deferred-only partial turn"
        Just (Right _) -> assertFailure "expected run to stop on the deferred calls"
        Just (Left (BlockedOnDeferredCalls sess)) -> do
            assertBool "the session is blocked on deferred calls" (isBlockedOnDeferredCalls sess)
            [map tcState p.pTrackedToolCalls | p <- partialTurns sess] @?= [[Deferred]]

{- | The synchronous stepper delivers late results too (a session that holds
background calls from an asynchronous run): same rule.
-}
statusReadNotRepeatedSyncStep :: Assertion
statusReadNotRepeatedSyncStep = do
    (_, gate, a1, s1) <- sessionWithBackgroundCall
    putMVar gate ()
    Right _ <- getToolCallStatus (buildContext a1 s1 convId) (waitForCompletionParams "call_slow")
    (_, res) <- runStepMSync convId a1{ctxExecutionMode = Synchronous} s1
    case res of
        Right s2 -> do
            occurrencesInUserTurns "slow-result" s2 @?= 0
            occurrencesInUserTurns "result already read" s2 @?= 1
            hasBackgroundCalls s2 @?= False
        Left _ -> assertFailure "expected the session to continue"

{- | Mail announcing a call that has no entity in this process (a durable
mailbox read after a restart) is the only carrier of that result, so it is
rendered like any other mail.
-}
foreignToolCallMailRendered :: Assertion
foreignToolCallMailRendered = do
    world <- mkWorld
    mb <- newInMemoryMailbox
    script <- newIORef [[]]
    completions <- newIORef []
    let agent =
            (mkAgent world YieldWhenAllDone (\_ _ -> pure $ TextResponse "unused"))
                { complete = scriptedComplete script completions
                , ctxMailbox = Just mb
                , usrQuery = pure (Just (UserQuery "hello" []))
                , step = askInputWhenIdle
                }
    _ <-
        mb.mbSend
            Outgoing
                { outId = Nothing
                , outFrom = FromToolCall (ToolCallId nil)
                , outPriority = Normal
                , outHops = 0
                , outBody = ToolCallFinished (ToolCallId nil) Completed (TextResponse "survivor-result")
                }
    (_, s1) <- stepOk agent (sessionWithCalls []){turns = []}
    occurrencesInUserTurns "survivor-result" s1 @?= 1

{- | 'runAsync' pauses with a call still running and used to drop the agent
that holds its engine. The agent handed back owns the call: shutting its
engine down stops the call's thread.
-}
runAsyncHandsBackEngine :: Assertion
runAsyncHandsBackEngine = do
    world <- mkWorld
    interrupted <- newEmptyMVar
    let slowCall _ _ = do
            threadDelay 10000000 `onException` putMVar interrupted ()
            pure $ TextResponse "should not happen"
    let agent = mkAgent world (YieldOnTimeout 20) slowCall
    assertBool "the agent starts without an engine" (not (isJustEngine agent))
    (agent', res) <- runAsyncKeepingAgent convId agent (sessionWithCalls [mkCall "call_slow" "slow"])
    case res of
        Right sess -> [Running] @=? [tc.tcState | p <- partialTurns sess, tc <- p.pTrackedToolCalls]
        Left _ -> assertFailure "expected the run to pause"
    case agent'.ctxAsyncEngine of
        Nothing -> assertFailure "expected the returned agent to hold the engine"
        Just engine -> shutdownAsyncEngine engine
    timeout 2000000 (readMVar interrupted) >>= \case
        Just () -> pure ()
        Nothing -> assertFailure "the returned engine does not own the running call"

{- | When a run fails, background calls do not keep running: the engine is
shut down with the agent.
-}
runFailureCancelsBackgroundCalls :: Assertion
runFailureCancelsBackgroundCalls = do
    world <- mkWorld
    interrupted <- newIORef False
    let tool _ _ =
            (threadDelay 10000000 >> pure (TextResponse "should not happen"))
                `onException` writeIORef interrupted True
    let agent =
            (mkAgent world (YieldOnTimeout 20) tool)
                { complete = \_ -> throwIO (userError "llm exploded")
                }
    res <- try $ runUntilBlocked convId agent (sessionWithCalls [mkCall "call_slow" "slow"])
    case res of
        Left (e :: IOException) -> ioeGetErrorString e @?= "llm exploded"
        Right _ -> assertFailure "expected the run to fail"
    readIORef interrupted >>= (@?= True)

-- | A hanging call is given up on and reported as failed.
callTimesOut :: Assertion
callTimesOut = do
    world <- mkWorld
    interrupted <- newIORef False
    let tool _ _ =
            (threadDelay 10000000 >> pure (TextResponse "should not happen"))
                `onException` writeIORef interrupted True
    let agent = (mkAgent world YieldWhenAllDone tool){ctxAsyncCallTimeout = Just 1}
    (_, s1) <- stepOk agent (sessionWithCalls [mkCall "call_slow" "slow"])
    case s1.turns of
        (UserTurn content _ : _) -> case map snd content.userToolResponses of
            [TextResponse txt] -> assertBool ("timeout reported: " <> show txt) ("timed out after 1s" `Text.isInfixOf` txt)
            other -> assertFailure $ "unexpected responses: " <> show other
        _ -> assertFailure "expected the call to finish as failed"
    readIORef interrupted >>= (@?= True)

-------------------------------------------------------------------------------
-- Phase 2 (todos/session-mailbox.md): attach / detach, wait
-------------------------------------------------------------------------------

{- | A 'RunAsync' call with @attachSeconds = 0@ detaches at once: the step
yields a 'PartialUserTurn' with the call still 'Running', marked with a
'tcDetachedReason', instead of blocking on it.
-}
attachSecondsDetaches :: Assertion
attachSecondsDetaches = do
    world <- mkWorld
    gate <- newEmptyMVar -- never filled: the call would otherwise block forever
    let agent =
            (mkAgent world YieldWhenAllDone (gatedToolCall gate))
                { ctxToolCallPolicy = \_ _ -> RunAsync (Just 0)
                }
    (_, s1) <- stepOk agent (sessionWithCalls [mkCall "call_slow" "slow"])
    case partialTurns s1 of
        [partial] -> case partial.pTrackedToolCalls of
            [tc] -> do
                tc.tcState @?= Running
                tc.tcDetachedReason @?= Just "still running after 0s"
            other -> assertFailure $ "expected one tracked call, got " <> show other
        other -> assertFailure $ "expected one partial turn, got " <> show (length other)

{- | An 'Interrupt'-priority envelope pre-empts R3a: an attached ('RunSync')
call is detached even though nothing made it finish, and the interrupt mail
is folded into the same turn.
-}
interruptDetachesAttachedCalls :: Assertion
interruptDetachesAttachedCalls = do
    world <- mkWorld
    mb <- newInMemoryMailbox
    gate <- newEmptyMVar -- never filled: proves the call was detached, not completed
    let agent =
            (mkAgent world YieldWhenAllDone (gatedToolCall gate))
                { ctxToolCallPolicy = \_ _ -> RunSync
                , ctxMailbox = Just mb
                }
    _ <-
        forkIO $ do
            threadDelay 100000
            _ <-
                mb.mbSend
                    Outgoing
                        { outId = Nothing
                        , outFrom = FromUser Nothing
                        , outPriority = Interrupt
                        , outHops = 0
                        , outBody = Control Pause
                        }
            pure ()
    (_, s1) <- stepOk agent (sessionWithCalls [mkCall "call_slow" "slow"])
    case partialTurns s1 of
        [partial] -> do
            case partial.pTrackedToolCalls of
                [tc] -> do
                    tc.tcState @?= Running
                    tc.tcDetachedReason @?= Just "interrupted"
                other -> assertFailure $ "expected one tracked call, got " <> show other
            assertBool "the interrupt mail was folded into the turn" (not (null partial.pUserMail))
        other -> assertFailure $ "expected one partial turn, got " <> show (length other)

{- | A 'Pause' 'Control' message sent to the mailbox while 'runUntilBlocked'
is genuinely blocked inside R3a's wait (on an attached, forever-running
'RunSync' call) stops the loop at its next iteration -- the same way
'System.Agents.Host.Runner''s run loop reacts to 'Control' mail, but here
through 'Loop.runUntilBlocked' itself, which has no 'SessionRunner' of its
own (this is what the TUI drives its conversations with). The attached
call is left running (unlike 'CancelAllAttached'/'CancelCalls'): its gate
is never filled, so a loop that never applied 'Control' mail at all -- as
'runUntilBlocked' used to, before it gained its own 'applyControlMail'
dispatch -- would hang here until the timeout below fires.
-}
runUntilBlockedStopsOnPauseMidWait :: Assertion
runUntilBlockedStopsOnPauseMidWait = do
    world <- mkWorld
    mb <- newInMemoryMailbox
    gate <- newEmptyMVar -- never filled: proves the loop stopped, not that the call finished
    let agent =
            (mkAgent world YieldWhenAllDone (gatedToolCall gate))
                { ctxToolCallPolicy = \_ _ -> RunSync
                , ctxMailbox = Just mb
                }
    _ <-
        forkIO $ do
            threadDelay 100000
            _ <-
                mb.mbSend
                    Outgoing
                        { outId = Nothing
                        , outFrom = FromUser Nothing
                        , outPriority = Normal
                        , outHops = 0
                        , outBody = Control Pause
                        }
            pure ()
    result <- timeout 5000000 $ runUntilBlocked convId agent (sessionWithCalls [mkCall "call_slow" "slow"])
    case result of
        Nothing -> assertFailure "runUntilBlocked did not return: Control Pause mail sent mid-wait was not observed"
        Just (Left _) -> assertFailure "expected the loop to pause with a session, not complete"
        Just (Right sess1) -> case partialTurns sess1 of
            [partial] -> case partial.pTrackedToolCalls of
                [tc] -> tc.tcState @?= Running
                other -> assertFailure $ "expected one tracked call, got " <> show other
            other -> assertFailure $ "expected one partial turn, got " <> show (length other)

{- | A non-'Interrupt' envelope must never detach an attached call: only
'Interrupt' priority pre-empts R3a.
-}
normalMailDoesNotDetach :: Assertion
normalMailDoesNotDetach = do
    world <- mkWorld
    mb <- newInMemoryMailbox
    gate <- newEmptyMVar
    let agent =
            (mkAgent world YieldWhenAllDone (gatedToolCall gate))
                { ctxToolCallPolicy = \_ _ -> RunSync
                , ctxMailbox = Just mb
                }
    _ <-
        forkIO $ do
            threadDelay 100000
            _ <-
                mb.mbSend
                    Outgoing
                        { outId = Nothing
                        , outFrom = FromUser Nothing
                        , outPriority = Normal
                        , outHops = 0
                        , outBody = UserMessage (UserQuery "hello" [])
                        }
            pure ()
    _ <-
        forkIO $ do
            threadDelay 300000
            putMVar gate ()
    (_, s1) <- stepOk agent (sessionWithCalls [mkCall "call_slow" "slow"])
    case s1.turns of
        (UserTurn content _ : _) -> map snd content.userToolResponses @?= [TextResponse "slow-result"]
        other -> assertFailure $ "expected the call to complete normally, got " <> show other

-- | Build a running background call and a context that can watch it, for
-- the @wait@ tests below.
mkWaitFixture :: IO (World, Base.ConversationId, Agent (LlmTurnContent, Session), Session, MVar (), Text)
mkWaitFixture = do
    world <- mkWorld
    gate <- newEmptyMVar
    let agent = (mkAgent world (YieldOnTimeout 0) (gatedToolCall gate))
    (agent', s1) <- stepOk agent (sessionWithCalls [mkCall "call_wait" "slow"])
    providerId <- case partialTurns s1 of
        [partial] -> case partial.pTrackedToolCalls of
            [tc] -> maybe (assertFailure "expected a provider call id" >> fail "unreachable") pure (providerToolCallId tc.tcCall)
            other -> assertFailure ("expected one tracked call, got " <> show other) >> fail "unreachable"
        other -> assertFailure ("expected one partial turn, got " <> show (length other)) >> fail "unreachable"
    pure (world, convId, agent', s1, gate, providerId)

-- | A context built the same way the stepper builds one for tool execution,
-- so @ctxAwaitMail@ is wired exactly as it is at runtime.
waitCtx :: Agent r -> Session -> ToolExecutionContext
waitCtx agent sess = buildContext agent sess convId

waitWakesOnNamedCall :: Assertion
waitWakesOnNamedCall = do
    (_world, _convId, agent, sess, gate, providerId) <- mkWaitFixture
    let ctx = waitCtx agent sess
    resultVar <- newEmptyMVar
    _ <- forkIO $ waitForCallsOrMail ctx (WaitParams providerId 5) >>= putMVar resultVar
    threadDelay 100000
    putMVar gate () -- lets the background call finish
    result <- timeout 3000000 (readMVar resultVar)
    result @?= Just (Right (WaitResult "call" (Just providerId)))

waitWakesOnAnyCall :: Assertion
waitWakesOnAnyCall = do
    (_world, _convId, agent, sess, gate, providerId) <- mkWaitFixture
    let ctx = waitCtx agent sess
    resultVar <- newEmptyMVar
    _ <- forkIO $ waitForCallsOrMail ctx (WaitParams "any-call" 5) >>= putMVar resultVar
    threadDelay 100000
    putMVar gate ()
    result <- timeout 3000000 (readMVar resultVar)
    result @?= Just (Right (WaitResult "call" (Just providerId)))

waitWakesOnMail :: Assertion
waitWakesOnMail = do
    (_world, _convId, agent0, sess, _gate, _providerId) <- mkWaitFixture
    mb <- newInMemoryMailbox
    let agent = agent0{ctxMailbox = Just mb}
    let ctx = waitCtx agent sess
    resultVar <- newEmptyMVar
    _ <- forkIO $ waitForCallsOrMail ctx (WaitParams "mail" 5) >>= putMVar resultVar
    threadDelay 100000
    _ <-
        mb.mbSend
            Outgoing
                { outId = Nothing
                , outFrom = FromUser Nothing
                , outPriority = Normal
                , outHops = 0
                , outBody = UserMessage (UserQuery "hi" [])
                }
    result <- timeout 3000000 (readMVar resultVar)
    result @?= Just (Right (WaitResult "mail" Nothing))
    -- Peeked, not consumed: the mail is still unread on the session's cursor.
    unread <- atomically (mbUnread mb sess.mailCursor)
    assertBool "the mail wait peeked is still unread" (not (null unread))

waitTimesOut :: Assertion
waitTimesOut = do
    (_world, _convId, agent, sess, _gate, _providerId) <- mkWaitFixture
    let ctx = waitCtx agent sess
    result <- timeout 3000000 (waitForCallsOrMail ctx (WaitParams "mail" 0))
    result @?= Just (Right (WaitResult "timeout" Nothing))

clampWaitSecondsCapsRequest :: Assertion
clampWaitSecondsCapsRequest = do
    clampWaitSeconds 10000 @?= 300
    clampWaitSeconds (-5) @?= 0
    clampWaitSeconds 30 @?= 30

{- | 'pollRunningCall' copies a child session id from a call's OS entity onto
the tracked call while it is still 'Running', once
'System.Agents.OS.Conversation.ToolCalls.recordChildSession' has written it
(the async engine's 'ctxRecordChildSession' hook does this for a
@prompt_agent_\<slug\>@ call once its child session exists).
-}
pollRunningCallCopiesChildSessionId :: Assertion
pollRunningCallCopiesChildSessionId = do
    world <- mkWorld
    sid <- newSessionId
    tid <- newTurnId
    callId <- newToolCallId
    eid <- createToolCallEntity world sid convId tid Nothing "prompt_agent_helper" Null callId
    childSid <- newSessionId
    recordChildSession world eid childSid
    let agent = mkAgent world YieldWhenAllDone (\_ _ -> pure (TextResponse "unused"))
    sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hi" [])
    let ctx = buildContext agent sess convId
        tc = (trackedCall Running){tcId = callId, tcEntityId = Just eid}
    tc' <- pollRunningCall ctx tc
    tc'.tcChildSessionId @?= Just childSid
    tc'.tcState @?= Running

-- | An ordinary call's OS entity never has a recorded child session, so
-- 'pollRunningCall' leaves 'tcChildSessionId' absent.
pollRunningCallLeavesChildSessionIdAbsent :: Assertion
pollRunningCallLeavesChildSessionIdAbsent = do
    world <- mkWorld
    sid <- newSessionId
    tid <- newTurnId
    callId <- newToolCallId
    eid <- createToolCallEntity world sid convId tid Nothing "bash_command" Null callId
    let agent = mkAgent world YieldWhenAllDone (\_ _ -> pure (TextResponse "unused"))
    sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hi" [])
    let ctx = buildContext agent sess convId
        tc = (trackedCall Running){tcId = callId, tcEntityId = Just eid}
    tc' <- pollRunningCall ctx tc
    tc'.tcChildSessionId @?= Nothing

-- | A detached (@running@) placeholder includes @childSessionId@ when the
-- tracked call has one, so the caller can @send-message@ a helper that is
-- still working.
placeholderIncludesChildSessionId :: Assertion
placeholderIncludesChildSessionId = do
    childSid <- newSessionId
    let tc = (trackedCall Running){tcChildSessionId = Just childSid}
        content = PartialUserTurnContent (SystemPrompt "sys") [] Nothing [tc] [] False
    case partialToolMessages content of
        [(_, JsonResponse (Object obj))] ->
            KeyMap.lookup "childSessionId" obj @?= Just (toJSON childSid)
        other -> assertFailure $ "expected one JSON placeholder, got " <> show other

-- | An ordinary detached call's placeholder has no @childSessionId@ key.
placeholderOmitsChildSessionIdByDefault :: Assertion
placeholderOmitsChildSessionIdByDefault = do
    let tc = trackedCall Running
        content = PartialUserTurnContent (SystemPrompt "sys") [] Nothing [tc] [] False
    case partialToolMessages content of
        [(_, JsonResponse (Object obj))] ->
            assertBool "no childSessionId key" (not (KeyMap.member "childSessionId" obj))
        other -> assertFailure $ "expected one JSON placeholder, got " <> show other

processReportsOutput :: Assertion
processReportsOutput = do
    reports <- newIORef []
    let report v = modifyIORef' reports (v :)
    (code, out, err) <-
        runProcessReportingOutput report (proc "sh" ["-c", "cat; echo one; echo two >&2; echo three"]) "input\n"
    code @?= ExitSuccess
    out @?= "input\none\nthree\n"
    err @?= "two\n"
    payloads <- reverse <$> readIORef reports
    assertBool "some progress was reported" (not (null payloads))
    case payloads of
        (Object first : _) -> do
            assertBool "stream is named" (KeyMap.member "stream" first)
            assertBool "line is included" (KeyMap.member "line" first)
        other -> assertFailure $ "unexpected payloads: " <> show other

{- | A background call running a subprocess is cancelled; the process is
terminated before it can write its marker file.
-}
processKilledOnCancel :: Assertion
processKilledOnCancel =
    withSystemTempDirectory "async-cancel" $ \dir -> do
        world <- mkWorld
        let marker = dir </> "finished"
        let tool _ _ = do
                (_, out, _) <- runProcessReportingOutput (\_ -> pure ()) (proc "sh" ["-c", "sleep 1 && touch " <> marker]) ""
                pure $ TextResponse (Text.pack (show out))
        let agent = mkAgent world (YieldOnTimeout 20) tool
        (a1, s1) <- stepOk agent (sessionWithCalls [mkCall "call_proc" "proc"])
        Right cancelled <- cancelToolCallById (buildContext a1 s1 convId) (CancelToolCallParams "call_proc" Nothing)
        ccrCancelled cancelled @?= True
        threadDelay 1500000
        doesFileExist marker >>= assertBool "process should have been terminated" . not

{- | A script that spawns a child is cancelled: the child dies with the
script's process group, so neither marker file is written.
-}
processGroupKilledOnCancel :: Assertion
processGroupKilledOnCancel =
    withSystemTempDirectory "async-cancel-group" $ \dir -> do
        world <- mkWorld
        let parentMarker = dir </> "parent"
            childMarker = dir </> "child"
            script = "sh -c 'sleep 1; touch " <> childMarker <> "' & sleep 1; touch " <> parentMarker
        let tool _ _ = do
                _ <- runProcessReportingOutput (\_ -> pure ()) (proc "sh" ["-c", script]) ""
                pure $ TextResponse "done"
        let agent = mkAgent world (YieldOnTimeout 20) tool
        (a1, s1) <- stepOk agent (sessionWithCalls [mkCall "call_proc" "proc"])
        Right cancelled <- cancelToolCallById (buildContext a1 s1 convId) (CancelToolCallParams "call_proc" Nothing)
        ccrCancelled cancelled @?= True
        threadDelay 1500000
        doesFileExist parentMarker >>= assertBool "script should have been killed" . not
        doesFileExist childMarker >>= assertBool "child process should have been killed" . not

markdownPartialTurn :: Assertion
markdownPartialTurn = do
    let done = (trackedCall Completed){tcCall = mkCall "call_done" "done", tcResult = Just (TextResponse "done-output")}
        running = (trackedCall Running){tcCall = mkCall "call_run" "run"}
        partial = PartialUserTurnContent (SystemPrompt "p") [] Nothing [done, running] [] False
        sess = (sessionWithCalls []){turns = [PartialUserTurn partial Nothing]}
        opts =
            SessionPrintOptions
                { sessionPrintFile = ""
                , showToolCallResults = ShownFull
                , showToolCallArguments = Hidden
                , nTurns = Nothing
                , repeatSystemPrompt = False
                , repeatTools = False
                , orderPreference = Chronological
                , noFunnyStamp = True
                }
        md = formatSessionAsMarkdown opts sess
    assertBool "running call listed" ("`run` (`call_run`): running" `Text.isInfixOf` md)
    assertBool "completed call listed" ("`done` (`call_done`): completed" `Text.isInfixOf` md)
    assertBool "finished result shown" ("done-output" `Text.isInfixOf` md)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{- | A session whose LLM ended its turn while a gated @slow@ call runs in the
background (strategy 'YieldOnTimeout', so the step yields while it runs).
-}
sessionWithBackgroundCall :: IO (World, MVar (), Agent (LlmTurnContent, Session), Session)
sessionWithBackgroundCall = do
    world <- mkWorld
    gate <- newEmptyMVar
    script <- newIORef [[mkCall "call_slow" "slow"], []]
    completions <- newIORef []
    let agent = (mkAgent world (YieldOnTimeout 20) (gatedToolCall gate)){complete = scriptedComplete script completions}
    (a1, s1) <- stepOk agent initialSession
    (a2, s2) <- stepOk a1 s1 -- starts the call; partial turn
    (a3, s3) <- stepOk a2 s2 -- LLM sees the placeholder, ends its turn
    case s3.turns of
        (LlmTurn _ _ : PartialUserTurn _ _ : _) -> pure ()
        _ -> assertFailure "expected an LLM turn over a partial turn"
    pure (world, gate, a3, s3)

-- | Like the TUI: ask for user input whenever the stepper would wait.
askInputWhenIdle :: Session -> IO (Action (LlmTurnContent, Session))
askInputWhenIdle sess =
    naiveTilNoToolCallStep sess >>= \case
        AskUserPrompt (MissingUserPrompt False []) -> pure $ AskUserPrompt (MissingUserPrompt True [])
        other -> pure other

activity :: UTCTime -> ToolCallPhase -> ToolCallActivity
activity t phase =
    ToolCallActivity
        { tcaSessionId = SessionId nil
        , tcaConversationId = convId
        , tcaToolCallId = ToolCallId nil
        , tcaProviderCallId = Just "call_x"
        , tcaToolName = "x"
        , tcaPhase = phase
        , tcaAt = t
        }

trackedCall :: ToolCallState -> TrackedToolCall
trackedCall st =
    TrackedToolCall
        { tcId = ToolCallId nil
        , tcCall = mkCall "call_x" "x"
        , tcState = st
        , tcResult = Nothing
        , tcContinuation = Nothing
        , tcPolicy = AppliedPolicy (RunAsync Nothing) Nothing
        , tcEntityId = Nothing
        , tcDeliveredLate = False
        , tcAttachDeadline = Nothing
        , tcDetachedReason = Nothing
        , tcChildSessionId = Nothing
        }

partialTurns :: Session -> [PartialUserTurnContent]
partialTurns sess = [p | PartialUserTurn p _ <- sess.turns]

convId :: Base.ConversationId
convId = Base.ConversationId nil

mkWorld :: IO World
mkWorld = atomically $ newWorld >>= registerToolCallComponents

-- | Run one async step and expect a session back.
stepOk :: Agent r -> Session -> IO (Agent r, Session)
stepOk agent sess = do
    (agent', res) <- runStepMAsync convId agent sess
    case res of
        Right sess' -> pure (agent', sess')
        Left _ -> assertFailure "expected the session to continue" >> pure (agent', sess)

isJustEngine :: Agent r -> Bool
isJustEngine a = case a.ctxAsyncEngine of
    Just _ -> True
    Nothing -> False

-- | An OpenAI-style tool call with a provider id.
mkCall :: Text -> Text -> LlmToolCall
mkCall providerId name =
    LlmToolCall $
        object
            [ "id" .= providerId
            , "type" .= ("function" :: Text)
            , "function" .= object ["name" .= name, "arguments" .= ("{}" :: Text)]
            ]

callName :: LlmToolCall -> Text
callName (LlmToolCall (Object obj)) =
    case KeyMap.lookup "function" obj of
        Just (Object func) | Just (String n) <- KeyMap.lookup "name" func -> n
        _ -> "unknown"
callName _ = "unknown"

-- | Tool implementation where @slow@ blocks until the gate is filled.
gatedToolCall :: MVar () -> a -> LlmToolCall -> IO UserToolResponse
gatedToolCall gate _ call =
    case callName call of
        "slow" -> readMVar gate >> pure (TextResponse "slow-result")
        name -> pure $ TextResponse (name <> "-result")

-- | LLM stub returning scripted tool calls and recording each completion.
scriptedComplete :: IORef [[LlmToolCall]] -> IORef [LlmCompletion] -> LlmCompletion -> IO (LlmResponse, [LlmToolCall])
scriptedComplete script completions completion = do
    modifyIORef' completions (completion :)
    calls <- atomicModifyIORef' script $ \case
        (c : rest) -> (rest, c)
        [] -> ([], [])
    pure (LlmResponse (Just "ok") Nothing Null Nothing, calls)

lastCompletion :: IORef [LlmCompletion] -> IO LlmCompletion
lastCompletion ref =
    readIORef ref >>= \case
        (c : _) -> pure c
        [] -> assertFailure "expected a completion" >> fail "unreachable"

{- | Every assistant message with tool calls must be followed by exactly one
tool message per call, as OpenAI-compatible APIs require.
-}
assertToolMessagesPaired :: LlmCompletion -> Assertion
assertToolMessagesPaired completion =
    go (reverse completion.completeConversationHistory)
  where
    go (LlmTurn llm _ : next : rest) = do
        ids llm.llmToolCalls @?= responseIds next
        go (next : rest)
    go [LlmTurn llm _] = ids llm.llmToolCalls @?= ids (map fst completion.completeToolResponses)
    go (_ : rest) = go rest
    go [] = pure ()

    ids = mapMaybe providerToolCallId
    responseIds (UserTurn content _) = ids (map fst content.userToolResponses)
    responseIds (PartialUserTurn content _) = ids (map fst (partialToolMessages content))
    responseIds (LlmTurn _ _) = []

statusParams :: Text -> GetToolCallStatusParams
statusParams tid = GetToolCallStatusParams tid True False 0

initialSession :: Session
initialSession =
    (sessionWithCalls [])
        { turns =
            [ UserTurn (UserTurnContent (SystemPrompt "test") [] (Just (UserQuery "go" [])) [] []) Nothing
            ]
        }

llmTurnWith :: [LlmToolCall] -> Turn
llmTurnWith calls = LlmTurn (LlmTurnContent (LlmResponse Nothing Nothing Null Nothing) calls) Nothing

sessionWithCalls :: [LlmToolCall] -> Session
sessionWithCalls calls =
    Session
        { turns = [llmTurnWith calls]
        , sessionId = SessionId nil
        , forkedFromSessionId = Nothing
        , turnId = TurnId nil
        , sessionVersion = Just 2
        , sessionExecutionMode = Just Asynchronous
        , mailCursor = 0
        }

dummyPortal :: ToolPortal
dummyPortal _ _ = pure $ ToolResult (object []) 0 "dummy"

mkAgent ::
    World ->
    AsyncYieldStrategy ->
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    Agent (LlmTurnContent, Session)
mkAgent world strategy tool =
    Agent
        { step = naiveTilNoToolCallStep
        , sysPrompt = pure $ SystemPrompt "test"
        , sysTools = pure []
        , usrQuery = pure Nothing
        , toolCall = tool
        , toolPortal = dummyPortal
        , complete = \_ -> pure (LlmResponse Nothing Nothing Null Nothing, [])
        , contextConfig = defaultContextConfig
        , ctxWorld = Just world
        , ctxEmit = Nothing
        , ctxCallStack = []
        , ctxParentConversation = Nothing
        , ctxExecutionMode = Asynchronous
        , ctxAsyncYieldStrategy = strategy
        , ctxMaxConcurrency = Nothing
        , ctxAsyncCallTimeout = Nothing
        , ctxToolCache = Nothing
        , ctxToolCallPolicy = \_ _ -> RunAsync Nothing
        , ctxToolExecutor = Nothing
        , ctxContinuationStore = Nothing
        , ctxDeploymentRunner = Nothing
        , ctxSessionBackend = Nothing
        , ctxAsyncEngine = Nothing
        , ctxAsyncTracer = silent
        , ctxParams = mempty
        , ctxInheritedBindings = []
        , ctxMailbox = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxRunSubagent = Nothing
        , ctxInterruptCompletions = False
        , ctxMailInToolResult = False
        }
