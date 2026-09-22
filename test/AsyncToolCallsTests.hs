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
import Control.Concurrent.STM (atomically, flushTQueue, newTQueueIO)
import Control.Exception (IOException, onException, throwIO, try)
import Control.Monad (void)
import Data.Aeson (Value (..), object, (.=))
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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@=?), (@?=))

import qualified System.Agents.Base as Base
import System.Agents.OS.Conversation.ToolCalls (registerToolCallComponents)
import System.Agents.OS.Core.Types (EntityId (..))
import System.Agents.OS.Core.World (World, newWorld)
import System.Agents.OS.Events (OSEvent (..), ToolCallActivity (..), ToolCallPhase (..))
import System.Agents.Session.Base
import System.Agents.SessionStore (readSessionFromFile, storeSessionToFile)
import System.Agents.SessionPrint (OrderPreference (..), PrintVisibility (..), SessionPrintOptions (..), formatSessionAsMarkdown)
import System.Agents.Session.Loop (isBlockedOnDeferredCalls, runUntilBlocked)
import System.Agents.Session.Step (buildContext, naiveTilNoToolCallStep, runStepMAsync)
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
import System.Agents.Tools.SystemToolbox.ToolCallStatus (cancelToolCallById, getToolCallStatus, listRunningToolCalls)
import System.Agents.Tools.SystemToolbox.Types (
    CancelToolCallParams (..),
    CancelToolCallResult (..),
    GetToolCallStatusParams (..),
    ListRunningToolCallsResult (..),
    RunningToolCallInfo (..),
    ToolCallStatusResult (..),
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
        , testCase "a failing run cancels background calls" runFailureCancelsBackgroundCalls
        , testCase "a call that outlives ctxAsyncCallTimeout fails" callTimesOut
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
                , tcPolicy = AppliedPolicy RunAsync Nothing
                , tcEntityId = Just (EntityId UUID.nil)
                , tcDeliveredLate = False
                }
    let partial = PartialUserTurnContent (SystemPrompt "test") [] Nothing [tracked] []
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
            | otherwise = RunAsync
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

-- | Start, progress, and completion reach the OS event queue.
engineEmitsActivity :: Assertion
engineEmitsActivity = do
    world <- mkWorld
    queue <- newTQueueIO
    let tool ctx _ = do
            mapM_ ($ String "halfway") (Ctx.ctxProgressCallback ctx)
            pure $ TextResponse "done"
    let agent = (mkAgent world YieldWhenAllDone tool){ctxEventQueue = Just queue}
    _ <- stepOk agent (sessionWithCalls [mkCall "call_a" "a"])
    events <- atomically $ flushTQueue queue
    [(act.tcaProviderCallId, act.tcaToolName, act.tcaPhase) | OSEvent_ToolCallActivity act <- events]
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
        stillRunning = (sessionWithCalls []){turns = [PartialUserTurn (PartialUserTurnContent (SystemPrompt "p") [] Nothing [runningCall] []) Nothing]}
        caughtUp = (sessionWithCalls []){turns = [PartialUserTurn (PartialUserTurnContent (SystemPrompt "p") [] Nothing [trackedCall Completed] []) Nothing]}
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
            | otherwise = RunAsync
    let agent = (mkAgent world (YieldOnTimeout 20) (gatedToolCall gate)){ctxToolCallPolicy = policy}
    void $ forkIO $ threadDelay 50000 >> putMVar gate ()
    res <- timeout 5000000 $ runUntilBlocked convId agent (sessionWithCalls [mkCall "call_defer" "defer_me", mkCall "call_slow" "slow"])
    case res of
        Nothing -> assertFailure "runUntilBlocked did not return"
        Just (Left _) -> assertFailure "expected a blocked session"
        Just (Right sess) -> do
            assertBool "blocked on deferred calls" (isBlockedOnDeferredCalls sess)
            [map tcState p.pTrackedToolCalls | p <- take 1 (partialTurns sess)] @?= [[Deferred, Completed]]

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
        partial = PartialUserTurnContent (SystemPrompt "p") [] Nothing [done, running] []
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
        , tcPolicy = AppliedPolicy RunAsync Nothing
        , tcEntityId = Nothing
        , tcDeliveredLate = False
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
        , ctxEventQueue = Nothing
        , ctxCallStack = []
        , ctxParentConversation = Nothing
        , ctxExecutionMode = Asynchronous
        , ctxAsyncYieldStrategy = strategy
        , ctxMaxConcurrency = Nothing
        , ctxAsyncCallTimeout = Nothing
        , ctxToolCache = Nothing
        , ctxToolCallPolicy = \_ _ -> RunAsync
        , ctxToolExecutor = Nothing
        , ctxContinuationStore = Nothing
        , ctxDeploymentRunner = Nothing
        , ctxSessionBackend = Nothing
        , ctxAsyncEngine = Nothing
        , ctxParams = mempty
        , ctxInheritedBindings = []
        , ctxMailbox = Nothing
        }
