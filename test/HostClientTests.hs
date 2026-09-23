{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Every 'Command' constructor, exercised once through 'inProcessClient'
against a real runner fixture (reusing "RunnerTests"' fixtures), plus a
'rcSubscribe' that sees @session.created@.
-}
module HostClientTests (tests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, testNode)
import RunnerTests (
    backgroundAll,
    currentSession,
    deferAll,
    expectRight,
    firstThen,
    gatedTool,
    hasBackgroundCall,
    message,
    onceThen,
    remoteCall,
    responseTexts,
    sessionTexts,
    singleToken,
    slowCall,
    testHost,
    waitUntil,
 )
import System.Agents.AgentTree (OSAgentNode (..))
import System.Agents.Host.Client
import System.Agents.Host.Runner (withSessionRunner)
import System.Agents.Protocol
import qualified System.Agents.Session.Base as SessionBase
import System.Agents.Session.Base (
    ControlMsg (StopRun),
    MailBody (AgentMessage, Control),
    Priority (..),
    SessionStatus (..),
    UserToolResponse (..),
 )
import System.Agents.SessionStore (SessionMeta (..), allSessionsQuery)

tests :: TestTree
tests =
    testGroup
        "Host.Client (inProcessClient)"
        [ testCase "CreateSession, PostMessage, GetSession, ListSessions, ListAgents, GetAgent" createAndListTest
        , testCase "SpawnSession creates a child, lineage only" spawnSessionTest
        , testCase "Resume runs an idle session to an answer" resumeTest
        , testCase "CompleteCall answers a deferred call" completeCallTest
        , testCase "Pause, then SendMail Control Resume, then CancelAttached, CancelRun" pauseMailCancelTest
        , testCase "SendMail/ListMail generic mail, ForkSession, DeleteSession" mailForkDeleteTest
        , testCase "AwaitRun waits for the run to stop; Stats reports load" awaitStatsTest
        , testCase "rcSubscribe sees session.created" subscribeSeesSessionCreatedTest
        , testCase "the TUI's own command sequence, in order, over one subscription" tuiSequenceTest
        , testCase "PostMessage while running is accepted as mail (§5, D3: what a busy draft ships into)" postWhileRunningBecomesMailTest
        , testCase "the TUI's draft-ship sequence: append while running, post one message on run.stopped" tuiDraftShipSequenceTest
        ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

createAndListTest :: Assertion
createAndListTest = do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient (Just "alice") runner

        -- CreateSession, with a first message: runs to an answer with 'Just UntilBlocked'.
        meta1 <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        (meta1', _) <- expectRight =<< awaitRun client meta1.smSessionId 5
        meta1'.smStatus @?= StatusIdle

        -- CreateSession, no message (G2): a ready session with no turn, no run.
        meta2 <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        meta2.smStatus @?= StatusReady

        -- PostMessage on 'meta1'' (StatusIdle, it already has an answer): a
        -- follow-up message, run to completion.
        meta3 <- expectRight =<< postMessage client meta1'.smSessionId (message "now go") (Just UntilBlocked) Map.empty
        (meta3', _) <- expectRight =<< awaitRun client meta3.smSessionId 5
        meta3'.smStatus @?= StatusIdle

        -- GetSession.
        (_, gotMeta) <- expectRight =<< getSession client meta1.smSessionId
        gotMeta.smSessionId @?= meta1.smSessionId

        -- ListSessions.
        listed <- expectRight =<< listSessions client allSessionsQuery
        length listed >= 2 @?= True

        -- ListAgents / GetAgent.
        agents <- expectRight =<< listAgents client
        map (.adSlug) agents @?= ["test-agent"]
        agent <- expectRight =<< getAgent client "test-agent"
        agent.adSlug @?= "test-agent"
        missing <- getAgent client "no-such-agent"
        missing @?= Left (UnknownAgent "no-such-agent")

spawnSessionTest :: Assertion
spawnSessionTest = do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient Nothing runner
        parentMeta <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        childMeta <- expectRight =<< spawnSession client parentMeta.smSessionId "test-agent" (message "helper, go")
        childMeta.smParent @?= Just parentMeta.smSessionId

resumeTest :: Assertion
resumeTest = do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient Nothing runner
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) Nothing Map.empty
        meta.smStatus @?= StatusReady
        _ <- expectRight =<< resumeSession client meta.smSessionId UntilBlocked Map.empty
        (resumed, _) <- expectRight =<< awaitRun client meta.smSessionId 5
        resumed.smStatus @?= StatusIdle

completeCallTest :: Assertion
completeCallTest = do
    node <- testNode deferAll
    host <- testHost [node] (\_ c -> firstThen [remoteCall "call_1"] c)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient Nothing runner
        meta <- expectRight =<< createSession client "test-agent" (Just (message "fetch it")) (Just UntilBlocked) Map.empty
        (blocked, active) <- expectRight =<< awaitRun client meta.smSessionId 5
        (blocked.smStatus, active) @?= (StatusWaitingExternal, False)
        token <- singleToken runner meta.smSessionId
        _ <- expectRight =<< completeCall client token (TextResponse "42") True Map.empty
        (final2, _) <- expectRight =<< awaitRun client meta.smSessionId 5
        final2.smStatus @?= StatusIdle
        sess <- currentSession runner meta.smSessionId
        assertBool "tool result delivered" ("42" `elem` responseTexts sess)

pauseMailCancelTest :: Assertion
pauseMailCancelTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    complete <- onceThen [slowCall]
    host <- testHost [node] (const complete)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient Nothing runner
        meta <- expectRight =<< createSession client "test-agent" (Just (message "go")) (Just UntilBlocked) Map.empty
        let sid = meta.smSessionId

        -- The LLM answers with a placeholder for the call, then the run
        -- waits for the call's late result: still an active run.
        waitUntil $ hasBackgroundCall <$> currentSession runner sid

        -- Pause and CancelAttached (SendMail Control, under the hood) work
        -- whether or not a run is active; here one is.
        paused <- expectRight =<< pauseSession client sid
        paused.smSessionId @?= sid
        afterAttached <- expectRight =<< cancelAttachedCalls client sid
        afterAttached.smSessionId @?= sid

        -- Hard-cancel the still-active run.
        cancelled <- expectRight =<< cancelRun client sid
        cancelled.smStatus @?= StatusReady
        again <- cancelRun client sid
        again @?= Left (NoActiveRun sid)

mailForkDeleteTest :: Assertion
mailForkDeleteTest = do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient Nothing runner
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        _receipt <- expectRight =<< sendMail client meta.smSessionId Normal (AgentMessage "note" Nothing False)
        mail <- expectRight =<< listMail client meta.smSessionId False
        assertBool "mail was recorded" (not (null mail))
        forked <- expectRight =<< forkSession client meta.smSessionId Nothing Nothing
        (forkedSess, _) <- expectRight =<< getSession client forked.smSessionId
        forkedSess.forkedFromSessionId @?= Just meta.smSessionId
        plan <- expectRight =<< deleteSession client forked.smSessionId DeleteForReal
        plan.dpSessions @?= [forked.smSessionId]

awaitStatsTest :: Assertion
awaitStatsTest = do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient Nothing runner
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        (final, active) <- expectRight =<< awaitRun client meta.smSessionId 5
        (final.smStatus, active) @?= (StatusIdle, False)
        s <- expectRight =<< stats client
        s.rsLiveSessions >= 1 @?= True

subscribeSeesSessionCreatedTest :: Assertion
subscribeSeesSessionCreatedTest = do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient Nothing runner
        sub <- expectRight =<< subscribeAll client
        meta <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        -- 'store' emits its own 'session.updated' before the caller's
        -- explicit 'session.created'; skip past anything else until we see it.
        let findCreated = do
                ev <- subNext sub
                case ev.evBody of
                    SessionCreated createdMeta -> pure createdMeta
                    _ -> findCreated
        createdMeta <- findCreated
        createdMeta.smSessionId @?= meta.smSessionId
        subClose sub

{- | Mirrors exactly the sequence "System.Agents.TUI.Core" and
"System.Agents.TUI.Event.Conversation" issue against a 'RunnerClient', end
to end, without a real LLM ('mockCompletion' answers immediately): a
promptless create (G2), 'subscribeAll' (the event bridge), a
'postMessage' observed as @run.started@ / @session.updated@ / @run.stopped@
in that order, an interrupting 'postMessage' (@nmInterrupt = True@), a
pause then a resume (the chat page's 'UntilBlocked'), a fork at turn 0, and
finally the @StopRun@ mail quitting the TUI sends every owned, still-running
session.
-}
tuiSequenceTest :: Assertion
tuiSequenceTest = do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient (Just "tui") runner

        sub <- expectRight =<< subscribeAll client

        -- Promptless create (G2): a ready session with no turn, no run.
        meta0 <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        meta0.smStatus @?= StatusReady
        let sid = meta0.smSessionId

        -- Send: PostMessage, which the runner runs to an answer.
        _ <- expectRight =<< postMessage client sid (message "hi") (Just UntilBlocked) Map.empty
        awaitKind sub "run.started"
        awaitKind sub "session.updated"
        (final1, _) <- expectRight =<< awaitRun client sid 5
        final1.smStatus @?= StatusIdle
        awaitKind sub "run.stopped"

        -- Interrupt: PostMessage with nmInterrupt.
        _ <- expectRight =<< postMessage client sid (NewMessage "more" [] True) (Just UntilBlocked) Map.empty
        (final2, _) <- expectRight =<< awaitRun client sid 5
        final2.smStatus @?= StatusIdle

        -- Pause, then resume with the chat page's mode (UntilBlocked). A
        -- pause posted while idle only queues a 'Control' 'Pause' envelope
        -- (nothing is running to act on it -- 'sendMail's own Haddock);
        -- 'resumeSession' still starts a run, which folds that queued
        -- envelope in at its first receive point, so the run may re-pause
        -- itself immediately. Either way, both calls must succeed.
        paused <- expectRight =<< pauseSession client sid
        paused.smSessionId @?= sid
        _ <- expectRight =<< resumeSession client sid UntilBlocked Map.empty
        (final3, _) <- expectRight =<< awaitRun client sid 5
        assertBool ("resumed to " <> show final3.smStatus) (final3.smStatus `elem` [StatusIdle, StatusPaused])

        -- Fork at turn 0 (newest-first index), keeping the source's agent.
        forked <- expectRight =<< forkSession client sid (Just 0) Nothing
        forked.smParent @?= meta0.smParent

        -- Quit: StopRun mail to every owned session that is still running
        -- (none is, here -- the sequence only checks the call succeeds the
        -- way "System.Agents.TUI.Event".stopConversations issues it).
        _ <- expectRight =<< sendMail client sid Interrupt (Control StopRun)

        subClose sub
  where
    awaitKind sub wanted = do
        ev <- subNext sub
        if eventKind ev.evBody == wanted then pure () else awaitKind sub wanted

{- | The runner behaviour the TUI's draft (@todos/os-as-standalone-server.md@
§5, D3) relies on: 'postMessage' sent while a session is 'StatusRunning' is
accepted, not refused, and folded in as mail rather than added as a turn
right away -- this is exactly what lets 'System.Agents.TUI.Event.Conversation.sendMessageTo'
treat "session busy" and "post now" as the same call on the client's side,
appending to the draft only to avoid three separate 'postMessage' calls in
a row.
-}
postWhileRunningBecomesMailTest :: Assertion
postWhileRunningBecomesMailTest = do
    gate <- newEmptyMVar
    node <- testNode "{}"
    host <- testHost [node] (\_ c -> readMVar gate >> mockCompletion c)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient (Just "tui") runner
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hello")) (Just UntilBlocked) Map.empty
        let sid = meta.smSessionId
        waitUntil $ (== StatusRunning) . (.smStatus) . snd <$> (expectRight =<< getSession client sid)
        accepted <- expectRight =<< postMessage client sid (message "too early") (Just UntilBlocked) Map.empty
        accepted.smStatus @?= StatusRunning
        putMVar gate ()
        (final, _) <- expectRight =<< awaitRun client sid 5
        final.smStatus @?= StatusIdle

{- | Mirrors the exact sequence 'sendMessageTo'\/'handleRunStopped' issue for
a draft (§5): while the session is running, the TUI never calls
'postMessage' for a second and third message typed in a row -- it only
appends them to a local 'Draft' and posts once, when @run.stopped@ reports
a status that accepts input ('shouldShipDraft'). This exercises only the
'RunnerClient' side of that: one 'postMessage' call standing in for the
whole appended draft, sent only after the run that was in progress when
drafting started has actually stopped.
-}
tuiDraftShipSequenceTest :: Assertion
tuiDraftShipSequenceTest = do
    gate <- newEmptyMVar
    node <- testNode "{}"
    host <- testHost [node] (\_ c -> readMVar gate >> mockCompletion c)
    withSessionRunner host $ \runner -> do
        let client = inProcessClient (Just "tui") runner
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hello")) (Just UntilBlocked) Map.empty
        let sid = meta.smSessionId
        waitUntil $ (== StatusRunning) . (.smStatus) . snd <$> (expectRight =<< getSession client sid)

        -- The TUI would have appended "more detail" and "and one more
        -- thing" to the draft locally here, without calling postMessage --
        -- nothing to assert against the client for that part (D3: a draft
        -- never crosses the wire).

        -- run.stopped, accepting input: the TUI ships the whole draft as
        -- one postMessage.
        putMVar gate ()
        (idle, _) <- expectRight =<< awaitRun client sid 5
        idle.smStatus @?= StatusIdle
        _ <- expectRight =<< postMessage client sid (message "more detail\n\nand one more thing") (Just UntilBlocked) Map.empty
        (final, _) <- expectRight =<< awaitRun client sid 5
        final.smStatus @?= StatusIdle
        (sess, _) <- expectRight =<< getSession client sid
        let texts = sessionTexts sess
        assertBool ("draft text reached the session: " <> show texts) (any ("more detail" `Text.isInfixOf`) texts && any ("and one more thing" `Text.isInfixOf`) texts)
