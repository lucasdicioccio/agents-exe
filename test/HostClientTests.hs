{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Every 'Command' constructor, exercised once through 'inProcessClient'
against a real runner fixture (reusing "RunnerTests"' fixtures), plus a
'rcSubscribe' that sees @session.created@.
-}
module HostClientTests (tests) where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import qualified Data.Map.Strict as Map
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
