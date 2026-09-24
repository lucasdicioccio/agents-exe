{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | 'httpClient' against a real @agents-server@ application (the same
'AgentsServer.Api.application' @agents-exe serve@ runs), on a random TCP
port or a Unix socket, over a runner built from "RunnerTests"' fixtures:
every 'Command' constructor, the TUI's own sequences from
"HostClientTests", event-stream reconnection with @Last-Event-ID@, replay,
bearer tokens, and the SSE parser.
-}
module HttpClientTests (tests) where

import Control.Concurrent.Async (race_, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM
import Control.Exception (bracket, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Socket as NS
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, testNode)
import AgentsServer.Api (AuthTokens, ServerEnv (..), application, newServerEnv)
import AgentsServer.Auth (authTokensFromList)
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
import System.Agents.AgentFactory (Completion)
import System.Agents.AgentTree (OSAgentNode (..))
import System.Agents.Host.Client
import System.Agents.Host.Client.Http
import System.Agents.Host.Runner (ReplayUnavailable (..), SessionRunner, withSessionRunner)
import qualified System.Agents.Host.Runner as Runner
import System.Agents.Protocol
import System.Agents.Session.Base (
    ControlMsg (StopRun),
    MailBody (AgentMessage, Control),
    Priority (..),
    Session (..),
    SessionStatus (..),
    UserToolResponse (..),
    dcvToken,
 )
import System.Agents.SessionStore (SessionMeta (..), SessionQuery (..), allSessionsQuery)

tests :: TestTree
tests =
    testGroup
        "Host.Client.Http (httpClient)"
        [ testCase "parseEndpoint: http, https, unix://, unix:, a bare path, and refusals" parseEndpointTest
        , testCase "feedSse: frames split across chunks, CRLF, comments, multi-line data" sseParserTest
        , testCase "CreateSession, PostMessage, GetSession, ListSessions, ListAgents, GetAgent" createAndListTest
        , testCase "CreateSession with a parent, SpawnSession" childSessionsTest
        , testCase "Resume runs an idle session to an answer" resumeTest
        , testCase "CompleteCall answers a deferred call" completeCallTest
        , testCase "Pause, CancelAttached, CancelRun (and NoActiveRun names the session)" pauseCancelTest
        , testCase "SendMail/ListMail, ForkSession, DeleteSession (dry run and real)" mailForkDeleteTest
        , testCase "AwaitRun waits for the run to stop; Stats reports load" awaitStatsTest
        , testCase "errors: unknown session, agent, token, turn keep what the command named" errorsTest
        , testCase "a server that is not there is a TransportError, and subscribing throws" unreachableTest
        , testCase "the TUI's sequence over one subscription" tuiSequenceTest
        , testCase "the TUI's draft-ship sequence" tuiDraftShipSequenceTest
        , testCase "the TUI's pending-call sequence" tuiPendingCallSequenceTest
        , testCase "rcSubscribe: replay from a seen seq; ReplayUnavailable past the ring" replayTest
        , testCase "OneSession subscription skips the snapshot" oneSessionTest
        , testCase "a dropped stream reconnects with Last-Event-ID and loses no event" reconnectTest
        , testCase "bearer tokens: refused without, own sessions with, owner-scoped feed" bearerTokenTest
        , testCase "over a Unix socket: Stats, a session, and its events" unixSocketTest
        ]

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

{- | A runner over @nodes@, served by 'application' on a random port, and an
'httpClient' pointing at it. @kill@ ends every open event stream
(server-side, as a dropped connection would), for 'reconnectTest'.
-}
data Fixture = Fixture
    { fxRunner :: SessionRunner
    , fxClient :: RunnerClient
    , fxPort :: Int
    , fxKillStreams :: IO ()
    , fxStreamOpens :: IO Int
    }

withFixture :: [OSAgentNode] -> Completion -> (Fixture -> IO a) -> IO a
withFixture = withFixtureAuth Nothing Nothing

withFixtureAuth :: Maybe AuthTokens -> Maybe Text -> [OSAgentNode] -> Completion -> (Fixture -> IO a) -> IO a
withFixtureAuth auth token nodes complete k = do
    host <- testHost nodes (const complete)
    withSessionRunner host $ \runner -> do
        env0 <- newServerEnv host runner auth
        let env = env0{envKeepAlive = 300_000}
        generation <- newTVarIO (0 :: Int)
        opens <- newIORef (0 :: Int)
        let app = killableStreams generation opens (application env)
        Warp.testWithApplication (pure app) $ \port -> do
            client <- httpClient (fastConfig (TcpEndpoint ("http://127.0.0.1:" <> show port))){hccToken = token}
            k
                Fixture
                    { fxRunner = runner
                    , fxClient = client
                    , fxPort = port
                    , fxKillStreams = atomically (modifyTVar' generation (+ 1))
                    , fxStreamOpens = readIORef opens
                    }

-- | Reconnect quickly in tests.
fastConfig :: Endpoint -> HttpClientConfig
fastConfig endpoint = (defaultHttpClientConfig endpoint){hccReconnectDelay = 0.02, hccReconnectMaxDelay = 0.2}

{- | Wrap every @text/event-stream@ response so that bumping @generation@
ends it (the handler returns, the server closes the chunked body), and
count how many such streams were opened.
-}
killableStreams :: TVar Int -> IORef Int -> Wai.Application -> Wai.Application
killableStreams generation opens app req respond =
    app req $ \rsp -> case Wai.responseToStream rsp of
        (status, headers, withBody)
            | lookup "Content-Type" headers == Just "text/event-stream" -> do
                atomicModifyIORef' opens (\n -> (n + 1, ()))
                g0 <- readTVarIO generation
                respond $ Wai.responseStream status headers $ \write flush ->
                    withBody $ \body ->
                        race_ (body write flush) $
                            atomically (readTVar generation >>= \g -> check (g /= g0))
            | otherwise -> respond rsp

-------------------------------------------------------------------------------
-- Pure parts
-------------------------------------------------------------------------------

parseEndpointTest :: Assertion
parseEndpointTest = do
    parseEndpoint "http://127.0.0.1:8080" @?= Right (TcpEndpoint "http://127.0.0.1:8080")
    parseEndpoint "http://127.0.0.1:8080/" @?= Right (TcpEndpoint "http://127.0.0.1:8080")
    parseEndpoint "https://agents.example/api" @?= Right (TcpEndpoint "https://agents.example/api")
    parseEndpoint "unix:///run/agents.sock" @?= Right (UnixEndpoint "/run/agents.sock")
    parseEndpoint "unix:agents.sock" @?= Right (UnixEndpoint "agents.sock")
    parseEndpoint "/tmp/agents.sock" @?= Right (UnixEndpoint "/tmp/agents.sock")
    parseEndpoint "./agents.sock" @?= Right (UnixEndpoint "./agents.sock")
    parseEndpoint "agents.sock" @?= Right (UnixEndpoint "agents.sock")
    assertBool "ftp is refused" (isLeft (parseEndpoint "ftp://host"))
    assertBool "a bare word is refused" (isLeft (parseEndpoint "localhost"))
    assertBool "unix:// with no path is refused" (isLeft (parseEndpoint "unix://"))
  where
    isLeft = either (const True) (const False)

sseParserTest :: Assertion
sseParserTest = do
    let chunks = ["id: 1\nevent: run.sta", "rted\ndata: {\"a\":1}\n", "\n: keepalive\n\n", "event: snapshot\r\ndata: x\r\ndata: y\r\n\r\nid: 3\n"]
        step (p, acc) c = let (p', fs) = feedSse p c in (p', acc <> fs)
        (_, frames) = foldl step (emptySseParser, []) chunks
    frames
        @?= [ SseFrame (Just "1") (Just "run.started") "{\"a\":1}"
            , SseFrame Nothing (Just "snapshot") "x\ny"
            ]

-------------------------------------------------------------------------------
-- Every command
-------------------------------------------------------------------------------

createAndListTest :: Assertion
createAndListTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        meta1 <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        (meta1', active) <- expectRight =<< awaitRun client meta1.smSessionId 5
        (meta1'.smStatus, active) @?= (StatusIdle, False)

        meta2 <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        meta2.smStatus @?= StatusReady
        (empty, _) <- expectRight =<< getSession client meta2.smSessionId
        length empty.turns @?= 0

        meta3 <- expectRight =<< postMessage client meta1'.smSessionId (message "now go") (Just UntilBlocked) Map.empty
        (meta3', _) <- expectRight =<< awaitRun client meta3.smSessionId 5
        meta3'.smStatus @?= StatusIdle

        -- A message with no run: stored, not run.
        meta4 <- expectRight =<< postMessage client meta2.smSessionId (message "later") Nothing Map.empty
        meta4.smStatus @?= StatusReady

        (sess, gotMeta) <- expectRight =<< getSession client meta1.smSessionId
        gotMeta.smSessionId @?= meta1.smSessionId
        assertBool "both messages are in the session" (all (`elem` sessionTexts sess) ["hi", "now go"])

        listed <- expectRight =<< listSessions client allSessionsQuery
        length listed @?= 2
        limited <- expectRight =<< listSessions client allSessionsQuery{sqLimit = Just 1}
        map (.smSessionId) limited @?= take 1 (map (.smSessionId) listed)
        byStatus <- expectRight =<< listSessions client allSessionsQuery{sqStatuses = Just [StatusReady]}
        map (.smSessionId) byStatus @?= [meta2.smSessionId]
        byAgent <- expectRight =<< listSessions client allSessionsQuery{sqAgent = Just "nobody"}
        byAgent @?= []

        agents <- expectRight =<< listAgents client
        map (.adSlug) agents @?= ["test-agent"]
        agent <- expectRight =<< getAgent client "test-agent"
        agent.adSlug @?= "test-agent"

childSessionsTest :: Assertion
childSessionsTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        parentMeta <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        child <- expectRight =<< createSessionAsChild client parentMeta.smSessionId "test-agent" Nothing Nothing Map.empty
        child.smParent @?= Just parentMeta.smSessionId
        spawned <- expectRight =<< spawnSession client parentMeta.smSessionId "test-agent" (message "helper, go")
        spawned.smParent @?= Just parentMeta.smSessionId
        (done, _) <- expectRight =<< awaitRun client spawned.smSessionId 5
        done.smStatus @?= StatusIdle
        children <- expectRight =<< listSessions client allSessionsQuery{sqParent = Just parentMeta.smSessionId}
        length children @?= 2

resumeTest :: Assertion
resumeTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) Nothing Map.empty
        meta.smStatus @?= StatusReady
        _ <- expectRight =<< resumeSession client meta.smSessionId UntilBlocked Map.empty
        (resumed, _) <- expectRight =<< awaitRun client meta.smSessionId 5
        resumed.smStatus @?= StatusIdle

completeCallTest :: Assertion
completeCallTest = do
    node <- testNode deferAll
    withFixture [node] (firstThen [remoteCall "call_1"]) $ \fx -> do
        let client = fx.fxClient
        meta <- expectRight =<< createSession client "test-agent" (Just (message "fetch it")) (Just UntilBlocked) Map.empty
        (blocked, active) <- expectRight =<< awaitRun client meta.smSessionId 5
        (blocked.smStatus, active) @?= (StatusWaitingExternal, False)
        token <- singleToken fx.fxRunner meta.smSessionId
        _ <- expectRight =<< completeCall client token (TextResponse "42") True Map.empty
        (final, _) <- expectRight =<< awaitRun client meta.smSessionId 5
        final.smStatus @?= StatusIdle
        sess <- currentSession fx.fxRunner meta.smSessionId
        assertBool "tool result delivered" ("42" `elem` responseTexts sess)
        again <- completeCall client token (TextResponse "43") True Map.empty
        again @?= Left (TokenAlreadyCompleted token)

pauseCancelTest :: Assertion
pauseCancelTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    complete <- onceThen [slowCall]
    withFixture [node] complete $ \fx -> do
        let client = fx.fxClient
        meta <- expectRight =<< createSession client "test-agent" (Just (message "go")) (Just UntilBlocked) Map.empty
        let sid = meta.smSessionId
        waitUntil $ hasBackgroundCall <$> currentSession fx.fxRunner sid
        -- Still running: an await with a short limit reports it active.
        (_, active) <- expectRight =<< awaitRun client sid 0.1
        active @?= True
        paused <- expectRight =<< pauseSession client sid
        paused.smSessionId @?= sid
        afterAttached <- expectRight =<< cancelAttachedCalls client sid
        afterAttached.smSessionId @?= sid
        cancelled <- expectRight =<< cancelRun client sid
        cancelled.smStatus @?= StatusReady
        again <- cancelRun client sid
        again @?= Left (NoActiveRun sid)
        putMVar gate ()

mailForkDeleteTest :: Assertion
mailForkDeleteTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        _ <- expectRight =<< awaitRun client meta.smSessionId 5
        receipt <- expectRight =<< sendMail client meta.smSessionId Normal (AgentMessage "note" Nothing False)
        mail <- expectRight =<< listMail client meta.smSessionId False
        assertBool ("the mail was recorded: " <> show receipt) (not (null mail))
        _ <- expectRight =<< listMail client meta.smSessionId True
        forked <- expectRight =<< forkSession client meta.smSessionId Nothing Nothing
        (forkedSess, _) <- expectRight =<< getSession client forked.smSessionId
        forkedSess.forkedFromSessionId @?= Just meta.smSessionId
        atTurn <- expectRight =<< forkSession client meta.smSessionId (Just 0) (Just "test-agent")
        atTurn.smAgent @?= Just "test-agent"
        dry <- expectRight =<< deleteSession client forked.smSessionId DryRun
        (dry.dpSessions, dry.dpDryRun) @?= ([forked.smSessionId], True)
        plan <- expectRight =<< deleteSession client forked.smSessionId DeleteForReal
        plan.dpSessions @?= [forked.smSessionId]
        gone <- getSession client forked.smSessionId
        gone @?= Left (UnknownSession forked.smSessionId)

awaitStatsTest :: Assertion
awaitStatsTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        (final, active) <- expectRight =<< awaitRun client meta.smSessionId 5
        (final.smStatus, active) @?= (StatusIdle, False)
        s <- expectRight =<< stats client
        s.rsLiveSessions >= 1 @?= True

errorsTest :: Assertion
errorsTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        meta <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        let sid = meta.smSessionId
        missingAgent <- getAgent client "no-such-agent"
        missingAgent @?= Left (UnknownAgent "no-such-agent")
        badCreate <- createSession client "no-such-agent" Nothing Nothing Map.empty
        badCreate @?= Left (UnknownAgent "no-such-agent")
        other <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        _ <- expectRight =<< deleteSession client other.smSessionId DeleteForReal
        missingSession <- getSession client other.smSessionId
        missingSession @?= Left (UnknownSession other.smSessionId)
        badTurn <- forkSession client sid (Just 7) Nothing
        badTurn @?= Left (UnknownTurn sid 7)
        badParams <- postMessage client sid (message "x") Nothing (Map.fromList [("nope", "1")])
        fmap (const ()) badParams @?= Left (UnknownParams [])

unreachableTest :: Assertion
unreachableTest = do
    client <- httpClient (fastConfig (TcpEndpoint "http://127.0.0.1:1"))
    stats client >>= \case
        Left (TransportError _) -> pure ()
        other -> assertFailure ("expected a TransportError, got " <> show other)
    result <- tryHttpClientError (client.rcSubscribe AllSessions Nothing)
    assertBool "subscribing to nothing throws HttpClientError" result
  where
    tryHttpClientError action = do
        r <- try action
        pure $ case r of
            Left (HttpClientError _) -> True
            Right _ -> False

-------------------------------------------------------------------------------
-- The TUI's sequences (HostClientTests), over HTTP
-------------------------------------------------------------------------------

tuiSequenceTest :: Assertion
tuiSequenceTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        sub <- expectRight =<< subscribeAll client
        meta0 <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        let sid = meta0.smSessionId
        awaitKind sub "session.created"
        _ <- expectRight =<< postMessage client sid (message "hi") (Just UntilBlocked) Map.empty
        awaitKind sub "run.started"
        awaitKind sub "session.updated"
        awaitKind sub "run.stopped"
        _ <- expectRight =<< postMessage client sid (NewMessage "more" [] True) (Just UntilBlocked) Map.empty
        (final2, _) <- expectRight =<< awaitRun client sid 5
        final2.smStatus @?= StatusIdle
        _ <- expectRight =<< pauseSession client sid
        _ <- expectRight =<< resumeSession client sid UntilBlocked Map.empty
        (final3, _) <- expectRight =<< awaitRun client sid 5
        assertBool ("resumed to " <> show final3.smStatus) (final3.smStatus `elem` [StatusIdle, StatusPaused])
        forked <- expectRight =<< forkSession client sid (Just 0) Nothing
        forked.smParent @?= meta0.smParent
        _ <- expectRight =<< sendMail client sid Interrupt (Control StopRun)
        subClose sub

tuiDraftShipSequenceTest :: Assertion
tuiDraftShipSequenceTest = do
    gate <- newEmptyMVar
    node <- testNode "{}"
    withFixture [node] (\c -> readMVar gate >> mockCompletion c) $ \fx -> do
        let client = fx.fxClient
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hello")) (Just UntilBlocked) Map.empty
        let sid = meta.smSessionId
        waitUntil $ (== StatusRunning) . (.smStatus) . snd <$> (expectRight =<< getSession client sid)
        putMVar gate ()
        (idle, _) <- expectRight =<< awaitRun client sid 5
        idle.smStatus @?= StatusIdle
        _ <- expectRight =<< postMessage client sid (message "more detail\n\nand one more thing") (Just UntilBlocked) Map.empty
        (final, _) <- expectRight =<< awaitRun client sid 5
        final.smStatus @?= StatusIdle
        (sess, _) <- expectRight =<< getSession client sid
        let texts = sessionTexts sess
        assertBool ("draft text reached the session: " <> show texts) (any ("more detail" `Text.isInfixOf`) texts && any ("and one more thing" `Text.isInfixOf`) texts)

tuiPendingCallSequenceTest :: Assertion
tuiPendingCallSequenceTest = do
    node <- testNode deferAll
    withFixture [node] (firstThen [remoteCall "call_1"]) $ \fx -> do
        let client = fx.fxClient
        sub <- expectRight =<< subscribeAll client
        meta <- expectRight =<< createSession client "test-agent" (Just (message "fetch it")) (Just UntilBlocked) Map.empty
        let sid = meta.smSessionId
        awaitKind sub "run.started"
        deferred <- awaitDeferred sub
        length deferred @?= 1
        (blocked, _) <- expectRight =<< awaitRun client sid 5
        blocked.smStatus @?= StatusWaitingExternal
        token <- singleToken fx.fxRunner sid
        map dcvToken deferred @?= [Just token]
        _ <- expectRight =<< completeCall client token (TextResponse "42") True Map.empty
        awaitKind sub "run.started"
        awaitKind sub "run.stopped"
        (final, _) <- expectRight =<< awaitRun client sid 5
        final.smStatus @?= StatusIdle
        sess <- currentSession fx.fxRunner sid
        assertBool "tool result delivered" ("42" `elem` responseTexts sess)
        subClose sub
  where
    awaitDeferred sub = do
        ev <- nextEvent sub
        case ev.evBody of
            CallsDeferred calls -> pure calls
            _ -> awaitDeferred sub

-------------------------------------------------------------------------------
-- Streams
-------------------------------------------------------------------------------

replayTest :: Assertion
replayTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        sub <- expectRight =<< subscribeAll client
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        firstEv <- nextEvent sub
        rest <- untilStopped sub
        subClose sub
        let everything = firstEv : rest
        -- From the first event's seq: exactly everything after it, in order.
        replaying <- expectRight =<< client.rcSubscribe AllSessions (Just firstEv.evSeq)
        replayed <- mapM (const (nextEvent replaying)) rest
        map (.evSeq) replayed @?= map (.evSeq) rest
        subClose replaying
        -- A seq this server never stamped cannot be replayed.
        let EventSeq lastN = (last everything).evSeq
        beyond <- client.rcSubscribe (OneSession meta.smSessionId) (Just (EventSeq (lastN + 1000)))
        case beyond of
            Left ReplayUnavailable -> pure ()
            Right s -> subClose s >> assertFailure "expected ReplayUnavailable"
        -- The same answer as the in-process client gives.
        inProcess <- Runner.subscribe fx.fxRunner (OneSession meta.smSessionId) (Just (EventSeq (lastN + 1000)))
        either (const (pure ())) (const (assertFailure "in-process replay should be unavailable too")) inProcess

oneSessionTest :: Assertion
oneSessionTest = do
    node <- testNode "{}"
    withFixture [node] mockCompletion $ \fx -> do
        let client = fx.fxClient
        meta <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        other <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        sub <- expectRight =<< client.rcSubscribe (OneSession meta.smSessionId) Nothing
        _ <- expectRight =<< postMessage client other.smSessionId (message "elsewhere") (Just UntilBlocked) Map.empty
        _ <- expectRight =<< awaitRun client other.smSessionId 5
        _ <- expectRight =<< postMessage client meta.smSessionId (message "hi") (Just UntilBlocked) Map.empty
        evs <- untilStopped sub
        assertBool "only this session's events" (all ((== Just meta.smSessionId) . (.evSession)) evs)
        subClose sub

{- | Kill every open event stream server-side while a run is producing
events (gated on the mock LLM), then let the run finish: the client
reconnects with @Last-Event-ID@ and, across the whole run, delivers a
gap-free, duplicate-free run of sequence numbers (every event of a
server-wide feed has the next seq).
-}
reconnectTest :: Assertion
reconnectTest = do
    gate <- newEmptyMVar
    node <- testNode "{}"
    withFixture [node] (\c -> readMVar gate >> mockCompletion c) $ \fx -> do
        let client = fx.fxClient
        sub <- expectRight =<< subscribeAll client
        meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
        started <- untilKind sub "run.started"
        opensBefore <- fx.fxStreamOpens
        fx.fxKillStreams
        -- Events emitted while the stream is down: the run finishes, and a
        -- second session is created.
        putMVar gate ()
        _ <- expectRight =<< awaitRun client meta.smSessionId 5
        _ <- expectRight =<< createSession client "test-agent" Nothing Nothing Map.empty
        rest <- untilKind sub "session.created"
        opensAfter <- fx.fxStreamOpens
        assertBool ("the stream was reopened: " <> show (opensBefore, opensAfter)) (opensAfter > opensBefore)
        let seqs = [n | ev <- started <> rest, let EventSeq n = ev.evSeq]
        case seqs of
            (s0 : _) -> seqs @?= take (length seqs) [s0 ..]
            [] -> assertFailure "no events at all"
        assertBool "the run's stop was delivered" (any (\ev -> eventKind ev.evBody == "run.stopped") rest)
        subClose sub
  where
    untilKind sub wanted = go []
      where
        go acc = do
            ev <- nextEvent sub
            let acc' = acc <> [ev]
            if eventKind ev.evBody == wanted then pure acc' else go acc'

bearerTokenTest :: Assertion
bearerTokenTest = do
    node <- testNode "{}"
    let tokens = authTokensFromList [("alice-token", "alice"), ("bob-token", "bob")]
    withFixtureAuth (Just tokens) (Just "alice-token") [node] mockCompletion $ \fx -> do
        let alice = fx.fxClient
            endpoint = TcpEndpoint ("http://127.0.0.1:" <> show fx.fxPort)
        anonymous <- httpClient (fastConfig endpoint)
        bob <- httpClient (fastConfig endpoint){hccToken = Just "bob-token"}
        -- /healthz is open; the rest needs a token.
        _ <- expectRight =<< stats anonymous
        refused <- listAgents anonymous
        case refused of
            Left (TransportError msg) -> assertBool (show msg) ("unauthorized" `Text.isInfixOf` msg)
            other -> assertFailure ("expected unauthorized, got " <> show other)
        -- scope=all needs an admin: alice's subscribeAll falls back to her own feed.
        sub <- expectRight =<< subscribeAll alice
        bobs <- expectRight =<< createSession bob "test-agent" Nothing Nothing Map.empty
        mine <- expectRight =<< createSession alice "test-agent" Nothing Nothing Map.empty
        mine.smOwner @?= Just "alice"
        created <- untilCreated sub
        created.smSessionId @?= mine.smSessionId
        -- Bob's session is invisible to alice, and named as unknown.
        hidden <- getSession alice bobs.smSessionId
        hidden @?= Left (UnknownSession bobs.smSessionId)
        listed <- expectRight =<< listSessions alice allSessionsQuery
        map (.smSessionId) listed @?= [mine.smSessionId]
        subClose sub
  where
    untilCreated sub =
        nextEvent sub >>= \ev -> case ev.evBody of
            SessionCreated meta -> pure meta
            _ -> untilCreated sub

unixSocketTest :: Assertion
unixSocketTest = withSystemTempDirectory "http-client-socket" $ \dir -> do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    let sockPath = dir </> "agents.sock"
    withSessionRunner host $ \runner -> do
        env0 <- newServerEnv host runner Nothing
        let env = env0{envKeepAlive = 300_000}
        bracket (listenUnix sockPath) NS.close $ \sock ->
            withAsync (Warp.runSettingsSocket Warp.defaultSettings sock (application env)) $ \_ -> do
                forM_' ["unix://" <> sockPath, sockPath] $ \url -> do
                    endpoint <- either assertFailure pure (parseEndpoint url)
                    client <- httpClient (fastConfig endpoint)
                    s <- expectRight =<< stats client
                    s.rsActiveRuns @?= 0
                    sub <- expectRight =<< subscribeAll client
                    meta <- expectRight =<< createSession client "test-agent" (Just (message "hi")) (Just UntilBlocked) Map.empty
                    evs <- untilStopped sub
                    assertBool "events of the new session" (any ((== Just meta.smSessionId) . (.evSession)) evs)
                    subClose sub
  where
    listenUnix path = do
        sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
        NS.bind sock (NS.SockAddrUnix path)
        NS.listen sock 16
        pure sock
    forM_' xs f = mapM_ f xs

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | The next event, or a failure after 5 seconds.
nextEvent :: Subscription -> IO Event
nextEvent sub =
    timeout 5_000_000 (subNext sub) >>= \case
        Just ev -> pure ev
        Nothing -> assertFailure "no event within 5 seconds"

awaitKind :: Subscription -> Text -> IO ()
awaitKind sub wanted = do
    ev <- nextEvent sub
    if eventKind ev.evBody == wanted then pure () else awaitKind sub wanted

-- | Events up to and including the next @run.stopped@.
untilStopped :: Subscription -> IO [Event]
untilStopped sub = go []
  where
    go acc = do
        ev <- nextEvent sub
        let acc' = acc <> [ev]
        if eventKind ev.evBody == "run.stopped" then pure acc' else go acc'
