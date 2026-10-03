{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Two session runners on one database, coordinated by run leases.

The leases here are a table in memory with a clock the tests move by hand,
so no time is spent waiting; the runners share one SQLite database. The
leases and signals of Postgres are tested in @agents-postgres-tests@.
-}
module RunnerCoordinationTests (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, stateTVar)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.SQLite.Simple (open)
import Prod.Tracer (silent)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, testNode)
import RunnerTests (expectRight, message, waitUntil)
import System.Agents.AgentFactory (AgentDeps (..), Completion, SessionSink (..), defaultAgentDeps)
import qualified System.Agents.Base as Base
import System.Agents.AgentTree (OSAgentNode (..))
import System.Agents.Host
import System.Agents.Host.Coordination
import System.Agents.Host.Runner
import System.Agents.Session.Async (mkSqliteContinuationStore)
import System.Agents.Session.Base hiding (SessionProgress (..))
import System.Agents.Session.Mailbox (MailStore (..), WatchRequest (..))
import System.Agents.Session.MailStore (mkSqliteMailStore)
import System.Agents.Session.WatchStore (mkSqliteWatchStore)
import System.Agents.SessionStore hiding (listSessions)

tests :: TestTree
tests =
    testGroup
        "Session runners sharing a database"
        [ testCase "a run held by one runner is busy for the other, which queues mail for it" heldElsewhereTest
        , testCase "an expired lease is taken over; the stalled owner's late write is dropped" takeOverTest
        , testCase "a runner that lost its lease stops its run at the next heartbeat" abandonTest
        , testCase "recovery at startup leaves the sessions another runner holds" recoveryLeavesHeldTest
        , testCase "another runner's writes are reported here, and a watch forwards them once" forwardedEventsTest
        ]

-- | The run is another runner's: no second run, no delete, and a message becomes mail.
heldElsewhereTest :: Assertion
heldElsewhereTest = do
    cluster <- newCluster
    gate <- newGate
    hostA <- clusterHost cluster "a" (gated gate)
    hostB <- clusterHost cluster "b" mockCompletion
    withRunner hostA $ \runnerA -> withRunner hostB $ \runnerB -> do
        sid <- startBlockedRun runnerA gate
        resume runnerB sid UntilBlocked Map.empty >>= (@?= Left (RunInProgress sid)) . void
        deleteSession runnerB sid DeleteForReal >>= (@?= Left (RunInProgress sid)) . void
        (_, active) <- expectRight =<< awaitRun runnerB sid 0.1
        assertBool "the other runner sees the run as active" active
        queued <- expectRight =<< postMessage runnerB sid (message "one more thing") (Just UntilBlocked) Map.empty
        queued.smStatus @?= StatusRunning
        (rsActiveRuns <$> runnerStats runnerB) >>= (@?= 0)
        -- The owner picks the mail up when it renews.
        heartbeat runnerA
        mail <- expectRight =<< listMail runnerA sid False
        [q | UserMessage (UserQuery q _) <- map (.envBody) mail] @?= ["one more thing"]
        openGate gate
        waitUntil (stoppedOn runnerA sid)
        -- Released: the other runner may run it now.
        _ <- expectRight =<< resumeOrPost runnerB sid
        (final, _) <- expectRight =<< awaitRun runnerB sid 5
        final.smStatus @?= StatusIdle

-- | The owner stalls past its lease; the other runner recovers the session and runs it.
takeOverTest :: Assertion
takeOverTest = do
    cluster <- newCluster
    gate <- newGate
    hostA <- clusterHost cluster "a" (gated gate)
    hostB <- clusterHost cluster "b" mockCompletion
    withRunner hostA $ \runnerA -> withRunner hostB $ \runnerB -> do
        sid <- startBlockedRun runnerA gate
        takeOverExpired runnerB >>= (@?= [])
        advance cluster (leaseTtl + 1)
        takeOverExpired runnerB >>= (@?= [sid])
        Just (_, taken) <- getSession runnerB sid
        taken.smStatus @?= StatusReady
        _ <- expectRight =<< resume runnerB sid UntilBlocked Map.empty
        (done, _) <- expectRight =<< awaitRun runnerB sid 5
        done.smStatus @?= StatusIdle
        -- The stalled owner wakes up, and finds the session is not its own.
        openGate gate
        waitUntil (stoppedOn runnerA sid)
        Just (_, late) <- getSession runnerA sid
        late.smStatus @?= StatusIdle
        late.smVersion @?= done.smVersion

-- | The owner's heartbeat finds its lease gone, and stops the run without storing.
abandonTest :: Assertion
abandonTest = do
    cluster <- newCluster
    gate <- newGate
    hostA <- clusterHost cluster "a" (gated gate)
    hostB <- clusterHost cluster "b" mockCompletion
    withRunner hostA $ \runnerA -> withRunner hostB $ \runnerB -> do
        sid <- startBlockedRun runnerA gate
        -- While the lease is renewed in time, nothing changes hands.
        advance cluster (leaseTtl - 1)
        heartbeat runnerA
        advance cluster (leaseTtl - 1)
        takeOverExpired runnerB >>= (@?= [])
        advance cluster 2
        takeOverExpired runnerB >>= (@?= [sid])
        Just (_, taken) <- getSession runnerB sid
        heartbeat runnerA
        (rsActiveRuns <$> runnerStats runnerA) >>= (@?= 0)
        Just (_, late) <- getSession runnerA sid
        late.smStatus @?= StatusReady
        late.smVersion @?= taken.smVersion

-- | A runner starting next to a live one recovers nothing that the live one holds.
recoveryLeavesHeldTest :: Assertion
recoveryLeavesHeldTest = do
    cluster <- newCluster
    gate <- newGate
    hostA <- clusterHost cluster "a" (gated gate)
    hostB <- clusterHost cluster "b" mockCompletion
    withRunner hostA $ \runnerA -> do
        sid <- startBlockedRun runnerA gate
        withRunner hostB $ \runnerB -> do
            recoverOnStartup runnerB >>= (@?= [])
            Just (_, meta) <- getSession runnerB sid
            meta.smStatus @?= StatusRunning
        openGate gate
        (done, _) <- expectRight =<< awaitRun runnerA sid 5
        done.smStatus @?= StatusIdle

{- | The runner that does not run the session reports what the other stores,
and a watch registered on both forwards the run's end once: from its owner.
-}
forwardedEventsTest :: Assertion
forwardedEventsTest = do
    cluster <- newCluster
    gate <- newGate
    hostA <- clusterHost cluster "a" (gated gate)
    hostB <- clusterHost cluster "b" mockCompletion
    withRunner hostA $ \runnerA -> withRunner hostB $ \runnerB -> do
        watcher <- expectRight =<< createSessionAs runnerA Nothing "test-agent" Nothing Nothing Map.empty
        target <- expectRight =<< createSessionAsWithParent runnerA (Just watcher.smSessionId) Nothing "test-agent" (Just (message "hello")) (Just UntilBlocked) Map.empty
        let sid = target.smSessionId
            watch = WatchRequest sid (Just ["run.stopped"]) Nothing Nothing
        readMVar gate.gateEntered
        _ <- expectRight =<< serverWatchSession runnerA watcher.smSessionId watch
        _ <- expectRight =<< serverWatchSession runnerB watcher.smSessionId watch
        next <- subscribeSession runnerB sid
        announce cluster "a" sid
        first <- expectEvent next
        eventKind first.evBody @?= "session.updated"
        openGate gate
        waitUntil (stoppedOn runnerA sid)
        announce cluster "a" sid
        kinds <- map (eventKind . (.evBody)) <$> sequence [expectEvent next, expectEvent next]
        kinds @?= ["session.updated", "run.stopped"]
        -- Time for the watch on B to forward, if it wrongly would.
        threadDelay 200000
        mail <- cluster.clStores.hsMail.msLoad watcher.smSessionId
        [kind | WatchedEvent _ kind _ <- map (.envBody) mail] @?= ["run.stopped"]
  where
    expectEvent next = timeout 5000000 next >>= maybe (assertFailure "no event in time") pure

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

-- | Longer than any test: the runners' own heartbeat never fires.
leaseTtl :: NominalDiffTime
leaseTtl = 3600

withRunner :: Host -> (SessionRunner -> IO a) -> IO a
withRunner host action = do
    runner <- newSessionRunnerWith defaultRunnerConfig{rcLeaseTtl = leaseTtl} host
    result <- action runner
    shutdownSessionRunner runner
    pure result

-- | A session whose run is inside its first LLM call, on the given runner.
startBlockedRun :: SessionRunner -> Gate -> IO SessionId
startBlockedRun runner gate = do
    meta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
    readMVar gate.gateEntered
    pure meta.smSessionId

stoppedOn :: SessionRunner -> SessionId -> IO Bool
stoppedOn runner sid = either (const False) (not . snd) <$> awaitRun runner sid 0

-- | Run the session again: resume it if a turn is pending, else post a message.
resumeOrPost :: SessionRunner -> SessionId -> IO (Either RunnerError SessionMeta)
resumeOrPost runner sid =
    getSession runner sid >>= \case
        Just (_, meta) | meta.smStatus == StatusIdle -> postMessage runner sid (message "again") (Just UntilBlocked) Map.empty
        _ -> resume runner sid UntilBlocked Map.empty

data Gate = Gate
    { gateEntered :: MVar ()
    , gateOpen :: MVar ()
    }

newGate :: IO Gate
newGate = Gate <$> newEmptyMVar <*> newEmptyMVar

openGate :: Gate -> IO ()
openGate gate = putMVar gate.gateOpen ()

-- | An LLM that answers once the gate is open.
gated :: Gate -> Completion
gated gate completion = do
    void $ tryPutMVar gate.gateEntered ()
    readMVar gate.gateOpen
    mockCompletion completion

-- | What the runners of a test share: the stores, the leases, and the clock.
data Cluster = Cluster
    { clStores :: HostStores
    , clLeases :: TVar (Map.Map SessionId (Text, NominalDiffTime))
    , clClock :: TVar NominalDiffTime
    , clListeners :: TVar [(Text, SessionId -> SessionSignal -> IO ())]
    , clNode :: OSAgentNode
    }

newCluster :: IO Cluster
newCluster = do
    conn <- open ":memory:"
    backend <- mkSqliteSessionStore conn
    continuations <- mkSqliteContinuationStore conn
    mail <- mkSqliteMailStore conn
    watches <- mkSqliteWatchStore conn
    Cluster (HostStores backend continuations mail watches Nothing noCoordination)
        <$> newTVarIO Map.empty
        <*> newTVarIO 0
        <*> newTVarIO []
        <*> testNode "{}"

advance :: Cluster -> NominalDiffTime -> IO ()
advance cluster by = atomically $ modifyTVar' cluster.clClock (+ by)

-- | Signal the other runners that the named one stored the session.
announce :: Cluster -> Text -> SessionId -> IO ()
announce cluster from sid = do
    listeners <- readTVarIO cluster.clListeners
    sequence_ [handler sid SessionStored | (owner, handler) <- listeners, owner /= from]

-- | Leases in a shared table, expiring on the cluster's clock. Signals are sent by 'announce'.
clusterCoordination :: Cluster -> Text -> Coordination
clusterCoordination cluster me =
    noCoordination
        { coEnabled = True
        , coInstance = me
        , coAcquire = \sid ttl -> atomically $ do
            now <- readTVar cluster.clClock
            stateTVar cluster.clLeases $ \leases -> case Map.lookup sid leases of
                Just (owner, until') | owner /= me && until' >= now -> (LeaseHeldBy owner, leases)
                _ -> (LeaseAcquired, Map.insert sid (me, now + ttl) leases)
        , coRenew = \sids ttl -> atomically $ do
            now <- readTVar cluster.clClock
            stateTVar cluster.clLeases $ \leases ->
                let held = [sid | sid <- sids, fmap fst (Map.lookup sid leases) == Just me]
                 in (held, foldr (\sid -> Map.insert sid (me, now + ttl)) leases held)
        , coRelease = \sid -> atomically $ modifyTVar' cluster.clLeases $ \leases ->
            if fmap fst (Map.lookup sid leases) == Just me then Map.delete sid leases else leases
        , coHolder = \sid -> atomically $ do
            now <- readTVar cluster.clClock
            leases <- readTVar cluster.clLeases
            pure $ case Map.lookup sid leases of
                Just (owner, until') | owner /= me && until' >= now -> Just owner
                _ -> Nothing
        , coExpired = running $ \now -> \case
            Just (owner, until') -> owner /= me && until' < now
            Nothing -> False
        , coAbandoned = running $ \now -> \case
            Just (_, until') -> until' < now
            Nothing -> True
        , coListen = \handler -> do
            atomically $ modifyTVar' cluster.clListeners ((me, handler) :)
            pure (pure ())
        }
  where
    running keep = do
        metas <- cluster.clStores.hsSessions.sbQuery allSessionsQuery{sqStatuses = Just [StatusRunning]}
        now <- readTVarIO cluster.clClock
        leases <- readTVarIO cluster.clLeases
        pure [m.smSessionId | m <- metas, keep now (Map.lookup m.smSessionId leases)]

-- | A host on the cluster's stores, named as a lease owner, with its own LLM.
clusterHost :: Cluster -> Text -> Completion -> IO Host
clusterHost cluster me complete = do
    let stores = cluster.clStores
        deps = (defaultAgentDeps []){adContinuationStore = Just stores.hsContinuations, adCompletion = Just (const complete)}
    stored <- noStoredAgents
    pure
        Host
            { hostAgents = Map.fromList [(Base.slug cluster.clNode.osNodeConfig, cluster.clNode)]
            , hostStoredAgents = stored
            , hostDeps = deps
            , hostSubAgentDeps = deps{adSessionSink = SinkBackend stores.hsSessions}
            , hostBackend = stores.hsSessions
            , hostContinuations = stores.hsContinuations
            , hostMail = stores.hsMail
            , hostWatches = stores.hsWatches
            , hostCoordination = clusterCoordination cluster me
            , hostTracer = silent
            , hostStreamTokens = False
            , hostProcessParams = mempty
            , hostLiveSessionTtl = 15 * 60
            }
