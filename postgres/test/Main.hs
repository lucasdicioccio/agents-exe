{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tests of the Postgres stores against a throwaway cluster.

The cluster is created with @initdb@ and started with @pg_ctl@ (found
through @pg_config --bindir@) in a temporary directory, listening on a free
local port. Set @AGENTS_TEST_POSTGRES_URL@ to use an existing server
instead; each test creates its own database there. Without either, the
tests are skipped.
-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Exception (IOException, bracket, bracket_, throwIO, try)
import Control.Monad (void, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (close, connectPostgreSQL, execute_, query_)
import Network.Socket (Family (AF_INET), SockAddr (..), SocketType (Stream), bind, defaultProtocol, getSocketName, socket, tupleToHostAddress)
import qualified Network.Socket as Socket
import Prod.Tracer (silent)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import System.Process (callProcess, readProcess)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson.Types as Aeson
import System.Agents.AgentFactory (Completion)
import System.Agents.AgentStore (AgentStore (..), StoredAgent (..))
import qualified System.Agents.Base as Base
import System.Agents.Host
import System.Agents.Host.Coordination
import System.Agents.Host.Runner
import System.Agents.Postgres
import System.Agents.Session.Async (ContinuationStore (..))
import System.Agents.Session.Base (
    DeferredCallView (..),
    Envelope (..),
    LlmCompletion (..),
    LlmResponse (..),
    LlmToolCall (..),
    MailBody (..),
    Mailbox (..),
    Outgoing (..),
    Priority (..),
    Receipt (..),
    Sender (..),
    SendError (..),
    SessionStatus (..),
    SystemPrompt (..),
    UserQuery (..),
    UserToolResponse (..),
    awaitMail,
    newDurableMailbox,
    newSessionFromPrompt,
    newSessionId,
    pendingDeferredCalls,
 )
import Control.Concurrent.STM (atomically)
import System.Agents.SessionStore hiding (listSessions)

main :: IO ()
main = do
    server <- findServer
    case server of
        Nothing ->
            defaultMain $
                testCase "Postgres tests skipped: no pg_config/initdb, and AGENTS_TEST_POSTGRES_URL unset" (pure ())
        Just withServer ->
            withServer $ \baseUrl ->
                defaultMain $
                    testGroup
                        "agents-postgres"
                        [ testCase "migrations run once" (migrationsTest baseUrl)
                        , testCase "compare-and-store, labels, and conflicts" (casTest baseUrl)
                        , testCase "queries filter, order, and limit" (queryTest baseUrl)
                        , testCase "concurrent compare-and-stores: exactly one wins" (concurrentCasTest baseUrl)
                        , testCase "a runner flow: deferred call, completion, cascade delete" (runnerTest baseUrl)
                        , testCase "stored agents: put, replace, list, delete" (agentStoreTest baseUrl)
                        , testCase "the mail store round-trips envelopes across mailboxes" (mailStoreTest baseUrl)
                        , testCase "a run lease is held by one process, renewed, and free once expired or released" (leaseTest baseUrl)
                        , testCase "two processes append mail to one session without reusing a sequence number" (sharedMailTest baseUrl)
                        , testCase "two servers: the run is on one, the other queues mail for it and follows it" (twoServersTest baseUrl)
                        , testCase "two servers: the other takes the session over once its owner stops renewing" (takeOverTest baseUrl)
                        ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

migrationsTest :: String -> Assertion
migrationsTest baseUrl = withDatabase baseUrl $ \url -> do
    withPostgresStores url $ \_ -> pure ()
    withPostgresStores url $ \_ -> pure ()
    bracket (connectPostgreSQL url) close $ \conn -> do
        rows <- query_ conn "SELECT component, version FROM schema_migrations ORDER BY component, version" :: IO [(Text, Int)]
        rows @?= [("agents", 1), ("continuations", 1), ("session_mail", 1), ("sessions", 1), ("sessions", 2), ("sessions", 3), ("sessions", 4), ("session_watches", 1)]

casTest :: String -> Assertion
casTest baseUrl = withDatabase baseUrl $ \url -> withPostgresStores url $ \stores -> do
    let backend = stores.hsSessions
    sid <- newSessionId
    sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])
    now <- getCurrentTime
    let meta0 = (freshSessionMeta sid now){smAgent = Just "agent", smOwner = Just "alice", smSecurity = SessionSecurity True (Just "digest-1")}
    m1 <- expectRight =<< backend.sbCompareAndStore meta0 sess
    m1.smVersion @?= 1
    again <- backend.sbCompareAndStore meta0 sess
    again @?= Left (VersionConflict sid 0 1)
    m2 <- expectRight =<< backend.sbCompareAndStore m1{smStatus = StatusRunning} sess
    m2.smVersion @?= 2
    stale <- backend.sbCompareAndStore m1 sess
    stale @?= Left (VersionConflict sid 1 2)
    -- Unconditional stores keep a running status and the labels.
    backend.sbStore sid sess
    Just (_, loaded) <- backend.sbLoadMeta sid
    (loaded.smVersion, loaded.smStatus, loaded.smAgent, loaded.smOwner) @?= (3, StatusRunning, Just "agent", Just "alice")
    loaded.smCreatedAt @?= m1.smCreatedAt
    -- the session's security is stored with it and survives unconditional stores
    loaded.smSecurity @?= SessionSecurity True (Just "digest-1")
    listed <- backend.sbList
    map fst listed @?= [sid]
    backend.sbDelete sid
    gone <- backend.sbLoad sid
    isJust gone @?= False

queryTest :: String -> Assertion
queryTest baseUrl = withDatabase baseUrl $ \url -> withPostgresStores url $ \stores -> do
    let backend = stores.hsSessions
    parent <- store backend (\m -> m{smAgent = Just "a", smOwner = Just "alice"})
    child <- store backend (\m -> m{smAgent = Just "b", smParent = Just parent.smSessionId})
    other <- store backend (\m -> m{smAgent = Just "a", smOwner = Just "bob", smStatus = StatusIdle})
    let ids q = map (.smSessionId) <$> backend.sbQuery q
    ids allSessionsQuery >>= (@?= map (.smSessionId) [other, child, parent])
    ids allSessionsQuery{sqAgent = Just "a"} >>= (@?= map (.smSessionId) [other, parent])
    ids allSessionsQuery{sqOwner = Just "alice"} >>= (@?= [parent.smSessionId])
    ids allSessionsQuery{sqParent = Just parent.smSessionId} >>= (@?= [child.smSessionId])
    ids allSessionsQuery{sqStatuses = Just [StatusIdle]} >>= (@?= [other.smSessionId])
    ids allSessionsQuery{sqStatuses = Just []} >>= (@?= [])
    ids allSessionsQuery{sqLimit = Just 2} >>= (@?= map (.smSessionId) [other, child])
    ids allSessionsQuery{sqUpdatedBefore = Just child.smUpdatedAt} >>= (@?= [parent.smSessionId])
  where
    store :: SessionBackend -> (SessionMeta -> SessionMeta) -> IO SessionMeta
    store backend f = do
        sid <- newSessionId
        sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])
        now <- getCurrentTime
        expectRight =<< backend.sbCompareAndStore (f (freshSessionMeta sid now)) sess

concurrentCasTest :: String -> Assertion
concurrentCasTest baseUrl = withDatabase baseUrl $ \url -> withPostgresStores url $ \stores -> do
    let backend = stores.hsSessions
    sid <- newSessionId
    sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])
    now <- getCurrentTime
    m1 <- expectRight =<< backend.sbCompareAndStore (freshSessionMeta sid now) sess
    results <- forConcurrently [1 :: Int .. 16] $ \_ -> backend.sbCompareAndStore m1 sess
    length [() | Right _ <- results] @?= 1
    Just (_, final) <- backend.sbLoadMeta sid
    final.smVersion @?= 2

-- | The agent the runner test serves.
agentConfig :: Aeson.Value
agentConfig =
    Aeson.object
        [ "slug" .= ("pg-test" :: Text)
        , "apiKeyId" .= ("none" :: Text)
        , "flavor" .= ("OpenAIv1" :: Text)
        , "modelUrl" .= ("http://127.0.0.1:1" :: Text)
        , "modelName" .= ("mock" :: Text)
        , "announce" .= ("a test agent" :: Text)
        , "systemPrompt" .= ["You are a test" :: Text]
        , "builtinToolboxes" .= ([] :: [Text])
        , "mcpServers" .= ([] :: [Text])
        , "executionMode" .= ("asynchronous" :: Text)
        , "toolCallPolicyConfig" .= Aeson.object ["default" .= Aeson.object ["tag" .= ("defer" :: Text), "reason" .= ("external" :: Text)], "rules" .= ([] :: [Text])]
        ]

runnerTest :: String -> Assertion
runnerTest baseUrl = withDatabase baseUrl $ \url -> withSystemTempDirectory "agents-pg-host" $ \dir -> do
    let agentFile = dir </> "agent.json"
        keysFile = dir </> "keys.json"
    LByteString.writeFile agentFile $ Aeson.encode $ Aeson.object ["tag" .= ("OpenAIAgentDescription" :: Text), "contents" .= agentConfig]
    writeFile keysFile "{}"
    let cfg = (defaultHostConfig [agentFile] keysFile "unused.db"){hcCompletion = Just (const (firstThen [remoteCall "call_1"]))}
    withPostgresStores url $ \stores ->
        withHostStores cfg stores silent $ \host ->
            withSessionRunner host $ \runner -> do
                meta <- expectRight =<< createSessionAs runner (Just "alice") "pg-test" (Just (NewMessage "fetch it" [] False)) (Just UntilBlocked) Map.empty
                let sid = meta.smSessionId
                (blocked, _) <- expectRight =<< awaitRun runner sid 5
                blocked.smStatus @?= StatusWaitingExternal
                Just (sess, _) <- getSession runner sid
                token <- case [t | v <- pendingDeferredCalls sess, Just t <- [v.dcvToken]] of
                    [t] -> pure t
                    other -> assertFailure ("expected one token, got " <> show (length other))
                stores.hsContinuations.csFindSession token >>= (@?= Just sid)
                _ <- expectRight =<< completeCall runner token (TextResponse "42") True Map.empty
                (idle, _) <- expectRight =<< awaitRun runner sid 5
                idle.smStatus @?= StatusIdle
                again <- completeCall runner token (TextResponse "43") True Map.empty
                again @?= Left (TokenAlreadyCompleted token)
                plan <- expectRight =<< deleteSession runner sid DeleteForReal
                plan @?= DeletionPlan [sid] 1 False
                stores.hsContinuations.csCountSession sid >>= (@?= 0)
                getSession runner sid >>= \case
                    Nothing -> pure ()
                    Just _ -> assertFailure "the session survived its deletion"

agentStoreTest :: String -> Assertion
agentStoreTest baseUrl = withDatabase baseUrl $ \url -> withPostgresStores url $ \stores -> do
    agentStore <- maybe (assertFailure "no agent store") pure stores.hsAgents
    agent <- either (assertFailure . ("bad config: " <>)) pure (Aeson.parseEither Aeson.parseJSON agentConfig)
    first <- agentStore.asPut (Just "alice") agent
    first.saUpdatedBy @?= Just "alice"
    _ <- agentStore.asPut Nothing agent
    listed <- agentStore.asList
    map (\sa -> (Base.slug sa.saConfig, sa.saUpdatedBy)) listed @?= [("pg-test", Nothing)]
    agentStore.asDelete "pg-test" >>= (@?= True)
    agentStore.asDelete "pg-test" >>= (@?= False)
    agentStore.asList >>= (@?= 0) . length

-- | 'System.Agents.Postgres.mkPostgresMailStore', hydrating a fresh
-- durable mailbox from what an earlier one wrote (mirrors how the runner
-- re-hydrates a session's mailbox on load).
mailStoreTest :: String -> Assertion
mailStoreTest baseUrl = withDatabase baseUrl $ \url -> withPostgresStores url $ \stores -> do
    sid <- newSessionId
    mb1 <- newDurableMailbox stores.hsMail sid
    let outgoing =
            Outgoing
                { outId = Nothing
                , outFrom = FromUser Nothing
                , outPriority = Normal
                , outHops = 0
                , outBody = UserMessage (UserQuery "hi" [])
                }
    r1 <- mb1.mbSend outgoing :: IO (Either SendError Receipt)
    either (assertFailure . ("send failed: " <>) . show) (const (pure ())) r1
    r2 <- mb1.mbSend outgoing{outBody = UserMessage (UserQuery "again" [])}
    either (assertFailure . ("send failed: " <>) . show) (const (pure ())) r2
    -- Simulate the session being reloaded on another process: a fresh
    -- mailbox over the same durable store.
    mb2 <- newDurableMailbox stores.hsMail sid
    unread <- atomically (awaitMail mb2 0 (const True))
    length unread @?= 2
    map envSeq unread @?= [1, 2]

-------------------------------------------------------------------------------
-- Several processes on one database
-------------------------------------------------------------------------------

-- | Two sets of stores on the database, as two processes would open them.
withTwoProcesses :: Char8.ByteString -> (HostStores -> HostStores -> IO a) -> IO a
withTwoProcesses url k = withPostgresStores url $ \a -> withPostgresStores url $ \b -> k a b

leaseTest :: String -> Assertion
leaseTest baseUrl = withDatabase baseUrl $ \url -> withTwoProcesses url $ \a b -> do
    let coA = a.hsCoordination
        coB = b.hsCoordination
    assertBool "each process has its own name" (coA.coInstance /= coB.coInstance)
    sid <- newSessionId
    coA.coAcquire sid 60 >>= (@?= LeaseNoSession)
    now <- getCurrentTime
    sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])
    _ <- expectRight =<< a.hsSessions.sbCompareAndStore (freshSessionMeta sid now){smStatus = StatusRunning} sess
    -- Stored as running by a process that left no lease: only startup recovers it.
    coB.coExpired >>= (@?= [])
    coB.coAbandoned >>= (@?= [sid])
    coA.coAcquire sid 60 >>= (@?= LeaseAcquired)
    coA.coAcquire sid 60 >>= (@?= LeaseAcquired)
    coB.coAcquire sid 60 >>= (@?= LeaseHeldBy coA.coInstance)
    coB.coHolder sid >>= (@?= Just coA.coInstance)
    coA.coHolder sid >>= (@?= Nothing)
    coB.coRenew [sid] 60 >>= (@?= [])
    coB.coRelease sid
    coB.coHolder sid >>= (@?= Just coA.coInstance)
    coB.coAbandoned >>= (@?= [])
    -- Renewed for a short while, then left to expire.
    coA.coRenew [sid] 0.2 >>= (@?= [sid])
    threadDelay 400_000
    coB.coHolder sid >>= (@?= Nothing)
    coB.coExpired >>= (@?= [sid])
    coA.coExpired >>= (@?= [])
    coB.coAcquire sid 60 >>= (@?= LeaseAcquired)
    coA.coRenew [sid] 60 >>= (@?= [])
    coA.coAcquire sid 60 >>= (@?= LeaseHeldBy coB.coInstance)
    coB.coRelease sid
    coA.coHolder sid >>= (@?= Nothing)
    coA.coAcquire sid 60 >>= (@?= LeaseAcquired)

sharedMailTest :: String -> Assertion
sharedMailTest baseUrl = withDatabase baseUrl $ \url -> withTwoProcesses url $ \a b -> do
    sid <- newSessionId
    heardByA <- newIORef []
    stop <- a.hsCoordination.coListen $ \s signal -> modifyIORef' heardByA ((s, signal) :)
    -- Both open the mailbox while it is empty: each would number from 1.
    mbA <- newDurableMailbox a.hsMail sid
    mbB <- newDurableMailbox b.hsMail sid
    let send :: Mailbox -> Text -> IO Receipt
        send mb text = expectRight =<< mb.mbSend (Outgoing Nothing (FromUser Nothing) Normal 0 (UserMessage (UserQuery text [])))
    r1 <- send mbA "from a"
    r2 <- send mbB "from b"
    r3 <- send mbA "from a again"
    map rcptSeq [r1, r2, r3] @?= [1, 2, 3]
    -- The second send of A caught up with B's on its way.
    unreadA <- atomically (awaitMail mbA 0 (const True))
    map envSeq unreadA @?= [1, 2, 3]
    mbB.mbSync
    unreadB <- atomically (awaitMail mbB 0 (const True))
    map envSeq unreadB @?= [1, 2, 3]
    -- A hears of B's mail, and not of its own.
    waitUntil $ (== [(sid, MailAccepted)]) <$> readIORef heardByA
    stop

-- | A server on the database: its own stores, host and runner, with the given LLM.
withServerOn :: Char8.ByteString -> RunnerConfig -> (HostStores -> HostStores) -> Completion -> (SessionRunner -> IO a) -> IO a
withServerOn url config adjust completion k = withSystemTempDirectory "agents-pg-host" $ \dir -> do
    let agentFile = dir </> "agent.json"
        keysFile = dir </> "keys.json"
    LByteString.writeFile agentFile $ Aeson.encode $ Aeson.object ["tag" .= ("OpenAIAgentDescription" :: Text), "contents" .= agentConfig]
    writeFile keysFile "{}"
    let cfg = (defaultHostConfig [agentFile] keysFile "unused.db"){hcCompletion = Just (const completion)}
    withPostgresStores url $ \stores ->
        withHostStores cfg (adjust stores) silent $ \host ->
            bracket (newSessionRunnerWith config host) shutdownSessionRunner k

twoServersTest :: String -> Assertion
twoServersTest baseUrl = withDatabase baseUrl $ \url -> do
    gate <- newGate
    withServerOn url defaultRunnerConfig id (gated gate) $ \serverA ->
        withServerOn url defaultRunnerConfig id answer $ \serverB -> do
            meta <- expectRight =<< createSessionAs serverA (Just "alice") "pg-test" (Just (NewMessage "hello" [] False)) (Just UntilBlocked) Map.empty
            let sid = meta.smSessionId
            readMVar gate.gateEntered
            -- Starting next to a live server recovers nothing of what it runs.
            recoverOnStartup serverB >>= (@?= [])
            resume serverB sid UntilBlocked Map.empty >>= (@?= Left (RunInProgress sid)) . void
            deleteSession serverB sid DeleteForReal >>= (@?= Left (RunInProgress sid)) . void
            (rsActiveRuns <$> runnerStats serverB) >>= (@?= 0)
            (running, active) <- expectRight =<< awaitRun serverB sid 0.2
            running.smStatus @?= StatusRunning
            assertBool "the run counts as active from the other server" active
            -- A message for the busy session becomes mail, which its owner hears of.
            _ <- expectRight =<< postMessage serverB sid (NewMessage "one more thing" [] False) (Just UntilBlocked) Map.empty
            waitUntil $ do
                mail <- either (const []) id <$> listMail serverA sid False
                pure ([q | UserMessage (UserQuery q _) <- map (.envBody) mail] == ["one more thing"])
            -- The other server follows the run: its events, and its end.
            next <- subscribeSession serverB sid
            openGate gate
            (done, stillActive) <- expectRight =<< awaitRun serverB sid 10
            assertBool "the run stopped" (not stillActive)
            assertBool ("the session is not running: " <> show done.smStatus) (done.smStatus /= StatusRunning)
            kinds <- drainKinds next
            assertBool ("session.updated was forwarded: " <> show kinds) ("session.updated" `elem` kinds)
            -- Released: the other server runs the session now.
            waitUntil $ (== Right False) . fmap snd <$> awaitRun serverA sid 0
            Just (_, before) <- getSession serverB sid
            _ <-
                expectRight
                    =<< if before.smStatus == StatusIdle
                        then postMessage serverB sid (NewMessage "again" [] False) (Just UntilBlocked) Map.empty
                        else resume serverB sid UntilBlocked Map.empty
            (final, _) <- expectRight =<< awaitRun serverB sid 10
            final.smStatus @?= StatusIdle
  where
    drainKinds :: IO Event -> IO [Text]
    drainKinds next =
        timeoutIO 300_000 next >>= \case
            Nothing -> pure []
            Just ev -> (eventKind ev.evBody :) <$> drainKinds next

takeOverTest :: String -> Assertion
takeOverTest baseUrl = withDatabase baseUrl $ \url -> do
    gate <- newGate
    cutOff <- newIORef False
    -- A server that can be cut off from its leases: it stops renewing, as a
    -- dead one would, while its run is still in memory.
    let flaky stores =
            stores
                { hsCoordination =
                    stores.hsCoordination
                        { coRenew = \sids ttl -> do
                            cut <- readIORef cutOff
                            when cut $ throwIO (userError "cut off")
                            stores.hsCoordination.coRenew sids ttl
                        }
                }
        config = defaultRunnerConfig{rcLeaseTtl = 1}
    withServerOn url config flaky (gated gate) $ \serverA ->
        withServerOn url config id answer $ \serverB -> do
            meta <- expectRight =<< createSessionAs serverA (Just "alice") "pg-test" (Just (NewMessage "hello" [] False)) (Just UntilBlocked) Map.empty
            let sid = meta.smSessionId
            readMVar gate.gateEntered
            -- Renewed: well past the lease's duration, the run is still A's.
            threadDelay 1_500_000
            resume serverB sid UntilBlocked Map.empty >>= (@?= Left (RunInProgress sid)) . void
            writeIORef cutOff True
            -- B's own heartbeat takes the session over, once the lease expired.
            waitUntil $ maybe False ((== StatusReady) . (.smStatus) . snd) <$> getSession serverB sid
            _ <- expectRight =<< resume serverB sid UntilBlocked Map.empty
            (done, _) <- expectRight =<< awaitRun serverB sid 10
            done.smStatus @?= StatusIdle
            -- A comes back: it finds the lease gone, and stores nothing.
            writeIORef cutOff False
            openGate gate
            waitUntil $ (== 0) . rsActiveRuns <$> runnerStats serverA
            Just (_, late) <- getSession serverA sid
            late.smStatus @?= StatusIdle
            late.smVersion @?= done.smVersion

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
    answer completion

-- | An LLM that answers at once, calling no tool.
answer :: Completion
answer _ = pure (LlmResponse (Just "done") Nothing Aeson.Null Nothing, [])

-- | Poll a condition every 20ms for up to 10 seconds.
waitUntil :: IO Bool -> Assertion
waitUntil check = go (500 :: Int)
  where
    go 0 = assertFailure "condition not reached in time"
    go n = do
        ok <- check
        if ok then pure () else threadDelay 20_000 >> go (n - 1)

timeoutIO :: Int -> IO a -> IO (Maybe a)
timeoutIO = timeout

-------------------------------------------------------------------------------
-- Mock LLM
-------------------------------------------------------------------------------

-- | The first completion of a turn calls the tools; the next one answers.
firstThen :: [LlmToolCall] -> Completion
firstThen calls completion
    | null completion.completeToolResponses = pure (LlmResponse Nothing Nothing Aeson.Null Nothing, calls)
    | otherwise = pure (LlmResponse (Just "done") Nothing Aeson.Null Nothing, [])

remoteCall :: Text -> LlmToolCall
remoteCall callId =
    LlmToolCall $
        Aeson.object
            [ "id" .= callId
            , "type" .= ("function" :: Text)
            , "function" .= Aeson.object ["name" .= ("fetch_remote" :: Text), "arguments" .= ("{}" :: Text)]
            ]

-------------------------------------------------------------------------------
-- Postgres server
-------------------------------------------------------------------------------

-- | How to get a server: an existing one, a throwaway cluster, or none.
findServer :: IO (Maybe ((String -> IO ()) -> IO ()))
findServer =
    lookupEnv "AGENTS_TEST_POSTGRES_URL" >>= \case
        Just url -> pure $ Just ($ url)
        Nothing -> do
            bindir <- try (readProcess "pg_config" ["--bindir"] "")
            case bindir of
                Left (_ :: IOException) -> pure Nothing
                Right out -> do
                    let dir = takeWhile (/= '\n') out
                    found <- doesFileExist (dir </> "initdb")
                    pure $ if found then Just (withCluster dir) else Nothing

-- | A cluster in a temporary directory, stopped afterwards.
withCluster :: FilePath -> (String -> IO ()) -> IO ()
withCluster bindir k = withSystemTempDirectory "agents-pg" $ \dir -> do
    let dataDir = dir </> "data"
    port <- freePort
    _ <- readProcess (bindir </> "initdb") ["-D", dataDir, "-A", "trust", "-U", "postgres", "-E", "UTF8", "--no-sync"] ""
    let options = "-p " <> show port <> " -k " <> dir <> " -c listen_addresses=127.0.0.1 -c fsync=off"
        pgCtl args = callProcess (bindir </> "pg_ctl") (["-D", dataDir, "-l", dir </> "log", "-s"] <> args)
    bracket_
        (pgCtl ["-o", options, "-w", "start"])
        (pgCtl ["-m", "immediate", "-w", "stop"])
        (k ("postgresql://postgres@127.0.0.1:" <> show port))

freePort :: IO Int
freePort = bracket (socket AF_INET Stream defaultProtocol) Socket.close $ \sock -> do
    bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    getSocketName sock >>= \case
        SockAddrInet port _ -> pure (fromIntegral port)
        other -> fail ("unexpected address " <> show other)

databaseCounter :: IORef Int
databaseCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE databaseCounter #-}

-- | A fresh database on the server, for one test.
withDatabase :: String -> (Char8.ByteString -> IO a) -> IO a
withDatabase baseUrl k = do
    n <- atomicModifyIORef' databaseCounter (\i -> (i + 1, i))
    now <- getCurrentTime
    let name = "agents_test_" <> show n <> "_" <> filter (`elem` ['0' .. '9']) (show now)
        admin = Char8.pack (baseUrl <> "/postgres")
    bracket_
        (withConn admin $ \c -> void $ execute_ c (fromStringQ ("CREATE DATABASE " <> name)))
        (withConn admin $ \c -> void $ execute_ c (fromStringQ ("DROP DATABASE " <> name <> " WITH (FORCE)")))
        (k (Char8.pack (baseUrl <> "/" <> name)))
  where
    withConn url = bracket (connectPostgreSQL url) close
    fromStringQ = fromString

expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> assertFailure ("unexpected error: " <> show e)) pure
