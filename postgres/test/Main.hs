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

import Control.Concurrent.Async (forConcurrently)
import Control.Exception (IOException, bracket, bracket_, try)
import Control.Monad (void)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef)
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
import System.Process (callProcess, readProcess)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.AgentFactory (Completion)
import System.Agents.Host
import System.Agents.Host.Runner
import System.Agents.Postgres
import System.Agents.Session.Async (ContinuationStore (..))
import System.Agents.Session.Base (
    DeferredCallView (..),
    LlmCompletion (..),
    LlmResponse (..),
    LlmToolCall (..),
    SessionStatus (..),
    SystemPrompt (..),
    UserQuery (..),
    UserToolResponse (..),
    newSessionFromPrompt,
    newSessionId,
    pendingDeferredCalls,
 )
import System.Agents.SessionStore

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
        rows @?= [("continuations", 1), ("sessions", 1)]

casTest :: String -> Assertion
casTest baseUrl = withDatabase baseUrl $ \url -> withPostgresStores url $ \stores -> do
    let backend = stores.hsSessions
    sid <- newSessionId
    sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])
    now <- getCurrentTime
    let meta0 = (freshSessionMeta sid now){smAgent = Just "agent", smOwner = Just "alice"}
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
                meta <- expectRight =<< createSessionAs runner (Just "alice") "pg-test" (NewMessage "fetch it" []) (Just UntilBlocked)
                let sid = meta.smSessionId
                (blocked, _) <- expectRight =<< awaitRun runner sid 5
                blocked.smStatus @?= StatusWaitingExternal
                Just (sess, _) <- getSession runner sid
                token <- case [t | v <- pendingDeferredCalls sess, Just t <- [v.dcvToken]] of
                    [t] -> pure t
                    other -> assertFailure ("expected one token, got " <> show (length other))
                stores.hsContinuations.csFindSession token >>= (@?= Just sid)
                _ <- expectRight =<< completeCall runner token (TextResponse "42") True
                (idle, _) <- expectRight =<< awaitRun runner sid 5
                idle.smStatus @?= StatusIdle
                again <- completeCall runner token (TextResponse "43") True
                again @?= Left (TokenAlreadyCompleted token)
                plan <- expectRight =<< deleteSession runner sid DeleteForReal
                plan @?= DeletionPlan [sid] 1 False
                stores.hsContinuations.csCountSession sid >>= (@?= 0)
                getSession runner sid >>= \case
                    Nothing -> pure ()
                    Just _ -> assertFailure "the session survived its deletion"
  where
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
