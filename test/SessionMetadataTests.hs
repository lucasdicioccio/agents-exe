{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for session metadata, versions, migrations, and catalogs.
module SessionMetadataTests (tests) where

import Control.Concurrent.STM (atomically, writeTVar)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.List (sort)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (Connection, Only (..), execute, execute_, open, query_)
import Prod.Tracer (silent)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, mockDeps, testNode)
import System.Agents.AgentFactory
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (AgentId (..), SessionIntrospectionScope (..))
import qualified System.Agents.Base as Base
import System.Agents.Session.Base
import System.Agents.Session.Loop (run)
import System.Agents.SessionStore
import System.Agents.Tools.SystemToolbox.Session (getListSessionsInfo)
import System.Agents.Tools.SystemToolbox.Types (SessionIntrospectionConfig (..), defaultSessionIntrospectionConfig)

tests :: TestTree
tests =
    testGroup
        "Session metadata"
        [ statusTests
        , sqliteTests
        , migrationTests
        , fileBackendTests
        , catalogTests
        , testCase "sub-agent sessions record their agent and parent" subAgentParentTest
        ]

-------------------------------------------------------------------------------
-- Status
-------------------------------------------------------------------------------

statusTests :: TestTree
statusTests =
    testGroup
        "sessionStatusOf"
        [ testCase "a pending user turn is ready" $ do
            sess <- readySession
            sessionStatusOf sess @?= StatusReady
        , testCase "a final answer is idle" $ do
            sess <- idleSession
            sessionStatusOf sess @?= StatusIdle
        , testCase "only deferred calls left is waiting_external" $ do
            sess <- waitingSession
            sessionStatusOf sess @?= StatusWaitingExternal
        , testCase "status text round-trips" $
            mapM_ (\s -> parseSessionStatus (sessionStatusText s) @?= Just s) [minBound .. maxBound]
        ]

-------------------------------------------------------------------------------
-- SQLite backend
-------------------------------------------------------------------------------

sqliteTests :: TestTree
sqliteTests =
    testGroup
        "SQLite backend"
        [ testCase "each store increments the version and derives the status" $ do
            backend <- sqliteBackend
            sess <- readySession
            backend.sbStore sess.sessionId sess
            Just (_, m1) <- backend.sbLoadMeta sess.sessionId
            (m1.smVersion, m1.smStatus) @?= (1, StatusReady)
            idle <- idleSessionWithId sess.sessionId
            backend.sbStore sess.sessionId idle
            Just (_, m2) <- backend.sbLoadMeta sess.sessionId
            (m2.smVersion, m2.smStatus) @?= (2, StatusIdle)
        , testCase "labels are kept by stores without labels" $ do
            backend <- sqliteBackend
            sess <- readySession
            parent <- SessionId <$> nextRandom
            backend.sbStoreLabelled (SessionLabels (Just "helper") (Just parent) Nothing) sess.sessionId sess
            backend.sbStore sess.sessionId sess
            Just (_, meta) <- backend.sbLoadMeta sess.sessionId
            (meta.smAgent, meta.smParent) @?= (Just "helper", Just parent)
        , testCase "compare-and-store writes when the version matches" $ do
            backend <- sqliteBackend
            sess <- readySession
            backend.sbStore sess.sessionId sess
            Just (_, meta) <- backend.sbLoadMeta sess.sessionId
            result <- backend.sbCompareAndStore meta{smStatus = StatusRunning} sess
            fmap (\m -> (m.smVersion, m.smStatus)) result @?= Right (2, StatusRunning)
        , testCase "compare-and-store rejects a stale version" $ do
            backend <- sqliteBackend
            sess <- readySession
            backend.sbStore sess.sessionId sess
            Just (_, stale) <- backend.sbLoadMeta sess.sessionId
            backend.sbStore sess.sessionId sess
            result <- backend.sbCompareAndStore stale sess
            result @?= Left (VersionConflict sess.sessionId 1 2)
        , testCase "compare-and-store at version 0 creates, but not twice" $ do
            backend <- sqliteBackend
            sess <- readySession
            fresh <- freshMeta sess.sessionId
            first <- backend.sbCompareAndStore fresh sess
            fmap (.smVersion) first @?= Right 1
            second <- backend.sbCompareAndStore fresh sess
            second @?= Left (VersionConflict sess.sessionId 0 1)
        , testCase "unconditional stores keep a running status" $ do
            backend <- sqliteBackend
            sess <- readySession
            fresh <- freshMeta sess.sessionId
            _ <- backend.sbCompareAndStore fresh{smStatus = StatusRunning} sess
            idle <- idleSessionWithId sess.sessionId
            backend.sbStore sess.sessionId idle
            Just (_, meta) <- backend.sbLoadMeta sess.sessionId
            meta.smStatus @?= StatusRunning
        , testCase "queries filter by agent, status, parent, and limit" $ do
            backend <- sqliteBackend
            parent <- SessionId <$> nextRandom
            a <- readySession
            b <- idleSession
            c <- idleSession
            backend.sbStoreLabelled (SessionLabels (Just "alpha") Nothing Nothing) a.sessionId a
            backend.sbStoreLabelled (SessionLabels (Just "beta") (Just parent) Nothing) b.sessionId b
            backend.sbStoreLabelled (SessionLabels (Just "beta") Nothing Nothing) c.sessionId c
            byAgent <- backend.sbQuery allSessionsQuery{sqAgent = Just "beta"}
            length byAgent @?= 2
            idle <- backend.sbQuery allSessionsQuery{sqStatuses = Just [StatusIdle]}
            map (.smSessionId) idle `sameElements` [b.sessionId, c.sessionId]
            children <- backend.sbQuery allSessionsQuery{sqParent = Just parent}
            map (.smSessionId) children @?= [b.sessionId]
            limited <- backend.sbQuery allSessionsQuery{sqLimit = Just 1}
            map (.smSessionId) limited @?= [c.sessionId]
            none <- backend.sbQuery allSessionsQuery{sqStatuses = Just []}
            none @?= []
        , testCase "composite backends load metadata from the fallback" $ do
            primary <- sqliteBackend
            secondary <- sqliteBackend
            sess <- readySession
            secondary.sbStore sess.sessionId sess
            let composite = mkCompositeSessionStore [primary, secondary]
            loaded <- composite.sbLoadMeta sess.sessionId
            fmap ((.smVersion) . snd) loaded @?= Just 1
        ]

-------------------------------------------------------------------------------
-- Migrations
-------------------------------------------------------------------------------

migrationTests :: TestTree
migrationTests =
    testGroup
        "migrations"
        [ testCase "a database from before metadata is migrated in place" $ do
            conn <- open ":memory:"
            -- The schema the first SQLite backend created, with one session.
            execute_ conn "CREATE TABLE sessions (session_id TEXT PRIMARY KEY, created_at TIMESTAMP NOT NULL, updated_at TIMESTAMP NOT NULL, json TEXT NOT NULL)"
            sess <- idleSession
            execute
                conn
                "INSERT INTO sessions (session_id, created_at, updated_at, json) VALUES (?, datetime('now'), datetime('now'), ?)"
                (sessionIdText sess.sessionId, encodeText sess)
            backend <- mkSqliteSessionStore conn
            Just (loaded, meta) <- backend.sbLoadMeta sess.sessionId
            loaded @?= sess
            (meta.smVersion, meta.smStatus) @?= (0, StatusIdle)
            -- Old rows can be taken over with a compare-and-store at version 0.
            result <- backend.sbCompareAndStore meta sess
            fmap (.smVersion) result @?= Right 1
        , testCase "a session's security (sealed, token digest) survives a store and a reload, but is never serialized" $ do
            conn <- open ":memory:"
            backend <- mkSqliteSessionStore conn
            sess <- readySession
            now <- getCurrentTime
            let security = SessionSecurity True (Just "abc123digest")
                meta0 = (freshSessionMeta sess.sessionId now){smSecurity = security}
            Right stored <- backend.sbCompareAndStore meta0 sess
            Just (_, loaded) <- backend.sbLoadMeta sess.sessionId
            loaded.smSecurity @?= security
            -- a later store from the loaded meta carries it on
            Right again <- backend.sbCompareAndStore loaded sess
            again.smSecurity @?= security
            -- clients see whether it is sealed, never the digest
            let wire = Text.pack (show (Aeson.encode stored))
            assertBool "sealed is shown" ("sealed" `Text.isInfixOf` wire)
            assertBool "the digest is not" (not ("abc123digest" `Text.isInfixOf` wire))
            Aeson.fromJSON (Aeson.toJSON stored) @?= Aeson.Success stored{smSecurity = SessionSecurity True Nothing}
        , testCase "migrations run once" $ do
            conn <- open ":memory:"
            initializeSessionSchema conn
            initializeSessionSchema conn
            versions <- query_ conn "SELECT version FROM schema_migrations WHERE component = 'sessions' ORDER BY version" :: IO [Only Int]
            map fromOnly versions @?= [1, 2, 3, 4, 5]
        ]

-------------------------------------------------------------------------------
-- File backend
-------------------------------------------------------------------------------

fileBackendTests :: TestTree
fileBackendTests =
    testGroup
        "file backend"
        [ testCase "stores metadata next to the session" $
            withSystemTempDirectory "session-meta" $ \dir -> do
                backend <- mkFileSessionStore dir
                sess <- idleSession
                backend.sbStoreLabelled (SessionLabels (Just "helper") Nothing Nothing) sess.sessionId sess
                Just (_, meta) <- backend.sbLoadMeta sess.sessionId
                (meta.smVersion, meta.smStatus, meta.smAgent) @?= (1, StatusIdle, Just "helper")
                listed <- backend.sbList
                map fst listed @?= [sess.sessionId]
        , testCase "a session file without metadata reads as version 0" $
            withSystemTempDirectory "session-meta" $ \dir -> do
                backend <- mkFileSessionStore dir
                sess <- readySession
                storeSessionToFile sess (dir </> ("conv." <> sessionIdString sess.sessionId <> ".json"))
                Just (_, meta) <- backend.sbLoadMeta sess.sessionId
                meta.smVersion @?= 0
        , testCase "compare-and-store rejects a stale version" $
            withSystemTempDirectory "session-meta" $ \dir -> do
                backend <- mkFileSessionStore dir
                sess <- readySession
                backend.sbStore sess.sessionId sess
                Just (_, stale) <- backend.sbLoadMeta sess.sessionId
                backend.sbStore sess.sessionId sess
                result <- backend.sbCompareAndStore stale sess
                assertBool "conflict" (isLeft result)
        ]

-------------------------------------------------------------------------------
-- Catalogs
-------------------------------------------------------------------------------

catalogTests :: TestTree
catalogTests =
    testGroup
        "catalogs"
        [ testCase "a backend catalog lists sessions and marks running ones busy" $ do
            backend <- sqliteBackend
            idle <- idleSession
            running <- readySession
            backend.sbStore idle.sessionId idle
            fresh <- freshMeta running.sessionId
            _ <- backend.sbCompareAndStore fresh{smStatus = StatusRunning} running
            entries <- (backendCatalog backend).catList
            [(isJust e.ceSession, e.ceBusy) | e <- entries] `sameElements` [(True, False), (True, True)]
            found <- (backendCatalog backend).catRead (sessionIdToConversationId idle.sessionId)
            found @?= Just idle
        , testCase "a file catalog lists and reads session files" $
            withSystemTempDirectory "session-catalog" $ \dir -> do
                let store = mkSimpleSessionStore dir
                sess <- idleSession
                let convId = sessionIdToConversationId sess.sessionId
                storeSession store convId sess
                entries <- (fileCatalog store).catList
                [(e.ceConversationId, e.ceSession, e.ceBusy, isJust e.ceUpdatedAt) | e <- entries]
                    @?= [(convId, Just sess, False, True)]
                found <- (fileCatalog store).catRead convId
                found @?= Just sess
        , testCase "list-sessions reads from a backend catalog" $ do
            backend <- sqliteBackend
            sess <- idleSession
            backend.sbStore sess.sessionId sess
            let config = (defaultSessionIntrospectionConfig (backendCatalog backend)){introspectionScope = ScopeAll}
            (_, result) <- getListSessionsInfo (Just config)
            case result of
                Aeson.Object obj -> KeyMap.lookup "totalAccessible" obj @?= Just (Aeson.Number 1)
                _ -> assertFailure "expected an object"
        ]

-------------------------------------------------------------------------------
-- Sub-agents
-------------------------------------------------------------------------------

{- | A parent agent calls a sub-agent tool; both store into one backend, and
the sub-session names the parent session.
-}
subAgentParentTest :: Assertion
subAgentParentTest = do
    backend <- sqliteBackend
    child <- testNode "{\"slug\": \"child\"}"
    parent <- testNode "{\"slug\": \"parent\"}"
    let deps =
            mockDeps
                { adSessionSink = SinkBackend backend
                , adCompletion = Just $ \node completion ->
                    if Base.slug node.osNodeConfig == "parent" && null completion.completeToolResponses
                        then pure (LlmResponse Nothing Nothing Aeson.Null Nothing, [callChild])
                        else mockCompletion completion
                }
    childAgentId <- AgentId <$> nextRandom
    atomically $ writeTVar parent.osNodeTools [OneShotTool.turnAgentRuntimeIntoIOTool silent deps child "parent" childAgentId Nothing True]
    sid <- newSessionId
    let convId = sessionIdToConversationId sid
    agent <- buildAgent silent deps RootAgent convId parent
    sPrompt <- agent.sysPrompt
    sTools <- agent.sysTools
    sess0 <- newSessionFromPrompt sid sPrompt sTools (UserQuery "ask the child" [])
    (_, final) <- run convId agent sess0
    assertBool "parent answered" (sessionStatusOf final == StatusIdle)
    children <- backend.sbQuery allSessionsQuery{sqParent = Just sid}
    map (.smAgent) children @?= [Just "child"]
    Just (_, parentMeta) <- backend.sbLoadMeta sid
    (parentMeta.smAgent, parentMeta.smParent) @?= (Just "parent", Nothing)
  where
    callChild =
        LlmToolCall $
            Aeson.object
                [ "id" Aeson..= ("call_1" :: String)
                , "type" Aeson..= ("function" :: String)
                , "function"
                    Aeson..= Aeson.object
                        [ "name" Aeson..= ("io_prompt_agent_child" :: String)
                        , "arguments" Aeson..= ("{\"what\": \"hello child\"}" :: String)
                        ]
                ]

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

sqliteBackend :: IO SessionBackend
sqliteBackend = mkSqliteSessionStore =<< (open ":memory:" :: IO Connection)

freshMeta :: SessionId -> IO SessionMeta
freshMeta sid = freshSessionMeta sid <$> getCurrentTime

readySession :: IO Session
readySession = do
    sid <- newSessionId
    newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])

idleSession :: IO Session
idleSession = newSessionId >>= idleSessionWithId

idleSessionWithId :: SessionId -> IO Session
idleSessionWithId sid = do
    sess <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])
    let answer = LlmTurn (LlmTurnContent (LlmResponse (Just "done") Nothing Aeson.Null Nothing) []) Nothing
    pure sess{turns = answer : sess.turns}

waitingSession :: IO Session
waitingSession = do
    sess <- readySession
    callId <- newToolCallId
    token <- newContinuationToken
    let deferred =
            TrackedToolCall
                { tcId = callId
                , tcCall = LlmToolCall Aeson.Null
                , tcState = Deferred
                , tcResult = Nothing
                , tcContinuation = Just token
                , tcPolicy = AppliedPolicy (Defer (Reason "external")) Nothing
                , tcEntityId = Nothing
                , tcDeliveredLate = False
                }
    let partial = PartialUserTurnContent (SystemPrompt "sys") [] Nothing [deferred] [] False
    pure sess{turns = PartialUserTurn partial Nothing : sess.turns}

sessionIdString :: SessionId -> String
sessionIdString (SessionId uuid) = show uuid

sessionIdText :: SessionId -> Text.Text
sessionIdText = Text.pack . sessionIdString

encodeText :: Session -> Text.Text
encodeText = TextEnc.decodeUtf8 . LBS.toStrict . Aeson.encode

sameElements :: (Show a, Ord a) => [a] -> [a] -> Assertion
sameElements xs ys = assertEqual "same elements" (sort xs) (sort ys)
