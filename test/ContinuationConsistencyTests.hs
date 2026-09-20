{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests that continuations and sessions stay consistent when waking.
module ContinuationConsistencyTests (tests) where

import qualified Data.Aeson as Aeson
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Database.SQLite.Simple (Only (..), open, query_)
import Prod.Tracer (silent)
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, mockDeps, testNode)
import System.Agents.AgentFactory
import System.Agents.Base (ConversationId)
import System.Agents.Session.Async (ContinuationStore (..), mkSqliteContinuationStore)
import System.Agents.Session.Base
import System.Agents.Session.Loop (runUntilBlocked)
import System.Agents.Session.Wake
import System.Agents.SessionStore

tests :: TestTree
tests =
    testGroup
        "Continuation consistency"
        [ testCase "waking marks the continuation completed" $ do
            env <- pausedRun
            outcome <- wakeSessionWith (Just env.store) Nothing env.paused [(env.token, TextResponse "42")]
            outcome.woApplied @?= [env.token]
            env.store.csLoad env.token >>= (@?= Nothing) . fmap (const ())
            env.store.csFindSession env.token >>= (@?= Just env.sid)
            sessionStatusOf outcome.woSession @?= StatusReady
        , testCase "waking twice reports the token as already completed" $ do
            env <- pausedRun
            first <- wakeSessionWith (Just env.store) Nothing env.paused [(env.token, TextResponse "42")]
            second <- wakeSessionWith (Just env.store) Nothing first.woSession [(env.token, TextResponse "43")]
            (second.woApplied, second.woAlreadyCompleted, second.woUnknown) @?= ([], [env.token], [])
            second.woSession @?= first.woSession
            completedAgain <- env.store.csComplete env.token (TextResponse "44")
            completedAgain @?= False
        , testCase "a token the session never issued is unknown" $ do
            env <- pausedRun
            other <- newContinuationToken
            outcome <- wakeSessionWith (Just env.store) Nothing env.paused [(other, TextResponse "42")]
            (outcome.woApplied, outcome.woAlreadyCompleted, outcome.woUnknown) @?= ([], [], [other])
            outcome.woSession @?= env.paused
        , testCase "the woken session resumes to a final answer" $ do
            env <- pausedRun
            outcome <- wakeSessionWith (Just env.store) Nothing env.paused [(env.token, TextResponse "42")]
            result <- runUntilBlocked env.convId env.agent outcome.woSession
            case result of
                Left (_, final) -> sessionStatusOf final @?= StatusIdle
                Right _ -> assertFailure "expected the session to complete"
        , testCase "finding a token's session uses the index, not the sessions" $ do
            env <- pausedRun
            (counting, loads) <- countLoads env.backend
            found <- findSessionForToken (Just env.store) counting env.token
            found @?= Just env.sid
            readIORef loads >>= (@?= 0)
        , testCase "without an index, the sessions are searched" $ do
            env <- pausedRun
            found <- findSessionForToken Nothing env.backend env.token
            found @?= Just env.sid
            other <- newContinuationToken
            findSessionForToken Nothing env.backend other >>= (@?= Nothing)
        , testCase "continuation migrations run once" $ do
            conn <- open ":memory:"
            _ <- mkSqliteContinuationStore conn
            _ <- mkSqliteContinuationStore conn
            versions <- query_ conn "SELECT version FROM schema_migrations WHERE component = 'continuations' ORDER BY version" :: IO [Only Int]
            map fromOnly versions @?= [1, 2]
        ]

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

data PausedRun = PausedRun
    { backend :: SessionBackend
    , store :: ContinuationStore
    , agent :: Agent (LlmTurnContent, Session)
    , sid :: SessionId
    , convId :: ConversationId
    , paused :: Session
    , token :: ContinuationToken
    }

{- | An asynchronous agent whose policy defers every call, run until it
waits on its one deferred call. Sessions and continuations share a database.
-}
pausedRun :: IO PausedRun
pausedRun = do
    conn <- open ":memory:"
    backend <- mkSqliteSessionStore conn
    store <- mkSqliteContinuationStore conn
    node <-
        testNode
            "{\"executionMode\": \"asynchronous\", \"toolCallPolicyConfig\": {\"default\": {\"tag\": \"defer\", \"reason\": \"external\"}, \"rules\": []}}"
    let deps =
            mockDeps
                { adSessionSink = SinkBackend backend
                , adContinuationStore = Just store
                , adCompletion = Just $ \_ completion ->
                    if null completion.completeToolResponses
                        then pure (LlmResponse Nothing Nothing Aeson.Null Nothing, [remoteCall])
                        else mockCompletion completion
                }
    sid <- newSessionId
    let convId = sessionIdToConversationId sid
    agent <- buildAgent silent deps RootAgent convId node
    sess0 <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "fetch it" [])
    result <- runUntilBlocked convId agent sess0
    paused <- case result of
        Right s -> pure s
        Left _ -> assertFailure "expected the run to wait on the deferred call"
    -- Agents store a session before each step, so the state a run stops in is
    -- stored by the caller, as the session commands and the runner do.
    backend.sbStore sid paused
    token <- case [t | PartialUserTurn p _ <- take 1 paused.turns, tc <- p.pTrackedToolCalls, Just t <- [tc.tcContinuation]] of
        [t] -> pure t
        other -> assertFailure ("expected one deferred call, got " <> show (length other))
    pure PausedRun{backend, store, agent, sid, convId, paused, token}
  where
    remoteCall =
        LlmToolCall $
            Aeson.object
                [ "id" Aeson..= ("call_1" :: String)
                , "type" Aeson..= ("function" :: String)
                , "function"
                    Aeson..= Aeson.object
                        [ "name" Aeson..= ("fetch_remote" :: String)
                        , "arguments" Aeson..= ("{}" :: String)
                        ]
                ]

-- | A backend that counts how many sessions are loaded through it.
countLoads :: SessionBackend -> IO (SessionBackend, IORef Int)
countLoads b = do
    loads <- newIORef 0
    let counted :: IO ()
        counted = modifyIORef' loads (+ 1)
    pure
        ( b
            { sbLoad = \s -> counted >> b.sbLoad s
            , sbLoadMeta = \s -> counted >> b.sbLoadMeta s
            }
        , loads
        )
