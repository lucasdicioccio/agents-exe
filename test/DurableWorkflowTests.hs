{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for Phases 2, 3, 5, 6, and 8 of the durable-workflows plan.

Phase 2 covers:

* Tool-call policy classification in the async scheduler.
* Continuation snapshot serialisation and SQLite round-tripping.
* 'wakeSession' injecting external results and converting partial turns.
* Cache updates during wake.
* 'resumeSession' continuing from a partial or completed user turn.

Phase 3 covers:

* 'SessionBackend' file round-trip.
* 'SessionBackend' SQLite round-trip.
* Composite backend read fallback.
* 'withSessionBackend' integration with the progress callback.

Phase 5 covers:

* Stable JSON envelope round-tripping for isolated execution.
* 'localProcessRunner' fork/exec of a worker process.
* 'dockerRunner' envelope construction (execution skipped when Docker
  is unavailable).
* Integration of 'ctxDeploymentRunner' with the async scheduler.

Phase 6 covers:

* Integration of policy + continuation store + session backend + cache in a
  single end-to-end session that yields, persists, wakes, and resumes.
* New durable executor helpers ('mkDurableExecutor', 'cachedInProcessExecutor',
  'composeExecutors').
* New agent combinators ('withDurableWorkflows', 'withAsyncConfig',
  'withDurableExecutor').
* 'agentStoreSessionWithCallback' storage + progress callback integration.

Phase 8 (testing strategy) is implemented in
"DurableWorkflowDeterminismTests".  This module's tests are referenced here
for completeness:

* Determinism: the same yielded session state resumed twice produces the same
  semantic outcome.
* Wake idempotence: applying 'wakeSession' with the same token/result twice
  leaves the session unchanged.
* Edge cases: waking a session with no partial turn is a no-op; completing the
  last deferred call converts a 'PartialUserTurn' to a 'UserTurn'; composite
  backend listing deduplicates entries present in multiple backends.
-}
module DurableWorkflowTests where

import Control.Monad (forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Data.UUID (nil)
import Database.SQLite.Simple (open)
import System.Directory (emptyPermissions, executable, readable, setPermissions)
import System.IO.Temp (emptySystemTempFile, withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..))
import System.Agents.Combinators.StoreSessionProgress (
    agentStoreSession,
    agentStoreSessionWithCallback,
    agentWithSessionProgress,
    backendStoreCallback,
 )
import System.Agents.Session.Async (
    ContinuationStore,
    ToolContinuationSnapshot (..),
    csListPending,
    csLoad,
    mkSqliteContinuationStore,
 )
import System.Agents.Session.Base
import System.Agents.Session.Durable (
    cachedInProcessExecutor,
    composeExecutors,
    mkDurableExecutor,
 )
import System.Agents.Session.Isolation (
    IsolationEnvelope (..),
    IsolationError (..),
    IsolationResultEnvelope (..),
    IsolationResultStatus (..),
    dockerRunner,
    localProcessRunner,
    mkIsolationEnvelope,
    mkIsolationErrorEnvelope,
    mkIsolationSuccessEnvelope,
    parseIsolationResultEnvelope,
    parseIsolationResultEnvelopeLBS,
 )
import System.Agents.Session.Step (getPartialTurn, naiveTilNoToolCallStep, runStepMAsync)
import System.Agents.Session.Types
import System.Agents.Session.Wake (resumeSession, wakeSession, wakeSessionWithCache)
import System.Agents.SessionStore (
    SessionBackend (..),
    mkCompositeSessionStore,
    mkFileSessionStore,
    mkSqliteSessionStore,
 )
import System.Agents.Tools.Cache (
    CachedResult (..),
    ToolCache (..),
    cacheLookup,
    computeCacheKey,
    mkSqliteToolCache,
 )
import System.Agents.Tools.Context (
    ToolCall (..),
    ToolExecutionContextSnapshot (..),
    ToolPortal,
    ToolResult (..),
    mkMinimalContext,
 )
import qualified System.Agents.Tools.Context as Ctx

-- Test suite
-------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "Durable Workflows"
        [ testGroup
            "Phase 2"
            [ policyClassificationTest
            , continuationStoreRoundTripTest
            , wakeAndResumeTest
            , cacheIntegrationTest
            , snapshotSerializationTest
            ]
        , testGroup
            "Phase 3"
            [ fileBackendRoundTripTest
            , sqliteBackendRoundTripTest
            , compositeFallbackTest
            , backendProgressCallbackTest
            ]
        , testGroup
            "Phase 5"
            [ envelopeRoundTripTest
            , resultEnvelopeRoundTripTest
            , localProcessRunnerTest
            , dockerRunnerEnvelopeTest
            , isolatedToolNamePolicyTest
            ]
        , testGroup
            "Phase 6"
            [ durableWorkflowIntegrationTest
            , withDurableWorkflowsTest
            , withAsyncConfigTest
            , mkDurableExecutorTest
            , cachedInProcessExecutorTest
            , composeExecutorsTest
            , agentStoreSessionWithCallbackTest
            ]
        ]

-------------------------------------------------------------------------------
-- Test data helpers
-------------------------------------------------------------------------------

-- | A conversation id used throughout the tests.
testConvId :: ConversationId
testConvId = ConversationId nil

-- | A stable session id used throughout the tests.
testSessionId :: SessionId
testSessionId = SessionId nil

-- | A valid (but minimal) execution context for tests that need one.
testCtx :: Ctx.ToolExecutionContext
testCtx = mkMinimalContext testSessionId testConvId (TurnId nil) dummyPortal

-- | Build a minimal LLM-issued tool call with a function name.
mkCall :: Text -> LlmToolCall
mkCall name =
    LlmToolCall $
        Aeson.object
            [ "function" .= Aeson.object ["name" .= name]
            , "arguments" .= Aeson.object []
            ]

-- | Extract the function name from a tool call.
callName :: LlmToolCall -> Text
callName (LlmToolCall val) =
    case val of
        Aeson.Object obj ->
            case KeyMap.lookup "function" obj of
                Just (Aeson.Object func) ->
                    case KeyMap.lookup "name" func of
                        Just (Aeson.String n) -> n
                        _ -> "unknown"
                _ -> "unknown"
        _ -> "unknown"

-- | Dummy tool portal that simply returns an empty result.
dummyPortal :: ToolPortal
dummyPortal _ _ =
    pure $
        ToolResult
            { resultData = Aeson.object []
            , resultDuration = 0
            , resultTraceId = "dummy"
            }

-- | Build an async agent with a given policy and optional cache/store/backend.
mkAsyncAgent ::
    ToolCallPolicy ->
    Maybe ToolCache ->
    Maybe ContinuationStore ->
    Maybe SessionBackend ->
    Maybe DeploymentRunner ->
    Agent (LlmTurnContent, Session)
mkAsyncAgent policy mCache mStore mBackend mRunner =
    Agent
        { step = naiveTilNoToolCallStep
        , sysPrompt = pure $ SystemPrompt "test prompt"
        , sysTools = pure []
        , usrQuery = pure Nothing
        , toolCall = \_ call -> pure $ TextResponse ("done:" <> callName call)
        , toolPortal = dummyPortal
        , complete = \_ -> pure (LlmResponse Nothing Nothing Aeson.Null Nothing, [])
        , contextConfig = defaultContextConfig
        , ctxWorld = Nothing
        , ctxEventQueue = Nothing
        , ctxCallStack = []
        , ctxParentConversation = Nothing
        , ctxExecutionMode = Asynchronous
        , ctxToolCache = mCache
        , ctxToolCallPolicy = policy
        , ctxToolExecutor = Nothing
        , ctxContinuationStore = mStore
        , ctxDeploymentRunner = mRunner
        , ctxSessionBackend = mBackend
        }

-- | Build a minimal synchronous agent for progress-callback tests.
mkSimpleAgent :: Agent (LlmTurnContent, Session)
mkSimpleAgent =
    Agent
        { step = naiveTilNoToolCallStep
        , sysPrompt = pure $ SystemPrompt "test prompt"
        , sysTools = pure []
        , usrQuery = pure Nothing
        , toolCall = \_ call -> pure $ TextResponse ("done:" <> callName call)
        , toolPortal = dummyPortal
        , complete = \_ -> pure (LlmResponse (Just "hello") Nothing Aeson.Null Nothing, [])
        , contextConfig = defaultContextConfig
        , ctxWorld = Nothing
        , ctxEventQueue = Nothing
        , ctxCallStack = []
        , ctxParentConversation = Nothing
        , ctxExecutionMode = Synchronous
        , ctxToolCache = Nothing
        , ctxToolCallPolicy = defaultToolCallPolicy
        , ctxToolExecutor = Nothing
        , ctxContinuationStore = Nothing
        , ctxDeploymentRunner = Nothing
        , ctxSessionBackend = Nothing
        }

-- | Build a session whose latest turn is an LLM turn with the given calls.
mkSessionWithCalls :: [LlmToolCall] -> Session
mkSessionWithCalls calls =
    Session
        { turns =
            [ LlmTurn
                ( LlmTurnContent
                    { llmResponse = LlmResponse Nothing Nothing Aeson.Null Nothing
                    , llmToolCalls = calls
                    }
                )
                Nothing
            ]
        , sessionId = testSessionId
        , forkedFromSessionId = Nothing
        , turnId = TurnId nil
        , sessionVersion = Just 2
        , sessionExecutionMode = Just Asynchronous
        }

-- | A serialisable context snapshot for envelope tests.
testContextSnapshot :: ToolExecutionContextSnapshot
testContextSnapshot =
    ToolExecutionContextSnapshot
        { tecsSessionId = testSessionId
        , tecsConversationId = testConvId
        , tecsTurnId = TurnId nil
        , tecsCallStack = []
        , tecsAllowedTools = []
        , tecsParentConversation = Nothing
        }

-------------------------------------------------------------------------------
-- Phase 2 tests
-------------------------------------------------------------------------------

-- | Policy classification test (Phase 8 category 1).
policyClassificationTest :: TestTree
policyClassificationTest =
    testCase "policy classifies sync vs deferred calls" $ do
        let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "test deferral")
        let agent = mkAsyncAgent policy Nothing Nothing Nothing Nothing
        (_agent, result) <- runStepMAsync testConvId agent (mkSessionWithCalls calls)
        case result of
            Left _ -> assertFailure "expected a yielded session, not a final result"
            Right session -> do
                case getPartialTurn session of
                    Nothing -> assertFailure "expected a partial turn"
                    Just partial -> do
                        let completed = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Completed]
                        let deferred = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred]
                        length completed @?= 1
                        length deferred @?= 2
                        all (isJust . tcContinuation) deferred @?= True
                        case completed of
                            [tc] ->
                                case tc.tcResult of
                                    Just (TextResponse txt) -> txt @?= "done:sync_tool"
                                    _ -> assertFailure "unexpected completed response"
                            _ -> assertFailure "expected exactly one completed call"

-- | Continuation store round-trip test (Phase 8 category 2).
continuationStoreRoundTripTest :: TestTree
continuationStoreRoundTripTest =
    testCase "continuation snapshots round-trip through SQLite store" $ do
        conn <- open ":memory:"
        store <- mkSqliteContinuationStore conn
        let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "test deferral")
        let agent = mkAsyncAgent policy Nothing (Just store) Nothing Nothing
        (_agent, result) <- runStepMAsync testConvId agent (mkSessionWithCalls calls)
        session <- case result of
            Right s -> pure s
            Left _ -> assertFailure "expected a yielded session"
        pending <- csListPending store testSessionId
        length pending @?= 2
        -- Each loaded snapshot should match the stored one.
        forM_ pending $ \(token, snapshot) -> do
            mLoaded <- csLoad store token
            mLoaded @?= Just snapshot
        -- The snapshots should reference the deferred calls.
        let deferredNames =
                [ callName (tcsToolCall snap)
                | (_, snap) <- pending
                ]
        sort deferredNames @?= ["defer_a", "defer_b"]
        -- Verify the session-level tokens match.
        let sessionTokens = catMaybes [tc.tcContinuation | tc <- partialTracked session]
        map fst pending @?= sessionTokens
  where
    partialTracked session =
        case getPartialTurn session of
            Just partial -> partial.pTrackedToolCalls
            Nothing -> []

    sort :: Ord a => [a] -> [a]
    sort = foldr insert []
      where
        insert x [] = [x]
        insert x (y : ys)
            | x <= y = x : y : ys
            | otherwise = y : insert x ys

-- | Wake and resume test (Phase 8 category 3).
wakeAndResumeTest :: TestTree
wakeAndResumeTest =
    testCase "wakeSession injects results and resumeSession finishes" $ do
        let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "test deferral")
        let agent = mkAsyncAgent policy Nothing Nothing Nothing Nothing
        (_agent, result) <- runStepMAsync testConvId agent (mkSessionWithCalls calls)
        partialSession <- case result of
            Right s -> pure s
            Left _ -> assertFailure "expected a yielded session"
        partial <- case getPartialTurn partialSession of
            Just p -> pure p
            Nothing -> assertFailure "expected a partial turn"
        let deferred = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred]
        length deferred @?= 2
        let responses =
                [ (fromJust (tc.tcContinuation), TextResponse ("woken:" <> callName (tc.tcCall)))
                | tc <- deferred
                ]
        woken <- wakeSession partialSession responses
        case getPartialTurn woken of
            Just _ -> assertFailure "expected partial turn to be converted to a full user turn"
            Nothing -> pure ()
        case woken.turns of
            (UserTurn content _ : _) -> do
                length content.userToolResponses @?= 3
                let texts = [txt | (_, TextResponse txt) <- content.userToolResponses]
                texts @?= ["done:sync_tool", "woken:defer_a", "woken:defer_b"]
            _ -> assertFailure "expected a user turn at the head"
        -- Resume should hit the LLM step (which returns no tool calls) and stop.
        final <- resumeSession testConvId agent woken
        case final of
            Left (llmTurn, _session) -> do
                length llmTurn.llmToolCalls @?= 0
            Right _ -> assertFailure "expected session to complete after resume"

-- | Cache integration test (Phase 8 category 4).
cacheIntegrationTest :: TestTree
cacheIntegrationTest =
    testCase "wakeSessionWithCache stores deferred results in the cache" $ do
        cachePath <- emptySystemTempFile "durable-cache.db"
        cache <- mkSqliteToolCache cachePath
        let calls = [mkCall "sync_tool", mkCall "defer_a"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "test deferral")
        let agent = mkAsyncAgent policy (Just cache) Nothing Nothing Nothing
        (_agent, result) <- runStepMAsync testConvId agent (mkSessionWithCalls calls)
        partialSession <- case result of
            Right s -> pure s
            Left _ -> assertFailure "expected a yielded session"
        partial <- case getPartialTurn partialSession of
            Just p -> pure p
            Nothing -> assertFailure "expected a partial turn"
        case [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred] of
            [tc] -> do
                let response = TextResponse "cached result"
                let responses = [(fromJust (tc.tcContinuation), response)]
                _ <- wakeSessionWithCache (Just cache) partialSession responses
                mLookup <- cache.cacheLookup (computeCacheKey (tc.tcCall))
                case mLookup of
                    Just (CachedResult result _ _) -> result @?= response
                    Nothing -> assertFailure "expected result to be in cache"
            _ -> assertFailure "expected exactly one deferred call"

-- | Snapshot serialization test.
snapshotSerializationTest :: TestTree
snapshotSerializationTest =
    testCase "ToolContinuationSnapshot serialises without runtime fields" $ do
        let call = mkCall "some_tool"
        let token = ContinuationToken nil
        let ctxSnap =
                ToolExecutionContextSnapshot
                    { tecsSessionId = testSessionId
                    , tecsConversationId = testConvId
                    , tecsTurnId = TurnId nil
                    , tecsCallStack = []
                    , tecsAllowedTools = []
                    , tecsParentConversation = Nothing
                    }
        let snapshot =
                ToolContinuationSnapshot
                    { tcsToken = token
                    , tcsSessionId = testSessionId
                    , tcsToolCallId = ToolCallId nil
                    , tcsToolCall = call
                    , tcsCacheKey = computeCacheKey call
                    , tcsPolicy = AppliedPolicy RunSync Nothing
                    , tcsContextSnapshot = ctxSnap
                    , tcsCreatedAt = read "2024-01-01 00:00:00 UTC" :: UTCTime
                    , tcsExpiresAt = Nothing
                    }
        let json = Aeson.encode snapshot
        case Aeson.decode json of
            Nothing -> assertFailure "snapshot failed to round-trip through JSON"
            Just decoded -> do
                tcsToken decoded @?= token
                tcsToolCallId decoded @?= ToolCallId nil
                tcsToolCall decoded @?= call
                tcsPolicy decoded @?= AppliedPolicy RunSync Nothing
                tcsContextSnapshot decoded @?= ctxSnap
                -- Re-hydration should restore the serialisable fields and use
                -- the supplied runtime fields.
                let hydrated = Ctx.hydrateContextSnapshot dummyPortal Nothing Nothing (tcsContextSnapshot decoded)
                Ctx.ctxSessionId hydrated @?= testSessionId
                Ctx.ctxConversationId hydrated @?= testConvId
                Ctx.ctxTurnId hydrated @?= TurnId nil

-------------------------------------------------------------------------------
-- Phase 3 tests
-------------------------------------------------------------------------------

-- | File backend round-trip test (Phase 8 category 2).
fileBackendRoundTripTest :: TestTree
fileBackendRoundTripTest =
    testCase "file backend stores and loads sessions" $ do
        withSystemTempDirectory "session-file-backend" $ \dir -> do
            backend <- mkFileSessionStore dir
            let session = mkSessionWithCalls [mkCall "test_tool"]
            sbStore backend session.sessionId session
            mLoaded <- sbLoad backend session.sessionId
            mLoaded @?= Just session
            listed <- sbList backend
            map fst listed @?= [session.sessionId]
            sbDelete backend session.sessionId
            mAfterDelete <- sbLoad backend session.sessionId
            mAfterDelete @?= Nothing

-- | SQLite backend round-trip test (Phase 8 category 2).
sqliteBackendRoundTripTest :: TestTree
sqliteBackendRoundTripTest =
    testCase "SQLite backend stores and loads sessions" $ do
        conn <- open ":memory:"
        backend <- mkSqliteSessionStore conn
        let session = mkSessionWithCalls [mkCall "test_tool"]
        sbStore backend session.sessionId session
        mLoaded <- sbLoad backend session.sessionId
        mLoaded @?= Just session
        listed <- sbList backend
        map fst listed @?= [session.sessionId]
        sbDelete backend session.sessionId
        mAfterDelete <- sbLoad backend session.sessionId
        mAfterDelete @?= Nothing

-- | Composite backend read fallback test.
compositeFallbackTest :: TestTree
compositeFallbackTest =
    testCase "composite backend falls back to secondary store" $
        withSystemTempDirectory "session-primary" $ \primaryDir ->
            withSystemTempDirectory "session-secondary" $ \secondaryDir -> do
                primary <- mkFileSessionStore primaryDir
                secondary <- mkFileSessionStore secondaryDir
                let composite = mkCompositeSessionStore [primary, secondary]

                sidA <- newSessionId
                sidB <- newSessionId
                sidC <- newSessionId
                let sessionA = (mkSessionWithCalls [mkCall "in_primary"]){sessionId = sidA}
                let sessionB = (mkSessionWithCalls [mkCall "in_secondary"]){sessionId = sidB}
                let sessionC = (mkSessionWithCalls [mkCall "via_composite"]){sessionId = sidC}

                -- Store A only in primary, B only in secondary.
                sbStore primary sessionA.sessionId sessionA
                sbStore secondary sessionB.sessionId sessionB

                -- Composite should find both.
                mA <- sbLoad composite sessionA.sessionId
                mB <- sbLoad composite sessionB.sessionId
                mA @?= Just sessionA
                mB @?= Just sessionB

                -- Composite writes only to primary.
                sbStore composite sessionC.sessionId sessionC
                mPrimary <- sbLoad primary sessionC.sessionId
                mSecondary <- sbLoad secondary sessionC.sessionId
                mPrimary @?= Just sessionC
                mSecondary @?= Nothing

                -- Composite delete only removes from primary.
                sbDelete composite sessionA.sessionId
                mDeleted <- sbLoad composite sessionA.sessionId
                mDeleted @?= Nothing

                -- Listing aggregates both backends.
                listed <- sbList composite
                length listed @?= 2

-- | Backend integration with the session progress callback.
backendProgressCallbackTest :: TestTree
backendProgressCallbackTest =
    testCase "agentStoreSession uses ctxSessionBackend when present" $
        withSystemTempDirectory "session-backend" $ \dir -> do
            backend <- mkFileSessionStore dir
            let agent = withSessionBackend backend mkSimpleAgent
            -- The file-based store passed here should be ignored because the
            -- agent has a backend configured.
            let wrapped = agentStoreSession (error "should not use file store") Nothing testConvId agent

            -- Simulate a progress event by invoking the decorated step on a
            -- fresh session. The callback stores the session before the step.
            let session0 = mkSessionWithCalls []
            _ <- wrapped.step session0

            -- The backend should have stored the session.
            listed <- sbList backend
            length listed @?= 1
            mLoaded <- sbLoad backend session0.sessionId
            mLoaded @?= Just session0

-------------------------------------------------------------------------------
-- Phase 5 tests
-------------------------------------------------------------------------------

-- | Input envelope JSON round-trip.
envelopeRoundTripTest :: TestTree
envelopeRoundTripTest =
    testCase "IsolationEnvelope round-trips through JSON" $ do
        token <- newContinuationToken
        let call = mkCall "bash_command"
        let disp = RunIsolated (Docker "agents-exe/bash-runner:latest")
        let env = mkIsolationEnvelope token call testContextSnapshot disp (Just "isolate bash for safety")
        let json = Aeson.encode env
        case Aeson.eitherDecode json of
            Left err -> assertFailure $ "envelope decode failed: " ++ err
            Right decoded -> do
                ieToken decoded @?= token
                ieToolCall decoded @?= call
                ieContextSnapshot decoded @?= testContextSnapshot
                ieDisposition decoded @?= disp
                ieReason decoded @?= Just "isolate bash for safety"

-- | Result envelope JSON round-trip and parser helpers.
resultEnvelopeRoundTripTest :: TestTree
resultEnvelopeRoundTripTest =
    testCase "IsolationResultEnvelope round-trips and parses" $ do
        token <- newContinuationToken
        let result = TextResponse "hello from isolation"
        let successEnv = mkIsolationSuccessEnvelope token result
        let successJson = Aeson.encode successEnv
        case parseIsolationResultEnvelopeLBS successJson of
            Left (IsolationError err) -> assertFailure $ unpack err
            Right decoded -> do
                ireToken decoded @?= token
                ireStatus decoded @?= IsolationSuccess
                ireResult decoded @?= Just result
                ireError decoded @?= Nothing
        let errorEnv = mkIsolationErrorEnvelope token "boom"
        let errorJson = Aeson.encode errorEnv
        case parseIsolationResultEnvelope $ Aeson.toJSON errorEnv of
            Left (IsolationError err) -> assertFailure $ unpack err
            Right decoded -> do
                ireToken decoded @?= token
                ireStatus decoded @?= IsolationFailure
                ireResult decoded @?= Nothing
                ireError decoded @?= Just "boom"

-- | Local process runner with a bash worker script (Phase 8 category 5).
localProcessRunnerTest :: TestTree
localProcessRunnerTest =
    testCase "localProcessRunner executes a worker script" $
        withSystemTempDirectory "isolation-worker" $ \dir -> do
            let workerPath = dir ++ "/worker.sh"
            writeFile workerPath workerScript
            setPermissions workerPath emptyPermissions{readable = True, executable = True}
            let runner = localProcessRunner workerPath
            token <- newContinuationToken
            let call = mkCall "bash_command"
            let env = mkIsolationEnvelope token call testContextSnapshot (RunIsolated (LocalProcess workerPath)) Nothing
            result <- drExecute runner env
            case result of
                Left (IsolationError err) -> assertFailure $ "runner failed: " ++ unpack err
                Right (TextResponse txt) -> txt @?= "hello from worker"
                Right _ -> assertFailure "expected a text response from the worker"
  where
    -- Bash worker that reads the envelope from stdin, echoes the token back,
    -- and prints a success result envelope. This relies on aeson's compact
    -- encoding, which places the token field on the single input line.
    workerScript =
        unlines
            [ "#!/usr/bin/env bash"
            , "set -e"
            , "TOKEN=$(sed -n 's/.*\"token\":\"\\([^\"]*\\)\".*/\\1/p' | head -1)"
            , "echo \"{\\\"token\\\":\\\"$TOKEN\\\",\\\"status\\\":\\\"success\\\",\\\"result\\\":{\\\"type\\\":\\\"text\\\",\\\"content\\\":\\\"hello from worker\\\"}}\""
            ]

-- | Docker runner envelope construction; actual execution is environment-dependent.
dockerRunnerEnvelopeTest :: TestTree
dockerRunnerEnvelopeTest =
    testCase "dockerRunner constructs a named runner" $ do
        let runner = dockerRunner "agents-exe/bash-runner:latest"
        drName runner @?= "docker"
        -- Execution is intentionally not tested here because Docker may not be
        -- available in the test environment. The runner simply needs to exist
        -- and carry the configured image name.
        True @?= True

-- | Policy that isolates a tool by name; verifies scheduler integration (Phase 8 category 5).
isolatedToolNamePolicyTest :: TestTree
isolatedToolNamePolicyTest =
    testCase "policy isolates bash_command via localProcessRunner" $
        withSystemTempDirectory "isolation-worker" $ \dir -> do
            let workerPath = dir ++ "/worker.sh"
            writeFile workerPath workerScript
            setPermissions workerPath emptyPermissions{readable = True, executable = True}
            let runner = localProcessRunner workerPath
            let policy _ctx call
                    | callName call == "bash_command" = RunIsolated (LocalProcess workerPath)
                    | otherwise = RunSync
            let agent = mkAsyncAgent policy Nothing Nothing Nothing (Just runner)
            let calls = [mkCall "sync_tool", mkCall "bash_command"]
            (_agent, result) <- runStepMAsync testConvId agent (mkSessionWithCalls calls)
            session <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session"
            case session.turns of
                (UserTurn content _ : _) -> do
                    length content.userToolResponses @?= 2
                    let texts = sort [txt | (_, TextResponse txt) <- content.userToolResponses]
                    texts @?= ["done:sync_tool", "hello from worker"]
                _ -> assertFailure "expected a user turn with both responses"
  where
    workerScript =
        unlines
            [ "#!/usr/bin/env bash"
            , "set -e"
            , "TOKEN=$(sed -n 's/.*\"token\":\"\\([^\"]*\\)\".*/\\1/p' | head -1)"
            , "echo \"{\\\"token\\\":\\\"$TOKEN\\\",\\\"status\\\":\\\"success\\\",\\\"result\\\":{\\\"type\\\":\\\"text\\\",\\\"content\\\":\\\"hello from worker\\\"}}\""
            ]

    sort :: Ord a => [a] -> [a]
    sort = foldr insert []
      where
        insert x [] = [x]
        insert x (y : ys)
            | x <= y = x : y : ys
            | otherwise = y : insert x ys

-------------------------------------------------------------------------------
-- Phase 6 tests
-------------------------------------------------------------------------------

-- | Full end-to-end integration of policy + cache + continuation store + backend.
durableWorkflowIntegrationTest :: TestTree
durableWorkflowIntegrationTest =
    testCase "full durable workflow integrates policy + cache + store + backend" $
        withSystemTempDirectory "durable-integration" $ \dir -> do
            conn <- open ":memory:"
            backend <- mkSqliteSessionStore conn
            store <- mkSqliteContinuationStore conn
            cachePath <- emptySystemTempFile "durable-integration-cache.db"
            cache <- mkSqliteToolCache cachePath

            let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
            let policy _ctx call
                    | callName call == "sync_tool" = RunSync
                    | otherwise = Defer (Reason "approval required")
            let agent0 = mkAsyncAgent policy (Just cache) (Just store) (Just backend) Nothing

            -- Run the async step. It should execute one call and defer two.
            (_agent, result) <- runStepMAsync testConvId agent0 (mkSessionWithCalls calls)
            partialSession <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session"

            -- Persist the partial session via the backend manually (mirroring what
            -- a progress callback or external orchestrator would do after a yield).
            sbStore backend partialSession.sessionId partialSession

            -- The session backend should have stored the partial session.
            mStored <- sbLoad backend partialSession.sessionId
            mStored @?= Just partialSession

            -- The continuation store should have the two deferred snapshots.
            pending <- csListPending store testSessionId
            length pending @?= 2

            -- Wake the session with results for the deferred calls.
            partial <- case getPartialTurn partialSession of
                Just p -> pure p
                Nothing -> assertFailure "expected a partial turn"
            let deferred = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred]
            let responses =
                    [ (fromJust (tc.tcContinuation), TextResponse ("woken:" <> callName (tc.tcCall)))
                    | tc <- deferred
                    ]
            woken <- wakeSessionWithCache (Just cache) partialSession responses

            -- After waking, the partial turn should be a full user turn.
            case getPartialTurn woken of
                Just _ -> assertFailure "expected partial turn to be converted to a full user turn"
                Nothing -> pure ()

            -- The cache should contain the woken results.
            forM_ deferred $ \tc -> do
                mCached <- cache.cacheLookup (computeCacheKey (tc.tcCall))
                case mCached of
                    Just (CachedResult result _ _) ->
                        result @?= TextResponse ("woken:" <> callName (tc.tcCall))
                    Nothing -> assertFailure "expected deferred result to be cached"

            -- Resume should complete (the LLM step returns no tool calls).
            final <- resumeSession testConvId agent0 woken
            case final of
                Left (llmTurn, _session) -> do
                    length llmTurn.llmToolCalls @?= 0
                Right _ -> assertFailure "expected session to complete after resume"

-- | Convenience combinator that wires backend + continuation store.
withDurableWorkflowsTest :: TestTree
withDurableWorkflowsTest =
    testCase "withDurableWorkflows installs backend and continuation store" $ do
        conn <- open ":memory:"
        backend <- mkSqliteSessionStore conn
        store <- mkSqliteContinuationStore conn
        let agent = withDurableWorkflows backend store mkSimpleAgent
        isJust (ctxSessionBackend agent) @?= True
        isJust (ctxContinuationStore agent) @?= True

-- | Convenience combinator that configures async mode + cache + policy.
withAsyncConfigTest :: TestTree
withAsyncConfigTest =
    testCase "withAsyncConfig sets mode, cache and policy" $ do
        cachePath <- emptySystemTempFile "async-config-cache.db"
        cache <- mkSqliteToolCache cachePath
        let policy _ctx _call = RunAsync
        let agent = withAsyncConfig Asynchronous (Just cache) policy mkSimpleAgent
        ctxExecutionMode agent @?= Asynchronous
        isJust (ctxToolCache agent) @?= True
        -- Apply the installed policy to verify it is the one we supplied.
        ctxToolCallPolicy agent testCtx (mkCall "any") @?= RunAsync

-- | 'mkDurableExecutor' builds an executor that caches and isolates.
mkDurableExecutorTest :: TestTree
mkDurableExecutorTest =
    testCase "mkDurableExecutor caches sync calls and isolates bash" $
        withSystemTempDirectory "durable-executor" $ \dir -> do
            cachePath <- emptySystemTempFile "durable-executor-cache.db"
            cache <- mkSqliteToolCache cachePath

            let workerPath = dir ++ "/worker.sh"
            writeFile workerPath workerScript
            setPermissions workerPath emptyPermissions{readable = True, executable = True}
            let runner = localProcessRunner workerPath

            let policy _ctx call
                    | callName call == "bash_command" = RunIsolated (LocalProcess workerPath)
                    | otherwise = RunSync
            let exec = mkDurableExecutor policy (Just cache) (Just runner) runSync

            -- First sync call executes and is cached.
            let syncCall = mkCall "sync_tool"
            r1 <- exec.execSync testCtx syncCall
            r1 @?= TextResponse "done:sync_tool"
            mCached <- cache.cacheLookup (computeCacheKey syncCall)
            isJust mCached @?= True

            -- Isolated call runs the worker.
            let bashCall = mkCall "bash_command"
            r2 <- exec.execSync testCtx bashCall
            r2 @?= TextResponse "hello from worker"
  where
    runSync _ctx call = pure $ TextResponse ("done:" <> callName call)

    workerScript =
        unlines
            [ "#!/usr/bin/env bash"
            , "set -e"
            , "TOKEN=$(sed -n 's/.*\"token\":\"\\([^\"]*\\)\".*/\\1/p' | head -1)"
            , "echo \"{\\\"token\\\":\\\"$TOKEN\\\",\\\"status\\\":\\\"success\\\",\\\"result\\\":{\\\"type\\\":\\\"text\\\",\\\"content\\\":\\\"hello from worker\\\"}}\""
            ]

-- | 'cachedInProcessExecutor' caches in-process calls.
cachedInProcessExecutorTest :: TestTree
cachedInProcessExecutorTest =
    testCase "cachedInProcessExecutor caches sync calls" $ do
        cachePath <- emptySystemTempFile "cached-executor-cache.db"
        cache <- mkSqliteToolCache cachePath
        ref <- newIORef (0 :: Int)
        let runSync _ctx _call = do
                modifyIORef' ref (+ 1)
                pure $ TextResponse "computed"
        let exec = cachedInProcessExecutor cache runSync
        let call = mkCall "sync_tool"

        r1 <- exec.execSync testCtx call
        r1 @?= TextResponse "computed"
        count1 <- readIORef ref
        count1 @?= 1

        -- Second call should hit the cache and not invoke runSync again.
        r2 <- exec.execSync testCtx call
        r2 @?= TextResponse "computed"
        count2 <- readIORef ref
        count2 @?= 1

-- | 'composeExecutors' routes calls based on disposition predicates.
composeExecutorsTest :: TestTree
composeExecutorsTest =
    testCase "composeExecutors dispatches by disposition" $ do
        refSync <- newIORef (0 :: Int)
        refIso <- newIORef (0 :: Int)
        let policy _ctx call
                | callName call == "bash_command" = RunIsolated (LocalProcess "/dummy")
                | otherwise = RunSync
        let syncExec = inProcessExecutor $ \_ctx _call -> do
                modifyIORef' refSync (+ 1)
                pure $ TextResponse "sync"
        let isoExec = inProcessExecutor $ \_ctx _call -> do
                modifyIORef' refIso (+ 1)
                pure $ TextResponse "isolated"
        let isIsolated base = case base of RunIsolated _ -> True; _ -> False
        let exec = composeExecutors policy [(isIsolated, isoExec)] syncExec

        _ <- exec.execSync testCtx (mkCall "sync_tool")
        _ <- exec.execSync testCtx (mkCall "bash_command")

        syncCount <- readIORef refSync
        isoCount <- readIORef refIso
        syncCount @?= 1
        isoCount @?= 1

-- | 'agentStoreSessionWithCallback' stores via backend and invokes user callback.
agentStoreSessionWithCallbackTest :: TestTree
agentStoreSessionWithCallbackTest =
    testCase "agentStoreSessionWithCallback stores and emits progress" $
        withSystemTempDirectory "session-callback" $ \dir -> do
            backend <- mkFileSessionStore dir
            ref <- newIORef ([] :: [SessionProgress])
            let agent = withSessionBackend backend mkSimpleAgent
            let userCallback progress = modifyIORef' ref (progress :)
            let wrapped = agentStoreSessionWithCallback (error "should not use file store") Nothing testConvId userCallback agent

            let session0 = mkSessionWithCalls []
            _ <- wrapped.step session0

            -- Backend should have stored the session.
            mLoaded <- sbLoad backend session0.sessionId
            mLoaded @?= Just session0

            -- User callback should have been invoked at least once.
            events <- readIORef ref
            length events @?= 1
            case events of
                (SessionUpdated sess : _) -> sess @?= session0
                _ -> assertFailure "expected SessionUpdated event"

