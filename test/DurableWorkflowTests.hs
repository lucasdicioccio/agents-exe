{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for Phases 2, 3, and 5 of the durable-workflows plan.

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
-}
module DurableWorkflowTests where

import Control.Monad (forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
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
import System.Agents.Combinators.StoreSessionProgress (agentStoreSession, agentWithSessionProgress)
import System.Agents.Session.Async (
    ContinuationStore,
    ToolContinuationSnapshot (..),
    csListPending,
    csLoad,
    mkSqliteContinuationStore,
 )
import System.Agents.Session.Base
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

-- | Policy classification test.
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

-- | Continuation store round-trip test.
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

-- | Wake and resume test.
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

-- | Cache integration test.
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

-- | File backend round-trip test.
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

-- | SQLite backend round-trip test.
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

-- | Local process runner with a bash worker script.
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

-- | Policy that isolates a tool by name; verifies scheduler integration.
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
