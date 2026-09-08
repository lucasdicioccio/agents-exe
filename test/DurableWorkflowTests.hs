{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for Phase 2 of the durable-workflows plan.

Covers:

* Tool-call policy classification in the async scheduler.
* Continuation snapshot serialisation and SQLite round-tripping.
* 'wakeSession' injecting external results and converting partial turns.
* Cache updates during wake.
* 'resumeSession' continuing from a partial or completed user turn.
-}
module DurableWorkflowTests where

import Control.Monad (forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (nil)
import Database.SQLite.Simple (open)
import System.IO.Temp (emptySystemTempFile)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Async (
    ContinuationStore,
    ToolContinuationSnapshot (..),
    csListPending,
    csLoad,
    mkSqliteContinuationStore,
 )
import System.Agents.Session.Base
import System.Agents.Session.Step (getPartialTurn, naiveTilNoToolCallStep, runStepMAsync)
import System.Agents.Session.Step (getPartialTurn, naiveTilNoToolCallStep, runStepMAsync)
import System.Agents.Session.Types
import System.Agents.Session.Wake (resumeSession, wakeSession, wakeSessionWithCache)
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
        "Durable Workflows (Phase 2)"
        [ policyClassificationTest
        , continuationStoreRoundTripTest
        , wakeAndResumeTest
        , cacheIntegrationTest
        , snapshotSerializationTest
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

-- | Build an async agent with a given policy and optional cache/store.
mkAsyncAgent ::
    ToolCallPolicy ->
    Maybe ToolCache ->
    Maybe ContinuationStore ->
    Agent (LlmTurnContent, Session)
mkAsyncAgent policy mCache mStore =
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
        , ctxDeploymentRunner = Nothing
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

-------------------------------------------------------------------------------
-- Policy classification test
-------------------------------------------------------------------------------

policyClassificationTest :: TestTree
policyClassificationTest =
    testCase "policy classifies sync vs deferred calls" $ do
        let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "test deferral")
        let agent = mkAsyncAgent policy Nothing Nothing
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

-------------------------------------------------------------------------------
-- Continuation store round-trip test
-------------------------------------------------------------------------------

continuationStoreRoundTripTest :: TestTree
continuationStoreRoundTripTest =
    testCase "continuation snapshots round-trip through SQLite store" $ do
        conn <- open ":memory:"
        store <- mkSqliteContinuationStore conn
        let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "test deferral")
        let agent = mkAsyncAgent policy Nothing (Just store)
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

-------------------------------------------------------------------------------
-- Wake and resume test
-------------------------------------------------------------------------------

wakeAndResumeTest :: TestTree
wakeAndResumeTest =
    testCase "wakeSession injects results and resumeSession finishes" $ do
        let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "test deferral")
        let agent = mkAsyncAgent policy Nothing Nothing
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

-------------------------------------------------------------------------------
-- Cache integration test
-------------------------------------------------------------------------------

cacheIntegrationTest :: TestTree
cacheIntegrationTest =
    testCase "wakeSessionWithCache stores deferred results in the cache" $ do
        cachePath <- emptySystemTempFile "durable-cache.db"
        cache <- mkSqliteToolCache cachePath
        let calls = [mkCall "sync_tool", mkCall "defer_a"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "test deferral")
        let agent = mkAsyncAgent policy (Just cache) Nothing
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

-------------------------------------------------------------------------------
-- Snapshot serialization test
-------------------------------------------------------------------------------

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

