{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Phase 8 durable-workflow tests.

This module covers the final testing-strategy phase from
`todos/durable-workflows.md`:

* Determinism: the same yielded session state resumed twice produces the
  same semantic outcome.
* Wake idempotence: calling 'wakeSession' a second time with the same
  token/result pair leaves the session unchanged.
* Edge cases:
    * Waking a session that has no partial turn is a no-op.
    * Completing the last deferred call converts a 'PartialUserTurn' into a
      full 'UserTurn'.
    * Composite backend listing deduplicates entries that exist in more
      than one backend.
-}
module DurableWorkflowDeterminismTests where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.UUID (nil)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Base
import System.Agents.Session.Step (getPartialTurn, naiveTilNoToolCallStep, runStepMAsync)
import System.Agents.Session.Types
import System.Agents.Session.Wake (resumeSession, wakeSession)
import System.Agents.SessionStore (
    SessionBackend (..),
    mkCompositeSessionStore,
    mkFileSessionStore,
 )
import System.Agents.Tools.Context (ToolPortal, ToolResult (..))

-- | A stable conversation id used throughout the tests.
testConvId :: ConversationId
testConvId = ConversationId nil

-- | A stable session id used throughout the tests.
testSessionId :: SessionId
testSessionId = SessionId nil

-- | Dummy tool portal that simply returns an empty result.
dummyPortal :: ToolPortal
dummyPortal _ _ =
    pure $
        ToolResult
            { resultData = Aeson.object []
            , resultDuration = 0
            , resultTraceId = "dummy"
            }

-- | Build a minimal LLM-issued tool call with a function name.
mkCall :: Text -> LlmToolCall
mkCall name =
    LlmToolCall $
        Aeson.object
            [ "function" Aeson..= Aeson.object ["name" Aeson..= name]
            , "arguments" Aeson..= Aeson.object []
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

-- | Build an async agent with a given policy.
mkAsyncAgent :: ToolCallPolicy -> Agent (LlmTurnContent, Session)
mkAsyncAgent policy =
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
        , ctxToolCache = Nothing
        , ctxToolCallPolicy = policy
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

-- | Test suite entry point.
tests :: TestTree
tests =
    testGroup
        "Durable Workflow Phase 8"
        [ determinismTest
        , wakeIdempotenceTest
        , wakeNoPartialTurnTest
        , completeLastDeferredTest
        , compositeBackendDeduplicationTest
        ]

-------------------------------------------------------------------------------
-- Determinism
-------------------------------------------------------------------------------

{- | Determinism test.

Runs the same yielded+woken session through 'resumeSession' twice and
asserts that the semantic outcome is identical.  Generated identifiers
such as 'turnId' are ignored because each resume appends a fresh turn.
-}
determinismTest :: TestTree
determinismTest =
    testCase "resuming the same session twice yields the same outcome" $ do
        let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "approval")
        let agent = mkAsyncAgent policy

        -- Yield a partial session with one sync call completed and two deferred.
        (_agent, result) <- runStepMAsync testConvId agent (mkSessionWithCalls calls)
        partialSession <- case result of
            Right s -> pure s
            Left _ -> assertFailure "expected a yielded session"

        partial <- case getPartialTurn partialSession of
            Just p -> pure p
            Nothing -> assertFailure "expected a partial turn"
        let deferred = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred]
        length deferred @?= 2

        -- Wake both deferred calls so the partial turn becomes a full user turn.
        let responses =
                [ (fromJust (tc.tcContinuation), TextResponse ("woken:" <> callName (tc.tcCall)))
                | tc <- deferred
                ]
        woken <- wakeSession partialSession responses
        case getPartialTurn woken of
            Just _ -> assertFailure "expected partial turn to be converted to a full user turn"
            Nothing -> pure ()

        -- Resume twice from the identical woken state.
        outcome1 <- resumeSession testConvId agent woken
        outcome2 <- resumeSession testConvId agent woken

        assertEquivalentOutcomes outcome1 outcome2

-- | Compare two resume outcomes for semantic equality, ignoring generated IDs.
assertEquivalentOutcomes ::
    Either (LlmTurnContent, Session) Session ->
    Either (LlmTurnContent, Session) Session ->
    Assertion
assertEquivalentOutcomes (Left (llm1, sess1)) (Left (llm2, sess2)) = do
    -- The LLM turn content must be identical.
    llm1 @?= llm2
    -- Both sessions should have gained exactly one new LLM turn.
    length sess1.turns @?= length sess2.turns
    -- All turns except the newest (head) are byte-for-byte equal because
    -- they came from the same woken session.
    drop 1 sess1.turns @?= drop 1 sess2.turns
    -- The newest turn must be an LLM turn with the same content.
    case (sess1.turns, sess2.turns) of
        ((LlmTurn c1 _ : _), (LlmTurn c2 _ : _)) -> c1 @?= c2
        _ -> assertFailure "expected the newest turn to be an LLM turn"
assertEquivalentOutcomes (Right _) (Right _) =
    assertFailure "expected sessions to complete, not yield again"
assertEquivalentOutcomes _ _ =
    assertFailure "outcomes differ: one completed and one yielded"

-------------------------------------------------------------------------------
-- Wake idempotence
-------------------------------------------------------------------------------

{- | Wake idempotence test.

Waking a session with the same token/result pair a second time should be a
no-op because 'wakeSession' only acts on calls that are still 'Deferred'.
-}
wakeIdempotenceTest :: TestTree
wakeIdempotenceTest =
    testCase "wakeSession is idempotent for the same token/result" $ do
        let calls = [mkCall "sync_tool", mkCall "defer_a"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "approval")
        let agent = mkAsyncAgent policy

        (_agent, result) <- runStepMAsync testConvId agent (mkSessionWithCalls calls)
        partialSession <- case result of
            Right s -> pure s
            Left _ -> assertFailure "expected a yielded session"

        partial <- case getPartialTurn partialSession of
            Just p -> pure p
            Nothing -> assertFailure "expected a partial turn"
        case [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred] of
            [tc] -> do
                let response = TextResponse "external result"
                let token = fromJust (tc.tcContinuation)
                woken1 <- wakeSession partialSession [(token, response)]
                woken2 <- wakeSession woken1 [(token, response)]
                woken1 @?= woken2
            _ -> assertFailure "expected exactly one deferred call"

-------------------------------------------------------------------------------
-- Edge cases
-------------------------------------------------------------------------------

{- | Waking a session with no partial turn is a no-op.

This protects callers that may invoke 'wakeSession' on an already-completed
session or on a session whose token has already been applied.
-}
wakeNoPartialTurnTest :: TestTree
wakeNoPartialTurnTest =
    testCase "wakeSession on a session without a partial turn is a no-op" $ do
        let session = mkSessionWithCalls []
        -- The session has an LLM turn with no calls, so there is no partial turn.
        case getPartialTurn session of
            Just _ -> assertFailure "test setup produced an unexpected partial turn"
            Nothing -> pure ()
        token <- newContinuationToken
        woke <- wakeSession session [(token, TextResponse "ignored")]
        woke @?= session

{- | Completing the last deferred call converts a partial turn to a user turn.

This is the smallest case of the wake/resume transition: a single deferred
call that receives its result should immediately become a full 'UserTurn'.
-}
completeLastDeferredTest :: TestTree
completeLastDeferredTest =
    testCase "completing the last deferred call produces a full UserTurn" $ do
        let calls = [mkCall "sync_tool", mkCall "defer_a"]
        let policy _ctx call
                | callName call == "sync_tool" = RunSync
                | otherwise = Defer (Reason "approval")
        let agent = mkAsyncAgent policy

        (_agent, result) <- runStepMAsync testConvId agent (mkSessionWithCalls calls)
        partialSession <- case result of
            Right s -> pure s
            Left _ -> assertFailure "expected a yielded session"

        partial <- case getPartialTurn partialSession of
            Just p -> pure p
            Nothing -> assertFailure "expected a partial turn"
        let deferred = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred]
        case deferred of
            [tc] -> do
                let response = TextResponse "final result"
                let token = fromJust (tc.tcContinuation)
                woken <- wakeSession partialSession [(token, response)]

                case getPartialTurn woken of
                    Just _ -> assertFailure "expected partial turn to become a full user turn"
                    Nothing -> pure ()

                case woken.turns of
                    (UserTurn content _ : _) -> do
                        length content.userToolResponses @?= 2
                        let texts = [txt | (_, TextResponse txt) <- content.userToolResponses]
                        texts @?= ["done:sync_tool", "final result"]
                    _ -> assertFailure "expected a user turn at the head"
            _ -> assertFailure "expected exactly one deferred call"

{- | Composite backend listing deduplicates entries from multiple backends.

If the same session id exists in both the primary and a secondary backend,
'sbList' should return a single entry.  Reads still fall back correctly.
-}
compositeBackendDeduplicationTest :: TestTree
compositeBackendDeduplicationTest =
    testCase "composite backend listing deduplicates shared entries" $
        withSystemTempDirectory "session-primary" $ \primaryDir ->
            withSystemTempDirectory "session-secondary" $ \secondaryDir -> do
                primary <- mkFileSessionStore primaryDir
                secondary <- mkFileSessionStore secondaryDir
                let composite = mkCompositeSessionStore [primary, secondary]

                let session = mkSessionWithCalls [mkCall "shared_tool"]
                -- Store the same session in both backends.
                sbStore primary session.sessionId session
                sbStore secondary session.sessionId session

                -- Listing should deduplicate, returning one entry.
                listed <- sbList composite
                length listed @?= 1
                map fst listed @?= [session.sessionId]

                -- Load should return the session from the primary backend.
                mLoaded <- sbLoad composite session.sessionId
                mLoaded @?= Just session

