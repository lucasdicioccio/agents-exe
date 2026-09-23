{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Unit tests for Phase 7 durable-workflow CLI helpers.

These tests exercise the pure parsing/formatting/extraction helpers in
"System.Agents.CLI.SessionDurable" plus a small integration test for the
pending/complete workflow using a temporary file session store.
-}
module SessionDurableTests where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (void)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (nil)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (ConversationId (..))
import qualified System.Agents.Base as Base
import System.Agents.CLI.SessionDurable (
    applyAgentDurableConfig,
    buildToolCallPolicy,
    extractDeferredCalls,
    extractIsolatedCalls,
    formatContinuationToken,
    parseContinuationToken,
    parseResultFile,
 )
import System.Agents.Session.Base
import System.Agents.Session.Step (applyContinuationMail, getPartialTurn, naiveTilNoToolCallStep, receiveMailForTurn, runStepM, runStepMAsync)
import System.Agents.Session.Types
import System.Agents.Session.Wake (wakeSession)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.Tools.Context (ToolPortal, ToolResult (..), mkMinimalContext)

-- | A stable session id for tests.
testSessionId :: SessionId
testSessionId = SessionId nil

-- | A stable conversation id for tests.
testConvId :: ConversationId
testConvId = ConversationId nil

-- | Dummy tool portal for constructing contexts.
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
        , ctxEmit = Nothing
        , ctxCallStack = []
        , ctxParentConversation = Nothing
        , ctxExecutionMode = Asynchronous
        , ctxAsyncYieldStrategy = YieldWhenAllDone
        , ctxMaxConcurrency = Nothing
        , ctxAsyncCallTimeout = Nothing
        , ctxToolCache = Nothing
        , ctxToolCallPolicy = policy
        , ctxToolExecutor = Nothing
        , ctxContinuationStore = Nothing
        , ctxDeploymentRunner = Nothing
        , ctxSessionBackend = Nothing
        , ctxAsyncEngine = Nothing
        , ctxParams = mempty
        , ctxInheritedBindings = []
        , ctxMailbox = Nothing
        , ctxMailRouter = Nothing
        , ctxSpawnSession = Nothing
        , ctxInterruptCompletions = False
        , ctxMailInToolResult = False
        }

-- | Like 'mkAsyncAgent', in synchronous mode, for R1's mail folding
-- (@todos/os-as-standalone-server.md@ Design §5), which only applies to
-- 'runStepMSync'\'s attached (sync) tool calls.
mkSyncAgent :: ToolCallPolicy -> Agent (LlmTurnContent, Session)
mkSyncAgent policy = (mkAsyncAgent policy){ctxExecutionMode = Synchronous}

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
        , mailCursor = 0
        }

-- | A brand-new session with no turns at all.
mkFreshSession :: Session
mkFreshSession =
    Session
        { turns = []
        , sessionId = testSessionId
        , forkedFromSessionId = Nothing
        , turnId = TurnId nil
        , sessionVersion = Just 2
        , sessionExecutionMode = Just Asynchronous
        , mailCursor = 0
        }

-- | An 'Outgoing' 'UserMessage' envelope, for tests that seed a mailbox.
userMessageOutgoing :: Text -> Outgoing
userMessageOutgoing txt =
    Outgoing
        { outId = Nothing
        , outFrom = FromUser Nothing
        , outPriority = Normal
        , outHops = 0
        , outBody = UserMessage (UserQuery txt [])
        }

expectRight :: (Show e) => Either e a -> IO a
expectRight (Right a) = pure a
expectRight (Left e) = assertFailure ("expected Right, got Left " <> show e) >> fail "unreachable"

-- | An 'Outgoing' 'ContinuationResult' envelope, for tests exercising the
-- @lsInbox@ replacement (@todos/session-mailbox.md@, a Phase 3 follow-up).
continuationResultOutgoing :: ContinuationToken -> UserToolResponse -> Outgoing
continuationResultOutgoing token result =
    Outgoing
        { outId = Nothing
        , outFrom = FromSystem "test"
        , outPriority = Normal
        , outHops = 0
        , outBody = ContinuationResult token result
        }

{- | Receive points R1 and R2 (@todos/session-mailbox.md@ §2), both the
'receiveMailForTurn' primitive directly and folded into a full async step.
-}
sessionMailboxTests :: TestTree
sessionMailboxTests =
    testGroup
        "Session mailbox (R1/R2)"
        [ testCase "R1 peeks unread mail without blocking and advances the cursor" $ do
            mb <- newInMemoryMailbox
            receipt <- expectRight =<< mb.mbSend (userMessageOutgoing "hi")
            (sess', envelopes) <- receiveMailForTurn (mkAsyncAgent defaultToolCallPolicy){ctxMailbox = Just mb} False mkFreshSession
            map envId envelopes @?= [receipt.rcptId]
            sess'.mailCursor @?= receipt.rcptSeq
        , testCase "R1 with nothing unread returns the session untouched" $ do
            mb <- newInMemoryMailbox
            (sess', envelopes) <- receiveMailForTurn (mkAsyncAgent defaultToolCallPolicy){ctxMailbox = Just mb} False mkFreshSession
            envelopes @?= []
            sess'.mailCursor @?= mkFreshSession.mailCursor
        , testCase "R2 blocks until mail arrives, then delivers it" $ do
            mb <- newInMemoryMailbox
            let agent = (mkAsyncAgent defaultToolCallPolicy){ctxMailbox = Just mb}
            done <-
                timeout (5 * 1000 * 1000) $ do
                    _ <- forkIO $ threadDelay (200 * 1000) >> (expectRight =<< mb.mbSend (userMessageOutgoing "delayed")) >> pure ()
                    receiveMailForTurn agent True mkFreshSession
            case done of
                Nothing -> assertFailure "receiveMailForTurn (R2) never woke up after mail arrived"
                Just (sess', envelopes) -> do
                    length envelopes @?= 1
                    sess'.mailCursor @?= maximum (map envSeq envelopes)
        , testCase "a Nothing mailbox leaves the session and cursor untouched" $ do
            let agent = mkAsyncAgent defaultToolCallPolicy
            (sess', envelopes) <- receiveMailForTurn agent True mkFreshSession
            envelopes @?= []
            sess'.mailCursor @?= mkFreshSession.mailCursor
        , testCase "unread mail is folded into the fresh turn's query and userMail (end to end)" $ do
            mb <- newInMemoryMailbox
            _ <- expectRight =<< mb.mbSend (userMessageOutgoing "hello from mail")
            let agent = (mkAsyncAgent defaultToolCallPolicy){ctxMailbox = Just mb}
            (_agent', result) <- runStepMAsync testConvId agent mkFreshSession
            session <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session" >> fail "unreachable"
            session.mailCursor @?= 1
            case session.turns of
                (UserTurn content _ : _) -> do
                    length content.userMail @?= 1
                    case content.userQuery of
                        Just q -> assertBool "query includes the mail text" ("hello from mail" `Text.isInfixOf` q.queryText)
                        Nothing -> assertFailure "expected a user query carrying the mail"
                other -> assertFailure $ "expected a user turn, got " <> show other
        , testCase "applyContinuationMail resolves a deferred call, and redelivery is a no-op" $ do
            -- Phase 3 follow-up (todos/session-mailbox.md): this is what
            -- replaces the old lsInbox/completeCall special case.
            let policy _ctx _call = Defer (Reason "approval")
                agent = mkAsyncAgent policy
            (_agent', result) <- runStepMAsync testConvId agent (mkSessionWithCalls [mkCall "defer_a"])
            session0 <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session" >> fail "unreachable"
            token <- case getPartialTurn session0 of
                Just partial -> case partial.pTrackedToolCalls of
                    [tc] -> case tc.tcContinuation of
                        Just t -> pure t
                        Nothing -> assertFailure "expected a continuation token" >> fail "unreachable"
                    other -> assertFailure ("expected exactly one tracked call, got " <> show other) >> fail "unreachable"
                Nothing -> assertFailure "expected a partial turn" >> fail "unreachable"
            mb <- newInMemoryMailbox
            _ <- expectRight =<< mb.mbSend (continuationResultOutgoing token (TextResponse "approved"))
            envelopes <- atomically (mbUnread mb 0)
            session1 <- applyContinuationMail agent envelopes session0
            case session1.turns of
                (UserTurn content _ : _) -> map snd content.userToolResponses @?= [TextResponse "approved"]
                other -> assertFailure $ "expected the deferred call to resolve into a user turn, got " <> show other
            -- Idempotent: mail delivery never consumes (only the cursor
            -- committing does), so redelivering the same envelope before
            -- its cursor advances must not re-apply the result.
            session2 <- applyContinuationMail agent envelopes session1
            session2 @?= session1
        , testCase "mailInToolResult folds mail into the last tool result of a round of sync calls" $ do
            mb <- newInMemoryMailbox
            _ <- expectRight =<< mb.mbSend (userMessageOutgoing "hello from mail")
            let agent = (mkSyncAgent defaultToolCallPolicy){ctxMailbox = Just mb, ctxMailInToolResult = True}
            (_agent', result) <- runStepM testConvId agent (mkSessionWithCalls [mkCall "t1", mkCall "t2"])
            session <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session" >> fail "unreachable"
            case session.turns of
                (UserTurn content _ : _) -> do
                    -- No separate user message: the mail went into the tool result instead.
                    content.userQuery @?= Nothing
                    length content.userMail @?= 1
                    case map snd content.userToolResponses of
                        [TextResponse first, TextResponse lastResp] -> do
                            first @?= "done:t1"
                            assertBool "first tool result untouched" ("hello from mail" `Text.isInfixOf` first == False)
                            assertBool "last tool result ends with the mail block" ("hello from mail" `Text.isInfixOf` lastResp)
                            assertBool "last tool result keeps its own text" ("done:t2" `Text.isInfixOf` lastResp)
                        other -> assertFailure ("expected two text tool responses, got " <> show other)
                other -> assertFailure $ "expected a user turn, got " <> show other
        , testCase "without mailInToolResult, mail is a separate user message after tool results (unchanged)" $ do
            mb <- newInMemoryMailbox
            _ <- expectRight =<< mb.mbSend (userMessageOutgoing "hello from mail")
            let agent = (mkSyncAgent defaultToolCallPolicy){ctxMailbox = Just mb, ctxMailInToolResult = False}
            (_agent', result) <- runStepM testConvId agent (mkSessionWithCalls [mkCall "t1", mkCall "t2"])
            session <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session" >> fail "unreachable"
            case session.turns of
                (UserTurn content _ : _) -> do
                    length content.userMail @?= 1
                    case content.userQuery of
                        Just q -> assertBool "query includes the mail text" ("hello from mail" `Text.isInfixOf` q.queryText)
                        Nothing -> assertFailure "expected a separate user query carrying the mail"
                    map snd content.userToolResponses @?= [TextResponse "done:t1", TextResponse "done:t2"]
                other -> assertFailure $ "expected a user turn, got " <> show other
        , testCase "mailInToolResult does not affect a plain user turn with no tool calls" $ do
            mb <- newInMemoryMailbox
            _ <- expectRight =<< mb.mbSend (userMessageOutgoing "hello from mail")
            let agent = (mkAsyncAgent defaultToolCallPolicy){ctxMailbox = Just mb, ctxMailInToolResult = True, ctxExecutionMode = Synchronous}
            (_agent', result) <- runStepM testConvId agent mkFreshSession
            session <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session" >> fail "unreachable"
            case session.turns of
                (UserTurn content _ : _) -> do
                    content.userToolResponses @?= []
                    case content.userQuery of
                        Just q -> assertBool "query includes the mail text" ("hello from mail" `Text.isInfixOf` q.queryText)
                        Nothing -> assertFailure "expected a user query carrying the mail"
                other -> assertFailure $ "expected a user turn, got " <> show other
        ]

-- | A session whose head turn is a plain 'UserTurn' ready for a completion
-- (so 'naiveTilNoToolCallStep' returns 'AskLlmCompletion' immediately).
mkPendingCompletionSession :: Text -> Session
mkPendingCompletionSession queryText =
    Session
        { turns =
            [ UserTurn
                (UserTurnContent (SystemPrompt "test") [] (Just (UserQuery queryText [])) [] [])
                Nothing
            ]
        , sessionId = testSessionId
        , forkedFromSessionId = Nothing
        , turnId = TurnId nil
        , sessionVersion = Just 2
        , sessionExecutionMode = Just Asynchronous
        , mailCursor = 0
        }

-- | An 'Outgoing' envelope at a given priority, for R4's interrupt tests.
priorityOutgoing :: Priority -> Text -> Outgoing
priorityOutgoing priority txt =
    Outgoing
        { outId = Nothing
        , outFrom = FromUser Nothing
        , outPriority = priority
        , outHops = 0
        , outBody = UserMessage (UserQuery txt [])
        }

{- | R4 (@todos/session-mailbox.md@ §2, D5): an 'Interrupt' envelope arriving
while 'complete' is in flight cancels it and amends the head turn, but only
when 'ctxInterruptCompletions' is set, and only for 'Interrupt' priority.
-}
interruptCompletionsTests :: TestTree
interruptCompletionsTests =
    testGroup
        "R4 (interruptCompletions)"
        [ testCase "an Interrupt envelope cancels the completion and amends the head turn" $ do
            mb <- newInMemoryMailbox
            (agent, callCount) <- mkSlowOnFirstCallAgent mb True
            _ <- forkIO $ threadDelay (100 * 1000) >> void (mb.mbSend (priorityOutgoing Interrupt "urgent"))
            result <-
                timeout (5 * 1000 * 1000) $
                    runStepMAsync testConvId agent (mkPendingCompletionSession "hi")
            session <- case result of
                Nothing -> assertFailure "runStepMAsync never returned" >> fail "unreachable"
                Just (_agent', Right s) -> pure s
                Just (_agent', Left _) -> assertFailure "expected a yielded session" >> fail "unreachable"
            readIORef callCount >>= (@?= 2)
            case session.turns of
                (LlmTurn llmContent _ : UserTurn userContent _ : _) -> do
                    llmContent.llmResponse.responseText @?= Just "fast"
                    length userContent.userMail @?= 1
                    case userContent.userQuery of
                        Just q -> do
                            assertBool "query keeps the original text" ("hi" `Text.isInfixOf` q.queryText)
                            assertBool "query includes the interrupting mail" ("urgent" `Text.isInfixOf` q.queryText)
                        Nothing -> assertFailure "expected a user query"
                    session.mailCursor @?= maximum (map envSeq userContent.userMail)
                other -> assertFailure $ "expected [LlmTurn, UserTurn, ...], got " <> show other
        , testCase "without interruptCompletions, an Interrupt envelope waits for the next R1/R2" $ do
            mb <- newInMemoryMailbox
            (agent, callCount) <- mkSlowOnFirstCallAgent mb False
            _ <- forkIO $ threadDelay (100 * 1000) >> void (mb.mbSend (priorityOutgoing Interrupt "urgent"))
            result <-
                timeout (5 * 1000 * 1000) $
                    runStepMAsync testConvId agent (mkPendingCompletionSession "hi")
            session <- case result of
                Nothing -> assertFailure "runStepMAsync never returned" >> fail "unreachable"
                Just (_agent', Right s) -> pure s
                Just (_agent', Left _) -> assertFailure "expected a yielded session" >> fail "unreachable"
            readIORef callCount >>= (@?= 1)
            case session.turns of
                (LlmTurn llmContent _ : UserTurn userContent _ : _) -> do
                    llmContent.llmResponse.responseText @?= Just "slow"
                    userContent.userMail @?= []
                other -> assertFailure $ "expected [LlmTurn, UserTurn, ...], got " <> show other
            session.mailCursor @?= 0
            unread <- atomically (mbUnread mb session.mailCursor)
            length unread @?= 1
        , testCase "a Normal-priority envelope never interrupts, even with interruptCompletions on" $ do
            mb <- newInMemoryMailbox
            (agent, callCount) <- mkSlowOnFirstCallAgent mb True
            _ <- forkIO $ threadDelay (100 * 1000) >> void (mb.mbSend (priorityOutgoing Normal "not urgent"))
            result <-
                timeout (5 * 1000 * 1000) $
                    runStepMAsync testConvId agent (mkPendingCompletionSession "hi")
            session <- case result of
                Nothing -> assertFailure "runStepMAsync never returned" >> fail "unreachable"
                Just (_agent', Right s) -> pure s
                Just (_agent', Left _) -> assertFailure "expected a yielded session" >> fail "unreachable"
            readIORef callCount >>= (@?= 1)
            case session.turns of
                (LlmTurn llmContent _ : UserTurn userContent _ : _) -> do
                    llmContent.llmResponse.responseText @?= Just "slow"
                    userContent.userMail @?= []
                other -> assertFailure $ "expected [LlmTurn, UserTurn, ...], got " <> show other
        ]
  where
    -- | An agent whose 'complete' sleeps 300ms on its first call (long enough
    -- for the concurrently-sent mail to arrive first) and returns "slow", but
    -- answers immediately with "fast" on any later call (a retry after R4
    -- amends the head turn should never need to wait again).
    mkSlowOnFirstCallAgent :: Mailbox -> Bool -> IO (Agent (LlmTurnContent, Session), IORef Int)
    mkSlowOnFirstCallAgent mb interruptOn = do
        callCount <- newIORef (0 :: Int)
        let fakeComplete _completion = do
                n <- atomicModifyIORef' callCount (\k -> (k + 1, k + 1))
                if n == 1
                    then threadDelay (300 * 1000) >> pure (LlmResponse (Just "slow") Nothing Aeson.Null Nothing, [])
                    else pure (LlmResponse (Just "fast") Nothing Aeson.Null Nothing, [])
        let agent =
                (mkAsyncAgent defaultToolCallPolicy)
                    { complete = fakeComplete
                    , ctxMailbox = Just mb
                    , ctxInterruptCompletions = interruptOn
                    }
        pure (agent, callCount)

-- | Test suite entry point.
tests :: TestTree
tests =
    testGroup
        "Session Durable CLI"
        [ tokenFormattingTests
        , resultFileParsingTests
        , policyConfigTests
        , buildPolicyTests
        , applyConfigTests
        , deferredCallExtractionTests
        , isolatedCallExtractionTests
        , pendingCompleteIntegrationTest
        , sessionMailboxTests
        , interruptCompletionsTests
        ]

-- | Continuation token formatting/parsing tests.
tokenFormattingTests :: TestTree
tokenFormattingTests =
    testGroup
        "Continuation token formatting"
        [ testCase "format and parse round-trip" $ do
            let token = ContinuationToken nil
            let txt = formatContinuationToken token
            parseContinuationToken txt @?= Just token
        , testCase "parse invalid token returns Nothing" $ do
            parseContinuationToken "not-a-uuid" @?= Nothing
        , testCase "parse formatted token returns original" $ do
            token <- newContinuationToken
            parseContinuationToken (formatContinuationToken token) @?= Just token
        ]

-- | Result-file parsing tests.
resultFileParsingTests :: TestTree
resultFileParsingTests =
    testGroup
        "Result file parsing"
        [ testCase "parses JSON UserToolResponse" $ do
            let json = "{\"type\":\"text\",\"content\":\"hello\"}"
            parseResultFile (LBS8.pack json) @?= Right (TextResponse "hello")
        , testCase "parses plain text as TextResponse" $ do
            parseResultFile (LBS8.pack "  plain result  ")
                @?= Right (TextResponse "plain result")
        , testCase "rejects empty result file" $ do
            case parseResultFile LBS.empty of
                Left _ -> pure ()
                Right _ -> assertFailure "expected empty file to be rejected"
        , testCase "parses JSON response with nested object" $ do
            let json = "{\"type\":\"json\",\"content\":{\"value\":42}}"
            parseResultFile (LBS8.pack json)
                @?= Right (JsonResponse (Aeson.object ["value" Aeson..= (42 :: Int)]))
        ]

-- | Deferred-call extraction tests.
deferredCallExtractionTests :: TestTree
deferredCallExtractionTests =
    testGroup
        "Deferred call extraction"
        [ testCase "extracts deferred calls from a partial turn" $ do
            let calls = [mkCall "sync_tool", mkCall "defer_a", mkCall "defer_b"]
            let policy _ctx call
                    | callName call == "sync_tool" = RunSync
                    | otherwise = Defer (Reason "approval")
            (_agent, result) <- runStepMAsync testConvId (mkAsyncAgent policy) (mkSessionWithCalls calls)
            session <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session"
            let deferred = extractDeferredCalls session
            length deferred @?= 2
            let names = [toolName | (_, _, toolName, _) <- deferred]
            sort names @?= ["defer_a", "defer_b"]
            all isJust [mTok | (_, mTok, _, _) <- deferred] @?= True
        , testCase "returns empty list when no partial turn" $ do
            let session = mkSessionWithCalls []
            extractDeferredCalls session @?= []
        ]

-- | Isolated-call extraction tests.
-- | Isolated-call extraction tests.
isolatedCallExtractionTests :: TestTree
isolatedCallExtractionTests =
    testGroup
        "Isolated call extraction"
        [ testCase "extracts RunIsolated deferred calls from a partial turn" $ do
            cid <- newToolCallId
            token <- newContinuationToken
            let call = mkCall "bash_command"
            let disp = RunIsolated (LocalProcess "/dummy/worker")
            let tracked =
                    TrackedToolCall
                        { tcId = cid
                        , tcCall = call
                        , tcState = Deferred
                        , tcResult = Nothing
                        , tcContinuation = Just token
                        , tcPolicy = AppliedPolicy disp Nothing
                        , tcEntityId = Nothing
                        , tcDeliveredLate = False
                        }
            let partial =
                    PartialUserTurnContent
                        { pUserPrompt = SystemPrompt "test"
                        , pUserTools = []
                        , pUserQuery = Nothing
                        , pTrackedToolCalls = [tracked]
                        }
            let session = (mkSessionWithCalls []){turns = [PartialUserTurn partial Nothing]}
            let isolated = extractIsolatedCalls session
            length isolated @?= 1
            case isolated of
                [(_, tok, c, d)] -> do
                    tok @?= token
                    callName c @?= "bash_command"
                    case snd (flattenDisposition d) of
                        RunIsolated _ -> pure ()
                        _ -> assertFailure "expected RunIsolated disposition"
                _ -> assertFailure "expected exactly one isolated call"
        , testCase "ignores non-isolated deferred calls" $ do
            cid <- newToolCallId
            token <- newContinuationToken
            let call = mkCall "defer_a"
            let disp = Defer (Reason "approval")
            let tracked =
                    TrackedToolCall
                        { tcId = cid
                        , tcCall = call
                        , tcState = Deferred
                        , tcResult = Nothing
                        , tcContinuation = Just token
                        , tcPolicy = AppliedPolicy disp Nothing
                        , tcEntityId = Nothing
                        , tcDeliveredLate = False
                        }
            let partial =
                    PartialUserTurnContent
                        { pUserPrompt = SystemPrompt "test"
                        , pUserTools = []
                        , pUserQuery = Nothing
                        , pTrackedToolCalls = [tracked]
                        }
            let session = (mkSessionWithCalls []){turns = [PartialUserTurn partial Nothing]}
            extractIsolatedCalls session @?= []
        ]

-- | Integration test: pending and complete via file session store.
pendingCompleteIntegrationTest :: TestTree
pendingCompleteIntegrationTest =
    testCase "pending/complete round-trip via file store" $
        withSystemTempDirectory "session-durable-cli" $ \dir -> do
            let store = SessionStore.mkSimpleSessionStore dir
            let calls = [mkCall "sync_tool", mkCall "defer_a"]
            let policy _ctx call
                    | callName call == "sync_tool" = RunSync
                    | otherwise = Defer (Reason "approval")
            (_agent, result) <- runStepMAsync testConvId (mkAsyncAgent policy) (mkSessionWithCalls calls)
            session <- case result of
                Right s -> pure s
                Left _ -> assertFailure "expected a yielded session"
            SessionStore.storeSession store testConvId session

            -- Pending should find the deferred call.
            let pending = extractDeferredCalls session
            length pending @?= 1
            token <- case pending of
                [(_, Just tok, _, _)] -> pure tok
                _ -> assertFailure "expected one deferred call with a token"

            -- Complete the deferred call.
            let response = TextResponse "external result"
            updated <- wakeSession session [(token, response)]
            case getPartialTurn updated of
                Just _ -> assertFailure "expected turn to be complete"
                Nothing -> pure ()

            -- After completion there should be no deferred calls.
-- | Minimal JSON agent config for testing config application.
minimalBaseAgent :: Base.Agent
minimalBaseAgent =
    Base.Agent
        { Base.slug = "test-agent"
        , Base.apiKeyId = "test-key"
        , Base.flavor = "openai"
        , Base.modelUrl = "http://example.com/v1"
        , Base.modelName = "test-model"
        , Base.announce = "test agent"
        , Base.systemPrompt = []
        , Base.toolDirectory = Nothing
        , Base.bashToolboxes = Nothing
        , Base.mcpServers = Nothing
        , Base.openApiToolboxes = Nothing
        , Base.postgrestToolboxes = Nothing
        , Base.builtinToolboxes = Nothing
        , Base.extraAgents = Nothing
        , Base.skillSources = Nothing
        , Base.autoEnableSkills = Nothing
        , Base.executionMode = Nothing
        , Base.toolCallPolicyConfig = Nothing
        , Base.asyncYieldStrategy = Nothing
        , Base.maxConcurrency = Nothing
        , Base.asyncCallTimeoutSeconds = Nothing
        , Base.bindings = Nothing
        , Base.parameters = Nothing
        , Base.pauseCancelsCalls = Nothing
        , Base.resumeOnAnyMail = Nothing
        , Base.interruptCompletions = Nothing
        , Base.mailInToolResult = Nothing
        , Base.mailScope = Nothing
        , Base.interruptScope = Nothing, Base.wakeOn = Nothing
        }

-- | Tool-call policy config JSON round-trip tests.
policyConfigTests :: TestTree
policyConfigTests =
    testGroup
        "ToolCallPolicyConfig JSON"
        [ testCase "round-trips default and rules" $ do
            let cfg =
                    Base.ToolCallPolicyConfig
                        RunSync
                        [ Base.ToolCallPolicyRule "bash_command" (Defer (Reason "approval"))
                        , Base.ToolCallPolicyRule "fetch_remote" (RunAsync Nothing)
                        ]
                        []
            let json = Aeson.encode cfg
            Aeson.decode json @?= Just cfg
        , testCase "round-trips wrappers" $ do
            let cfg =
                    Base.ToolCallPolicyConfig
                        RunSync
                        []
                        [ Base.ToolCallWrapperRule
                            (Base.WrapperMatch (Just "http_*"))
                            [WithTimeout 30, WithRetries 2]
                        ]
            let json = Aeson.encode cfg
            Aeson.decode json @?= Just cfg
        , testCase "round-trips a before/after hook wrapper (session-mailbox.md §6 example)" $ do
            let cfg =
                    Base.ToolCallPolicyConfig
                        RunSync
                        []
                        [ Base.ToolCallWrapperRule
                            (Base.WrapperMatch (Just "deploy_*"))
                            [ WithBeforeHook (HookCommand "hooks/approve-deploy")
                            , WithAfterHook (HookTool "audit_log")
                            ]
                        ]
            let json = Aeson.encode cfg
            Aeson.decode json @?= Just cfg
        , testCase "parses example policy config JSON" $ do
            let json =
                    LBS8.pack $
                        unlines
                            [ "{"
                            , "  \"default\": {\"tag\":\"runSync\"},"
                            , "  \"rules\": ["
                            , "    {\"tool\":\"bash_command\",\"disposition\":{\"tag\":\"defer\",\"reason\":\"approval required\"}},"
                            , "    {\"tool\":\"fetch_remote\",\"disposition\":{\"tag\":\"runAsync\"}}"
                            , "  ]"
                            , "}"
                            ]
            case Aeson.decode json :: Maybe Base.ToolCallPolicyConfig of
                Nothing -> assertFailure "failed to parse policy config"
                Just cfg -> do
                    Base.tpcDefaultDisposition cfg @?= RunSync
                    map Base.tprToolName (Base.tpcRules cfg) @?= ["bash_command", "fetch_remote"]
                    Base.tpcWrappers cfg @?= []
        , testCase "parses wrappers from JSON" $ do
            let json =
                    LBS8.pack $
                        unlines
                            [ "{"
                            , "  \"default\": {\"tag\":\"runSync\"},"
                            , "  \"rules\": [],"
                            , "  \"wrappers\": ["
                            , "    {\"match\":{\"tool\":\"http_*\"},\"decorators\":["
                            , "      {\"tag\":\"retries\",\"count\":2},"
                            , "      {\"tag\":\"timeout\",\"seconds\":30}"
                            , "    ]}"
                            , "  ]"
                            , "}"
                            ]
            case Aeson.decode json :: Maybe Base.ToolCallPolicyConfig of
                Nothing -> assertFailure "failed to parse policy config"
                Just cfg -> do
                    map (Base.wmTool . Base.twrMatch) (Base.tpcWrappers cfg) @?= [Just "http_*"]
                    map Base.twrDecorators (Base.tpcWrappers cfg) @?= [[WithRetries 2, WithTimeout 30]]
        ]

-- | 'buildToolCallPolicy' tests.
buildPolicyTests :: TestTree
buildPolicyTests =
    testGroup
        "buildToolCallPolicy"
        [ testCase "uses rule disposition for matching tool" $ do
            let cfg = Base.ToolCallPolicyConfig RunSync [Base.ToolCallPolicyRule "defer_me" (Defer (Reason "test"))] []
            let policy = buildToolCallPolicy cfg
            policy undefined (mkCall "defer_me") @?= Defer (Reason "test")
        , testCase "uses default disposition for unknown tool" $ do
            let cfg = Base.ToolCallPolicyConfig RunSync [Base.ToolCallPolicyRule "defer_me" (Defer (Reason "test"))] []
            let policy = buildToolCallPolicy cfg
            policy undefined (mkCall "unknown") @?= RunSync
        , testCase "composes matching wrapper decorators onto the base disposition" $ do
            let cfg =
                    Base.ToolCallPolicyConfig
                        RunSync
                        []
                        [ Base.ToolCallWrapperRule (Base.WrapperMatch (Just "http_*")) [WithTimeout 30]
                        , Base.ToolCallWrapperRule (Base.WrapperMatch (Just "*")) [WithLabel "traced"]
                        ]
            let policy = buildToolCallPolicy cfg
            policy undefined (mkCall "http_get") @?= Decorate [WithTimeout 30, WithLabel "traced"] RunSync
            policy undefined (mkCall "bash_command") @?= Decorate [WithLabel "traced"] RunSync
        ]

-- | 'applyAgentDurableConfig' tests.
applyConfigTests :: TestTree
applyConfigTests =
    testGroup
        "applyAgentDurableConfig"
        [ testCase "sets execution mode from JSON agent" $ do
            let jsonAgent = minimalBaseAgent{Base.executionMode = Just Asynchronous}
            let agent = mkAsyncAgent defaultToolCallPolicy
            let agent' = applyAgentDurableConfig jsonAgent agent
            ctxExecutionMode agent' @?= Asynchronous
        , testCase "sets tool-call policy from JSON agent" $ do
            let cfg = Base.ToolCallPolicyConfig RunSync [Base.ToolCallPolicyRule "bash_command" (Defer (Reason "approval"))] []
            let jsonAgent = minimalBaseAgent{Base.toolCallPolicyConfig = Just cfg}
            let agent = mkAsyncAgent defaultToolCallPolicy
            let agent' = applyAgentDurableConfig jsonAgent agent
            ctxToolCallPolicy agent' undefined (mkCall "bash_command") @?= Defer (Reason "approval")
            ctxToolCallPolicy agent' undefined (mkCall "other") @?= RunSync
        , testCase "sets yield strategy and max concurrency from JSON agent" $ do
            let jsonAgent =
                    minimalBaseAgent
                        { Base.asyncYieldStrategy = Just YieldOnAnyProgress
                        , Base.maxConcurrency = Just 2
                        , Base.asyncCallTimeoutSeconds = Just 30
                        }
            let agent' = applyAgentDurableConfig jsonAgent (mkAsyncAgent defaultToolCallPolicy)
            ctxAsyncYieldStrategy agent' @?= YieldOnAnyProgress
            ctxMaxConcurrency agent' @?= Just 2
            ctxAsyncCallTimeout agent' @?= Just 30
        , testCase "leaves unset fields untouched" $ do
            let agent = (mkAsyncAgent defaultToolCallPolicy){ctxAsyncYieldStrategy = YieldOnTimeout 5}
            let agent' = applyAgentDurableConfig minimalBaseAgent agent
            ctxAsyncYieldStrategy agent' @?= YieldOnTimeout 5
            ctxMaxConcurrency agent' @?= Nothing
        , testCase "parses async fields from agent JSON" $ do
            let json =
                    "{\"slug\":\"a\",\"apiKeyId\":\"k\",\"flavor\":\"openai\",\"modelUrl\":\"u\",\"modelName\":\"m\",\"announce\":\"x\",\"systemPrompt\":[],\"executionMode\":\"asynchronous\",\"asyncYieldStrategy\":{\"tag\":\"yieldOnTimeout\",\"milliseconds\":500},\"maxConcurrency\":3,\"asyncCallTimeoutSeconds\":120}"
            case Aeson.eitherDecode json of
                Left err -> assertFailure err
                Right parsed -> do
                    Base.executionMode parsed @?= Just Asynchronous
                    Base.asyncYieldStrategy parsed @?= Just (YieldOnTimeout 500)
                    Base.maxConcurrency parsed @?= Just 3
                    Base.asyncCallTimeoutSeconds parsed @?= Just 120
        ]

-- | Simple insertion sort for tests.
sort :: Ord a => [a] -> [a]
sort = foldr insert []
  where
    insert x [] = [x]
    insert x (y : ys)
        | x <= y = x : y : ys
        | otherwise = y : insert x ys

