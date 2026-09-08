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
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.UUID (nil)
import System.IO.Temp (withSystemTempDirectory)
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
import System.Agents.Session.Step (getPartialTurn, naiveTilNoToolCallStep, runStepMAsync)
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
        "Session Durable CLI"
        [ tokenFormattingTests
        , resultFileParsingTests
        , policyConfigTests
        , buildPolicyTests
        , applyConfigTests
        , deferredCallExtractionTests
        , isolatedCallExtractionTests
        , pendingCompleteIntegrationTest
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
                        , Base.ToolCallPolicyRule "fetch_remote" RunAsync
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
        ]

-- | 'buildToolCallPolicy' tests.
buildPolicyTests :: TestTree
buildPolicyTests =
    testGroup
        "buildToolCallPolicy"
        [ testCase "uses rule disposition for matching tool" $ do
            let cfg = Base.ToolCallPolicyConfig RunSync [Base.ToolCallPolicyRule "defer_me" (Defer (Reason "test"))]
            let policy = buildToolCallPolicy cfg
            policy undefined (mkCall "defer_me") @?= Defer (Reason "test")
        , testCase "uses default disposition for unknown tool" $ do
            let cfg = Base.ToolCallPolicyConfig RunSync [Base.ToolCallPolicyRule "defer_me" (Defer (Reason "test"))]
            let policy = buildToolCallPolicy cfg
            policy undefined (mkCall "unknown") @?= RunSync
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
            let cfg = Base.ToolCallPolicyConfig RunSync [Base.ToolCallPolicyRule "bash_command" (Defer (Reason "approval"))]
            let jsonAgent = minimalBaseAgent{Base.toolCallPolicyConfig = Just cfg}
            let agent = mkAsyncAgent defaultToolCallPolicy
            let agent' = applyAgentDurableConfig jsonAgent agent
            ctxToolCallPolicy agent' undefined (mkCall "bash_command") @?= Defer (Reason "approval")
            ctxToolCallPolicy agent' undefined (mkCall "other") @?= RunSync
        ]

-- | Simple insertion sort for tests.
sort :: Ord a => [a] -> [a]
sort = foldr insert []
  where
    insert x [] = [x]
    insert x (y : ys)
        | x <= y = x : y : ys
        | otherwise = y : insert x ys

