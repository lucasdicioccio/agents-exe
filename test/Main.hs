module Main where

import System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Base as Base
import System.Agents.Tools.Context as Context
import System.Agents.Session.Types (SessionId(..), TurnId(..))
import qualified System.Agents.Session.Base as SessionBase

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "All Tests"
        [ openAIRateLimitTests
        , extraAgentRefTests
        , agentSerializationTests
        , callStackEntryTests
        , toolExecutionContextTests
        , recursionTrackingTests
        ]

openAIRateLimitTests :: TestTree
openAIRateLimitTests =
    testGroup
        "OpenAI rate-limits"
        [ testCase "1d1h1m1s" $
            parseRateLimitDelay "1d1h1m1s" @?= 1 + 60 + 3600 + 86400
        , testCase "1h1m1s" $
            parseRateLimitDelay "1h1m1s" @?= 1 + 60 + 3600
        , testCase "1m1s" $
            parseRateLimitDelay "1m1s" @?= 1 + 60
        , testCase "1s" $
            parseRateLimitDelay "1s" @?= 1
        , testCase "1.376s" $
            parseRateLimitDelay "1.376s" @?= 2
        , testCase "120ms" $
            parseRateLimitDelay "120ms" @?= 1
        , testCase "26.001s" $
            parseRateLimitDelay "26.001s" @?= 27
        ]

extraAgentRefTests :: TestTree
extraAgentRefTests =
    testGroup
        "ExtraAgentRef JSON serialization"
        [ testCase "serialize to JSON" $ do
            let ref = Base.ExtraAgentRef { Base.extraAgentSlug = "helper-bot", Base.extraAgentPath = "./helpers/bot.json" }
            let json = encode ref
            Text.unpack (decodeUtf8 json) @?= "{\"slug\":\"helper-bot\",\"path\":\"./helpers/bot.json\"}"
        , testCase "deserialize from JSON" $ do
            let json = "{\"slug\":\"helper-bot\",\"path\":\"./helpers/bot.json\"}"
            let mRef = decode (encodeUtf8 json) :: Maybe Base.ExtraAgentRef
            mRef @?= Just (Base.ExtraAgentRef { Base.extraAgentSlug = "helper-bot", Base.extraAgentPath = "./helpers/bot.json" })
        , testCase "round-trip" $ do
            let ref = Base.ExtraAgentRef { Base.extraAgentSlug = "superb-agent", Base.extraAgentPath = "../superb/agent.json" }
            let json = encode ref
            let mRef = decode json :: Maybe Base.ExtraAgentRef
            mRef @?= Just ref
        ]
  where
    encodeUtf8 = LBS.fromStrict . Text.encodeUtf8
    decodeUtf8 = Text.decodeUtf8 . LBS.toStrict

agentSerializationTests :: TestTree
agentSerializationTests =
    testGroup
        "Agent JSON serialization"
        [ testCase "backward compatibility - agent without extra-agents" $ do
            -- This JSON format should still work with existing agent.json files
            let json = Text.unlines
                    [ "{"
                    , "  \"slug\": \"test-agent\","
                    , "  \"apiKeyId\": \"openai\","
                    , "  \"flavor\": \"openai\","
                    , "  \"modelUrl\": \"https://api.openai.com/v1\","
                    , "  \"modelName\": \"gpt-4\","
                    , "  \"announce\": \"A test agent\","
                    , "  \"systemPrompt\": [\"You are helpful\"],"
                    , "  \"toolDirectory\": \"tools\""
                    , "}"
                    ]
            let mAgent = decode (encodeUtf8 json) :: Maybe Base.Agent
            case mAgent of
                Nothing -> assertFailure "Failed to parse agent without extra-agents"
                Just agent -> do
                    Base.slug agent @?= "test-agent"
                    Base.extraAgents agent @?= Nothing
        , testCase "agent with extra-agents" $ do
            let json = Text.unlines
                    [ "{"
                    , "  \"slug\": \"boss-kimi\","
                    , "  \"apiKeyId\": \"openrouter\","
                    , "  \"flavor\": \"openai\","
                    , "  \"modelUrl\": \"https://openrouter.ai/api/v1\","
                    , "  \"modelName\": \"moonshot-ai/kimi-k2\","
                    , "  \"toolDirectory\": \"tools\","
                    , "  \"extraAgents\": ["
                    , "    {\"slug\": \"superb-agent\", \"path\": \"../superb/agent.json\"},"
                    , "    {\"slug\": \"helper-bot\", \"path\": \"./helpers/bot.json\"}"
                    , "  ],"
                    , "  \"announce\": \"The agent\","
                    , "  \"systemPrompt\": [\"You are a helpful software agent\"]"
                    , "}"
                    ]
            let mAgent = decode (encodeUtf8 json) :: Maybe Base.Agent
            case mAgent of
                Nothing -> assertFailure "Failed to parse agent with extra-agents"
                Just agent -> do
                    Base.slug agent @?= "boss-kimi"
                    Base.apiKeyId agent @?= "openrouter"
                    case Base.extraAgents agent of
                        Nothing -> assertFailure "extraAgents should not be Nothing"
                        Just refs -> length refs @?= 2
        , testCase "round-trip with extra-agents" $ do
            let agent = Base.Agent
                    { Base.slug = "test-agent"
                    , Base.apiKeyId = "openai"
                    , Base.flavor = "openai"
                    , Base.modelUrl = "https://api.openai.com/v1"
                    , Base.modelName = "gpt-4"
                    , Base.announce = "A test agent"
                    , Base.systemPrompt = ["You are helpful"]
                    , Base.toolDirectory = "tools"
                    , Base.mcpServers = Nothing
                    , Base.extraAgents = Just
                        [ Base.ExtraAgentRef { Base.extraAgentSlug = "helper", Base.extraAgentPath = "./helper.json" }
                        ]
                    }
            let json = encode agent
            let mAgent = decode json :: Maybe Base.Agent
            mAgent @?= Just agent
        , testCase "round-trip without extra-agents" $ do
            let agent = Base.Agent
                    { Base.slug = "test-agent"
                    , Base.apiKeyId = "openai"
                    , Base.flavor = "openai"
                    , Base.modelUrl = "https://api.openai.com/v1"
                    , Base.modelName = "gpt-4"
                    , Base.announce = "A test agent"
                    , Base.systemPrompt = ["You are helpful"]
                    , Base.toolDirectory = "tools"
                    , Base.mcpServers = Nothing
                    , Base.extraAgents = Nothing
                    }
            let json = encode agent
            let mAgent = decode json :: Maybe Base.Agent
            mAgent @?= Just agent
        , testCase "AgentDescription round-trip" $ do
            let agent = Base.Agent
                    { Base.slug = "test-agent"
                    , Base.apiKeyId = "openai"
                    , Base.flavor = "openai"
                    , Base.modelUrl = "https://api.openai.com/v1"
                    , Base.modelName = "gpt-4"
                    , Base.announce = "A test agent"
                    , Base.systemPrompt = ["You are helpful"]
                    , Base.toolDirectory = "tools"
                    , Base.mcpServers = Just []
                    , Base.extraAgents = Just
                        [ Base.ExtraAgentRef { Base.extraAgentSlug = "helper", Base.extraAgentPath = "./helper.json" }
                        ]
                    }
            let desc = Base.AgentDescription agent
            let json = encode desc
            let mDesc = decode json :: Maybe Base.AgentDescription
            mDesc @?= Just desc
        ]
  where
    encodeUtf8 = LBS.fromStrict . Text.encodeUtf8

-------------------------------------------------------------------------------
-- CallStackEntry Tests
-------------------------------------------------------------------------------

callStackEntryTests :: TestTree
callStackEntryTests =
    testGroup
        "CallStackEntry"
        [ testCase "JSON serialization round-trip" $ do
            convId <- Base.newConversationId
            let entry = Context.CallStackEntry
                    { Context.callAgentSlug = "test-agent"
                    , Context.callConversationId = convId
                    , Context.callDepth = 3
                    }
            let json = encode entry
            let mEntry = decode json :: Maybe Context.CallStackEntry
            mEntry @?= Just entry
        , testCase "equality" $ do
            convId <- Base.newConversationId
            convId2 <- Base.newConversationId
            let entry1 = Context.CallStackEntry "agent-a" convId 1
            let entry2 = Context.CallStackEntry "agent-a" convId 1
            let entry3 = Context.CallStackEntry "agent-b" convId 1
            let entry4 = Context.CallStackEntry "agent-a" convId2 1
            entry1 @?= entry2
            assertBool "Different slugs should not be equal" (entry1 /= entry3)
            assertBool "Different conversation IDs should not be equal" (entry1 /= entry4)
        , testCase "show representation" $ do
            convId <- Base.newConversationId
            let entry = Context.CallStackEntry "test-agent" convId 2
            let shown = show entry
            assertBool "Show should contain slug" $ "test-agent" `Text.isInfixOf` Text.pack shown
        ]

-------------------------------------------------------------------------------
-- ToolExecutionContext Tests
-------------------------------------------------------------------------------

toolExecutionContextTests :: TestTree
toolExecutionContextTests =
    testGroup
        "ToolExecutionContext"
        [ testCase "mkMinimalContext creates context with empty call stack" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let ctx = Context.mkMinimalContext sessionId convId turnId
            Context.ctxCallStack ctx @?= []
            Context.ctxMaxDepth ctx @?= Nothing
            Context.ctxSessionId ctx @?= sessionId
            Context.ctxConversationId ctx @?= convId
            Context.ctxTurnId ctx @?= turnId
            Context.ctxAgentId ctx @?= Nothing
            Context.ctxFullSession ctx @?= Nothing
        , testCase "mkRootContext creates context with root entry in call stack" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let ctx = Context.mkRootContext sessionId convId turnId Nothing Nothing (Just 5)
            Context.ctxCallStack ctx @?= [Context.CallStackEntry "root" convId 0]
            Context.ctxMaxDepth ctx @?= Just 5
        , testCase "mkToolExecutionContext with full parameters" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            agentId <- Base.newAgentId
            convId2 <- Base.newConversationId
            let callStack = [Context.CallStackEntry "agent-1" convId 0, Context.CallStackEntry "agent-2" convId2 1]
            let ctx = Context.mkToolExecutionContext sessionId convId turnId (Just agentId) Nothing callStack (Just 10)
            Context.ctxCallStack ctx @?= callStack
            Context.ctxMaxDepth ctx @?= Just 10
            Context.ctxAgentId ctx @?= Just agentId
        , testCase "JSON round-trip with call stack and max depth" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            agentId <- Base.newAgentId
            convId2 <- Base.newConversationId
            let callStack =
                    [ Context.CallStackEntry "agent-1" convId 0
                    , Context.CallStackEntry "agent-2" convId2 1
                    ]
            let ctx = Context.mkToolExecutionContext sessionId convId turnId (Just agentId) Nothing callStack (Just 5)
            let json = encode ctx
            let mCtx = decode json :: Maybe Context.ToolExecutionContext
            case mCtx of
                Nothing -> assertFailure "Failed to decode ToolExecutionContext"
                Just decodedCtx -> do
                    Context.ctxCallStack decodedCtx @?= callStack
                    Context.ctxMaxDepth decodedCtx @?= Just 5
                    Context.ctxSessionId decodedCtx @?= sessionId
                    Context.ctxAgentId decodedCtx @?= Just agentId
        , testCase "hasFullSession returns False when session is Nothing" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let ctx = Context.mkMinimalContext sessionId convId turnId
            Context.hasFullSession ctx @?= False
        , testCase "hasAgentId returns False when agentId is Nothing" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let ctx = Context.mkMinimalContext sessionId convId turnId
            Context.hasAgentId ctx @?= False
        ]

-------------------------------------------------------------------------------
-- Recursion Tracking Tests
-------------------------------------------------------------------------------

recursionTrackingTests :: TestTree
recursionTrackingTests =
    testGroup
        "Recursion tracking"
        [ testCase "currentRecursionDepth returns 0 for empty call stack" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let ctx = Context.mkMinimalContext sessionId convId turnId
            Context.currentRecursionDepth ctx @?= 0
        , testCase "currentRecursionDepth returns 1 for root context" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let ctx = Context.mkRootContext sessionId convId turnId Nothing Nothing Nothing
            Context.currentRecursionDepth ctx @?= 1
        , testCase "pushAgentContext increments depth" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            newConvId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId Nothing Nothing Nothing
            case Context.pushAgentContext "sub-agent" newConvId rootCtx of
                Left err -> assertFailure $ "Should not have failed: " ++ show err
                Right newCtx -> do
                    Context.currentRecursionDepth newCtx @?= 2
                    -- Check that the new entry is at the head of the list
                    case Context.ctxCallStack newCtx of
                        (entry:_) -> do
                            Context.callAgentSlug entry @?= "sub-agent"
                            Context.callConversationId entry @?= newConvId
                            Context.callDepth entry @?= 1
                        [] -> assertFailure "Expected at least one entry in call stack"
        , testCase "pushAgentContext respects max depth limit" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            newConvId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            -- Create root context with max depth of 1
            let rootCtx = Context.mkRootContext sessionId convId turnId Nothing Nothing (Just 1)
            -- First push should fail because root is already at depth 1 (max)
            case Context.pushAgentContext "sub-agent" newConvId rootCtx of
                Left (Context.MaxRecursionDepthExceeded stack) -> do
                    length stack @?= 1  -- Root entry
                Right _ -> assertFailure "Should have failed with MaxRecursionDepthExceeded"
        , testCase "isAtDepth correctly checks depth threshold" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId Nothing Nothing Nothing
            Context.isAtDepth 1 rootCtx @?= True
            Context.isAtDepth 2 rootCtx @?= False
        , testCase "callChain returns entries in root-first order" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            newConvId1 <- Base.newConversationId
            newConvId2 <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId Nothing Nothing Nothing
            case Context.pushAgentContext "agent-1" newConvId1 rootCtx of
                Left err -> assertFailure $ "First push should not fail: " ++ show err
                Right ctx1 -> case Context.pushAgentContext "agent-2" newConvId2 ctx1 of
                    Left err -> assertFailure $ "Second push should not fail: " ++ show err
                    Right ctx2 -> do
                        let chain = Context.callChain ctx2
                        length chain @?= 3
                        -- Root should be first in callChain (last in internal stack)
                        Context.callAgentSlug (head chain) @?= "root"
                        Context.callAgentSlug (chain !! 1) @?= "agent-1"
                        Context.callAgentSlug (chain !! 2) @?= "agent-2"
        , testCase "isAgentInCallStack detects agent in stack" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId Nothing Nothing Nothing
            Context.isAgentInCallStack "root" rootCtx @?= True
            Context.isAgentInCallStack "nonexistent" rootCtx @?= False
        , testCase "isAgentInCallStack detects agent after push" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            newConvId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId Nothing Nothing Nothing
            case Context.pushAgentContext "helper-agent" newConvId rootCtx of
                Left err -> assertFailure $ "Push should not fail: " ++ show err
                Right newCtx -> do
                    Context.isAgentInCallStack "helper-agent" newCtx @?= True
                    Context.isAgentInCallStack "root" newCtx @?= True
                    Context.isAgentInCallStack "nonexistent" newCtx @?= False
        , testCase "pushAgentContext updates conversation ID" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            newConvId <- Base.newConversationId
            turnId <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId Nothing Nothing Nothing
            case Context.pushAgentContext "sub-agent" newConvId rootCtx of
                Left err -> assertFailure $ "Push should not fail: " ++ show err
                Right newCtx -> do
                    Context.ctxConversationId newCtx @?= newConvId
                    -- Session ID and Turn ID should be preserved
                    Context.ctxSessionId newCtx @?= sessionId
                    Context.ctxTurnId newCtx @?= turnId
        ]

