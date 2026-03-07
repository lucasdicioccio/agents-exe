module Main where

import System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Base as Base
import System.Agents.Tools.Context as Context
import System.Agents.Session.Types (SessionId(..), TurnId(..))
import qualified System.Agents.Session.Base as SessionBase
import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Tools.OpenAPI.Types as OpenAPI

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
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
        , agentConfigGraphTests
        , referenceValidationTests
        , cycleDetectionTests
        , openAPITypesTests
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
-- OpenAPI Types Tests
-------------------------------------------------------------------------------

openAPITypesTests :: TestTree
openAPITypesTests =
    testGroup
        "OpenAPI Types"
        [ testCase "parse minimal OpenAPI spec" $ do
            let json = "{\"paths\": {}}"
            let mSpec = decode (encodeUtf8 json) :: Maybe OpenAPI.OpenAPISpec
            case mSpec of
                Nothing -> assertFailure "Failed to parse minimal OpenAPI spec"
                Just spec -> do
                    Map.null (OpenAPI.specPaths spec) @?= True
                    OpenAPI.specComponents spec @?= Nothing
        , testCase "parse OpenAPI spec with simple path" $ do
            let json = Text.unlines
                    [ "{"
                    , "  \"paths\": {"
                    , "    \"/pets\": {"
                    , "      \"get\": {"
                    , "        \"operationId\": \"listPets\","
                    , "        \"summary\": \"List all pets\""
                    , "      }"
                    , "    }"
                    , "  }"
                    , "}"
                    ]
            let mSpec = decode (encodeUtf8 json) :: Maybe OpenAPI.OpenAPISpec
            case mSpec of
                Nothing -> assertFailure "Failed to parse OpenAPI spec with path"
                Just spec -> do
                    Map.size (OpenAPI.specPaths spec) @?= 1
                    case Map.lookup "/pets" (OpenAPI.specPaths spec) of
                        Nothing -> assertFailure "Path /pets not found"
                        Just methods -> do
                            Map.size methods @?= 1
                            case Map.lookup "GET" methods of
                                Nothing -> assertFailure "GET method not found"
                                Just op -> do
                                    OpenAPI.opOperationId op @?= Just "listPets"
                                    OpenAPI.opSummary op @?= Just "List all pets"
        , testCase "parse parameter with path location" $ do
            let json = Text.unlines
                    [ "{"
                    , "  \"name\": \"petId\","
                    , "  \"in\": \"path\","
                    , "  \"required\": true,"
                    , "  \"schema\": {"
                    , "    \"type\": \"string\""
                    , "  }"
                    , "}"
                    ]
            let mParam = decode (encodeUtf8 json) :: Maybe OpenAPI.Parameter
            case mParam of
                Nothing -> assertFailure "Failed to parse parameter"
                Just param -> do
                    OpenAPI.paramName param @?= "petId"
                    OpenAPI.paramIn param @?= OpenAPI.ParamInPath
                    OpenAPI.paramRequired param @?= True
                    OpenAPI.paramDescription param @?= Nothing
        , testCase "parse parameter with query location" $ do
            let json = Text.unlines
                    [ "{"
                    , "  \"name\": \"limit\","
                    , "  \"in\": \"query\","
                    , "  \"description\": \"Maximum number of results\","
                    , "  \"schema\": {"
                    , "    \"type\": \"integer\""
                    , "  }"
                    , "}"
                    ]
            let mParam = decode (encodeUtf8 json) :: Maybe OpenAPI.Parameter
            case mParam of
                Nothing -> assertFailure "Failed to parse query parameter"
                Just param -> do
                    OpenAPI.paramName param @?= "limit"
                    OpenAPI.paramIn param @?= OpenAPI.ParamInQuery
                    OpenAPI.paramRequired param @?= False
                    OpenAPI.paramDescription param @?= Just "Maximum number of results"
        , testCase "parse schema with $ref" $ do
            let json = "{\"$ref\": \"#/components/schemas/Pet\"}"
            let mSchema = decode (encodeUtf8 json) :: Maybe OpenAPI.Schema
            case mSchema of
                Nothing -> assertFailure "Failed to parse $ref schema"
                Just schema -> do
                    OpenAPI.schemaRef schema @?= Just "#/components/schemas/Pet"
                    OpenAPI.schemaType schema @?= Nothing
        , testCase "parse inline schema with properties" $ do
            let json = Text.unlines
                    [ "{"
                    , "  \"type\": \"object\","
                    , "  \"properties\": {"
                    , "    \"name\": { \"type\": \"string\" },"
                    , "    \"age\": { \"type\": \"integer\" }"
                    , "  },"
                    , "  \"required\": [\"name\"]"
                    , "}"
                    ]
            let mSchema = decode (encodeUtf8 json) :: Maybe OpenAPI.Schema
            case mSchema of
                Nothing -> assertFailure "Failed to parse inline schema"
                Just schema -> do
                    OpenAPI.schemaType schema @?= Just "object"
                    case OpenAPI.schemaProperties schema of
                        Nothing -> assertFailure "Properties not found"
                        Just props -> Map.size props @?= 2
                    OpenAPI.schemaRequired schema @?= Just ["name"]
        , testCase "parse request body with content" $ do
            let json = Text.unlines
                    [ "{"
                    , "  \"description\": \"Pet to create\","
                    , "  \"required\": true,"
                    , "  \"content\": {"
                    , "    \"application/json\": {"
                    , "      \"schema\": { \"type\": \"object\" }"
                    , "    }"
                    , "  }"
                    , "}"
                    ]
            let mBody = decode (encodeUtf8 json) :: Maybe OpenAPI.RequestBody
            case mBody of
                Nothing -> assertFailure "Failed to parse request body"
                Just body -> do
                    OpenAPI.reqBodyDescription body @?= Just "Pet to create"
                    OpenAPI.reqBodyRequired body @?= True
                    Map.size (OpenAPI.reqBodyContent body) @?= 1
                    case Map.lookup "application/json" (OpenAPI.reqBodyContent body) of
                        Nothing -> assertFailure "application/json content not found"
                        Just _ -> pure ()
        , testCase "parse components with schemas" $ do
            let json = Text.unlines
                    [ "{"
                    , "  \"schemas\": {"
                    , "    \"Pet\": {"
                    , "      \"type\": \"object\","
                    , "      \"properties\": {"
                    , "        \"name\": { \"type\": \"string\" }"
                    , "      }"
                    , "    }"
                    , "  }"
                    , "}"
                    ]
            let mComponents = decode (encodeUtf8 json) :: Maybe OpenAPI.Components
            case mComponents of
                Nothing -> assertFailure "Failed to parse components"
                Just comps -> do
                    case OpenAPI.componentsSchemas comps of
                        Nothing -> assertFailure "Schemas not found"
                        Just schemas -> do
                            Map.size schemas @?= 1
                            case Map.lookup "Pet" schemas of
                                Nothing -> assertFailure "Pet schema not found"
                                Just petSchema ->
                                    OpenAPI.schemaType petSchema @?= Just "object"
        , testCase "round-trip: OpenAPISpec serialization" $ do
            let spec = OpenAPI.OpenAPISpec
                    { OpenAPI.specPaths = Map.singleton "/test" (Map.singleton "GET" testOperation)
                    , OpenAPI.specComponents = Nothing
                    }
            let json = encode spec
            let mSpec = decode json :: Maybe OpenAPI.OpenAPISpec
            mSpec @?= Just spec
        , testCase "round-trip: Parameter serialization" $ do
            let param = OpenAPI.Parameter
                    { OpenAPI.paramName = "id"
                    , OpenAPI.paramIn = OpenAPI.ParamInQuery
                    , OpenAPI.paramDescription = Just "The ID"
                    , OpenAPI.paramRequired = True
                    , OpenAPI.paramSchema = Just (OpenAPI.Schema Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                    }
            let json = encode param
            let mParam = decode json :: Maybe OpenAPI.Parameter
            mParam @?= Just param
        ]
  where
    encodeUtf8 = LBS.fromStrict . Text.encodeUtf8
    testOperation = OpenAPI.Operation
        { OpenAPI.opOperationId = Just "testOp"
        , OpenAPI.opSummary = Just "Test operation"
        , OpenAPI.opDescription = Nothing
        , OpenAPI.opParameters = []
        , OpenAPI.opRequestBody = Nothing
        }

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
                        case chain of
                            (first:_) -> Context.callAgentSlug first @?= "root"
                            [] -> assertFailure "Expected non-empty chain"
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

-------------------------------------------------------------------------------
-- AgentConfigGraph Tests
-------------------------------------------------------------------------------

agentConfigGraphTests :: TestTree
agentConfigGraphTests =
    testGroup
        "AgentConfigGraph"
        [ testCase "empty graph" $ do
            let graph = AgentTree.AgentConfigGraph Map.empty Map.empty
            Map.null (AgentTree.graphNodes graph) @?= True
            Map.null (AgentTree.graphEdges graph) @?= True
        , testCase "graph with single node" $ do
            let agent = mkTestAgent "root-agent"
            let node = AgentTree.AgentConfigNode
                    { AgentTree.nodeFile = "/agents/root.json"
                    , AgentTree.nodeConfig = agent
                    , AgentTree.nodeChildren = []
                    , AgentTree.nodeExtraRefs = []
                    }
            let nodes = Map.fromList [("root-agent", node)]
            let edges = Map.fromList [("root-agent", [])]
            let graph = AgentTree.AgentConfigGraph nodes edges

            Map.size (AgentTree.graphNodes graph) @?= 1
            Map.size (AgentTree.graphEdges graph) @?= 1
        , testCase "graph with parent and child" $ do
            let parentAgent = mkTestAgent "parent"
            let childAgent = mkTestAgent "child"

            let childNode = AgentTree.AgentConfigNode
                    { AgentTree.nodeFile = "/agents/tools/child.json"
                    , AgentTree.nodeConfig = childAgent
                    , AgentTree.nodeChildren = []
                    , AgentTree.nodeExtraRefs = []
                    }

            let parentNode = AgentTree.AgentConfigNode
                    { AgentTree.nodeFile = "/agents/parent.json"
                    , AgentTree.nodeConfig = parentAgent
                    , AgentTree.nodeChildren = ["child"]
                    , AgentTree.nodeExtraRefs = []
                    }

            let nodes = Map.fromList [("parent", parentNode), ("child", childNode)]
            let edges = Map.fromList [("parent", ["child"]), ("child", [])]
            let graph = AgentTree.AgentConfigGraph nodes edges

            Map.size (AgentTree.graphNodes graph) @?= 2
            AgentTree.nodeChildren parentNode @?= ["child"]
        , testCase "graph with extra refs" $ do
            let mainAgent = mkTestAgentWithExtra "main-agent" ["helper-a", "helper-b"]
            let helperA = mkTestAgent "helper-a"
            let helperB = mkTestAgent "helper-b"

            let mainNode = AgentTree.AgentConfigNode
                    { AgentTree.nodeFile = "/agents/main.json"
                    , AgentTree.nodeConfig = mainAgent
                    , AgentTree.nodeChildren = []
                    , AgentTree.nodeExtraRefs = ["helper-a", "helper-b"]
                    }

            let nodeA = AgentTree.AgentConfigNode
                    { AgentTree.nodeFile = "/agents/helper-a.json"
                    , AgentTree.nodeConfig = helperA
                    , AgentTree.nodeChildren = []
                    , AgentTree.nodeExtraRefs = []
                    }

            let nodeB = AgentTree.AgentConfigNode
                    { AgentTree.nodeFile = "/agents/helper-b.json"
                    , AgentTree.nodeConfig = helperB
                    , AgentTree.nodeChildren = []
                    , AgentTree.nodeExtraRefs = []
                    }

            let nodes = Map.fromList [("main-agent", mainNode), ("helper-a", nodeA), ("helper-b", nodeB)]
            let graph = AgentTree.AgentConfigGraph nodes (Map.map (\n -> AgentTree.nodeChildren n ++ AgentTree.nodeExtraRefs n) nodes)

            AgentTree.nodeExtraRefs mainNode @?= ["helper-a", "helper-b"]
            Map.size (AgentTree.graphNodes graph) @?= 3
        ]
  where
    mkTestAgent slug = Base.Agent
        { Base.slug = slug
        , Base.apiKeyId = "test-key"
        , Base.flavor = "openai"
        , Base.modelUrl = "https://api.openai.com/v1"
        , Base.modelName = "gpt-4"
        , Base.announce = "Test agent " <> slug
        , Base.systemPrompt = ["You are helpful"]
        , Base.toolDirectory = "tools"
        , Base.mcpServers = Nothing
        , Base.extraAgents = Nothing
        }

    mkTestAgentWithExtra slug extras = Base.Agent
        { Base.slug = slug
        , Base.apiKeyId = "test-key"
        , Base.flavor = "openai"
        , Base.modelUrl = "https://api.openai.com/v1"
        , Base.modelName = "gpt-4"
        , Base.announce = "Test agent " <> slug
        , Base.systemPrompt = ["You are helpful"]
        , Base.toolDirectory = "tools"
        , Base.mcpServers = Nothing
        , Base.extraAgents = Just [Base.ExtraAgentRef e (Text.unpack e <> ".json") | e <- extras]
        }

-------------------------------------------------------------------------------
-- Reference Validation Tests
-------------------------------------------------------------------------------

referenceValidationTests :: TestTree
referenceValidationTests =
    testGroup
        "Reference Validation"
        [ testCase "valid references pass validation" $ do
            let graph = mkValidGraph
            case AgentTree.validateReferences graph of
                Left errs -> assertFailure $ "Expected validation to pass, got: " ++ show errs
                Right () -> pure ()
        , testCase "detects missing reference" $ do
            let graph = mkGraphWithMissingRef
            case AgentTree.validateReferences graph of
                Left errs -> do
                    -- Should have at least one MissingAgentReference error
                    let isMissingRef err = case err of
                            AgentTree.ReferenceError (AgentTree.MissingAgentReference _ _ _) -> True
                            _ -> False
                    assertBool "Expected MissingAgentReference error" (any isMissingRef errs)
                Right () -> assertFailure "Expected validation to fail due to missing reference"
        , testCase "detects duplicate slug" $ do
            -- Note: Duplicate detection happens during discovery, not validation
            -- But we test the ReferenceError type here
            let err = AgentTree.DuplicateAgentSlug "duplicate-slug" ["/a.json", "/b.json"]
            case err of
                AgentTree.DuplicateAgentSlug slug files -> do
                    slug @?= "duplicate-slug"
                    length files @?= 2
                _ -> assertFailure "Expected DuplicateAgentSlug"
        , testCase "MissingAgentReference contains correct info" $ do
            let err = AgentTree.MissingAgentReference "referrer" "/referrer.json" "missing"
            case err of
                AgentTree.MissingAgentReference from file missing -> do
                    from @?= "referrer"
                    file @?= "/referrer.json"
                    missing @?= "missing"
        ]
  where
    mkValidGraph =
        let agent1 = Base.Agent
                { Base.slug = "agent-1"
                , Base.apiKeyId = "test-key"
                , Base.flavor = "openai"
                , Base.modelUrl = "https://api.openai.com/v1"
                , Base.modelName = "gpt-4"
                , Base.announce = "Agent 1"
                , Base.systemPrompt = ["Helpful"]
                , Base.toolDirectory = "tools"
                , Base.mcpServers = Nothing
                , Base.extraAgents = Nothing
                }
            agent2 = Base.Agent
                { Base.slug = "agent-2"
                , Base.apiKeyId = "test-key"
                , Base.flavor = "openai"
                , Base.modelUrl = "https://api.openai.com/v1"
                , Base.modelName = "gpt-4"
                , Base.announce = "Agent 2"
                , Base.systemPrompt = ["Helpful"]
                , Base.toolDirectory = "tools"
                , Base.mcpServers = Nothing
                , Base.extraAgents = Just [Base.ExtraAgentRef "agent-1" "/agent-1.json"]
                }

            node1 = AgentTree.AgentConfigNode
                { AgentTree.nodeFile = "/agent-1.json"
                , AgentTree.nodeConfig = agent1
                , AgentTree.nodeChildren = []
                , AgentTree.nodeExtraRefs = []
                }
            node2 = AgentTree.AgentConfigNode
                { AgentTree.nodeFile = "/agent-2.json"
                , AgentTree.nodeConfig = agent2
                , AgentTree.nodeChildren = []
                , AgentTree.nodeExtraRefs = ["agent-1"]
                }

            nodes = Map.fromList [("agent-1", node1), ("agent-2", node2)]
            edges = Map.fromList [("agent-1", []), ("agent-2", ["agent-1"])]
        in AgentTree.AgentConfigGraph nodes edges

    mkGraphWithMissingRef =
        let agent = Base.Agent
                { Base.slug = "agent-with-bad-ref"
                , Base.apiKeyId = "test-key"
                , Base.flavor = "openai"
                , Base.modelUrl = "https://api.openai.com/v1"
                , Base.modelName = "gpt-4"
                , Base.announce = "Agent with bad ref"
                , Base.systemPrompt = ["Helpful"]
                , Base.toolDirectory = "tools"
                , Base.mcpServers = Nothing
                , Base.extraAgents = Just [Base.ExtraAgentRef "nonexistent" "/nonexistent.json"]
                }

            node = AgentTree.AgentConfigNode
                { AgentTree.nodeFile = "/agent.json"
                , AgentTree.nodeConfig = agent
                , AgentTree.nodeChildren = []
                , AgentTree.nodeExtraRefs = ["nonexistent"]
                }

            nodes = Map.fromList [("agent-with-bad-ref", node)]
            edges = Map.fromList [("agent-with-bad-ref", ["nonexistent"])]
        in AgentTree.AgentConfigGraph nodes edges

-------------------------------------------------------------------------------
-- Cycle Detection Tests
-------------------------------------------------------------------------------

cycleDetectionTests :: TestTree
cycleDetectionTests =
    testGroup
        "Cycle Detection"
        [ testCase "no cycles in linear chain" $ do
            let graph = mkLinearGraph
            let cycles = AgentTree.detectCycles graph
            cycles @?= []
        , testCase "detects self-reference cycle" $ do
            let graph = mkSelfRefGraph
            let cycles = AgentTree.detectCycles graph
            assertBool "Expected at least one cycle" (not (null cycles))
        , testCase "detects mutual reference cycle" $ do
            let graph = mkMutualRefGraph
            let cycles = AgentTree.detectCycles graph
            assertBool "Expected at least one cycle" (not (null cycles))
        , testCase "detects cycle in larger graph" $ do
            let graph = mkComplexGraphWithCycle
            let cycles = AgentTree.detectCycles graph
            assertBool "Expected at least one cycle" (not (null cycles))
        , testCase "no false positives in tree structure" $ do
            let graph = mkTreeGraph
            let cycles = AgentTree.detectCycles graph
            cycles @?= []
        ]
  where
    mkLinearGraph =
        -- a -> b -> c (no cycle)
        let nodes = Map.fromList
                [ ("a", mkNode "a" ["b"] [])
                , ("b", mkNode "b" ["c"] [])
                , ("c", mkNode "c" [] [])
                ]
            edges = Map.fromList [("a", ["b"]), ("b", ["c"]), ("c", [])]
        in AgentTree.AgentConfigGraph nodes edges

    mkSelfRefGraph =
        -- a -> a (self-reference cycle)
        let nodes = Map.fromList [("a", mkNode "a" ["a"] [])]
            edges = Map.fromList [("a", ["a"])]
        in AgentTree.AgentConfigGraph nodes edges

    mkMutualRefGraph =
        -- a <-> b (mutual reference)
        let nodes = Map.fromList
                [ ("a", mkNode "a" [] ["b"])
                , ("b", mkNode "b" [] ["a"])
                ]
            edges = Map.fromList [("a", ["b"]), ("b", ["a"])]
        in AgentTree.AgentConfigGraph nodes edges

    mkComplexGraphWithCycle =
        -- a -> b -> c -> a (cycle)
        -- plus d -> e (no cycle)
        let nodes = Map.fromList
                [ ("a", mkNode "a" ["b"] [])
                , ("b", mkNode "b" ["c"] [])
                , ("c", mkNode "c" [] ["a"])  -- closes the cycle
                , ("d", mkNode "d" ["e"] [])
                , ("e", mkNode "e" [] [])
                ]
            edges = Map.fromList
                [ ("a", ["b"])
                , ("b", ["c"])
                , ("c", ["a"])
                , ("d", ["e"])
                , ("e", [])
                ]
        in AgentTree.AgentConfigGraph nodes edges

    mkTreeGraph =
        --     a
        --    / \
        --   b   c
        --  /   / \
        -- d   e   f
        let nodes = Map.fromList
                [ ("a", mkNode "a" ["b", "c"] [])
                , ("b", mkNode "b" ["d"] [])
                , ("c", mkNode "c" ["e", "f"] [])
                , ("d", mkNode "d" [] [])
                , ("e", mkNode "e" [] [])
                , ("f", mkNode "f" [] [])
                ]
            edges = Map.fromList
                [ ("a", ["b", "c"])
                , ("b", ["d"])
                , ("c", ["e", "f"])
                , ("d", [])
                , ("e", [])
                , ("f", [])
                ]
        in AgentTree.AgentConfigGraph nodes edges

    mkNode slug children extras = AgentTree.AgentConfigNode
        { AgentTree.nodeFile = "/" <> Text.unpack slug <> ".json"
        , AgentTree.nodeConfig = mkAgent slug
        , AgentTree.nodeChildren = children
        , AgentTree.nodeExtraRefs = extras
        }

    mkAgent slug = Base.Agent
        { Base.slug = slug
        , Base.apiKeyId = "test-key"
        , Base.flavor = "openai"
        , Base.modelUrl = "https://api.openai.com/v1"
        , Base.modelName = "gpt-4"
        , Base.announce = "Agent " <> slug
        , Base.systemPrompt = ["Helpful"]
        , Base.toolDirectory = "tools"
        , Base.mcpServers = Nothing
        , Base.extraAgents = Nothing
        }

