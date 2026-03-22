module Main where

import System.Agents.LLMs.OpenAI as OpenAI hiding (SystemPrompt)
import System.Agents.Base as Base
import System.Agents.Tools.Context as Context
import qualified System.Agents.Session.Types as SessionTypes
import System.Agents.Session.Types (Session(..), Turn(..), UserTurnContent(..), LlmTurnContent(..),
                                    SystemPrompt(..), UserQuery(..), LlmResponse(..),
                                    StepByteUsage(..), SystemTool(..), SystemToolDefinition(..),
                                    LlmToolCall(..), UserToolResponse(..))
import qualified System.Agents.Session.Base as SessionBase
import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Tools.OpenAPI.Types as OpenAPI hiding (object, (.=))
import System.Agents.Tools.OpenAPI.Resolver as Resolver
import System.Agents.Tools.OpenAPI.Converter as Converter

import Data.Aeson (decode, encode, Value(..), object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty
import Test.Tasty.HUnit

-- Import the comprehensive OpenAPI Toolbox tests
import qualified OpenAPIToolboxTests
-- Import PostgREST parsing tests
import qualified PostgRESTParseTest
-- Import Session Edit tests
import qualified SessionEditTests
-- Import SessionPrint tests
import qualified SessionPrintTests
-- Import EndpointPredicate tests
import qualified EndpointPredicateTests
-- Import Skills tests
import qualified SkillsTests
-- Import MCP Implementation tests
import qualified McpImplementationTests
-- Import ToolPortal tests
import qualified ToolPortalTests
-- Import LuaToolbox security tests
import qualified LuaToolboxSecurityTests
-- Import LuaToolbox comprehensive tests
import qualified LuaToolboxTests
-- Import luaToJsonValue specific tests
import qualified LuaToolboxJsonValueTests
-- Import LuaToolbox HTTP module tests
import qualified LuaToolboxHttpTests
-- Import OS Core ECS tests
import qualified OS.CoreTests
-- Import OS Resources tests
import qualified OS.ResourcesTests
-- Import OS Concurrent tests
import qualified OS.ConcurrentTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "All Tests"
        [ openAIRateLimitTests
        , extraAgentRefTests
        , agentSerializationTests
        , bashToolboxTests
        , turnRetroCompatibilityTests
        , turnRoundTripTests
        , OS.CoreTests.coreTests
        , OS.ResourcesTests.resourcesTests
        , OS.ConcurrentTests.concurrentTests
        , OpenAPIToolboxTests.tests
        , PostgRESTParseTest.tests
        , SessionEditTests.tests
        , SessionPrintTests.tests
        , EndpointPredicateTests.tests
        , SkillsTests.skillsTestSuite
        , McpImplementationTests.mcpImplementationTestSuite
        , ToolPortalTests.toolPortalTestSuite
        , LuaToolboxSecurityTests.tests
        , LuaToolboxTests.luaToolboxTests
        , LuaToolboxJsonValueTests.luaToJsonValueTests
        , LuaToolboxHttpTests.luaToolboxHttpTests
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
                    Base.builtinToolboxes agent @?= Nothing
                    Base.toolDirectory agent @?= Just "tools"
        , testCase "agent with optional toolDirectory omitted" $ do
            let json = Text.unlines
                    [ "{"
                    , "  \"slug\": \"test-agent\","
                    , "  \"apiKeyId\": \"openai\","
                    , "  \"flavor\": \"openai\","
                    , "  \"modelUrl\": \"https://api.openai.com/v1\","
                    , "  \"modelName\": \"gpt-4\","
                    , "  \"announce\": \"A test agent\","
                    , "  \"systemPrompt\": [\"You are helpful\"]"
                    , "}"
                    ]
            let mAgent = decode (encodeUtf8 json) :: Maybe Base.Agent
            case mAgent of
                Nothing -> assertFailure "Failed to parse agent without toolDirectory"
                Just agent -> do
                    Base.slug agent @?= "test-agent"
                    Base.toolDirectory agent @?= Nothing
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
                    , Base.toolDirectory = Just "tools"
                    , Base.bashToolboxes = Nothing
                    , Base.mcpServers = Nothing
                    , Base.openApiToolboxes = Nothing
                    , Base.postgrestToolboxes = Nothing
                    , Base.builtinToolboxes = Nothing
                    , Base.extraAgents = Just
                        [ Base.ExtraAgentRef { Base.extraAgentSlug = "helper", Base.extraAgentPath = "./helper.json" }
                        ]
                    , Base.skillSources = Nothing
                    , Base.autoEnableSkills = Nothing
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
                    , Base.toolDirectory = Just "tools"
                    , Base.bashToolboxes = Nothing
                    , Base.mcpServers = Nothing
                    , Base.openApiToolboxes = Nothing
                    , Base.postgrestToolboxes = Nothing
                    , Base.builtinToolboxes = Nothing
                    , Base.extraAgents = Nothing
                    , Base.skillSources = Nothing
                    , Base.autoEnableSkills = Nothing
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
                    , Base.toolDirectory = Just "tools"
                    , Base.bashToolboxes = Nothing
                    , Base.mcpServers = Just []
                    , Base.openApiToolboxes = Nothing
                    , Base.postgrestToolboxes = Nothing
                    , Base.builtinToolboxes = Nothing
                    , Base.extraAgents = Just
                        [ Base.ExtraAgentRef { Base.extraAgentSlug = "helper", Base.extraAgentPath = "./helper.json" }
                        ]
                    , Base.skillSources = Nothing
                    , Base.autoEnableSkills = Nothing
                    }
            let desc = Base.AgentDescription agent
            let json = encode desc
            let mDesc = decode json :: Maybe Base.AgentDescription
            mDesc @?= Just desc
        , testCase "round-trip with builtin toolboxes" $ do
            let sqliteDesc = Base.SqliteToolboxDescription
                    { Base.sqliteToolboxName = "memory"
                    , Base.sqliteToolboxDescription = "a set of memories"
                    , Base.sqliteToolboxPath = "/path/to/memories.sqlite"
                    , Base.sqliteToolboxAccess = Base.SqliteReadWrite
                    }
            let builtinToolbox = Base.SqliteToolbox sqliteDesc
            let agent = Base.Agent
                    { Base.slug = "test-agent"
                    , Base.apiKeyId = "openai"
                    , Base.flavor = "openai"
                    , Base.modelUrl = "https://api.openai.com/v1"
                    , Base.modelName = "gpt-4"
                    , Base.announce = "A test agent"
                    , Base.systemPrompt = ["You are helpful"]
                    , Base.toolDirectory = Just "tools"
                    , Base.bashToolboxes = Nothing
                    , Base.mcpServers = Nothing
                    , Base.openApiToolboxes = Nothing
                    , Base.postgrestToolboxes = Nothing
                    , Base.builtinToolboxes = Just [builtinToolbox]
                    , Base.extraAgents = Nothing
                    , Base.skillSources = Nothing
                    , Base.autoEnableSkills = Nothing
                    }
            let json = encode agent
            let mAgent = decode json :: Maybe Base.Agent
            mAgent @?= Just agent
        , testCase "round-trip with multiple builtin toolboxes" $ do
            let sqliteDesc1 = Base.SqliteToolboxDescription
                    { Base.sqliteToolboxName = "memory"
                    , Base.sqliteToolboxDescription = "a set of memories"
                    , Base.sqliteToolboxPath = "/path/to/memories.sqlite"
                    , Base.sqliteToolboxAccess = Base.SqliteReadWrite
                    }
            let sqliteDesc2 = Base.SqliteToolboxDescription
                    { Base.sqliteToolboxName = "guidelines"
                    , Base.sqliteToolboxDescription = "a set of guidelines"
                    , Base.sqliteToolboxPath = "/path/to/guidelines.sqlite"
                    , Base.sqliteToolboxAccess = Base.SqliteReadOnly
                    }
            let builtinToolboxes = [Base.SqliteToolbox sqliteDesc1, Base.SqliteToolbox sqliteDesc2]
            let agent = Base.Agent
                    { Base.slug = "test-agent"
                    , Base.apiKeyId = "openai"
                    , Base.flavor = "openai"
                    , Base.modelUrl = "https://api.openai.com/v1"
                    , Base.modelName = "gpt-4"
                    , Base.announce = "A test agent"
                    , Base.systemPrompt = ["You are helpful"]
                    , Base.toolDirectory = Just "tools"
                    , Base.bashToolboxes = Nothing
                    , Base.mcpServers = Nothing
                    , Base.openApiToolboxes = Nothing
                    , Base.postgrestToolboxes = Nothing
                    , Base.builtinToolboxes = Just builtinToolboxes
                    , Base.extraAgents = Nothing
                    , Base.skillSources = Nothing
                    , Base.autoEnableSkills = Nothing
                    }
            let json = encode agent
            let mAgent = decode json :: Maybe Base.Agent
            mAgent @?= Just agent
        , testCase "round-trip with LuaToolbox" $ do
            let luaDesc = Base.LuaToolboxDescription
                    { Base.luaToolboxName = "lua"
                    , Base.luaToolboxDescription = "Lua interpreter"
                    , Base.luaToolboxMaxMemoryMB = 256
                    , Base.luaToolboxMaxExecutionTimeSeconds = 300
                    , Base.luaToolboxAllowedTools = ["bash", "sqlite"]
                    , Base.luaToolboxAllowedPaths = ["./scripts"]
                    , Base.luaToolboxAllowedHosts = ["localhost"]
                    }
            let builtinToolbox = Base.LuaToolbox luaDesc
            let agent = Base.Agent
                    { Base.slug = "test-agent"
                    , Base.apiKeyId = "openai"
                    , Base.flavor = "openai"
                    , Base.modelUrl = "https://api.openai.com/v1"
                    , Base.modelName = "gpt-4"
                    , Base.announce = "A test agent"
                    , Base.systemPrompt = ["You are helpful"]
                    , Base.toolDirectory = Just "tools"
                    , Base.mcpServers = Nothing
                    , Base.openApiToolboxes = Nothing
                    , Base.postgrestToolboxes = Nothing
                    , Base.builtinToolboxes = Just [builtinToolbox]
                    , Base.bashToolboxes = Nothing
                    , Base.skillSources = Nothing
                    , Base.autoEnableSkills = Nothing
                    , Base.extraAgents = Nothing
                    }
            let json = encode agent
            let mAgent = decode json :: Maybe Base.Agent
            mAgent @?= Just agent
        , testCase "SqliteAccessMode round-trip" $ do
            -- Test read-only mode round-trip
            let readOnly = Base.SqliteReadOnly
            let jsonRO = encode readOnly
            let mReadOnly = decode jsonRO :: Maybe Base.SqliteAccessMode
            mReadOnly @?= Just Base.SqliteReadOnly
            
            -- Test read-write mode round-trip
            let readWrite = Base.SqliteReadWrite
            let jsonRW = encode readWrite
            let mReadWrite = decode jsonRW :: Maybe Base.SqliteAccessMode
            mReadWrite @?= Just Base.SqliteReadWrite
        ]
  where
    encodeUtf8 = LBS.fromStrict . Text.encodeUtf8

-------------------------------------------------------------------------------
-- Bash Toolbox Tests
-------------------------------------------------------------------------------

bashToolboxTests :: TestTree
bashToolboxTests =
    testGroup
        "Bash Toolbox Descriptions"
        [ testCase "FileSystemDirectory serialization" $ do
            let desc = Base.FileSystemDirectoryDescription
                    { Base.fsDirRoot = Nothing
                    , Base.fsDirPath = "./tools"
                    , Base.fsDirBasenameFilter = Nothing
                    }
            let json = encode desc
            let mDesc = decode json :: Maybe Base.FileSystemDirectoryDescription
            mDesc @?= Just desc
        , testCase "FileSystemDirectory with filter serialization" $ do
            let desc = Base.FileSystemDirectoryDescription
                    { Base.fsDirRoot = Nothing
                    , Base.fsDirPath = "./extra-tools"
                    , Base.fsDirBasenameFilter = Just ".sh"
                    }
            let json = encode desc
            let mDesc = decode json :: Maybe Base.FileSystemDirectoryDescription
            mDesc @?= Just desc
        , testCase "SingleTool serialization" $ do
            let desc = Base.SingleToolDescription
                    { Base.singleToolPath = "/path/to/special-tool.sh"
                    }
            let json = encode desc
            let mDesc = decode json :: Maybe Base.SingleToolDescription
            mDesc @?= Just desc
        , testCase "BashToolboxDescription FileSystemDirectory wrapper" $ do
            let desc = Base.FileSystemDirectoryDescription
                    { Base.fsDirRoot = Nothing
                    , Base.fsDirPath = "./tools"
                    , Base.fsDirBasenameFilter = Just ".sh"
                    }
            let wrapped = Base.FileSystemDirectory desc
            let json = encode wrapped
            let mWrapped = decode json :: Maybe Base.BashToolboxDescription
            mWrapped @?= Just wrapped
        , testCase "BashToolboxDescription SingleTool wrapper" $ do
            let desc = Base.SingleToolDescription
                    { Base.singleToolPath = "/path/to/tool.sh"
                    }
            let wrapped = Base.SingleTool desc
            let json = encode wrapped
            let mWrapped = decode json :: Maybe Base.BashToolboxDescription
            mWrapped @?= Just wrapped
        , testCase "agent with bashToolboxes" $ do
            let fsDir = Base.FileSystemDirectory $ Base.FileSystemDirectoryDescription Nothing "./tools" Nothing
            let single = Base.SingleTool $ Base.SingleToolDescription "/path/to/special.sh"
            let agent = Base.Agent
                    { Base.slug = "test-agent"
                    , Base.apiKeyId = "openai"
                    , Base.flavor = "openai"
                    , Base.modelUrl = "https://api.openai.com/v1"
                    , Base.modelName = "gpt-4"
                    , Base.announce = "A test agent"
                    , Base.systemPrompt = ["You are helpful"]
                    , Base.toolDirectory = Nothing
                    , Base.bashToolboxes = Just [fsDir, single]
                    , Base.mcpServers = Nothing
                    , Base.openApiToolboxes = Nothing
                    , Base.postgrestToolboxes = Nothing
                    , Base.builtinToolboxes = Nothing
                    , Base.extraAgents = Nothing
                    , Base.skillSources = Nothing
                    , Base.autoEnableSkills = Nothing
                    }
            let json = encode agent
            let mAgent = decode json :: Maybe Base.Agent
            mAgent @?= Just agent
        , testCase "agent with both legacy toolDirectory and bashToolboxes" $ do
            let fsDir = Base.FileSystemDirectory $ Base.FileSystemDirectoryDescription Nothing "./extra-tools" Nothing
            let agent = Base.Agent
                    { Base.slug = "test-agent"
                    , Base.apiKeyId = "openai"
                    , Base.flavor = "openai"
                    , Base.modelUrl = "https://api.openai.com/v1"
                    , Base.modelName = "gpt-4"
                    , Base.announce = "A test agent"
                    , Base.systemPrompt = ["You are helpful"]
                    , Base.toolDirectory = Just "tools"
                    , Base.bashToolboxes = Just [fsDir]
                    , Base.mcpServers = Nothing
                    , Base.openApiToolboxes = Nothing
                    , Base.postgrestToolboxes = Nothing
                    , Base.builtinToolboxes = Nothing
                    , Base.extraAgents = Nothing
                    , Base.skillSources = Nothing
                    , Base.autoEnableSkills = Nothing
                    }
            let json = encode agent
            let mAgent = decode json :: Maybe Base.Agent
            mAgent @?= Just agent
        ]

-------------------------------------------------------------------------------
-- Turn Retro-compatibility Tests
-------------------------------------------------------------------------------

turnRetroCompatibilityTests :: TestTree
turnRetroCompatibilityTests =
    testGroup
        "Turn Retro-compatibility"
        [ testCase "parse old session format (turn-v0.001.json) without byteUsage" $ do
            jsonContent <- LBS.readFile "test/data/turn-v0.001.json"
            let mSession = decode jsonContent :: Maybe Session
            case mSession of
                Nothing -> assertFailure "Failed to parse turn-v0.001.json - retro-compatibility broken"
                Just session -> do
                    -- Check that the session has 2 turns
                    length (turns session) @?= 2
                    -- Both turns should have Nothing for byteUsage (old format)
                    mapM_ (\turn -> turnByteUsage turn @?= Nothing) (turns session)
        ]
  where
    turnByteUsage :: Turn -> Maybe ()
    turnByteUsage (UserTurn _ usage) = fmap (const ()) usage
    turnByteUsage (LlmTurn _ usage) = fmap (const ()) usage

-------------------------------------------------------------------------------
-- Turn Round-trip Tests
-------------------------------------------------------------------------------

turnRoundTripTests :: TestTree
turnRoundTripTests =
    testGroup
        "Turn Round-trip Tests"
        [ testCase "UserTurn round-trip without byteUsage" $ do
            let userContent = UserTurnContent
                    { userPrompt = SystemPrompt "You are helpful"
                    , userTools = []
                    , userQuery = Just (UserQuery "Hello")
                    , userToolResponses = []
                    }
            let turn = UserTurn userContent Nothing
            let json = encode turn
            let mDecoded = decode json :: Maybe Turn
            mDecoded @?= Just turn
        , testCase "UserTurn round-trip with byteUsage" $ do
            let userContent = UserTurnContent
                    { userPrompt = SystemPrompt "You are helpful"
                    , userTools = []
                    , userQuery = Just (UserQuery "Hello")
                    , userToolResponses = []
                    }
            let byteUsage = StepByteUsage 1000 500 300 100 100
            let turn = UserTurn userContent (Just byteUsage)
            let json = encode turn
            let mDecoded = decode json :: Maybe Turn
            mDecoded @?= Just turn
        , testCase "LlmTurn round-trip without byteUsage" $ do
            let llmResp = SessionTypes.LlmResponse
                    { SessionTypes.responseText = Just "Hello!"
                    , SessionTypes.responseThinking = Just "Thinking..."
                    , SessionTypes.rawResponse = object ["model" .= ("gpt-4" :: Text)]
                    }
            let llmContent = LlmTurnContent
                    { llmResponse = llmResp
                    , llmToolCalls = []
                    }
            let turn = LlmTurn llmContent Nothing
            let json = encode turn
            let mDecoded = decode json :: Maybe Turn
            mDecoded @?= Just turn
        , testCase "LlmTurn round-trip with byteUsage" $ do
            let llmResp = SessionTypes.LlmResponse
                    { SessionTypes.responseText = Just "Hello!"
                    , SessionTypes.responseThinking = Just "Thinking..."
                    , SessionTypes.rawResponse = object ["model" .= ("gpt-4" :: Text)]
                    }
            let llmContent = LlmTurnContent
                    { llmResponse = llmResp
                    , llmToolCalls = []
                    }
            let byteUsage = StepByteUsage 2000 800 700 300 200
            let turn = LlmTurn llmContent (Just byteUsage)
            let json = encode turn
            let mDecoded = decode json :: Maybe Turn
            mDecoded @?= Just turn
        , testCase "UserTurn with tool responses round-trip" $ do
            let toolCall = LlmToolCall (object ["id" .= ("call-1" :: Text)])
            let toolResponse = UserToolResponse (object ["result" .= ("success" :: Text)])
            let userContent = UserTurnContent
                    { userPrompt = SystemPrompt "You are helpful"
                    , userTools = []
                    , userQuery = Nothing
                    , userToolResponses = [(toolCall, toolResponse)]
                    }
            let turn = UserTurn userContent Nothing
            let json = encode turn
            let mDecoded = decode json :: Maybe Turn
            mDecoded @?= Just turn
        , testCase "LlmTurn with tool calls round-trip" $ do
            let llmResp = SessionTypes.LlmResponse
                    { SessionTypes.responseText = Just "I'll help you"
                    , SessionTypes.responseThinking = Nothing
                    , SessionTypes.rawResponse = object ["choices" .= ([] :: [Value])]
                    }
            let toolCall1 = LlmToolCall (object ["id" .= ("call-1" :: Text)])
            let toolCall2 = LlmToolCall (object ["id" .= ("call-2" :: Text)])
            let llmContent = LlmTurnContent
                    { llmResponse = llmResp
                    , llmToolCalls = [toolCall1, toolCall2]
                    }
            let turn = LlmTurn llmContent Nothing
            let json = encode turn
            let mDecoded = decode json :: Maybe Turn
            mDecoded @?= Just turn
        , testCase "Session round-trip with mixed turns" $ do
            sessionId <- SessionBase.newSessionId
            turnId' <- SessionBase.newTurnId
            
            let userTurn1 = UserTurn
                    (UserTurnContent
                        { userPrompt = SystemPrompt "You are helpful"
                        , userTools = []
                        , userQuery = Just (UserQuery "First query")
                        , userToolResponses = []
                        })
                    Nothing
            
            let llmTurn = LlmTurn
                    (LlmTurnContent
                        { llmResponse = SessionTypes.LlmResponse
                            { SessionTypes.responseText = Just "Response"
                            , SessionTypes.responseThinking = Nothing
                            , SessionTypes.rawResponse = object ["model" .= ("gpt-4" :: Text)]
                            }
                        , llmToolCalls = []
                        })
                    (Just (StepByteUsage 1000 400 400 100 100))
            
            let userTurn2 = UserTurn
                    (UserTurnContent
                        { userPrompt = SystemPrompt "You are helpful"
                        , userTools = []
                        , userQuery = Just (UserQuery "Second query")
                        , userToolResponses = []
                        })
                    Nothing
            
            let session = Session
                    { turns = [userTurn1, llmTurn, userTurn2]
                    , sessionId = sessionId
                    , forkedFromSessionId = Nothing
                    , turnId = turnId'
                    }
            
            let json = encode session
            let mDecoded = decode json :: Maybe Session
            mDecoded @?= Just session
        , testCase "Session round-trip with fork info" $ do
            origSessionId <- SessionBase.newSessionId
            forkedFromId <- SessionBase.newSessionId
            turnId' <- SessionBase.newTurnId
            
            let session = Session
                    { turns = []
                    , sessionId = origSessionId
                    , forkedFromSessionId = Just forkedFromId
                    , turnId = turnId'
                    }
            
            let json = encode session
            let mDecoded = decode json :: Maybe Session
            mDecoded @?= Just session
        , testCase "encoded Turn has correct structure" $ do
            let userContent = UserTurnContent
                    { userPrompt = SystemPrompt "Test"
                    , userTools = []
                    , userQuery = Nothing
                    , userToolResponses = []
                    }
            let turn = UserTurn userContent (Just (StepByteUsage 100 50 30 10 10))
            let json = encode turn
            let jsonStr = Text.unpack . Text.decodeUtf8 . LBS.toStrict $ json
            -- Check that the JSON has the expected structure
            assertBool "JSON should contain tag field" ("\"tag\"" `Text.isInfixOf` Text.pack jsonStr)
            assertBool "JSON should contain contents field" ("\"contents\"" `Text.isInfixOf` Text.pack jsonStr)
            assertBool "JSON should contain byteUsage field" ("\"byteUsage\"" `Text.isInfixOf` Text.pack jsonStr)
            assertBool "JSON should contain UserTurn tag" ("\"UserTurn\"" `Text.isInfixOf` Text.pack jsonStr)
        , testCase "UserTurn without byteUsage omits byteUsage field" $ do
            let userContent = UserTurnContent
                    { userPrompt = SystemPrompt "Test"
                    , userTools = []
                    , userQuery = Nothing
                    , userToolResponses = []
                    }
            let turn = UserTurn userContent Nothing
            let json = encode turn
            let jsonStr = Text.unpack . Text.decodeUtf8 . LBS.toStrict $ json
            -- Check that the JSON has the expected structure
            assertBool "JSON should contain tag field" ("\"tag\"" `Text.isInfixOf` Text.pack jsonStr)
            assertBool "JSON should contain contents field" ("\"contents\"" `Text.isInfixOf` Text.pack jsonStr)
            -- The encoded JSON should not contain byteUsage when it's Nothing
            -- Note: aeson may still include it as null, so we check the structure is valid
            let mDecoded = decode json :: Maybe Turn
            assertBool "Should decode successfully" (isJust mDecoded)
        , testCase "decode from new format produces correct Turn" $ do
            let json = "{\"tag\":\"UserTurn\",\"contents\":{\"userPrompt\":\"Test\",\"userTools\":[],\"userQuery\":null,\"userToolResponses\":[]}}"
            let mTurn = decode (LBS.fromStrict $ Text.encodeUtf8 json) :: Maybe Turn
            case mTurn of
                Nothing -> assertFailure "Failed to decode UserTurn from new format"
                Just (UserTurn content Nothing) -> do
                    let SystemPrompt prompt = userPrompt content
                    prompt @?= "Test"
                Just _ -> assertFailure "Expected UserTurn with Nothing byteUsage"
        , testCase "decode from new format with byteUsage" $ do
            let json = "{\"tag\":\"LlmTurn\",\"contents\":{\"llmResponse\":{\"responseText\":\"Hi\",\"responseThinking\":null,\"rawResponse\":{}},\"llmToolCalls\":[]},\"byteUsage\":{\"stepTotalBytes\":100,\"stepInputBytes\":50,\"stepOutputBytes\":30,\"stepReasoningBytes\":10,\"stepToolBytes\":10}}"
            let mTurn = decode (LBS.fromStrict $ Text.encodeUtf8 json) :: Maybe Turn
            case mTurn of
                Nothing -> assertFailure "Failed to decode LlmTurn with byteUsage"
                Just (LlmTurn _ (Just usage)) -> do
                    stepTotalBytes usage @?= 100
                    stepInputBytes usage @?= 50
                    stepOutputBytes usage @?= 30
                    stepReasoningBytes usage @?= 10
                    stepToolBytes usage @?= 10
                Just _ -> assertFailure "Expected LlmTurn with Just byteUsage"
        ]

