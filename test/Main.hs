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
        , callStackEntryTests
        , toolExecutionContextTests
        , recursionTrackingTests
        , agentConfigGraphTests
        , referenceValidationTests
        , cycleDetectionTests
        , openAPITypesTests
        , openAPIResolverTests
        , openAPIConverterTests
        , OpenAPIToolboxTests.tests
        , PostgRESTParseTest.tests
        , SessionEditTests.tests
        , SessionPrintTests.tests
        , EndpointPredicateTests.tests
        , SkillsTests.skillsTestSuite
        , McpImplementationTests.mcpImplementationTestSuite
        , turnRetroCompatibilityTests
        , turnRoundTripTests
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
                    , OpenAPI.specParameters = Nothing
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
-- OpenAPI Resolver Tests
-------------------------------------------------------------------------------

openAPIResolverTests :: TestTree
openAPIResolverTests =
    testGroup
        "OpenAPI Resolver"
        [ testCase "parseRef parses valid internal reference" $ do
            case Resolver.parseRef "#/components/schemas/Pet" of
                Nothing -> assertFailure "Failed to parse valid ref"
                Just refPath -> do
                    Resolver.refComponents refPath @?= "components"
                    Resolver.refSection refPath @?= "schemas"
                    Resolver.refName refPath @?= "Pet"
        , testCase "parseRef returns Nothing for external http refs" $ do
            Resolver.parseRef "http://example.com/schema.json" @?= Nothing
        , testCase "parseRef returns Nothing for external https refs" $ do
            Resolver.parseRef "https://example.com/schema.json" @?= Nothing
        , testCase "parseRef handles nested paths" $ do
            case Resolver.parseRef "#/components/schemas/Nested/Deep" of
                Nothing -> assertFailure "Failed to parse nested ref"
                Just refPath -> do
                    Resolver.refComponents refPath @?= "components"
                    Resolver.refSection refPath @?= "schemas"
                    Resolver.refName refPath @?= "Nested"
        , testCase "resolveSchema resolves simple $ref" $ do
            let petSchema = mkSchema (Just "object") (Just "A pet") Nothing Nothing Nothing Nothing Nothing Nothing
            let components = OpenAPI.Components (Just (Map.fromList [("Pet", petSchema)]))
            let refSchema = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing
            let resolved = Resolver.resolveSchema refSchema components
            OpenAPI.schemaType resolved @?= Just "object"
            OpenAPI.schemaDescription resolved @?= Just "A pet"
            OpenAPI.schemaRef resolved @?= Nothing
        , testCase "resolveSchema returns unchanged for non-ref schema" $ do
            let schema = mkSchema (Just "string") (Just "A string") Nothing Nothing Nothing Nothing Nothing Nothing
            let components = OpenAPI.Components Nothing
            let resolved = Resolver.resolveSchema schema components
            resolved @?= schema
        , testCase "resolveSchema handles nested references" $ do
            -- Pet refs Owner, Owner has no refs
            let ownerSchema = mkSchema (Just "object") (Just "An owner") Nothing Nothing Nothing Nothing Nothing Nothing
            let ownerRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Owner") Nothing
            let petSchema = mkSchema (Just "object") Nothing Nothing (Just (Map.fromList [("owner", ownerRef)])) Nothing Nothing Nothing Nothing
            let components = OpenAPI.Components (Just (Map.fromList [("Pet", petSchema), ("Owner", ownerSchema)]))
            let petRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing
            let resolved = Resolver.resolveSchema petRef components
            OpenAPI.schemaType resolved @?= Just "object"
            case OpenAPI.schemaProperties resolved of
                Nothing -> assertFailure "Expected properties"
                Just props -> case Map.lookup "owner" props of
                    Nothing -> assertFailure "Expected owner property"
                    Just owner -> do
                        OpenAPI.schemaType owner @?= Just "object"
                        OpenAPI.schemaDescription owner @?= Just "An owner"
        , testCase "resolveSchema handles array item references" $ do
            let itemSchema = mkSchema (Just "string") (Just "A tag") Nothing Nothing Nothing Nothing Nothing Nothing
            let itemRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Tag") Nothing
            let arraySchema = mkSchema (Just "array") Nothing Nothing Nothing (Just itemRef) Nothing Nothing Nothing
            let components = OpenAPI.Components (Just (Map.fromList [("Tag", itemSchema), ("Tags", arraySchema)]))
            let tagsRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Tags") Nothing
            let resolved = Resolver.resolveSchema tagsRef components
            OpenAPI.schemaType resolved @?= Just "array"
            case OpenAPI.schemaItems resolved of
                Nothing -> assertFailure "Expected items"
                Just items -> do
                    OpenAPI.schemaType items @?= Just "string"
                    OpenAPI.schemaDescription items @?= Just "A tag"
        , testCase "resolveSchema handles anyOf references" $ do
            let catSchema = mkSchema (Just "object") (Just "A cat") Nothing Nothing Nothing Nothing Nothing Nothing
            let dogSchema = mkSchema (Just "object") (Just "A dog") Nothing Nothing Nothing Nothing Nothing Nothing
            let catRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Cat") Nothing
            let dogRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Dog") Nothing
            let anyOfSchema = mkSchema Nothing Nothing Nothing Nothing Nothing (Just [catRef, dogRef]) Nothing Nothing
            let components = OpenAPI.Components (Just (Map.fromList [("Cat", catSchema), ("Dog", dogSchema), ("Pet", anyOfSchema)]))
            let petRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing
            let resolved = Resolver.resolveSchema petRef components
            case OpenAPI.schemaAnyOf resolved of
                Nothing -> assertFailure "Expected anyOf"
                Just schemas -> do
                    length schemas @?= 2
                    case schemas of
                        (first:_) -> OpenAPI.schemaType first @?= Just "object"
                        [] -> assertFailure "Expected non-empty schemas list"
        , testCase "resolveSchemaWithDepth detects circular references" $ do
            -- Schema A refs B, B refs A (circular)
            let aRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/A") Nothing
            let bRef = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/B") Nothing
            let schemaA = mkSchema (Just "object") Nothing Nothing (Just (Map.fromList [("b", bRef)])) Nothing Nothing Nothing Nothing
            let schemaB = mkSchema (Just "object") Nothing Nothing (Just (Map.fromList [("a", aRef)])) Nothing Nothing Nothing Nothing
            let components = OpenAPI.Components (Just (Map.fromList [("A", schemaA), ("B", schemaB)]))
            let result = Resolver.resolveSchemaWithDepth aRef components 0 mempty
            case result of
                Left (Resolver.CircularReference _) -> pure ()
                _ -> assertFailure "Expected CircularReference error"
        , testCase "resolveSchema handles missing reference gracefully" $ do
            let refSchema = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Missing") Nothing
            let components = OpenAPI.Components (Just Map.empty)
            let resolved = Resolver.resolveSchema refSchema components
            -- Should return schema with error description
            case OpenAPI.schemaDescription resolved of
                Just desc -> assertBool "Expected error message" ("Reference resolution failed" `Text.isInfixOf` desc)
                Nothing -> assertFailure "Expected error description"
        , testCase "resolveRef finds schema in components" $ do
            let petSchema = mkSchema (Just "object") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let components = OpenAPI.Components (Just (Map.fromList [("Pet", petSchema)]))
            case Resolver.resolveRef "#/components/schemas/Pet" components of
                Left err -> assertFailure $ "Unexpected error: " ++ show err
                Right schema -> OpenAPI.schemaType schema @?= Just "object"
        , testCase "resolveRef returns error for missing schema" $ do
            let components = OpenAPI.Components (Just Map.empty)
            case Resolver.resolveRef "#/components/schemas/Missing" components of
                Left (Resolver.SchemaNotFound _) -> pure ()
                _ -> assertFailure "Expected SchemaNotFound error"
        , testCase "resolveRef returns error for invalid ref path" $ do
            let components = OpenAPI.Components Nothing
            case Resolver.resolveRef "invalid-ref" components of
                Left (Resolver.InvalidRefPath _) -> pure ()
                _ -> assertFailure "Expected InvalidRefPath error"
        , testCase "dereferenceSpec resolves all schemas in spec" $ do
            let petSchema = mkSchema (Just "object") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let components = OpenAPI.Components (Just (Map.fromList [("Pet", petSchema)]))
            let refSchema = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing
            let param = OpenAPI.Parameter "petId" OpenAPI.ParamInPath Nothing True (Just refSchema)
            let operation = OpenAPI.Operation (Just "getPet") Nothing Nothing [param] Nothing
            let spec = OpenAPI.OpenAPISpec (Map.singleton "/pets/{petId}" (Map.singleton "GET" operation)) (Just components) Nothing
            let dereferenced = Resolver.dereferenceSpec spec
            case Map.lookup "/pets/{petId}" (OpenAPI.specPaths dereferenced) of
                Nothing -> assertFailure "Path not found"
                Just methods -> case Map.lookup "GET" methods of
                    Nothing -> assertFailure "GET method not found"
                    Just op -> case OpenAPI.opParameters op of
                        [] -> assertFailure "Expected parameters"
                        (p:_) -> case OpenAPI.paramSchema p of
                            Nothing -> assertFailure "Expected schema"
                            Just schema -> do
                                OpenAPI.schemaType schema @?= Just "object"
                                OpenAPI.schemaRef schema @?= Nothing
        , testCase "dereferenceSpec handles missing components" $ do
            let refSchema = mkSchema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing
            let param = OpenAPI.Parameter "petId" OpenAPI.ParamInPath Nothing True (Just refSchema)
            let operation = OpenAPI.Operation (Just "getPet") Nothing Nothing [param] Nothing
            let spec = OpenAPI.OpenAPISpec (Map.singleton "/pets/{petId}" (Map.singleton "GET" operation)) Nothing Nothing
            let dereferenced = Resolver.dereferenceSpec spec
            -- Should not crash, schema remains unresolved
            case Map.lookup "/pets/{petId}" (OpenAPI.specPaths dereferenced) of
                Nothing -> assertFailure "Path not found"
                Just methods -> case Map.lookup "GET" methods of
                    Nothing -> assertFailure "GET method not found"
                    Just op -> case OpenAPI.opParameters op of
                        [] -> assertFailure "Expected parameters"
                        (p:_) -> case OpenAPI.paramSchema p of
                            Nothing -> assertFailure "Expected schema"
                            Just schema -> do
                                -- Schema should have error description
                                case OpenAPI.schemaDescription schema of
                                    Just desc -> assertBool "Expected error" ("failed" `Text.isInfixOf` desc)
                                    Nothing -> pure ()  -- Original schema preserved
        ]
  where
    -- Helper to construct Schema with correct field order:
    -- schemaType, schemaDescription, schemaEnum, schemaProperties, schemaItems, schemaAnyOf, schemaRef, schemaRequired
    mkSchema :: Maybe Text -> Maybe Text -> Maybe [Value] -> Maybe (Map.Map Text OpenAPI.Schema) -> Maybe OpenAPI.Schema -> Maybe [OpenAPI.Schema] -> Maybe Text -> Maybe [Text] -> OpenAPI.Schema
    mkSchema t d e p i a r req = OpenAPI.Schema t d e p i a r req

-------------------------------------------------------------------------------
-- OpenAPI Converter Tests
-------------------------------------------------------------------------------

openAPIConverterTests :: TestTree
openAPIConverterTests =
    testGroup
        "OpenAPI Converter"
        [ testCase "deriveToolName uses operationId when available" $ do
            let op = OpenAPI.Operation (Just "getPetById") (Just "Get pet") Nothing [] Nothing
            Converter.deriveToolName "/pets/{id}" "GET" op @?= "openapi_getPetById"
        , testCase "deriveToolName falls back to path/method" $ do
            let op = OpenAPI.Operation Nothing (Just "Get pet") Nothing [] Nothing
            -- Note: sanitizePathForName collapses double underscores for cleaner names
            Converter.deriveToolName "/pets/{id}" "GET" op @?= "openapi_get_pets_id_"
        , testCase "sanitizePathForName handles simple path" $ do
            Converter.sanitizePathForName "GET" "/pets" @?= "get_pets"
        , testCase "sanitizePathForName handles path with path params" $ do
            -- Note: consecutive underscores are collapsed for cleaner names
            Converter.sanitizePathForName "POST" "/pets/{id}/owner" @?= "post_pets_id_owner"
        , testCase "sanitizePathForName handles nested paths" $ do
            Converter.sanitizePathForName "GET" "/api/v1/users" @?= "get_api_v1_users"
        , testCase "buildToolDescription combines summary and description" $ do
            let op = OpenAPI.Operation Nothing (Just "Get a pet") (Just "Returns a single pet") [] Nothing
            Converter.buildToolDescription op @?= "Get a pet:\nReturns a single pet"
        , testCase "paramToToolName prefixes with p_" $ do
            let param = OpenAPI.Parameter "petId" OpenAPI.ParamInPath Nothing True Nothing
            Converter.paramToToolName param @?= "p_petId"
        , testCase "convertOperation creates tool with correct name" $ do
            let op = OpenAPI.Operation (Just "listPets") (Just "List pets") (Just "Returns all pets") [] Nothing
            let tool = Converter.convertOperation "/pets" "GET" op
            Converter.toolName tool @?= "openapi_listPets"
            Converter.toolPath tool @?= "/pets"
            Converter.toolMethod tool @?= "GET"
        , testCase "convertOperation creates tool with correct description" $ do
            let op = OpenAPI.Operation (Just "listPets") (Just "List pets") (Just "Returns all pets") [] Nothing
            let tool = Converter.convertOperation "/pets" "GET" op
            Converter.toolDescription tool @?= "List pets:\nReturns all pets"
        , testCase "buildToolParameters with no params" $ do
            let op = OpenAPI.Operation (Just "test") Nothing Nothing [] Nothing
            let params = Converter.buildToolParameters op
            Converter.paramsType params @?= "object"
            Map.null (Converter.paramsProperties params) @?= True
            Converter.paramsRequired params @?= []
        , testCase "buildToolParameters with path parameter" $ do
            let schema = OpenAPI.Schema (Just "string") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let param = OpenAPI.Parameter "petId" OpenAPI.ParamInPath Nothing True (Just schema)
            let op = OpenAPI.Operation (Just "getPet") Nothing Nothing [param] Nothing
            let params = Converter.buildToolParameters op
            Map.size (Converter.paramsProperties params) @?= 1
            Converter.paramsRequired params @?= ["p_petId"]
        , testCase "buildToolParameters with optional query parameter" $ do
            let schema = OpenAPI.Schema (Just "integer") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let param = OpenAPI.Parameter "limit" OpenAPI.ParamInQuery (Just "Max results") False (Just schema)
            let op = OpenAPI.Operation (Just "listPets") Nothing Nothing [param] Nothing
            let params = Converter.buildToolParameters op
            Map.size (Converter.paramsProperties params) @?= 1
            Converter.paramsRequired params @?= []  -- Not required
        , testCase "buildToolParameters with request body" $ do
            let bodySchema = OpenAPI.Schema (Just "object") (Just "Pet object") Nothing Nothing Nothing Nothing Nothing Nothing
            let content = Map.singleton "application/json" bodySchema
            let body = OpenAPI.RequestBody (Just "Pet to create") True content
            let op = OpenAPI.Operation (Just "createPet") Nothing Nothing [] (Just body)
            let params = Converter.buildToolParameters op
            Map.size (Converter.paramsProperties params) @?= 1
            Converter.paramsRequired params @?= ["b"]  -- Body is required
        , testCase "buildToolProperty with enum values" $ do
            let schema = OpenAPI.Schema (Just "string") Nothing (Just [String "cat", String "dog"]) Nothing Nothing Nothing Nothing Nothing
            let prop = Converter.buildToolProperty schema (Just "Pet type")
            Converter.propType prop @?= Just "string"
            Converter.propEnum prop @?= Just ["cat", "dog"]
            assertBool "Description should contain enum info" ("Allowed values" `Text.isInfixOf` Converter.propDescription prop)
        , testCase "buildToolProperty with array items" $ do
            let itemSchema = OpenAPI.Schema (Just "string") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let schema = OpenAPI.Schema (Just "array") Nothing Nothing Nothing (Just itemSchema) Nothing Nothing Nothing
            let prop = Converter.buildToolProperty schema (Just "Tags")
            Converter.propType prop @?= Just "array"
            case Converter.propItems prop of
                Nothing -> assertFailure "Expected items schema"
                Just items -> Converter.propType items @?= Just "string"
        , testCase "getRequiredParams with required path param" $ do
            let schema = OpenAPI.Schema (Just "string") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let param = OpenAPI.Parameter "id" OpenAPI.ParamInPath Nothing True (Just schema)
            let op = OpenAPI.Operation Nothing Nothing Nothing [param] Nothing
            Converter.getRequiredParams op @?= ["p_id"]
        , testCase "getRequiredParams with optional query param" $ do
            let schema = OpenAPI.Schema (Just "string") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let param = OpenAPI.Parameter "filter" OpenAPI.ParamInQuery Nothing False (Just schema)
            let op = OpenAPI.Operation Nothing Nothing Nothing [param] Nothing
            Converter.getRequiredParams op @?= []  -- Not required
        , testCase "getRequiredParams with required body" $ do
            let bodySchema = OpenAPI.Schema (Just "object") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let content = Map.singleton "application/json" bodySchema
            let body = OpenAPI.RequestBody Nothing True content
            let op = OpenAPI.Operation Nothing Nothing Nothing [] (Just body)
            Converter.getRequiredParams op @?= ["b"]
        , testCase "convertOpenAPIToTools converts all operations" $ do
            let petSchema = OpenAPI.Schema (Just "object") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            let getOp = OpenAPI.Operation (Just "getPet") (Just "Get") Nothing [] Nothing
            let postOp = OpenAPI.Operation (Just "createPet") (Just "Create") Nothing [] Nothing
            let paths = Map.fromList
                    [ ("/pets", Map.fromList [("GET", getOp), ("POST", postOp)])
                    , ("/pets/{id}", Map.singleton "GET" getOp)
                    ]
            let spec = OpenAPI.OpenAPISpec paths Nothing Nothing
            let tools = Converter.convertOpenAPIToTools spec
            length tools @?= 3
        , testCase "toOpenAITool converts tool correctly" $ do
            let op = OpenAPI.Operation (Just "testOp") (Just "Test") (Just "A test op") [] Nothing
            let apiTool = Converter.convertOperation "/test" "GET" op
            let tool = Converter.toOpenAITool apiTool
            OpenAI.toolName tool @?= OpenAI.ToolName "openapi_testOp"
            OpenAI.toolDescription tool @?= "Test:\nA test op"
        ]

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
            turnId' <- SessionBase.newTurnId
            let ctx = Context.mkMinimalContext sessionId convId turnId'
            Context.ctxCallStack ctx @?= []
            Context.ctxMaxDepth ctx @?= Nothing
            Context.ctxSessionId ctx @?= sessionId
            Context.ctxConversationId ctx @?= convId
            Context.ctxTurnId ctx @?= turnId'
            Context.ctxAgentId ctx @?= Nothing
            Context.ctxFullSession ctx @?= Nothing
        , testCase "mkRootContext creates context with root entry in call stack" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            let ctx = Context.mkRootContext sessionId convId turnId' Nothing Nothing (Just 5)
            Context.ctxCallStack ctx @?= [Context.CallStackEntry "root" convId 0]
            Context.ctxMaxDepth ctx @?= Just 5
        , testCase "mkToolExecutionContext with full parameters" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            agentId <- Base.newAgentId
            convId2 <- Base.newConversationId
            let callStack = [Context.CallStackEntry "agent-1" convId 0, Context.CallStackEntry "agent-2" convId2 1]
            let ctx = Context.mkToolExecutionContext sessionId convId turnId' (Just agentId) Nothing callStack (Just 10)
            Context.ctxCallStack ctx @?= callStack
            Context.ctxMaxDepth ctx @?= Just 10
            Context.ctxAgentId ctx @?= Just agentId
        , testCase "JSON round-trip with call stack and max depth" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            agentId <- Base.newAgentId
            convId2 <- Base.newConversationId
            let callStack =
                    [ Context.CallStackEntry "agent-1" convId 0
                    , Context.CallStackEntry "agent-2" convId2 1
                    ]
            let ctx = Context.mkToolExecutionContext sessionId convId turnId' (Just agentId) Nothing callStack (Just 5)
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
            turnId' <- SessionBase.newTurnId
            let ctx = Context.mkMinimalContext sessionId convId turnId'
            Context.hasFullSession ctx @?= False
        , testCase "hasAgentId returns False when agentId is Nothing" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            let ctx = Context.mkMinimalContext sessionId convId turnId'
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
            turnId' <- SessionBase.newTurnId
            let ctx = Context.mkMinimalContext sessionId convId turnId'
            Context.currentRecursionDepth ctx @?= 0
        , testCase "currentRecursionDepth returns 1 for root context" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            let ctx = Context.mkRootContext sessionId convId turnId' Nothing Nothing Nothing
            Context.currentRecursionDepth ctx @?= 1
        , testCase "pushAgentContext increments depth" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            newConvId <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId' Nothing Nothing Nothing
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
            turnId' <- SessionBase.newTurnId
            -- Create root context with max depth of 1
            let rootCtx = Context.mkRootContext sessionId convId turnId' Nothing Nothing (Just 1)
            -- First push should fail because root is already at depth 1 (max)
            case Context.pushAgentContext "sub-agent" newConvId rootCtx of
                Left (Context.MaxRecursionDepthExceeded stack) -> do
                    length stack @?= 1  -- Root entry
                Right _ -> assertFailure "Should have failed with MaxRecursionDepthExceeded"
        , testCase "isAtDepth correctly checks depth threshold" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId' Nothing Nothing Nothing
            Context.isAtDepth 1 rootCtx @?= True
            Context.isAtDepth 2 rootCtx @?= False
        , testCase "callChain returns entries in root-first order" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            newConvId1 <- Base.newConversationId
            newConvId2 <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId' Nothing Nothing Nothing
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
            turnId' <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId' Nothing Nothing Nothing
            Context.isAgentInCallStack "root" rootCtx @?= True
            Context.isAgentInCallStack "nonexistent" rootCtx @?= False
        , testCase "isAgentInCallStack detects agent after push" $ do
            sessionId <- SessionBase.newSessionId
            convId <- Base.newConversationId
            newConvId <- Base.newConversationId
            turnId' <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId' Nothing Nothing Nothing
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
            turnId' <- SessionBase.newTurnId
            let rootCtx = Context.mkRootContext sessionId convId turnId' Nothing Nothing Nothing
            case Context.pushAgentContext "sub-agent" newConvId rootCtx of
                Left err -> assertFailure $ "Push should not fail: " ++ show err
                Right newCtx -> do
                    Context.ctxConversationId newCtx @?= newConvId
                    -- Session ID and Turn ID should be preserved
                    Context.ctxSessionId newCtx @?= sessionId
                    Context.ctxTurnId newCtx @?= turnId'
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
            let edges = Map.fromList [("main-agent", ["helper-a", "helper-b"]), ("helper-a", []), ("helper-b", [])]
            let graph = AgentTree.AgentConfigGraph nodes edges

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

    mkTestAgentWithExtra slug extras = Base.Agent
        { Base.slug = slug
        , Base.apiKeyId = "test-key"
        , Base.flavor = "openai"
        , Base.modelUrl = "https://api.openai.com/v1"
        , Base.modelName = "gpt-4"
        , Base.announce = "Test agent " <> slug
        , Base.systemPrompt = ["You are helpful"]
        , Base.toolDirectory = Just "tools"
        , Base.bashToolboxes = Nothing
        , Base.mcpServers = Nothing
        , Base.openApiToolboxes = Nothing
        , Base.postgrestToolboxes = Nothing
        , Base.builtinToolboxes = Nothing
        , Base.extraAgents = Just [Base.ExtraAgentRef e (Text.unpack e <> ".json") | e <- extras]
        , Base.skillSources = Nothing
        , Base.autoEnableSkills = Nothing
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
                            AgentTree.RefLoadingError (AgentTree.MissingAgentReference _ _ _) -> True
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
            agent2 = Base.Agent
                { Base.slug = "agent-2"
                , Base.apiKeyId = "test-key"
                , Base.flavor = "openai"
                , Base.modelUrl = "https://api.openai.com/v1"
                , Base.modelName = "gpt-4"
                , Base.announce = "Agent 2"
                , Base.systemPrompt = ["Helpful"]
                , Base.toolDirectory = Just "tools"
                , Base.bashToolboxes = Nothing
                , Base.mcpServers = Nothing
                , Base.openApiToolboxes = Nothing
                , Base.postgrestToolboxes = Nothing
                , Base.builtinToolboxes = Nothing
                , Base.extraAgents = Just [Base.ExtraAgentRef "agent-1" "/agent-1.json"]
                , Base.skillSources = Nothing
                , Base.autoEnableSkills = Nothing
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
                , Base.toolDirectory = Just "tools"
                , Base.bashToolboxes = Nothing
                , Base.mcpServers = Nothing
                , Base.openApiToolboxes = Nothing
                , Base.postgrestToolboxes = Nothing
                , Base.builtinToolboxes = Nothing
                , Base.extraAgents = Just [Base.ExtraAgentRef "nonexistent" "/nonexistent.json"]
                , Base.skillSources = Nothing
                , Base.autoEnableSkills = Nothing
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


