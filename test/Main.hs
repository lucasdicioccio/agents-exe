{-# LANGUAGE OverloadedStrings #-}

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

import qualified OpenAPIToolboxTests
import qualified PostgRESTParseTest
import qualified SessionEditTests
import qualified SessionPrintTests
import qualified EndpointPredicateTests
import qualified SkillsTests
import qualified McpImplementationTests
import qualified ToolPortalTests
import qualified LuaToolboxSecurityTests
import qualified LuaToolboxTests
import qualified LuaToolboxJsonValueTests
import qualified LuaToolboxHttpTests
import qualified OS.CoreTests
import qualified OS.ConversationTests
import qualified OS.ResourcesTests
import qualified OS.ConcurrentTests
import qualified OS.InterfaceTests
import qualified OS.IntegrationTests
import qualified OS.PersistenceTests
import qualified OS.VersionedSqliteTests
import qualified ActivationSessionTests
import qualified DeveloperToolboxTests

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
        , OS.ConversationTests.conversationTests
        , OS.ResourcesTests.resourcesTests
        , OS.ConcurrentTests.concurrentTests
        , OS.InterfaceTests.tests
        , OS.IntegrationTests.integrationTests
        , OS.PersistenceTests.persistenceTests
        , OS.VersionedSqliteTests.versionedSqliteTests
        , OpenAPIToolboxTests.tests
        , PostgRESTParseTest.tests
        , SessionEditTests.tests
        , SessionPrintTests.tests
        , EndpointPredicateTests.tests
        , SkillsTests.skillsTestSuite
        , ActivationSessionTests.activationSessionTestSuite
        , McpImplementationTests.mcpImplementationTestSuite
        , ToolPortalTests.toolPortalTestSuite
        , LuaToolboxSecurityTests.tests
        , LuaToolboxTests.luaToolboxTests
        , LuaToolboxJsonValueTests.luaToJsonValueTests
        , LuaToolboxHttpTests.luaToolboxHttpTests
        , DeveloperToolboxTests.tests
        ]

openAIRateLimitTests :: TestTree
openAIRateLimitTests =
    testGroup
        "OpenAI rate-limits"
        [ testCase "1d1h1m1s" $ parseRateLimitDelay "1d1h1m1s" @?= 1 + 60 + 3600 + 86400
        , testCase "1h1m1s" $ parseRateLimitDelay "1h1m1s" @?= 1 + 60 + 3600
        , testCase "1m1s" $ parseRateLimitDelay "1m1s" @?= 1 + 60
        , testCase "1s" $ parseRateLimitDelay "1s" @?= 1
        , testCase "1.376s" $ parseRateLimitDelay "1.376s" @?= 2
        , testCase "120ms" $ parseRateLimitDelay "120ms" @?= 1
        , testCase "26.001s" $ parseRateLimitDelay "26.001s" @?= 27
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
        ]
  where
    encodeUtf8 = LBS.fromStrict . Text.encodeUtf8

bashToolboxTests :: TestTree
bashToolboxTests =
    testGroup
        "Bash Toolbox Descriptions"
        [ testCase "FileSystemDirectory serialization" $ do
            let desc = Base.FileSystemDirectoryDescription
                    { Base.fsDirRoot = Nothing
                    , Base.fsDirPath = "./tools"
                    , Base.fsDirBasenameFilter = Nothing
                    , Base.fsDirActivation = Nothing
                    }
            let json = encode desc
            let mDesc = decode json :: Maybe Base.FileSystemDirectoryDescription
            mDesc @?= Just desc
        ]

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
                    length (turns session) @?= 2
                    mapM_ (\turn -> turnByteUsage turn @?= Nothing) (turns session)
        ]
  where
    turnByteUsage :: Turn -> Maybe ()
    turnByteUsage (UserTurn _ usage) = fmap (const ()) usage
    turnByteUsage (LlmTurn _ usage) = fmap (const ()) usage

turnRoundTripTests :: TestTree
turnRoundTripTests =
    testGroup
        "Turn Round-trip Tests"
        [ testCase "UserTurn round-trip without byteUsage" $ do
            let userContent = UserTurnContent
                    { userPrompt = SystemPrompt "You are helpful"
                    , userTools = []
                    , userQuery = Just (UserQuery "Hello" [])
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
                    , userQuery = Just (UserQuery "Hello" [])
                    , userToolResponses = []
                    }
            let byteUsage = StepByteUsage 1000 500 300 100 100 Nothing
            let turn = UserTurn userContent (Just byteUsage)
            let json = encode turn
            let mDecoded = decode json :: Maybe Turn
            mDecoded @?= Just turn
        ]

