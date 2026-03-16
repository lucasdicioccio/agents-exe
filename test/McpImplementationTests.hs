{- |
Module      : McpImplementationTests
Description : Tests for MCP (Model Context Protocol) implementation

This module contains tests for verifying MCP data types and JSON parsing,
starting with simple data parsing tests for InitializeResult.
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
module McpImplementationTests where

import Data.Aeson (decode, eitherDecode, encode, Value(..), Object, fromJSON, Result(..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

-- Import the MCP Base module
import System.Agents.MCP.Base

-- | Test suite entry point
mcpImplementationTestSuite :: TestTree
mcpImplementationTestSuite =
    testGroup
        "MCP Implementation Tests"
        [ initializeResultParsingTests
        ]

-------------------------------------------------------------------------------
-- InitializeResult Parsing Tests
-------------------------------------------------------------------------------

-- | Tests for parsing InitializeResult from JSON
initializeResultParsingTests :: TestTree
initializeResultParsingTests =
    testGroup
        "InitializeResult JSON Parsing"
        [ testCase "mcp-server-initialize-01.json is valid JSON object" $ do
            -- Read the test file
            jsonContent <- LBS.readFile "test/data/mcp-server-initialize-01.json"
            
            -- Verify it's valid JSON (parse to Value)
            let mValue = decode jsonContent :: Maybe Value
            assertBool "File should contain valid JSON" (mValue /= Nothing)
            
            -- Verify it's a JSON object (not array, string, etc.)
            case mValue of
                Just (Object _) -> pure () -- Success
                Just other -> assertFailure $ "Expected JSON Object, got: " ++ show other
                Nothing -> assertFailure "Failed to parse JSON"
        
        , testCase "mcp-server-initialize-01.json has 'result' key" $ do
            jsonContent <- LBS.readFile "test/data/mcp-server-initialize-01.json"
            
            let mValue = decode jsonContent :: Maybe Object
            assertBool "File should contain a JSON Object" (mValue /= Nothing)
            
            case mValue of
                Just obj -> do
                    -- Check that "result" key exists
                    case KeyMap.lookup "result" obj of
                        Just _ -> pure () -- Success
                        Nothing -> assertFailure "Missing 'result' key in JSON object"
                Nothing -> assertFailure "Failed to parse JSON object"
        
        , testCase "mcp-server-initialize-01.json result parses as InitializeResult" $ do
            jsonContent <- LBS.readFile "test/data/mcp-server-initialize-01.json"
            
            -- Parse the full JSON-RPC response structure
            let mValue = decode jsonContent :: Maybe Value
            case mValue of
                Just (Object obj) -> do
                    case KeyMap.lookup "result" obj of
                        Just resultValue -> do
                            -- Try to parse the result as InitializeResult
                            case fromJSON @InitializeResult resultValue of
                                Success result -> do
                                    -- Verify expected values
                                    result.protocolVersion @?= "2024-11-05"
                                    result.serverInfo.name @?= "test-mcp-server"
                                    result.serverInfo.version @?= "1.0.0"
                                    result.instructions @?= Just "Test MCP server for verification purposes"
                                    
                                    -- Verify capabilities flags
                                    let caps = result.capabilities
                                    caps.flags @?= []  -- All flags are false in the test file
                                    
                                Error err -> assertFailure $ "Failed to parse as InitializeResult: " ++ err
                        Nothing -> assertFailure "Missing 'result' key"
                _ -> assertFailure "Failed to parse JSON as object"
        
        , testCase "InitializeResult round-trip serialization" $ do
            let originalResult = InitializeResult
                    { protocolVersion = "2024-11-05"
                    , capabilities = ServerCapabilities
                        { experimental = Nothing
                        , logging = Nothing
                        , flags = [ToolsListChanged]  -- Test with a flag set
                        }
                    , serverInfo = Implementation
                        { name = "test-server"
                        , version = "1.0.0"
                        }
                    , instructions = Just "Test instructions"
                    }
            
            -- Serialize to JSON
            let json = encode originalResult
            
            -- Deserialize back
            case eitherDecode @InitializeResult json of
                Left err -> assertFailure $ "Failed to deserialize: " ++ err
                Right decodedResult -> do
                    decodedResult @?= originalResult
        
        , testCase "InitializeResult with empty instructions parses correctly" $ do
            let json = "{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{\"prompts\":{\"listChanged\":false},\"resources\":{\"listChanged\":false,\"subscribe\":false},\"tools\":{\"listChanged\":true}},\"serverInfo\":{\"name\":\"minimal\",\"version\":\"0.1\"}}"
            
            case eitherDecode @InitializeResult json of
                Left err -> assertFailure $ "Failed to parse minimal InitializeResult: " ++ err
                Right result -> do
                    result.protocolVersion @?= "2024-11-05"
                    result.serverInfo.name @?= "minimal"
                    result.instructions @?= Nothing  -- Should be Nothing when omitted
                    -- Verify the ToolsListChanged flag is set
                    assertBool "ToolsListChanged flag should be set" (ToolsListChanged `elem` result.capabilities.flags)
        
        , testCase "ServerCapabilities flags parsing" $ do
            -- Test all flags can be parsed
            let jsonAllFlags = "{\"protocolVersion\":\"1.0\",\"capabilities\":{\"prompts\":{\"listChanged\":true},\"resources\":{\"listChanged\":true,\"subscribe\":true},\"tools\":{\"listChanged\":true}},\"serverInfo\":{\"name\":\"full\",\"version\":\"1.0\"}}"
            
            case eitherDecode @InitializeResult jsonAllFlags of
                Left err -> assertFailure $ "Failed to parse full capabilities: " ++ err
                Right result -> do
                    let caps = result.capabilities.flags
                    assertBool "PromptsListChanged should be set" (PromptsListChanged `elem` caps)
                    assertBool "ResourcesListChanged should be set" (ResourcesListChanged `elem` caps)
                    assertBool "ResourcesSubscribe should be set" (ResourcesSubscribe `elem` caps)
                    assertBool "ToolsListChanged should be set" (ToolsListChanged `elem` caps)
        ]

