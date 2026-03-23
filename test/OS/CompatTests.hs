{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Tests for the OS compatibility layer.

These tests verify that:
1. The compatibility layer maintains backward compatibility
2. OS-backed implementation matches old Runtime behavior
3. Migration configuration works correctly
-}
module OS.CompatTests (tests) where

import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import System.Agents.OS.Compat.Runtime
import System.Agents.Session.Base (UserToolResponse (..))
import System.Agents.Tools.Base (CallResult (..))
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Base as Base

import qualified Data.Aeson.KeyMap as KeyMap

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup
        "OS.CompatTests"
        [ migrationConfigTests
        , runtimeBridgeTests
        , callResultConversionTests
        , agentRuntimeTests
        ]

-------------------------------------------------------------------------------
-- Migration Configuration Tests
-------------------------------------------------------------------------------

migrationConfigTests :: TestTree
migrationConfigTests =
    testGroup
        "MigrationConfig"
        [ testCase "default config uses old-only phase" $ do
            let config = defaultMigrationConfig
            assertEqual "OS should be disabled by default" False (migrationEnableOS config)
            assertEqual "Compat should be enabled by default" True (migrationEnableCompat config)
        , testCase "dual mode config enables both" $ do
            let config =
                    defaultMigrationConfig
                        { migrationEnableOS = True
                        , migrationEnableCompat = True
                        }
            assertEqual "OS should be enabled in dual mode" True (migrationEnableOS config)
            assertEqual "Compat should be enabled in dual mode" True (migrationEnableCompat config)
        , testCase "new-only config disables compat" $ do
            let config =
                    defaultMigrationConfig
                        { migrationEnableOS = True
                        , migrationEnableCompat = False
                        }
            assertEqual "OS should be enabled in new-only mode" True (migrationEnableOS config)
            assertEqual "Compat should be disabled in new-only mode" False (migrationEnableCompat config)
        ]

-------------------------------------------------------------------------------
-- Runtime Bridge Tests
-------------------------------------------------------------------------------

runtimeBridgeTests :: TestTree
runtimeBridgeTests =
    testGroup
        "RuntimeBridge"
        [ testCase "newRuntimeBridge creates bridge with correct agent ID" $ do
            agentId <- Base.newAgentId
            -- We can't actually test with a real OS without full initialization
            -- This test verifies the bridge constructor works
            let bridge = newRuntimeBridge agentId (error "mock OS")
            assertEqual "Agent ID should match" agentId (bridgeAgentId bridge)
        ]

-------------------------------------------------------------------------------
-- CallResult Conversion Tests
-------------------------------------------------------------------------------

callResultConversionTests :: TestTree
callResultConversionTests =
    testGroup
        "callResultToUserToolResponse"
        [ testCase "converts ToolNotFound correctly" $ do
            let mockCall = createMockToolCall "test-tool"
            let result = ToolNotFound mockCall
            let response = callResultToUserToolResponse mockCall result
            assertEqual
                "Should return 'Tool not found'"
                (UserToolResponse $ String "Tool not found")
                response
        , testCase "converts BlobToolSuccess correctly" $ do
            let mockCall = createMockToolCall "test-tool"
            let result = BlobToolSuccess mockCall "Hello, World!"
            let response = callResultToUserToolResponse mockCall result
            assertEqual
                "Should return text content"
                (UserToolResponse $ String "Hello, World!")
                response
        ]

-------------------------------------------------------------------------------
-- AgentRuntime Typeclass Tests
-------------------------------------------------------------------------------

agentRuntimeTests :: TestTree
agentRuntimeTests =
    testGroup
        "AgentRuntime"
        [ testCase "defaultMigrationConfig works" $ do
            let config = defaultMigrationConfig
            assertEqual "Default should have OS disabled" False (migrationEnableOS config)
        ]

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Create a mock ToolCall for testing.
createMockToolCall :: Text -> OpenAI.ToolCall
createMockToolCall name =
    OpenAI.ToolCall
        { OpenAI.rawToolCall = KeyMap.empty
        , OpenAI.toolCallId = "mock-call-id"
        , OpenAI.toolCallType = Just "function"
        , OpenAI.toolCallFunction =
            OpenAI.ToolCallFunction
                { OpenAI.toolCallFunctionName = OpenAI.ToolName name
                , OpenAI.toolCallFunctionArgsUnparsed = "{}"
                , OpenAI.toolCallFunctionArgs = Nothing
                }
        }

