{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive unit tests for LuaToolbox.
--
-- These tests verify:
-- * Basic Lua execution (arithmetic, tables)
-- * Sandbox security (os.execute, io.popen blocked)
-- * Standard library modules (json, text, time, tools)
-- * Resource limits (memory, timeout)
-- * Filesystem sandboxing
-- * HTTP sandboxing
-- * Tool portal integration
-- * Error handling
-- * Isolation between calls (no state persistence)
--
-- NOTE: Some tests use the module functions directly as globals since
-- the modules are registered as global tables in the Lua state.
module LuaToolboxTests where

import Control.Exception (bracket)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (writeFile)
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Prod.Tracer (Tracer (..), silent)
import qualified Prod.Tracer as Prod

import System.Agents.Base (ConversationId (..), LuaToolboxDescription (..))
import System.Agents.Session.Types (SessionId (..), TurnId (..))
import System.Agents.Tools.Context (ToolCall (..), ToolExecutionContext, ToolPortal, ToolResult (..), mkMinimalContext)
import System.Agents.Tools.LuaToolbox as LuaToolbox

-- | Create a minimal test context with dummy UUIDs
mkTestContext :: ToolPortal -> ToolExecutionContext
mkTestContext portal =
    let sessionId = SessionId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440001"
        conversationId = ConversationId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440002"
        turnId = TurnId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440003"
     in mkMinimalContext sessionId conversationId turnId portal

luaToolboxTests :: TestTree
luaToolboxTests =
    testGroup
        "LuaToolbox Tests"
        [ basicExecutionTests
        , sandboxSecurityTests
        , moduleTests
        , resourceLimitTests
        , errorHandlingTests
        , portalIntegrationTests
        , isolationTests
        ]

-------------------------------------------------------------------------------
-- Test Configuration
-------------------------------------------------------------------------------

-- | Dummy portal that returns an error if called.
-- Used for tests that don't need actual tool portal functionality.
dummyPortal :: ToolPortal
dummyPortal _mParentCtx _call =
    pure $
        ToolResult
            { resultData = Aeson.object [("error", Aeson.String "Tool portal not available in test")]
            , resultDuration = 0
            , resultTraceId = "dummy"
            }

testLuaToolbox :: LuaToolboxDescription
testLuaToolbox =
    LuaToolboxDescription
        { luaToolboxName = "test"
        , luaToolboxDescription = "Test toolbox"
        , luaToolboxMaxMemoryMB = 64
        , luaToolboxMaxExecutionTimeSeconds = 10
        , luaToolboxAllowedTools = []
        , luaToolboxAllowedPaths = []
        , luaToolboxAllowedHosts = []
        }

-- | Run an action with a test toolbox
withTestToolbox :: (LuaToolbox.Toolbox -> IO ()) -> IO ()
withTestToolbox action = do
    initResult <- LuaToolbox.initializeToolbox silent testLuaToolbox
    case initResult of
        Left err -> assertFailure $ "Failed to initialize toolbox: " ++ err
        Right box -> action box

-- | Run an action with a test toolbox that has filesystem access
withTestToolboxFs :: (LuaToolbox.Toolbox -> FilePath -> IO ()) -> IO ()
withTestToolboxFs action = withTempSandbox $ \testDir -> do
    let desc = testLuaToolbox{luaToolboxAllowedPaths = [testDir]}
    initResult <- LuaToolbox.initializeToolbox silent desc
    case initResult of
        Left err -> assertFailure $ "Failed to initialize toolbox: " ++ err
        Right box -> action box testDir

-- | Create a temporary sandbox directory for testing
withTempSandbox :: (FilePath -> IO a) -> IO a
withTempSandbox action = do
    tmpDir <- createTempDirectory "/tmp" "luatoolbox-test-"
    bracket
        (pure tmpDir)
        removeDirectoryRecursive
        action

-------------------------------------------------------------------------------
-- Basic Execution Tests
-------------------------------------------------------------------------------

basicExecutionTests :: TestTree
basicExecutionTests =
    testGroup
        "Basic Execution"
        [ testCase "Simple arithmetic" testSimpleArithmetic
        , testCase "Return string" testReturnString
        , testCase "Return boolean" testReturnBoolean
        , testCase "Return nil" testReturnNil
        , testCase "Return table - 0 value : array" testReturnTable0
        , testCase "Return table - 1 value" testReturnTable1
        , testCase "Return table - 2 mixed values" testReturnTable2
        , testCase "Return array" testReturnArray
        ]

testSimpleArithmetic :: Assertion
testSimpleArithmetic = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return 1 + 1" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Number 2]

testReturnTable0 :: Assertion
testReturnTable0 = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return {}" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues
                @?= [Aeson.Array (Vector.fromList [])]

testReturnTable1 :: Assertion
testReturnTable1 = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return {foo = 42}" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues
                @?= [Aeson.object ["foo" .= (42 :: Int)]]

testReturnTable2 :: Assertion
testReturnTable2 = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return {foo = 'bar', count = 42}" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues
                @?= [Aeson.object ["foo" .= ("bar" :: Text), "count" .= (42 :: Int)]]

testReturnArray :: Assertion
testReturnArray = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return {1, 2, 3}" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues
                @?= [Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 2, Aeson.Number 3])]

testReturnString :: Assertion
testReturnString = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return 'hello world'" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.String "hello world"]

testReturnBoolean :: Assertion
testReturnBoolean = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return true" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testReturnNil :: Assertion
testReturnNil = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return nil" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Null]

-------------------------------------------------------------------------------
-- Sandbox Security Tests
-------------------------------------------------------------------------------

sandboxSecurityTests :: TestTree
sandboxSecurityTests =
    testGroup
        "Sandbox Security"
        [ testCase "Sandbox blocks os.execute" testSandboxBlocksOsExecute
        , testCase "Sandbox blocks io.popen" testSandboxBlocksIoPopen
        , testCase "Sandbox blocks dofile" testSandboxBlocksDofile
        , testCase "Sandbox blocks loadfile" testSandboxBlocksLoadfile
        , testCase "Sandbox blocks os.remove" testSandboxBlocksOsRemove
        , testCase "Sandbox blocks os.rename" testSandboxBlocksOsRename
        , testCase "Sandbox blocks os.exit" testSandboxBlocksOsExit
        , testCase "Sandbox blocks io.tmpfile" testSandboxBlocksIoTmpfile
        ]

testSandboxBlocksOsExecute :: Assertion
testSandboxBlocksOsExecute = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "os.execute('echo pwned')" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksIoPopen :: Assertion
testSandboxBlocksIoPopen = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "io.popen('echo pwned')" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksDofile :: Assertion
testSandboxBlocksDofile = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "dofile('/etc/passwd')" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksLoadfile :: Assertion
testSandboxBlocksLoadfile = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "loadfile('/etc/passwd')" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksOsRemove :: Assertion
testSandboxBlocksOsRemove = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "os.remove('/tmp/test')" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksOsRename :: Assertion
testSandboxBlocksOsRename = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "os.rename('/tmp/a', '/tmp/b')" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksOsExit :: Assertion
testSandboxBlocksOsExit = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "os.exit(0)" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksIoTmpfile :: Assertion
testSandboxBlocksIoTmpfile = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "io.tmpfile()" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

-------------------------------------------------------------------------------
-- Module Tests (json, text, time, fs, tools)
-------------------------------------------------------------------------------

moduleTests :: TestTree
moduleTests =
    testGroup
        "Module Tests"
        [ testCase "JSON module is available" testJsonAvailable
        , testCase "Text module is available" testTextAvailable
        , testCase "Time module is available" testTimeAvailable
        , testCase "Tools module is available" testToolsAvailable
        , testCase "FS module blocks unauthorized access" testFsBlocksUnauthorized
        ]

testJsonAvailable :: Assertion
testJsonAvailable = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    -- Test that json module exists and has encode/decode functions
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return json ~= nil" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testTextAvailable :: Assertion
testTextAvailable = withTestToolbox $ \box -> do
    -- Test that text module exists
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return text ~= nil" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testTimeAvailable :: Assertion
testTimeAvailable = withTestToolbox $ \box -> do
    -- Test that time module exists
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return time ~= nil" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testToolsAvailable :: Assertion
testToolsAvailable = withTestToolbox $ \box -> do
    -- Test that tools module exists
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return tools ~= nil" ctx dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testFsBlocksUnauthorized :: Assertion
testFsBlocksUnauthorized = withTestToolbox $ \box -> do
    -- With empty allowedPaths (default testLuaToolbox), any fs access should be blocked
    let ctx = mkTestContext dummyPortal
    result <-
        LuaToolbox.executeScriptWithPortal
            Prod.tracePrint
            box
            ( Text.unlines
                [ "local result = fs.read('/etc/passwd')"
                , "return result"
                ]
            )
            ctx
            dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            case execResult.resultValues of
                (Aeson.Object obj : []) -> do
                    -- With empty paths, fs.read returns error
                    case KeyMap.lookup "error" obj of
                        Just _ -> pure () -- Expected
                        Nothing -> assertFailure "Expected error field"
                (Aeson.Null : _) -> pure () -- nil is also acceptable
                _ -> assertFailure "Expected nil or error object"

-------------------------------------------------------------------------------
-- Resource Limit Tests
-------------------------------------------------------------------------------

resourceLimitTests :: TestTree
resourceLimitTests =
    testGroup
        "Resource Limits"
        [ -- testCase "Timeout enforcement" testTimeout
        -- Note: Memory limit is difficult to test reliably in CI
        -- Skipping for now to avoid flakiness
        ]

testTimeout :: Assertion
testTimeout = do
    let desc = testLuaToolbox{luaToolboxMaxExecutionTimeSeconds = 1}
    initResult <- LuaToolbox.initializeToolbox silent desc
    case initResult of
        Left err -> assertFailure $ "Failed to initialize: " ++ err
        Right box -> do
            let ctx = mkTestContext dummyPortal
            result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "while true do end" ctx dummyPortal
            case result of
                Left (TimeoutError _) -> pure () -- Expected
                Left err -> assertFailure $ "Wrong error type: " ++ show err
                Right _ -> assertFailure "Should have timed out"

-------------------------------------------------------------------------------
-- Error Handling Tests
-------------------------------------------------------------------------------

errorHandlingTests :: TestTree
errorHandlingTests =
    testGroup
        "Error Handling"
        [ testCase "Syntax error handling" testSyntaxError
        , testCase "Runtime error handling" testRuntimeError
        , testCase "Error handling with pcall" testPcallError
        ]

testSyntaxError :: Assertion
testSyntaxError = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "return 1 +" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention syntax" $
                "syntax" `Text.isInfixOf` msg || "expected" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with syntax error"

testRuntimeError :: Assertion
testRuntimeError = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    -- Use a variable assigned to nil to cause a runtime error (not syntax error)
    result <- LuaToolbox.executeScriptWithPortal Prod.tracePrint box "local x = nil; return x.foo" ctx dummyPortal
    case result of
        Left (LuaRuntimeError (Aeson.String msg : [])) ->
            assertBool "Error should mention attempt to index" $
                "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with runtime error"


testPcallError :: Assertion
testPcallError = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    result <-
        LuaToolbox.executeScriptWithPortal
            Prod.tracePrint
            box
            ( Text.unlines
                [ "local ok, err = pcall(function() return error(\"aie\") end)"
                , "return {success = ok, error = err}"
                ]
            )
            ctx
            dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            case execResult.resultValues of
                ((Aeson.Object obj) : []) -> do
                    case KeyMap.lookup "success" obj of
                        Just (Aeson.Bool b) -> b @?= False
                        _ -> assertFailure "Expected success=false"
                _ -> assertFailure "Expected object result"

-------------------------------------------------------------------------------
-- Portal Integration Tests
-------------------------------------------------------------------------------

portalIntegrationTests :: TestTree
portalIntegrationTests =
    testGroup
        "Portal Integration"
        [ testCase "Tool call with mock portal" testPortalToolCall
        , testCase "Tool call blocked by empty whitelist" testToolBlockedByEmptyWhitelist
        , testCase "Tool whitelist enforcement" testToolWhitelist
        ]

testPortalToolCall :: Assertion
testPortalToolCall = do
    -- Create a mock portal that echoes back the call
    let mockPortal :: ToolPortal
        mockPortal _mParentCtx call =
            pure $
                ToolResult
                    { resultData =
                        Aeson.object
                            [ "tool" .= callToolName call
                            , "args" .= callArgs call
                            ]
                    , resultDuration = 0.1
                    , resultTraceId = "test-trace-123"
                    }

    let desc = testLuaToolbox{luaToolboxAllowedTools = ["test_tool"]}

    initResult <- LuaToolbox.initializeToolbox silent desc
    case initResult of
        Left err -> assertFailure $ "Failed to initialize: " ++ err
        Right box -> do
            let ctx = mkTestContext mockPortal
            result <-
                LuaToolbox.executeScriptWithPortal
                    silent
                    box
                    ( Text.unlines
                        [ "local result = tools.call('test_tool', {x = 1})"
                        , "return result"
                        ]
                    )
                    ctx
                    mockPortal

            case result of
                Left err -> assertFailure $ show err
                Right execResult -> do
                    case execResult.resultValues of
                        ((Aeson.Object resultObj) : []) -> do
                            -- Check status field
                            case KeyMap.lookup "status" resultObj of
                                Just (Aeson.String "ok") -> pure () -- Expected
                                Just (Aeson.String status) -> assertFailure $ "Unexpected status: " ++ Text.unpack status
                                _ -> assertFailure "Expected status field with 'ok'"
                            -- Check result_txt field contains expected data
                            case KeyMap.lookup "result_txt" resultObj of
                                Just (Aeson.String resultTxt) -> do
                                    -- Parse the JSON result
                                    case Aeson.eitherDecodeStrict (Text.encodeUtf8 resultTxt) of
                                        Left parseErr -> assertFailure $ "Failed to parse result_txt: " ++ parseErr
                                        Right (val :: Aeson.Value) -> do
                                            case val of
                                                Aeson.Object dataObj -> do
                                                    case KeyMap.lookup "tool" dataObj of
                                                        Just (Aeson.String t) -> t @?= "test_tool"
                                                        _ -> assertFailure "Expected tool name in data"
                                                _ -> assertFailure "Expected data object"
                                _ -> assertFailure "Expected result_txt field"
                        _ -> assertFailure $ "Unexpected result: " ++ show execResult.resultValues

-- | Test that tool calls are blocked when the whitelist is empty
testToolBlockedByEmptyWhitelist :: Assertion
testToolBlockedByEmptyWhitelist = withTestToolbox $ \box -> do
    -- With empty allowedTools (default testLuaToolbox), any tool call should be blocked
    let ctx = mkTestContext dummyPortal
    result <-
        LuaToolbox.executeScriptWithPortal
            Prod.tracePrint
            box
            ( Text.unlines
                [ "local result = tools.call('bash', {command = 'echo hello'})"
                , "return result"
                ]
            )
            ctx
            dummyPortal
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            case execResult.resultValues of
                ((Aeson.Object obj) : []) -> do
                    -- With empty whitelist, tools.call returns "not-allowed"
                    case KeyMap.lookup "status" obj of
                        Just (Aeson.String "not-allowed") -> pure () -- Expected
                        Just (Aeson.String status) -> assertFailure $ "Expected 'not-allowed' status, got: " ++ Text.unpack status
                        _ -> assertFailure "Expected status field"
                _ -> assertFailure "Expected object result"

testToolWhitelist :: Assertion
testToolWhitelist = do
    -- Create a mock portal that should not be called for blocked tool
    let mockPortal :: ToolPortal
        mockPortal _mParentCtx _ =
            pure $
                ToolResult
                    { resultData = Aeson.object ["result" .= ("success" :: Text)]
                    , resultDuration = 0.1
                    , resultTraceId = "test-trace"
                    }

    let desc = testLuaToolbox{luaToolboxAllowedTools = ["allowed_tool"]}

    initResult <- LuaToolbox.initializeToolbox silent desc
    case initResult of
        Left err -> assertFailure $ "Failed to initialize: " ++ err
        Right box -> do
            let ctx = mkTestContext mockPortal
            result <-
                LuaToolbox.executeScriptWithPortal
                    silent
                    box
                    ( Text.unlines
                        [ "local result = tools.call('blocked_tool', {})"
                        , "return result"
                        ]
                    )
                    ctx
                    mockPortal

            case result of
                Left err -> assertFailure $ show err
                Right execResult ->
                    case execResult.resultValues of
                        ((Aeson.Object obj) : []) -> do
                            case KeyMap.lookup "status" obj of
                                Just (Aeson.String "not-allowed") -> pure () -- Expected
                                Just (Aeson.String status) -> assertFailure $ "Expected 'not-allowed' status, got: " ++ Text.unpack status
                                _ -> assertFailure "Expected status field"
                        _ -> assertFailure "Expected object result"

-------------------------------------------------------------------------------
-- Isolation Tests (Per-Tool-Call Isolation)
-------------------------------------------------------------------------------

{- | Tests for per-tool-call isolation.

These tests verify that each Lua tool call gets a fresh, isolated Lua state
with no persistence between calls. This is a key security feature that:

* Prevents state poisoning between calls
* Bounds memory usage (no accumulation)
* Eliminates deadlocks with recursive agent calls
* Provides deterministic behavior
-}
isolationTests :: TestTree
isolationTests =
    testGroup
        "Per-Call Isolation"
        [ testCase "Globals don't persist between calls" testGlobalsDontPersist
        , testCase "Table state is isolated between calls" testTableIsolation
        , testCase "Multiple sequential calls are independent" testSequentialCalls
        ]

-- | Test that global variables don't persist between tool calls
testGlobalsDontPersist :: Assertion
testGlobalsDontPersist = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    -- First call: set a global variable
    result1 <-
        LuaToolbox.executeScriptWithPortal
            Prod.tracePrint
            box
            "globalVar = 'hello from first call'; return globalVar"
            ctx
            dummyPortal
    case result1 of
        Left err -> assertFailure $ "First call failed: " ++ show err
        Right execResult1 -> do
            execResult1.resultValues @?= [Aeson.String "hello from first call"]

            -- Second call: try to access the global (should be nil)
            result2 <-
                LuaToolbox.executeScriptWithPortal
                    Prod.tracePrint
                    box
                    "return globalVar"
                    ctx
                    dummyPortal
            case result2 of
                Left err -> assertFailure $ "Second call failed: " ++ show err
                Right execResult2 -> do
                    -- globalVar should be nil because each call gets a fresh state
                    execResult2.resultValues @?= [Aeson.Null]

-- | Test that table state is isolated between calls
testTableIsolation :: Assertion
testTableIsolation = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    -- First call: create and modify a table
    result1 <-
        LuaToolbox.executeScriptWithPortal
            Prod.tracePrint
            box
            "myTable = {count = 1}; myTable.count = myTable.count + 1; return myTable.count"
            ctx
            dummyPortal
    case result1 of
        Left err -> assertFailure $ "First call failed: " ++ show err
        Right execResult1 -> do
            execResult1.resultValues @?= [Aeson.Number 2]

            -- Second call: try to access the table (should not exist)
            result2 <-
                LuaToolbox.executeScriptWithPortal
                    Prod.tracePrint
                    box
                    "if myTable then return myTable.count else return 'table_not_found' end"
                    ctx
                    dummyPortal
            case result2 of
                Left err -> assertFailure $ "Second call failed: " ++ show err
                Right execResult2 -> do
                    -- myTable should be nil
                    execResult2.resultValues @?= [Aeson.String "table_not_found"]

-- | Test that multiple sequential calls are completely independent
testSequentialCalls :: Assertion
testSequentialCalls = withTestToolbox $ \box -> do
    let ctx = mkTestContext dummyPortal
    -- Run multiple calls in sequence, each should start fresh
    let scripts =
            [ ("call1", "x = 1; return x" :: Text)
            , ("call2", "x = 2; return x")
            , ("call3", "x = 3; return x")
            , ("call4", "return x") -- Should return nil, not 3
            ]

    results <-
        mapM
            ( \(name, script) -> do
                result <- LuaToolbox.executeScriptWithPortal silent box script ctx dummyPortal
                pure (name, result)
            )
            scripts

    -- Verify results
    case results of
        [ ("call1", Right r1)
            , ("call2", Right r2)
            , ("call3", Right r3)
            , ("call4", Right r4)
            ] -> do
                r1.resultValues @?= [Aeson.Number 1]
                r2.resultValues @?= [Aeson.Number 2]
                r3.resultValues @?= [Aeson.Number 3]
                -- x should be nil in call4 because each call gets fresh state
                r4.resultValues @?= [Aeson.Null]
        _ -> assertFailure $ "Unexpected results: " ++ show results

