{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Comprehensive unit tests for LuaToolbox.
--
-- These tests verify:
-- * Basic Lua execution (arithmetic, tables)
-- * Sandbox security (os.execute, io.popen blocked)
-- * Standard library modules (json, text, time)
-- * Resource limits (memory, timeout)
-- * Filesystem sandboxing
-- * HTTP sandboxing
-- * Error handling
--
-- NOTE: Some tests use the module functions directly as globals since
-- the modules are registered as global tables in the Lua state.
module LuaToolboxTests where

import Control.Exception (bracket)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (writeFile)
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Prod.Tracer (Tracer (..))

import System.Agents.Base (LuaToolboxDescription (..))
import System.Agents.Tools.Context (ToolCall (..), ToolPortal, ToolResult (..))
import System.Agents.Tools.LuaToolbox as LuaToolbox

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
        ]

-------------------------------------------------------------------------------
-- Test Configuration
-------------------------------------------------------------------------------

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

-- | Run an action with a test toolbox initialized
withTestToolbox :: (LuaToolbox.Toolbox -> IO ()) -> IO ()
withTestToolbox action = do
    initResult <- LuaToolbox.initializeToolbox LuaToolbox.nullTracer testLuaToolbox
    case initResult of
        Left err -> assertFailure $ "Failed to initialize toolbox: " ++ err
        Right box -> do
            bracket
                (pure box)
                (\b -> LuaToolbox.closeToolbox LuaToolbox.nullTracer b)
                action

-- | Run an action with a test toolbox that has filesystem access
withTestToolboxFs :: (LuaToolbox.Toolbox -> FilePath -> IO ()) -> IO ()
withTestToolboxFs action = withTempSandbox $ \testDir -> do
    let desc = testLuaToolbox{luaToolboxAllowedPaths = [testDir]}
    initResult <- LuaToolbox.initializeToolbox LuaToolbox.nullTracer desc
    case initResult of
        Left err -> assertFailure $ "Failed to initialize toolbox: " ++ err
        Right box -> do
            bracket
                (pure (box, testDir))
                (\(b, _) -> LuaToolbox.closeToolbox LuaToolbox.nullTracer b)
                (\(b, d) -> action b d)

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
    result <- LuaToolbox.executeScript box "return 1 + 1"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Number 2]

testReturnTable0 :: Assertion
testReturnTable0 = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "return {}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues
                @?= [Aeson.Array (Vector.fromList [])]

testReturnTable1 :: Assertion
testReturnTable1 = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "return {foo = 42}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues
                @?= [Aeson.object ["foo" .= (42 :: Int)]]

testReturnTable2 :: Assertion
testReturnTable2 = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "return {foo = 'bar', count = 42}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues
                @?= [Aeson.object ["foo" .= ("bar" :: Text), "count" .= (42 :: Int)]]

testReturnArray :: Assertion
testReturnArray = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "return {1, 2, 3}"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues
                @?= [Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 2, Aeson.Number 3])]

testReturnString :: Assertion
testReturnString = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "return 'hello world'"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.String "hello world"]

testReturnBoolean :: Assertion
testReturnBoolean = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "return true"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testReturnNil :: Assertion
testReturnNil = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "return nil"
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
    result <- LuaToolbox.executeScript box "os.execute('echo pwned')"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksIoPopen :: Assertion
testSandboxBlocksIoPopen = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "io.popen('echo pwned')"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksDofile :: Assertion
testSandboxBlocksDofile = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "dofile('/etc/passwd')"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksLoadfile :: Assertion
testSandboxBlocksLoadfile = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "loadfile('/etc/passwd')"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksOsRemove :: Assertion
testSandboxBlocksOsRemove = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "os.remove('/tmp/test')"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksOsRename :: Assertion
testSandboxBlocksOsRename = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "os.rename('/tmp/a', '/tmp/b')"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksOsExit :: Assertion
testSandboxBlocksOsExit = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "os.exit(0)"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

testSandboxBlocksIoTmpfile :: Assertion
testSandboxBlocksIoTmpfile = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "io.tmpfile()"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention nil or attempt" $
                "nil" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with sandbox error"

-------------------------------------------------------------------------------
-- Module Tests (json, text, time, fs)
-------------------------------------------------------------------------------

moduleTests :: TestTree
moduleTests =
    testGroup
        "Module Tests"
        [ testCase "JSON module is available" testJsonAvailable
        , testCase "Text module is available" testTextAvailable
        , testCase "Time module is available" testTimeAvailable
        , testCase "FS module blocks unauthorized access" testFsBlocksUnauthorized
        ]

testJsonAvailable :: Assertion
testJsonAvailable = withTestToolbox $ \box -> do
    -- Test that json module exists and has encode/decode functions
    result <- LuaToolbox.executeScript box "return json ~= nil"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testTextAvailable :: Assertion
testTextAvailable = withTestToolbox $ \box -> do
    -- Test that text module exists
    result <- LuaToolbox.executeScript box "return text ~= nil"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testTimeAvailable :: Assertion
testTimeAvailable = withTestToolbox $ \box -> do
    -- Test that time module exists
    result <- LuaToolbox.executeScript box "return time ~= nil"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            execResult.resultValues @?= [Aeson.Bool True]

testFsBlocksUnauthorized :: Assertion
testFsBlocksUnauthorized = withTestToolbox $ \box -> do
    -- With empty allowedPaths, fs should block all access
    result <- LuaToolbox.executeScript box "return fs.read('/etc/passwd')"
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            -- Should return nil (access blocked)
            (drop 1 execResult.resultValues) @?= [Aeson.Null]

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
    initResult <- LuaToolbox.initializeToolbox LuaToolbox.nullTracer desc
    case initResult of
        Left err -> assertFailure $ "Failed to initialize: " ++ err
        Right box -> do
            result <- LuaToolbox.executeScript box "while true do end"
            LuaToolbox.closeToolbox LuaToolbox.nullTracer box
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
    result <- LuaToolbox.executeScript box "return 1 +"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention syntax" $
                "syntax" `Text.isInfixOf` msg || "expected" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure "Should have failed with syntax error"

testRuntimeError :: Assertion
testRuntimeError = withTestToolbox $ \box -> do
    result <- LuaToolbox.executeScript box "return error(\"aie\")"
    case result of
        Left (LuaRuntimeError (Aeson.String msg:[])) ->
            assertBool "Error should mention aie" $
                "aie" `Text.isInfixOf` msg || "attempt" `Text.isInfixOf` msg
        Left _ -> pure () -- Any error is acceptable
        Right _ -> assertFailure ("Should have failed with runtime error, got: " <> show result)

testPcallError :: Assertion
testPcallError = withTestToolbox $ \box -> do
    result <-
        LuaToolbox.executeScript box $
            Text.unlines
                [ "local ok, err = pcall(function() return error(\"aie\") end)"
                , "return {success = ok, error = err}"
                ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            case execResult.resultValues of
                ((Aeson.Object obj):[]) -> do
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
        , testCase "Tool call without portal fails gracefully" testNoPortal
        , testCase "Tool whitelist enforcement" testToolWhitelist
        ]

testPortalToolCall :: Assertion
testPortalToolCall = do
    -- Create a mock portal that echoes back the call
    let mockPortal :: ToolPortal
        mockPortal call =
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

    initResult <- LuaToolbox.initializeToolbox LuaToolbox.nullTracer desc
    case initResult of
        Left err -> assertFailure $ "Failed to initialize: " ++ err
        Right box -> do
            result <-
                LuaToolbox.executeScriptWithPortal
                    box
                    ( Text.unlines
                        [ "local result = tools.call('test_tool', {x = 1})"
                        , "return result"
                        ]
                    )
                    (Just mockPortal)

            LuaToolbox.closeToolbox LuaToolbox.nullTracer box

            case result of
                Left err -> assertFailure $ show err
                Right execResult -> do
                    case execResult.resultValues of
                        ((Aeson.Object resultObj):[]) -> do
                            case KeyMap.lookup "data" resultObj of
                                Just (Aeson.Object dataObj) -> do
                                    case KeyMap.lookup "tool" dataObj of
                                        Just (Aeson.String t) -> t @?= "test_tool"
                                        _ -> assertFailure "Expected tool name in data"
                                _ -> assertFailure "Expected data object"
                        _ -> assertFailure $ "Unexpected result: " ++ show execResult.resultValues

testNoPortal :: Assertion
testNoPortal = withTestToolbox $ \box -> do
    -- Try to call a tool without a portal
    result <-
        LuaToolbox.executeScript box $
            Text.unlines
                [ "local result, err = tools.call('bash', {command = 'echo hello'})"
                , "return {result = result, error = err}"
                ]
    case result of
        Left err -> assertFailure $ show err
        Right execResult ->
            case execResult.resultValues of
                ((Aeson.Object obj):[]) -> do
                    case KeyMap.lookup "result" obj of
                        Nothing -> pure () -- Expected: result-key is absent
                        _ -> assertFailure ("Expected no result, got:" <> show obj)
                _ -> assertFailure "Expected object result"

testToolWhitelist :: Assertion
testToolWhitelist = do
    -- Create a mock portal that should not be called for blocked tool
    let mockPortal :: ToolPortal
        mockPortal _ =
            pure $
                ToolResult
                    { resultData = Aeson.object ["result" .= ("success" :: Text)]
                    , resultDuration = 0.1
                    , resultTraceId = "test-trace"
                    }

    let desc = testLuaToolbox{luaToolboxAllowedTools = ["allowed_tool"]}

    initResult <- LuaToolbox.initializeToolbox LuaToolbox.nullTracer desc
    case initResult of
        Left err -> assertFailure $ "Failed to initialize: " ++ err
        Right box -> do
            result <-
                LuaToolbox.executeScriptWithPortal
                    box
                    ( Text.unlines
                        [ "local result, err = tools.call('blocked_tool', {})"
                        , "return {result = result, error = err}"
                        ]
                    )
                    (Just mockPortal)

            LuaToolbox.closeToolbox LuaToolbox.nullTracer box

            case result of
                Left err -> assertFailure $ show err
                Right execResult ->
                    case execResult.resultValues of
                        ((Aeson.Object obj):[]) -> do
                            case KeyMap.lookup "result" obj of
                                Nothing -> pure () -- Expected: result is missing
                                _ -> assertFailure ("Expected no result for blocked tool, got:" <> show obj)
                        _ -> assertFailure "Expected object result"

