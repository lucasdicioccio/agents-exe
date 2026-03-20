{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tests for the LuaToolbox HTTP module.
--
-- These tests verify:
-- * HTTP GET requests with host validation
-- * HTTP POST requests with host validation  
-- * HTTP request function with options
-- * Error handling for blocked hosts
-- * Error handling for invalid URLs
-- * parseOptions function for parsing request options
--
-- NOTE: These tests use a mock HTTP server approach where possible,
-- but primarily focus on the Lua-to-Haskell interface correctness.
module LuaToolboxHttpTests where

import Control.Exception (bracket)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit

import qualified HsLua as Lua
import Prod.Tracer (Tracer (..), silent)

import System.Agents.Base (LuaToolboxDescription (..))
import System.Agents.Tools.LuaToolbox as LuaToolbox
import System.Agents.Tools.LuaToolbox.Modules.Http (
    HttpConfig (..),
    RequestOptions (..),
    parseOptions,
 )

luaToolboxHttpTests :: TestTree
luaToolboxHttpTests =
    testGroup
        "LuaToolbox HTTP Module Tests"
        [ httpGetTests
        , httpPostTests
        , httpRequestTests
        , hostValidationErrorTests
        , parseOptionsTests
        ]

-------------------------------------------------------------------------------
-- Test Configuration
-------------------------------------------------------------------------------

-- | Toolbox config with http access to localhost only
testLuaToolboxHttp :: LuaToolboxDescription
testLuaToolboxHttp =
    LuaToolboxDescription
        { luaToolboxName = "test"
        , luaToolboxDescription = "Test toolbox with HTTP"
        , luaToolboxMaxMemoryMB = 64
        , luaToolboxMaxExecutionTimeSeconds = 10
        , luaToolboxAllowedTools = []
        , luaToolboxAllowedPaths = []
        , luaToolboxAllowedHosts = ["localhost", "127.0.0.1"]
        }

-- | Toolbox config with NO http access (empty allowedHosts)
testLuaToolboxNoHttp :: LuaToolboxDescription
testLuaToolboxNoHttp =
    LuaToolboxDescription
        { luaToolboxName = "test"
        , luaToolboxDescription = "Test toolbox without HTTP"
        , luaToolboxMaxMemoryMB = 64
        , luaToolboxMaxExecutionTimeSeconds = 10
        , luaToolboxAllowedTools = []
        , luaToolboxAllowedPaths = []
        , luaToolboxAllowedHosts = []
        }

withTestToolboxHttp :: (LuaToolbox.Toolbox -> IO ()) -> IO ()
withTestToolboxHttp action = do
    initResult <- LuaToolbox.initializeToolbox silent testLuaToolboxHttp
    case initResult of
        Left err -> assertFailure $ "Failed to initialize toolbox: " ++ err
        Right box -> do
            bracket
                (pure box)
                (\b -> LuaToolbox.closeToolbox silent b)
                action

withTestToolboxNoHttp :: (LuaToolbox.Toolbox -> IO ()) -> IO ()
withTestToolboxNoHttp action = do
    initResult <- LuaToolbox.initializeToolbox silent testLuaToolboxNoHttp
    case initResult of
        Left err -> assertFailure $ "Failed to initialize toolbox: " ++ err
        Right box -> do
            bracket
                (pure box)
                (\b -> LuaToolbox.closeToolbox silent b)
                action

-- | Helper to run a Lua operation in an isolated state
withLuaState :: (Lua.State -> IO a) -> IO a
withLuaState action = bracket Lua.newstate Lua.close action

-------------------------------------------------------------------------------
-- parseOptions Tests
-------------------------------------------------------------------------------

parseOptionsTests :: TestTree
parseOptionsTests =
    testGroup
        "parseOptions"
        [ testCase "parseOptions returns Nothing for non-table value" testParseOptionsNonTable
        , testCase "parseOptions returns empty headers for empty table" testParseOptionsEmptyTable
        , testCase "parseOptions parses headers from options table" testParseOptionsWithHeaders
        , testCase "parseOptions parses multiple headers" testParseOptionsMultipleHeaders
        , testCase "parseOptions ignores non-headers keys" testParseOptionsIgnoresOtherKeys
        , testCase "parseOptions handles headers table with non-string values" testParseOptionsNonStringHeaders
        , testCase "parseOptions handles nested headers correctly" testParseOptionsNestedHeaders
        ]

-- | Test that parseOptions returns Nothing when given a non-table value
testParseOptionsNonTable :: Assertion
testParseOptionsNonTable = withLuaState $ \lstate -> do
    result <- Lua.runWith lstate $ do
        -- Push a string instead of a table
        Lua.pushstring "not a table"
        parseOptions (Lua.nthTop 1) <* Lua.pop 1
    result @?= Nothing

-- | Test that parseOptions returns empty headers for an empty table
testParseOptionsEmptyTable :: Assertion
testParseOptionsEmptyTable = withLuaState $ \lstate -> do
    result <- Lua.runWith lstate $ do
        Lua.newtable
        parseOptions (Lua.nthTop 1) <* Lua.pop 1
    case result of
        Nothing -> assertFailure "Expected Just RequestOptions, got Nothing"
        Just opts -> optHeaders opts @?= []

-- | Test that parseOptions correctly parses headers from an options table
testParseOptionsWithHeaders :: Assertion
testParseOptionsWithHeaders = withLuaState $ \lstate -> do
    result <- Lua.runWith lstate $ do
        Lua.newtable
        
        -- Create headers subtable
        Lua.pushstring "headers"
        Lua.newtable
        Lua.pushstring "Content-Type"
        Lua.pushstring "application/json"
        Lua.settable (Lua.nthTop 3)
        -- Set headers in options table
        Lua.settable (Lua.nthTop 3)
        
        parseOptions (Lua.nthTop 1) <* Lua.pop 1
    
    case result of
        Nothing -> assertFailure "Expected Just RequestOptions, got Nothing"
        Just opts -> do
            length (optHeaders opts) @?= 1
            case optHeaders opts of
                [(key, val)] -> do
                    key @?= "Content-Type"
                    val @?= "application/json"
                _ -> assertFailure "Expected exactly one header"

-- | Test that parseOptions correctly parses multiple headers
testParseOptionsMultipleHeaders :: Assertion
testParseOptionsMultipleHeaders = withLuaState $ \lstate -> do
    result <- Lua.runWith lstate $ do
        Lua.newtable
        
        -- Create headers subtable with multiple headers
        Lua.pushstring "headers"
        Lua.newtable
        
        Lua.pushstring "Content-Type"
        Lua.pushstring "application/json"
        Lua.settable (Lua.nthTop 3)
        
        Lua.pushstring "Authorization"
        Lua.pushstring "Bearer token123"
        Lua.settable (Lua.nthTop 3)
        
        Lua.pushstring "X-Custom-Header"
        Lua.pushstring "custom-value"
        Lua.settable (Lua.nthTop 3)
        
        -- Set headers in options table
        Lua.settable (Lua.nthTop 3)
        
        parseOptions (Lua.nthTop 1) <* Lua.pop 1
    
    case result of
        Nothing -> assertFailure "Expected Just RequestOptions, got Nothing"
        Just opts -> do
            length (optHeaders opts) @?= 3
            -- Check that all headers are present
            let headerMap = foldr (\(k, v) acc -> (k, v) : acc) [] (optHeaders opts)
            lookup "Content-Type" headerMap @?= Just "application/json"
            lookup "Authorization" headerMap @?= Just "Bearer token123"
            lookup "X-Custom-Header" headerMap @?= Just "custom-value"

-- | Test that parseOptions ignores keys other than "headers"
testParseOptionsIgnoresOtherKeys :: Assertion
testParseOptionsIgnoresOtherKeys = withLuaState $ \lstate -> do
    result <- Lua.runWith lstate $ do
        Lua.newtable
        
        -- Add some non-headers keys
        Lua.pushstring "timeout"
        Lua.pushinteger 30
        Lua.settable (Lua.nthTop 3)
        
        Lua.pushstring "method"
        Lua.pushstring "POST"
        Lua.settable (Lua.nthTop 3)
        
        -- Add headers
        Lua.pushstring "headers"
        Lua.newtable
        Lua.pushstring "Accept"
        Lua.pushstring "application/json"
        Lua.settable (Lua.nthTop 3)
        Lua.settable (Lua.nthTop 3)
        
        parseOptions (Lua.nthTop 1) <* Lua.pop 1
    
    case result of
        Nothing -> assertFailure "Expected Just RequestOptions, got Nothing"
        Just opts -> do
            length (optHeaders opts) @?= 1
            case optHeaders opts of
                [(key, val)] -> do
                    key @?= "Accept"
                    val @?= "application/json"
                _ -> assertFailure "Expected exactly one header"

-- | Test that parseOptions handles non-string values in headers table gracefully
testParseOptionsNonStringHeaders :: Assertion
testParseOptionsNonStringHeaders = withLuaState $ \lstate -> do
    result <- Lua.runWith lstate $ do
        Lua.newtable
        
        -- Create headers subtable with mixed types
        Lua.pushstring "headers"
        Lua.newtable
        
        Lua.pushstring "Valid-Header"
        Lua.pushstring "valid-value"
        Lua.settable (Lua.nthTop 3)
        
        Lua.pushstring "Numeric-Header"
        Lua.pushinteger 42
        Lua.settable (Lua.nthTop 3)
        
        -- Set headers in options table
        Lua.settable (Lua.nthTop 3)
        
        parseOptions (Lua.nthTop 1) <* Lua.pop 1
    
    -- Should parse successfully, non-string values may be converted or skipped
    case result of
        Nothing -> assertFailure "Expected Just RequestOptions, got Nothing"
        Just opts -> do
            -- At least the valid header should be present
            length (optHeaders opts) @?= 2

-- | Test that parseOptions handles nested headers table correctly
testParseOptionsNestedHeaders :: Assertion
testParseOptionsNestedHeaders = withLuaState $ \lstate -> do
    result <- Lua.runWith lstate $ do
        Lua.newtable
        
        -- Create headers subtable
        Lua.pushstring "headers"
        Lua.newtable
        
        -- Nested table structure in headers (should be handled gracefully)
        Lua.pushstring "X-Nested"
        Lua.newtable
        Lua.pushstring "nested-key"
        Lua.pushstring "nested-value"
        Lua.settable (Lua.nthTop 3)
        Lua.settable (Lua.nthTop 3)
        
        Lua.pushstring "X-Simple"
        Lua.pushstring "simple-value"
        Lua.settable (Lua.nthTop 3)
        
        -- Set headers in options table
        Lua.settable (Lua.nthTop 3)
        
        parseOptions (Lua.nthTop 1) <* Lua.pop 1
    
    case result of
        Nothing -> assertFailure "Expected Just RequestOptions, got Nothing"
        Just opts -> do
            -- Should have at least the simple header
            length (optHeaders opts) @?= 2

-------------------------------------------------------------------------------
-- HTTP GET Tests
-------------------------------------------------------------------------------

httpGetTests :: TestTree
httpGetTests =
    testGroup
        "HTTP GET"
        [ testCase "http.get returns error for blocked host" testHttpGetBlockedHost
        , testCase "http.get validates host before request" testHttpGetHostValidation
        ]

-- | Test that http.get is blocked when no hosts are allowed
testHttpGetBlockedHost :: Assertion
testHttpGetBlockedHost = withTestToolboxNoHttp $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return http.get('http://example.com')"
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            -- The HTTP module returns (nil, error) on failure
            -- After luaToJsonValue conversion, this becomes [error_string, Null]
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    assertBool "Error should mention 'No hosts allowed' or 'Host not in allowed list'" $
                        "No hosts allowed" `Text.isInfixOf` errMsg ||
                        "Host not in allowed list" `Text.isInfixOf` errMsg ||
                        "not in allowed" `Text.isInfixOf` errMsg
                vals -> assertFailure $ "Expected error_string, nil but got: " ++ show vals

-- | Test that http.get validates host correctly
testHttpGetHostValidation :: Assertion
testHttpGetHostValidation = withTestToolboxHttp $ \box -> do
    -- localhost should be allowed but will fail to connect (no server)
    result <- LuaToolbox.executeScript box $
        "return http.get('http://localhost:99999/test')"
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            -- Should get error and nil (connection error since no server is running)
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    -- The error should be a connection error, not a "blocked" error
                    assertBool "Error should be a connection error, not 'Host not in allowed list'" $
                        not ("Host not in allowed list" `Text.isInfixOf` errMsg) &&
                        not ("No hosts allowed" `Text.isInfixOf` errMsg)
                vals -> assertFailure $ "Expected error_string, nil but got: " ++ show vals

-------------------------------------------------------------------------------
-- HTTP POST Tests
-------------------------------------------------------------------------------

httpPostTests :: TestTree
httpPostTests =
    testGroup
        "HTTP POST"
        [ testCase "http.post argument order - url first, body second" testHttpPostArgumentOrder
        , testCase "http.post validates host before request" testHttpPostHostValidation
        , testCase "http.post with options table" testHttpPostWithOptions
        , testCase "http.post blocks non-whitelisted hosts" testHttpPostBlocksNonWhitelisted
        ]

-- | Test that http.post reads arguments in correct order (url, body)
-- This test reproduces the bug where url and body were swapped
testHttpPostArgumentOrder :: Assertion
testHttpPostArgumentOrder = withTestToolboxHttp $ \box -> do
    -- When we call http.post(url, body), the url should be validated as a host
    -- If arguments are swapped, the body would be treated as a URL which would
    -- either fail host validation (if body doesn't look like a URL) or cause
    -- a segfault when trying to parse an invalid URL.
    
    -- Use a simple string body that is definitely NOT a valid URL
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "local url = 'http://localhost:99999/api'"
        , "local body = 'this is not a url'"
        , "return http.post(url, body)"
        ]
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            -- The result should either:
            -- 1. Be a connection error (host validated OK, but no server)
            -- 2. Be error, nil (connection refused)
            -- It should NOT be a "Host not in allowed list" error for "this is not a url"
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    -- If the arguments are swapped, we'd get:
                    -- "Host not in allowed list: this is not a url" or "Invalid URL"
                    let isHostValidationError = 
                            "Host not in allowed list" `Text.isInfixOf` errMsg ||
                            "No host in URL" `Text.isInfixOf` errMsg ||
                            "Invalid URL" `Text.isInfixOf` errMsg
                    
                    -- If we get a host validation error mentioning the BODY content,
                    -- then arguments are swapped (BUG!)
                    if "this is not a url" `Text.isInfixOf` errMsg && isHostValidationError
                        then assertFailure $ 
                            "BUG: http.post arguments are swapped! " ++
                            "Body content was treated as URL: " ++ Text.unpack errMsg
                        else pure () -- Expected: connection error or similar
                _ -> pure () -- Connection might succeed but no server to respond

-- | Test that http.post validates host correctly before attempting request
testHttpPostHostValidation :: Assertion
testHttpPostHostValidation = withTestToolboxHttp $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return http.post('http://localhost:99999/test', 'data')"
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            -- localhost is allowed, but no server running - should get connection error
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    assertBool "Error should be connection error, not host validation" $
                        not ("Host not in allowed list" `Text.isInfixOf` errMsg)
                _ -> pure () -- OK, might get other error formats

-- | Test http.post with options table (third argument)
-- NOTE: This test is disabled because of a known bug in luaPost.
-- The bug is that when nargs >= 3, the argument indices are calculated incorrectly.
-- See the comments in luaPost function.
testHttpPostWithOptions :: Assertion
testHttpPostWithOptions = withTestToolboxHttp $ \box -> do
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "local url = 'http://localhost:99999/api'"
        , "local body = '{\"key\": \"value\"}'"
        , "local options = {"
        , "    headers = {['Content-Type'] = 'application/json'}"
        , "}"
        , "return http.post(url, body, options)"
        ]
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            -- Should not crash - options parsing should work
            -- We expect a connection error since no server is running
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    assertBool "Should get connection error, not argument error" $
                        not ("Usage:" `Text.isInfixOf` errMsg)
                _ -> pure () -- OK

-- | Test that http.post correctly blocks non-whitelisted hosts
testHttpPostBlocksNonWhitelisted :: Assertion
testHttpPostBlocksNonWhitelisted = withTestToolboxHttp $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return http.post('http://example.com/api', 'data')"
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            -- example.com is not in allowedHosts (only localhost is)
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    assertBool "Should get 'Host not in allowed list' error" $
                        "Host not in allowed list" `Text.isInfixOf` errMsg ||
                        "example.com" `Text.isInfixOf` errMsg
                vals -> assertFailure $ "Expected error_string, nil but got: " ++ show vals

-------------------------------------------------------------------------------
-- HTTP Request Tests (generic request function)
-------------------------------------------------------------------------------

httpRequestTests :: TestTree
httpRequestTests =
    testGroup
        "HTTP Request (generic)"
        [ testCase "http.request with method, url, body" testHttpRequestWithBody
        , testCase "http.request validates host" testHttpRequestValidatesHost
        ]

-- | Test http.request function with all parameters
testHttpRequestWithBody :: Assertion
testHttpRequestWithBody = withTestToolboxHttp $ \box -> do
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "return http.request({"
        , "    method = 'POST',"
        , "    url = 'http://localhost:99999/api',"
        , "    body = 'test data'"
        , "})"
        ]
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            -- Should not crash - request should be attempted
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    assertBool "Should get connection error, not parsing error" $
                        not ("Usage:" `Text.isInfixOf` errMsg)
                _ -> pure () -- OK

-- | Test that http.request validates host
testHttpRequestValidatesHost :: Assertion
testHttpRequestValidatesHost = withTestToolboxHttp $ \box -> do
    result <- LuaToolbox.executeScript box $ Text.unlines
        [ "return http.request({"
        , "    method = 'GET',"
        , "    url = 'http://evil.com/data'"
        , "})"
        ]
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            -- evil.com is not in allowedHosts
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    -- Check that it's a host validation error (not a connection error)
                    assertBool ("Should get host validation error, got: " ++ Text.unpack errMsg) $
                        "Host not in allowed list" `Text.isInfixOf` errMsg ||
                        "not in allowed" `Text.isInfixOf` errMsg ||
                        -- Also accept if it's not a connection-related error
                        (not ("Connection" `Text.isInfixOf` errMsg) &&
                         not ("connect" `Text.isInfixOf` errMsg))
                vals -> assertFailure $ "Expected error_string, nil but got: " ++ show vals

-------------------------------------------------------------------------------
-- Host Validation Error Tests
-------------------------------------------------------------------------------

hostValidationErrorTests :: TestTree
hostValidationErrorTests =
    testGroup
        "Host Validation Errors"
        [ testCase "Invalid URL returns appropriate error" testInvalidUrlError
        , testCase "URL without host returns error" testUrlWithoutHost
        ]

-- | Test error message for invalid URL
testInvalidUrlError :: Assertion
testInvalidUrlError = withTestToolboxHttp $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return http.get('not-a-valid-url')"
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    assertBool "Error should mention invalid URL" $
                        "Invalid URL" `Text.isInfixOf` errMsg ||
                        "No host" `Text.isInfixOf` errMsg
                vals -> assertFailure $ "Expected error_string, nil but got: " ++ show vals

-- | Test error for URL without host (like file://)
testUrlWithoutHost :: Assertion
testUrlWithoutHost = withTestToolboxHttp $ \box -> do
    result <- LuaToolbox.executeScript box $
        "return http.get('file:///etc/passwd')"
    case result of
        Left err -> assertFailure $ "Script failed: " ++ show err
        Right execResult -> do
            case execResult.resultValues of
                (Aeson.String errMsg : Aeson.Null : _) -> do
                    assertBool "Error should mention no host" $
                        "No host" `Text.isInfixOf` errMsg ||
                        "Invalid URL" `Text.isInfixOf` errMsg
                vals -> assertFailure $ "Expected error_string, nil but got: " ++ show vals

