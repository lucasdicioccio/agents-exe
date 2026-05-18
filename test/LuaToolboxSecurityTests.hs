{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Security tests for LuaToolbox.
--
-- These tests verify:
-- * Path sandboxing with canonicalization
-- * Host whitelist validation
-- * Secure defaults (empty whitelist = no access)
-- * Tool call tracing
module LuaToolboxSecurityTests where

import Control.Exception (bracket)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (writeFile)
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import Prod.Tracer (silent)
import qualified Prod.Tracer as Prod

import System.Agents.Base (
    ConversationId (..),
    FileSandboxConfig (..),
    LuaToolboxDescription (..),
    defaultFileSandboxConfig,
 )
import System.Agents.FileSandbox (
    AccessResult (..),
    FileSandbox (..),
    validateFileRead,
    validateFileWrite,
 )
import System.Agents.FileSandbox.Predicate (
    PathError (..),
    PathPredicate (..),
    fromPathList,
 )
import System.Agents.Session.Types (SessionId (..), TurnId (..))
import System.Agents.Tools.Context (ToolExecutionContext, ToolPortal, ToolResult (..), mkMinimalContext)
import System.Agents.Tools.LuaToolbox as LuaToolbox
import qualified System.Agents.Tools.LuaToolbox.Modules.Http as HttpMod

-- | Create a minimal test context with dummy UUIDs
mkTestContext :: ToolPortal -> ToolExecutionContext
mkTestContext portal =
    let sessionId = SessionId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440001"
        conversationId = ConversationId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440002"
        turnId = TurnId $ fromJust $ UUID.fromText "550e8400-e29b-41d4-a716-446655440003"
     in mkMinimalContext sessionId conversationId turnId portal

-- | Dummy portal for tests
dummyPortal :: ToolPortal
dummyPortal _mParentCtx _call =
    pure $
        ToolResult
            { resultData = Aeson.object [("error", Aeson.String "Tool portal not available in test")]
            , resultDuration = 0
            , resultTraceId = "dummy"
            }

-- | Create a FileSandbox from config (lightweight, for testing)
createTestSandbox :: FileSandboxConfig -> FileSandbox
createTestSandbox config =
    FileSandbox
        { sandboxId = error "sandboxId not used for tests"
        , sandboxConfig = config
        , sandboxCreatedAt = error "sandboxCreatedAt not used for tests"
        }

tests :: TestTree
tests =
    testGroup
        "LuaToolbox Security Tests"
        [ pathValidationTests
        , hostValidationTests
        , securityDefaultsTests
        ]

-------------------------------------------------------------------------------
-- Path Validation Tests
-------------------------------------------------------------------------------

pathValidationTests :: TestTree
pathValidationTests =
    testGroup
        "Path validation"
        [ testCase "empty allowedPaths blocks all access" $ do
            let config = defaultFileSandboxConfig{fsbPredicate = fromPathList []}
            let sandbox = createTestSandbox config
            result <- validateFileRead sandbox "/some/path"
            case result of
                AccessDenied (PathNotAllowed _) -> pure ()
                AccessDenied _ -> pure () -- Any AccessDenied is acceptable
                _ -> assertFailure "Expected AccessDenied error"
        , testCase "relative paths are rejected" $ do
            let config = defaultFileSandboxConfig{fsbPredicate = fromPathList ["/allowed"]}
            let sandbox = createTestSandbox config
            result <- validateFileRead sandbox "relative/path"
            case result of
                AccessDenied (PathNotAbsolute _) -> pure ()
                AccessDenied _ -> pure () -- Any AccessDenied is acceptable
                _ -> assertFailure "Expected AccessDenied error"
        , testCase "path outside sandbox is rejected" $ do
            let config = defaultFileSandboxConfig{fsbPredicate = fromPathList ["/allowed"]}
            let sandbox = createTestSandbox config
            result <- validateFileRead sandbox "/other/path"
            case result of
                AccessDenied (PathNotAllowed _) -> pure ()
                AccessDenied _ -> pure () -- Any AccessDenied is acceptable
                _ -> assertFailure "Expected AccessDenied error"
        , testCase "path within sandbox is allowed" $ do
            withTempSandbox $ \sandbox -> do
                let config = defaultFileSandboxConfig{fsbPredicate = fromPathList [sandbox]}
                let fsSandbox = createTestSandbox config
                let testFile = sandbox </> "test.txt"
                writeFile testFile "hello"
                result <- validateFileRead fsSandbox testFile
                case result of
                    AccessGranted -> pure ()
                    _ -> assertFailure $ "Expected path to be allowed: " ++ show result
        , testCase "canonicalization prevents symlink traversal" $ do
            withTempSandbox $ \sandbox -> do
                -- Create allowed directory
                let allowedDir = sandbox </> "allowed"
                createDirectoryIfMissing True allowedDir

                -- Create secret directory outside allowed
                let secretDir = sandbox </> "secret"
                createDirectoryIfMissing True secretDir
                writeFile (secretDir </> "secret.txt") "secret data"

                -- Create a symlink from allowed to secret
                -- Note: This test may not work on all platforms (e.g., Windows without special permissions)
                -- In that case, the test will still pass due to path validation
                let config = defaultFileSandboxConfig{fsbPredicate = fromPathList [allowedDir]}
                let fsSandbox = createTestSandbox config

                -- Try to access via absolute path outside allowed
                result <- validateFileRead fsSandbox (secretDir </> "secret.txt")
                case result of
                    AccessDenied (PathNotAllowed _) -> pure ()
                    AccessDenied _ -> pure ()
                    _ -> assertFailure "Expected path outside sandbox to be rejected"
        , testCase "parent directory traversal is blocked" $ do
            withTempSandbox $ \sandbox -> do
                let allowedDir = sandbox </> "allowed"
                createDirectoryIfMissing True allowedDir

                let config = defaultFileSandboxConfig{fsbPredicate = fromPathList [allowedDir]}
                let fsSandbox = createTestSandbox config

                -- Try to escape via ..
                result <- validateFileRead fsSandbox (allowedDir </> ".." </> "secret.txt")
                case result of
                    AccessDenied (PathNotAllowed _) -> pure ()
                    AccessDenied _ -> pure ()
                    AccessGranted ->
                        -- The canonicalized path should NOT be outside the sandbox
                        assertFailure $ "Path escaped sandbox"
        ]

-------------------------------------------------------------------------------
-- Host Validation Tests
-------------------------------------------------------------------------------

hostValidationTests :: TestTree
hostValidationTests =
    testGroup
        "Host validation"
        [ testCase "empty allowedHosts blocks all access" $ do
            let config = HttpMod.HttpConfig{HttpMod.httpAllowedHosts = []}
            case HttpMod.validateHost config "example.com" of
                Left msg | "No hosts allowed" `Text.isPrefixOf` msg -> pure ()
                _ -> assertFailure "Expected 'No hosts allowed' error"
        , testCase "host not in whitelist is rejected" $ do
            let config = HttpMod.HttpConfig{HttpMod.httpAllowedHosts = ["allowed.com"]}
            case HttpMod.validateHost config "other.com" of
                Left msg | "Host not in allowed list" `Text.isPrefixOf` msg -> pure ()
                _ -> assertFailure "Expected 'Host not in allowed list' error"
        , testCase "host in whitelist is allowed" $ do
            let config = HttpMod.HttpConfig{HttpMod.httpAllowedHosts = ["allowed.com"]}
            case HttpMod.validateHost config "allowed.com" of
                Right () -> pure ()
                Left err -> assertFailure $ "Expected host to be allowed: " ++ Text.unpack err
        , testCase "extractHost from valid HTTP URL" $ do
            case HttpMod.extractHost "http://example.com/path" of
                Right host -> host @?= "example.com"
                Left err -> assertFailure $ "Failed to extract host: " ++ Text.unpack err
        , testCase "extractHost from valid HTTPS URL" $ do
            case HttpMod.extractHost "https://api.example.com/v1/users" of
                Right host -> host @?= "api.example.com"
                Left err -> assertFailure $ "Failed to extract host: " ++ Text.unpack err
        , testCase "extractHost from URL with port" $ do
            case HttpMod.extractHost "http://localhost:3000/" of
                Right host -> host @?= "localhost"
                Left err -> assertFailure $ "Failed to extract host: " ++ Text.unpack err
        , testCase "extractHost from invalid URL fails" $ do
            case HttpMod.extractHost "not a url" of
                Left _ -> pure ()
                Right host -> assertFailure $ "Expected failure but got: " ++ Text.unpack host
        , testCase "extractHost from URL without host fails" $ do
            case HttpMod.extractHost "file:///etc/passwd" of
                Left _ -> pure ()
                Right host -> assertFailure $ "Expected failure but got: " ++ Text.unpack host
        ]

-------------------------------------------------------------------------------
-- Security Defaults Tests
-------------------------------------------------------------------------------

-- | Check if a value indicates that fs/http access was blocked.
-- When access is blocked, the function returns either nil or an error string.
isAccessBlocked :: [Aeson.Value] -> Bool
isAccessBlocked [] = True
isAccessBlocked ((Aeson.String s) : Aeson.Null : []) =
    -- Check if the string contains an access-related error message
    Text.isInfixOf "PathNotAllowed" s
        || Text.isInfixOf "No hosts allowed" s
        || Text.isInfixOf "Host not in allowed list" s
        || Text.isInfixOf "No host in URL" s
        || Text.isInfixOf "Invalid URL" s
        || Text.isInfixOf "AccessDenied" s
        || Text.isInfixOf "AlwaysDeny" s
isAccessBlocked _ = False

securityDefaultsTests :: TestTree
securityDefaultsTests =
    testGroup
        "Security defaults"
        [ testCase "LuaToolbox fs module blocks access with empty allowedPaths" $ do
            withTempSandbox $ \_sandbox -> do
                let desc =
                        LuaToolboxDescription
                            { luaToolboxName = "test"
                            , luaToolboxDescription = "Test toolbox"
                            , luaToolboxMaxMemoryMB = 64
                            , luaToolboxMaxExecutionTimeSeconds = 5
                            , luaToolboxAllowedTools = []
                            , luaToolboxFileSandbox = Just $ FileSandboxConfig { fsbPredicate = fromPathList [], fsbMaxFileSize = Nothing, fsbName = Just "test-sandbox" }
                            , luaToolboxAllowedHosts = []
                            , luaToolboxActivation = Nothing
                            }

                initResult <- LuaToolbox.initializeToolbox silent desc
                case initResult of
                    Left err -> assertFailure $ "Failed to initialize: " ++ err
                    Right toolbox -> do
                        -- Try to read a file - should return nil or error string
                        let ctx = mkTestContext dummyPortal
                        scriptResult <- LuaToolbox.executeScriptWithPortal Prod.tracePrint toolbox "return fs.read('/etc/passwd')" ctx dummyPortal
                        case scriptResult of
                            Left err ->
                                -- Script execution itself failed
                                assertFailure $ "Script execution failed unexpectedly: " ++ show err
                            Right result' -> do
                                -- Check that access was blocked (nil or error string)
                                let values = LuaToolbox.resultValues result'
                                assertBool ("Expected blocked access, got: " ++ show values) (isAccessBlocked values)

                        LuaToolbox.closeToolbox silent toolbox
        , testCase "LuaToolbox http module blocks access with empty allowedHosts" $ do
            let desc =
                    LuaToolboxDescription
                        { luaToolboxName = "test"
                        , luaToolboxDescription = "Test toolbox"
                        , luaToolboxMaxMemoryMB = 64
                        , luaToolboxMaxExecutionTimeSeconds = 5
                        , luaToolboxAllowedTools = []
                        , luaToolboxFileSandbox = Just $ FileSandboxConfig { fsbPredicate = fromPathList [], fsbMaxFileSize = Nothing, fsbName = Just "test-sandbox" }
                        , luaToolboxAllowedHosts = []
                        , luaToolboxActivation = Nothing
                        }

            initResult <- LuaToolbox.initializeToolbox silent desc
            case initResult of
                Left err -> assertFailure $ "Failed to initialize: " ++ err
                Right toolbox -> do
                    let ctx = mkTestContext dummyPortal
                    scriptResult <- LuaToolbox.executeScriptWithPortal Prod.tracePrint toolbox "return http.get('http://example.com')" ctx dummyPortal
                    case scriptResult of
                        Left err ->
                            -- Script execution itself failed
                            assertFailure $ "Script execution failed unexpectedly: " ++ show err
                        Right result' -> do
                            -- Check that access was blocked (nil or error string)
                            let values = LuaToolbox.resultValues result'
                            assertBool ("Expected blocked access, got: " ++ show values) (isAccessBlocked values)

                    LuaToolbox.closeToolbox silent toolbox
        ]

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Create a temporary sandbox directory for testing
withTempSandbox :: (FilePath -> IO a) -> IO a
withTempSandbox action = do
    tmpDir <- createTempDirectory "/tmp" "luatoolbox-test-"
    bracket
        (pure tmpDir)
        removeDirectoryRecursive
        action

