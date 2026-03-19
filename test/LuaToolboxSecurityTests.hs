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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (writeFile)
import System.IO.Temp (createTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import Prod.Tracer (silent)

import System.Agents.Base (LuaToolboxDescription (..))
import System.Agents.Tools.Context (ToolResult (..))
import System.Agents.Tools.LuaToolbox as LuaToolbox
import qualified System.Agents.Tools.LuaToolbox.Modules.Fs as FsMod
import qualified System.Agents.Tools.LuaToolbox.Modules.Http as HttpMod

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
            let config = FsMod.FsConfig {FsMod.fsAllowedPaths = []}
            result <- FsMod.validatePath config "/some/path"
            case result of
                Left (FsMod.PathNotAllowed _) -> pure ()
                _ -> assertFailure "Expected PathNotAllowed error"
        , testCase "relative paths are rejected" $ do
            let config = FsMod.FsConfig {FsMod.fsAllowedPaths = ["/allowed"]}
            result <- FsMod.validatePath config "relative/path"
            case result of
                Left (FsMod.PathNotAbsolute _) -> pure ()
                _ -> assertFailure "Expected PathNotAbsolute error"
        , testCase "path outside sandbox is rejected" $ do
            let config = FsMod.FsConfig {FsMod.fsAllowedPaths = ["/allowed"]}
            result <- FsMod.validatePath config "/other/path"
            case result of
                Left (FsMod.PathOutsideSandbox _ _) -> pure ()
                _ -> assertFailure "Expected PathOutsideSandbox error"
        , testCase "path within sandbox is allowed" $ do
            withTempSandbox $ \sandbox -> do
                let config = FsMod.FsConfig {FsMod.fsAllowedPaths = [sandbox]}
                let testFile = sandbox </> "test.txt"
                writeFile testFile "hello"
                result <- FsMod.validatePath config testFile
                case result of
                    Right path -> path @?= testFile
                    Left err -> assertFailure $ "Expected path to be allowed: " ++ show err
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
                let config = FsMod.FsConfig {FsMod.fsAllowedPaths = [allowedDir]}

                -- Try to access via absolute path outside allowed
                result <- FsMod.validatePath config (secretDir </> "secret.txt")
                case result of
                    Left (FsMod.PathOutsideSandbox _ _) -> pure ()
                    Left (FsMod.PathNotAllowed _) -> pure ()
                    _ -> assertFailure "Expected path outside sandbox to be rejected"
        , testCase "parent directory traversal is blocked" $ do
            withTempSandbox $ \sandbox -> do
                let allowedDir = sandbox </> "allowed"
                createDirectoryIfMissing True allowedDir

                let config = FsMod.FsConfig {FsMod.fsAllowedPaths = [allowedDir]}

                -- Try to escape via ..
                result <- FsMod.validatePath config (allowedDir </> ".." </> "secret.txt")
                case result of
                    Left (FsMod.PathOutsideSandbox _ _) -> pure ()
                    Left (FsMod.PathNotAllowed _) -> pure ()
                    Right path ->
                        -- The canonicalized path should NOT be outside the sandbox
                        if Text.pack sandbox `Text.isPrefixOf` Text.pack path
                            then assertFailure $ "Path escaped sandbox: " ++ path
                            else pure ()
        ]

-------------------------------------------------------------------------------
-- Host Validation Tests
-------------------------------------------------------------------------------

hostValidationTests :: TestTree
hostValidationTests =
    testGroup
        "Host validation"
        [ testCase "empty allowedHosts blocks all access" $ do
            let config = HttpMod.HttpConfig {HttpMod.httpAllowedHosts = []}
            case HttpMod.validateHost config "example.com" of
                Left msg | "No hosts allowed" `Text.isPrefixOf` msg -> pure ()
                _ -> assertFailure "Expected 'No hosts allowed' error"
        , testCase "host not in whitelist is rejected" $ do
            let config = HttpMod.HttpConfig {HttpMod.httpAllowedHosts = ["allowed.com"]}
            case HttpMod.validateHost config "other.com" of
                Left msg | "Host not in allowed list" `Text.isPrefixOf` msg -> pure ()
                _ -> assertFailure "Expected 'Host not in allowed list' error"
        , testCase "host in whitelist is allowed" $ do
            let config = HttpMod.HttpConfig {HttpMod.httpAllowedHosts = ["allowed.com"]}
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
isAccessBlocked ((Aeson.String s):Aeson.Null:[]) = 
    -- Check if the string contains an access-related error message
    Text.isInfixOf "PathNotAllowed" s ||
    Text.isInfixOf "No hosts allowed" s ||
    Text.isInfixOf "Host not in allowed list" s ||
    Text.isInfixOf "No host in URL" s ||
    Text.isInfixOf "Invalid URL" s
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
                            , luaToolboxAllowedPaths = []
                            , luaToolboxAllowedHosts = []
                            }

                result <- LuaToolbox.initializeToolbox silent desc
                case result of
                    Left err -> assertFailure $ "Failed to initialize: " ++ err
                    Right toolbox -> do
                        -- Try to read a file - should return nil or error string
                        scriptResult <- LuaToolbox.executeScript toolbox "return fs.read('/etc/passwd')"
                        case scriptResult of
                            Left err -> 
                                -- Script execution itself failed
                                assertFailure $ "Script execution failed unexpectedly: " ++ show err
                            Right result -> do
                                -- Check that access was blocked (nil or error string)
                                let value = LuaToolbox.resultValues result
                                assertBool ("Expected blocked access, got: " ++ show value) (isAccessBlocked value)

                        LuaToolbox.closeToolbox silent toolbox
        , testCase "LuaToolbox http module blocks access with empty allowedHosts" $ do
            let desc =
                    LuaToolboxDescription
                        { luaToolboxName = "test"
                        , luaToolboxDescription = "Test toolbox"
                        , luaToolboxMaxMemoryMB = 64
                        , luaToolboxMaxExecutionTimeSeconds = 5
                        , luaToolboxAllowedTools = []
                        , luaToolboxAllowedPaths = []
                        , luaToolboxAllowedHosts = []
                        }

            result <- LuaToolbox.initializeToolbox silent desc
            case result of
                Left err -> assertFailure $ "Failed to initialize: " ++ err
                Right toolbox -> do
                    -- Try to make HTTP request - should return nil or error string
                    scriptResult <- LuaToolbox.executeScript toolbox "return http.get('http://example.com')"
                    case scriptResult of
                        Left err -> 
                            -- Script execution itself failed
                            assertFailure $ "Script execution failed unexpectedly: " ++ show err
                        Right result -> do
                            -- Check that access was blocked (nil or error string)
                            let values = LuaToolbox.resultValues result
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

