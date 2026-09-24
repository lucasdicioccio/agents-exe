{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for 'System.Agents.CLI.ConfigLoader', the config-loading and
agent-file resolution rules @app/Main.hs@ used to implement inline
(@todos/os-as-standalone-server.md@, gap G7, Phase 1).
-}
module ConfigLoaderTests (tests) where

import qualified Data.Aeson as Aeson
import Data.List (sort)
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Agents.CLI.ConfigLoader as ConfigLoader
import System.Agents.Base (Agent (..), AgentDescription (..))

tests :: TestTree
tests =
    testGroup
        "ConfigLoader"
        [ resolveAgentFilesTests
        , loadAgentsExeConfigTests
        , paramsFileTests
        ]

-------------------------------------------------------------------------------
-- resolveAgentFiles: --agent SLUG narrowing
-------------------------------------------------------------------------------

resolveAgentFilesTests :: TestTree
resolveAgentFilesTests =
    testGroup
        "resolveAgentFiles"
        [ testCase "no slug: files are returned unchanged" $
            withSystemTempDirectory "cfg-loader" $ \dir -> do
                fa <- writeAgentFile dir "a.json" "agent-a"
                fb <- writeAgentFile dir "b.json" "agent-b"
                result <- ConfigLoader.resolveAgentFiles [fa, fb] Nothing
                result @?= Right [fa, fb]
        , testCase "slug matches: narrows to the one file" $
            withSystemTempDirectory "cfg-loader" $ \dir -> do
                fa <- writeAgentFile dir "a.json" "agent-a"
                fb <- writeAgentFile dir "b.json" "agent-b"
                result <- ConfigLoader.resolveAgentFiles [fa, fb] (Just "agent-b")
                result @?= Right [fb]
        , testCase "slug not found: Left with an error listing available slugs" $
            withSystemTempDirectory "cfg-loader" $ \dir -> do
                fa <- writeAgentFile dir "a.json" "agent-a"
                fb <- writeAgentFile dir "b.json" "agent-b"
                result <- ConfigLoader.resolveAgentFiles [fa, fb] (Just "does-not-exist")
                case result of
                    Left err -> do
                        assertBool "mentions the missing slug" ("does-not-exist" `Text.isInfixOf` err)
                        assertBool "lists agent-a" ("agent-a" `Text.isInfixOf` err)
                        assertBool "lists agent-b" ("agent-b" `Text.isInfixOf` err)
                    Right _ -> assertFailure "expected Left, got Right"
        ]

-------------------------------------------------------------------------------
-- loadAgentsExeConfig: locating and parsing agents-exe.cfg.json
-------------------------------------------------------------------------------

loadAgentsExeConfigTests :: TestTree
loadAgentsExeConfigTests =
    testGroup
        "loadAgentsExeConfig"
        [ testCase "no config file: falls back to CONFIGDIR/default/*.json" $
            withSystemTempDirectory "cfg-loader-cwd" $ \cwd ->
                withSystemTempDirectory "cfg-loader-home" $ \home -> do
                    let defaultDir = home </> "default"
                    createDirectoryIfMissing True defaultDir
                    _ <- writeAgentFile defaultDir "x.json" "agent-x"
                    _ <- writeAgentFile defaultDir "y.json" "agent-y"
                    rc <- withCurrentDirectory cwd (ConfigLoader.loadAgentsExeConfig home)
                    rc.rcConfigDir @?= home
                    sort rc.rcAgentFiles @?= sort [defaultDir </> "x.json", defaultDir </> "y.json"]
        , testCase "config file: agentsFiles plus agentsDirectories expansion" $
            withSystemTempDirectory "cfg-loader-cwd" $ \cwd ->
                withSystemTempDirectory "cfg-loader-home" $ \home -> do
                    fa <- writeAgentFile cwd "explicit.json" "agent-explicit"
                    let subdir = cwd </> "more-agents"
                    createDirectoryIfMissing True subdir
                    fb <- writeAgentFile subdir "b.json" "agent-b"
                    fc <- writeAgentFile subdir "c.json" "agent-c"
                    LByteString.writeFile (cwd </> "agents-exe.cfg.json") $
                        Aeson.encode $
                            Aeson.object
                                [ "agentsFiles" Aeson..= [fa]
                                , "agentsDirectories" Aeson..= [subdir]
                                ]
                    rc <- withCurrentDirectory cwd (ConfigLoader.loadAgentsExeConfig home)
                    sort rc.rcAgentFiles @?= sort ([fa] <> [fb, fc])
        ]

-------------------------------------------------------------------------------
-- --params-file
-------------------------------------------------------------------------------

paramsFileTests :: TestTree
paramsFileTests =
    testGroup
        "loadParamsFile"
        [ testCase "plain values" $
            withSystemTempDirectory "cfg-loader-params" $ \dir -> do
                let path = dir </> "params.json"
                LByteString.writeFile path (Aeson.encode (Aeson.object ["greeting" Aeson..= ("hi" :: Text)]))
                result <- ConfigLoader.loadParamsFile path
                case result of
                    Right _ -> pure ()
                    Left err -> assertFailure ("expected success, got: " <> Text.unpack err)
        , testCase "invalid JSON: Left mentions the flag and path" $
            withSystemTempDirectory "cfg-loader-params" $ \dir -> do
                let path = dir </> "bad.json"
                LByteString.writeFile path "not json"
                result <- ConfigLoader.loadParamsFile path
                case result of
                    Left err -> assertBool "mentions --params-file" ("--params-file" `Text.isInfixOf` err)
                    Right _ -> assertFailure "expected Left, got Right"
        ]

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

writeAgentFile :: FilePath -> FilePath -> Text -> IO FilePath
writeAgentFile dir name slugName = do
    createDirectoryIfMissing True dir
    let path = dir </> name
        agent =
            Agent
                { slug = slugName
                , apiKeyId = "main-key"
                , flavor = "OpenAIv1"
                , modelUrl = "https://api.openai.com/v1"
                , modelName = "gpt-4o"
                , announce = "test agent"
                , systemPrompt = ["You are a test agent."]
                , toolDirectory = Nothing
                , bashToolboxes = Nothing
                , mcpServers = Nothing
                , openApiToolboxes = Nothing
                , postgrestToolboxes = Nothing
                , builtinToolboxes = Nothing
                , extraAgents = Nothing
                , skillSources = Nothing
                , autoEnableSkills = Nothing
                , executionMode = Nothing
                , toolCallPolicyConfig = Nothing
                , asyncYieldStrategy = Nothing
                , maxConcurrency = Nothing
                , asyncCallTimeoutSeconds = Nothing
                , bindings = Nothing
                , parameters = Nothing
                , pauseCancelsCalls = Nothing
                , resumeOnAnyMail = Nothing
                , interruptCompletions = Nothing
                , mailInToolResult = Nothing
                , mailScope = Nothing
                , interruptScope = Nothing
                , wakeOn = Nothing
                }
    LByteString.writeFile path (Aeson.encode (AgentDescription agent))
    pure path
