{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for what @agents-exe new agent@ generates, and for how it tells
-- whether the new agent is listed in @agents-exe.cfg.json@.
module NewAgentTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Base (
    Agent (..),
    AgentDescription (..),
    BuiltinToolboxDescription (..),
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
    FileSandboxConfig (..),
    FileSandboxSpec (..),
    SqliteToolboxDescription (..),
    SqliteVersioningConfig (..),
    SystemToolCapability (..),
    SystemToolboxDescription (..),
    resolveBuiltinToolboxSandbox,
 )
import System.Agents.CLI.ConfigLoader (AgentsExeConfig (..))
import System.Agents.CLI.New (
    ConfigUpdateMode (..),
    NewAgentOptions (..),
    addAgentFileToConfig,
    agentListedInConfig,
    buildAgentConfig,
    configEntryForAgent,
    defaultWorkspaceSandbox,
    workspaceSandboxName,
 )
import System.Agents.CLI.New.ModelCatalog (defaultModelCatalog)
import System.Agents.FileSandbox.Predicate (PathPredicate (..))

tests :: TestTree
tests =
    testGroup
        "new agent"
        [ generatedAgentTests
        , configListingTests
        ]

newAgent :: IO Agent
newAgent =
    case buildAgentConfig defaultModelCatalog (NewAgentOptions "helper" "./helper.json" Nothing ConfigUpdateAsk) of
        Left err -> assertFailure err
        Right (_, agent) -> pure agent

generatedAgentTests :: TestTree
generatedAgentTests =
    testGroup
        "generated agent"
        [ testCase "declares a workspace sandbox over the working directory" $ do
            agent <- newAgent
            agent.fileSandboxes @?= Just (Map.singleton workspaceSandboxName defaultWorkspaceSandbox)
            defaultWorkspaceSandbox.fsbPredicate @?= DirectoryRecursive "./"
        , testCase "can read and edit files within the workspace sandbox" $ do
            agent <- newAgent
            case agent.builtinToolboxes of
                Just (DeveloperToolbox dev : _) -> do
                    dev.developerToolboxFileSandbox @?= Just (NamedFileSandbox workspaceSandboxName)
                    mapM_
                        (\cap -> assertBool (show cap) (cap `elem` dev.developerToolboxCapabilities))
                        [DevToolReadFileRange, DevToolWriteFileRange, DevToolPatchFile]
                other -> assertFailure ("unexpected toolboxes: " <> show other)
        , testCase "can list directories within the workspace sandbox" $ do
            agent <- newAgent
            case agent.builtinToolboxes of
                Just [_, SystemToolbox sys, _] -> do
                    sys.systemToolboxFileSandbox @?= Just (NamedFileSandbox workspaceSandboxName)
                    assertBool "list-directory" (SystemToolListDirectory `elem` sys.systemToolboxCapabilities)
                other -> assertFailure ("unexpected toolboxes: " <> show other)
        , testCase "has a read-write memory database named after the agent" $ do
            agent <- newAgent
            case agent.builtinToolboxes of
                Just [_, _, SqliteToolbox mem] -> do
                    mem.sqliteToolboxName @?= "memory"
                    mem.sqliteToolboxVersioning @?= SqliteReadWrite "./helper-memory.sqlite"
                other -> assertFailure ("unexpected toolboxes: " <> show other)
        , testCase "sandbox references resolve against the agent's fileSandboxes" $ do
            agent <- newAgent
            let named = maybe Map.empty id agent.fileSandboxes
            case traverse (resolveBuiltinToolboxSandbox named) (maybe [] id agent.builtinToolboxes) of
                Left err -> assertFailure (show err)
                Right _ -> pure ()
        , testCase "round-trips through the agent file format" $ do
            agent <- newAgent
            Aeson.eitherDecode (Aeson.encode (AgentDescription agent)) @?= Right (AgentDescription agent)
        ]

config :: [FilePath] -> [FilePath] -> AgentsExeConfig
config dirs files =
    AgentsExeConfig
        { agentsConfigDir = Nothing
        , agentsDirectories = dirs
        , agentsFiles = files
        , agentsLogs = Nothing
        , cfgPromptAliases = Nothing
        , cfgSelfDescribeSlug = Nothing
        , cfgSelfDescribeDescription = Nothing
        , cfgKeymapPath = Nothing
        , cfgSessions = Nothing
        , cfgTramajLibraries = []
        }

configListingTests :: TestTree
configListingTests =
    testGroup
        "listing in agents-exe.cfg.json"
        [ testCase "an agent named in agentsFiles is listed" $ do
            agentListedInConfig "/work" (config [] ["./helper.json"]) "helper.json" @?= True
            agentListedInConfig "/work" (config [] ["/work/helper.json"]) "./helper.json" @?= True
        , testCase "an agent directly in an agentsDirectories entry is listed" $ do
            agentListedInConfig "/work" (config ["./agents/"] []) "agents/helper.json" @?= True
            agentListedInConfig "/work" (config ["agents"] []) "./agents/helper.json" @?= True
        , testCase "agentsDirectories do not recurse, and only pick up .json files" $ do
            agentListedInConfig "/work" (config ["./agents/"] []) "agents/sub/helper.json" @?= False
            agentListedInConfig "/work" (config ["./agents/"] []) "agents/helper.txt" @?= False
        , testCase "an agent elsewhere is not listed" $
            agentListedInConfig "/work" (config ["./agents/"] ["./other.json"]) "helper.json" @?= False
        , testCase "the entry is relative when the config sits in the working directory" $ do
            configEntryForAgent "/work" "/work/agents-exe.cfg.json" "agents/helper.json" @?= "./agents/helper.json"
            configEntryForAgent "/work/sub" "/work/agents-exe.cfg.json" "helper.json" @?= "/work/sub/helper.json"
        , testCase "adding to agentsFiles keeps the other fields" $ do
            let before = Aeson.object ["agentsFiles" Aeson..= ["./a.json" :: String], "selfDescribeSlug" Aeson..= ("x" :: String)]
                after = Aeson.object ["agentsFiles" Aeson..= ["./a.json" :: String, "./b.json"], "selfDescribeSlug" Aeson..= ("x" :: String)]
            addAgentFileToConfig "./b.json" before @?= Right after
        , testCase "adding to a config without agentsFiles creates it" $
            addAgentFileToConfig "./b.json" (Aeson.object [])
                @?= Right (Aeson.object ["agentsFiles" Aeson..= ["./b.json" :: String]])
        , testCase "a malformed agentsFiles is refused" $
            addAgentFileToConfig "./b.json" (Aeson.object ["agentsFiles" Aeson..= ("x" :: String)])
                @?= Left "agentsFiles is not an array"
        ]
