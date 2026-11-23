{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'check' command handler.

The check command validates agent configuration files and displays
information about loaded agents including their tools.

With @--probe@ (@todos/bash-tools-v2.md@ §1.5) it also runs the @check@ verb
of every bash tool the agents load, under the tool's real @runtime@, and
reports each one as @ok@, @failed@, @unverified@ or @skipped@, with what the
operator's @--sandbox@ resolved to for it; @--json@ prints that report as
JSON. A capability (§1.5) is reported with every backend probed, in the
order they are tried (after the operator's @--backend@), and its active
backend; any other bash tool binary is a capability of its own. Exit codes
with @--probe@: 1 when a configuration does not load, 2 when it loads but a
tool, or a required capability, has no active backend, 0 otherwise.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.Check (
    Trace (..),
    handleCheck,
    printAgentCheck,
    ToolsOutputMode (..),
    CheckOptions (..),
) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM, forM_, unless, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentFactory as AgentFactory
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ToolDescription (..), ToolName (..))
import System.Agents.Tools.Base (Tool (..), ToolDef (..))
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.Params.Types as Params
import qualified System.Agents.Tools.ScriptCheck as ScriptCheck
import qualified System.Agents.Base as Base
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.Capability as Capability
import System.Agents.Tools.ScriptRuntime (ToolRuntimeConfig (..), getToolRuntimeConfig)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import qualified Data.List as List

data Trace
    = AgentTreeTrace !AgentTree.TreeTrace
    | OneShotToolTrace !OneShotTool.Trace
    deriving (Show)

-- | How to display tool information in the check output
data ToolsOutputMode
    = -- | No tool information (default)
      ToolsNone
    | -- | List tool names in textual form
      ToolsList
    | -- | Show the JSON description of the internal representation
      ToolsAgentsExe
    | -- | Show the OpenAI.LLM JSON representation sent to LLM completion calls
      ToolsOpenAI
    deriving (Show, Eq)

-- | Options for the check command
data CheckOptions = CheckOptions
    { toolsOutputMode :: ToolsOutputMode
    , showConfig :: Bool
    -- ^ Print the agent file's JSON; for a @.tramaj@ template, the JSON it evaluates to.
    , probe :: Bool
    -- ^ Run every bash tool's @check@ verb and report it (@--probe@).
    , probeJson :: Bool
    -- ^ Print the probe report as JSON (@--json@, with @--probe@).
    }
    deriving (Show, Eq)

-- | Handle the check command: validate and display agent configuration
handleCheck ::
    Prod.Tracer IO Trace ->
    -- | Check options
    CheckOptions ->
    -- | Path to the API keys file
    FilePath ->
    -- | Operator-supplied parameter values (@--set@/@--pin@)
    Params.ProcessParams ->
    -- | Libraries @.tramaj@ agent files may import
    FileLoader.TemplateLibraries ->
    -- | List of agent files to check
    [FilePath] ->
    IO ()
handleCheck tracer opts apiKeysFile processParams templateLibraries agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    loadFailed <- newIORef False
    scripts <- newIORef []
    capabilities <- newIORef []
    let quiet = opts.probe && opts.probeJson

    forM_ agentFiles $ \agentFile -> do
        when opts.showConfig $ printEvaluatedConfig (FileLoader.TemplateEnv templateLibraries processParams) agentFile
        AgentTree.withAgentTree
            AgentTree.Props
                { AgentTree.apiKeys = apiKeys
                , AgentTree.apiKeysFile = apiKeysFile
                , AgentTree.rootAgentFile = agentFile
                , AgentTree.interactiveTracer = Prod.contramap AgentTreeTrace tracer
                , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool (Prod.contramap OneShotToolTrace tracer) (AgentFactory.fileAgentDeps SessionStore.defaultSessionStore apiKeys)
                , AgentTree.sessionCatalog = SessionStore.fileCatalog SessionStore.defaultSessionStore
                , AgentTree.processParams = processParams
                , AgentTree.templateLibraries = templateLibraries
                }
            $ \result -> case result of
                AgentTree.Errors errs -> do
                    modifyIORef' loadFailed (const True)
                    if quiet then mapM_ (Text.hPutStrLn stderr . Text.pack . show) errs else mapM_ print errs
                AgentTree.Initialized tree -> do
                    -- Print agent check information
                    unless quiet $ printAgentCheckOS opts tree
                    when opts.probe $ do
                        found <- treeBashScripts tree
                        modifyIORef' scripts (<> found)
                        modifyIORef' capabilities (<> treeCapabilities tree)

    when opts.probe $ do
        failed <- readIORef loadFailed
        found <- readIORef scripts
        caps <- List.nub <$> readIORef capabilities
        binaryReports <- forM (ScriptCheck.toolBinaries found) $ \(desc, names) -> do
            r <- ScriptCheck.probeScript names desc
            pure (ScriptCheck.capabilityReport r.probeName [r])
        capReports <- forM caps probeCapabilityDescription
        let reports = binaryReports <> map snd capReports
        if opts.probeJson
            then LBS8.putStrLn (Aeson.encodePretty reports)
            else printProbeReport binaryReports capReports
        let missing =
                not (all ScriptCheck.hasActiveBackend binaryReports)
                    || or [Base.capabilityIsRequired c && not (ScriptCheck.hasActiveBackend r) | (c, r) <- capReports]
        if failed
            then exitWith (ExitFailure 1)
            else when missing $ exitWith (ExitFailure 2)

-- | Probes every backend of a capability, in the order they are tried.
probeCapabilityDescription :: Base.CapabilityDescription -> IO (Base.CapabilityDescription, ScriptCheck.CapabilityReport)
probeCapabilityDescription cap = do
    cfg <- getToolRuntimeConfig
    cwd <- getCurrentDirectory
    let paths = [if BashToolbox.isRelative p then cwd </> p else p | p <- cap.capBackends]
    backends <- Capability.loadBackendsWith cfg Prod.silent paths
    report <- Capability.probeCapability cfg cap.capName (Capability.orderBackends cfg.backendOverrides cap.capName backends)
    pure (cap, report)

-- | Every capability declared by any agent of the tree.
treeCapabilities :: AgentTree.OSAgentTree -> [Base.CapabilityDescription]
treeCapabilities tree =
    [ c
    | node <- Map.elems (AgentTree.osTreeNodes tree)
    , Base.Capability c <- maybe [] id (Base.bashToolboxes node.osNodeConfig)
    ]

{- | Every bash tool loaded by any agent of the tree, except the active
backends of capabilities, which are reported with their capability.
-}
treeBashScripts :: AgentTree.OSAgentTree -> IO [Bash.ScriptDescription]
treeBashScripts tree = do
    perNode <- forM (Map.elems (AgentTree.osTreeNodes tree)) $ \node -> do
        tools <- readTVarIO (AgentTree.osNodeTools node)
        let capNames = [c.capName | Base.Capability c <- maybe [] id (Base.bashToolboxes node.osNodeConfig)]
        pure
            [ script
            | tool <- tools
            , BashTool script <- [tool.innerTool.toolDef]
            , script.scriptInfo.scriptSlug `notElem` capNames
            ]
    pure (concat perNode)

printProbeReport :: [ScriptCheck.CapabilityReport] -> [(Base.CapabilityDescription, ScriptCheck.CapabilityReport)] -> IO ()
printProbeReport binaries caps = do
    Text.putStrLn ""
    Text.putStrLn "**Probes:**"
    when (null binaries && null caps) $ Text.putStrLn "(no bash tools)"
    forM_ binaries $ \c -> forM_ c.capabilityBackends $ \p ->
        Text.putStrLn ("- " <> ScriptCheck.renderProbeLine p)
    forM_ caps $ \(cap, c) -> do
        Text.putStrLn $
            "- capability "
                <> c.capabilityName
                <> (if Base.capabilityIsRequired cap then "" else " (optional)")
                <> ": "
                <> maybe "no active backend" ("active " <>) c.capabilityActive
        forM_ c.capabilityBackends $ \p ->
            Text.putStrLn ("  - " <> ScriptCheck.renderProbeLine p)

{- | Print the JSON an agent file holds, as loading sees it: a @.tramaj@
template is evaluated first. A file that does not load prints nothing here;
loading reports the error.
-}
printEvaluatedConfig :: FileLoader.TemplateEnv -> FilePath -> IO ()
printEvaluatedConfig env agentFile = do
    result <- FileLoader.readAgentValueFile env agentFile
    case result of
        Left _ -> pure ()
        Right (value, _) -> LBS8.putStrLn (Aeson.encodePretty value)

-- | Print agent check information using OS-native structures
printAgentCheckOS :: CheckOptions -> AgentTree.OSAgentTree -> IO ()
printAgentCheckOS opts tree = do
    let rootNode = AgentTree.osTreeRoot tree

    -- Get tools from OS-native TVar
    tools <- readTVarIO (AgentTree.osNodeTools rootNode)
    let toolCount = length tools

    -- Access agent info from the node
    let agent = rootNode.osNodeConfig
    Text.putStrLn $
        agent.slug
            <> ": "
            <> agent.announce
            <> " ("
            <> Text.pack (show toolCount)
            <> " tools)"

    -- Print tool information based on the selected mode
    case opts.toolsOutputMode of
        ToolsNone -> pure ()
        ToolsList -> printToolsList tools
        ToolsAgentsExe -> printToolsAgentsExe tools
        ToolsOpenAI -> printToolsOpenAI tools

-- | Display agent check information (public API preserved for compatibility)
printAgentCheck :: CheckOptions -> AgentTree.AgentConfigTree -> IO ()
printAgentCheck opts configTree = do
    -- For backward compatibility, use a minimal display
    let agent = configTree.agentConfig
    Text.putStrLn $ agent.slug <> ": " <> agent.announce

    case opts.toolsOutputMode of
        ToolsNone -> pure ()
        _ -> Text.putStrLn "(Tool details require full agent loading)"

-- | Print tools as a simple list
printToolsList :: [ToolRegistration] -> IO ()
printToolsList tools = do
    Text.putStrLn ""
    Text.putStrLn "**Tools:**"
    forM_ tools $ \tool -> do
        let toolDecl = declareTool tool
        let name = toolDecl.toolDescriptionName.getToolName
        let desc = toolDecl.toolDescriptionText
        Text.putStrLn $ "- **" <> name <> "**: " <> desc
    Text.putStrLn ""

-- | Print tools as agents-exe JSON representation
printToolsAgentsExe :: [ToolRegistration] -> IO ()
printToolsAgentsExe tools = do
    Text.putStrLn ""
    Text.putStrLn "<details>"
    Text.putStrLn "<summary>agents-exe tool representation</summary>"
    Text.putStrLn ""
    Text.putStrLn "```json"
    forM_ tools $ \tool -> do
        let toolDecl = declareTool tool
        let jsonVal =
                Aeson.object
                    [ "name" Aeson..= toolDecl.toolDescriptionName.getToolName
                    , "description" Aeson..= toolDecl.toolDescriptionText
                    , "paramProperties" Aeson..= toolDecl.toolDescriptionParamProperties
                    ]
        LBS8.putStrLn $ Aeson.encodePretty jsonVal
    Text.putStrLn "```"
    Text.putStrLn "</details>"
    Text.putStrLn ""

-- | Print tools as OpenAI LLM JSON representation
printToolsOpenAI :: [ToolRegistration] -> IO ()
printToolsOpenAI tools = do
    Text.putStrLn ""
    Text.putStrLn "<details>"
    Text.putStrLn "<summary>OpenAI LLM tool representation</summary>"
    Text.putStrLn ""
    Text.putStrLn "```json"
    let openAiTools = map declareTool tools
    LBS8.putStrLn $ Aeson.encodePretty openAiTools
    Text.putStrLn "```"
    Text.putStrLn "</details>"
    Text.putStrLn ""
