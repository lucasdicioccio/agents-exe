{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'check' command handler.

The check command validates agent configuration files and displays
information about loaded agents including their tools.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.Check (
    handleCheck,
    printAgentCheck,
    ToolsOutputMode (..),
    CheckOptions (..),
) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (ToolRegistration (..))

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
    }
    deriving (Show, Eq)

-- | Handle the check command: validate and display agent configuration
handleCheck ::
    -- | Check options
    CheckOptions ->
    -- | Path to the API keys file
    FilePath ->
    -- | List of agent files to check
    [FilePath] ->
    IO ()
handleCheck opts apiKeysFile agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile

    forM_ agentFiles $ \agentFile -> do
        -- Use silent tracer to suppress diagnostic output during agent loading
        AgentTree.withAgentTree
            AgentTree.Props
                { AgentTree.apiKeys = apiKeys
                , AgentTree.rootAgentFile = agentFile
                , AgentTree.interactiveTracer = Prod.silent
                , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool SessionStore.defaultSessionStore apiKeys
                }
            $ \result -> case result of
                AgentTree.Errors errs -> mapM_ print errs
                AgentTree.Initialized tree -> do
                    -- Print agent check information
                    printAgentCheckOS opts tree

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
        let name = OpenAI.getToolName $ OpenAI.toolName toolDecl
        let desc = OpenAI.toolDescription toolDecl
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
                    [ "name" Aeson..= OpenAI.getToolName (OpenAI.toolName toolDecl)
                    , "description" Aeson..= OpenAI.toolDescription toolDecl
                    , "paramProperties" Aeson..= OpenAI.toolParamProperties toolDecl
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
