{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'check' command handler.

The check command validates agent configuration files and displays
information about loaded agents including their tools.
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
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Runtime as Runtime
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

-- | Default check options
defaultCheckOptions :: CheckOptions
defaultCheckOptions = CheckOptions{toolsOutputMode = ToolsNone}

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
        -- Create a new runtime registry for this agent tree
        registry <- AgentTree.newRuntimeRegistry
        -- Use silent tracer to suppress diagnostic output during agent loading
        AgentTree.withAgentTreeRuntime
            AgentTree.Props
                { AgentTree.apiKeys = apiKeys
                , AgentTree.rootAgentFile = agentFile
                , AgentTree.interactiveTracer = Prod.silent
                , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool SessionStore.defaultSessionStore
                , AgentTree.runtimeRegistry = registry
                }
            $ \result -> case result of
                AgentTree.Errors errs -> mapM_ print errs
                AgentTree.Initialized tree -> printAgentCheck opts tree

-- | Display agent check information
printAgentCheck :: CheckOptions -> AgentTree.AgentTree -> IO ()
printAgentCheck opts tree = do
    tools <- readTVarIO $ Runtime.agentTools tree.agentRuntime
    let toolCount = length tools
    Text.putStrLn $
        Runtime.agentSlug tree.agentRuntime
            <> ": "
            <> Runtime.agentAnnounce tree.agentRuntime
            <> " ("
            <> Text.pack (show toolCount)
            <> " tools)"

    -- Print tool information based on the selected mode
    case opts.toolsOutputMode of
        ToolsNone -> pure ()
        ToolsList -> printToolsList tools
        ToolsAgentsExe -> printToolsAgentsExe tools
        ToolsOpenAI -> printToolsOpenAI tools

    -- Recursively print child agents
    forM_ tree.agentChildren (printAgentCheck opts)

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

