{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module for the 'check' command handler.
--
-- The check command validates agent configuration files and displays
-- information about loaded agents including their tools.
module System.Agents.CLI.Check
    ( handleCheck
    , printAgentCheck
    ) where

import Control.Monad (forM_)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.Runtime as Runtime
import qualified System.Agents.SessionStore as SessionStore

-- | Handle the check command: validate and display agent configuration
handleCheck ::
    -- | Path to the API keys file
    FilePath ->
    -- | List of agent files to check
    [FilePath] ->
    IO ()
handleCheck apiKeysFile agentFiles = do
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
                AgentTree.Initialized tree -> printAgentCheck tree

-- | Display agent check information
printAgentCheck :: AgentTree.AgentTree -> IO ()
printAgentCheck tree = do
    tools <- Runtime.agentTools tree.agentRuntime
    let toolCount = length tools
    Text.putStrLn $
        Runtime.agentSlug tree.agentRuntime
        <> ": "
        <> Runtime.agentAnnounce tree.agentRuntime
        <> " ("
        <> Text.pack (show toolCount)
        <> " tools)"

