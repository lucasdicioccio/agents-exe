{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'tui' command handler.

The tui command launches an interactive terminal user interface for
chatting with agents.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.TUI (
    Trace(..),
    -- * Types
    TuiOptions (..),

    -- * Handler
    handleTUI,
) where

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.SessionStore as SessionStore
import qualified System.Agents.TUI.Core as TUI

data Trace
  = TUITrace !TUI.Trace
  | AgentTreeTrace !AgentTree.TreeTrace
  | OneShotToolTrace !OneShotTool.Trace
  deriving (Show)

-- | Options for the TUI command
data TuiOptions = TuiOptions
    {
    }
    deriving (Show)

-- | Handle the TUI command: launch interactive terminal interface
handleTUI ::
    -- | Base tracer for logging
    Prod.Tracer IO Trace ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files to load
    [FilePath] ->
    IO ()
handleTUI tracer sessionStore apiKeysFile agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    let oneAgent agentFile = do
            pure $
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.apiKeysFile = apiKeysFile
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.interactiveTracer = (Prod.contramap AgentTreeTrace tracer)
                    , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool (Prod.contramap OneShotToolTrace tracer) sessionStore apiKeys
                    }
    -- Use traverse to sequence the IO actions for creating Props
    agentPropsList <- traverse oneAgent agentFiles
    TUI.runTUI (Prod.contramap TUITrace tracer) sessionStore apiKeys agentPropsList
