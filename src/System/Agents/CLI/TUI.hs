{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'tui' command handler.

The tui command launches an interactive terminal user interface for
chatting with agents.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.TUI (
    Trace (..),

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

-- | Trace type for TUI command.
data Trace
    = TUITrace !TUI.Trace
    | AgentTreeTrace !AgentTree.TreeTrace
    | OneShotToolTrace !OneShotTool.Trace
    deriving (Show)

-- | Options for the TUI command
data TuiOptions = TuiOptions
    { tuiKeymapPath :: Maybe FilePath
    -- ^ Optional path to keymap configuration file
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
    -- | Optional path to keymap file
    Maybe FilePath ->
    -- | List of agent files to load
    [FilePath] ->
    IO ()
handleTUI tracer sessionStore apiKeysFile mKeymapPath agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile

    -- Load keymap if a path is provided, otherwise use default
    keymap <- case mKeymapPath of
        Just path -> TUI.loadKeymapFromFile path
        Nothing -> pure TUI.defaultKeyMapping

    let oneAgent agentFile = do
            pure $
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.apiKeysFile = apiKeysFile
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.interactiveTracer = (Prod.contramap AgentTreeTrace tracer)
                    , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool (Prod.contramap OneShotToolTrace tracer) sessionStore apiKeys
                    , AgentTree.sessionStore = sessionStore
                    }
    -- Use traverse to sequence the IO actions for creating Props
    agentPropsList <- traverse oneAgent agentFiles
    TUI.runTUIWithKeymap (Prod.contramap TUITrace tracer) sessionStore apiKeys keymap agentPropsList

