{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'tui' command handler.

The tui command launches an interactive terminal user interface for
chatting with agents.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.TUI (
    -- * Types
    TuiOptions (..),

    -- * Handler
    handleTUI,
) where

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (AgentId, AgentSlug)
import System.Agents.Runtime.Trace (Trace)
import qualified System.Agents.SessionStore as SessionStore
import qualified System.Agents.TUI.Core as TUI
import System.Agents.ToolRegistration (ToolRegistration)

-- | Options for the TUI command
data TuiOptions = TuiOptions
    {
    }
    deriving (Show)

-- | Handle the TUI command: launch interactive terminal interface
handleTUI ::
    -- | Base tracer for logging
    Prod.Tracer IO AgentTree.TreeTrace ->
    -- | Tracer for sub-agent calls (conversation traces)
    Prod.Tracer IO Trace ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files to load
    [FilePath] ->
    IO ()
handleTUI baseTracer subAgentTracer sessionStore apiKeysFile agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    let oneAgent agentFile = do
            pure $
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.interactiveTracer = baseTracer
                    , AgentTree.agentToTool = makeAgentTool sessionStore apiKeys subAgentTracer
                    }
    -- Use traverse to sequence the IO actions for creating Props
    agentPropsList <- traverse oneAgent agentFiles
    TUI.runTUI sessionStore apiKeys agentPropsList

{- | Create an agent tool function with session tracking and tracing.

This wraps 'turnAgentRuntimeIntoIOTool' with callbacks for session tracking
and a tracer for capturing sub-agent traces. The tracer allows parent
conversations to monitor sub-agent execution including:

* When sub-agents start and complete
* LLM calls made by sub-agents
* Tool calls made by sub-agents
* Success/failure status
-}
makeAgentTool ::
    SessionStore.SessionStore ->
    AgentTree.LoadedApiKeys ->
    Prod.Tracer IO Trace ->
    AgentTree.OSAgentNode ->
    AgentSlug ->
    AgentId ->
    ToolRegistration
makeAgentTool store apiKeys tracer node slug agentId =
    OneShotTool.turnAgentRuntimeIntoIOTool
        store
        apiKeys
        node
        slug
        agentId
        OneShotTool.defaultAgentCallCallbacks
        tracer
        OneShotTool.defaultParentSessionLookup
