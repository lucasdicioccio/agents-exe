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

import Brick.BChan (writeBChan)
import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (AgentId, AgentSlug)
import qualified System.Agents.SessionStore as SessionStore
import qualified System.Agents.TUI.Core as TUI
import System.Agents.TUI.Types (AppEvent (..))
import System.Agents.ToolRegistration (ToolRegistration)

-- | Options for the TUI command
data TuiOptions = TuiOptions
    {
    }
    deriving (Show)

{- | Handle the TUI command: launch interactive terminal interface

This function creates Props where sub-agent traces are wrapped in
'SubAgentTrace' and written to the TUI's event channel, allowing
LLM tool calls and sub-agent activities to be traced and displayed
in the TUI's debug view.
-}
handleTUI ::
    -- | Unified tracer for all events
    Prod.Tracer IO AgentTree.TreeTrace ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files to load
    [FilePath] ->
    IO ()
handleTUI tracer sessionStore apiKeysFile agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    -- The sub-agent tracer will be created in runTUI where the event channel is available
    -- We pass a function that creates Props given the event channel
    let makeProps evChan agentFile = do
            -- Create a tracer that extracts SubAgentTrace events and writes to the event channel
            let subAgentTracer = Prod.Tracer $ \treeTr ->
                    case treeTr of
                        AgentTree.SubAgentTrace tr -> writeBChan evChan (AppEvent_AgentTrace tr)
                        _ -> pure ()
            -- Combine the tracers: both receive TreeTrace events
            let combinedTracer = Prod.traceBoth tracer subAgentTracer
            pure $
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.tracer = combinedTracer
                    , AgentTree.agentToTool = makeAgentTool sessionStore apiKeys combinedTracer
                    }
    TUI.runTUIWithTracer sessionStore apiKeys makeProps agentFiles

{- | Create an agent tool function with session tracking and tracing.

This wraps 'turnAgentRuntimeIntoIOTool' with callbacks for session tracking
and a tracer for capturing sub-agent traces. The tracer allows parent
conversations to monitor sub-agent execution including:

* When sub-agents start and complete
* LLM calls made by sub-agents
* Tool calls made by sub-agents
* Success/failure status

The parent session lookup uses the SessionStore to establish parent-child
relationships between conversations, enabling the TUI to display hierarchical
conversation trees.

Sub-agent traces are wrapped in 'SubAgentTrace' constructor for unified tracing.
-}
makeAgentTool ::
    SessionStore.SessionStore ->
    AgentTree.LoadedApiKeys ->
    Prod.Tracer IO AgentTree.TreeTrace ->
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
        (Prod.contramap AgentTree.SubAgentTrace tracer)
        (OneShotTool.sessionIdFromConversationId store)

