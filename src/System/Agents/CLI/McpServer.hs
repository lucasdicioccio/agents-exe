{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'mcp-server' command handler.

The mcp-server command starts a Model Context Protocol server that
exposes agents as MCP tools for integration with MCP clients.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.McpServer (
    -- * Handler
    handleMcpServer,

    -- * Types
    McpServerAgent (..),

    -- * Agent creation
    createMcpServerAgent,

    -- * Tool access
    listMcpServerAgentTools,
) where

import Control.Concurrent.STM (readTVarIO)
import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (AgentId, AgentSlug)
import System.Agents.CLI.TraceUtils (traceUsefulPromptStderr)
import qualified System.Agents.MCP.Server as McpServer
import System.Agents.Runtime.Trace (Trace)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (ToolRegistration)

import System.Agents.AgentTree (OSAgentNode (..), OSAgentTree (..))

{- | MCP Server agent using OS-native structures.

This structure wraps an OSAgentNode for MCP server operation.
-}
data McpServerAgent = McpServerAgent
    { mcpAgentId :: AgentId
    -- ^ Unique identifier for this agent
    , mcpTree :: OSAgentTree
    -- ^ The agent's tree structure
    , mcpNode :: OSAgentNode
    -- ^ The specific node for this agent
    }

-- | Manual Show instance for McpServerAgent.
instance Show McpServerAgent where
    show agent =
        "McpServerAgent {mcpAgentId = "
            ++ show agent.mcpAgentId
            ++ ", mcpTree = <OSAgentTree>, mcpNode = <OSAgentNode>}"

{- | Create a McpServerAgent from an OSAgentTree.

This function extracts the root agent from the tree for MCP server operation.
-}
createMcpServerAgent :: OSAgentTree -> McpServerAgent
createMcpServerAgent tree =
    let rootNode = osTreeRoot tree
     in McpServerAgent
            { mcpAgentId = rootNode.osNodeAgentId
            , mcpTree = tree
            , mcpNode = rootNode
            }

{- | List tools for a McpServerAgent.

Reads tools directly from the OS-native TVar.
-}
listMcpServerAgentTools :: McpServerAgent -> IO [ToolRegistration]
listMcpServerAgentTools agent =
    readTVarIO (osNodeTools agent.mcpNode)

{- | Create an agent tool function with session tracking and tracing.

This wraps 'turnAgentRuntimeIntoIOTool' with callbacks for session tracking
and a tracer for capturing sub-agent traces. The tracer allows monitoring
of recursive agent calls including start/completion events, LLM traces,
and tool execution traces.

When the sub-agent tracer is 'Nothing', no traces are emitted (backward
compatible silent behavior). For debugging, pass 'Just' a tracer that
writes to stderr or a custom trace handler.
-}
makeAgentTool ::
    SessionStore.SessionStore ->
    AgentTree.LoadedApiKeys ->
    Maybe (Prod.Tracer IO Trace) ->
    AgentTree.OSAgentNode ->
    AgentSlug ->
    AgentId ->
    ToolRegistration
makeAgentTool store apiKeys mTracer node slug agentId =
    OneShotTool.turnAgentRuntimeIntoIOTool
        store
        apiKeys
        node
        slug
        agentId
        OneShotTool.defaultAgentCallCallbacks
        (maybe Prod.silent id mTracer)
        OneShotTool.defaultParentSessionLookup

{- | Handle the MCP server command: start MCP server for agents

The tracer configuration is now unified in Props:
- 'treeLoadingTracer' is used for tree loading traces (combined with stderr tracer)
- 'subAgentTracer' is an optional Maybe tracer for sub-agent calls
  (Nothing = silent mode for backward compatibility)
-}
handleMcpServer ::
    -- | Tracer for tree loading and configuration events
    Prod.Tracer IO AgentTree.TreeTrace ->
    -- | Optional tracer for sub-agent calls (Nothing = silent)
    Maybe (Prod.Tracer IO Trace) ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files to expose
    [FilePath] ->
    IO ()
handleMcpServer treeLoadingTracer mSubAgentTracer sessionStore apiKeysFile agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    let oneAgent agentFilePath = do
            -- Use OS-native props with unified tracer configuration
            pure $
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.rootAgentFile = agentFilePath
                    , AgentTree.treeLoadingTracer =
                        Prod.traceBoth
                            treeLoadingTracer
                            traceUsefulPromptStderr
                    , AgentTree.subAgentTracer = mSubAgentTracer
                    , AgentTree.agentToTool = makeAgentTool sessionStore apiKeys mSubAgentTracer
                    }
    -- Use traverse to sequence the IO actions for creating Props
    agentPropsList <- traverse oneAgent agentFiles
    McpServer.multiAgentsServer McpServer.defaultMcpServerConfig agentPropsList

