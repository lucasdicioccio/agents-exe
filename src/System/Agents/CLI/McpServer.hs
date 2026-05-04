{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'mcp-server' command handler.

The mcp-server command starts a Model Context Protocol server that
exposes agents as MCP tools for integration with MCP clients.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.McpServer (
    Trace (..),

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
import System.Agents.CLI.TraceUtils (traceUsefulPromptStderr)
import qualified System.Agents.MCP.Server as McpServer
import qualified System.Agents.SessionStore as SessionStore

import System.Agents.AgentTree (OSAgentNode (..), OSAgentTree (..))
import System.Agents.Base (AgentId)
import System.Agents.ToolRegistration (ToolRegistration)

data Trace
    = AgentTreeTrace !AgentTree.TreeTrace
    | McpServerTrace !McpServer.Trace
    | OneShotToolTrace !OneShotTool.Trace
    deriving (Show)

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

-- | Handle the MCP server command: start MCP server for agents
handleMcpServer ::
    -- | Base tracer for logging
    Prod.Tracer IO Trace ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files to expose
    [FilePath] ->
    IO ()
handleMcpServer tracer sessionStore apiKeysFile agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    let ttTracer = Prod.contramap AgentTreeTrace tracer
        oneAgent agentFilePath = do
            -- Use OS-native props (no registry needed)
            pure $
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.apiKeysFile = apiKeysFile
                    , AgentTree.rootAgentFile = agentFilePath
                    , AgentTree.interactiveTracer =
                        Prod.traceBoth
                            ttTracer
                            traceUsefulPromptStderr
                    , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool (Prod.contramap OneShotToolTrace tracer) sessionStore apiKeys
                    , AgentTree.sessionStore = sessionStore
                    }
    -- Use traverse to sequence the IO actions for creating Props
    agentPropsList <- traverse oneAgent agentFiles
    McpServer.multiAgentsServer (Prod.contramap McpServerTrace tracer) McpServer.defaultMcpServerConfig agentPropsList

