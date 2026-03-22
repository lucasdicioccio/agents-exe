{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'mcp-server' command handler.

The mcp-server command starts a Model Context Protocol server that
exposes agents as MCP tools for integration with MCP clients.
-}
module System.Agents.CLI.McpServer (
    handleMcpServer,
) where

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.CLI.TraceUtils (traceUsefulPromptStderr)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Server as McpServer
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.TraceUtils (traceWaitingOpenAIRateLimits)

-- | Handle the MCP server command: start MCP server for agents
handleMcpServer ::
    -- | Base tracer for logging
    Prod.Tracer IO AgentTree.Trace ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files to expose
    [FilePath] ->
    IO ()
handleMcpServer baseTracer sessionStore apiKeysFile agentFiles = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    let oneAgent agentFile = do
            registry <- AgentTree.newRuntimeRegistry
            pure $
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.interactiveTracer =
                        Prod.traceBoth
                            baseTracer
                            ( Prod.traceBoth
                                traceUsefulPromptStderr
                                (traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) (\_ -> pure ()))
                            )
                    -- Wrap the function to match the new signature (ignoring the tracer argument)
                    , AgentTree.agentToTool = \_tracer rt slug aid -> OneShotTool.turnAgentRuntimeIntoIOTool sessionStore rt slug aid
                    , AgentTree.runtimeRegistry = registry
                    }
    -- Use traverse to sequence the IO actions for creating Props
    agentPropsList <- traverse oneAgent agentFiles
    McpServer.multiAgentsServer McpServer.defaultMcpServerConfig agentPropsList

