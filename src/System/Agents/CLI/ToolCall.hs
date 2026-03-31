{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'tool-call' command handler.

The tool-call command loads the first agent, registers its tools, and then
calls a specified tool with a JSON payload read from stdin. This is useful
for testing and invoking tools directly without going through the LLM.

Example usage:
    echo '{"cmd": "echo hello"}' | agents-exe tool-call bash

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.ToolCall (
    handleToolCall,
    ToolCallOptions (..),
    ToolCallAgent (..),
    createToolCallAgent,
) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (stderr)

import Prod.Tracer (Tracer, contramap)
import qualified System.Agents.Runtime.Trace as Runtime
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.SessionStore as SessionStore

import System.Agents.AgentTree (OSAgentNode (..), OSAgentTree (..))
import System.Agents.Base (AgentId)
import System.Agents.ToolPortal (makeToolPortal)
import System.Agents.Tools.Context (ToolCall (..))

-- | Options for the tool-call command
data ToolCallOptions = ToolCallOptions
    { toolName :: Text.Text
    -- ^ Name of the tool to call
    , logFile :: Maybe FilePath
    -- ^ Optional log file for tracing
    }
    deriving (Show, Eq)

{- | ToolCall agent using OS-native structures.

This structure wraps an OSAgentNode for direct tool execution.
-}
data ToolCallAgent = ToolCallAgent
    { toolCallAgentId :: AgentId
    -- ^ Unique identifier for this agent
    , toolCallTree :: OSAgentTree
    -- ^ The agent's tree structure
    , toolCallNode :: OSAgentNode
    -- ^ The specific node for this agent
    }

-- | Manual Show instance for ToolCallAgent.
instance Show ToolCallAgent where
    show agent =
        "ToolCallAgent {toolCallAgentId = "
            ++ show agent.toolCallAgentId
            ++ ", toolCallTree = <OSAgentTree>, toolCallNode = <OSAgentNode>}"

{- | Create a ToolCallAgent from an OSAgentTree.

This function extracts the root agent from the tree for tool execution.
-}
createToolCallAgent :: OSAgentTree -> ToolCallAgent
createToolCallAgent tree =
    let rootNode = osTreeRoot tree
     in ToolCallAgent
            { toolCallAgentId = rootNode.osNodeAgentId
            , toolCallTree = tree
            , toolCallNode = rootNode
            }

-- | Handle the tool-call command
handleToolCall ::
    Tracer IO AgentTree.TreeTrace ->
    -- | Path to the API keys file
    ToolCallOptions ->
    -- | Path to the API keys file
    FilePath ->
    -- | List of agent files (only first is used)
    [FilePath] ->
    IO ()
handleToolCall baseTracer opts apiKeysFile agentFiles = do
    -- Read JSON payload from stdin
    stdinContent <- LByteString.getContents
    args <- case Aeson.eitherDecode stdinContent of
        Left err -> do
            Text.hPutStrLn stderr $ "Error: Invalid JSON payload from stdin: " <> Text.pack err
            error "Invalid JSON payload"
        Right val -> pure val

    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    let rtTracer = contramap AgentTree.RuntimeTrace baseTracer
    let tpTracer = contramap (Runtime.ToolPortalTrace "-tool-portal-agent-slug-missing-") rtTracer

    -- Process only the first agent file (like OneShot)
    case agentFiles of
        [] -> do
            Text.hPutStrLn stderr "Error: No agent file specified"
            error "No agent file specified"
        (agentFilePath : _) -> do
            -- Load the agent tree
            AgentTree.withAgentTree
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.rootAgentFile = agentFilePath
                    , AgentTree.interactiveTracer = baseTracer
                    , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool rtTracer SessionStore.defaultSessionStore apiKeys
                    }
                $ \result -> case result of
                    AgentTree.Errors errs -> do
                        Text.hPutStrLn stderr "Errors loading agent:"
                        mapM_ (Text.hPutStrLn stderr . Text.pack . show) errs
                        error "Failed to load agent"
                    AgentTree.Initialized tree -> do
                        -- Create ToolCallAgent
                        let agent = createToolCallAgent tree

                        let portal = makeToolPortal tpTracer (osNodeTools agent.toolCallNode)

                        -- Create the tool call
                        let toolCall =
                                ToolCall
                                    { callToolName = opts.toolName
                                    , callArgs = args
                                    }

                        -- Execute the tool call through the portal
                        result' <- portal toolCall

                        -- Output the result as JSON
                        LByteString.putStr $ Aeson.encode result'
                        Text.putStrLn ""
