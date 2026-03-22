{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'tool-call' command handler.

The tool-call command loads the first agent, registers its tools, and then
calls a specified tool with a JSON payload read from stdin. This is useful
for testing and invoking tools directly without going through the LLM.

Example usage:
    echo '{"cmd": "echo hello"}' | agents-exe tool-call bash
-}
module System.Agents.CLI.ToolCall (
    handleToolCall,
    ToolCallOptions (..),
) where

import Control.Concurrent.STM (readTVarIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (stderr)

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.Runtime as Runtime
import qualified System.Agents.SessionStore as SessionStore
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

-- | Handle the tool-call command
handleToolCall ::
    -- | Tool call options
    ToolCallOptions ->
    -- | Path to the API keys file
    FilePath ->
    -- | List of agent files (only first is used)
    [FilePath] ->
    IO ()
handleToolCall opts apiKeysFile agentFiles = do
    -- Read JSON payload from stdin
    stdinContent <- LByteString.getContents
    args <- case Aeson.eitherDecode stdinContent of
        Left err -> do
            Text.hPutStrLn stderr $ "Error: Invalid JSON payload from stdin: " <> Text.pack err
            error "Invalid JSON payload"
        Right val -> pure val

    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile

    -- Process only the first agent file (like OneShot)
    case agentFiles of
        [] -> do
            Text.hPutStrLn stderr "Error: No agent file specified"
            error "No agent file specified"
        (agentFile : _) -> do
            -- Create runtime registry
            registry <- AgentTree.newRuntimeRegistry

            -- Use silent tracer (tool portal handles its own output)
            let baseTracer = Prod.silent

            -- Load the agent tree runtime
            AgentTree.withAgentTreeRuntime
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.rootAgentFile = agentFile
                    , AgentTree.interactiveTracer = baseTracer
                    -- Wrap the function to match the new signature (ignoring the tracer argument)
                    , AgentTree.agentToTool = \_tracer rt slug aid -> OneShotTool.turnAgentRuntimeIntoIOTool SessionStore.defaultSessionStore rt slug aid
                    , AgentTree.runtimeRegistry = registry
                    }
                $ \result -> case result of
                    AgentTree.Errors errs -> do
                        Text.hPutStrLn stderr "Errors loading agent:"
                        mapM_ (Text.hPutStrLn stderr . Text.pack . show) errs
                        error "Failed to load agent"
                    AgentTree.Initialized tree -> do
                        -- Get registered tools from the runtime
                        registrations <- readTVarIO $ Runtime.agentTools tree.agentRuntime

                        -- Create tool portal with silent tracer
                        let portal = makeToolPortal Prod.silent registrations

                        -- Create the tool call
                        let toolCall =
                                ToolCall
                                    { callToolName = opts.toolName
                                    , callArgs = args
                                    , callCallerId = "cli-tool-call"
                                    }

                        -- Execute the tool call through the portal
                        result' <- portal toolCall

                        -- Output the result as JSON
                        LByteString.putStr $ Aeson.encode result'
                        Text.putStrLn ""

