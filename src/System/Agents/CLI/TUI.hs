{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'tui' command handler.

The tui command launches an interactive terminal user interface for
chatting with agents.
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
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.SessionStore as SessionStore
import qualified System.Agents.TUI.Core as TUI
import System.Agents.TraceUtils (traceWaitingOpenAIRateLimits)

-- | Options for the TUI command
data TuiOptions = TuiOptions
    {
    }
    deriving (Show)

-- | Handle the TUI command: launch interactive terminal interface
handleTUI ::
    -- | Base tracer for logging
    Prod.Tracer IO AgentTree.Trace ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files to load
    [FilePath] ->
    IO ()
handleTUI baseTracer sessionStore apiKeysFile agentFiles = do
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
                            (traceWaitingOpenAIRateLimits (OpenAI.ApiLimits 100 10000) print)
                    -- Wrap the function to match the new signature (ignoring the tracer argument)
                    , AgentTree.agentToTool = \_tracer rt slug aid -> OneShotTool.turnAgentRuntimeIntoIOTool sessionStore rt slug aid
                    , AgentTree.runtimeRegistry = registry
                    }
    -- Use traverse to sequence the IO actions for creating Props
    agentPropsList <- traverse oneAgent agentFiles
    TUI.runTUI sessionStore agentPropsList

