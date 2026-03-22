{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'tui' command handler.

The tui command launches an interactive terminal user interface for
chatting with agents.

This module integrates with the recursive conversation capture feature
by using the callback-based sub-agent tool creation, enabling:

1. Parent-child session tracking - sub-agents are linked to their parents
2. Progress callbacks - TUI receives updates about sub-agent lifecycle
3. Correlation tracing - traces include sub-agent correlation IDs

See System.Agents.AgentTree.OneShotTool for details on the callback mechanism.
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

{- | Handle the TUI command: launch interactive terminal interface.

This function sets up the TUI with support for recursive agent calls.
Each agent's runtime is wrapped with callback support using
'turnAgentRuntimeIntoIOToolWithCallbacks', enabling:

* Sub-agent sessions are linked to parent sessions via parent IDs
* Session progress is tracked and emitted as TUI events
* Traces include correlation IDs for sub-agent calls

The tracer passed to 'agentToTool' is the parent agent's tracer,
which is used to emit correlation traces linking sub-agent execution
to the parent conversation.
-}
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
                    , -- Use the callback-based tool creation for recursive agent support.
                      -- The tracer argument is the parent agent's tracer for correlation.
                      AgentTree.agentToTool = \parentTracer rt slug agentId ->
                        OneShotTool.turnAgentRuntimeIntoIOToolWithCallbacks
                            OneShotTool.defaultSubAgentConfig
                            parentTracer
                            rt
                            slug
                            agentId
                    , AgentTree.runtimeRegistry = registry
                    }
    -- Use traverse to sequence the IO actions for creating Props
    agentPropsList <- traverse oneAgent agentFiles
    TUI.runTUI sessionStore agentPropsList
