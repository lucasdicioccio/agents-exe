{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'run' command handler (one-shot mode).

The run command executes a single prompt against an agent and outputs
the response. This is the non-interactive mode for agents-exe.

This module uses OS-native structures for agent management.
-}
module System.Agents.CLI.OneShot (
    -- * Types
    OneShotOptions (..),
    OneShotAgent (..),

    -- * Handler
    handleOneShot,

    -- * Agent creation
    createOneShotAgent,

    -- * Tool access
    listOneShotAgentTools,
) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_)
import Data.Map (Map)
import Data.Text (Text)

import qualified Prod.Tracer as Prod
import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (AgentId, AgentSlug)
import System.Agents.Runtime.Trace (Trace)
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (ToolRegistration)

import System.Agents.AgentTree (OSAgentNode (..), OSAgentTree (..))

import System.Agents.CLI.Aliases (AliasDefinition)
import System.Agents.CLI.PromptScript (PromptScript, interpretPromptScript)

-- | Options for the one-shot command
data OneShotOptions = OneShotOptions
    { sessionFile :: Maybe FilePath
    , thinkingOutput :: OneShot.ThinkingOutput
    , promptScript :: PromptScript
    }
    deriving (Show)

{- | OneShot agent using OS-native structures.

This structure wraps an OSAgentNode for one-shot execution.
Tools are accessed directly from the node's tools TVar.
-}
data OneShotAgent = OneShotAgent
    { oneShotAgentId :: AgentId
    -- ^ Unique identifier for this agent
    , oneShotTree :: OSAgentTree
    -- ^ The agent's tree structure
    , oneShotNode :: OSAgentNode
    -- ^ The specific node for this agent
    }

-- | Manual Show instance for OneShotAgent.
instance Show OneShotAgent where
    show agent =
        "OneShotAgent {oneShotAgentId = "
            ++ show agent.oneShotAgentId
            ++ ", oneShotTree = <OSAgentTree>, oneShotNode = <OSAgentNode>}"

{- | Create a OneShotAgent from an OSAgentTree.

This function extracts the root agent from the tree for one-shot execution.
-}
createOneShotAgent :: OSAgentTree -> OneShotAgent
createOneShotAgent tree =
    let rootNode = osTreeRoot tree
     in OneShotAgent
            { oneShotAgentId = rootNode.osNodeAgentId
            , oneShotTree = tree
            , oneShotNode = rootNode
            }

{- | List tools for a OneShotAgent.

Reads tools directly from the OS-native TVar.
-}
listOneShotAgentTools :: OneShotAgent -> IO [ToolRegistration]
listOneShotAgentTools agent =
    readTVarIO (osNodeTools agent.oneShotNode)

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

{- | Handle the one-shot run command

The tracer configuration is now unified in Props:
- 'treeLoadingTracer' is used for tree loading traces
- 'subAgentTracer' is an optional Maybe tracer for sub-agent calls
  (Nothing = silent mode for backward compatibility)
-}
handleOneShot ::
    -- | Tracer for tree loading and configuration events
    Prod.Tracer IO AgentTree.TreeTrace ->
    -- | Optional tracer for sub-agent calls (Nothing = silent)
    Maybe (Prod.Tracer IO Trace) ->
    -- | Session store for persistence
    SessionStore.SessionStore ->
    -- | Path to API keys file
    FilePath ->
    -- | List of agent files (only first is used)
    [FilePath] ->
    -- | Available prompt aliases
    Map Text AliasDefinition ->
    -- | One-shot options
    OneShotOptions ->
    IO ()
handleOneShot treeLoadingTracer mSubAgentTracer sessionStore apiKeysFile agentFiles aliases opts = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    forM_ (take 1 agentFiles) $ \agentFilePath -> do
        promptContents <- interpretPromptScript aliases opts.promptScript opts.sessionFile
        mSession <- maybe (pure Nothing) SessionStore.readSessionFromFile opts.sessionFile
        -- Use OS-native agent loading with unified tracer configuration
        -- The subAgentTracer is passed to Props for use when sub-agents are called
        let oneShot text props = OneShot.mainOneShotTextWithThinking 
                (maybe Prod.silent id mSubAgentTracer) 
                sessionStore 
                opts.sessionFile 
                mSession 
                opts.thinkingOutput 
                props 
                text
        oneShot promptContents $
            AgentTree.Props
                { AgentTree.apiKeys = apiKeys
                , AgentTree.rootAgentFile = agentFilePath
                , AgentTree.treeLoadingTracer = treeLoadingTracer
                , AgentTree.subAgentTracer = mSubAgentTracer
                , AgentTree.agentToTool = makeAgentTool sessionStore apiKeys mSubAgentTracer
                }

