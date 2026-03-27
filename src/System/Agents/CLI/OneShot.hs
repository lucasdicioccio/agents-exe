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
and a tracer for capturing sub-agent traces.

The sub-agent tracer is derived from the unified TreeTrace tracer by
extracting 'SubAgentTrace' events. When the main tracer is silent,
sub-agent tracing is effectively disabled.
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
        OneShotTool.defaultParentSessionLookup

{- | Handle the one-shot run command

The tracer is now unified under a single 'TreeTrace' type. Sub-agent
traces are wrapped in 'SubAgentTrace' constructor. To disable sub-agent
tracing, use 'Prod.silent' or don't emit 'SubAgentTrace' events.
-}
handleOneShot ::
    -- | Unified tracer for all events
    Prod.Tracer IO AgentTree.TreeTrace ->
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
handleOneShot tracer sessionStore apiKeysFile agentFiles aliases opts = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile
    forM_ (take 1 agentFiles) $ \agentFilePath -> do
        promptContents <- interpretPromptScript aliases opts.promptScript opts.sessionFile
        mSession <- maybe (pure Nothing) SessionStore.readSessionFromFile opts.sessionFile
        -- Use OS-native agent loading with unified tracer configuration
        let oneShot text props = OneShot.mainOneShotTextWithThinking 
                (Prod.contramap AgentTree.SubAgentTrace tracer) 
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
                , AgentTree.tracer = tracer
                , AgentTree.agentToTool = makeAgentTool sessionStore apiKeys tracer
                }

