{- | Unified handle for accessing OS-native agents across interfaces.

This module provides a single 'AgentHandle' type used by both the OneShot
and TUI interfaces. It replaces the duplicate agent wrapper definitions that
previously existed in 'System.Agents.CLI.OneShot' and
'System.Agents.TUI.Types.Core'.
-}
module System.Agents.OS.AgentHandle (
    -- * Agent Handle
    AgentHandle (..),
    createAgentHandle,

    -- * Tool Access
    getAgentTools,
    refreshAgentTools,

    -- * Agent Properties
    getAgentSlug,
    getAgentId,
    getAgentConfig,
) where

import Control.Concurrent.STM (readTVarIO)
import Data.Text (Text)
import System.Agents.AgentTree (OSAgentNode (..), OSAgentTree (..))
import System.Agents.Base (Agent, AgentId, slug)
import System.Agents.ToolRegistration (ToolRegistration)

-- | Unified handle for agent access across interfaces.
--
-- Replaces the duplicate @OneShotAgent@ and @TuiAgent@ wrapper definitions.
data AgentHandle = AgentHandle
    { ahAgentId :: AgentId
    -- ^ Unique identifier for this agent
    , ahTree :: OSAgentTree
    -- ^ The agent's tree structure
    , ahNode :: OSAgentNode
    -- ^ The specific node for this agent
    }

-- | Create a handle from an 'OSAgentTree' (uses the root node).
createAgentHandle :: OSAgentTree -> AgentHandle
createAgentHandle tree =
    let root = osTreeRoot tree
     in AgentHandle
            { ahAgentId = osNodeAgentId root
            , ahTree = tree
            , ahNode = root
            }

-- | Get the current tools from the agent's 'TVar'.
getAgentTools :: AgentHandle -> IO [ToolRegistration]
getAgentTools = readTVarIO . osNodeTools . ahNode

-- | Alias for 'getAgentTools' (for semantic clarity).
refreshAgentTools :: AgentHandle -> IO [ToolRegistration]
refreshAgentTools = getAgentTools

-- | Get the agent slug from its configuration.
getAgentSlug :: AgentHandle -> Text
getAgentSlug = (.slug) . osNodeConfig . ahNode

-- | Get the agent ID.
getAgentId :: AgentHandle -> AgentId
getAgentId = ahAgentId

-- | Get the agent configuration.
getAgentConfig :: AgentHandle -> Agent
getAgentConfig = osNodeConfig . ahNode

