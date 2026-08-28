{- | OS (Operating System) module for the Agents framework.

This module provides the Entity-Component-System (ECS) based architecture
for managing agents, toolboxes, and resources.

Note: This module selectively re-exports 'ResourceScope' from
'System.Agents.OS.Core' rather than 'System.Agents.OS.Resources' to avoid
a name conflict, since both modules define the same type name.
-}
module System.Agents.OS (
    -- * Core ECS
    module System.Agents.OS.Core,

    -- * Agents
    module System.Agents.OS.Agents,

    -- * Agent Tree
    module System.Agents.OS.AgentTree,

    -- * Resources (excluding conflicting types)
    module System.Agents.OS.Resources,

    -- * Concurrent Access
    module System.Agents.OS.Concurrent,

    -- * Persistence
    module System.Agents.OS.Persistence,

    -- * Conversation Tracking
    module System.Agents.OS.Conversation,

    -- * Events
    module System.Agents.OS.Events,
) where

import System.Agents.OS.AgentTree
import System.Agents.OS.Agents
import System.Agents.OS.Concurrent
import System.Agents.OS.Conversation
import System.Agents.OS.Core
import System.Agents.OS.Events
import System.Agents.OS.Persistence
import System.Agents.OS.Resources hiding (ResourceScope)

