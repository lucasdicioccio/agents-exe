{-# LANGUAGE OverloadedRecordDot #-}

{- |
OS (Operating System) module for the Agents framework.

This module provides the Entity-Component-System (ECS) based architecture
for managing agents, toolboxes, and resources.

Note: This module selectively re-exports to avoid name conflicts:
- 'ResourceScope' is exported from System.Agents.OS.Core (not Resources)
- 'ConversationStatus' and 'AgentStatus' are exported from System.Agents.OS.Core (not Interfaces)
- 'createAgent' is exported from System.Agents.OS.Agents via AgentTree (not Interfaces)
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

    -- * Conversation Tracking (excluding conflicting types)
    module System.Agents.OS.Conversation,

    -- * Interfaces (with selective export to avoid conflicts)
    InterfaceHandle (..),
    InterfaceConfig (..),
    OSInterface (..),
    AgentHandle (..),
    ConversationHandle (..),
    OSEvent (..),
    subscribeToEvents,
    unsubscribeFromEvents,
    defaultInterfaceConfig,
    InterfaceMode (..),
    sendMessage,
    receiveMessage,
    startConversation,
    endConversation,
    getConversationStatus,
    destroyAgent,
    OS (..),
) where

import System.Agents.OS.AgentTree
import System.Agents.OS.Agents
import System.Agents.OS.Concurrent
import System.Agents.OS.Conversation hiding (ConversationStatus)
import System.Agents.OS.Core
import System.Agents.OS.Interfaces (
    AgentHandle (..),
    ConversationHandle (..),
    InterfaceConfig (..),
    InterfaceHandle (..),
    InterfaceMode (..),
    OS (..),
    OSEvent (..),
    OSInterface (..),
    defaultInterfaceConfig,
    destroyAgent,
    endConversation,
    getConversationStatus,
    receiveMessage,
    sendMessage,
    startConversation,
    subscribeToEvents,
    unsubscribeFromEvents,
 )
import System.Agents.OS.Persistence
import System.Agents.OS.Resources hiding (ResourceScope)
