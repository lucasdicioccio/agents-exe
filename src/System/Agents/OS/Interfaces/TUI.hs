{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | OS-native TUI interface - SKELETON IMPLEMENTATION.

This module provides the structure for a complete interactive terminal 
user interface using OS-native ECS primitives. This is currently a skeleton 
that defines the interface structure - full implementation to be completed.

For now, the legacy System.Agents.TUI.* modules are still used for actual
TUI functionality.
-}
module System.Agents.OS.Interfaces.TUI (
    -- * TUI Interface Handle
    TUIInterfaceHandle (..),
    TUIInterfaceConfig (..),

    -- * Initialization
    initTUIInterface,
    runTUIInterface,
    shutdownTUIInterface,

    -- * Agent Management
    registerAgent,
    getRegisteredAgents,

    -- * Layout Management
    setLayoutMode,
    getLayoutMode,

    -- * Multi-Agent Configuration
    MultiAgentConfig (..),

    -- * Configuration
    defaultTUIInterfaceConfig,
    LayoutMode (..),

    -- * Events
    TUIEvent (..),

    -- * Tracing
    TUITrace (..),

    -- * Registered Agent
    RegisteredAgent (..),
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TQueue, TVar, atomically, modifyTVar', newTQueueIO, newTVarIO, readTVarIO, writeTVar)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Prod.Tracer (Tracer (..))

import System.Agents.Base (AgentId, ConversationId, newAgentId)
import System.Agents.OS.Compat.Runtime (initializeOS)
import System.Agents.OS.Interfaces (InterfaceConfig (..), InterfaceHandle (..), defaultInterfaceConfig)

-------------------------------------------------------------------------------
-- Tracing
-------------------------------------------------------------------------------

-- | Trace events for TUI interface debugging.
data TUITrace
    = TUITrace_Initialized
    | TUITrace_Error Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Layout Configuration
-------------------------------------------------------------------------------

-- | Layout modes for the TUI display.
data LayoutMode
    = SingleAgent
    | SplitVertical
    | SplitHorizontal
    | GridLayout Int Int
    | Tabbed
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Multi-Agent Configuration
-------------------------------------------------------------------------------

-- | Configuration for multi-agent mode (skeleton).
data MultiAgentConfig = MultiAgentConfig
    { maAgentIds :: [AgentId]
    }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- TUI Events
-------------------------------------------------------------------------------

-- | Events emitted by the TUI interface.
data TUIEvent
    = TUIEvent_AgentStarted AgentId
    | TUIEvent_ConversationStarted ConversationId AgentId
    | TUIEvent_Error Text
    | TUIEvent_Shutdown
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Registered Agent
-------------------------------------------------------------------------------

-- | A registered agent in the TUI interface.
data RegisteredAgent = RegisteredAgent
    { regAgentId :: AgentId
    , regCreatedAt :: Text -- Simplified for skeleton
    }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- TUI Interface Configuration
-------------------------------------------------------------------------------

-- | Configuration specific to TUI interface.
data TUIInterfaceConfig = TUIInterfaceConfig
    { tuiBaseConfig :: InterfaceConfig
    , tuiDefaultLayout :: LayoutMode
    , tuiEnableMultiAgent :: Bool
    , tuiMaxConversations :: Int
    , tuiTracer :: Tracer IO TUITrace
    }

-- | Default TUI interface configuration.
defaultTUIInterfaceConfig :: IO TUIInterfaceConfig
defaultTUIInterfaceConfig = do
    let tracer = Tracer $ \_ -> pure ()
    pure $
        TUIInterfaceConfig
            { tuiBaseConfig = defaultInterfaceConfig
            , tuiDefaultLayout = SingleAgent
            , tuiEnableMultiAgent = False
            , tuiMaxConversations = 5
            , tuiTracer = tracer
            }

-------------------------------------------------------------------------------
-- TUI Interface Handle
-------------------------------------------------------------------------------

-- | Handle for TUI-specific interface operations.
data TUIInterfaceHandle = TUIInterfaceHandle
    { tuiBaseHandle :: InterfaceHandle
    , tuiLayout :: TVar LayoutMode
    , tuiEventQueue :: TQueue TUIEvent
    , tuiShutdown :: TVar Bool
    , tuiRegisteredAgents :: TVar (Map AgentId RegisteredAgent)
    }

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Initialize the TUI interface (skeleton implementation).
initTUIInterface :: TUIInterfaceConfig -> IO TUIInterfaceHandle
initTUIInterface config = do
    os <- initializeOS
    shutdownVar <- newTVarIO False
    agentsVar <- newTVarIO Map.empty
    conversationsVar <- newTVarIO Map.empty
    eventQueue <- newTQueueIO :: IO (TQueue TUIEvent)
    layoutVar <- newTVarIO (tuiDefaultLayout config)
    registeredAgentsVar <- newTVarIO Map.empty

    let baseHandle =
            InterfaceHandle
                { ihOS = os
                , ihConfig = tuiBaseConfig config
                , ihEventSub = error "TUI uses tuiEventQueue, not ihEventSub"
                , ihShutdown = shutdownVar
                , ihAgents = agentsVar
                , ihConversations = conversationsVar
                }

    -- Use the tracer
    let Tracer tracerFn = tuiTracer config
    tracerFn TUITrace_Initialized

    pure $
        TUIInterfaceHandle
            { tuiBaseHandle = baseHandle
            , tuiLayout = layoutVar
            , tuiEventQueue = eventQueue
            , tuiShutdown = shutdownVar
            , tuiRegisteredAgents = registeredAgentsVar
            }

-- | Set the current layout mode.
setLayoutMode :: TUIInterfaceHandle -> LayoutMode -> IO ()
setLayoutMode handle mode = do
    atomically $ writeTVar (tuiLayout handle) mode

-- | Get the current layout mode.
getLayoutMode :: TUIInterfaceHandle -> IO LayoutMode
getLayoutMode handle =
    readTVarIO (tuiLayout handle)

-- | Register an agent (skeleton - creates placeholder registration).
registerAgent :: TUIInterfaceHandle -> Text -> IO AgentId
registerAgent handle _name = do
    agentId <- newAgentId
    let regAgent = RegisteredAgent agentId ""
    atomically $ modifyTVar' (tuiRegisteredAgents handle) $ Map.insert agentId regAgent
    pure agentId

-- | Get all registered agents.
getRegisteredAgents :: TUIInterfaceHandle -> IO [(AgentId, RegisteredAgent)]
getRegisteredAgents handle = do
    Map.toList <$> readTVarIO (tuiRegisteredAgents handle)

-- | Run the TUI interface event loop (skeleton - does nothing).
runTUIInterface :: TUIInterfaceHandle -> IO ()
runTUIInterface handle = do
    let loop = do
            shutdown <- readTVarIO (tuiShutdown handle)
            if shutdown
                then pure ()
                else do
                    threadDelay 1000000
                    loop
    loop

-- | Shutdown the TUI interface.
shutdownTUIInterface :: TUIInterfaceHandle -> IO ()
shutdownTUIInterface handle = do
    atomically $ writeTVar (tuiShutdown handle) True

