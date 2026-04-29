{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | TUI-specific interface for the OS model.

This module provides TUI-specific operations built on top of the common
OS interface. It handles multi-agent coordination, layout management, and
interactive session handling.

== Features

1. Multi-agent support with role-based coordination
2. Layout mode management
3. Interactive conversation handling
4. Agent bus for inter-agent communication
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
    unregisterAgent,
    getRegisteredAgents,

    -- * Layout Management
    setLayoutMode,
    getLayoutMode,

    -- * Conversation Management
    ConversationStatus (..),
    startConversation,
    pauseConversation,
    resumeConversation,

    -- * Utility
    defaultTUIInterfaceConfig,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO, writeTVar)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import System.Agents.Base (AgentId, ConversationId, newAgentId, newConversationId)
import System.Agents.OS.Compat.Runtime (initializeOS)
import System.Agents.OS.Interfaces (
    InterfaceConfig (..),
    InterfaceHandle (..),
    InterfaceMode (..),
    defaultInterfaceConfig,
 )
import System.Agents.Session.Base (Session)
import System.Agents.TUI.Types (
    LayoutMode (..),
 )

-------------------------------------------------------------------------------
-- TUI Interface Configuration
-------------------------------------------------------------------------------

-- | Configuration specific to TUI interface.
data TUIInterfaceConfig = TUIInterfaceConfig
    { tuiBaseConfig :: InterfaceConfig
    -- ^ Base interface configuration
    , tuiDefaultLayout :: LayoutMode
    -- ^ Default layout mode
    , tuiMaxConversations :: Int
    -- ^ Maximum concurrent conversations per agent
    }
    deriving (Show, Eq)

-- | Default TUI interface configuration.
defaultTUIInterfaceConfig :: TUIInterfaceConfig
defaultTUIInterfaceConfig =
    TUIInterfaceConfig
        { tuiBaseConfig = (defaultInterfaceConfig){ifcMode = ModeTUI}
        , tuiDefaultLayout = SingleAgent
        , tuiMaxConversations = 5
        }

-------------------------------------------------------------------------------
-- TUI Interface Handle
-------------------------------------------------------------------------------

-- | Handle for TUI-specific interface operations.
data TUIInterfaceHandle = TUIInterfaceHandle
    { tuiBaseHandle :: InterfaceHandle
    -- ^ Base interface handle
    , tuiLayout :: TVar LayoutMode
    -- ^ Current layout mode
    , tuiRegisteredAgents :: TVar (Map.Map AgentId ())
    -- ^ Registered agents with their bridges
    , tuiConversations :: TVar (Map.Map ConversationId TUIConversationHandle)
    -- ^ Active conversations
    }

-- | Conversation handle for TUI.
data TUIConversationHandle = TUIConversationHandle
    { tchConversationId :: ConversationId
    , tchAgentId :: AgentId
    , tchSession :: Maybe Session
    , tchStatus :: ConversationStatus
    , tchAsync :: Maybe (Async ())
    }

-- | Manual Show instance for TUIConversationHandle (Async doesn't have Show).
instance Show TUIConversationHandle where
    show ch =
        "TUIConversationHandle {tchConversationId = "
            ++ show ch.tchConversationId
            ++ ", tchAgentId = "
            ++ show ch.tchAgentId
            ++ ", tchSession = "
            ++ show ch.tchSession
            ++ ", tchStatus = "
            ++ show ch.tchStatus
            ++ ", tchAsync = <Async>}"

-- | Conversation status for TUI.
data ConversationStatus
    = ConversationStatus_Active
    | ConversationStatus_WaitingForInput
    | ConversationStatus_Paused
    | ConversationStatus_Completed
    | ConversationStatus_Failed Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Initialize the TUI interface.
initTUIInterface :: TUIInterfaceConfig -> IO TUIInterfaceHandle
initTUIInterface config = do
    -- Initialize OS
    os <- initializeOS

    -- Create base interface components
    shutdownVar <- newTVarIO False
    agentsVar <- newTVarIO Map.empty
    conversationsVar <- newTVarIO Map.empty

    -- Create a fake event queue - in real implementation would use STM
    let fakeQueue = error "Event queue requires STM implementation"

    let baseHandle =
            InterfaceHandle
                { ihOS = os
                , ihConfig = tuiBaseConfig config
                , ihEventSub = fakeQueue
                , ihShutdown = shutdownVar
                , ihAgents = agentsVar
                , ihConversations = conversationsVar
                }

    layoutVar <- newTVarIO (tuiDefaultLayout config)
    registeredAgentsVar <- newTVarIO Map.empty
    tuiConversationsVar <- newTVarIO Map.empty

    pure $
        TUIInterfaceHandle
            { tuiBaseHandle = baseHandle
            , tuiLayout = layoutVar
            , tuiRegisteredAgents = registeredAgentsVar
            , tuiConversations = tuiConversationsVar
            }

-- | Run the TUI interface event loop.
runTUIInterface :: TUIInterfaceHandle -> IO ()
runTUIInterface handle = do
    -- Event loop that processes:
    -- 1. User input events
    -- 2. Agent events
    -- 3. Inter-agent messages
    let loop = do
            shutdown <- readTVarIO (ihShutdown $ tuiBaseHandle handle)
            if shutdown
                then pure ()
                else do
                    threadDelay 10000 -- 10ms
                    loop
    loop

-- | Shutdown the TUI interface gracefully.
shutdownTUIInterface :: TUIInterfaceHandle -> IO ()
shutdownTUIInterface handle = do
    atomically $ writeTVar (ihShutdown $ tuiBaseHandle handle) True

-------------------------------------------------------------------------------
-- Agent Management
-------------------------------------------------------------------------------

-- | Register a new agent with the TUI interface.
registerAgent :: TUIInterfaceHandle -> Text -> IO (AgentId)
registerAgent handle _agentName = do
    -- Generate a new agent ID
    agentId <- newAgentId

    -- Register in the interface
    atomically $ modifyTVar (tuiRegisteredAgents handle) $ Map.insert agentId ()

    pure agentId

-- | Unregister an agent from the TUI interface.
unregisterAgent :: TUIInterfaceHandle -> AgentId -> IO ()
unregisterAgent handle agentId = do
    atomically $ modifyTVar (tuiRegisteredAgents handle) $ Map.delete agentId

-- | Get all registered agents.
getRegisteredAgents :: TUIInterfaceHandle -> IO [(AgentId, ())]
getRegisteredAgents handle = do
    Map.toList <$> readTVarIO (tuiRegisteredAgents handle)

-------------------------------------------------------------------------------
-- Layout Management
-------------------------------------------------------------------------------

-- | Set the current layout mode.
setLayoutMode :: TUIInterfaceHandle -> LayoutMode -> IO ()
setLayoutMode handle mode = do
    atomically $ writeTVar (tuiLayout handle) mode

-- | Get the current layout mode.
getLayoutMode :: TUIInterfaceHandle -> IO LayoutMode
getLayoutMode handle =
    readTVarIO (tuiLayout handle)

-------------------------------------------------------------------------------
-- Conversation Management
-------------------------------------------------------------------------------

-- | Start a new conversation with an agent.
startConversation ::
    TUIInterfaceHandle ->
    AgentId ->
    Text ->
    IO TUIConversationHandle
startConversation handle agentId _initialMessage = do
    -- Verify agent is registered
    registered <- getRegisteredAgents handle
    case lookup agentId registered of
        Nothing -> error "Agent not registered"
        Just _bridge -> do
            -- Create conversation
            convId <- newConversationId
            let convHandle =
                    TUIConversationHandle
                        { tchConversationId = convId
                        , tchAgentId = agentId
                        , tchSession = Nothing
                        , tchStatus = ConversationStatus_WaitingForInput
                        , tchAsync = Nothing
                        }

            -- Register conversation
            atomically $ modifyTVar (tuiConversations handle) $ Map.insert convId convHandle

            pure convHandle

-- | Pause an active conversation.
pauseConversation :: TUIInterfaceHandle -> ConversationId -> IO ()
pauseConversation handle convId = do
    atomically $ modifyTVar (tuiConversations handle) $ Map.adjust (\ch -> ch{tchStatus = ConversationStatus_Paused}) convId

-- | Resume a paused conversation.
resumeConversation :: TUIInterfaceHandle -> ConversationId -> IO ()
resumeConversation handle convId = do
    atomically $ modifyTVar (tuiConversations handle) $ Map.adjust (\ch -> ch{tchStatus = ConversationStatus_Active}) convId
