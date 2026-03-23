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

== Migration Notes

Currently, this module provides a compatibility layer that works with
RuntimeBridge. As the OS model matures, these operations will be
implemented natively using OS primitives.
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

    -- * Multi-Agent Coordination
    configureMultiAgent,

    -- * Agent Bus Operations
    createAgentBus,
    sendInterAgentMessage,
    broadcastMessage,

    -- * Conversation Management
    ConversationStatus (..),
    startConversation,
    pauseConversation,
    resumeConversation,

    -- * Utility
    defaultTUIInterfaceConfig,
    MultiAgentConfig (..),
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO, writeTQueue, writeTVar)
import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import System.Agents.Base (AgentId, ConversationId, newAgentId, newConversationId)
import System.Agents.OS.Compat.Runtime (RuntimeBridge, initializeOS, newRuntimeBridge)
import System.Agents.OS.Interfaces (
    InterfaceConfig (..),
    InterfaceHandle (..),
    InterfaceMode (..),
    defaultInterfaceConfig,
 )
import System.Agents.Session.Base (Session)
import System.Agents.TUI.Types (
    AgentBus (..),
    AgentRoleConfig (..),
    CoordinationStrategy (..),
    InterAgentMessage (..),
    LayoutMode (..),
    MessageType (..),
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
    , tuiEnableMultiAgent :: Bool
    -- ^ Enable multi-agent coordination features
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
        , tuiEnableMultiAgent = False
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
    , tuiAgentBus :: Maybe AgentBus
    -- ^ Optional agent bus for multi-agent mode
    , tuiMultiAgentConfig :: Maybe MultiAgentConfig
    -- ^ Multi-agent configuration if enabled
    , tuiRegisteredAgents :: TVar (Map.Map AgentId RuntimeBridge)
    -- ^ Registered agents with their bridges
    , tuiConversations :: TVar (Map.Map ConversationId TUIConversationHandle)
    -- ^ Active conversations
    }

-- | Multi-agent configuration.
data MultiAgentConfig = MultiAgentConfig
    { maAgents :: [AgentRoleConfig]
    , maCoordinationStrategy :: CoordinationStrategy
    }
    deriving (Show)

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

    -- Create agent bus if multi-agent is enabled
    agentBus <-
        if tuiEnableMultiAgent config
            then Just <$> createAgentBus
            else pure Nothing

    pure $
        TUIInterfaceHandle
            { tuiBaseHandle = baseHandle
            , tuiLayout = layoutVar
            , tuiAgentBus = agentBus
            , tuiMultiAgentConfig = Nothing
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
registerAgent :: TUIInterfaceHandle -> Text -> IO (AgentId, RuntimeBridge)
registerAgent handle _agentName = do
    -- Generate a new agent ID
    agentId <- newAgentId

    -- Create a RuntimeBridge for this agent
    let os = ihOS (tuiBaseHandle handle)
    let bridge = newRuntimeBridge agentId os

    -- Register in the interface
    atomically $ modifyTVar (tuiRegisteredAgents handle) $ Map.insert agentId bridge

    pure (agentId, bridge)

-- | Unregister an agent from the TUI interface.
unregisterAgent :: TUIInterfaceHandle -> AgentId -> IO ()
unregisterAgent handle agentId = do
    atomically $ modifyTVar (tuiRegisteredAgents handle) $ Map.delete agentId

-- | Get all registered agents.
getRegisteredAgents :: TUIInterfaceHandle -> IO [(AgentId, RuntimeBridge)]
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
-- Multi-Agent Coordination
-------------------------------------------------------------------------------

-- | Configure multi-agent mode.
configureMultiAgent :: TUIInterfaceHandle -> MultiAgentConfig -> IO ()
configureMultiAgent handle config = do
    -- Validate that all configured agents are registered
    registered <- getRegisteredAgents handle
    let registeredIds = Set.fromList $ map fst registered
    let configuredIds = Set.fromList $ map arcAgentId (maAgents config)

    if configuredIds `Set.isSubsetOf` registeredIds
        then do
            -- Valid configuration
            error "configureMultiAgent: Full implementation pending"
        else do
            error "configureMultiAgent: Some configured agents are not registered"

-------------------------------------------------------------------------------
-- Agent Bus Operations
-------------------------------------------------------------------------------

-- | Create a new agent bus.
createAgentBus :: IO AgentBus
createAgentBus = do
    channels <- newTVarIO Map.empty
    pure $ AgentBus channels

-- | Send a message from one agent to another.
sendInterAgentMessage ::
    AgentBus ->
    -- | From agent
    AgentId ->
    -- | To agent
    AgentId ->
    -- | Message type
    MessageType ->
    -- | Content
    Value ->
    IO ()
sendInterAgentMessage bus fromAgent toAgent msgType content = do
    let message =
            InterAgentMessage
                { iamFrom = fromAgent
                , iamTo = toAgent
                , iamType = msgType
                , iamContent = content
                }

    -- Get or create channel for target agent
    channels <- readTVarIO (busChannels bus)
    case Map.lookup toAgent channels of
        Just queue -> atomically $ writeTQueue queue message
        Nothing -> pure () -- Target agent not listening, message dropped

-- | Broadcast a message to all subscribed agents.
broadcastMessage ::
    AgentBus ->
    -- | From agent
    AgentId ->
    -- | Content
    Value ->
    -- | List of target agents
    [AgentId] ->
    IO ()
broadcastMessage bus fromAgent content targets = do
    mapM_ (\toAgent -> sendInterAgentMessage bus fromAgent toAgent MessageType_Broadcast content) targets

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
