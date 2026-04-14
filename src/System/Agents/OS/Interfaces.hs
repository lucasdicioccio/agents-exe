{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Common interface abstraction for TUI and OneShot interfaces.

This module provides a unified interface that both TUI and OneShot can use
to interact with the OS. The interface uses OS-native structures.
-}
module System.Agents.OS.Interfaces (
    -- * Interface Handle
    InterfaceHandle (..),
    InterfaceConfig (..),

    -- * Interface Operations (via typeclass)
    OSInterface (..),

    -- * Agent Operations
    AgentHandle (..),
    AgentStatus (..),
    createAgent,
    destroyAgent,
    sendMessage,
    receiveMessage,

    -- * Conversation Operations
    ConversationHandle (..),
    ConversationStatus (..),
    startConversation,
    endConversation,
    getConversationStatus,

    -- * Event Handling
    OSEvent (..),
    subscribeToEvents,
    unsubscribeFromEvents,

    -- * Utility Functions
    defaultInterfaceConfig,
    InterfaceMode (..),

    -- * Re-exports from Compat.Runtime (deprecated)
    OS (..),
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TQueue, TVar, readTVarIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)

import System.Agents.Base (AgentId, ConversationId)
import System.Agents.OS.Compat.Runtime (OS (..))
import System.Agents.OS.Events (OSEvent (..))
import System.Agents.Session.Base (Session)

data InterfaceMode
    = -- | Interactive TUI mode with multi-agent support
      ModeTUI
    | -- | Batch OneShot mode with single agent
      ModeOneShot
    deriving (Show, Eq)

-- | Configuration for initializing an interface.
data InterfaceConfig = InterfaceConfig
    { ifcMode :: InterfaceMode
    -- ^ Operating mode (TUI interactive or OneShot batch)
    , ifcMaxAgents :: Int
    -- ^ Maximum number of concurrent agents (0 for unlimited)
    , ifcEnableEvents :: Bool
    -- ^ Whether to enable event subscription
    , ifcSessionStorePath :: Maybe FilePath
    -- ^ Optional path for session persistence
    }
    deriving (Show, Eq)

-- | Default interface configuration.
defaultInterfaceConfig :: InterfaceConfig
defaultInterfaceConfig =
    InterfaceConfig
        { ifcMode = ModeOneShot
        , ifcMaxAgents = 10
        , ifcEnableEvents = True
        , ifcSessionStorePath = Nothing
        }

-------------------------------------------------------------------------------
-- Interface Handle
-------------------------------------------------------------------------------

{- | Handle to an initialized OS interface.

The interface handle maintains the connection to the OS and tracks
all active agents and conversations. It uses a TVar for the shutdown
flag to allow atomic updates.
-}
data InterfaceHandle = InterfaceHandle
    { ihOS :: OS
    -- ^ The underlying OS instance
    , ihConfig :: InterfaceConfig
    -- ^ Configuration used to initialize
    , ihEventSub :: TQueue OSEvent
    -- ^ Event queue for subscribers
    , ihShutdown :: TVar Bool
    -- ^ Shutdown flag for graceful termination
    , ihAgents :: TVar (Map AgentId AgentHandle)
    -- ^ Active agents
    , ihConversations :: TVar (Map ConversationId ConversationHandle)
    -- ^ Active conversations
    }

-------------------------------------------------------------------------------
-- OS Interface Typeclass
-------------------------------------------------------------------------------

-- | Typeclass for OS interface operations.
class OSInterface m where
    -- | Initialize the interface with an OS instance.
    initInterface :: OS -> InterfaceConfig -> m InterfaceHandle

    -- | Run the interface (blocking).
    runInterface :: InterfaceHandle -> m ()

    -- | Shutdown the interface gracefully.
    shutdownInterface :: InterfaceHandle -> m ()

-- | Concrete IO implementation of OSInterface.
instance OSInterface IO where
    initInterface _os _config = do
        error "initInterface: Not yet implemented - use OS-native interface"

    runInterface _handle = do
        error "runInterface: Not yet implemented - use OS-native interface"

    shutdownInterface _handle = do
        error "shutdownInterface: Not yet implemented - use OS-native interface"

-------------------------------------------------------------------------------
-- Agent Handle
-------------------------------------------------------------------------------

-- | Agent status.
data AgentStatus
    = AgentStatus_Idle
    | AgentStatus_Running
    | AgentStatus_Paused
    | AgentStatus_Error Text
    | AgentStatus_Shutdown
    deriving (Show, Eq)

-- | Handle to an agent within the interface.
data AgentHandle = AgentHandle
    { ahAgentId :: AgentId
    -- ^ Unique agent identifier
    , ahThreadId :: Maybe ThreadId
    -- ^ Thread ID if agent is running
    , ahStatus :: AgentStatus
    -- ^ Current agent status
    , ahCreatedAt :: UTCTime
    -- ^ Creation timestamp
    }

-- | Manual Show instance for AgentHandle (Async doesn't have Show).
instance Show AgentHandle where
    show ah =
        "AgentHandle {ahAgentId = "
            ++ show ah.ahAgentId
            ++ ", ahThreadId = "
            ++ show ah.ahThreadId
            ++ ", ahStatus = "
            ++ show ah.ahStatus
            ++ ", ahCreatedAt = "
            ++ show ah.ahCreatedAt
            ++ "}"

-- | Create a new agent in the interface.
createAgent :: InterfaceHandle -> Text -> IO AgentHandle
createAgent _handle _agentName = do
    -- Placeholder implementation
    error "createAgent: Not yet implemented - use OS-native interface"

-- | Destroy an agent and clean up resources.
destroyAgent :: InterfaceHandle -> AgentId -> IO ()
destroyAgent _handle _agentId = do
    -- Placeholder implementation
    error "destroyAgent: Not yet implemented - use OS-native interface"

-- | Send a message to an agent.
sendMessage :: InterfaceHandle -> AgentId -> Text -> IO ()
sendMessage _handle _agentId _message = do
    -- Placeholder implementation
    error "sendMessage: Not yet implemented - use OS-native interface"

-- | Receive a message from an agent (blocking).
receiveMessage :: InterfaceHandle -> AgentId -> IO Text
receiveMessage _handle _agentId = do
    -- Placeholder implementation
    error "receiveMessage: Not yet implemented - use OS-native interface"

-------------------------------------------------------------------------------
-- Conversation Handle
-------------------------------------------------------------------------------

-- | Conversation status.
data ConversationStatus
    = ConversationStatus_Pending
    | ConversationStatus_Active
    | ConversationStatus_WaitingInput
    | ConversationStatus_Completed
    | ConversationStatus_Failed Text
    deriving (Show, Eq)

-- | Handle to a conversation within the interface.
data ConversationHandle = ConversationHandle
    { chConversationId :: ConversationId
    -- ^ Unique conversation identifier
    , chAgentId :: AgentId
    -- ^ Agent participating in this conversation
    , chSession :: Maybe Session
    -- ^ Current session state
    , chAsync :: Maybe (Async ())
    -- ^ Async handle for running conversation
    , chStatus :: ConversationStatus
    -- ^ Current conversation status
    }

-- | Manual Show instance for ConversationHandle (Async doesn't have Show).
instance Show ConversationHandle where
    show ch =
        "ConversationHandle {chConversationId = "
            ++ show ch.chConversationId
            ++ ", chAgentId = "
            ++ show ch.chAgentId
            ++ ", chSession = "
            ++ show ch.chSession
            ++ ", chAsync = <Async>, chStatus = "
            ++ show ch.chStatus
            ++ "}"

-- | Start a new conversation with an agent.
startConversation :: InterfaceHandle -> AgentId -> Text -> IO ConversationHandle
startConversation _handle _agentId _initialMessage = do
    -- Placeholder implementation
    error "startConversation: Not yet implemented - use OS-native interface"

-- | End a conversation.
endConversation :: InterfaceHandle -> ConversationId -> IO ()
endConversation _handle _convId = do
    -- Placeholder implementation
    error "endConversation: Not yet implemented - use OS-native interface"

-- | Get the current status of a conversation.
getConversationStatus :: InterfaceHandle -> ConversationId -> IO ConversationStatus
getConversationStatus handle convId = do
    convs <- readTVarIO (ihConversations handle)
    case Map.lookup convId convs of
        Just conv -> pure (chStatus conv)
        Nothing -> pure (ConversationStatus_Failed "Conversation not found")

-------------------------------------------------------------------------------
-- Event Handling
-------------------------------------------------------------------------------

-- | Subscribe to OS events.
subscribeToEvents :: InterfaceHandle -> IO (TQueue OSEvent)
subscribeToEvents handle = pure (ihEventSub handle)

-- | Unsubscribe from OS events.
unsubscribeFromEvents :: InterfaceHandle -> IO ()
unsubscribeFromEvents _handle = do
    -- In a full implementation, this would remove the subscriber
    pure ()
