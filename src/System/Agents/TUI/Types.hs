{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core types and data structures for the TUI application.
module System.Agents.TUI.Types where

import Brick.BChan (BChan)
import Brick.Focus (FocusRing, focusRing)
import Brick.Widgets.Edit (Editor, editorText)
import Brick.Widgets.List (List, list)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TQueue, TVar, newTVarIO)
import Control.Lens (makeLenses)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as Vector

import System.Agents.AgentTree (AgentTree)
import System.Agents.Base (AgentId, ConversationId (..))
import System.Agents.OS.Compat.Runtime (RuntimeBridge)
import qualified System.Agents.Runtime as Runtime
import System.Agents.Session.Base
import System.Agents.SessionStore (SessionStore)
import System.Agents.ToolRegistration (ToolRegistration)

-------------------------------------------------------------------------------
-- Widget Names
-------------------------------------------------------------------------------

-- | Widget names for Brick focus management.
data WidgetName
    = AgentListWidget
    | SessionsListWidget
    | ConversationListWidget
    | MessageEditorWidget
    | ConversationViewWidget
    | SessionViewWidget
    | AgentInfoWidget
    deriving (Show, Eq, Ord)

-- | Type alias for widget names.
type N = WidgetName

-------------------------------------------------------------------------------
-- Status Message Types
-------------------------------------------------------------------------------

-- | Severity level for status messages.
data StatusSeverity
    = StatusInfo
    | StatusWarning
    | StatusError
    deriving (Show, Eq)

-- | A status message with timestamp for auto-clear logic.
data StatusMessage = StatusMessage
    { statusText :: Text
    , statusSeverity :: StatusSeverity
    , statusTimestamp :: UTCTime
    }
    deriving (Show)

-------------------------------------------------------------------------------
-- Application Events
-------------------------------------------------------------------------------

-- | Events that can be sent to the TUI application.
data AppEvent
    = AppEvent_Heartbeat
    | AppEvent_AgentStepProgrress ConversationId Session
    | AppEvent_AgentNeedsInput ConversationId
    | AppEvent_AgentTrace Runtime.Trace
    | AppEvent_ShowStatus StatusSeverity Text
    | AppEvent_ClearStatus
    deriving (Show)

-------------------------------------------------------------------------------
-- Agent Types (OS-Compatible)
-------------------------------------------------------------------------------

{- | TUI Agent using RuntimeBridge for OS compatibility.

This structure replaces the old direct Agent usage with a RuntimeBridge
that connects to the new OS model. The bridge provides the same interface
while using the OS backend.
-}
data TuiAgent = TuiAgent
    { tuiAgentId :: AgentId
    -- ^ Unique identifier for this agent
    , tuiBridge :: RuntimeBridge
    -- ^ Bridge to the OS for runtime operations
    , tuiTree :: AgentTree
    -- ^ The agent's tree structure (kept for metadata access)
    }

-- | Legacy accessor for backward compatibility during migration.
sessionAgent :: TuiAgent -> Agent (LlmTurnContent, Session)
sessionAgent _ = error "sessionAgent: Deprecated, use RuntimeBridge interface"

-- | Legacy accessor for backward compatibility during migration.
agentTree :: TuiAgent -> AgentTree
agentTree = tuiTree

-------------------------------------------------------------------------------
-- Multi-Agent Coordination Types
-------------------------------------------------------------------------------

-- | Role definition for agents in multi-agent conversations.
data AgentRole
    = AgentRole_Orchestrator
    -- ^ Coordinates other agents
    | AgentRole_Specialist Text
    -- ^ Specialized agent with a specific role description
    | AgentRole_Worker
    -- ^ General worker agent
    | AgentRole_Observer
    -- ^ Observes but doesn't initiate
    deriving (Show, Eq)

-- | Configuration for an agent's role in multi-agent mode.
data AgentRoleConfig = AgentRoleConfig
    { arcAgentId :: AgentId
    , arcRole :: AgentRole
    , arcCanInitiate :: Bool
    -- ^ Whether this agent can start new conversations
    , arcSubscribesTo :: [AgentId]
    -- ^ Agents this agent listens to for messages
    }
    deriving (Show)

-- | Strategy for coordinating multiple agents.
data CoordinationStrategy
    = CoordinationStrategy_RoundRobin
    -- ^ Each agent takes turns
    | CoordinationStrategy_Hierarchical AgentId
    -- ^ One agent orchestrates others
    | CoordinationStrategy_Collaborative
    -- ^ Agents collaborate freely
    deriving (Show, Eq)

-- | Configuration for multi-agent conversations.
data MultiAgentConfig = MultiAgentConfig
    { maAgents :: [AgentRoleConfig]
    , maCoordinationStrategy :: CoordinationStrategy
    }
    deriving (Show)

-- | Message type for inter-agent communication.
data MessageType
    = MessageType_Direct
    -- ^ Direct message to specific agent
    | MessageType_Broadcast
    -- ^ Message to all subscribed agents
    | MessageType_Response
    -- ^ Response to a previous message
    | MessageType_Request
    -- ^ Request for action from another agent
    deriving (Show, Eq)

-- | Message sent between agents.
data InterAgentMessage = InterAgentMessage
    { iamFrom :: AgentId
    , iamTo :: AgentId
    , iamType :: MessageType
    , iamContent :: Value
    }
    deriving (Show)

-- | Agent bus for inter-agent communication.
newtype AgentBus = AgentBus
    { busChannels :: TVar (Map AgentId (TQueue InterAgentMessage))
    }

-------------------------------------------------------------------------------
-- Layout Configuration
-------------------------------------------------------------------------------

-- | Layout modes for the TUI display.
data LayoutMode
    = SingleAgent
    -- ^ Single agent view (default)
    | SplitVertical
    -- ^ Split screen vertically
    | SplitHorizontal
    -- ^ Split screen horizontally
    | GridLayout Int Int
    -- ^ Grid with rows and columns
    | Tabbed
    -- ^ Tabbed interface for switching between agents
    deriving (Show, Eq)

-- | TUI configuration including layout and theme.
data TUIConfig = TUIConfig
    { tuiTheme :: Theme
    , tuiKeyBindings :: Map Key EventType
    , tuiLayout :: LayoutMode
    }
    deriving (Show)

-- | Theme configuration (placeholder - to be expanded).
newtype Theme = Theme { themeName :: Text }
    deriving (Show, Eq)

-- | Key type for keybindings.
newtype Key = Key Text
    deriving (Show, Eq, Ord)

-- | Event type for keybindings.
newtype EventType = EventType Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Conversation Types
-------------------------------------------------------------------------------

-- | Status of a conversation regarding its execution state.
data ConversationStatus
    = -- | Conversation is currently running (agent is processing)
      ConversationStatus_Active
    | -- | Conversation is waiting for user input
      ConversationStatus_WaitingForInput
    | -- | Conversation is paused (blocks step iteration until unpaused)
      ConversationStatus_Paused
    deriving (Show, Eq)

-- | A conversation with an agent.
data Conversation = Conversation
    { conversationId :: ConversationId
    , conversationAgent :: TuiAgent
    , conversationThreadId :: Maybe ThreadId
    -- ^ Nothing for restored conversations that haven't been continued yet
    , conversationSession :: Maybe Session
    , conversationName :: Text
    , conversationChan :: BChan (Maybe UserQuery)
    , conversationStatus :: ConversationStatus
    -- ^ Current status of the conversation
    , conversationOnProgress :: OnSessionProgress
    -- ^ Callback for session progress updates
    }

-------------------------------------------------------------------------------
-- Auxiliary Task Types
-------------------------------------------------------------------------------

{- | An auxiliary task running asynchronously in the background.
These tasks don't block the TUI but are tracked for cleanup.
-}
data AuxiliaryTask
    = -- | An external viewer process viewing a specific conversation/session
      Viewer (Async ()) ConversationId SessionId

instance Show AuxiliaryTask where
    show (Viewer _ convId sessId) =
        "Viewer <async> " ++ show convId ++ " " ++ show sessId

-------------------------------------------------------------------------------
-- Session Configuration
-------------------------------------------------------------------------------

-- | Configuration for session handling in the TUI.
data SessionConfig = SessionConfig
    { sessionStore :: SessionStore
    }

-------------------------------------------------------------------------------
-- Core State
-------------------------------------------------------------------------------

-- | The core state holding agents and conversations.
data Core = Core
    { coreAgents :: [TuiAgent]
    , coreConversations :: [Conversation]
    , corePausedConversations :: Set ConversationId
    -- ^ Set of conversation IDs that are currently paused
    , coreBufferedMessages :: TVar (Map ConversationId [Text])
    {- ^ Buffered messages per conversation - allows users to send messages
    while the agent is processing tool calls. Messages are consumed and
    concatenated when the agent collects user input.
    -}
    }

makeLenses ''Core

-------------------------------------------------------------------------------
-- UI State
-------------------------------------------------------------------------------

-- | UI-related state.
data UIState = UIState
    { _uiFocusRing :: FocusRing WidgetName
    , _zoomed :: Bool
    , _agentList :: List WidgetName TuiAgent
    , _sessionList :: List WidgetName Session
    , _conversationList :: List WidgetName Conversation
    , _messageEditor :: Editor Text WidgetName
    , _selectedAgentInfo :: Maybe TuiAgent
    , _unreadConversations :: Set ConversationId
    , _ongoingConversations :: Set ConversationId
    {- ^ Conversations currently being processed by an agent
    (kept for backward compatibility and heartbeat tracking)
    -}
    , _auxiliaryTasks :: [AuxiliaryTask]
    -- ^ Background async tasks (e.g., external viewers)
    , _coreAgentTools :: [(AgentId, [ToolRegistration])]
    , _statusMessage :: Maybe StatusMessage
    -- ^ Current status message to display (if any)
    }

makeLenses ''UIState

-------------------------------------------------------------------------------
-- Main TUI State
-------------------------------------------------------------------------------

-- | Main TUI state combining core and UI.
data TuiState = TuiState
    { _tuiCore :: TVar Core
    , _tuiUI :: UIState
    , _eventChan :: BChan AppEvent
    , _sessionConfig :: SessionConfig
    }

makeLenses ''TuiState

-------------------------------------------------------------------------------
-- Initialization Helpers
-------------------------------------------------------------------------------

-- | Create initial UI state.
initUIState :: [TuiAgent] -> [Session] -> UIState
initUIState agents loadedSessions =
    UIState
        { _uiFocusRing =
            focusRing
                [ AgentListWidget
                , ConversationListWidget
                , SessionsListWidget
                , MessageEditorWidget
                , SessionViewWidget
                , AgentInfoWidget
                ]
        , _zoomed = False
        , _agentList = list AgentListWidget (Vector.fromList agents) 1
        , _conversationList = list ConversationListWidget Vector.empty 1
        , _sessionList = list SessionsListWidget (Vector.fromList loadedSessions) 1
        , _messageEditor = editorText MessageEditorWidget (Just 1) ""
        , _selectedAgentInfo = listToMaybe agents
        , _unreadConversations = Set.empty
        , _ongoingConversations = Set.empty
        , _auxiliaryTasks = []
        , _coreAgentTools = []
        , _statusMessage = Nothing
        }

-- | Create initial Core state.
initCore :: [TuiAgent] -> IO Core
initCore agents = do
    bufferVar <- newTVarIO Map.empty
    pure $ Core agents [] Set.empty bufferVar

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Update a conversation's session in the list.
updateConversationSession :: ConversationId -> Session -> [Conversation] -> [Conversation]
updateConversationSession convId newSession =
    map (\c -> if conversationId c == convId then c{conversationSession = Just newSession} else c)

-- | Update a conversation in the list.
updateConversation :: Conversation -> [Conversation] -> [Conversation]
updateConversation conv =
    map (\c -> if conversationId c == conversationId conv then conv else c)

