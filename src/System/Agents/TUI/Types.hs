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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as Vector

import System.Agents.AgentTree (LoadedApiKeys, OSAgentNode, OSAgentTree)
import System.Agents.Base (AgentId, ConversationId (..))
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.OS.Core.World (World)
import System.Agents.OS.Events (OSEvent)
import System.Agents.Runtime.Trace (Trace)
import System.Agents.Session.Base
import System.Agents.SessionStore (SessionStore)
import System.Agents.TUI.FileBrowser (FileBrowser)
import System.Agents.TUI.KeyMapping (KeyMapping)
import System.Agents.ToolRegistration (ToolRegistration)

-------------------------------------------------------------------------------
-- Widget Names
-------------------------------------------------------------------------------

-- | Widget names for the TUI focus ring and viewport management.
data WidgetName
    = -- | Agent list in the left sidebar
      AgentListWidget
    | -- | Agent information/details panel
      AgentInfoWidget
    | -- | Conversation list in the left sidebar
      ConversationListWidget
    | -- | Saved sessions list in the History tab
      SessionsListWidget
    | -- | Message editor input area
      MessageEditorWidget
    | -- | Main conversation display viewport
      ConversationViewWidget
    | -- | Session content display viewport
      SessionViewWidget
    | -- | For viewport scrolling during turn navigation
      TurnNavigationWidget
    | -- | For focusing the queued messages list
      QueuedMessageListWidget
    | -- | For the attachment list below the message editor
      AttachmentListWidget
    | -- | For the file path input dialog
      FilePathInputWidget
    deriving (Show, Eq, Ord)

-- | Type alias for widget names.
type N = WidgetName

-------------------------------------------------------------------------------
-- Tab Types
-------------------------------------------------------------------------------

-- | Tabs available in the TUI interface.
data Tab
    = AgentsTab
    | ChatsTab
    | HistoryTab
    | HelpTab
    deriving (Show, Eq)

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
-- Turn Navigation Types
-------------------------------------------------------------------------------

-- | State for turn-by-turn navigation
data TurnNavigationState = TurnNavigationState
    { _navSession :: Session
    -- ^ The session being navigated
    , _navSelectedTurnIndex :: Int
    -- ^ Currently selected turn index (0-based)
    , _navTotalTurns :: Int
    -- ^ Total number of turns for display
    }
    deriving (Show)

makeLenses ''TurnNavigationState

-------------------------------------------------------------------------------
-- Attachment Dialog State
-------------------------------------------------------------------------------

-- | State for the file attachment dialog
data AttachmentDialogState
    = -- | No dialog is open
      AttachmentDialogClosed
    | -- | Path input dialog is open (legacy text input)
      AttachmentDialogPathInput
    | -- | FileBrowser widget dialog is open
      AttachmentDialogFileBrowser
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Application Events
-------------------------------------------------------------------------------

-- | Events that can be sent to the TUI application.
data AppEvent
    = AppEvent_Heartbeat
    | AppEvent_AgentStepProgrress ConversationId Session
    | AppEvent_AgentNeedsInput ConversationId
    | AppEvent_AgentTrace Trace
    | AppEvent_ShowStatus StatusSeverity Text
    | AppEvent_ClearStatus
    | -- \** Subcall Events for TUI visibility

      -- | A subcall conversation has started
      AppEvent_SubcallStarted
        { appSubcallParentId :: ConversationId
        , appSubcallId :: ConversationId
        , appSubcallAgentSlug :: Text
        , appSubcallDepth :: Int
        }
    | -- | A subcall has made progress
      AppEvent_SubcallProgress ConversationId Session
    | -- | A subcall has completed successfully
      AppEvent_SubcallCompleted ConversationId Text
    | -- | A subcall has failed
      AppEvent_SubcallFailed ConversationId Text
    deriving (Show)

-------------------------------------------------------------------------------
-- Agent Types (OS-Native)
-------------------------------------------------------------------------------

{- | TUI Agent using OS-native structures.

This structure wraps an OSAgentNode for use in the TUI.
Tools are accessed directly from the node's tools TVar.
-}
data TuiAgent = TuiAgent
    { tuiAgentId :: AgentId
    -- ^ Unique identifier for this agent
    , tuiTree :: OSAgentTree
    -- ^ The agent's tree structure
    , tuiNode :: OSAgentNode
    -- ^ The specific node for this agent
    , tuiSlug :: Text
    -- ^ The agent's slug
    }

-- | Manual Show instance for TuiAgent.
instance Show TuiAgent where
    show agent =
        "TuiAgent {tuiAgentId = "
            ++ show agent.tuiAgentId
            ++ ", tuiSlug = "
            ++ show agent.tuiSlug
            ++ ", tuiTree = <OSAgentTree>, tuiNode = <OSAgentNode>}"

-- | Legacy accessor for backward compatibility.
agentTree :: TuiAgent -> OSAgentTree
agentTree = tuiTree

-------------------------------------------------------------------------------
-- Layout Configuration
-------------------------------------------------------------------------------

-- | Layout modes for the TUI display.
data LayoutMode
    = -- | Single agent view (default)
      SingleAgent
    | -- | Split screen vertically
      SplitVertical
    | -- | Split screen horizontally
      SplitHorizontal
    | -- | Grid with rows and columns
      GridLayout Int Int
    | -- | Tabbed interface for switching between agents
      Tabbed
    deriving (Show, Eq)

-- | TUI configuration including layout and theme.
data TUIConfig = TUIConfig
    { tuiTheme :: Theme
    , tuiKeyBindings :: Map Key EventType
    , tuiLayout :: LayoutMode
    }
    deriving (Show)

-- | Theme configuration (placeholder - to be expanded).
newtype Theme = Theme {themeName :: Text}
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

{- | A conversation with an agent.

Conversations can be either root-level user-initiated conversations
or subcalls (nested agent invocations). Subcall conversations have
additional metadata for TUI visibility and lineage tracking.
-}
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
    , conversationIsSubcall :: Bool
    -- ^ Whether this is a subcall (nested agent invocation)
    , conversationParentId :: Maybe ConversationId
    -- ^ Parent conversation ID for subcalls (Nothing for root conversations)
    , conversationSubcallDepth :: Int
    -- ^ Subcall nesting depth (0 = root, 1+ = nested)
    }

-- | Manual Show instance for Conversation.
instance Show Conversation where
    show conv =
        "Conversation {conversationId = "
            ++ show conv.conversationId
            ++ ", conversationAgent = "
            ++ show conv.conversationAgent
            ++ ", conversationName = "
            ++ show conv.conversationName
            ++ ", conversationIsSubcall = "
            ++ show conv.conversationIsSubcall
            ++ ", conversationParentId = "
            ++ show conv.conversationParentId
            ++ ", ...}"

-------------------------------------------------------------------------------
-- Auxiliary Task Types
-------------------------------------------------------------------------------

-- | Auxiliary tasks running in the background (e.g., external viewers).
data AuxiliaryTask
    = -- | External markdown viewer task
      Viewer
      { viewerAsync :: Async ()
      , viewerConversationId :: ConversationId
      , viewerSessionId :: SessionId
      }

-------------------------------------------------------------------------------
-- Session Configuration
-------------------------------------------------------------------------------

-- | Configuration for TUI sessions.
data SessionConfig = SessionConfig
    { sessionStore :: SessionStore
    -- ^ Storage for sessions
    , sessionApiKeys :: LoadedApiKeys
    -- ^ API keys for agents
    , sessionKeyMapping :: KeyMapping
    -- ^ Key mapping for keyboard shortcuts
    }

-------------------------------------------------------------------------------
-- Core State
-------------------------------------------------------------------------------

{- | Core state shared across the TUI.

This is stored in a TVar for thread-safe access. It contains
mutable state that needs to be accessed from multiple threads.
-}
data Core = Core
    { coreConversations :: [Conversation]
    -- ^ Active conversations
    , coreAgentTools :: [(AgentId, [ToolRegistration])]
    -- ^ Tools per agent for display
    , coreBufferedMessages :: TVar (Map ConversationId [Text])
    -- ^ Buffered messages per conversation (queued while agent is processing)
    , corePausedConversations :: Set ConversationId
    -- ^ Set of paused conversation IDs
    , coreWorld :: Maybe World
    {- ^ Optional OS World for ECS operations. Enables subcall visibility
    in the TUI by allowing sub-agent conversations to be tracked as entities.
    -}
    , coreOSEventQueue :: Maybe (TQueue OSEvent)
    {- ^ Optional OS event queue for subcall event emission. Enables the TUI
    to receive notifications about subcall lifecycle (start, progress, completion).
    -}
    }

makeLenses ''Core

-------------------------------------------------------------------------------
-- UI State
-------------------------------------------------------------------------------

{- | UI-specific state for the TUI.

This contains all the visual/interaction state that doesn't need
to be thread-safe and is only accessed from the UI thread.
-}
data UIState = UIState
    { _uiFocusRing :: FocusRing WidgetName
    -- ^ Focus ring for widget navigation
    , _currentTab :: Tab
    -- ^ Currently active tab
    , _helpContent :: [Text]
    -- ^ Help text content lines
    , _turnNavigation :: Maybe TurnNavigationState
    -- ^ When Just, we are in turn navigation mode
    , _queuedMessagesFocus :: Maybe Int
    -- ^ Index of currently selected queued message
    , _attachedFiles :: Map ConversationId [MediaAttachment]
    -- ^ Media attachments per conversation
    , _attachmentDialogState :: AttachmentDialogState
    -- ^ File attachment dialog state
    , _filePathInput :: Editor Text WidgetName
    -- ^ Editor for file path input
    , _selectedAttachmentIndex :: Maybe Int
    -- ^ Selected attachment index
    , _agentList :: List WidgetName TuiAgent
    -- ^ List widget for agents
    , _conversationList :: List WidgetName Conversation
    -- ^ List widget for conversations
    , _sessionList :: List WidgetName Session
    -- ^ List widget for saved sessions
    , _messageEditor :: Editor Text WidgetName
    -- ^ Editor for message input
    , _selectedAgentInfo :: Maybe TuiAgent
    -- ^ Currently selected agent for display
    , _statusMessage :: Maybe StatusMessage
    -- ^ Current status message (if any)
    , _zoomed :: Bool
    -- ^ Whether zoom mode is active
    , _quitConfirmationPending :: Bool
    -- ^ Whether quit confirmation is pending
    , _unreadConversations :: Set ConversationId
    -- ^ Set of conversations with unread messages
    , _fileBrowser :: Maybe (FileBrowser WidgetName)
    -- ^ File browser widget for attachments (using custom implementation)
    , _auxiliaryTasks :: [AuxiliaryTask]
    -- ^ Background tasks (e.g., external viewers)
    , _uiBufferedMessages :: Map ConversationId [Text]
    -- ^ Copy of buffered messages from Core for UI rendering
    , _uiAgentTools :: [(AgentId, [ToolRegistration])]
    -- ^ Tools per agent for display (mirror of Core's coreAgentTools)
    }

makeLenses ''UIState

-------------------------------------------------------------------------------
-- TUI State
-------------------------------------------------------------------------------

-- | Complete TUI state combining core and UI components.
data TuiState = TuiState
    { _tuiCore :: TVar Core
    -- ^ Thread-safe core state
    , _tuiUI :: UIState
    -- ^ UI-specific state
    , _eventChan :: BChan AppEvent
    -- ^ Channel for application events
    , _sessionConfig :: SessionConfig
    -- ^ Session configuration
    , _keyMapping :: KeyMapping
    -- ^ Current key mapping for keyboard shortcuts
    }

makeLenses ''TuiState

-------------------------------------------------------------------------------
-- Initialization Functions
-------------------------------------------------------------------------------

-- | Initialize UI state with default values.
initUIState :: [Text] -> [TuiAgent] -> [Session] -> UIState
initUIState helpText agents sessions =
    UIState
        { _uiFocusRing = focusRing [AgentListWidget]
        , _currentTab = AgentsTab
        , _helpContent = helpText
        , _turnNavigation = Nothing
        , _queuedMessagesFocus = Nothing
        , _attachedFiles = Map.empty
        , _attachmentDialogState = AttachmentDialogClosed
        , _filePathInput = editorText FilePathInputWidget (Just 1) ""
        , _selectedAttachmentIndex = Nothing
        , _agentList = list AgentListWidget (Vector.fromList agents) 1
        , _conversationList = list ConversationListWidget Vector.empty 1
        , _sessionList = list SessionsListWidget (Vector.fromList sessions) 1
        , _messageEditor = editorText MessageEditorWidget Nothing ""
        , _selectedAgentInfo = listToMaybe agents
        , _statusMessage = Nothing
        , _zoomed = False
        , _quitConfirmationPending = False
        , _unreadConversations = Set.empty
        , _fileBrowser = Nothing
        , _auxiliaryTasks = []
        , _uiBufferedMessages = Map.empty
        , _uiAgentTools = []
        }

-- | Initialize core state with optional World and EventQueue.
initCore :: Maybe World -> Maybe (TQueue OSEvent) -> IO Core
initCore mWorld mEventQueue = do
    bufferedVar <- newTVarIO Map.empty
    pure
        Core
            { coreConversations = []
            , coreAgentTools = []
            , coreBufferedMessages = bufferedVar
            , corePausedConversations = Set.empty
            , coreWorld = mWorld
            , coreOSEventQueue = mEventQueue
            }

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Update a conversation's session in a list of conversations.
updateConversationSession :: ConversationId -> Session -> [Conversation] -> [Conversation]
updateConversationSession targetConvId newSession =
    map
        ( \conv ->
            if conversationId conv == targetConvId
                then conv{conversationSession = Just newSession}
                else conv
        )

