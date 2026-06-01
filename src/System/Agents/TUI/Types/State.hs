{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : System.Agents.TUI.Types.State
Description : State management types for the TUI

This module defines types for application state and state transitions,
including Core, UIState, TuiState, and initialization functions.
-}
module System.Agents.TUI.Types.State (
    -- * Core State
    Core (..),
    coreConversations,
    coreAgentTools,
    coreBufferedMessages,
    corePausedConversations,
    coreWorld,
    coreOSEventQueue,
    initCore,

    -- * Focus Ring
    buildFocusRingForTab,

    -- * UI State
    UIState (..),
    uiFocusRing,
    currentTab,
    helpContent,
    turnNavigation,
    queuedMessagesFocus,
    attachedFiles,
    attachmentDialogState,
    filePathInput,
    selectedAttachmentIndex,
    agentList,
    conversationList,
    sessionList,
    messageEditor,
    selectedAgentInfo,
    statusMessage,
    zoomed,
    quitConfirmationPending,
    unreadConversations,
    fileBrowser,
    auxiliaryTasks,
    uiBufferedMessages,
    uiAgentTools,
    buffers,
    bufferFocus,
    initUIState,

    -- * TUI State
    TuiState (..),
    tuiCore,
    tuiUI,
    eventChan,
    sessionConfig,
    keyMapping,
) where

import Brick.BChan (BChan)
import Brick.Focus (FocusRing, focusRing)
import Brick.Widgets.Edit (Editor, editorText)
import Brick.Widgets.FileBrowser (FileBrowser)
import Brick.Widgets.List (List, list)
import Control.Concurrent.STM (TQueue, TVar, newTVarIO)
import Control.Lens (makeLenses)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector

import System.Agents.Base (AgentId, ConversationId (..))
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.OS.Core.World (World)
import System.Agents.OS.Events (OSEvent)
import System.Agents.Session.Base (Session)
import System.Agents.TUI.Buffer (Buffer)
import System.Agents.TUI.KeyMapping (KeyMapping)
import System.Agents.TUI.Types.Conversation (Conversation)
import System.Agents.TUI.Types.Core (
    AppEvent,
    AttachmentDialogState (..),
    AuxiliaryTask,
    SessionConfig (..),
    StatusMessage,
    Tab (..),
    TuiAgent,
    TurnNavigationState,
    WidgetName (..),
 )
import System.Agents.ToolRegistration (ToolRegistration)

-------------------------------------------------------------------------------
-- Core State
-------------------------------------------------------------------------------

{- | Core state shared across the TUI.

This is stored in a TVar for thread-safe access. It contains
mutable state that needs to be accessed from multiple threads.
-}
data Core = Core
    { _coreConversations :: [Conversation]
    -- ^ Active conversations
    , _coreAgentTools :: [(AgentId, [ToolRegistration])]
    -- ^ Tools per agent for display
    , _coreBufferedMessages :: TVar (Map ConversationId [Text])
    -- ^ Buffered messages per conversation (queued while agent is processing)
    , _corePausedConversations :: Set ConversationId
    -- ^ Set of paused conversation IDs
    , _coreWorld :: Maybe World
    {- ^ Optional OS World for ECS operations. Enables subcall visibility
    in the TUI by allowing sub-agent conversations to be tracked as entities.
    -}
    , _coreOSEventQueue :: Maybe (TQueue OSEvent)
    {- ^ Optional OS event queue for subcall event emission. Enables the TUI
    to receive notifications about subcall lifecycle (start, progress, completion).
    -}
    }

makeLenses ''Core

-- | Initialize core state with optional World and EventQueue.
initCore :: Maybe World -> Maybe (TQueue OSEvent) -> IO Core
initCore mWorld mEventQueue = do
    bufferedVar <- newTVarIO Map.empty
    pure
        Core
            { _coreConversations = []
            , _coreAgentTools = []
            , _coreBufferedMessages = bufferedVar
            , _corePausedConversations = Set.empty
            , _coreWorld = mWorld
            , _coreOSEventQueue = mEventQueue
            }

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
    -- ^ File browser widget for attachments
    , _auxiliaryTasks :: [AuxiliaryTask]
    -- ^ Background tasks (e.g., external viewers)
    , _uiBufferedMessages :: Map ConversationId [Text]
    -- ^ Copy of buffered messages from Core for UI rendering
    , _uiAgentTools :: [(AgentId, [ToolRegistration])]
    -- ^ Tools per agent for display (mirror of Core's coreAgentTools)
    , _buffers :: [Buffer]
    -- ^ Global in-memory buffers (most recent first)
    , _bufferFocus :: Maybe Int
    -- ^ Index of selected buffer in the widget
    }

makeLenses ''UIState

{- | Build a focus ring for a given tab.

The ring includes base widgets plus tab-specific widgets inserted appropriately.
The focus ring order is designed so that pressing Tab from a base widget
will first visit the tab-specific widget(s) before moving to the next base widget.
-}
buildFocusRingForTab :: Tab -> FocusRing WidgetName
buildFocusRingForTab tab =
    case tab of
        AgentsTab ->
            focusRing [AgentListWidget, AgentInfoWidget, ConversationListWidget, SessionsListWidget]
        ChatsTab ->
            focusRing [ConversationListWidget, MessageEditorWidget, AttachmentListWidget, BufferListWidget, QueuedMessageListWidget, ConversationViewWidget, SessionsListWidget, AgentListWidget]
        HistoryTab ->
            focusRing [SessionsListWidget, SessionViewWidget, AgentListWidget, ConversationListWidget]
        HelpTab ->
            focusRing [AgentListWidget, ConversationListWidget, SessionsListWidget]

-- | Initialize UI state with default values.
initUIState :: [Text] -> [TuiAgent] -> [Session] -> UIState
initUIState helpText agents sessions =
    UIState
        { _uiFocusRing = buildFocusRingForTab AgentsTab
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
        , _buffers = []
        , _bufferFocus = Nothing
        }

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
