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
    coreClient,
    coreParams,
    coreOwnedSessions,
    initCore,

    -- * Focus Ring
    buildFocusRingForTab,

    -- * UI State
    UIState (..),
    uiFocusRing,
    currentTab,
    helpContent,
    turnNavigation,
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
    buffers,
    bufferFocus,
    toolCallViews,
    historySessionCache,
    historyDirty,
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
import Control.Concurrent.STM (TVar)
import Control.Lens (makeLenses)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector

import System.Agents.Base (ConversationId (..))
import System.Agents.Host.Client (RunnerClient)
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.Session.Base (SessionId)
import System.Agents.SessionStore (SessionMeta)
import System.Agents.Tools.Params.Types (ParamName)
import System.Agents.TUI.Buffer (Buffer)
import System.Agents.TUI.KeyMapping (KeyMapping)
import System.Agents.TUI.ToolCallActivity (ToolCallViews)
import System.Agents.TUI.Types.Conversation (Conversation)
import System.Agents.TUI.Types.Core (
    AppEvent,
    AttachmentDialogState (..),
    AuxiliaryTask,
    HistorySessionEntry (..),
    SessionConfig (..),
    StatusMessage,
    Tab (..),
    TuiAgent,
    TurnNavigationState,
    WidgetName (..),
 )

-------------------------------------------------------------------------------
-- Core State
-------------------------------------------------------------------------------

{- | Core state shared across the TUI.

This is stored in a TVar for thread-safe access. It contains
mutable state that needs to be accessed from multiple threads.

Phase 3b-i: everything that used to be owned by the TUI's own runtime
(@_coreWorld@, @_coreOSEventQueue@, @_coreMailRouter@,
@_coreBufferedMessages@, @_corePausedConversations@) is gone. The TUI now
only keeps its own view of the sessions it has open, plus the
'RunnerClient' it drives them through.
-}
data Core = Core
    { _coreConversations :: [Conversation]
    -- ^ Active conversations (client-side views of sessions)
    , _coreClient :: RunnerClient
    -- ^ The client every command goes through (Design §3).
    , _coreParams :: Map ParamName Aeson.Value
    -- ^ Non-secret and secret parameter values resupplied on every
    -- 'CreateSession'\/'PostMessage' (D7): the TUI's own
    -- @--params-file@, since the runner never stores secrets.
    , _coreOwnedSessions :: Set SessionId
    -- ^ Sessions this TUI process started (embedded mode): on quit, a
    -- 'SendMail' 'StopRun' is due to each of them (-- TODO(3b-ii)).
    }

makeLenses ''Core

-- | Initialize core state with the client this TUI drives its sessions through.
initCore :: RunnerClient -> Map ParamName Aeson.Value -> IO Core
initCore client params =
    pure
        Core
            { _coreConversations = []
            , _coreClient = client
            , _coreParams = params
            , _coreOwnedSessions = Set.empty
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
    , _sessionList :: List WidgetName SessionMeta
    -- ^ List widget for saved sessions (History tab; empty until 3b-iii)
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
    , _buffers :: [Buffer]
    -- ^ Global in-memory buffers (most recent first)
    , _bufferFocus :: Maybe Int
    -- ^ Index of selected buffer in the widget
    , _toolCallViews :: ToolCallViews
    -- ^ Live status of background tool calls, per session
    , _historySessionCache :: Map SessionId HistorySessionEntry
    -- ^ Full 'Session's fetched for the History tab ('Client.getSession'),
    -- keyed by id; overwritten (never merged) on @session.updated@ for that
    -- id, so a cached entry is always the freshest one this TUI has seen.
    -- 'HistoryLoading' while a fetch is in flight, 'HistoryFailed' if it
    -- errored -- see 'System.Agents.TUI.Event.ensureHistorySessionCached'.
    , _historyDirty :: Bool
    -- ^ Set by any @session.created@\/@session.updated@\/@session.deleted@
    -- since the last refresh; the heartbeat refreshes 'sessionList' via
    -- 'Client.listSessions' when this is set, then clears it -- at most one
    -- 'Client.listSessions' round trip per heartbeat even for a burst of
    -- events.
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
            focusRing [ConversationListWidget, MessageEditorWidget, AttachmentListWidget, BufferListWidget, DraftPanelWidget, ConversationViewWidget, SessionsListWidget, AgentListWidget]
        HistoryTab ->
            focusRing [SessionsListWidget, SessionViewWidget, AgentListWidget, ConversationListWidget]
        HelpTab ->
            focusRing [AgentListWidget, ConversationListWidget, SessionsListWidget]

-- | Initialize UI state with default values.
initUIState :: [Text] -> [TuiAgent] -> [SessionMeta] -> UIState
initUIState helpText agents sessions =
    UIState
        { _uiFocusRing = buildFocusRingForTab AgentsTab
        , _currentTab = AgentsTab
        , _helpContent = helpText
        , _turnNavigation = Nothing
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
        , _buffers = []
        , _bufferFocus = Nothing
        , _toolCallViews = Map.empty
        , _historySessionCache = Map.empty
        , _historyDirty = True
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
