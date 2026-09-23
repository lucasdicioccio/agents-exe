{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : System.Agents.TUI.Types.Core
Description : Core type definitions for the TUI system

This module contains the fundamental type definitions used throughout the TUI system,
including widget names, tabs, status messages, events, agent types, and configuration.

Phase 3b-i (@todos/os-as-standalone-server.md@): the TUI is a client of
'System.Agents.Host.Client.RunnerClient'. 'TuiAgent' wraps a serializable
'AgentDescriptor' instead of an OS-native 'System.Agents.OS.AgentHandle.AgentHandle',
and 'AppEvent' carries runner-shaped payloads instead of raw 'OSEvent's.
-}
module System.Agents.TUI.Types.Core (
    -- * Widget Names
    WidgetName (..),
    N,

    -- * Tabs
    Tab (..),

    -- * Status Messages
    StatusSeverity (..),
    StatusMessage (..),

    -- * Turn Navigation
    TurnNavigationState (..),
    navSession,
    navSelectedTurnIndex,
    navTotalTurns,

    -- * Attachment Dialog
    AttachmentDialogState (..),

    -- * Application Events
    AppEvent (..),

    -- * History tab session cache
    HistorySessionEntry (..),

    -- * Agent Types
    TuiAgent (..),
    tuiSlug,

    -- * Layout and Configuration
    LayoutMode (..),
    TUIConfig (..),
    Theme (..),
    Key (..),
    EventType (..),

    -- * Auxiliary Tasks
    AuxiliaryTask (..),

    -- * Session Configuration
    SessionConfig (..),
    mkSessionConfig,
) where

import Control.Concurrent.Async (Async)
import Control.Lens (makeLenses)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)

import System.Agents.Base (ConversationId (..))
import System.Agents.OS.Events (ToolCallActivity)
import System.Agents.Protocol (AgentDescriptor (..), RunMode)
import System.Agents.Runtime.Trace (Trace)
import System.Agents.Session.Base (
    DeferredCallView,
    Session,
    SessionId,
    SessionStatus,
 )
import System.Agents.SessionStore (SessionMeta)
import System.Agents.TUI.KeyMapping (KeyMapping)
import System.Agents.TUI.MessageComposer (InputConfig)

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
    | -- | For focusing the draft panel
      DraftPanelWidget
    | -- | For focusing the pending-calls panel (Phase 3c)
      PendingPanelWidget
    | -- | For the attachment list below the message editor
      AttachmentListWidget
    | -- | For the file path input dialog
      FilePathInputWidget
    | -- | For the inline buffer list below the message editor
      BufferListWidget
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

{- | Events that can be sent to the TUI application.

Phase 3b-i: these carry 'RunnerClient'-shaped payloads
("System.Agents.Protocol"'s 'EventBody', once 3b-ii wires
@Client.subscribeAll@ to this channel) instead of raw OS events. Known
regression, accepted until Phase 5 (see the spec's Phase 3b note):
there is no per-step "subcall progress" event carrying a whole child
'Session' any more, only started\/completed\/failed.
-}
data AppEvent
    = AppEvent_Heartbeat
    | -- | A session got a new stored version: its id, the fresh 'Session'
      -- and 'SessionMeta' (@session.updated@; bridged from
      -- 'System.Agents.Host.Client.subscribeAll' in
      -- 'System.Agents.TUI.Core.bridgeRunnerEvents').
      AppEvent_SessionUpdated ConversationId Session SessionMeta
    | -- | A run started on a session (@run.started@).
      AppEvent_RunStarted ConversationId RunMode
    | -- | A run stopped; the resulting status tells whether the session
      -- now accepts input, is paused, or blocked on deferred calls
      -- (@run.stopped@; replaces the old @AppEvent_AgentNeedsInput@).
      AppEvent_RunStopped ConversationId SessionStatus
    | -- | A session's run failed (@session.failed@).
      AppEvent_SessionFailed ConversationId Text
    | -- | Calls are now deferred, awaiting an external result (@calls.deferred@).
      AppEvent_CallsDeferred ConversationId [DeferredCallView]
    | -- | A new session was created, anywhere in this owner's tree (@session.created@).
      AppEvent_SessionCreated SessionMeta
    | -- | A session was deleted (@session.deleted@).
      AppEvent_SessionDeleted ConversationId
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
    | -- | A subcall has completed successfully
      AppEvent_SubcallCompleted ConversationId Text
    | -- | A subcall has failed
      AppEvent_SubcallFailed ConversationId Text
    | -- | A background tool call started, progressed, or finished
      AppEvent_ToolCallActivity ToolCallActivity
    | -- | The agent roster changed (@agents.changed@ / @ListAgents@ refresh).
      AppEvent_AgentsRefreshed [AgentDescriptor]
    | -- | The History tab's session list was refreshed ('Client.listSessions').
      AppEvent_SessionsRefreshed [SessionMeta]
    deriving (Show)

{- | What the History tab knows about one selected session's full 'Session'
(@todos/os-as-standalone-server.md@ §4, Phase 3b-iv): 'ensureHistorySessionCached'
in "System.Agents.TUI.Event" inserts 'HistoryLoading' before issuing the
'Client.getSession' fetch, so the view has something to render while it is
in flight, and replaces it with 'HistoryLoaded' or 'HistoryFailed' when the
fetch settles -- a failed fetch is no longer silently dropped.
-}
data HistorySessionEntry
    = HistoryLoading
    | HistoryLoaded Session
    | HistoryFailed Text
    deriving (Show)

-------------------------------------------------------------------------------
-- Agent Types
-------------------------------------------------------------------------------

{- | TUI Agent, a thin wrapper over the runner's serializable
'AgentDescriptor' (Phase 3a). No live handle, no OS-native tree: every
detail the TUI shows about an agent (model, prompt, tools with their
'System.Agents.Tools.Activation.Activation', helpers) is already on the
descriptor 'ListAgents'\/'GetAgent' returns.
-}
newtype TuiAgent = TuiAgent
    { tuiAgentDescriptor :: AgentDescriptor
    }
    deriving (Show, Eq)

-- | The agent's slug.
tuiSlug :: TuiAgent -> Text
tuiSlug = adSlug . tuiAgentDescriptor

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
{- | The legacy file-store fallback used to be threaded through here so the
TUI could composite it into its own backend by hand; since
@todos/os-as-standalone-server.md@ §6 (Phase 3b-iv), 'Host.hcLegacySessionDirs'
does that compositing once, inside the 'System.Agents.Host.Host' the TUI's
runner is built over, so this config carries only what the TUI itself
still needs.
-}
data SessionConfig = SessionConfig
    { sessionKeyMapping :: KeyMapping
    -- ^ Key mapping for keyboard shortcuts
    , sessionInputConfig :: InputConfig
    -- ^ Input configuration for message editor
    }

-- | Create a session config with all required fields.
mkSessionConfig :: KeyMapping -> InputConfig -> SessionConfig
mkSessionConfig keymap inputConfig =
    SessionConfig
        { sessionKeyMapping = keymap
        , sessionInputConfig = inputConfig
        }
