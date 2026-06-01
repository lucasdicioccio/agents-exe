{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : System.Agents.TUI.Types.Core
Description : Core type definitions for the TUI system

This module contains the fundamental type definitions used throughout the TUI system,
including widget names, tabs, status messages, events, agent types, and configuration.
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

    -- * Agent Types
    TuiAgent (..),
    agentTree,

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

import System.Agents.AgentTree (LoadedApiKeys, OSAgentNode, OSAgentTree)
import System.Agents.Base (AgentId, ConversationId (..))
import System.Agents.Runtime.Trace (Trace)
import System.Agents.Session.Base (Session, SessionId)
import System.Agents.SessionStore (SessionStore)
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
    | -- | For focusing the queued messages list
      QueuedMessageListWidget
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
    , sessionInputConfig :: InputConfig
    -- ^ Input configuration for message editor
    }

-- | Create a session config with all required fields.
mkSessionConfig :: SessionStore -> LoadedApiKeys -> KeyMapping -> InputConfig -> SessionConfig
mkSessionConfig store apiKeys keymap inputConfig =
    SessionConfig
        { sessionStore = store
        , sessionApiKeys = apiKeys
        , sessionKeyMapping = keymap
        , sessionInputConfig = inputConfig
        }

