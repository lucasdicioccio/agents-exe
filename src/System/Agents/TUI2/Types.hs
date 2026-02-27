{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core types and data structures for the TUI application.
module System.Agents.TUI2.Types where

import Brick.BChan (BChan)
import Brick.Focus (FocusRing, focusRing)
import Brick.Widgets.Edit (Editor, editorText)
import Brick.Widgets.List (List, list)
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TVar)
import Control.Lens (makeLenses)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector

import System.Agents.AgentTree (AgentTree)
import System.Agents.ToolRegistration (ToolRegistration)
import System.Agents.Base (AgentId, ConversationId (..))
import qualified System.Agents.Runtime as Runtime
import System.Agents.Session.Base

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
    | AgentToolsWidget
    deriving (Show, Eq, Ord)

-- | Type alias for widget names.
type N = WidgetName

-------------------------------------------------------------------------------
-- Application Events
-------------------------------------------------------------------------------

-- | Events that can be sent to the TUI application.
data AppEvent
    = AppEvent_Heartbeat
    | AppEvent_AgentStepProgrress ConversationId Session
    | AppEvent_AgentNeedsInput ConversationId
    | AppEvent_AgentTrace Runtime.Trace
    deriving (Show)

-------------------------------------------------------------------------------
-- Agent Types
-------------------------------------------------------------------------------

-- | Wrapper for an agent with its tree structure.
data TuiAgent = TuiAgent
    { sessionAgent :: Agent (LlmTurnContent, Session)
    , agentTree :: AgentTree
    }

-------------------------------------------------------------------------------
-- Conversation Types
-------------------------------------------------------------------------------

-- | Status of a conversation regarding its execution state.
data ConversationStatus
    = ConversationStatus_Active
    -- ^ Conversation is currently running (agent is processing)
    | ConversationStatus_WaitingForInput
    -- ^ Conversation is waiting for user input
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
    , conversationFilePath :: Maybe FilePath
    -- ^ Optional path to the session file on disk (Nothing if not persisting)
    , conversationOnProgress :: OnSessionProgress
    -- ^ Callback for session progress updates
    }

-------------------------------------------------------------------------------
-- Session Configuration
-------------------------------------------------------------------------------

-- | Configuration for session handling in the TUI.
data SessionConfig = SessionConfig
    { sessionOnProgress :: OnSessionProgress
    -- ^ Callback for session progress. Defaults to 'ignoreSessionProgress'.
    , sessionFilePrefix :: Maybe FilePath
    -- ^ Optional file prefix for loading existing sessions and generating file paths.
    }

-- | Default session configuration with no persistence.
defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
    { sessionOnProgress = ignoreSessionProgress
    , sessionFilePrefix = Nothing
    }

-------------------------------------------------------------------------------
-- Core State
-------------------------------------------------------------------------------

-- | The core state holding agents and conversations.
data Core = Core
    { coreAgents :: [TuiAgent]
    , coreConversations :: [Conversation]
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
    -- ^ Conversations currently being processed by an agent
    , _coreAgentTools :: [(AgentId,[ToolRegistration])]
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
              , AgentToolsWidget
              ]
        , _zoomed = False
        , _agentList = list AgentListWidget (Vector.fromList agents) 1
        , _conversationList = list ConversationListWidget Vector.empty 1
        , _sessionList = list SessionsListWidget (Vector.fromList loadedSessions) 1
        , _messageEditor = editorText MessageEditorWidget (Just 1) ""
        , _selectedAgentInfo = listToMaybe agents
        , _unreadConversations = Set.empty
        , _ongoingConversations = Set.empty
        , _coreAgentTools = []
        }

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

