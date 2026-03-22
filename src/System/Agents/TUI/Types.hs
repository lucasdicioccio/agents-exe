{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core types and data structures for the TUI application.
module System.Agents.TUI.Types where

import Brick.BChan (BChan, writeBChan)
import Brick.Focus (FocusRing, focusRing)
import Brick.Widgets.Edit (Editor, editorText)
import Brick.Widgets.List (List, list)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Lens (makeLenses)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Vector as Vector

import System.Agents.AgentTree (AgentTree)
import System.Agents.Base (AgentId, ConversationId (..))
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
    -- NEW EVENTS:
    | AppEvent_SubAgentSessionStarted ConversationId Session
    -- ^ Emitted when a sub-agent session starts (parent conversation ID, child session)
    | AppEvent_SubAgentSessionUpdated ConversationId Session
    -- ^ Emitted when a sub-agent session updates (parent conversation ID, updated session)
    | AppEvent_SubAgentSessionCompleted ConversationId Session
    -- ^ Emitted when a sub-agent session completes (parent conversation ID, completed session)
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
-- Conversation Tree Types
-------------------------------------------------------------------------------

-- | A node in the conversation tree representing either a root or child conversation.
--
-- This type enables hierarchical display of conversations in the TUI, where
-- sub-agent calls are shown as children of their parent conversations.
--
-- Example tree structure:
-- @
-- ConversationNode (root conversation)
--   ├── ConversationNode (sub-agent 1)
--   │     └── ConversationNode (nested sub-agent)
--   └── ConversationNode (sub-agent 2)
-- @
data ConversationNode = ConversationNode
    { nodeConversation :: Conversation
    -- ^ The conversation at this node
    , nodeChildren :: [ConversationNode]
    -- ^ Child conversations (sub-agent calls)
    , nodeExpanded :: Bool
    -- ^ Whether children are visible in the UI
    }

-- | Build a conversation tree from a flat list of conversations.
--
-- This function organizes conversations hierarchically based on their
-- parent-child relationships. Root conversations (those without a
-- 'parentConversationId' in their session) appear at the top level,
-- with child conversations nested under their parents.
--
-- Conversations without session information are treated as root nodes
-- since their parent relationship cannot be determined.
--
-- Example:
-- @
-- let tree = buildConversationTree conversations
-- -- tree now contains hierarchical structure for UI display
-- @
buildConversationTree :: [Conversation] -> [ConversationNode]
buildConversationTree convs =
    let -- Build a map from conversation ID to conversation
        convMap = Map.fromList [(conversationId c, c) | c <- convs]

        -- Find root conversations (those without parentConversationId in their session)
        roots = filter (isNothing . getParentId . conversationSession) convs
    in map (buildNode convMap) roots
  where
    -- Recursively build a node and its children
    buildNode :: Map ConversationId Conversation -> Conversation -> ConversationNode
    buildNode convMap conv =
        let myId = conversationId conv
            -- Find all conversations whose parent is this conversation
            children = filter ((== Just myId) . getParentId . conversationSession) (Map.elems convMap)
        in ConversationNode
            { nodeConversation = conv
            , nodeChildren = map (buildNode convMap) children
            , nodeExpanded = False  -- Collapsed by default
            }

    -- Extract the parent conversation ID from a session
    getParentId :: Maybe Session -> Maybe ConversationId
    getParentId Nothing = Nothing
    getParentId (Just sess) = parentConversationId sess

-------------------------------------------------------------------------------
-- Sub-Agent Callback Factory
-------------------------------------------------------------------------------

-- | Create an 'OnSessionProgress' callback for a sub-agent that reports to the TUI event channel.
--
-- This factory function creates a callback that translates session progress events
-- into TUI application events, enabling the TUI to track and display sub-agent
-- session lifecycle in the conversation hierarchy.
--
-- The parent conversation ID is used to associate the sub-agent session with
-- its parent in the UI tree structure.
--
-- Example:
-- @
-- callback <- makeSubAgentCallback eventChan parentConvId
-- -- Use callback when creating sub-agent session
-- @
makeSubAgentCallback :: BChan AppEvent -> ConversationId -> OnSessionProgress
makeSubAgentCallback chan parentConvId progress =
    case progress of
        SessionStarted sess ->
            writeBChan chan (AppEvent_SubAgentSessionStarted parentConvId sess)
        SessionUpdated sess ->
            writeBChan chan (AppEvent_SubAgentSessionUpdated parentConvId sess)
        SessionCompleted sess ->
            writeBChan chan (AppEvent_SubAgentSessionCompleted parentConvId sess)
        SessionFailed _sess err ->
            writeBChan chan (AppEvent_ShowStatus StatusError $
                "Sub-agent session failed: " <> err)

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
    -- NEW FIELDS:
    , coreSubAgentCallbacks :: Map ConversationId OnSessionProgress
    -- ^ Callbacks for sub-agent session progress, keyed by parent conversation ID.
    -- These callbacks are used when a conversation spawns sub-agent sessions,
    -- allowing the TUI to track nested conversation hierarchies.
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
    -- NEW FIELDS:
    , _conversationTreeExpanded :: Set ConversationId
    -- ^ Set of conversation IDs whose children are expanded in the UI tree view.
    -- This tracks which parent conversations have their sub-agent children visible.
    , _selectedConversationPath :: [ConversationId]
    -- ^ Path from root to currently selected conversation, represented as a list
    -- of conversation IDs from the root to the currently selected node.
    -- An empty list means no conversation is selected.
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
        -- NEW FIELDS:
        , _conversationTreeExpanded = Set.empty
        -- ^ Initially, no conversation trees are expanded
        , _selectedConversationPath = []
        -- ^ Initially, no conversation path is selected
        }

-- | Create initial Core state.
initCore :: [TuiAgent] -> IO Core
initCore agents = do
    bufferVar <- newTVarIO Map.empty
    pure $ Core
        { coreAgents = agents
        , coreConversations = []
        , corePausedConversations = Set.empty
        , coreBufferedMessages = bufferVar
        -- NEW FIELDS:
        , coreSubAgentCallbacks = Map.empty
        -- ^ Initially, no sub-agent callbacks are registered
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

