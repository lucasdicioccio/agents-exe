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
import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
import Data.Aeson (Value)
import Data.List (find)
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
import System.Agents.Runtime.Trace (Trace)
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
    | AppEvent_AgentTrace Trace
    | AppEvent_ShowStatus StatusSeverity Text
    | AppEvent_ClearStatus
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
-- Multi-Agent Coordination Types
-------------------------------------------------------------------------------

-- | Role definition for agents in multi-agent conversations.
data AgentRole
    = -- | Coordinates other agents
      AgentRole_Orchestrator
    | -- | Specialized agent with a specific role description
      AgentRole_Specialist Text
    | -- | General worker agent
      AgentRole_Worker
    | -- | Observes but doesn't initiate
      AgentRole_Observer
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
    = -- | Each agent takes turns
      CoordinationStrategy_RoundRobin
    | -- | One agent orchestrates others
      CoordinationStrategy_Hierarchical AgentId
    | -- | Agents collaborate freely
      CoordinationStrategy_Collaborative
    deriving (Show, Eq)

-- | Configuration for multi-agent conversations.
data MultiAgentConfig = MultiAgentConfig
    { maAgents :: [AgentRoleConfig]
    , maCoordinationStrategy :: CoordinationStrategy
    }
    deriving (Show)

-- | Message type for inter-agent communication.
data MessageType
    = -- | Direct message to specific agent
      MessageType_Direct
    | -- | Message to all subscribed agents
      MessageType_Broadcast
    | -- | Response to a previous message
      MessageType_Response
    | -- | Request for action from another agent
      MessageType_Request
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
-- Conversation Status and Types
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

-- | Manual Show instance for Conversation (BChan and ThreadId don't have Show)
instance Show Conversation where
    show conv =
        "Conversation { conversationId = "
            ++ show conv.conversationId
            ++ ", conversationAgent = "
            ++ show conv.conversationAgent
            ++ ", conversationThreadId = "
            ++ show (fmap (const ("<ThreadId>" :: String)) conv.conversationThreadId)
            ++ ", conversationSession = "
            ++ show conv.conversationSession
            ++ ", conversationName = "
            ++ show conv.conversationName
            ++ ", conversationChan = <BChan>, conversationStatus = "
            ++ show conv.conversationStatus
            ++ ", conversationOnProgress = <OnSessionProgress> }"

-------------------------------------------------------------------------------
-- Conversation Tree Types
-------------------------------------------------------------------------------

-- | Enhanced conversation node with hierarchy information for tree view.
data ConversationNode = ConversationNode
    { cnConversation :: Conversation
    -- ^ The conversation itself
    , cnParentId :: Maybe ConversationId
    -- ^ Parent conversation ID (Nothing for root)
    , cnChildIds :: [ConversationId]
    -- ^ Child conversation IDs
    , cnDepth :: Int
    -- ^ Depth in the conversation tree (0 for roots)
    , cnIsExpanded :: Bool
    -- ^ Whether this conversation's children are visible
    }

-- | Manual Show instance for ConversationNode
instance Show ConversationNode where
    show node =
        "ConversationNode { cnConversation = "
            ++ show node.cnConversation
            ++ ", cnParentId = "
            ++ show node.cnParentId
            ++ ", cnChildIds = "
            ++ show node.cnChildIds
            ++ ", cnDepth = "
            ++ show node.cnDepth
            ++ ", cnIsExpanded = "
            ++ show node.cnIsExpanded
            ++ " }"

-- | State for tracking conversation tree visualization.
data ConversationTreeState = ConversationTreeState
    { _expandedConversations :: Set ConversationId
    -- ^ Set of conversation IDs that have been expanded
    , _conversationDepth :: Map ConversationId Int
    -- ^ Cache of conversation depths for quick lookup
    , _childConversationCache :: Map ConversationId [ConversationId]
    -- ^ Cache of child conversation IDs per parent
    }
    deriving (Show)

makeLenses ''ConversationTreeState

-- | Initial empty conversation tree state.
initConversationTreeState :: ConversationTreeState
initConversationTreeState =
    ConversationTreeState
        { _expandedConversations = Set.empty
        , _conversationDepth = Map.empty
        , _childConversationCache = Map.empty
        }

-- | Information about a conversation's position in the hierarchy.
data ConversationHierarchyInfo = ConversationHierarchyInfo
    { chiParentId :: Maybe ConversationId
    , chiChildCount :: Int
    , chiDepth :: Int
    , chiIsExpanded :: Bool
    }
    deriving (Show)

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
    , sessionApiKeys :: LoadedApiKeys
    -- ^ API keys for creating HTTP runtime for LLM calls
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
    , coreConversationTreeState :: ConversationTreeState
    -- ^ State for tracking conversation hierarchy
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
    pure $
        Core
            { coreAgents = agents
            , coreConversations = []
            , corePausedConversations = Set.empty
            , coreBufferedMessages = bufferVar
            , coreConversationTreeState = initConversationTreeState
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

{- | Build a flat list of conversations for display, respecting expanded state.

This function takes the list of all conversations and returns a list
suitable for display in the UI, with children hidden if their parent
is collapsed.
-}
buildDisplayConversationList :: [Conversation] -> ConversationTreeState -> [Conversation]
buildDisplayConversationList allConvs treeState =
    -- Start with root conversations (those at depth 0 or unknown depth)
    let rootConvs = filter (isRoot treeState) allConvs
     in concatMap (buildSubtree allConvs treeState) rootConvs
  where
    isRoot :: ConversationTreeState -> Conversation -> Bool
    isRoot ts conv =
        case Map.lookup (conversationId conv) (ts ^. conversationDepth) of
            Nothing -> True
            Just depth -> depth == 0

    buildSubtree :: [Conversation] -> ConversationTreeState -> Conversation -> [Conversation]
    buildSubtree convs ts parent =
        let children = findChildren convs ts (conversationId parent)
            isExpanded = Set.member (conversationId parent) (ts ^. expandedConversations)
         in [parent]
                ++ if isExpanded
                    then concatMap (buildSubtree convs ts) children
                    else []

    findChildren :: [Conversation] -> ConversationTreeState -> ConversationId -> [Conversation]
    findChildren convs ts parentId =
        case Map.lookup parentId (ts ^. childConversationCache) of
            Just childIds ->
                [c | c <- convs, conversationId c `elem` childIds]
            Nothing ->
                -- Fallback: check session forkedFromSessionId
                [c | c <- convs, isChildOf parentId c]

    isChildOf :: ConversationId -> Conversation -> Bool
    isChildOf parentId conv =
        case conversationSession conv >>= (.forkedFromSessionId) of
            Just parentSessionId ->
                -- Find parent conversation by session ID
                case find ((== Just parentSessionId) . fmap (.sessionId) . conversationSession) allConvs of
                    Just parentConv -> conversationId parentConv == parentId
                    Nothing -> False
            Nothing -> False

-- | Check if a conversation has children (sub-agent calls).
hasChildConversations :: Conversation -> [Conversation] -> Bool
hasChildConversations conv allConvs =
    any (isChildOf (conversationId conv)) allConvs
  where
    isChildOf :: ConversationId -> Conversation -> Bool
    isChildOf parentId childConv =
        case conversationSession childConv >>= (.forkedFromSessionId) of
            Just parentSessionId ->
                case find ((== Just parentSessionId) . fmap (.sessionId) . conversationSession) allConvs of
                    Just parentConv -> conversationId parentConv == parentId
                    Nothing -> False
            Nothing -> False

-- | Get child count for a conversation.
getChildConversationCount :: Conversation -> [Conversation] -> Int
getChildConversationCount conv allConvs =
    length [c | c <- allConvs, isChildOf (conversationId conv) c]
  where
    isChildOf :: ConversationId -> Conversation -> Bool
    isChildOf parentId childConv =
        case conversationSession childConv >>= (.forkedFromSessionId) of
            Just parentSessionId ->
                case find ((== Just parentSessionId) . fmap (.sessionId) . conversationSession) allConvs of
                    Just parentConv -> conversationId parentConv == parentId
                    Nothing -> False
            Nothing -> False

-- | Get depth of a conversation in the tree (0 for root).
getConversationDepth :: Conversation -> [Conversation] -> Int
getConversationDepth conv allConvs = go 0 (conversationSession conv >>= (.forkedFromSessionId))
  where
    go :: Int -> Maybe SessionId -> Int
    go depth Nothing = depth
    go depth (Just parentSessionId) =
        case find ((== Just parentSessionId) . fmap (.sessionId) . conversationSession) allConvs of
            Just parentConv -> go (depth + 1) (conversationSession parentConv >>= (.forkedFromSessionId))
            Nothing -> depth

-- | Toggle expanded state for a conversation.
toggleExpanded :: ConversationId -> ConversationTreeState -> ConversationTreeState
toggleExpanded convId treeState =
    if Set.member convId (treeState ^. expandedConversations)
        then treeState & expandedConversations %~ Set.delete convId
        else treeState & expandedConversations %~ Set.insert convId

-- | Expand all conversations.
expandAll :: [Conversation] -> ConversationTreeState -> ConversationTreeState
expandAll convs treeState =
    treeState & expandedConversations .~ Set.fromList (map conversationId convs)

-- | Collapse all conversations (except roots).
collapseAll :: [Conversation] -> ConversationTreeState -> ConversationTreeState
collapseAll _convs treeState =
    treeState & expandedConversations .~ Set.empty

-- | Update conversation tree cache based on current conversations.
updateConversationTreeCache :: [Conversation] -> ConversationTreeState -> ConversationTreeState
updateConversationTreeCache convs treeState =
    let
        -- Build depth map
        depthMap = Map.fromList [(conversationId c, getConversationDepth c convs) | c <- convs]
        -- Build child cache
        childMap = Map.fromList [(conversationId c, findChildren c convs) | c <- convs]
     in
        treeState
            & conversationDepth .~ depthMap
            & childConversationCache .~ childMap
  where
    findChildren :: Conversation -> [Conversation] -> [ConversationId]
    findChildren parent allCs =
        map conversationId $
            filter (isChildOf (conversationId parent)) allCs

    isChildOf :: ConversationId -> Conversation -> Bool
    isChildOf parentId childConv =
        case conversationSession childConv >>= (.forkedFromSessionId) of
            Just parentSessionId ->
                case find ((== Just parentSessionId) . fmap (.sessionId) . conversationSession) convs of
                    Just parentConv -> conversationId parentConv == parentId
                    Nothing -> False
            Nothing -> False
