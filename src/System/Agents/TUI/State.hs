{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Agents.TUI.State where

import Brick.Focus (FocusRing, focusRing)
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Lens hiding (zoom) -- (makeLenses, to, use, (%=))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Vector as Vector

import System.Agents.Base (AgentId)
import qualified System.Agents.Conversation as Conversation
import System.Agents.Dialogues (LoadedAgent (..), OngoingConversation (..), StartedConversation (..))
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Runtime as Runtime
import System.Agents.ToolRegistration (ToolRegistration)

data BrickWidgetName
    = UnifiedList
    | PromptEditor
    | FocusedConversation
    | AgentInfo
    deriving (Show, Eq, Ord)
type N = BrickWidgetName

data Entities
    = Entities
    { _loadedAgents :: [LoadedAgent]
    , _conversations :: IORef [StartedConversation]
    }
makeLenses ''Entities

data Projections
    = Projections
    { _tools :: [(AgentId, [ToolRegistration])]
    }
makeLenses ''Projections

data ChatHandle
    = ChatEntryPoint LoadedAgent
    | ConversationEntryPoint OngoingConversation

data UI
    = UI
    { _focus :: FocusRing N
    , _zoomed :: Bool
    , _promptEditor :: Editor Text N
    , _unifiedList :: List N ChatHandle
    }
makeLenses ''UI

data TuiState
    = TuiState
    { _entities :: Entities
    , _projections :: Projections
    , _ui :: UI
    }
makeLenses ''TuiState

newTuiState :: [LoadedAgent] -> IO TuiState
newTuiState agents =
    TuiState
        <$> entitiesV
        <*> projectionsV
        <*> uiV
  where
    entitiesV =
        Entities
            <$> pure agents
            <*> newIORef []
    projectionsV =
        Projections
            <$> traverse readTools agents
    uiV =
        UI
            <$> pure (focusRing [UnifiedList, PromptEditor, AgentInfo])
            <*> pure False
            <*> pure (editorText PromptEditor Nothing "")
            <*> pure (list UnifiedList (Vector.fromList (orderUnifiedConversations agents [])) 0)

referenceConversation :: TuiState -> StartedConversation -> IO ()
referenceConversation st0 conv = do
    modifyIORef st0._entities._conversations (conv :)

listConversations :: TuiState -> IO [OngoingConversation]
listConversations st0 = do
    convs <- readIORef st0._entities._conversations
    traces <- traverse (Conversation.traces . conversationState) convs
    statuses <- traverse (Conversation.status . conversationState) convs
    pure $ zipWith3 update convs traces statuses
  where
    update :: StartedConversation -> [Runtime.Trace] -> Conversation.ConversationStatus -> OngoingConversation
    update conv trs status =
        OngoingConversation
            { conversationId = conv.conversationState.conversationId
            , conversingAgent = conv.conversingAgent
            , conversationStatus = status
            , conversationHistory = trs
            , headline = conv.headline
            , prompt = conv.conversationState.prompt
            }

orderUnifiedConversations ::
    [LoadedAgent] ->
    [OngoingConversation] ->
    [ChatHandle]
orderUnifiedConversations las ocs =
    orderChatHandles allItems
  where
    agentItems = fmap ChatEntryPoint las
    conversationsItems = fmap ConversationEntryPoint ocs
    allItems = agentItems <> conversationsItems

orderChatHandles :: [ChatHandle] -> [ChatHandle]
orderChatHandles items =
    List.sortBy orderByAgent items
  where
    orderByAgent :: ChatHandle -> ChatHandle -> Ordering
    orderByAgent (ChatEntryPoint la1) (ChatEntryPoint la2) =
        la1.loadedAgentInfo.slug `compare` la2.loadedAgentInfo.slug
    orderByAgent (ConversationEntryPoint c1) (ConversationEntryPoint c2) =
        c1.conversingAgent.slug `compare` c2.conversingAgent.slug
    orderByAgent (ConversationEntryPoint c1) (ChatEntryPoint la2) =
        let cmp = c1.conversingAgent.slug `compare` la2.loadedAgentInfo.slug
         in if cmp == EQ then GT else cmp
    orderByAgent (ChatEntryPoint la1) (ConversationEntryPoint c2) =
        let cmp = la1.loadedAgentInfo.slug `compare` c2.conversingAgent.slug
         in if cmp == EQ then LT else cmp

readTools :: LoadedAgent -> IO (AgentId, [ToolRegistration])
readTools agent = do
    x <- agent.loadedAgentRuntime.agentTools
    pure (agent.loadedAgentRuntime.agentId, x)

