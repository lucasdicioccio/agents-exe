{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Agents.TUI.State where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Vector as Vector

import qualified System.Agents.Agent as Agent
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Party as Party

import Brick.Focus (FocusRing, focusRing)
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Lens hiding (zoom) -- (makeLenses, to, use, (%=))

data BrickWidgetName
    = UnifiedList
    | PromptEditor
    | FocusedConversation
    deriving (Show, Eq, Ord)
type N = BrickWidgetName

data LoadedAgent
    = LoadedAgent
    { loadedAgentRuntime :: Agent.Runtime
    , loadedAgentTools :: [Agent.ToolRegistration]
    , loadedAgentInfo :: FileLoader.OpenAIAgent
    }

data OngoingConversation
    = OngoingConversation
    { conversingAgent :: FileLoader.OpenAIAgent
    , conversationState :: Party.ConversationState
    , conversationHistory :: [Agent.Trace]
    , headline :: Text
    }

data Entities
    = Entities
    { _loadedAgents :: [LoadedAgent]
    , _conversations :: IORef [OngoingConversation]
    }
makeLenses ''Entities

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
    , _ui :: UI
    }
makeLenses ''TuiState

newCliState :: [LoadedAgent] -> IO TuiState
newCliState agents =
    TuiState
        <$> entitiesV
        <*> uiV
  where
    entitiesV =
        Entities
            <$> pure agents
            <*> newIORef []
    uiV =
        UI
            <$> pure (focusRing [UnifiedList, PromptEditor])
            <*> pure False
            <*> pure (editorText PromptEditor Nothing "")
            <*> pure (list UnifiedList (Vector.fromList (orderUnifiedConversations agents [])) 0)

referenceConversation :: TuiState -> OngoingConversation -> IO ()
referenceConversation st0 conv = do
    modifyIORef st0._entities._conversations (conv :)

listConversations :: TuiState -> IO [OngoingConversation]
listConversations st0 = do
    convs <- readIORef st0._entities._conversations
    traces <- traverse (Party.traces . conversationState) convs
    pure $ zipWith update convs traces
  where
    update conv trs =
        conv
            { conversationHistory = trs
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
