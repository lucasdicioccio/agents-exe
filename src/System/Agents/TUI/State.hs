{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Agents.TUI.State where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
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

type LoadedAgent =
    (Agent.Runtime, [Agent.ToolRegistration], FileLoader.OpenAIAgent)

data OngoingConversation
    = OngoingConversation
    { conversingAgent :: FileLoader.OpenAIAgent
    , conversationState :: Party.ConversationState
    , conversationHistory :: [Agent.Trace]
    , historyChanged :: Bool
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
            <$> pure (focusRing [UnifiedList, PromptEditor, FocusedConversation])
            <*> pure False
            <*> pure (editorText PromptEditor Nothing "@")
            <*> pure (list UnifiedList (Vector.fromList $ fmap ChatEntryPoint agents) 0)

addConversation :: TuiState -> OngoingConversation -> IO ()
addConversation st0 conv = do
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
            , historyChanged = conv.historyChanged || length conv.conversationHistory /= length trs
            }
