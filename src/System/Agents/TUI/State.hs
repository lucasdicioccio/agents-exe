{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Agents.TUI.State where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (atomically)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import GHC.IO.Handle (Handle)
import Prod.Tracer (Tracer (..), silent)
import System.IO (stderr, stdout)

import qualified System.Agents.Agent as Agent
import System.Agents.Base (newConversationId)
import System.Agents.Conversation
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Party as Party
import qualified System.Agents.Tools as Tools
import qualified System.Agents.Tools.Bash as Tools
import qualified System.Agents.Tools.IO as Tools

import Brick
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev, focusRing)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Lens hiding (zoom) -- (makeLenses, to, use, (%=))
import qualified Graphics.Vty as Vty

data BrickWidgetName
    = AgentsList
    | ConversationsList
    | UnifiedList
    | PromptEditor
    | FocusedConversation
    deriving (Show, Eq, Ord)
type N = BrickWidgetName

type LoadedAgent = (Agent.Runtime, [Agent.ToolRegistration], FileLoader.OpenAIAgent)

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

data ConversingEntryPoint
    = ChatEntryPoint LoadedAgent
    | ConversationEntryPoint OngoingConversation

data UI
    = UI
    { _focus :: FocusRing N
    , _zoomed :: Bool
    , _promptEditor :: Editor Text N
    , _agentsList :: List N LoadedAgent
    , _conversationsList :: List N OngoingConversation
    , _unifiedList :: List N ConversingEntryPoint
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
        <$> entities
        <*> ui
  where
    entities =
        Entities
            <$> pure agents
            <*> newIORef []
    ui =
        UI
            <$> pure (focusRing [UnifiedList, AgentsList, ConversationsList, PromptEditor, FocusedConversation])
            <*> pure False
            <*> pure (editorText PromptEditor Nothing "@")
            <*> pure (list AgentsList (Vector.fromList agents) 1)
            <*> pure (list ConversationsList (Vector.fromList []) 0)
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
