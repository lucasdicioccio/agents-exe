{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Agents.TUI where

import qualified Control.Concurrent.Async as Async
import Control.Monad (void)
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
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Lens (makeLenses)
import qualified Graphics.Vty as Vty

type BrickWidgetName = Text
type N = BrickWidgetName

type LoadedAgent = (Agent.Runtime, FileLoader.OpenAIAgent)

data OngoingConversation
    = OngoingConversation
    { conversingAgent :: FileLoader.OpenAIAgent
    , conversationState :: Party.ConversationState
    }

data Entities
    = Entities
    { _conversations :: IORef [OngoingConversation]
    }
makeLenses ''Entities

data UI
    = UI
    { _promptEditor :: Editor Text N
    }
makeLenses ''UI

data TuiState
    = TuiState
    { _entities :: Entities
    , _ui :: UI
    }
makeLenses ''TuiState

newCliState :: IO TuiState
newCliState =
    TuiState
        <$> entities
        <*> ui
  where
    entities =
        Entities
            <$> newIORef []
    ui =
        UI
            <$> pure (editorText "prompt-editor" (Just 2) "@")

addConversation :: TuiState -> OngoingConversation -> IO ()
addConversation st0 conv = do
    modifyIORef st0._entities._conversations (conv :)

listConversations :: TuiState -> IO [(OngoingConversation, [Agent.Trace])]
listConversations st0 = do
    convs <- readIORef st0._entities._conversations
    traces <- traverse (Party.traces . conversationState) convs
    pure $ zip convs traces

runMultiAgents :: [LoadedAgent] -> IO ()
runMultiAgents [] = print "no agents loaded"
runMultiAgents agents = do
    st0 <- newCliState
    let app =
            App
                { appDraw = tui_appDraw
                , appChooseCursor = tui_appChooseCursor
                , appHandleEvent = tui_appHandleEvent
                , appStartEvent = tui_appStartEvent
                , appAttrMap = tui_appAttrMap
                }

    void $ defaultMain app st0
  where
    tui_appChooseCursor :: TuiState -> [CursorLocation N] -> Maybe (CursorLocation N)
    tui_appChooseCursor _ _ = Nothing

    tui_appHandleEvent :: BrickEvent N e0 -> EventM N TuiState ()
    tui_appHandleEvent ev =
        case ev of
            VtyEvent (Vty.EvKey Vty.KEsc _) -> halt
            _ -> do
                zoom (ui . promptEditor) $ handleEditorEvent ev

    tui_appStartEvent :: EventM a TuiState ()
    tui_appStartEvent = pure ()

    tui_appAttrMap :: TuiState -> AttrMap
    tui_appAttrMap _ = attrMap Vty.defAttr []

    tui_appDraw :: TuiState -> [Widget N]
    tui_appDraw st = [render_ui st]

    render_ui :: TuiState -> Widget N
    render_ui st =
        hBox
            [ render_agentsList
            , render_messagesList
            ]
            <=> hBox
                [ render_promptEditor st
                ]

    render_promptEditor :: TuiState -> Widget N
    render_promptEditor st =
        renderEditor
            (txt . Text.unlines)
            False
            st._ui._promptEditor

    render_agentsList :: Widget N
    render_agentsList = do
        let lst = list "agents-list" (Vector.fromList agents) 1 :: List N LoadedAgent
        borderWithLabel
            (txt "agents")
            (renderList render_agentsList_Agent True lst)

    render_agentsList_Agent :: Bool -> LoadedAgent -> Widget N
    render_agentsList_Agent _ (_, agent) =
        txt agent.slug -- borderWithLabel (txt agent.slug) $ txt agent.announce
    render_messagesList :: Widget N
    render_messagesList =
        txt " todo"

{-

    go state = do
            "conversations" -> do
                convs <- listConversations state
                Text.putStr $
                    Text.unlines
                        [ Text.unlines
                            [ Text.pack $ show conv.conversingAgent.slug
                            , Text.pack $ show conv.conversationState.conversationId
                            , Text.pack $ show trs
                            ]
                        | (conv,trs) <- convs
                        ]
            txt
                | "@" `Text.isPrefixOf` txt -> do
                    let (atName, spaceCmd) = Text.break ((==) ' ') txt
                    let name = Text.drop 1 atName
                    let cmd = Text.drop 1 spaceCmd
                    let foundAgent = List.find (\(_, agent) -> agent.slug == name) agents
                    case foundAgent of
                        Nothing -> print ("no such agent", name)
                        Just (rt, agent) -> do
                            if cmd == ""
                                then print agent
                                else do
                                    Party.converse rt cmd >>= addConversation state . (OngoingConversation agent)
            txt -> putStrLn helpStr

-}

-------------------------------------------------------------------------------

mainMultiAgents2 :: Int -> [Props] -> [LoadedAgent] -> IO ()
mainMultiAgents2 idx (props : xs) agents = do
    withAgentRuntime props go
  where
    go (Initialized ai) = do
        case ai.agentDescription of
            (FileLoader.Unspecified _) -> do
                print ("cannot load an agent with unspecified description" :: Text)
            (FileLoader.OpenAIAgentDescription oai) -> do
                mainMultiAgents2 (succ idx) xs ((ai.agentRuntime, oai) : agents)
    go _ = do
        print ("failed to initialize" :: Text)
mainMultiAgents2 _ [] agents = do
    runMultiAgents agents

mainMultiAgents :: [Props] -> IO ()
mainMultiAgents xs = mainMultiAgents2 0 xs []
