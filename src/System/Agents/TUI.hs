{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Agents.TUI where

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
    | PromptEditor
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
    { _conversations :: IORef [OngoingConversation]
    }
makeLenses ''Entities

data UI
    = UI
    { _focus :: FocusRing N
    , _promptEditor :: Editor Text N
    , _agentsList :: List N LoadedAgent
    , _conversationsList :: List N OngoingConversation
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
            <$> newIORef []
    ui =
        UI
            <$> pure (focusRing [AgentsList, PromptEditor, ConversationsList])
            <*> pure (editorText PromptEditor Nothing "@")
            <*> pure (list AgentsList (Vector.fromList agents) 1)
            <*> pure (list ConversationsList (Vector.fromList []) 0)

addConversation :: TuiState -> OngoingConversation -> IO ()
addConversation st0 conv = do
    modifyIORef st0._entities._conversations (conv :)

listConversations :: TuiState -> IO [OngoingConversation]
listConversations st0 = do
    convs <- readIORef st0._entities._conversations
    traces <- traverse (Party.traces . conversationState) convs
    pure $ zipWith (\conv trs -> conv{conversationHistory = trs, historyChanged = length conv.conversationHistory /= length trs}) convs traces

runMultiAgents :: [LoadedAgent] -> IO ()
runMultiAgents [] = print "no agents loaded"
runMultiAgents agents = do
    st0 <- newCliState agents
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
    tui_appChooseCursor st locs =
        case focusGetCurrent st._ui._focus of
            Just PromptEditor -> showCursorNamed PromptEditor locs
            Just AgentsList -> Nothing
            Just ConversationsList -> Nothing
            Nothing -> Nothing

    refreshStuffFromIOs :: EventM N TuiState ()
    refreshStuffFromIOs = do
        refreshStuffFromIOs_Conversations

    refreshStuffFromIOs_Conversations :: EventM N TuiState ()
    refreshStuffFromIOs_Conversations = do
        st0 <- get
        items <- liftIO (listConversations st0)
        ui . conversationsList .= (list ConversationsList (Vector.fromList items) 0)

    tui_appHandleEvent :: BrickEvent N e0 -> EventM N TuiState ()
    tui_appHandleEvent ev = do
        case ev of
            VtyEvent (Vty.EvKey Vty.KEsc _) -> halt
            VtyEvent (Vty.EvKey (Vty.KChar '\t') _) ->
                (ui . focus) %= focusNext
            VtyEvent (Vty.EvKey Vty.KBackTab _) ->
                (ui . focus) %= focusPrev
            VtyEvent (Vty.EvKey Vty.KEnter mods)
                | Vty.MMeta `elem` mods -> do
                    item <- use (ui . agentsList)
                    conv <- use (ui . conversationsList)
                    case listSelectedElement item of
                        Nothing -> pure ()
                        (Just (_, (rt, _, oai))) -> do
                            case listSelectedElement conv of
                                Nothing -> do
                                    startingPrompt <- use (ui . promptEditor . to getEditContents . to Text.unlines)
                                    conv <- liftIO $ Party.converse rt startingPrompt
                                    st0 <- get
                                    liftIO $ addConversation st0 (OngoingConversation oai conv [] False)
                                (Just (_, c)) -> do
                                    continuingPrompt <- use (ui . promptEditor . to getEditContents . to Text.unlines)
                                    ok <- liftIO . atomically $ c.conversationState.prompt (Just continuingPrompt)
                                    pure ()
                            refreshStuffFromIOs_Conversations
            ev@(VtyEvent vtyEv) -> do
                currentFocus <- use (ui . focus . to focusGetCurrent)
                case currentFocus of
                    Nothing -> pure ()
                    (Just AgentsList) ->
                        zoom (ui . agentsList) $ handleListEvent vtyEv
                    (Just ConversationsList) ->
                        zoom (ui . conversationsList) $ handleListEvent vtyEv
                    (Just PromptEditor) -> do
                        zoom (ui . promptEditor) $ handleEditorEvent ev
            _ -> pure ()

    tui_appStartEvent :: EventM a TuiState ()
    tui_appStartEvent = pure ()

    tui_appAttrMap :: TuiState -> AttrMap
    tui_appAttrMap _ = attrMap Vty.defAttr []

    tui_appDraw :: TuiState -> [Widget N]
    tui_appDraw st = [render_ui st]

    render_ui :: TuiState -> Widget N
    render_ui st =
        hBox
            [ borderWithLabel
                (txt "agents")
                (hLimit 18 $ render_agentsList st)
            , borderWithLabel
                (txt "conversations")
                (hLimit 50 $ render_conversationsList st)
                {-
                            , borderWithLabel
                                (txt "info")
                                (hLimit 60 $ render_focusedAgentInfo st)
                            , borderWithLabel
                                (txt "tools")
                                (hLimit 60 $ render_focusedAgentTools st)
                -}
            ]
            <+> vBox
                [ borderWithLabel
                    (txt "chat")
                    (render_promptEditor st)
                , borderWithLabel
                    (txt "conv")
                    (hLimit 120 $ render_focusedConversation st)
                ]

    render_promptEditor :: TuiState -> Widget N
    render_promptEditor st =
        renderEditor
            (txt . Text.unlines)
            (focusGetCurrent st._ui._focus == Just PromptEditor)
            st._ui._promptEditor

    render_agentsList :: TuiState -> Widget N
    render_agentsList st =
        let lst = st._ui._agentsList
         in renderList render_agentsList_Agent True lst

    render_agentsList_Agent :: Bool -> LoadedAgent -> Widget N
    render_agentsList_Agent True (_, _, agent) =
        txt ("> " <> agent.slug)
    render_agentsList_Agent False (_, _, agent) =
        txt ("  " <> agent.slug)

    render_conversationsList :: TuiState -> Widget N
    render_conversationsList st =
        let lst = st._ui._conversationsList
         in renderList render_conversationsList_Conversation True lst

    render_conversationsList_Conversation :: Bool -> OngoingConversation -> Widget N
    render_conversationsList_Conversation active conv =
        txt (flags <> conv.conversingAgent.slug)
      where
        flags = activeFlag <> modifiedFlag
        activeFlag = if active then ">" else " "
        modifiedFlag = if conv.historyChanged then "*" else " "

    render_focusedAgentInfo :: TuiState -> Widget N
    render_focusedAgentInfo st =
        case listSelectedElement st._ui._agentsList of
            Nothing ->
                txt "select an agent to show info"
            Just (_, (rt, _, oai)) ->
                txt oai.slug
                    <=> str (show rt.agentId)
                    <=> txt oai.announce
                    <=> txt (Text.unlines oai.systemPrompt)
                    <=> str oai.toolDirectory

    render_focusedAgentTools :: TuiState -> Widget N
    render_focusedAgentTools st =
        case listSelectedElement st._ui._agentsList of
            Nothing ->
                txt "select an agent to show tools"
            Just (_, (_, tools, _)) ->
                txt $ renderToolRegistry tools

    render_focusedConversation :: TuiState -> Widget N
    render_focusedConversation st =
        case listSelectedElement st._ui._conversationsList of
            Nothing ->
                txt "no history"
            Just (_, conv) ->
                str (show (length conv.conversationHistory))
                    <=> str (show conv.conversationHistory)

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
                tools <- Agent.agentTools ai.agentRuntime
                mainMultiAgents2 (succ idx) xs ((ai.agentRuntime, tools, oai) : agents)
    go _ = do
        print ("failed to initialize" :: Text)
mainMultiAgents2 _ [] agents = do
    runMultiAgents agents

mainMultiAgents :: [Props] -> IO ()
mainMultiAgents xs = mainMultiAgents2 0 xs []

renderToolRegistry :: (Aeson.ToJSON b) => [Tools.Registration a b c] -> Text
renderToolRegistry registry =
    Text.unlines $
        fmap renderRegisteredTool registry
  where
    renderRegisteredTool :: (Aeson.ToJSON b) => Tools.Registration a b c -> Text
    renderRegisteredTool reg =
        case reg.innerTool.toolDef of
            Tools.BashTool bashScript ->
                Text.unwords ["command", Text.pack bashScript.scriptPath, Text.decodeUtf8 $ LByteString.toStrict $ Aeson.encode reg.declareTool]
            Tools.IOTool ioScript ->
                Text.unwords ["io", ioScript.ioSlug, ioScript.ioDescription]
