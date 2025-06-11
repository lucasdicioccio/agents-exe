{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.TUI.Handler where

import Brick
import Brick.Focus (focusGetCurrent, focusNext, focusPrev, focusRing)
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Concurrent.STM (atomically)
import Control.Lens hiding (zoom) -- (makeLenses, to, use, (%=))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as Text
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty

import System.Agents.Base (ConversationId)
import qualified System.Agents.Conversation as Conversation
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.Runtime as Agent
import System.Agents.TUI.Event (AppEvent (..))
import System.Agents.TUI.State

setSelectedUnifiedConversation :: Maybe Int -> EventM N TuiState ()
setSelectedUnifiedConversation n =
    ui . unifiedList . listSelectedL .= n

setSelectedUnifiedConversationById :: ConversationId -> EventM N TuiState ()
setSelectedUnifiedConversationById cId = do
    lst <- use (ui . unifiedList)
    let idx = List.findIndex f (toList lst)
    setSelectedUnifiedConversation idx
  where
    f (ChatEntryPoint _) = False
    f (ConversationEntryPoint conv) = conv.conversationId == cId

replaceUnifiedConversationsList :: [LoadedAgent] -> [OngoingConversation] -> EventM N TuiState ()
replaceUnifiedConversationsList agents convs = do
    let handles = orderUnifiedConversations agents convs
    ui . unifiedList .= (list UnifiedList (Vector.fromList handles) 0)

refreshStuffFromIOs_Conversations :: EventM N TuiState ()
refreshStuffFromIOs_Conversations = do
    st0 <- get
    let agents = st0._entities._loadedAgents
    convs <- liftIO (listConversations st0)
    replaceUnifiedConversationsList agents convs
    setSelectedUnifiedConversation (fmap fst (listSelectedElement st0._ui._unifiedList))

showHandle :: ChatHandle -> String
showHandle (ChatEntryPoint la) =
    Text.unpack $ "(agent)" <> la.loadedAgentInfo.slug
showHandle (ConversationEntryPoint oc) =
    "(conv)" <> show oc.conversationId

unZoom :: EventM N TuiState ()
unZoom =
    (ui . zoomed) .= False

flipZoom :: EventM N TuiState ()
flipZoom =
    (ui . zoomed) %= not

cycleFocusFwd :: EventM N TuiState ()
cycleFocusFwd = do
    (ui . focus) %= focusNext
    unZoom

cycleFocusBwd :: EventM N TuiState ()
cycleFocusBwd = do
    (ui . focus) %= focusPrev
    unZoom

tui_appHandleEvent_AppEvent_AgentTrace :: Agent.Trace -> EventM N TuiState ()
tui_appHandleEvent_AppEvent_AgentTrace ev = do
    refreshStuffFromIOs_Conversations
    case ev of
        Agent.AgentTrace_Conversation _ _ cId (Agent.NewConversation) ->
            setSelectedUnifiedConversationById cId
        _ ->
            pure ()

tui_appHandleEvent_AppEvent_Heartbeat :: EventM N TuiState ()
tui_appHandleEvent_AppEvent_Heartbeat = do
    refreshStuffFromIOs_Conversations

tui_appHandleEvent :: BrickEvent N AppEvent -> EventM N TuiState ()
tui_appHandleEvent ev = do
    case ev of
        AppEvent (AppEvent_AgentTrace appEv) -> do
            tui_appHandleEvent_AppEvent_AgentTrace appEv
        AppEvent (AppEvent_Heartbeat) -> do
            tui_appHandleEvent_AppEvent_Heartbeat
        VtyEvent (Vty.EvKey Vty.KEsc _) -> halt
        VtyEvent (Vty.EvKey (Vty.KChar '\t') _) ->
            cycleFocusFwd
        VtyEvent (Vty.EvKey Vty.KBackTab _) ->
            cycleFocusBwd
        (VtyEvent vtyEv) -> do
            currentFocus <- use (ui . focus . to focusGetCurrent)
            case currentFocus of
                Nothing -> pure ()
                (Just UnifiedList) -> do
                    zoom (ui . unifiedList) $ handleListEvent vtyEv
                    st1 <- get
                    case listSelectedElement st1._ui._unifiedList of
                        Nothing ->
                            ui . focus
                                .= focusRing
                                    [ UnifiedList
                                    , PromptEditor
                                    ]
                        (Just (_, (ChatEntryPoint _))) ->
                            ui . focus
                                .= focusRing
                                    [ UnifiedList
                                    , PromptEditor
                                    ]
                        (Just (_, (ConversationEntryPoint _))) ->
                            ui . focus
                                .= focusRing
                                    [ UnifiedList
                                    , PromptEditor
                                    , FocusedConversation
                                    ]
                (Just PromptEditor) -> do
                    zoom (ui . promptEditor) $ handleEditorEvent ev
                    tui_handleViewPortEvent_PromptEditor ev
                (Just FocusedConversation) -> do
                    tui_handleViewPortEvent_Conversation ev
        _ -> pure ()

tui_handleViewPortEvent_PromptEditor :: BrickEvent N e0 -> EventM N TuiState ()
tui_handleViewPortEvent_PromptEditor ev = do
    case ev of
        VtyEvent (Vty.EvKey Vty.KEnter mods)
            | Vty.MMeta `elem` mods -> do
                item <- use (ui . unifiedList)
                case listSelectedElement item of
                    Nothing -> pure ()
                    (Just (_, (ChatEntryPoint la))) -> do
                        startingPrompt <- use (ui . promptEditor . to getEditContents . to textLinesToPrompt)
                        conv <- liftIO $ Conversation.converse la.loadedAgentRuntime startingPrompt
                        st0 <- get
                        liftIO $
                            referenceConversation
                                st0
                                (StartedConversation la.loadedAgentInfo conv startingPrompt)
                    (Just (_, (ConversationEntryPoint conv))) -> do
                        continuingPrompt <- use (ui . promptEditor . to getEditContents . to textLinesToPrompt)
                        _ <- liftIO . atomically $ conv.prompt (if continuingPrompt == "" then Nothing else Just continuingPrompt)
                        pure ()
                (ui . promptEditor . editContentsL) .= Text.textZipper [] Nothing
        _ -> pure ()

textLinesToPrompt :: [Text] -> Text
textLinesToPrompt = Text.stripEnd . Text.unlines

tui_handleViewPortEvent_Conversation :: BrickEvent N e0 -> EventM N TuiState ()
tui_handleViewPortEvent_Conversation ev = do
    case ev of
        VtyEvent (Vty.EvKey (Vty.KUp) _) -> do
            let vp = viewportScroll FocusedConversation
            vScrollBy vp (-1)
        VtyEvent (Vty.EvKey (Vty.KDown) _) -> do
            let vp = viewportScroll FocusedConversation
            vScrollBy vp 1
        VtyEvent (Vty.EvKey (Vty.KLeft) _) -> do
            let vp = viewportScroll FocusedConversation
            hScrollBy vp (-1)
        VtyEvent (Vty.EvKey (Vty.KRight) _) -> do
            let vp = viewportScroll FocusedConversation
            hScrollBy vp 1
        VtyEvent (Vty.EvKey (Vty.KChar 'z') _) -> do
            flipZoom
        _ -> pure ()

tui_appStartEvent :: EventM a TuiState ()
tui_appStartEvent = pure ()
