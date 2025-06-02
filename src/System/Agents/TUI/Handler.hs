{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.TUI.Handler where

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

import System.Agents.TUI.State

refreshStuffFromIOs :: EventM N TuiState ()
refreshStuffFromIOs = do
    refreshStuffFromIOs_Conversations

refreshStuffFromIOs_Conversations :: EventM N TuiState ()
refreshStuffFromIOs_Conversations = do
    st0 <- get
    items <- liftIO (listConversations st0)
    ui . conversationsList .= (list ConversationsList (Vector.fromList items) 0)

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

tui_appHandleEvent :: BrickEvent N e0 -> EventM N TuiState ()
tui_appHandleEvent ev = do
    case ev of
        AppEvent _ -> do
            refreshStuffFromIOs_Conversations
        VtyEvent (Vty.EvKey Vty.KEsc _) -> halt
        VtyEvent (Vty.EvKey (Vty.KChar '\t') _) ->
            cycleFocusFwd
        VtyEvent (Vty.EvKey Vty.KBackTab _) ->
            cycleFocusBwd
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
                    tui_handleViewPortEvent_PromptEditor ev
                    setCurrentConversationAsRead
                (Just FocusedConversation) -> do
                    tui_handleViewPortEvent_Conversation ev
                    setCurrentConversationAsRead
        _ -> pure ()

tui_handleViewPortEvent_PromptEditor :: BrickEvent N e0 -> EventM N TuiState ()
tui_handleViewPortEvent_PromptEditor ev = do
    case ev of
        VtyEvent (Vty.EvKey Vty.KEnter mods)
            | Vty.MMeta `elem` mods -> do
                item <- use (ui . agentsList)
                conv <- use (ui . conversationsList)
                case listSelectedElement item of
                    Nothing -> pure ()
                    (Just (_, (rt, _, oai))) -> do
                        case listSelectedElement conv of
                            Nothing -> do
                                startingPrompt <- use (ui . promptEditor . to getEditContents . to textLinesToPrompt)
                                conv <- liftIO $ Party.converse rt startingPrompt
                                st0 <- get
                                liftIO $ addConversation st0 (OngoingConversation oai conv [] False)
                            (Just (_, c)) -> do
                                continuingPrompt <- use (ui . promptEditor . to getEditContents . to textLinesToPrompt)
                                ok <- liftIO . atomically $ c.conversationState.prompt (Just continuingPrompt)
                                pure ()
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

setCurrentConversationAsRead :: EventM N TuiState ()
setCurrentConversationAsRead = do
    st <- get
    case listSelectedElement st._ui._conversationsList of
        Nothing ->
            pure ()
        Just (_, conv) ->
            ui . conversationsList . listSelectedElementL %= setRead
  where
    setRead :: OngoingConversation -> OngoingConversation
    setRead conv = conv{historyChanged = False}
