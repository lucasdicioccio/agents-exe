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
    convs <- liftIO (listConversations st0)
    -- todo: only show conversations if item is selected
    -- todo: propagate refreshed info if new things happen
    let handles = orderUnifiedConversations st0._entities._loadedAgents convs
    ui . unifiedList .= (list UnifiedList (Vector.fromList handles) 0)

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
    List.sortBy (flip orderByAgent) items
  where
    orderByAgent :: ChatHandle -> ChatHandle -> Ordering
    orderByAgent (ChatEntryPoint (_, _, la1)) (ChatEntryPoint (_, _, la2)) =
        la1.slug `compare` la2.slug
    orderByAgent (ConversationEntryPoint c1) (ConversationEntryPoint c2) =
        c1.conversingAgent.slug `compare` c2.conversingAgent.slug
    orderByAgent (ConversationEntryPoint c1) (ChatEntryPoint (_, _, la2)) =
        let cmp = c1.conversingAgent.slug `compare` la2.slug
         in if cmp == EQ then GT else cmp
    orderByAgent (ChatEntryPoint (_, _, la1)) (ConversationEntryPoint c2) =
        let cmp = la1.slug `compare` c2.conversingAgent.slug
         in if cmp == EQ then LT else cmp

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
                (Just UnifiedList) ->
                    zoom (ui . unifiedList) $ handleListEvent vtyEv
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
                item <- use (ui . unifiedList)
                case listSelectedElement item of
                    Nothing -> pure ()
                    (Just (_, (ChatEntryPoint (rt, _, oai)))) -> do
                        startingPrompt <- use (ui . promptEditor . to getEditContents . to textLinesToPrompt)
                        conv <- liftIO $ Party.converse rt startingPrompt
                        st0 <- get
                        liftIO $ addConversation st0 (OngoingConversation oai conv [] False)
                    (Just (_, (ConversationEntryPoint conv))) -> do
                        continuingPrompt <- use (ui . promptEditor . to getEditContents . to textLinesToPrompt)
                        ok <- liftIO . atomically $ conv.conversationState.prompt (Just continuingPrompt)
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
    pure ()
  where
    setRead :: OngoingConversation -> OngoingConversation
    setRead conv = conv{historyChanged = False}
