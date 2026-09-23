{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Queued message management event handlers for the TUI.

Phase 3b-i (@todos/os-as-standalone-server.md@ D3, §5): there is no more
kernel-visible "queue" of discrete messages -- @Core@ never buffered
messages to begin with in the new model, and a conversation's draft
('System.Agents.TUI.Types.Conversation.Draft') is TUI-local. This module's
"queued messages" list is now purely a UI-local
'System.Agents.TUI.Types.State.uiBufferedMessages' map, kept only for the
existing widget until 3b-iii turns it into the real draft editor.
-}
module System.Agents.TUI.Event.Queue (
    -- * Queue Management
    handleClearQueuedMessages,
    handleDeleteSelectedMessage,
    handleQueueNavigation,
) where

import Brick
import Brick.BChan (writeBChan)
import Control.Lens (to, use, (%=), (.=))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Brick.Widgets.List as List
import System.Agents.TUI.Types (
    AppEvent (..),
    Conversation (..),
    ConversationStatus (..),
    N,
    StatusSeverity (..),
    TuiState,
    conversationId,
    conversationList,
    conversationStatus,
    eventChan,
    queuedMessagesFocus,
    tuiUI,
    uiBufferedMessages,
 )

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Get the currently focused conversation in EventM context
getCurrentConversation :: EventM N TuiState (Maybe Conversation)
getCurrentConversation = do
    mElem <- use (tuiUI . conversationList . to List.listSelectedElement)
    pure $ fmap snd mElem

-------------------------------------------------------------------------------
-- Queue Management
-------------------------------------------------------------------------------

-- | Clear all queued messages for the current conversation.
handleClearQueuedMessages :: EventM N TuiState ()
handleClearQueuedMessages = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv ->
            if conversationStatus conv /= ConversationStatus_Paused
                then showStatus StatusWarning "Can only clear queued messages when paused (Ctrl+E)"
                else do
                    let convId = conversationId conv
                    tuiUI . uiBufferedMessages %= Map.insert convId []
                    tuiUI . queuedMessagesFocus .= Nothing
                    showStatus StatusInfo "All queued messages cleared"

-- | Delete the currently selected queued message.
handleDeleteSelectedMessage :: EventM N TuiState ()
handleDeleteSelectedMessage = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv ->
            if conversationStatus conv /= ConversationStatus_Paused
                then showStatus StatusWarning "Can only delete messages when paused (Ctrl+E)"
                else do
                    let convId = conversationId conv
                    mSelectedIdx <- use (tuiUI . queuedMessagesFocus)
                    case mSelectedIdx of
                        Nothing -> showStatus StatusWarning "Select a message first (use Up/Down arrows)"
                        Just idx -> do
                            buffered <- use (tuiUI . uiBufferedMessages)
                            case Map.lookup convId buffered of
                                Nothing -> pure ()
                                Just msgs ->
                                    if idx < 0 || idx >= length msgs
                                        then pure ()
                                        else do
                                            let newMsgs = deleteAt idx msgs
                                            tuiUI . uiBufferedMessages %= Map.insert convId newMsgs
                                            let newIdx = if null newMsgs then Nothing else Just (min idx (length newMsgs - 1))
                                            tuiUI . queuedMessagesFocus .= newIdx
                                            showStatus StatusInfo "Message deleted"

-- | Navigate through queued messages.
handleQueueNavigation :: Int -> EventM N TuiState ()
handleQueueNavigation direction = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> pure ()
        Just conv -> do
            let convId = conversationId conv
            buffered <- use (tuiUI . uiBufferedMessages)
            case Map.lookup convId buffered of
                Nothing -> pure ()
                Just msgs -> do
                    let count = length msgs
                    current <- use (tuiUI . queuedMessagesFocus)
                    let newIdx = case current of
                            Nothing -> if direction > 0 then 0 else count - 1
                            Just idx -> max 0 $ min (count - 1) (idx + direction)
                    tuiUI . queuedMessagesFocus .= Just newIdx

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Delete an element at a specific index.
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs ++ drop (idx + 1) xs
