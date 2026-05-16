{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Queued message management event handlers for the TUI.

This module handles queued message management when the conversation is paused,
including clearing queued messages, deleting selected messages, and navigation.
-}
module System.Agents.TUI.Event.Queue (
    -- * Queue Management
    handleClearQueuedMessages,
    handleDeleteSelectedMessage,
    handleQueueNavigation,
) where

import Brick
import Brick.BChan (writeBChan)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Lens (to, use, (%=), (.=), (^.))
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
    coreBufferedMessages,
    eventChan,
    queuedMessagesFocus,
    tuiCore,
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
        Just conv -> do
            if conversationStatus conv /= ConversationStatus_Paused
                then showStatus StatusWarning "Can only clear queued messages when paused (Ctrl+E)"
                else do
                    let convId = conversationId conv
                    coreRef <- use tuiCore
                    core <- liftIO $ readTVarIO coreRef
                    liftIO $ atomically $ modifyTVar (core ^. coreBufferedMessages) $ Map.insert convId []
                    tuiUI . uiBufferedMessages %= Map.insert convId []
                    tuiUI . queuedMessagesFocus .= Nothing
                    showStatus StatusInfo "All queued messages cleared"

-- | Delete the currently selected queued message.
handleDeleteSelectedMessage :: EventM N TuiState ()
handleDeleteSelectedMessage = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
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
                                            coreRef <- use tuiCore
                                            core <- liftIO $ readTVarIO coreRef
                                            liftIO $ atomically $ modifyTVar (core ^. coreBufferedMessages) $ Map.insert convId newMsgs
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
