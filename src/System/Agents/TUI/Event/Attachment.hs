{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Attachment list event handlers for the TUI.

This module handles attachment list events and clipboard paste functionality.
-}
module System.Agents.TUI.Event.Attachment (
    -- * Attachment List Events
    handleAttachmentListEvent,
    handleRemoveSelectedAttachment,
    handleClearAllAttachments,

    -- * Clipboard Paste
    handleClipboardPaste,
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.Edit (editContentsL)
import Control.Lens (to, use, (%=), (.=))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as TextZipper
import qualified Graphics.Vty as Vty

import Prod.Tracer (Tracer)

import qualified Brick.Widgets.List as List
import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.TUI.Clipboard (
    ContentAction (..),
    analyzeContent,
    detectClipboardContent,
    hasClipboardSupport,
 )
import System.Agents.TUI.KeyMapping (
    EventName (..),
    KeyMapping,
    matchesEvent,
 )
import System.Agents.TUI.Types (
    AppEvent (..),
    Conversation (..),
    N,
    StatusSeverity (..),
    TuiState,
    attachedFiles,
    conversationId,
    conversationList,
    eventChan,
    messageEditor,
    selectedAttachmentIndex,
    tuiUI,
 )

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Get the currently focused conversation from the TUI state.
getCurrentConversation :: EventM N TuiState (Maybe Conversation)
getCurrentConversation = do
    mElem <- use (tuiUI . conversationList . to List.listSelectedElement)
    pure $ fmap snd mElem

-------------------------------------------------------------------------------
-- Attachment List Events
-------------------------------------------------------------------------------

-- | Handle attachment list events.
handleAttachmentListEvent :: Vty.Event -> KeyMapping -> EventM N TuiState ()
handleAttachmentListEvent ev keymap = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> pure ()
        Just conv -> do
            let convId = conversationId conv
            attachments <- use (tuiUI . attachedFiles)
            case Map.lookup convId attachments of
                Nothing -> pure ()
                Just atts -> do
                    let count = length atts
                    case ev of
                        Vty.EvKey key mods
                            | matchesEvent keymap EventNavigateUp (Vty.EvKey key mods) -> do
                                current <- use (tuiUI . selectedAttachmentIndex)
                                let newIdx = case current of
                                        Nothing -> count - 1
                                        Just idx -> max 0 (idx - 1)
                                tuiUI . selectedAttachmentIndex .= Just newIdx
                        Vty.EvKey key mods
                            | matchesEvent keymap EventNavigateDown (Vty.EvKey key mods) -> do
                                current <- use (tuiUI . selectedAttachmentIndex)
                                let newIdx = case current of
                                        Nothing -> 0
                                        Just idx -> min (count - 1) (idx + 1)
                                tuiUI . selectedAttachmentIndex .= Just newIdx
                        Vty.EvKey key mods
                            | matchesEvent keymap EventDeleteItem (Vty.EvKey key mods) ->
                                handleRemoveSelectedAttachment
                        _ -> pure ()

-- | Remove the currently selected attachment.
handleRemoveSelectedAttachment :: EventM N TuiState ()
handleRemoveSelectedAttachment = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            let convId = conversationId conv
            mSelectedIdx <- use (tuiUI . selectedAttachmentIndex)
            case mSelectedIdx of
                Nothing -> showStatus StatusWarning "Select an attachment first (use Up/Down arrows)"
                Just idx -> do
                    attachments <- use (tuiUI . attachedFiles)
                    case Map.lookup convId attachments of
                        Nothing -> pure ()
                        Just atts ->
                            if idx < 0 || idx >= length atts
                                then pure ()
                                else do
                                    let newAtts = deleteAt idx atts
                                    tuiUI . attachedFiles %= Map.insert convId newAtts
                                    let newIdx = if null newAtts then Nothing else Just (min idx (length newAtts - 1))
                                    tuiUI . selectedAttachmentIndex .= newIdx
                                    showStatus StatusInfo "Attachment removed"

-- | Clear all attachments for the current conversation.
handleClearAllAttachments :: EventM N TuiState ()
handleClearAllAttachments = do
    mConv <- getCurrentConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            let convId = conversationId conv
            attachments <- use (tuiUI . attachedFiles)
            case Map.lookup convId attachments of
                Nothing -> showStatus StatusInfo "No attachments to clear"
                Just atts -> do
                    tuiUI . attachedFiles %= Map.delete convId
                    tuiUI . selectedAttachmentIndex .= Nothing
                    showStatus StatusInfo $ "Cleared " <> Text.pack (show $ length atts) <> " attachment(s)"

-------------------------------------------------------------------------------
-- Clipboard Paste
-------------------------------------------------------------------------------

-- | Handle Ctrl+Shift+V for clipboard paste.
handleClipboardPaste :: Tracer IO a -> EventM N TuiState ()
handleClipboardPaste _tracer = do
    hasSupport <- liftIO hasClipboardSupport
    if not hasSupport
        then showStatus StatusError "Clipboard not available - install xclip, wl-clipboard, or pbpaste"
        else do
            mContent <- liftIO detectClipboardContent
            case mContent of
                Nothing -> showStatus StatusWarning "Clipboard is empty or inaccessible"
                Just content -> do
                    action <- liftIO $ analyzeContent content
                    case action of
                        IgnoreContent -> showStatus StatusWarning "No attachable content in clipboard"
                        PasteAsText text -> do
                            editorContents <- use (tuiUI . messageEditor . editContentsL)
                            let newContents = TextZipper.insertMany text editorContents
                            tuiUI . messageEditor . editContentsL .= newContents
                            showStatus StatusInfo "Text pasted from clipboard"
                        AttachAsMedia attachment -> do
                            mConv <- getCurrentConversation
                            case mConv of
                                Nothing -> showStatus StatusError "No conversation selected"
                                Just conv -> do
                                    let convId = conversationId conv
                                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId [attachment]
                                    let filename = fromMaybe "unnamed" attachment.mediaFilename
                                    showStatus StatusInfo $ "Attached from clipboard: " <> filename
                        AttachMultipleFiles attachments -> do
                            mConv <- getCurrentConversation
                            case mConv of
                                Nothing -> showStatus StatusError "No conversation selected"
                                Just conv -> do
                                    let convId = conversationId conv
                                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId attachments
                                    showStatus StatusInfo $ "Attached " <> Text.pack (show $ length attachments) <> " files from clipboard"

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Delete an element at a specific index.
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs ++ drop (idx + 1) xs
