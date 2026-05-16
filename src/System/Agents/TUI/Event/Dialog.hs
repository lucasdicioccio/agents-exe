{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | File dialog event handlers for the TUI.

This module handles file path dialog and file browser dialog events,
including loading files as media attachments and MIME type detection.
-}
module System.Agents.TUI.Event.Dialog (
    -- * File Path Dialog
    handleFilePathDialogEvent,
    openFilePathDialog,
    closeFilePathDialog,
    handleConfirmFileAttachment,

    -- * File Browser Dialog
    handleFileBrowserDialogEvent,
    openFileBrowserDialog,
    closeFileBrowserDialog,
    handleFileBrowserSelection,

    -- * File Loading
    loadMediaAttachment,
    loadFileAsAttachment,
    detectMimeType,
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.Edit (editContentsL, getEditContents, handleEditorEvent)
import Brick.Widgets.FileBrowser (
    fileBrowserCursor,
    fileInfoFilePath,
    handleFileBrowserEvent,
    newFileBrowser,
    selectNonDirectories,
 )
import Control.Lens (to, use, (%=), (.=), _Just)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Zipper as TextZipper
import qualified Graphics.Vty as Vty
import System.FilePath (takeExtension, takeFileName)

import qualified Brick.Widgets.List as List
import System.Agents.CLI.PromptScript (parseMediaReference, resolveMediaType)
import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.TUI.Types (
    AppEvent (..),
    AttachmentDialogState (..),
    Conversation (..),
    N,
    StatusSeverity (..),
    TuiState,
    WidgetName (..),
    attachedFiles,
    attachmentDialogState,
    conversationId,
    conversationList,
    eventChan,
    fileBrowser,
    filePathInput,
    tuiUI,
 )

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Get the currently focused conversation, if any.
getFocusedConversation :: EventM N TuiState (Maybe Conversation)
getFocusedConversation = do
    mConv <- use (tuiUI . conversationList . to List.listSelectedElement)
    pure $ fmap snd mConv

-------------------------------------------------------------------------------
-- File Path Dialog (Legacy)
-------------------------------------------------------------------------------

-- | Handle events when file path dialog is open.
handleFilePathDialogEvent :: BrickEvent N AppEvent -> EventM N TuiState ()
handleFilePathDialogEvent ev =
    case ev of
        VtyEvent (Vty.EvKey Vty.KEnter []) -> handleConfirmFileAttachment
        VtyEvent (Vty.EvKey Vty.KEsc []) -> do
            closeFilePathDialog
            showStatus StatusInfo "Attachment cancelled"
        VtyEvent vtyEv -> zoom (tuiUI . filePathInput) $ handleEditorEvent (VtyEvent vtyEv)
        _ -> pure ()

-- | Confirm file attachment from path input.
handleConfirmFileAttachment :: EventM N TuiState ()
handleConfirmFileAttachment = do
    pathLines <- use (tuiUI . filePathInput . to getEditContents)
    let pathText = Text.strip $ Text.unlines pathLines

    if Text.null pathText
        then do
            closeFilePathDialog
            showStatus StatusWarning "No file path entered"
        else do
            result <- liftIO $ loadMediaAttachment (Text.unpack pathText)
            case result of
                Left err -> do
                    closeFilePathDialog
                    showStatus StatusError $ Text.pack err
                Right attachment -> do
                    mConv <- getFocusedConversation
                    case mConv of
                        Nothing -> do
                            closeFilePathDialog
                            showStatus StatusError "No conversation selected"
                        Just conv -> do
                            let convId = conversationId conv
                            tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId [attachment]
                            closeFilePathDialog
                            showStatus StatusInfo $ "Attached: " <> fromMaybe "unnamed" attachment.mediaFilename

-- | Close the file path dialog (legacy) and reset input.
closeFilePathDialog :: EventM N TuiState ()
closeFilePathDialog = do
    tuiUI . attachmentDialogState .= AttachmentDialogClosed
    tuiUI . filePathInput . editContentsL .= TextZipper.textZipper [] Nothing

-- | Open the file browser dialog (replaces legacy text input).
openFilePathDialog :: EventM N TuiState ()
openFilePathDialog = openFileBrowserDialog

-------------------------------------------------------------------------------
-- File Browser Dialog
-------------------------------------------------------------------------------

-- | Handle events when file browser dialog is open.
handleFileBrowserDialogEvent :: BrickEvent N AppEvent -> EventM N TuiState ()
handleFileBrowserDialogEvent ev =
    case ev of
        VtyEvent (Vty.EvKey Vty.KEsc []) -> do
            closeFileBrowserDialog
            showStatus StatusInfo "Attachment cancelled"
        VtyEvent (Vty.EvKey Vty.KEnter []) -> handleFileBrowserSelection
        VtyEvent vtyEv -> do
            mFb <- use (tuiUI . fileBrowser)
            case mFb of
                Nothing -> pure ()
                Just _ -> zoom (tuiUI . fileBrowser . _Just) $ handleFileBrowserEvent vtyEv
        _ -> pure ()

-- | Handle file selection from FileBrowser.
handleFileBrowserSelection :: EventM N TuiState ()
handleFileBrowserSelection = do
    mFb <- use (tuiUI . fileBrowser)
    case mFb of
        Nothing -> do
            closeFileBrowserDialog
            showStatus StatusError "File browser not initialized"
        Just fb -> do
            case fileBrowserCursor fb of
                Nothing -> do
                    closeFileBrowserDialog
                    showStatus StatusWarning "No file selected"
                Just fileInfo -> do
                    let filePath = fileInfoFilePath fileInfo
                    result <- liftIO $ loadFileAsAttachment filePath
                    case result of
                        Left err -> do
                            closeFileBrowserDialog
                            showStatus StatusError $ Text.pack err
                        Right attachment -> do
                            mConv <- getFocusedConversation
                            case mConv of
                                Nothing -> do
                                    closeFileBrowserDialog
                                    showStatus StatusError "No conversation selected"
                                Just conv -> do
                                    let convId = conversationId conv
                                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId [attachment]
                                    closeFileBrowserDialog
                                    showStatus StatusInfo $ "Attached: " <> fromMaybe "unnamed" attachment.mediaFilename

-- | Close file browser dialog and cleanup.
closeFileBrowserDialog :: EventM N TuiState ()
closeFileBrowserDialog = do
    tuiUI . attachmentDialogState .= AttachmentDialogClosed
    tuiUI . fileBrowser .= Nothing

-- | Open file browser dialog.
openFileBrowserDialog :: EventM N TuiState ()
openFileBrowserDialog = do
    fb <- liftIO $ newFileBrowser selectNonDirectories FilePathInputWidget Nothing
    tuiUI . fileBrowser .= Just fb
    tuiUI . attachmentDialogState .= AttachmentDialogFileBrowser

-------------------------------------------------------------------------------
-- File Loading
-------------------------------------------------------------------------------

-- | Load a media attachment from a file path.
loadMediaAttachment :: FilePath -> IO (Either String MediaAttachment)
loadMediaAttachment input = do
    case parseMediaReference input of
        Left err -> pure $ Left err
        Right mediaRef -> do
            case resolveMediaType mediaRef of
                Left err -> pure $ Left err
                Right mimeType -> do
                    let filePath = case Text.breakOn ";" (Text.pack input) of
                            (_, "") -> input
                            (_, rest) -> Text.unpack $ Text.drop 1 rest
                    fileContent <- ByteString.readFile filePath
                    let base64Data = TextEncoding.decodeUtf8 $ Base64.encode fileContent
                    let filename = Just $ Text.pack $ takeFileName filePath
                    pure $ Right $ MediaAttachment mimeType base64Data filename

-- | Load file as MediaAttachment.
loadFileAsAttachment :: FilePath -> IO (Either String MediaAttachment)
loadFileAsAttachment filePath = do
    let mimeType = detectMimeType filePath
    fileContent <- ByteString.readFile filePath
    let base64Data = TextEncoding.decodeUtf8 $ Base64.encode fileContent
    let filename = Just $ Text.pack $ takeFileName filePath
    pure $ Right $ MediaAttachment mimeType base64Data filename

-- | Simple MIME type detection from file extension.
detectMimeType :: FilePath -> Text.Text
detectMimeType fp =
    let lowerFp = map toLower fp
        ext = takeExtension lowerFp
     in case ext of
            ".png" -> "image/png"
            ".jpg" -> "image/jpeg"
            ".jpeg" -> "image/jpeg"
            ".gif" -> "image/gif"
            ".webp" -> "image/webp"
            ".svg" -> "image/svg+xml"
            ".bmp" -> "image/bmp"
            ".tiff" -> "image/tiff"
            ".mp3" -> "audio/mpeg"
            ".wav" -> "audio/wav"
            ".ogg" -> "audio/ogg"
            ".mp4" -> "video/mp4"
            ".webm" -> "video/webm"
            ".mov" -> "video/quicktime"
            ".pdf" -> "application/pdf"
            ".json" -> "application/json"
            ".xml" -> "application/xml"
            ".zip" -> "application/zip"
            ".gz" -> "application/gzip"
            ".tar" -> "application/x-tar"
            ".txt" -> "text/plain"
            ".md" -> "text/markdown"
            ".html" -> "text/html"
            ".css" -> "text/css"
            ".js" -> "text/javascript"
            ".py" -> "text/x-python"
            ".hs" -> "text/x-haskell"
            ".c" -> "text/x-c"
            ".cpp" -> "text/x-c++"
            ".rs" -> "text/x-rust"
            ".go" -> "text/x-go"
            ".sh" -> "text/x-shellscript"
            ".yaml" -> "text/yaml"
            ".yml" -> "text/yaml"
            _ -> "application/octet-stream"
