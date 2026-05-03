{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Event handling for the TUI application.

This module handles all user input and application events for the TUI.
During migration to the OS model, tool operations use the RuntimeBridge
which synchronizes tools between the legacy Runtime and OS Core.
-}
module System.Agents.TUI.Event where

import Brick hiding (Down)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev, focusRing, focusSetCurrent)
import Brick.Widgets.Edit (editContentsL, getEditContents, handleEditorEvent)
import Brick.Widgets.List (handleListEvent, listElements, listInsert, listMoveTo, listSelectedElement)
import Control.Concurrent.Async (async, poll)
import Control.Concurrent.STM (STM, atomically, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Lens hiding (zoom)
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Zipper as TextZipper
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, takeFileName)
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (writeSystemTempFile)
import System.Process (readProcessWithExitCode)

import Prod.Tracer (Tracer (..))
import System.Agents.AgentTree (OSAgentNode (..), osNodeTools)
import System.Agents.Base (ConversationId (..), newConversationId)
import System.Agents.CLI.PromptScript (parseMediaReference, resolveMediaType)
import System.Agents.Media.Types (MediaAttachment (..))
import qualified System.Agents.OneShot as OneShot (Trace)
import qualified System.Agents.Runtime.Trace as Runtime (Trace)
import System.Agents.Session.Base
import System.Agents.SessionPrint (OrderPreference (..), PrintVisibility (..), SessionPrintOptions (..), formatSessionAsMarkdown)
import System.Agents.TUI.Clipboard (
    ClipboardContent (..),
    detectClipboardContent,
    hasClipboardSupport,
    loadMediaAttachmentFromPath,
 )
import System.Agents.TUI.FileBrowser (
    FileBrowserSelection (..),
    fileBrowserSelection,
    handleFileBrowserEvent,
    newFileBrowser,
 )
import System.Agents.TUI.KeyMapping (
    EventName (..),
    defaultKeyMapping,
    generateHelpContent,
    matchesEvent,
 )
import System.Agents.TUI.Types

data Trace
    = RuntimeTrace !Runtime.Trace
    | OneShotTrace !OneShot.Trace
    deriving (Show)
{- | Default keyboard shortcuts help content.
Generated from the default key mapping.
-}
defaultHelpContent :: [Text]
defaultHelpContent = generateHelpContent defaultKeyMapping
-- | Alias for defaultHelpContent for backward compatibility.
initHelpContent :: [Text]
initHelpContent = defaultHelpContent

{- | The base widgets that are always present in the focus ring.
These correspond to the main navigation lists.
-}
baseFocusWidgets :: [WidgetName]
baseFocusWidgets = [AgentListWidget, ConversationListWidget, SessionsListWidget]

{- | Build a focus ring for a given tab.
The ring includes base widgets plus tab-specific widgets inserted appropriately.
The focus ring order is designed so that pressing Tab from a base widget
will first visit the tab-specific widget(s) before moving to the next base widget.
-}
buildFocusRingForTab :: Tab -> FocusRing WidgetName
buildFocusRingForTab tab =
    -- Order: base widget, then its tab-specific widget(s), then next base widget, etc.
    case tab of
        AgentsTab ->
            focusRing [AgentListWidget, AgentInfoWidget, ConversationListWidget, SessionsListWidget]
        ChatsTab ->
            focusRing [ConversationListWidget, MessageEditorWidget, AttachmentListWidget, QueuedMessageListWidget, ConversationViewWidget, SessionsListWidget, AgentListWidget]
        HistoryTab ->
            focusRing [SessionsListWidget, SessionViewWidget, AgentListWidget, ConversationListWidget]
        HelpTab ->
            focusRing baseFocusWidgets

{- | Build a focus ring for a tab while preserving the current focus if valid.
If the current focus widget exists in the new tab's ring, it will be maintained.
Otherwise, the focus will be set to the tab's entry widget.
-}
buildFocusRingForTabPreserving :: Tab -> Maybe WidgetName -> FocusRing WidgetName
buildFocusRingForTabPreserving tab mCurrentFocus =
    let newRing = buildFocusRingForTab tab
     in case mCurrentFocus of
            Just currentFocus | currentFocus `elem` focusRingElements newRing ->
                focusSetCurrent currentFocus newRing
            _ -> newRing

{- | Get the default (entry) widget for a tab.
This is the widget that should receive focus when switching to this tab.
-}
tabEntryWidget :: Tab -> WidgetName
tabEntryWidget AgentsTab = AgentListWidget
tabEntryWidget ChatsTab = ConversationListWidget
tabEntryWidget HistoryTab = SessionsListWidget
tabEntryWidget HelpTab = AgentListWidget

-- | Cycle to the next tab forward.
cycleTabForward :: EventM N TuiState ()
cycleTabForward = do
    currentTabValue <- use (tuiUI . currentTab)
    let next = nextTab currentTabValue
    tuiUI . currentTab .= next
    mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    tuiUI . uiFocusRing .= buildFocusRingForTabPreserving next mCurrentFocus

-- | Cycle to the previous tab backward.
cycleTabBackward :: EventM N TuiState ()
cycleTabBackward = do
    currentTabValue <- use (tuiUI . currentTab)
    let prev = prevTab currentTabValue
    tuiUI . currentTab .= prev
    mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    tuiUI . uiFocusRing .= buildFocusRingForTabPreserving prev mCurrentFocus

-- | Get all elements in a focus ring.
focusRingElements :: FocusRing WidgetName -> [WidgetName]
focusRingElements fr =
    go (focusSetCurrent (tabEntryWidget AgentsTab) fr) []
  where
    go ring acc =
        case focusGetCurrent ring of
            Just w | w `elem` acc -> reverse acc
            Just w -> go (focusNext ring) (w : acc)
            Nothing -> reverse acc

-- | Get the next tab in the cycle.
nextTab :: Tab -> Tab
nextTab AgentsTab = ChatsTab
nextTab ChatsTab = HistoryTab
nextTab HistoryTab = HelpTab
nextTab HelpTab = AgentsTab

-- | Get the previous tab in the cycle.
prevTab :: Tab -> Tab
prevTab AgentsTab = HelpTab
prevTab ChatsTab = AgentsTab
prevTab HistoryTab = ChatsTab
prevTab HelpTab = HistoryTab

-- | Switch to the Chats tab and focus the message editor.
switchToChatsAndFocusMessage :: EventM N TuiState ()
switchToChatsAndFocusMessage = do
    tuiUI . currentTab .= ChatsTab
    tuiUI . uiFocusRing .= focusSetCurrent MessageEditorWidget (buildFocusRingForTab ChatsTab)
    tuiUI . zoomed .= False

-- | Handle Ctrl+Q with confirmation.
handleQuit :: EventM N TuiState ()
handleQuit = do
    pending <- use (tuiUI . quitConfirmationPending)
    if pending
        then halt
        else do
            tuiUI . quitConfirmationPending .= True
            showStatus StatusWarning "Are you sure? Press Ctrl+Q again to quit"

-- | Reset quit confirmation state.
resetQuitConfirmation :: EventM N TuiState ()
resetQuitConfirmation = do
    tuiUI . quitConfirmationPending .= False

-- | Main event handler for the TUI application.
tui_appHandleEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
tui_appHandleEvent tracer ev = do
    dialogState <- use (tuiUI . attachmentDialogState)
    case dialogState of
        AttachmentDialogPathInput -> handleFilePathDialogEvent ev
        AttachmentDialogFileBrowser -> handleFileBrowserDialogEvent ev
        AttachmentDialogClosed -> do
            mNavState <- use (tuiUI . turnNavigation)
            case mNavState of
                Just navState -> handleTurnNavigationEvent tracer navState ev
                Nothing -> handleNormalEvent tracer ev

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
                Just fb -> do
                    newFb <- liftIO $ handleFileBrowserEvent vtyEv fb
                    tuiUI . fileBrowser .= Just newFb
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
            case fileBrowserSelection fb of
                NoSelection -> do
                    closeFileBrowserDialog
                    showStatus StatusWarning "No file selected"
                DirectorySelected _path -> do
                    -- In attachment mode, we don't allow directory selection
                    -- The user needs to enter the directory and select a file
                    pure ()
                FileSelected filePath -> do
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

-- | Load file as MediaAttachment.
loadFileAsAttachment :: FilePath -> IO (Either String MediaAttachment)
loadFileAsAttachment filePath = do
    let mimeType = detectMimeType filePath
    fileContent <- ByteString.readFile filePath
    let base64Data = TextEncoding.decodeUtf8 $ Base64.encode fileContent
    let filename = Just $ Text.pack $ takeFileName filePath
    pure $ Right $ MediaAttachment mimeType base64Data filename

-- | Simple MIME type detection from file extension.
detectMimeType :: FilePath -> Text
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

-- | Close file browser dialog and cleanup.
closeFileBrowserDialog :: EventM N TuiState ()
closeFileBrowserDialog = do
    tuiUI . attachmentDialogState .= AttachmentDialogClosed
    tuiUI . fileBrowser .= Nothing

-- | Open file browser dialog.
openFileBrowserDialog :: EventM N TuiState ()
openFileBrowserDialog = do
    fb <- liftIO $ newFileBrowser FilePathInputWidget Nothing
    tuiUI . fileBrowser .= Just fb
    tuiUI . attachmentDialogState .= AttachmentDialogFileBrowser

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

-- | Handle subcall completed event.
handleSubcallCompleted :: ConversationId -> Text -> EventM N TuiState ()
handleSubcallCompleted subcallId result = do
    let subcallShort = Text.take 8 (Text.pack $ show subcallId)
    showStatus StatusInfo $ "Subcall completed cid=" <> subcallShort <> ": " <> Text.take 30 result
    updateConversationStatus subcallId ConversationStatus_WaitingForInput

-- | Handle subcall failed event.
handleSubcallFailed :: ConversationId -> Text -> EventM N TuiState ()
handleSubcallFailed subcallId err = do
    let subcallShort = Text.take 8 (Text.pack $ show subcallId)
    showStatus StatusError $ "Subcall failed cid=" <> subcallShort <> ": " <> err
    updateConversationStatus subcallId ConversationStatus_WaitingForInput

-- | Close the file path dialog (legacy) and reset input.
closeFilePathDialog :: EventM N TuiState ()
closeFilePathDialog = do
    tuiUI . attachmentDialogState .= AttachmentDialogClosed
    tuiUI . filePathInput . editContentsL .= TextZipper.textZipper [] Nothing

-- | Open the file browser dialog (replaces legacy text input).
openFilePathDialog :: EventM N TuiState ()
openFilePathDialog = openFileBrowserDialog

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Extract short identifier from ConversationId for debugging.
shortConvId :: ConversationId -> Text
shortConvId (ConversationId uuid) = Text.take 8 $ Text.pack $ UUID.toString uuid

-- ============================================================================
-- Turn Navigation Event Handling
-- ============================================================================

{- | Handle events when in turn navigation mode.

In this mode, the user can navigate through turns of a session using
up/down arrow keys, fork at a selected turn with 'f', and exit with
Enter or Escape.
-}
handleTurnNavigationEvent :: Tracer IO Trace -> TurnNavigationState -> BrickEvent N AppEvent -> EventM N TuiState ()
handleTurnNavigationEvent tracer navState ev = do
    case ev of
        VtyEvent (Vty.EvKey Vty.KUp []) ->
            handleTurnNavigationUp navState
        VtyEvent (Vty.EvKey Vty.KDown []) ->
            handleTurnNavigationDown navState
        VtyEvent (Vty.EvKey Vty.KEsc []) ->
            exitTurnNavigation
        VtyEvent (Vty.EvKey Vty.KEnter []) ->
            exitTurnNavigation
        VtyEvent (Vty.EvKey (Vty.KChar 'f') []) ->
            handleForkAtTurn tracer navState
        VtyEvent (Vty.EvKey (Vty.KChar 'F') []) ->
            handleForkAtTurn tracer navState
        AppEvent (AppEvent_AgentStepProgrress convId session) -> do
            handleConversationUpdated convId session
        _ -> pure ()

-- | Navigate up in turn navigation (to newer turns).
handleTurnNavigationUp :: TurnNavigationState -> EventM N TuiState ()
handleTurnNavigationUp navState = do
    let currentIdx = navState ^. navSelectedTurnIndex
    when (currentIdx > 0) $ do
        let newIdx = currentIdx - 1
        tuiUI . turnNavigation .= Just (navState & navSelectedTurnIndex .~ newIdx)

-- | Navigate down in turn navigation (to older turns).
handleTurnNavigationDown :: TurnNavigationState -> EventM N TuiState ()
handleTurnNavigationDown navState = do
    let currentIdx = navState ^. navSelectedTurnIndex
        maxIdx = navState ^. navTotalTurns - 1
    when (currentIdx < maxIdx) $ do
        let newIdx = currentIdx + 1
        tuiUI . turnNavigation .= Just (navState & navSelectedTurnIndex .~ newIdx)

-- | Exit turn navigation mode.
exitTurnNavigation :: EventM N TuiState ()
exitTurnNavigation = do
    tuiUI . turnNavigation .= Nothing
    showStatus StatusInfo "Exited turn navigation mode"

-- | Fork conversation at the selected turn.
handleForkAtTurn :: Tracer IO Trace -> TurnNavigationState -> EventM N TuiState ()
handleForkAtTurn _tracer navState = do
    let session = navState ^. navSession
        selectedIdx = navState ^. navSelectedTurnIndex
        totalTurns = length session.turns
    
    -- Calculate the number of turns to keep
    let keepCount = selectedIdx + 1
    
    when (keepCount > 0 && keepCount <= totalTurns) $ do
        -- Get the turns to preserve (0 to selectedIdx)
        let preservedTurns = take keepCount session.turns
        
        -- Create a new forked session
        newSessionId' <- liftIO newSessionId
        newTurnId' <- liftIO newTurnId
        let forkedSession = Session
                { turns = preservedTurns
                , sessionId = newSessionId'
                , forkedFromSessionId = Just session.sessionId
                , turnId = newTurnId'
                , sessionVersion = session.sessionVersion
                , sessionExecutionMode = session.sessionExecutionMode
                }
        
        -- Find the agent for this conversation
        mConv <- getFocusedConversation
        case mConv of
            Nothing -> showStatus StatusError "No conversation selected to fork"
            Just conv -> do
                let agent = conversationAgent conv
                
                -- Create conversation name indicating it's a fork
                let forkName = conversationName conv <> " (fork at turn " <> Text.pack (show keepCount) <> ")"
                
                -- Start a new conversation with the forked session
                handleRestoredConversation agent forkName forkedSession
                exitTurnNavigation
                showStatus StatusInfo $ "Forked conversation at turn " <> Text.pack (show keepCount)

-- ============================================================================
-- Normal Event Handling
-- ============================================================================

{- | Handle normal (non-navigation) events.

This is the main event handler when not in turn navigation mode or
attachment dialog mode.
-}
handleNormalEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
handleNormalEvent tracer ev = do
    keyMap <- use keyMapping
    
    -- Check for quit confirmation reset on any non-quit key press
    case ev of
        VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) -> pure ()
        VtyEvent _ -> resetQuitConfirmation
        _ -> pure ()
    
    -- Extract VtyEvent for keybinding checks
    let mVtyEv = case ev of
            VtyEvent vtyEv -> Just vtyEv
            _ -> Nothing
    
    -- First check for global keybindings
    case mVtyEv of
        Just vtyEv | matchesEvent keyMap EventQuit vtyEv -> handleQuit
                   | matchesEvent keyMap EventCycleTabForward vtyEv -> cycleTabForward
                   | matchesEvent keyMap EventCycleTabBackward vtyEv -> cycleTabBackward
                   | matchesEvent keyMap EventCycleFocusForward vtyEv -> cycleFocusForward
                   | matchesEvent keyMap EventCycleFocusBackward vtyEv -> cycleFocusBackward
                   | matchesEvent keyMap EventToggleZoom vtyEv -> toggleZoom
                   | matchesEvent keyMap EventNewConversation vtyEv -> handleNewConversationFromEditor tracer
                   | matchesEvent keyMap EventSendMessage vtyEv -> handleSendMessage tracer
                   | matchesEvent keyMap EventAttachFile vtyEv -> openFilePathDialog
                   | matchesEvent keyMap EventClearAttachments vtyEv -> handleClearAllAttachments
                   | matchesEvent keyMap EventPasteClipboard vtyEv -> handleClipboardPaste
                   | matchesEvent keyMap EventExportSession vtyEv -> handleDumpSessionToMarkdown
                   | matchesEvent keyMap EventViewSessionChronological vtyEv -> 
                         handleViewSessionWithExternalViewer Chronological
                   | matchesEvent keyMap EventViewSessionReverse vtyEv -> 
                         handleViewSessionWithExternalViewer Antichronological
                   | matchesEvent keyMap EventClearQueuedMessages vtyEv -> handleClearQueuedMessages
                   | matchesEvent keyMap EventTogglePause vtyEv -> handleTogglePauseConversation
                   | matchesEvent keyMap EventRefreshTools vtyEv -> handleRefreshTools
                   | otherwise -> handleNonGlobalEvent tracer ev
        Nothing -> handleNonGlobalEvent tracer ev

-- | Handle non-global events (application events and widget-specific events).
handleNonGlobalEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
handleNonGlobalEvent tracer ev = do
    currentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    case ev of
        -- Application events
        AppEvent AppEvent_Heartbeat -> handleHeartbeat
        AppEvent (AppEvent_ShowStatus severity text) -> handleShowStatus severity text
        AppEvent AppEvent_ClearStatus -> handleClearStatus
        AppEvent (AppEvent_AgentStepProgrress convId session) -> handleConversationUpdated convId session
        AppEvent (AppEvent_AgentNeedsInput convId) -> handleConversationNeedsInput convId
        AppEvent (AppEvent_SubcallStarted parentId subcallId agentSlug depth) -> 
            handleSubcallStarted tracer parentId subcallId agentSlug depth
        AppEvent (AppEvent_SubcallProgress subcallId session) -> 
            handleSubcallProgress subcallId session
        AppEvent (AppEvent_SubcallCompleted subcallId result) -> 
            handleSubcallCompleted subcallId result
        AppEvent (AppEvent_SubcallFailed subcallId err) -> 
            handleSubcallFailed subcallId err
        
        -- Vty events
        VtyEvent vtyEv -> handleWidgetSpecificEvent tracer currentFocus vtyEv
        
        _ -> pure ()

-- | Handle widget-specific events.
handleWidgetSpecificEvent :: Tracer IO Trace -> Maybe WidgetName -> Vty.Event -> EventM N TuiState ()
handleWidgetSpecificEvent tracer currentFocus ev = do
    case currentFocus of
        Just AgentListWidget -> handleAgentListEvent tracer ev
        Just ConversationListWidget -> handleConversationListEvent tracer ev
        Just SessionsListWidget -> handleSessionsListEvent tracer ev
        Just MessageEditorWidget -> handleMessageEditorEvent ev
        Just ConversationViewWidget -> handleConversationViewEvent ev
        Just SessionViewWidget -> handleSessionViewEvent ev
        Just AgentInfoWidget -> handleAgentInfoEvent ev
        Just QueuedMessageListWidget -> handleQueuedMessageListEvent ev
        Just AttachmentListWidget -> handleAttachmentListEvent ev
        _ -> handleGlobalVtyEvent ev

-- | Handle global Vty events (not widget-specific).
handleGlobalVtyEvent :: Vty.Event -> EventM N TuiState ()
handleGlobalVtyEvent ev = do
    case ev of
        Vty.EvKey Vty.KEsc [] -> do
            mNav <- use (tuiUI . turnNavigation)
            case mNav of
                Just _ -> exitTurnNavigation
                Nothing -> pure ()
        _ -> pure ()

-- ============================================================================
-- Widget-Specific Event Handlers
-- ============================================================================

-- | Handle events for the agent list widget.
handleAgentListEvent :: Tracer IO Trace -> Vty.Event -> EventM N TuiState ()
handleAgentListEvent _tracer ev = do
    zoom (tuiUI . agentList) $ handleListEvent ev
    -- Update selected agent info after navigation
    mSelected <- use (tuiUI . agentList . to listSelectedElement)
    case mSelected of
        Just (_, agent) -> tuiUI . selectedAgentInfo .= Just agent
        Nothing -> pure ()

-- | Handle events for the conversation list widget.
handleConversationListEvent :: Tracer IO Trace -> Vty.Event -> EventM N TuiState ()
handleConversationListEvent _tracer ev = do
    case ev of
        Vty.EvKey Vty.KEnter [] -> do
            mConv <- getFocusedConversation
            case mConv of
                Just conv | conversationStatus conv == ConversationStatus_WaitingForInput ->
                    switchToChatsAndFocusMessage
                _ -> pure ()
        _ -> do
            zoom (tuiUI . conversationList) $ handleListEvent ev
            -- Mark conversation as read when selected
            mConv <- getFocusedConversation
            case mConv of
                Just conv -> do
                    tuiUI . unreadConversations %= Set.delete (conversationId conv)
                Nothing -> pure ()

-- | Handle events for the sessions list widget.
handleSessionsListEvent :: Tracer IO Trace -> Vty.Event -> EventM N TuiState ()
handleSessionsListEvent _tracer ev = do
    zoom (tuiUI . sessionList) $ handleListEvent ev

-- | Handle events for the message editor widget.
handleMessageEditorEvent :: Vty.Event -> EventM N TuiState ()
handleMessageEditorEvent ev = do
    zoom (tuiUI . messageEditor) $ handleEditorEvent (VtyEvent ev)

-- | Handle events for the conversation view widget (scrolling).
handleConversationViewEvent :: Vty.Event -> EventM N TuiState ()
handleConversationViewEvent _ev = do
    -- Just allow default viewport scrolling behavior
    pure ()

-- | Handle events for the session view widget (scrolling).
handleSessionViewEvent :: Vty.Event -> EventM N TuiState ()
handleSessionViewEvent _ev = do
    -- Just allow default viewport scrolling behavior
    pure ()

-- | Handle events for the agent info widget.
handleAgentInfoEvent :: Vty.Event -> EventM N TuiState ()
handleAgentInfoEvent _ev = do
    -- Just allow default viewport scrolling behavior
    pure ()

-- | Handle events for the queued message list widget.
handleQueuedMessageListEvent :: Vty.Event -> EventM N TuiState ()
handleQueuedMessageListEvent ev = do
    case ev of
        Vty.EvKey Vty.KUp [] -> handleQueueNavigation (-1)
        Vty.EvKey Vty.KDown [] -> handleQueueNavigation 1
        Vty.EvKey Vty.KDel [] -> handleDeleteSelectedMessage
        Vty.EvKey Vty.KBS [] -> handleDeleteSelectedMessage
        _ -> pure ()

-- | Handle events for the attachment list widget.
handleAttachmentListEvent :: Vty.Event -> EventM N TuiState ()
handleAttachmentListEvent ev = do
    case ev of
        Vty.EvKey Vty.KUp [] -> handleAttachmentNavigation (-1)
        Vty.EvKey Vty.KDown [] -> handleAttachmentNavigation 1
        Vty.EvKey Vty.KDel [] -> handleRemoveSelectedAttachment
        Vty.EvKey Vty.KBS [] -> handleRemoveSelectedAttachment
        _ -> pure ()

-- | Navigate attachment selection.
handleAttachmentNavigation :: Int -> EventM N TuiState ()
handleAttachmentNavigation delta = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> pure ()
        Just conv -> do
            attachments <- use (tuiUI . attachedFiles)
            let convId = conversationId conv
            case Map.lookup convId attachments of
                Nothing -> pure ()
                Just atts -> do
                    let count = length atts
                    currentIdx <- use (tuiUI . selectedAttachmentIndex)
                    let newIdx = case currentIdx of
                            Nothing -> if delta > 0 then 0 else count - 1
                            Just idx -> max 0 (min (count - 1) (idx + delta))
                    tuiUI . selectedAttachmentIndex .= Just newIdx

-- ============================================================================
-- Attachment Management
-- ============================================================================

-- | Remove the currently selected attachment.
handleRemoveSelectedAttachment :: EventM N TuiState ()
handleRemoveSelectedAttachment = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            let convId = conversationId conv
            mIdx <- use (tuiUI . selectedAttachmentIndex)
            case mIdx of
                Nothing -> showStatus StatusWarning "No attachment selected"
                Just idx -> do
                    attachments <- use (tuiUI . attachedFiles)
                    case Map.lookup convId attachments of
                        Nothing -> showStatus StatusWarning "No attachments for this conversation"
                        Just atts -> do
                            let newAtts = deleteAt idx atts
                            if null newAtts
                                then tuiUI . attachedFiles %= Map.delete convId
                                else tuiUI . attachedFiles %= Map.insert convId newAtts
                            -- Adjust selection index
                            when (idx >= length newAtts) $ do
                                let newIdx = if null newAtts then Nothing else Just (length newAtts - 1)
                                tuiUI . selectedAttachmentIndex .= newIdx
                            showStatus StatusInfo "Attachment removed"

-- | Clear all attachments for the current conversation.
handleClearAllAttachments :: EventM N TuiState ()
handleClearAllAttachments = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            let convId = conversationId conv
            tuiUI . attachedFiles %= Map.delete convId
            tuiUI . selectedAttachmentIndex .= Nothing
            showStatus StatusInfo "All attachments cleared"

-- | Handle clipboard paste (images or files).
handleClipboardPaste :: EventM N TuiState ()
handleClipboardPaste = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            hasSupport <- liftIO hasClipboardSupport
            if not hasSupport
                then showStatus StatusError "Clipboard support not available"
                else do
                    mContent <- liftIO detectClipboardContent
                    case mContent of
                        Nothing -> showStatus StatusWarning "Clipboard is empty"
                        Just content -> handleClipboardContent conv content

-- | Handle detected clipboard content.
handleClipboardContent :: Conversation -> ClipboardContent -> EventM N TuiState ()
handleClipboardContent conv content = do
    let convId = conversationId conv
    case content of
        ClipboardImage bs mime -> do
            -- Create attachment from binary data
            let base64Data = TextEncoding.decodeUtf8 $ Base64.encode bs
                attachment = MediaAttachment mime base64Data (Just "clipboard-image")
            tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId [attachment]
            showStatus StatusInfo "Image pasted from clipboard"
        ClipboardFilePath path -> do
            result <- liftIO $ loadMediaAttachmentFromPath path
            case result of
                Left err -> showStatus StatusError $ Text.pack err
                Right attachment -> do
                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId [attachment]
                    showStatus StatusInfo $ "File pasted: " <> fromMaybe "unnamed" attachment.mediaFilename
        ClipboardText text -> do
            -- Insert text at cursor position in message editor
            currentEditor <- use (tuiUI . messageEditor)
            let currentLines = getEditContents currentEditor
            let newLines = case currentLines of
                    [] -> [text]
                    (l:ls) -> (l <> text) : ls
            tuiUI . messageEditor . editContentsL .= TextZipper.textZipper newLines Nothing
            showStatus StatusInfo "Text pasted from clipboard"
        ClipboardFilePaths paths -> do
            -- Handle multiple file paths
            results <- liftIO $ mapM loadMediaAttachmentFromPath paths
            let attachments = [att | Right att <- results]
            if null attachments
                then showStatus StatusWarning "No valid files to paste"
                else do
                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId attachments
                    showStatus StatusInfo $ Text.pack (show (length attachments)) <> " files pasted from clipboard"
        ClipboardUnknown -> do
            showStatus StatusWarning "Unknown clipboard content"

-- ============================================================================
-- Focus Management
-- ============================================================================

-- | Map a widget to its associated tab.
widgetToTab :: WidgetName -> Tab
widgetToTab AgentListWidget = AgentsTab
widgetToTab AgentInfoWidget = AgentsTab
widgetToTab ConversationListWidget = ChatsTab
widgetToTab SessionsListWidget = HistoryTab
widgetToTab MessageEditorWidget = ChatsTab
widgetToTab ConversationViewWidget = ChatsTab
widgetToTab AttachmentListWidget = ChatsTab
widgetToTab QueuedMessageListWidget = ChatsTab
widgetToTab SessionViewWidget = HistoryTab
widgetToTab TurnNavigationWidget = ChatsTab
widgetToTab FilePathInputWidget = ChatsTab

-- | Update the current tab based on the focused widget.
updateTabFromFocus :: EventM N TuiState ()
updateTabFromFocus = do
    mFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    case mFocus of
        Just widget -> do
            let newTab = widgetToTab widget
            tuiUI . currentTab .= newTab
        Nothing -> pure ()

-- | Cycle focus forward through the focus ring.
cycleFocusForward :: EventM N TuiState ()
cycleFocusForward = do
    oldTab <- use (tuiUI . currentTab)
    tuiUI . uiFocusRing %= focusNext
    mFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    case mFocus of
        Just widget -> 
            when (widgetToTab widget /= oldTab) $ do
                tuiUI . currentTab .= widgetToTab widget
        Nothing -> pure ()

-- | Cycle focus backward through the focus ring.
cycleFocusBackward :: EventM N TuiState ()
cycleFocusBackward = do
    oldTab <- use (tuiUI . currentTab)
    tuiUI . uiFocusRing %= focusPrev
    mFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    case mFocus of
        Just widget -> 
            when (widgetToTab widget /= oldTab) $ do
                tuiUI . currentTab .= widgetToTab widget
        Nothing -> pure ()

-- | Toggle zoom mode.
toggleZoom :: EventM N TuiState ()
toggleZoom = do
    isZoomed <- use (tuiUI . zoomed)
    tuiUI . zoomed .= not isZoomed
    showStatus StatusInfo $ if not isZoomed then "Zoom enabled" else "Zoom disabled"

-- ============================================================================
-- Session and Conversation Access
-- ============================================================================

-- | Get the currently focused session from the session list.
getFocusedSession :: EventM N TuiState (Maybe Session)
getFocusedSession = do
    mSess <- use (tuiUI . sessionList . to listSelectedElement)
    pure $ fmap snd mSess

-- | Get the currently focused conversation from the conversation list.
getFocusedConversation :: EventM N TuiState (Maybe Conversation)
getFocusedConversation = do
    mConv <- use (tuiUI . conversationList . to listSelectedElement)
    pure $ fmap snd mConv

-- | Get the ID of the currently focused conversation.
getFocusedConversationId :: EventM N TuiState (Maybe ConversationId)
getFocusedConversationId = do
    mConv <- getFocusedConversation
    pure $ fmap conversationId mConv

-- ============================================================================
-- Session Export and Viewing
-- ============================================================================

-- | Format the current session as markdown.
formatSessionMarkdown :: Session -> OrderPreference -> Text
formatSessionMarkdown session order =
    let opts = SessionPrintOptions
            { sessionPrintFile = ""
            , showToolCallResults = ShownFull
            , showToolCallArguments = ShownFull
            , nTurns = Nothing
            , repeatSystemPrompt = False
            , repeatTools = False
            , orderPreference = order
            , noFunnyStamp = True
            }
    in formatSessionAsMarkdown opts session

-- | Export the current session to a markdown file.
handleDumpSessionToMarkdown :: EventM N TuiState ()
handleDumpSessionToMarkdown = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            case conversationSession conv of
                Nothing -> showStatus StatusError "No session to export"
                Just session -> do
                    let convIdStr = Text.unpack $ shortConvId (conversationId conv)
                    let filename = "session-" <> convIdStr <> ".md"
                    let markdown = formatSessionMarkdown session Chronological
                    liftIO $ TextIO.writeFile filename markdown
                    showStatus StatusInfo $ "Session exported to " <> Text.pack filename

-- | View the current session with an external markdown viewer.
handleViewSessionWithExternalViewer :: OrderPreference -> EventM N TuiState ()
handleViewSessionWithExternalViewer order = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            case conversationSession conv of
                Nothing -> showStatus StatusError "No session to view"
                Just session -> do
                    -- Create a temp file with the session markdown
                    tempFile <- liftIO $ writeSystemTempFile "session-view.md" ""
                    let markdown = formatSessionMarkdown session order
                    
                    -- Write to temp file
                    liftIO $ TextIO.writeFile tempFile markdown
                    
                    -- Try to open with external viewer
                    mViewer <- liftIO $ lookupEnv "AGENTS_MARKDOWN_VIEWER"
                    case mViewer of
                        Nothing -> showStatus StatusError "AGENTS_MARKDOWN_VIEWER not set"
                        Just viewer -> do
                            -- Launch viewer asynchronously
                            asyncTask <- liftIO $ async $ do
                                (exitCode, _, err) <- readProcessWithExitCode viewer [tempFile] ""
                                case exitCode of
                                    ExitSuccess -> pure ()
                                    ExitFailure code -> 
                                        hPutStrLn stderr $ "Viewer exited with code " ++ show code ++ ": " ++ err
                            
                            -- Store the task for cleanup
                            let auxTask = Viewer
                                    { viewerAsync = asyncTask
                                    , viewerConversationId = conversationId conv
                                    , viewerSessionId = session.sessionId
                                    }
                            tuiUI . auxiliaryTasks %= (auxTask :)
                            
                            showStatus StatusInfo "Opening session in external viewer"

-- ============================================================================
-- Heartbeat and Cleanup
-- ============================================================================

-- | Handle heartbeat event - perform periodic cleanup and updates.
handleHeartbeat :: EventM N TuiState ()
handleHeartbeat = do
    -- Clean up completed auxiliary tasks
    cleanupAuxiliaryTasks
    
    -- Refresh UI from core state
    syncWithCoreState

-- | Clean up completed auxiliary tasks.
cleanupAuxiliaryTasks :: EventM N TuiState ()
cleanupAuxiliaryTasks = do
    tasks <- use (tuiUI . auxiliaryTasks)
    remaining <- liftIO $ filterM (fmap not . isTaskComplete) tasks
    tuiUI . auxiliaryTasks .= remaining
  where
    isTaskComplete :: AuxiliaryTask -> IO Bool
    isTaskComplete (Viewer task _ _) = do
        mResult <- poll task
        pure $ case mResult of
            Just _ -> True
            Nothing -> False

-- | Sync UI state with core state.
syncWithCoreState :: EventM N TuiState ()
syncWithCoreState = do
    core <- use tuiCore
    coreState <- liftIO $ readTVarIO core
    
    -- Sync buffered messages
    bufferedMsgs <- liftIO $ readAndClearBufferedMessages coreState
    tuiUI . uiBufferedMessages .= bufferedMsgs
    
    -- Sync agent tools
    tuiUI . uiAgentTools .= coreAgentTools coreState

-- | Handle showing a status message.
handleShowStatus :: StatusSeverity -> Text -> EventM N TuiState ()
handleShowStatus severity text = do
    now <- liftIO getCurrentTime
    let msg = StatusMessage text severity now
    tuiUI . statusMessage .= Just msg

-- | Handle clearing the status message.
handleClearStatus :: EventM N TuiState ()
handleClearStatus = do
    tuiUI . statusMessage .= Nothing

-- ============================================================================
-- Conversation Management
-- ============================================================================

-- | Handle creating a new conversation from the editor state.
handleNewConversationFromEditor :: Tracer IO Trace -> EventM N TuiState ()
handleNewConversationFromEditor tracer = do
    mAgent <- use (tuiUI . selectedAgentInfo)
    case mAgent of
        Nothing -> showStatus StatusError "No agent selected"
        Just agent -> handleNewConversation tracer agent

-- | Create a new conversation with the given agent.
handleNewConversation :: Tracer IO Trace -> TuiAgent -> EventM N TuiState ()
handleNewConversation _tracer agent = do
    convId <- liftIO newConversationId
    
    -- Create a new channel for this conversation
    convChan <- liftIO $ newBChan 100
    
    -- Get agent tools
    tools <- liftIO $ readTVarIO (osNodeTools $ tuiNode agent)
    
    -- Store tools in UI state
    tuiUI . uiAgentTools %= ((tuiAgentId agent, tools) :)
    
    -- Create onProgress callback
    onProgress <- liftIO $ buildOnProgress convChan
    
    let conv = Conversation
            { conversationId = convId
            , conversationAgent = agent
            , conversationThreadId = Nothing
            , conversationSession = Nothing
            , conversationName = tuiSlug agent <> " " <> shortConvId convId
            , conversationChan = convChan
            , conversationStatus = ConversationStatus_WaitingForInput
            , conversationOnProgress = onProgress
            , conversationIsSubcall = False
            , conversationParentId = Nothing
            , conversationSubcallDepth = 0
            }
    
    -- Add conversation to core state
    core <- use tuiCore
    liftIO $ atomically $ modifyTVar core $ \c ->
        c { coreConversations = conv : coreConversations c }
    
    -- Add conversation to UI list
    tuiUI . conversationList %= listInsert 0 conv
    tuiUI . conversationList %= listMoveTo 0
    
    -- Switch to Chats tab and focus message editor
    switchToChatsAndFocusMessage
    showStatus StatusInfo $ "New conversation started with " <> tuiSlug agent
-- | Handle a restored conversation with an existing session.
handleRestoredConversation :: TuiAgent -> Text -> Session -> EventM N TuiState ()
handleRestoredConversation agent convName session = do
    convId <- liftIO newConversationId
    
    -- Create a new channel for this conversation
    convChan <- liftIO $ newBChan 100
    
    onProgress <- liftIO $ buildOnProgress convChan
    
    let conv = Conversation
            { conversationId = convId
            , conversationAgent = agent
            , conversationThreadId = Nothing
            , conversationSession = Just session
            , conversationName = convName
            , conversationChan = convChan
            , conversationStatus = ConversationStatus_WaitingForInput
            , conversationOnProgress = onProgress
            , conversationIsSubcall = False
            , conversationParentId = Nothing
            , conversationSubcallDepth = 0
            }
    
    -- Add conversation to core state
    core <- use tuiCore
    liftIO $ atomically $ modifyTVar core $ \c ->
        c { coreConversations = conv : coreConversations c }
    
    -- Add conversation to UI list
    tuiUI . conversationList %= listInsert 0 conv
    tuiUI . conversationList %= listMoveTo 0
    
    -- Switch to Chats tab
    switchToChatsAndFocusMessage
    showStatus StatusInfo $ "Restored conversation: " <> convName
handleConversationNeedsInput :: ConversationId -> EventM N TuiState ()
handleConversationNeedsInput convId = do
    updateConversationStatus convId ConversationStatus_WaitingForInput
    
    -- Check if this conversation is currently focused
    mCurrentConv <- getFocusedConversation
    case mCurrentConv of
        Just conv | conversationId conv == convId -> do
            -- If we're viewing this conversation, show notification
            showStatus StatusInfo "Agent is waiting for input"
        _ -> do
            -- Otherwise mark as unread
            tuiUI . unreadConversations %= Set.insert convId

-- | Update the status of a conversation.
updateConversationStatus :: ConversationId -> ConversationStatus -> EventM N TuiState ()
updateConversationStatus convId newStatus = do
    -- Update in core state
    core <- use tuiCore
    liftIO $ atomically $ modifyTVar core $ \c ->
        c { coreConversations = map updateStatus (coreConversations c) }
    
    -- Update in UI list
    tuiUI . conversationList %= fmap updateStatus
  where
    updateStatus conv
        | conversationId conv == convId = conv { conversationStatus = newStatus }
        | otherwise = conv

-- | Handle conversation update from progress event.
handleConversationUpdated :: ConversationId -> Session -> EventM N TuiState ()
handleConversationUpdated convId session = do
    -- Update in core state
    core <- use tuiCore
    liftIO $ atomically $ modifyTVar core $ \c ->
        c { coreConversations = map updateSession (coreConversations c) }
    
    -- Update in UI list
    tuiUI . conversationList %= fmap updateSession
  where
    updateSession conv
        | conversationId conv == convId = conv { conversationSession = Just session }
        | otherwise = conv

-- | Refresh tools for the currently selected agent.
handleRefreshTools :: EventM N TuiState ()
handleRefreshTools = do
    mAgent <- use (tuiUI . selectedAgentInfo)
    case mAgent of
        Nothing -> showStatus StatusError "No agent selected"
        Just agent -> do
            refreshToolsForAgent agent
            showStatus StatusInfo $ "Tools refreshed for " <> tuiSlug agent

-- | Refresh tools for a specific agent.
refreshToolsForAgent :: TuiAgent -> EventM N TuiState ()
refreshToolsForAgent agent = do
    tools <- liftIO $ readTVarIO (osNodeTools $ tuiNode agent)
    tuiUI . uiAgentTools %= ((tuiAgentId agent, tools) :)

-- | Append a conversation to the conversation list.
appendConversation :: Conversation -> EventM N TuiState ()
appendConversation conv = do
    tuiUI . conversationList %= listInsert 0 conv

-- ============================================================================
-- Pause/Unpause Conversation
-- ============================================================================

-- | Toggle pause state for the current conversation.
handleTogglePauseConversation :: EventM N TuiState ()
handleTogglePauseConversation = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            let convId = conversationId conv
                newStatus = case conversationStatus conv of
                    ConversationStatus_Paused -> ConversationStatus_WaitingForInput
                    _ -> ConversationStatus_Paused
            
            updateConversationStatus convId newStatus
            
            -- Update paused conversations set in core
            core <- use tuiCore
            liftIO $ atomically $ modifyTVar core $ \c ->
                case newStatus of
                    ConversationStatus_Paused ->
                        c { corePausedConversations = Set.insert convId (corePausedConversations c) }
                    _ ->
                        c { corePausedConversations = Set.delete convId (corePausedConversations c) }
            
            showStatus StatusInfo $ if newStatus == ConversationStatus_Paused 
                then "Conversation paused" 
                else "Conversation resumed"

-- | Check if a conversation is paused.
isConversationPaused :: ConversationId -> TuiState -> Bool
isConversationPaused convId st =
    case getConversation convId st of
        Just conv -> conversationStatus conv == ConversationStatus_Paused
        Nothing -> False

-- | Get a conversation by ID.
getConversation :: ConversationId -> TuiState -> Maybe Conversation
getConversation convId st =
    let convs = Vector.toList $ st ^. tuiUI . conversationList . to listElements
    in listToMaybe $ filter ((== convId) . conversationId) convs

-- ============================================================================
-- Queued Message Management
-- ============================================================================

-- | Clear all queued messages for the current conversation.
handleClearQueuedMessages :: EventM N TuiState ()
handleClearQueuedMessages = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            let convId = conversationId conv
            -- Clear in UI
            tuiUI . uiBufferedMessages %= Map.delete convId
            -- Also clear in core
            coreTVar <- use tuiCore
            core <- liftIO $ readTVarIO coreTVar
            liftIO $ atomically $ modifyTVar (coreBufferedMessages core) $ 
                Map.filterWithKey (\k _ -> k /= convId)
            showStatus StatusInfo "Queued messages cleared"

-- | Delete the currently selected queued message.
handleDeleteSelectedMessage :: EventM N TuiState ()
handleDeleteSelectedMessage = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> pure ()
        Just conv -> do
            let convId = conversationId conv
            mIdx <- use (tuiUI . queuedMessagesFocus)
            case mIdx of
                Nothing -> pure ()
                Just idx -> do
                    buffered <- use (tuiUI . uiBufferedMessages)
                    case Map.lookup convId buffered of
                        Nothing -> pure ()
                        Just msgs -> do
                            let newMsgs = deleteAt idx msgs
                            if null newMsgs
                                then tuiUI . uiBufferedMessages %= Map.delete convId
                                else tuiUI . uiBufferedMessages %= Map.insert convId newMsgs
                            -- Adjust selection
                            when (idx >= length newMsgs) $ do
                                let newIdx = if null newMsgs then Nothing else Just (length newMsgs - 1)
                                tuiUI . queuedMessagesFocus .= newIdx

-- | Delete element at index from a list.
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_:xs) = xs
deleteAt n (x:xs) = x : deleteAt (n-1) xs

-- | Navigate through queued messages.
handleQueueNavigation :: Int -> EventM N TuiState ()
handleQueueNavigation delta = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> pure ()
        Just conv -> do
            let convId = conversationId conv
            buffered <- use (tuiUI . uiBufferedMessages)
            case Map.lookup convId buffered of
                Nothing -> pure ()
                Just msgs -> do
                    let count = length msgs
                    currentIdx <- use (tuiUI . queuedMessagesFocus)
                    let newIdx = case currentIdx of
                            Nothing -> if delta > 0 then 0 else count - 1
                            Just idx -> max 0 (min (count - 1) (idx + delta))
                    tuiUI . queuedMessagesFocus .= Just newIdx

-- ============================================================================
-- Buffered Messages
-- ============================================================================

-- | Build the onProgress callback for a conversation.
buildOnProgress :: BChan (Maybe UserQuery) -> IO OnSessionProgress
buildOnProgress _chan = do
    -- Note: This is a placeholder. In a real implementation, we would
    -- capture the conversation ID and use it in the callback.
    -- For now, return a no-op handler.
    pure $ const (pure ())

-- | Build the onProgress callback for a specific conversation.
buildOnProgressForConversation :: BChan AppEvent -> ConversationId -> IO OnSessionProgress
buildOnProgressForConversation chan convId = do
    pure $ \progress -> case progress of
        SessionStarted _ -> pure ()
        SessionUpdated session -> 
            writeBChan chan (AppEvent_AgentStepProgrress convId session)
        SessionCompleted _ -> 
            writeBChan chan (AppEvent_AgentNeedsInput convId)
        SessionFailed _ err -> do
            writeBChan chan (AppEvent_ShowStatus StatusError $ "Session failed: " <> err)
            writeBChan chan (AppEvent_AgentNeedsInput convId)

-- | Read and clear buffered messages for all conversations (STM version).
readAndClearBufferedMessagesSTM :: Core -> STM (Map ConversationId [Text])
readAndClearBufferedMessagesSTM core = do
    msgs <- readTVar (coreBufferedMessages core)
    writeTVar (coreBufferedMessages core) Map.empty
    pure msgs

-- | Read and clear buffered messages for all conversations (IO version).
readAndClearBufferedMessages :: Core -> IO (Map ConversationId [Text])
readAndClearBufferedMessages core = 
    atomically $ readAndClearBufferedMessagesSTM core

-- | Add a buffered message for a conversation.
addBufferedMessage :: Core -> ConversationId -> Text -> STM ()
addBufferedMessage core convId msg = do
    modifyTVar (coreBufferedMessages core) $ \m ->
        Map.insertWith (\new old -> old ++ new) convId [msg] m

-- ============================================================================
-- Subcall Handling
-- ============================================================================

-- | Handle a subcall started event.
handleSubcallStarted :: Tracer IO Trace -> ConversationId -> ConversationId -> Text -> Int -> EventM N TuiState ()
handleSubcallStarted _tracer parentId subcallId agentSlug depth = do
    -- Find the agent by slug
    mAgent <- findAgentBySlug agentSlug
    case mAgent of
        Nothing -> do
            showStatus StatusError $ "Subcall agent not found: " <> agentSlug
        Just agent -> do
            -- Create a new channel for this subcall conversation
            convChan <- liftIO $ newBChan 100
            
            onProgress <- liftIO $ buildOnProgress convChan
            
            let conv = Conversation
                    { conversationId = subcallId
                    , conversationAgent = agent
                    , conversationThreadId = Nothing
                    , conversationSession = Nothing
                    , conversationName = tuiSlug agent <> " (subcall " <> Text.pack (show depth) <> ")"
                    , conversationChan = convChan
                    , conversationStatus = ConversationStatus_Active
                    , conversationOnProgress = onProgress
                    , conversationIsSubcall = True
                    , conversationParentId = Just parentId
                    , conversationSubcallDepth = depth
                    }
            
            -- Add to core
            core <- use tuiCore
            liftIO $ atomically $ modifyTVar core $ \c ->
                c { coreConversations = conv : coreConversations c }
            
            -- Add to UI
            tuiUI . conversationList %= listInsert 0 conv
            
            showStatus StatusInfo $ "Subcall started: " <> agentSlug

-- | Find an agent by its slug.
findAgentBySlug :: Text -> EventM N TuiState (Maybe TuiAgent)
findAgentBySlug slug = do
    agents <- use (tuiUI . agentList . to (Vector.toList . listElements))
    pure $ listToMaybe $ filter ((== slug) . tuiSlug) agents

-- | Handle subcall progress update.
handleSubcallProgress :: ConversationId -> Session -> EventM N TuiState ()
handleSubcallProgress subcallId session = do
    handleConversationUpdated subcallId session

-- | Create a subcall conversation entry (for programmatic creation).
createSubcallConversationEntry :: TuiAgent -> ConversationId -> Maybe ConversationId -> Int -> BChan (Maybe UserQuery) -> OnSessionProgress -> Conversation
createSubcallConversationEntry agent convId parentId depth chan onProgress =
    Conversation
        { conversationId = convId
        , conversationAgent = agent
        , conversationThreadId = Nothing
        , conversationSession = Nothing
        , conversationName = tuiSlug agent <> " (subcall " <> Text.pack (show depth) <> ")"
        , conversationChan = chan
        , conversationStatus = ConversationStatus_Active
        , conversationOnProgress = onProgress
        , conversationIsSubcall = True
        , conversationParentId = parentId
        , conversationSubcallDepth = depth
        }

-- ============================================================================
-- Message Sending and Conversation Running
-- ============================================================================

-- | Run a conversation - start the agent loop.
runConversation :: Tracer IO Trace -> Conversation -> Maybe UserQuery -> IO ()
runConversation _tracer _conv _mQuery = do
    -- This is a simplified version - the real implementation would
    -- create an Agent from the TuiAgent and run the session loop
    -- For now, we just mark it as an example
    pure ()

-- | Handle sending a message from the editor.
handleSendMessage :: Tracer IO Trace -> EventM N TuiState ()
handleSendMessage _tracer = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            -- Get message text
            editorLines <- use (tuiUI . messageEditor . to getEditContents)
            let msgText = Text.strip $ Text.unlines editorLines
            
            -- Get attachments
            attachments <- use (tuiUI . attachedFiles)
            let convId = conversationId conv
            let convAttachments = fromMaybe [] $ Map.lookup convId attachments
            
            if Text.null msgText && null convAttachments
                then showStatus StatusWarning "No message to send"
                else do
                    -- Create user query
                    let query = UserQuery msgText convAttachments
                    
                    -- Check if conversation is paused
                    st <- get
                    let isPaused = isConversationPaused convId st
                    
                    if isPaused
                        then do
                            -- Buffer the message
                            coreTVar <- use tuiCore
                            core <- liftIO $ readTVarIO coreTVar
                            liftIO $ atomically $ addBufferedMessage core convId msgText
                            tuiUI . uiBufferedMessages %= Map.insertWith (\new old -> old ++ new) convId [msgText]
                            showStatus StatusInfo "Message queued (conversation paused)"
                        else do
                            -- Send immediately
                            liftIO $ writeBChan (conversationChan conv) (Just query)
                            updateConversationStatus convId ConversationStatus_Active
                            showStatus StatusInfo "Message sent"
                    
                    -- Clear editor and attachments
                    tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing
                    tuiUI . attachedFiles %= Map.delete convId
                    tuiUI . selectedAttachmentIndex .= Nothing

