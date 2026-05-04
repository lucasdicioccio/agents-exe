{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Event handling for the TUI application.

This module handles all user input and application events for the TUI.
During migration to the OS model, tool operations use the RuntimeBridge
which synchronizes tools between the legacy Runtime and OS Core.
-}
module System.Agents.TUI.Event where

import Brick
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev, focusRing, focusSetCurrent)
import Brick.Widgets.Edit (editContentsL, getEditContents, handleEditorEvent)
import Brick.Widgets.FileBrowser (
    fileBrowserCursor,
    fileInfoFilePath,
    handleFileBrowserEvent,
    newFileBrowser,
    selectNonDirectories,
 )
import Brick.Widgets.List (handleListEvent, listElements, listInsert, listSelectedElement, listSelectedL)
import qualified Brick.Widgets.List as List
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, poll)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Lens (to, use, (%=), (.=), (^.), _Just)
import Control.Monad (filterM, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Zipper as TextZipper
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, takeFileName, (<.>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (writeSystemTempFile)
import System.Process (readProcessWithExitCode)

import System.Agents.AgentTree (OSAgentNode (..), osNodeTools)
import System.Agents.Base (AgentId (..), ConversationId (..), newConversationId)
import System.Agents.CLI.PromptScript (parseMediaReference, resolveMediaType)
import System.Agents.Combinators.ProgressiveDisclosure (agentEvaluateActiveTools)
import System.Agents.Media.Types (MediaAttachment (..))
import qualified System.Agents.OneShot as OneShot (Trace, mapProgressiveDisclosureTrace, nodeToAgent)
import qualified System.Agents.Runtime.Trace as Runtime (Trace)
import System.Agents.Session.Base (
    Action (..),
    Agent (..),
    MissingUserPrompt (..),
    OnSessionProgress,
    Session (..),
    SessionProgress (..),
    UserQuery (..),
    newSessionId,
    newTurnId,
 )
import qualified System.Agents.Session.Loop as Loop (run)
import System.Agents.SessionPrint (OrderPreference (..), PrintVisibility (..), SessionPrintOptions (..), formatSessionAsMarkdown)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.TUI.Clipboard (
    ContentAction (..),
    analyzeContent,
    detectClipboardContent,
    hasClipboardSupport,
 )
import System.Agents.TUI.KeyMapping (
    EventName (..),
    KeyMapping,
    defaultKeyMapping,
    generateHelpContent,
    matchesEvent,
 )
import System.Agents.TUI.MessageComposer (
    shouldSendMessage,
    stripSendTrigger,
 )
import System.Agents.TUI.Render (sortConversationsForNesting)
import System.Agents.TUI.Types (
    AppEvent (..),
    AttachmentDialogState (..),
    AuxiliaryTask (..),
    Conversation (..),
    ConversationStatus (..),
    Core (..),
    N,
    SessionConfig (..),
    StatusMessage (..),
    StatusSeverity (..),
    Tab (..),
    TuiAgent (..),
    TuiState,
    TurnNavigationState (..),
    WidgetName (..),
    agentList,
    attachedFiles,
    attachmentDialogState,
    auxiliaryTasks,
    conversationId,
    conversationList,
    conversationName,
    conversationParentId,
    conversationSession,
    conversationStatus,
    conversationSubcallDepth,
    coreBufferedMessages,
    coreConversations,
    corePausedConversations,
    currentTab,
    eventChan,
    fileBrowser,
    filePathInput,
    keyMapping,
    messageEditor,
    navSelectedTurnIndex,
    navSession,
    navTotalTurns,
    queuedMessagesFocus,
    quitConfirmationPending,
    selectedAgentInfo,
    selectedAttachmentIndex,
    sessionConfig,
    sessionList,
    statusMessage,
    tuiAgentId,
    tuiCore,
    tuiNode,
    tuiSlug,
    tuiTree,
    tuiUI,
    turnNavigation,
    uiAgentTools,
    uiBufferedMessages,
    uiFocusRing,
    unreadConversations,
    updateConversationSession,
    zoomed,
 )
import System.Agents.Tools.Context (CallStackEntry (..))

-- Import Tracer for creating a no-op tracer
import Prod.Tracer (Tracer (..), contramap)

data Trace
    = RuntimeTrace !Runtime.Trace
    | OneShotTrace !OneShot.Trace
    deriving (Show)

{- | Default keyboard shortcuts help content.
Generated from the default key mapping.
-}
defaultHelpContent :: [Text.Text]
defaultHelpContent = generateHelpContent defaultKeyMapping

-- | Alias for defaultHelpContent for backward compatibility.
initHelpContent :: [Text.Text]
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

{- | Get the default (entry) widget for a tab.
This is the widget that should receive focus when switching to this tab.
-}
tabEntryWidget :: Tab -> WidgetName
tabEntryWidget AgentsTab = AgentListWidget
tabEntryWidget ChatsTab = ConversationListWidget
tabEntryWidget HistoryTab = SessionsListWidget
tabEntryWidget HelpTab = AgentListWidget

{- | Build a focus ring for a tab, attempting to preserve the current focus if valid.
If the current focus is not in the new tab's focus ring, falls back to the tab's entry widget.
-}
buildFocusRingForTabPreserving :: Tab -> Maybe WidgetName -> FocusRing WidgetName
buildFocusRingForTabPreserving tab mCurrentFocus =
    let newRing = buildFocusRingForTab tab
        validFocus = case mCurrentFocus of
            Just wf | wf `elem` focusRingElements newRing -> Just wf
            _ -> Nothing
        startFocus = case validFocus of
            Just wf -> wf
            Nothing -> tabEntryWidget tab
     in focusSetCurrent startFocus newRing

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

-------------------------------------------------------------------------------
-- Tab Cycling Functions
-------------------------------------------------------------------------------

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

-- | Cycle to the next tab forward.
cycleTabForward :: EventM N TuiState ()
cycleTabForward = do
    current <- use (tuiUI . currentTab)
    let next = nextTab current
    tuiUI . currentTab .= next
    mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    tuiUI . uiFocusRing .= buildFocusRingForTabPreserving next mCurrentFocus

-- | Cycle to the previous tab backward.
cycleTabBackward :: EventM N TuiState ()
cycleTabBackward = do
    current <- use (tuiUI . currentTab)
    let prev = prevTab current
    tuiUI . currentTab .= prev
    mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    tuiUI . uiFocusRing .= buildFocusRingForTabPreserving prev mCurrentFocus

-------------------------------------------------------------------------------
-- Navigation Helpers
-------------------------------------------------------------------------------

-- | Switch to the Chats tab and focus the message editor.
switchToChatsAndFocusMessage :: EventM N TuiState ()
switchToChatsAndFocusMessage = do
    tuiUI . currentTab .= ChatsTab
    tuiUI . uiFocusRing .= focusSetCurrent MessageEditorWidget (buildFocusRingForTab ChatsTab)
    tuiUI . zoomed .= False

-------------------------------------------------------------------------------
-- Quit Confirmation
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Main Event Handler
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- File Path Dialog Event Handler (Legacy)
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
                            showStatus StatusInfo $ "Attached: " <> maybe "unnamed" id attachment.mediaFilename

-------------------------------------------------------------------------------
-- File Browser Dialog Event Handler
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
                                    showStatus StatusInfo $ "Attached: " <> maybe "unnamed" id attachment.mediaFilename

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
handleSubcallCompleted :: ConversationId -> Text.Text -> EventM N TuiState ()
handleSubcallCompleted subcallId result = do
    let subcallShort = Text.take 8 (Text.pack $ show subcallId)
    showStatus StatusInfo $ "Subcall completed cid=" <> subcallShort <> ": " <> Text.take 30 result
    -- Update the conversation status to indicate it's no longer running
    updateConversationStatus subcallId ConversationStatus_WaitingForInput

-- | Handle subcall failed event.
handleSubcallFailed :: ConversationId -> Text.Text -> EventM N TuiState ()
handleSubcallFailed subcallId err = do
    let subcallShort = Text.take 8 (Text.pack $ show subcallId)
    showStatus StatusError $ "Subcall failed cid=" <> subcallShort <> ": " <> err
    -- Update the conversation status to indicate it's no longer running
    updateConversationStatus subcallId ConversationStatus_WaitingForInput

-- | Close the file path dialog (legacy) and reset input.
closeFilePathDialog :: EventM N TuiState ()
closeFilePathDialog = do
    tuiUI . attachmentDialogState .= AttachmentDialogClosed
    tuiUI . filePathInput . editContentsL .= TextZipper.textZipper [] Nothing

-- | Open the file browser dialog (replaces legacy text input).
openFilePathDialog :: EventM N TuiState ()
openFilePathDialog = openFileBrowserDialog

-------------------------------------------------------------------------------
-- Turn Navigation Event Handler
-------------------------------------------------------------------------------

-- | Handle events when in turn navigation mode.
handleTurnNavigationEvent :: Tracer IO Trace -> TurnNavigationState -> BrickEvent N AppEvent -> EventM N TuiState ()
handleTurnNavigationEvent tracer navState ev = do
    keymap <- use keyMapping
    case ev of
        AppEvent AppEvent_Heartbeat -> handleHeartbeat
        AppEvent (AppEvent_AgentStepProgrress convId sess) -> handleConversationUpdated convId sess
        AppEvent (AppEvent_AgentNeedsInput convId) -> handleConversationNeedsInput convId
        AppEvent (AppEvent_AgentTrace _) -> pure ()
        AppEvent (AppEvent_ShowStatus severity text) -> handleShowStatus severity text
        AppEvent AppEvent_ClearStatus -> handleClearStatus
        AppEvent (AppEvent_SubcallStarted parentId subcallId slug depth) ->
            handleSubcallStarted tracer parentId subcallId slug depth
        AppEvent (AppEvent_SubcallProgress subcallId sess) ->
            handleSubcallProgress subcallId sess
        AppEvent (AppEvent_SubcallCompleted subcallId result) ->
            handleSubcallCompleted subcallId result
        AppEvent (AppEvent_SubcallFailed subcallId err) ->
            handleSubcallFailed subcallId err
        VtyEvent vtyEv
            | matchesEvent keymap EventExitTurnNavigation vtyEv -> do
                tuiUI . turnNavigation .= Nothing
                showStatus StatusInfo "Exited turn navigation"
                resetQuitConfirmation
        VtyEvent vtyEv
            | matchesEvent keymap EventNavigateUp vtyEv -> do
                let currentIdx = navState ^. navSelectedTurnIndex
                    newIdx = max 0 (currentIdx - 1)
                tuiUI . turnNavigation .= Just (navState{_navSelectedTurnIndex = newIdx})
        VtyEvent vtyEv
            | matchesEvent keymap EventNavigateDown vtyEv -> do
                let currentIdx = navState ^. navSelectedTurnIndex
                    maxIdx = (navState ^. navTotalTurns) - 1
                    newIdx = min maxIdx (currentIdx + 1)
                tuiUI . turnNavigation .= Just (navState{_navSelectedTurnIndex = newIdx})
        VtyEvent vtyEv
            | matchesEvent keymap EventForkAtTurn vtyEv -> handleForkAtTurn tracer navState
        _ -> pure ()

-- | Fork a new conversation at the selected turn.
handleForkAtTurn :: Tracer IO Trace -> TurnNavigationState -> EventM N TuiState ()
handleForkAtTurn tracer navState = do
    let session = navState ^. navSession
        selectedIdx = navState ^. navSelectedTurnIndex
        originalSessionId = session.sessionId
    let turnsToKeep = drop selectedIdx session.turns
    newSessionId' <- liftIO newSessionId
    newTurnId' <- liftIO newTurnId
    let forkedSession =
            Session
                { turns = turnsToKeep
                , sessionId = newSessionId'
                , forkedFromSessionId = Just originalSessionId
                , turnId = newTurnId'
                , sessionVersion = Just 1
                , sessionExecutionMode = Nothing
                }
    mAgent <- use (tuiUI . agentList . to listSelectedElement)
    case mAgent of
        Just (_, baseTuiAgent) -> do
            runConversation tracer baseTuiAgent forkedSession
            showStatus StatusInfo $ "Forked conversation at turn " <> Text.pack (show (selectedIdx + 1))
        Nothing -> showStatus StatusWarning "No agent selected to fork conversation"

-------------------------------------------------------------------------------
-- Normal Event Handler (Non-Navigation)
-------------------------------------------------------------------------------

-- | Handle normal (non-navigation) events.
handleNormalEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
handleNormalEvent tracer ev = do
    keymap <- use keyMapping
    case ev of
        AppEvent AppEvent_Heartbeat -> handleHeartbeat
        AppEvent (AppEvent_AgentStepProgrress convId sess) -> handleConversationUpdated convId sess
        AppEvent (AppEvent_AgentNeedsInput convId) -> handleConversationNeedsInput convId
        AppEvent (AppEvent_AgentTrace _) -> pure ()
        AppEvent (AppEvent_ShowStatus severity text) -> handleShowStatus severity text
        AppEvent AppEvent_ClearStatus -> handleClearStatus
        AppEvent (AppEvent_SubcallStarted parentId subcallId slug depth) ->
            handleSubcallStarted tracer parentId subcallId slug depth
        AppEvent (AppEvent_SubcallProgress subcallId sess) ->
            handleSubcallProgress subcallId sess
        AppEvent (AppEvent_SubcallCompleted subcallId result) ->
            handleSubcallCompleted subcallId result
        AppEvent (AppEvent_SubcallFailed subcallId err) ->
            handleSubcallFailed subcallId err
        VtyEvent vtyEv
            | matchesEvent keymap EventQuit vtyEv -> handleQuit
        VtyEvent vtyEv
            | matchesEvent keymap EventCycleTabBackward vtyEv -> do
                resetQuitConfirmation
                cycleTabBackward
        VtyEvent vtyEv
            | matchesEvent keymap EventCycleTabForward vtyEv -> do
                resetQuitConfirmation
                cycleTabForward
        VtyEvent vtyEv
            | matchesEvent keymap EventCycleFocusForward vtyEv -> do
                resetQuitConfirmation
                cycleFocusForward
        VtyEvent vtyEv
            | matchesEvent keymap EventCycleFocusBackward vtyEv -> do
                resetQuitConfirmation
                cycleFocusBackward
        VtyEvent vtyEv
            | matchesEvent keymap EventRefreshTools vtyEv -> do
                resetQuitConfirmation
                handleRefreshTools
        VtyEvent vtyEv
            | matchesEvent keymap EventToggleZoom vtyEv -> do
                resetQuitConfirmation
                toggleZoom
        VtyEvent vtyEv
            | matchesEvent keymap EventNewConversation vtyEv -> do
                resetQuitConfirmation
                handleNewConversationFromEditor tracer
        VtyEvent vtyEv
            | matchesEvent keymap EventContinueSession vtyEv -> do
                resetQuitConfirmation
                handleRestoredConversation tracer
        VtyEvent vtyEv
            | matchesEvent keymap EventSendMessage vtyEv -> do
                resetQuitConfirmation
                handleSendMessage
        VtyEvent vtyEv
            | matchesEvent keymap EventTogglePause vtyEv -> do
                resetQuitConfirmation
                handleTogglePauseConversation
        VtyEvent vtyEv
            | matchesEvent keymap EventAttachFile vtyEv -> do
                resetQuitConfirmation
                openFilePathDialog
        VtyEvent vtyEv
            | matchesEvent keymap EventClearAttachments vtyEv -> do
                resetQuitConfirmation
                handleClearAllAttachments
        VtyEvent vtyEv
            | matchesEvent keymap EventPasteClipboard vtyEv -> do
                resetQuitConfirmation
                handleClipboardPaste tracer
        VtyEvent vtyEv
            | matchesEvent keymap EventExportSession vtyEv -> do
                resetQuitConfirmation
                handleDumpSessionToMarkdown
        VtyEvent vtyEv
            | matchesEvent keymap EventViewSessionChronological vtyEv -> do
                resetQuitConfirmation
                handleViewSessionWithExternalViewer Chronological
        VtyEvent vtyEv
            | matchesEvent keymap EventViewSessionReverse vtyEv -> do
                resetQuitConfirmation
                handleViewSessionWithExternalViewer Antichronological
        VtyEvent vtyEv
            | matchesEvent keymap EventClearQueuedMessages vtyEv -> do
                resetQuitConfirmation
                handleClearQueuedMessages
        VtyEvent vtyEv -> do
            resetQuitConfirmation
            currentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
            case currentFocus of
                Just AgentListWidget -> handleAgentListEvent vtyEv
                Just SessionsListWidget -> handleSessionsListEvent vtyEv
                Just ConversationListWidget -> handleConversationListEvent vtyEv keymap
                Just MessageEditorWidget -> handleMessageEditorEvent ev
                Just ConversationViewWidget -> handleConversationViewEvent tracer vtyEv keymap
                Just SessionViewWidget -> handleSessionViewEvent tracer vtyEv keymap
                Just AgentInfoWidget -> handleAgentInfoEvent vtyEv
                Just QueuedMessageListWidget -> handleQueuedMessageListEvent vtyEv keymap
                Just AttachmentListWidget -> handleAttachmentListEvent vtyEv keymap
                _ -> pure ()
        _ -> pure ()

-------------------------------------------------------------------------------
-- Subcall Event Handlers
-------------------------------------------------------------------------------

{- | Handle subcall started event.
This creates a conversation entry for the subcall in the TUI.
Note: The actual execution is handled by runSubAgentWithEventEmission in OneShotTool.hs,
which emits progress events that update this conversation.
-}
handleSubcallStarted :: Tracer IO Trace -> ConversationId -> ConversationId -> Text.Text -> Int -> EventM N TuiState ()
handleSubcallStarted _tracer parentId subcallId slug depth = do
    -- Show status message with session IDs for debugging
    let parentShort = shortConvId parentId
    let subcallShort = shortConvId subcallId
    showStatus StatusInfo $ "Subcall d=" <> Text.pack (show depth) <> " pid=" <> parentShort <> " cid=" <> subcallShort <> " " <> slug

    -- Look up the agent by slug from the agent list
    agents <- use (tuiUI . agentList . to listElements)
    case findAgentBySlug slug agents of
        Just tuiAgent -> do
            -- Create the subcall conversation entry
            -- The session will be populated by progress events from OneShotTool
            createSubcallConversationEntry tuiAgent subcallId parentId depth
        Nothing -> do
            showStatus StatusWarning $ "Agent not found for subcall: " <> slug

-- | Find a TuiAgent by slug from the agent list.
findAgentBySlug :: Text.Text -> Vector.Vector TuiAgent -> Maybe TuiAgent
findAgentBySlug slug agents =
    Vector.find (\a -> tuiSlug a == slug) agents

-- | Handle subcall progress event.
handleSubcallProgress :: ConversationId -> Session -> EventM N TuiState ()
handleSubcallProgress subcallId sess = do
    coreRef <- use tuiCore
    -- Check if this subcall conversation exists
    coreState <- liftIO $ readTVarIO coreRef
    let exists = any (\c -> conversationId c == subcallId) (coreConversations coreState)
    if exists
        then do
            -- Update existing conversation
            liftIO $ atomically $ modifyTVar coreRef $ \c ->
                c{coreConversations = updateConversationSession subcallId sess (coreConversations c)}
        else do
            -- Conversation doesn't exist yet - heartbeat will pick it up
            let subcallShort = Text.take 8 (Text.pack $ show subcallId)
            showStatus StatusWarning $ "Subcall progress for unknown conv: " <> subcallShort

-------------------------------------------------------------------------------
-- Widget-Specific Event Handlers
-------------------------------------------------------------------------------

-- | Handle agent list navigation.
handleAgentListEvent :: Vty.Event -> EventM N TuiState ()
handleAgentListEvent ev = do
    zoom (tuiUI . agentList) $ handleListEvent ev
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, agent) -> do
            tuiUI . selectedAgentInfo .= Just agent
            refreshToolsForAgent agent
        Nothing -> pure ()

-- | Handle conversation list navigation.
handleConversationListEvent :: Vty.Event -> KeyMapping -> EventM N TuiState ()
handleConversationListEvent ev keymap =
    case ev of
        Vty.EvKey key mods
            | matchesEvent keymap EventOpenConversation (Vty.EvKey key mods) -> do
                mSelected <- use (tuiUI . conversationList . to listSelectedElement)
                case mSelected of
                    Just (_, conv) -> do
                        switchToChatsAndFocusMessage
                        tuiUI . unreadConversations %= Set.delete (conversationId conv)
                    Nothing -> pure ()
        _ -> do
            zoom (tuiUI . conversationList) $ handleListEvent ev
            selected <- use (tuiUI . conversationList . to listSelectedElement)
            case selected of
                Just (_, conv) ->
                    tuiUI . unreadConversations %= Set.delete (conversationId conv)
                Nothing -> pure ()

-- | Handle sessions list navigation.
handleSessionsListEvent :: Vty.Event -> EventM N TuiState ()
handleSessionsListEvent ev = zoom (tuiUI . sessionList) $ handleListEvent ev

-- | Handle message editor events.
handleMessageEditorEvent :: BrickEvent N AppEvent -> EventM N TuiState ()
handleMessageEditorEvent ev = do
    zoom (tuiUI . messageEditor) $ handleEditorEvent ev
    case ev of
        VtyEvent (Vty.EvKey Vty.KEnter mods)
            | Vty.MCtrl `elem` mods -> handleSendMessage
        _ -> checkTripleNewlineTrigger

-- | Check if triple-newline trigger should send the message.
checkTripleNewlineTrigger :: EventM N TuiState ()
checkTripleNewlineTrigger = do
    config <- use sessionConfig
    let inputCfg = sessionInputConfig config
    msgLines <- use (tuiUI . messageEditor . to getEditContents)
    -- Use intercalate instead of unlines to avoid trailing newline
    let msgText = Text.intercalate "\n" msgLines
    when (shouldSendMessage inputCfg msgText) $ do
        -- Strip the trigger suffix before sending
        let cleanedText = stripSendTrigger inputCfg msgText
        tuiUI . messageEditor . editContentsL .= TextZipper.textZipper (Text.lines cleanedText) Nothing
        handleSendMessage

-- | Handle conversation view scrolling and turn navigation.
handleConversationViewEvent :: Tracer IO Trace -> Vty.Event -> KeyMapping -> EventM N TuiState ()
handleConversationViewEvent _tracer ev keymap = do
    mConv <- getFocusedConversation
    hasQueuedMessages <- case mConv of
        Just conv -> do
            buffered <- use (tuiUI . uiBufferedMessages)
            pure $ case Map.lookup (conversationId conv) buffered of
                Just msgs | conversationStatus conv == ConversationStatus_Paused -> not (null msgs)
                _ -> False
        Nothing -> pure False

    case ev of
        Vty.EvKey key mods
            | matchesEvent keymap EventEnterTurnNavigation (Vty.EvKey key mods) -> do
                mSession <- getFocusedSession
                case mSession of
                    Just session | not (null session.turns) -> do
                        let navState =
                                TurnNavigationState
                                    { _navSession = session
                                    , _navSelectedTurnIndex = length session.turns - 1
                                    , _navTotalTurns = length session.turns
                                    }
                        tuiUI . turnNavigation .= Just navState
                        showStatus StatusInfo "Navigation mode: Up/Down to navigate, F to fork, Enter/Esc to exit"
                    _ -> showStatus StatusWarning "No session or empty session to navigate"
        Vty.EvKey key mods
            | hasQueuedMessages && matchesEvent keymap EventNavigateUp (Vty.EvKey key mods) ->
                handleQueueNavigation (-1)
        Vty.EvKey key mods
            | hasQueuedMessages && matchesEvent keymap EventNavigateDown (Vty.EvKey key mods) ->
                handleQueueNavigation 1
        Vty.EvKey key mods
            | hasQueuedMessages && matchesEvent keymap EventDeleteItem (Vty.EvKey key mods) ->
                handleDeleteSelectedMessage
        Vty.EvKey Vty.KUp _ -> vScrollBy (viewportScroll ConversationViewWidget) (-1)
        Vty.EvKey Vty.KDown _ -> vScrollBy (viewportScroll ConversationViewWidget) 1
        Vty.EvKey Vty.KLeft _ -> hScrollBy (viewportScroll ConversationViewWidget) (-1)
        Vty.EvKey Vty.KRight _ -> hScrollBy (viewportScroll ConversationViewWidget) 1
        Vty.EvKey Vty.KPageUp _ -> vScrollPage (viewportScroll ConversationViewWidget) Up
        Vty.EvKey Vty.KPageDown _ -> vScrollPage (viewportScroll ConversationViewWidget) Down
        _ -> pure ()

{- | Handle session view scrolling.
| Handle session view scrolling.
-}
handleSessionViewEvent :: Tracer IO Trace -> Vty.Event -> KeyMapping -> EventM N TuiState ()
handleSessionViewEvent _tracer ev keymap =
    case ev of
        Vty.EvKey key mods
            | matchesEvent keymap EventEnterTurnNavigation (Vty.EvKey key mods) -> do
                mSession <- getFocusedSession
                case mSession of
                    Just session | not (null session.turns) -> do
                        let navState =
                                TurnNavigationState
                                    { _navSession = session
                                    , _navSelectedTurnIndex = 0
                                    , _navTotalTurns = length session.turns
                                    }
                        tuiUI . turnNavigation .= Just navState
                        showStatus StatusInfo "Navigation mode: Up/Down to navigate, F to fork, Enter/Esc to exit"
                    _ -> showStatus StatusWarning "No session or empty session to navigate"
        Vty.EvKey Vty.KUp _ -> vScrollBy (viewportScroll SessionViewWidget) (-1)
        Vty.EvKey Vty.KDown _ -> vScrollBy (viewportScroll SessionViewWidget) 1
        Vty.EvKey Vty.KLeft _ -> hScrollBy (viewportScroll SessionViewWidget) (-1)
        Vty.EvKey Vty.KRight _ -> hScrollBy (viewportScroll SessionViewWidget) 1
        Vty.EvKey Vty.KPageUp _ -> vScrollPage (viewportScroll SessionViewWidget) Up
        Vty.EvKey Vty.KPageDown _ -> vScrollPage (viewportScroll SessionViewWidget) Down
        _ -> pure ()

-- | Handle agent info scrolling.
handleAgentInfoEvent :: Vty.Event -> EventM N TuiState ()
handleAgentInfoEvent ev =
    case ev of
        Vty.EvKey Vty.KUp _ -> vScrollBy (viewportScroll AgentInfoWidget) (-1)
        Vty.EvKey Vty.KDown _ -> vScrollBy (viewportScroll AgentInfoWidget) 1
        Vty.EvKey Vty.KLeft _ -> hScrollBy (viewportScroll AgentInfoWidget) (-1)
        Vty.EvKey Vty.KRight _ -> hScrollBy (viewportScroll AgentInfoWidget) 1
        _ -> pure ()

-- | Handle queued message list events.
handleQueuedMessageListEvent :: Vty.Event -> KeyMapping -> EventM N TuiState ()
handleQueuedMessageListEvent ev keymap = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> pure ()
        Just conv -> do
            if conversationStatus conv /= ConversationStatus_Paused
                then pure ()
                else do
                    let convId = conversationId conv
                    buffered <- use (tuiUI . uiBufferedMessages)
                    case Map.lookup convId buffered of
                        Nothing -> pure ()
                        Just msgs -> do
                            let count = length msgs
                            case ev of
                                Vty.EvKey key mods
                                    | matchesEvent keymap EventNavigateUp (Vty.EvKey key mods) -> do
                                        current <- use (tuiUI . queuedMessagesFocus)
                                        let newIdx = case current of
                                                Nothing -> count - 1
                                                Just idx -> max 0 (idx - 1)
                                        tuiUI . queuedMessagesFocus .= Just newIdx
                                Vty.EvKey key mods
                                    | matchesEvent keymap EventNavigateDown (Vty.EvKey key mods) -> do
                                        current <- use (tuiUI . queuedMessagesFocus)
                                        let newIdx = case current of
                                                Nothing -> 0
                                                Just idx -> min (count - 1) (idx + 1)
                                        tuiUI . queuedMessagesFocus .= Just newIdx
                                Vty.EvKey key mods
                                    | matchesEvent keymap EventDeleteItem (Vty.EvKey key mods) ->
                                        handleDeleteSelectedMessage
                                Vty.EvKey key mods
                                    | matchesEvent keymap EventClearQueuedMessages (Vty.EvKey key mods) ->
                                        handleClearQueuedMessages
                                _ -> pure ()

-- | Handle attachment list events.
handleAttachmentListEvent :: Vty.Event -> KeyMapping -> EventM N TuiState ()
handleAttachmentListEvent ev keymap = do
    mConv <- getFocusedConversation
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
    mConv <- getFocusedConversation
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
    mConv <- getFocusedConversation
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

-- | Handle Ctrl+Shift+V for clipboard paste.
handleClipboardPaste :: Tracer IO Trace -> EventM N TuiState ()
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
                            mConv <- getFocusedConversation
                            case mConv of
                                Nothing -> showStatus StatusError "No conversation selected"
                                Just conv -> do
                                    let convId = conversationId conv
                                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId [attachment]
                                    let filename = maybe "unnamed" id attachment.mediaFilename
                                    showStatus StatusInfo $ "Attached from clipboard: " <> filename
                        AttachMultipleFiles attachments -> do
                            mConv <- getFocusedConversation
                            case mConv of
                                Nothing -> showStatus StatusError "No conversation selected"
                                Just conv -> do
                                    let convId = conversationId conv
                                    tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId attachments
                                    showStatus StatusInfo $ "Attached " <> Text.pack (show $ length attachments) <> " files from clipboard"

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Extract short identifier from ConversationId for debugging.
shortConvId :: ConversationId -> Text.Text
shortConvId (ConversationId uuid) = Text.take 8 $ Text.pack $ UUID.toString uuid

-------------------------------------------------------------------------------
-- Markdown Export Handlers
-------------------------------------------------------------------------------

-- | Get the currently focused session, if any.
getFocusedSession :: EventM N TuiState (Maybe Session)
getFocusedSession = do
    mConv <- use (tuiUI . conversationList . to listSelectedElement)
    case mConv of
        Just (_, conv) -> do
            case conversationSession conv of
                Just sess -> pure (Just sess)
                Nothing -> do
                    config <- use sessionConfig
                    liftIO $ SessionStore.readSession config.sessionStore (conversationId conv)
        Nothing -> do
            mSession <- use (tuiUI . sessionList . to listSelectedElement)
            pure $ fmap snd mSession

-- | Get the currently focused conversation, if any.
getFocusedConversation :: EventM N TuiState (Maybe Conversation)
getFocusedConversation = do
    mConv <- use (tuiUI . conversationList . to listSelectedElement)
    pure $ fmap snd mConv

-- | Get the conversation ID of the currently focused conversation.
getFocusedConversationId :: EventM N TuiState (Maybe ConversationId)
getFocusedConversationId = do
    mConv <- use (tuiUI . conversationList . to listSelectedElement)
    pure $ fmap (conversationId . snd) mConv

-- | Format a session as markdown with the specified order preference.
formatSessionMarkdown :: OrderPreference -> Session -> Text.Text
formatSessionMarkdown orderPref session =
    let opts =
            SessionPrintOptions
                { sessionPrintFile = ""
                , showToolCallResults = ShownFull
                , showToolCallArguments = ShownFull
                , nTurns = Nothing
                , repeatSystemPrompt = False
                , repeatTools = False
                , orderPreference = orderPref
                , noFunnyStamp = True
                }
     in formatSessionAsMarkdown opts session

-- | Handle Ctrl+p: Dump the currently focused session to a markdown file.
handleDumpSessionToMarkdown :: EventM N TuiState ()
handleDumpSessionToMarkdown = do
    mSession <- getFocusedSession
    mConvId <- getFocusedConversationId
    case (mSession, mConvId) of
        (Just session, Just (ConversationId cid)) -> do
            let markdown = formatSessionMarkdown Chronological session
                fileName = "conv." <> show cid <.> "md"
            liftIO $ TextIO.writeFile fileName markdown
            showStatus StatusInfo $ "Exported to " <> Text.pack fileName
        (Just session, Nothing) -> do
            let markdown = formatSessionMarkdown Chronological session
                fileName = "sess." <> show session.sessionId <.> "md"
            liftIO $ TextIO.writeFile fileName markdown
            showStatus StatusInfo $ "Exported to " <> Text.pack fileName
        _ -> showStatus StatusWarning "No session or conversation selected"

-- | Handle Ctrl+t or Ctrl+r: Display the currently focused session with an external viewer.
handleViewSessionWithExternalViewer :: OrderPreference -> EventM N TuiState ()
handleViewSessionWithExternalViewer orderPref = do
    mViewer <- liftIO $ lookupEnv "AGENT_MD_VIEWER"
    case mViewer of
        Just viewerCmd -> do
            mSession <- getFocusedSession
            mConvId <- getFocusedConversationId
            case (mSession, mConvId) of
                (Just session, Just convId) -> do
                    let markdown = formatSessionMarkdown orderPref session
                    tempFile <- liftIO $ writeSystemTempFile "session-view-" (Text.unpack markdown)
                    asyncHandle <- liftIO $ async $ do
                        result <- readProcessWithExitCode viewerCmd [tempFile] ""
                        case result of
                            (ExitFailure code, _, err) ->
                                hPutStrLn stderr $ "AGENT_MD_VIEWER failed with exit code " ++ show code ++ ": " ++ err
                            _ -> pure ()
                    let task = Viewer asyncHandle convId session.sessionId
                    tuiUI . auxiliaryTasks %= (task :)
                    showStatus StatusInfo $ "Opening with " <> Text.pack viewerCmd
                (Just _, Nothing) -> showStatus StatusWarning "No conversation selected"
                (Nothing, _) -> showStatus StatusWarning "No session selected"
        Nothing -> showStatus StatusWarning "AGENT_MD_VIEWER not set"

-------------------------------------------------------------------------------
-- Focus Management
-------------------------------------------------------------------------------

-- | Get the corresponding Tab for a WidgetName.
widgetToTab :: WidgetName -> Maybe Tab
widgetToTab AgentListWidget = Just AgentsTab
widgetToTab ConversationListWidget = Just ChatsTab
widgetToTab SessionsListWidget = Just HistoryTab
widgetToTab _ = Nothing

-- | Update the current tab based on the focused widget.
updateTabFromFocus :: EventM N TuiState ()
updateTabFromFocus = do
    mFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
    case mFocus >>= widgetToTab of
        Just tab -> do
            currentTab' <- use (tuiUI . currentTab)
            when (tab /= currentTab') $ do
                tuiUI . currentTab .= tab
                mCurrentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
                tuiUI . uiFocusRing .= buildFocusRingForTabPreserving tab mCurrentFocus
        Nothing -> pure ()

-- | Cycle focus forward through widgets.
cycleFocusForward :: EventM N TuiState ()
cycleFocusForward = do
    tuiUI . uiFocusRing %= focusNext
    tuiUI . zoomed .= False
    updateTabFromFocus

-- | Cycle focus backward through widgets.
cycleFocusBackward :: EventM N TuiState ()
cycleFocusBackward = do
    tuiUI . uiFocusRing %= focusPrev
    tuiUI . zoomed .= False
    updateTabFromFocus

-- | Toggle zoom mode for current widget.
toggleZoom :: EventM N TuiState ()
toggleZoom = tuiUI . zoomed %= not

-------------------------------------------------------------------------------
-- Application Event Handlers
-------------------------------------------------------------------------------

-- | Handle heartbeat - refresh UI state and auto-clear expired status messages.
handleHeartbeat :: EventM N TuiState ()
handleHeartbeat = do
    mSelectedConvId <- getFocusedConversationId
    coreRef <- use tuiCore
    coreState <- liftIO $ readTVarIO coreRef
    let convs = coreConversations coreState

    let sortedConvs = sortConversationsForNesting convs

    tuiUI . conversationList .= List.list ConversationListWidget (Vector.fromList sortedConvs) 1

    case mSelectedConvId of
        Just selectedConvId -> do
            let newConvs = Vector.fromList sortedConvs
            case Vector.findIndex (\c -> conversationId c == selectedConvId) newConvs of
                Just idx -> tuiUI . conversationList . listSelectedL .= Just idx
                Nothing -> pure ()
        Nothing -> pure ()
    selectedAgent <- use (tuiUI . selectedAgentInfo)
    case selectedAgent of
        Just agent -> refreshToolsForAgent agent
        Nothing -> pure ()
    buffered <- liftIO $ readTVarIO (coreBufferedMessages coreState)
    tuiUI . uiBufferedMessages .= buffered
    mStatus <- use (tuiUI . statusMessage)
    case mStatus of
        Just status -> do
            now <- liftIO getCurrentTime
            when (diffUTCTime now status.statusTimestamp > 5) $
                tuiUI . statusMessage .= Nothing
        Nothing -> pure ()
    cleanupAuxiliaryTasks

-- | Remove completed auxiliary tasks from the state.
cleanupAuxiliaryTasks :: EventM N TuiState ()
cleanupAuxiliaryTasks = do
    tasks <- use (tuiUI . auxiliaryTasks)
    activeTasks <- liftIO $ filterM isTaskActive tasks
    tuiUI . auxiliaryTasks .= activeTasks
  where
    isTaskActive :: AuxiliaryTask -> IO Bool
    isTaskActive (Viewer asyncHandle _ _) = do
        mResult <- poll asyncHandle
        pure $ case mResult of
            Nothing -> True
            Just _ -> False

-- | Show a status message with the given severity.
handleShowStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
handleShowStatus severity text = do
    now <- liftIO getCurrentTime
    tuiUI . statusMessage .= Just (StatusMessage text severity now)

-- | Clear the current status message.
handleClearStatus :: EventM N TuiState ()
handleClearStatus = tuiUI . statusMessage .= Nothing

-- | Handle new conversation event.
handleNewConversation :: ConversationId -> EventM N TuiState ()
handleNewConversation convId = do
    convs <- use (tuiUI . conversationList . to listElements)
    case Vector.findIndex (\c -> conversationId c == convId) convs of
        Just idx -> do
            tuiUI . conversationList . listSelectedL .= Just idx
            tuiUI . unreadConversations %= Set.delete convId
        Nothing -> pure ()

-- | Handle needs input update event.
handleConversationNeedsInput :: ConversationId -> EventM N TuiState ()
handleConversationNeedsInput convId = do
    updateConversationStatus convId ConversationStatus_WaitingForInput

-- | Update conversation status in core.
updateConversationStatus :: ConversationId -> ConversationStatus -> EventM N TuiState ()
updateConversationStatus convId newStatus = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c
            { coreConversations =
                map
                    ( \conv ->
                        if conversationId conv == convId
                            then conv{conversationStatus = newStatus}
                            else conv
                    )
                    (coreConversations c)
            }

-- | Handle conversation update event.
handleConversationUpdated :: ConversationId -> Session -> EventM N TuiState ()
handleConversationUpdated convId sess = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{coreConversations = updateConversationSession convId sess (coreConversations c)}
    selected <- use (tuiUI . conversationList . to listSelectedElement)
    case selected of
        Just (_, conv)
            | conversationId conv /= convId ->
                tuiUI . unreadConversations %= Set.insert convId
        _ -> pure ()
    handleHeartbeat

-- | Refresh tools for the given agent.
refreshToolsForAgent :: TuiAgent -> EventM N TuiState ()
refreshToolsForAgent agent = do
    tools <- liftIO $ readTVarIO (osNodeTools $ tuiNode agent)
    tuiUI . uiAgentTools %= updateAgentTools (tuiAgentId agent) tools
  where
    updateAgentTools :: AgentId -> [a] -> [(AgentId, [a])] -> [(AgentId, [a])]
    updateAgentTools aid newTools =
        ((aid, newTools) :) . filter ((/= aid) . fst)

-- | Handle F5 key: Refresh tools for selected agent.
handleRefreshTools :: EventM N TuiState ()
handleRefreshTools = do
    selected <- use (tuiUI . selectedAgentInfo)
    case selected of
        Just agent -> do
            tools <- liftIO $ readTVarIO (osNodeTools $ tuiNode agent)
            tuiUI . uiAgentTools %= updateAgentTools (tuiAgentId agent) tools
            showStatus StatusInfo $ "Refreshed " <> Text.pack (show $ length tools) <> " tools"
        Nothing -> showStatus StatusWarning "No agent selected"
  where
    updateAgentTools :: AgentId -> [a] -> [(AgentId, [a])] -> [(AgentId, [a])]
    updateAgentTools aid newTools =
        ((aid, newTools) :) . filter ((/= aid) . fst)

-------------------------------------------------------------------------------
-- Core State Manipulation
-------------------------------------------------------------------------------

-- | Append a conversation to the front of the core's conversation list.
appendConversation :: Conversation -> Core -> Core
appendConversation conv c = c{coreConversations = conv : coreConversations c}

-------------------------------------------------------------------------------
-- Conversation Management
-------------------------------------------------------------------------------

-- | Create a new conversation from the selected agent.
handleNewConversationFromEditor :: Tracer IO Trace -> EventM N TuiState ()
handleNewConversationFromEditor tracer = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, baseTuiAgent) -> do
            session <- liftIO (Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing)
            runConversation tracer baseTuiAgent session
        _ -> pure ()

-- | Continue a session restored from storage.
handleRestoredConversation :: Tracer IO Trace -> EventM N TuiState ()
handleRestoredConversation tracer = do
    mSession <- use (tuiUI . sessionList . to listSelectedElement)
    mAgent <- use (tuiUI . agentList . to listSelectedElement)
    case (,) <$> mSession <*> mAgent of
        Just ((_, session), (_, baseTuiAgent)) -> do
            runConversation tracer baseTuiAgent session
        _ -> pure ()

-- | Toggle pause/unpause for the currently selected conversation.
handleTogglePauseConversation :: EventM N TuiState ()
handleTogglePauseConversation = do
    mSelectedConv <- use (tuiUI . conversationList . to listSelectedElement)
    case mSelectedConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just (_, conv) -> do
            let convId = conversationId conv
            coreRef <- use tuiCore
            isPaused <- Set.member convId . corePausedConversations <$> liftIO (readTVarIO coreRef)
            if isPaused
                then do
                    liftIO $ atomically $ modifyTVar coreRef $ \c ->
                        c{corePausedConversations = Set.delete convId (corePausedConversations c)}
                    updateConversationStatus convId ConversationStatus_WaitingForInput
                    tuiUI . queuedMessagesFocus .= Nothing
                    showStatus StatusInfo $ "Unpaused: " <> conversationName conv
                else do
                    liftIO $ atomically $ modifyTVar coreRef $ \c ->
                        c{corePausedConversations = Set.insert convId (corePausedConversations c)}
                    updateConversationStatus convId ConversationStatus_Paused
                    showStatus StatusInfo $ "Paused: " <> conversationName conv

-- | Check if a conversation is currently paused.
isConversationPaused :: ConversationId -> Core -> Bool
isConversationPaused convId core = Set.member convId (corePausedConversations core)

-------------------------------------------------------------------------------
-- Queue Management
-------------------------------------------------------------------------------

-- | Clear all queued messages for the current conversation.
handleClearQueuedMessages :: EventM N TuiState ()
handleClearQueuedMessages = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            if conversationStatus conv /= ConversationStatus_Paused
                then showStatus StatusWarning "Can only clear queued messages when paused (Ctrl+E)"
                else do
                    let convId = conversationId conv
                    coreRef <- use tuiCore
                    core <- liftIO $ readTVarIO coreRef
                    liftIO $ atomically $ modifyTVar (coreBufferedMessages core) $ Map.insert convId []
                    tuiUI . uiBufferedMessages %= Map.insert convId []
                    tuiUI . queuedMessagesFocus .= Nothing
                    showStatus StatusInfo "All queued messages cleared"

-- | Delete the currently selected queued message.
handleDeleteSelectedMessage :: EventM N TuiState ()
handleDeleteSelectedMessage = do
    mConv <- getFocusedConversation
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
                                            liftIO $ atomically $ modifyTVar (coreBufferedMessages core) $ Map.insert convId newMsgs
                                            tuiUI . uiBufferedMessages %= Map.insert convId newMsgs
                                            let newIdx = if null newMsgs then Nothing else Just (min idx (length newMsgs - 1))
                                            tuiUI . queuedMessagesFocus .= newIdx
                                            showStatus StatusInfo "Message deleted"

-- | Delete an element at a specific index.
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = take idx xs ++ drop (idx + 1) xs

-- | Navigate through queued messages.
handleQueueNavigation :: Int -> EventM N TuiState ()
handleQueueNavigation direction = do
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
                    current <- use (tuiUI . queuedMessagesFocus)
                    let newIdx = case current of
                            Nothing -> if direction > 0 then 0 else count - 1
                            Just idx -> max 0 $ min (count - 1) (idx + direction)
                    tuiUI . queuedMessagesFocus .= Just newIdx

-------------------------------------------------------------------------------
-- Session Progress Callback
-------------------------------------------------------------------------------

-- | Build the progress callback for a conversation.
buildOnProgress :: ConversationId -> BChan AppEvent -> OnSessionProgress
buildOnProgress convId outChan progress = do
    case progress of
        SessionUpdated sess -> writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
        SessionCompleted sess -> writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
        SessionStarted sess -> writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
        SessionFailed sess _ -> writeBChan outChan (AppEvent_AgentStepProgrress convId sess)

-- | STM operation to read and clear buffered messages for a conversation.
readAndClearBufferedMessagesSTM :: ConversationId -> TVar (Map ConversationId [Text.Text]) -> STM [Text.Text]
readAndClearBufferedMessagesSTM convId bufferVar = do
    buffers <- readTVar bufferVar
    case Map.lookup convId buffers of
        Nothing -> pure []
        Just msgs -> do
            writeTVar bufferVar (Map.insert convId [] buffers)
            pure msgs

-- | Read and clear buffered messages for a conversation.
readAndClearBufferedMessages :: ConversationId -> Core -> IO (Maybe Text.Text)
readAndClearBufferedMessages convId core = do
    msgs <- atomically $ readAndClearBufferedMessagesSTM convId core.coreBufferedMessages
    pure $ case msgs of
        [] -> Nothing
        _ -> Just $ Text.unlines $ reverse msgs

-- | Add a message to the buffer for a conversation.
addBufferedMessage :: ConversationId -> Core -> Text.Text -> IO ()
addBufferedMessage convId core msg =
    atomically $ modifyTVar core.coreBufferedMessages $ Map.insertWith (\new old -> new ++ old) convId [msg]

-------------------------------------------------------------------------------
-- Subcall Conversation Management
-------------------------------------------------------------------------------

{- | Create a conversation entry for a sub-agent call.

This function creates a conversation entry in the TUI that represents a nested
agent invocation. Unlike regular conversations, the execution is handled by
runSubAgentWithEventEmission in OneShotTool.hs, which emits progress events
that update this conversation entry.

The conversation uses the SAME decoration/naming as regular conversations
to ensure visual consistency in the TUI. The tree branches (└─>, ├─, │)
provide the visual nesting indication, not the conversation name itself.
-}
createSubcallConversationEntry ::
    -- | The agent being invoked as a tool
    TuiAgent ->
    -- | The conversation ID for this subcall (pre-generated)
    ConversationId ->
    -- | Parent conversation ID (the caller)
    ConversationId ->
    -- | Nesting depth (1+ for subcalls)
    Int ->
    EventM N TuiState ()
createSubcallConversationEntry tuiAgent convId parentId depth = do
    -- Create the conversation entry
    -- Note: conversationThreadId is Nothing because the execution is handled by OneShotTool
    -- conversationChan is a dummy channel since we don't need to send messages to subcalls
    inChan <- liftIO $ newBChan 100

    -- Use the SAME decoration as regular conversations: "@" <> slug
    -- The tree branches in renderConversationForest provide the visual nesting
    let conv =
            Conversation
                { conversationId = convId
                , conversationAgent = tuiAgent
                , conversationThreadId = Nothing
                , conversationSession = Nothing
                , conversationName = "@" <> tuiSlug tuiAgent
                , conversationChan = inChan
                , conversationStatus = ConversationStatus_Active
                , conversationOnProgress = \_ -> pure () -- Progress comes via SubcallProgress events
                , conversationIsSubcall = True
                , conversationParentId = Just parentId
                , conversationSubcallDepth = depth
                }

    -- Add to core state
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ appendConversation conv

    -- Add to UI conversation list
    tuiUI . conversationList %= listInsert 0 conv
    -- Debug: show conversation created
    let convShort = shortConvId convId
    let parentShort = shortConvId parentId
    showStatus StatusInfo $ "Created subcall d=" <> Text.pack (show depth) <> " cid=" <> convShort <> " pid=" <> parentShort

-------------------------------------------------------------------------------
-- Run Conversation
-------------------------------------------------------------------------------

{- | Run a conversation with the given agent and session.

This function creates an agent with the World and EventQueue from the Core
state, enabling subcall conversations to be visible in the TUI.
-}
runConversation :: Tracer IO Trace -> TuiAgent -> Session -> EventM N TuiState ()
runConversation tracer baseTuiAgent session = do
    config <- use sessionConfig
    convId <- liftIO $ newConversationId
    outChan <- use eventChan
    inChan <- liftIO $ newBChan 100
    let notifyProgress = buildOnProgress convId outChan
    let node = tuiNode baseTuiAgent
    agent0 <- liftIO $ OneShot.nodeToAgent config.sessionStore Nothing convId (contramap OneShotTrace tracer) config.sessionApiKeys node
    agent1 <- liftIO $ agentEvaluateActiveTools (contramap (OneShotTrace . OneShot.mapProgressiveDisclosureTrace) tracer) (osNodeTools node) agent0
    coreRef <- use tuiCore

    -- Get the World and EventQueue from Core for subcall visibility
    coreState <- liftIO $ readTVarIO coreRef
    let mWorld = coreWorld coreState
    let mEventQueue = coreOSEventQueue coreState

    let notifyNeedInput = writeBChan outChan (AppEvent_AgentNeedsInput convId)

    -- Set the World and EventQueue on the agent for subcall visibility
    -- and initialize the call stack with a root entry
    let agentWithOS =
            agent1
                { ctxWorld = mWorld
                , ctxEventQueue = mEventQueue
                , ctxCallStack = [CallStackEntry "root" convId 0]
                }

    let a =
            agentWithOS
                { step = \sess -> do
                    let waitIfPaused = do
                            core <- readTVarIO coreRef
                            when (isConversationPaused convId core) $ do
                                threadDelay 200000
                                waitIfPaused
                    waitIfPaused
                    notifyProgress (SessionUpdated sess)
                    ret <- agentWithOS.step sess
                    case ret of
                        Stop _r -> pure $ AskUserPrompt (MissingUserPrompt True [])
                        _ -> pure ret
                , usrQuery = do
                    core <- readTVarIO coreRef
                    buffered <- readAndClearBufferedMessages convId core
                    case buffered of
                        Nothing -> notifyNeedInput >> readBChan inChan
                        Just buftxt -> pure (Just $ UserQuery buftxt [])
                }
    let tuiAgent = TuiAgent (tuiAgentId baseTuiAgent) (tuiTree baseTuiAgent) (tuiNode baseTuiAgent) (tuiSlug baseTuiAgent)
    threadId <- liftIO $ forkIO $ do
        notifyProgress (SessionStarted session)
        void $ Loop.run convId a session
        notifyProgress (SessionCompleted session)
    let conv =
            Conversation
                { conversationId = convId
                , conversationAgent = tuiAgent
                , conversationThreadId = Just threadId
                , conversationSession = Nothing
                , conversationName = "@" <> tuiSlug baseTuiAgent
                , conversationChan = inChan
                , conversationStatus = ConversationStatus_WaitingForInput
                , conversationOnProgress = notifyProgress
                , conversationIsSubcall = False
                , conversationParentId = Nothing
                , conversationSubcallDepth = 0
                }
    liftIO $ atomically $ modifyTVar coreRef $ appendConversation conv
    tuiUI . conversationList %= listInsert 0 conv

    switchToChatsAndFocusMessage

-- | Send a message in the current conversation.
handleSendMessage :: EventM N TuiState ()
handleSendMessage = do
    msgLines <- use (tuiUI . messageEditor . to getEditContents)
    let msgText = Text.strip $ Text.unlines msgLines
    mConv <- getFocusedConversation
    attachments <- case mConv of
        Just conv -> do
            atts <- use (tuiUI . attachedFiles)
            pure $ Map.findWithDefault [] (conversationId conv) atts
        Nothing -> pure []
    when (not (Text.null msgText) || not (null attachments)) $ do
        case mConv of
            Just conv -> do
                let convId = conversationId conv
                coreRef <- use tuiCore
                isPaused <- Set.member convId . corePausedConversations <$> liftIO (readTVarIO coreRef)
                if isPaused
                    then do
                        core <- liftIO $ readTVarIO coreRef
                        liftIO $ addBufferedMessage convId core msgText
                        tuiUI . uiBufferedMessages %= Map.insertWith (\new old -> old ++ new) convId [msgText]
                        tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing
                        showStatus StatusInfo "Message queued (conversation paused)"
                    else do
                        let chan = conversationChan conv
                        liftIO $ writeBChan chan (Just $ UserQuery msgText attachments)
                        tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing
                        tuiUI . attachedFiles %= Map.delete convId
                        tuiUI . selectedAttachmentIndex .= Nothing
                        updateConversationStatus convId ConversationStatus_Active
            Nothing -> showStatus StatusWarning "No conversation selected"

