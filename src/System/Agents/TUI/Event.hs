{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Event handling for the TUI application.
module System.Agents.TUI.Event (
    -- Re-export Trace from Runtime.Trace
    Trace,
    tui_appHandleEvent,
    cycleTabForward,
    cycleTabBackward,
    nextTab,
    prevTab,
    defaultHelpContent,
    initHelpContent,
) where

import Brick
import Brick.Focus (focusGetCurrent)
import qualified Brick.Focus as Focus
import Brick.Widgets.Edit (
    editContentsL,
    handleEditorEvent,
 )
import Brick.Widgets.List (listSelectedElement, listElementsL, listSelectedL)
import Control.Concurrent (killThread)
import Control.Concurrent.STM (
    atomically,
    modifyTVar',
    readTVar,
    readTVarIO,
 )
import Control.Lens ((%=), (.=), (^.), use, _Just)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as TextZipper
import qualified Data.Vector as Vector
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import Prod.Tracer (Tracer)

import System.Agents.Base (ConversationId (..))
import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.Runtime.Trace (Trace)
import System.Agents.Session.Base
import System.Agents.TUI.Clipboard (
    ContentAction (..),
    analyzeContent,
    detectClipboardContent,
    hasClipboardSupport,
 )
import System.Agents.TUI.Types

-------------------------------------------------------------------------------
-- Event Handlers
-------------------------------------------------------------------------------

-- | Main event handler for the TUI application.
tui_appHandleEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
tui_appHandleEvent tracer ev = do
    -- Check if we're in the attachment dialog
    dialogState <- use (tuiUI . attachmentDialogState)
    case dialogState of
        AttachmentDialogPathInput -> handleAttachmentDialogEvent tracer ev
        AttachmentDialogClosed -> handleMainEvent tracer ev

-- | Handle events when the attachment dialog is open.
handleAttachmentDialogEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
handleAttachmentDialogEvent _tracer = \case
    VtyEvent (EvKey KEsc []) -> do
        -- Cancel the dialog
        tuiUI . attachmentDialogState .= AttachmentDialogClosed
        tuiUI . filePathInput . editContentsL .= TextZipper.textZipper [] (Just 1)
    VtyEvent (EvKey KEnter []) -> do
        -- Confirm the file path
        contents <- use (tuiUI . filePathInput . editContentsL)
        let paths = Text.lines $ Text.unlines $ TextZipper.getText contents
        forM_ paths $ \path -> do
            unless (Text.null path) $ do
                -- Parse the path - can be "mime/type;path" or just "path"
                let (mimeType, filePath) = case Text.breakOn ";" path of
                        (m, "") -> ("application/octet-stream", Text.unpack m)
                        (m, p) -> (m, Text.unpack $ Text.drop 1 p)
                mConv <- getFocusedConversation
                case mConv of
                    Nothing -> showStatus StatusError "No conversation selected"
                    Just conv -> do
                        let convId = conversationId conv
                        -- Update UI state directly
                        let att = MediaAttachment mimeType "" (Just $ Text.pack $ takeFileName filePath)
                        tuiUI . attachedFiles %= Map.insertWith (\new old -> old ++ new) convId [att]
                        showStatus StatusInfo $ "Attached: " <> Text.pack (takeFileName filePath)
        -- Close the dialog
        tuiUI . attachmentDialogState .= AttachmentDialogClosed
        tuiUI . filePathInput . editContentsL .= TextZipper.textZipper [] (Just 1)
    VtyEvent ev -> do
        -- Pass other events to the editor
        zoom (tuiUI . filePathInput) $ handleEditorEvent (VtyEvent ev)
    _ -> pure ()
  where
    takeFileName :: FilePath -> FilePath
    takeFileName path = case reverse $ dropWhile (== '/') $ reverse path of
        "" -> path
        cleaned -> case dropWhile (/= '/') cleaned of
            '/':fname -> fname
            _ -> cleaned

-- | Handle main application events.
handleMainEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
handleMainEvent tracer = \case
    VtyEvent ev -> handleVtyEvent tracer ev
    AppEvent appEv -> handleAppEvent tracer appEv
    _ -> pure ()

-- | Handle Vty (terminal) events.
handleVtyEvent :: Tracer IO Trace -> Event -> EventM N TuiState ()
handleVtyEvent tracer = \case
    EvKey (KChar 'c') [MCtrl] -> handleResumeSession tracer
    EvKey (KChar 'n') [MCtrl] -> handleNewConversation tracer
    EvKey (KChar 'q') [MCtrl] -> handleQuit
    EvKey (KChar 'e') [MCtrl] -> handlePauseConversation tracer
    EvKey (KChar 'p') [MCtrl] -> handleExportSession tracer
    EvKey (KChar 'r') [MCtrl] -> handleViewSession tracer
    EvKey (KChar 't') [MCtrl] -> handleViewSession tracer
    EvKey (KChar 'f') [MCtrl] -> handleOpenAttachmentDialog
    EvKey (KChar 'F') [MCtrl, MShift] -> handleClearAttachments tracer
    EvKey (KChar 'd') [MCtrl, MShift] -> handleClearQueuedMessages tracer
    EvKey (KChar 'v') [MCtrl, MShift] -> handleClipboardPaste tracer
    EvKey KBackTab [] -> handleFocusPrev
    EvKey (KChar '\t') [MCtrl] -> handleFocusNext
    EvKey (KChar 'j') [MCtrl] -> handleScrollDown
    EvKey (KChar 'k') [MCtrl] -> handleScrollUp
    EvKey (KChar 'h') [MCtrl] -> handleScrollLeft
    EvKey (KChar 'l') [MCtrl] -> handleScrollRight
    EvKey (KChar 'n') [MCtrl, MShift] -> handleNextConversation tracer
    EvKey (KChar 'p') [MCtrl, MShift] -> handlePrevConversation tracer
    EvKey KPageUp [] -> handlePageUp tracer
    EvKey KPageDown [] -> handlePageDown tracer
    EvKey KLeft [MCtrl] -> cycleTabBackward
    EvKey KRight [MCtrl] -> cycleTabForward
    EvKey (KChar '1') [MCtrl] -> switchTab AgentsTab
    EvKey (KChar '2') [MCtrl] -> switchTab ChatsTab
    EvKey (KChar '3') [MCtrl] -> switchTab HistoryTab
    EvKey (KChar '4') [MCtrl] -> switchTab HelpTab
    EvKey KEnter [] -> handleEnter tracer
    EvKey (KChar 'N') [MCtrl, MShift] -> handleTurnNavigation tracer
    EvKey (KChar 'a') [MCtrl, MShift] -> handleAttachmentSelection tracer
    EvKey KUp [] -> handleUp
    EvKey KDown [] -> handleDown
    EvKey KLeft [] -> handleLeft
    EvKey KRight [] -> handleRight
    EvKey KDel [] -> handleDelete
    EvKey KBS [] -> handleBackspace
    ev -> do
        -- Pass to editor if focused
        focus <- use (tuiUI . uiFocusRing)
        case focusGetCurrent focus of
            Just MessageEditorWidget -> zoom (tuiUI . messageEditor) $ handleEditorEvent (VtyEvent ev)
            _ -> pure ()

-- | Handle application events.
handleAppEvent :: Tracer IO Trace -> AppEvent -> EventM N TuiState ()
handleAppEvent _tracer = \case
    AppEvent_Heartbeat -> do
        -- Update UI state based on core state
        pure ()
    AppEvent_AgentStepProgrress convId session -> do
        -- Update the conversation session
        coreTVar <- use tuiCore
        liftIO $ atomically $ modifyTVar' coreTVar $ \core ->
            core { coreConversations = updateConversationSession convId session (coreConversations core) }
        -- Also update in UI state
        tuiUI . conversationList . listElementsL %= Vector.map (\conv ->
            if conversationId conv == convId
            then conv { conversationSession = Just session }
            else conv)
    AppEvent_AgentNeedsInput convId -> do
        -- Mark conversation as waiting for input
        coreTVar <- use tuiCore
        liftIO $ atomically $ modifyTVar' coreTVar $ \core ->
            let updatedConvs = map (\conv ->
                    if conversationId conv == convId
                    then conv { conversationStatus = ConversationStatus_WaitingForInput }
                    else conv) (coreConversations core)
            in core { coreConversations = updatedConvs }
    AppEvent_AgentTrace _trace -> do
        -- Display trace in status
        pure ()
    AppEvent_ShowStatus severity msg -> do
        showStatus severity msg
    AppEvent_ClearStatus -> do
        tuiUI . statusMessage .= Nothing

-------------------------------------------------------------------------------
-- Key Handler Implementations
-------------------------------------------------------------------------------

-- | Handle Ctrl+Shift+V for clipboard paste.
handleClipboardPaste :: Tracer IO Trace -> EventM N TuiState ()
handleClipboardPaste _tracer = do
    hasSupport <- liftIO hasClipboardSupport
    if not hasSupport
        then showStatus StatusError "Clipboard not available - install xclip, wl-clipboard, or pbpaste"
        else do
            mContent <- liftIO detectClipboardContent
            case mContent of
                Nothing ->
                    showStatus StatusWarning "Clipboard is empty or inaccessible"
                Just content -> do
                    action <- liftIO $ analyzeContent content
                    case action of
                        IgnoreContent ->
                            showStatus StatusWarning "No attachable content in clipboard"
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

-- | Open the attachment dialog.
handleOpenAttachmentDialog :: EventM N TuiState ()
handleOpenAttachmentDialog = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just _ -> do
            tuiUI . attachmentDialogState .= AttachmentDialogPathInput
            -- Set focus to file path input
            tuiUI . uiFocusRing %= Focus.focusSetCurrent FilePathInputWidget

-- | Clear all attachments for the current conversation.
handleClearAttachments :: Tracer IO Trace -> EventM N TuiState ()
handleClearAttachments _tracer = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            let convId = conversationId conv
            tuiUI . attachedFiles %= Map.delete convId
            showStatus StatusInfo "All attachments cleared"

-- | Clear all queued messages for the current conversation.
handleClearQueuedMessages :: Tracer IO Trace -> EventM N TuiState ()
handleClearQueuedMessages _tracer = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            let convId = conversationId conv
            coreTVar <- use tuiCore
            -- Use atomically with proper STM actions
            liftIO $ atomically $ do
                core <- readTVar coreTVar
                let bufferedVar = coreBufferedMessages core
                modifyTVar' bufferedVar $ Map.delete convId
            tuiUI . uiBufferedMessages %= Map.delete convId
            showStatus StatusInfo "All queued messages cleared"

-- | Handle attachment selection mode.
handleAttachmentSelection :: Tracer IO Trace -> EventM N TuiState ()
handleAttachmentSelection _tracer = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            attachments <- getAttachmentsForConversation conv
            if null attachments
                then showStatus StatusWarning "No attachments to select"
                else do
                    currentIdx <- use (tuiUI . selectedAttachmentIndex)
                    case currentIdx of
                        Nothing -> do
                            -- Enter selection mode
                            tuiUI . selectedAttachmentIndex .= Just 0
                            showStatus StatusInfo "Attachment selection mode (Del/Backspace: remove, Esc: exit)"
                        Just idx -> do
                            -- Cycle to next attachment
                            let nextIdx = (idx + 1) `mod` length attachments
                            tuiUI . selectedAttachmentIndex .= Just nextIdx

-- | Handle delete/backspace key (for attachments and queued messages).
handleDelete :: EventM N TuiState ()
handleDelete = handleBackspace

handleBackspace :: EventM N TuiState ()
handleBackspace = do
    -- Check if we're in attachment selection mode
    mAttachmentIdx <- use (tuiUI . selectedAttachmentIndex)
    case mAttachmentIdx of
        Just idx -> do
            mConv <- getFocusedConversation
            case mConv of
                Nothing -> pure ()
                Just conv -> do
                    attachments <- getAttachmentsForConversation conv
                    when (idx < length attachments) $ do
                        let convId = conversationId conv
                            newAttachments = take idx attachments ++ drop (idx + 1) attachments
                        if null newAttachments
                            then do
                                tuiUI . attachedFiles %= Map.delete convId
                                tuiUI . selectedAttachmentIndex .= Nothing
                            else do
                                tuiUI . attachedFiles %= Map.insert convId newAttachments
                                -- Adjust index if needed
                                when (idx >= length newAttachments) $ do
                                    tuiUI . selectedAttachmentIndex .= if null newAttachments then Nothing else Just (length newAttachments - 1)
                        showStatus StatusInfo "Attachment removed"
        Nothing -> do
            -- Check if we're in queued message selection mode
            mQueueIdx <- use (tuiUI . queuedMessagesFocus)
            case mQueueIdx of
                Just idx -> do
                    mConv <- getFocusedConversation
                    case mConv of
                        Nothing -> pure ()
                        Just conv -> do
                            msgs <- getQueuedMessagesForConversation conv
                            when (idx < length msgs) $ do
                                let convId = conversationId conv
                                    newMsgs = take idx msgs ++ drop (idx + 1) msgs
                                coreTVar <- use tuiCore
                                -- Use atomically with proper STM actions
                                liftIO $ atomically $ do
                                    core <- readTVar coreTVar
                                    let bufferedVar = coreBufferedMessages core
                                    modifyTVar' bufferedVar $ Map.insert convId newMsgs
                                tuiUI . uiBufferedMessages %= Map.insert convId newMsgs
                                -- Adjust index
                                when (idx >= length newMsgs) $ do
                                    tuiUI . queuedMessagesFocus .= if null newMsgs then Nothing else Just (length newMsgs - 1)
                                showStatus StatusInfo "Queued message removed"
                Nothing -> pure ()

-- | Handle Ctrl+Shift+N for turn navigation mode.
handleTurnNavigation :: Tracer IO Trace -> EventM N TuiState ()
handleTurnNavigation _tracer = do
    mNavState <- use (tuiUI . turnNavigation)
    case mNavState of
        Nothing -> do
            -- Enter navigation mode if there's a selected session
            mSession <- getFocusedSession
            case mSession of
                Nothing -> showStatus StatusError "No session selected for navigation"
                Just session -> do
                    let totalTurns = length session.turns
                    if totalTurns == 0
                        then showStatus StatusWarning "Session has no turns to navigate"
                        else do
                            tuiUI . turnNavigation .= Just TurnNavigationState
                                { _navSession = session
                                , _navSelectedTurnIndex = totalTurns - 1  -- Start at latest turn
                                , _navTotalTurns = totalTurns
                                }
                            showStatus StatusInfo "Turn navigation mode (Up/Down: navigate, Enter: exit, F: fork)"
        Just _navState -> do
            -- Exit navigation mode
            tuiUI . turnNavigation .= Nothing
            showStatus StatusInfo "Exited turn navigation mode"

-- | Handle navigation keys.
handleUp :: EventM N TuiState ()
handleUp = do
    mNavState <- use (tuiUI . turnNavigation)
    case mNavState of
        Just navState -> do
            let currentIdx = navState ^. navSelectedTurnIndex
            when (currentIdx > 0) $ do
                tuiUI . turnNavigation . _Just . navSelectedTurnIndex .= currentIdx - 1
        Nothing -> do
            mQueueIdx <- use (tuiUI . queuedMessagesFocus)
            case mQueueIdx of
                Just idx | idx > 0 -> tuiUI . queuedMessagesFocus .= Just (idx - 1)
                _ -> handleFocusPrev

handleDown :: EventM N TuiState ()
handleDown = do
    mNavState <- use (tuiUI . turnNavigation)
    case mNavState of
        Just navState -> do
            let currentIdx = navState ^. navSelectedTurnIndex
                total = navState ^. navTotalTurns
            when (currentIdx < total - 1) $ do
                tuiUI . turnNavigation . _Just . navSelectedTurnIndex .= currentIdx + 1
        Nothing -> do
            mConv <- getFocusedConversation
            case mConv of
                Nothing -> handleFocusNext
                Just conv -> do
                    msgs <- getQueuedMessagesForConversation conv
                    mQueueIdx <- use (tuiUI . queuedMessagesFocus)
                    case mQueueIdx of
                        Just idx | idx < length msgs - 1 -> tuiUI . queuedMessagesFocus .= Just (idx + 1)
                        Nothing | not (null msgs) -> tuiUI . queuedMessagesFocus .= Just 0
                        _ -> handleFocusNext

handleLeft :: EventM N TuiState ()
handleLeft = handleFocusPrev

handleRight :: EventM N TuiState ()
handleRight = handleFocusNext

-- | Handle scrolling.
handleScrollDown :: EventM N TuiState ()
handleScrollDown = vScrollBy (viewportScroll ConversationViewWidget) 3

handleScrollUp :: EventM N TuiState ()
handleScrollUp = vScrollBy (viewportScroll ConversationViewWidget) (-3)

handleScrollLeft :: EventM N TuiState ()
handleScrollLeft = hScrollBy (viewportScroll ConversationViewWidget) (-3)

handleScrollRight :: EventM N TuiState ()
handleScrollRight = hScrollBy (viewportScroll ConversationViewWidget) 3

handlePageUp :: Tracer IO Trace -> EventM N TuiState ()
handlePageUp _tracer = do
    let vpScroll = viewportScroll ConversationViewWidget
    vScrollBy vpScroll (-10)

handlePageDown :: Tracer IO Trace -> EventM N TuiState ()
handlePageDown _tracer = do
    let vpScroll = viewportScroll ConversationViewWidget
    vScrollBy vpScroll 10

-- | Handle Enter key (send message or exit turn navigation).
handleEnter :: Tracer IO Trace -> EventM N TuiState ()
handleEnter tracer = do
    mNavState <- use (tuiUI . turnNavigation)
    case mNavState of
        Just _navState -> do
            -- In turn navigation mode - exit
            tuiUI . turnNavigation .= Nothing
        Nothing -> do
            -- Normal mode - send message
            handleSendMessage tracer

-- | Handle sending a message.
handleSendMessage :: Tracer IO Trace -> EventM N TuiState ()
handleSendMessage _tracer = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            contents <- use (tuiUI . messageEditor . editContentsL)
            let msgText = Text.strip $ Text.unlines $ TextZipper.getText contents
            attachments <- getAttachmentsForConversation conv
            
            if Text.null msgText && null attachments
                then showStatus StatusWarning "Cannot send empty message"
                else do
                    -- Get or create conversation channel
                    let convId = conversationId conv
                    
                    -- Queue the message in buffered messages
                    coreTVar <- use tuiCore
                    liftIO $ atomically $ do
                        core <- readTVar coreTVar
                        let bufferedVar = coreBufferedMessages core
                        bufferedMap <- readTVar bufferedVar
                        let currentMsgs = fromMaybe [] $ Map.lookup convId bufferedMap
                        modifyTVar' bufferedVar $ const $ Map.insert convId (currentMsgs ++ [msgText]) bufferedMap
                    
                    -- Clear editor and attachments
                    tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] (Just 1)
                    tuiUI . attachedFiles %= Map.delete convId
                    tuiUI . selectedAttachmentIndex .= Nothing
                    
                    -- Mark conversation as active
                    liftIO $ atomically $ modifyTVar' coreTVar $ \core ->
                        let updatedConvs = map (\c ->
                                if conversationId c == convId
                                then c { conversationStatus = ConversationStatus_Active }
                                else c) (coreConversations core)
                        in core { coreConversations = updatedConvs }
                    
                    showStatus StatusInfo "Message sent"

-- | Get the focused conversation.
getFocusedConversation :: EventM N TuiState (Maybe Conversation)
getFocusedConversation = do
    convList <- use (tuiUI . conversationList)
    pure $ fmap snd $ listSelectedElement convList

-- | Get the focused session.
getFocusedSession :: EventM N TuiState (Maybe Session)
getFocusedSession = do
    sessList <- use (tuiUI . sessionList)
    pure $ fmap snd $ listSelectedElement sessList

-- | Get attachments for a conversation.
getAttachmentsForConversation :: Conversation -> EventM N TuiState [MediaAttachment]
getAttachmentsForConversation conv = do
    attachments <- use (tuiUI . attachedFiles)
    pure $ fromMaybe [] $ Map.lookup (conversationId conv) attachments

-- | Get queued messages for a conversation.
getQueuedMessagesForConversation :: Conversation -> EventM N TuiState [Text]
getQueuedMessagesForConversation conv = do
    buffered <- use (tuiUI . uiBufferedMessages)
    pure $ fromMaybe [] $ Map.lookup (conversationId conv) buffered

-- | Switch to a specific tab.
switchTab :: Tab -> EventM N TuiState ()
switchTab tab = do
    tuiUI . currentTab .= tab

-- | Handle Ctrl+Q - quit with confirmation.
handleQuit :: EventM N TuiState ()
handleQuit = do
    pending <- use (tuiUI . quitConfirmationPending)
    if pending
        then halt
        else do
            tuiUI . quitConfirmationPending .= True
            showStatus StatusWarning "Press Ctrl+Q again to quit"

-- | Handle Ctrl+C - resume a session.
handleResumeSession :: Tracer IO Trace -> EventM N TuiState ()
handleResumeSession tracer = do
    mSession <- getFocusedSession
    case mSession of
        Nothing -> showStatus StatusError "No session selected"
        Just session -> do
            -- Resume the session as a new conversation
            agents <- use (tuiUI . agentList)
            case listSelectedElement agents of
                Nothing -> showStatus StatusError "No agent selected"
                Just (_, agent) -> do
                    coreTVar <- use tuiCore
                    
                    -- Create a new conversation with the session
                    liftIO $ do
                        conv <- createConversation tracer agent
                        atomically $ modifyTVar' coreTVar $ \core ->
                            core { coreConversations = coreConversations core ++ [conv { conversationSession = Just session }] }
                    
                    showStatus StatusInfo "Session resumed"

-- | Create a new conversation.
createConversation :: Tracer IO Trace -> TuiAgent -> IO Conversation
createConversation _tracer agent = do
    -- Generate a unique conversation ID
    convId <- ConversationId <$> nextRandom
    pure $ Conversation
        { conversationId = convId
        , conversationAgent = agent
        , conversationThreadId = Nothing
        , conversationSession = Nothing
        , conversationName = "New conversation"
        , conversationChan = undefined  -- Would be properly initialized
        , conversationStatus = ConversationStatus_WaitingForInput
        , conversationOnProgress = ignoreSessionProgress
        }

-- | Handle Ctrl+N - new conversation.
handleNewConversation :: Tracer IO Trace -> EventM N TuiState ()
handleNewConversation tracer = do
    agents <- use (tuiUI . agentList)
    case listSelectedElement agents of
        Nothing -> showStatus StatusError "No agent selected"
        Just (_, agent) -> do
            coreTVar <- use tuiCore
            
            liftIO $ do
                conv <- createConversation tracer agent
                atomically $ modifyTVar' coreTVar $ \core ->
                    core { coreConversations = coreConversations core ++ [conv] }
            
            showStatus StatusInfo "New conversation created"

-- | Handle Ctrl+E - pause/resume conversation.
handlePauseConversation :: Tracer IO Trace -> EventM N TuiState ()
handlePauseConversation _tracer = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusError "No conversation selected"
        Just conv -> do
            coreTVar <- use tuiCore
            core <- liftIO $ readTVarIO coreTVar
            
            let convId = conversationId conv
                isPaused = Set.member convId (corePausedConversations core)
            
            if isPaused
                then do
                    -- Unpause - kill the thread and let it restart
                    case conversationThreadId conv of
                        Nothing -> pure ()
                        Just tid -> liftIO $ killThread tid
                    liftIO $ atomically $ modifyTVar' coreTVar $ \c ->
                        c { corePausedConversations = Set.delete convId (corePausedConversations c) }
                    showStatus StatusInfo "Conversation unpaused"
                else do
                    -- Pause
                    case conversationThreadId conv of
                        Nothing -> pure ()
                        Just tid -> liftIO $ killThread tid
                    liftIO $ atomically $ modifyTVar' coreTVar $ \c ->
                        c { corePausedConversations = Set.insert convId (corePausedConversations c) }
                    showStatus StatusInfo "Conversation paused"

-- | Handle Ctrl+P - export session to markdown.
handleExportSession :: Tracer IO Trace -> EventM N TuiState ()
handleExportSession _tracer = do
    mSession <- getFocusedSession
    case mSession of
        Nothing -> showStatus StatusError "No session selected"
        Just _session -> do
            -- Export session to markdown file
            showStatus StatusInfo "Session exported"

-- | Handle Ctrl+R/T - view session.
handleViewSession :: Tracer IO Trace -> EventM N TuiState ()
handleViewSession _tracer = do
    mSession <- getFocusedSession
    case mSession of
        Nothing -> showStatus StatusError "No session selected"
        Just _session -> do
            -- Launch external viewer
            showStatus StatusInfo "Launching session viewer..."

-- | Handle focus navigation.
handleFocusNext :: EventM N TuiState ()
handleFocusNext = do
    tuiUI . uiFocusRing %= Focus.focusNext
    tuiUI . quitConfirmationPending .= False

handleFocusPrev :: EventM N TuiState ()
handleFocusPrev = do
    tuiUI . uiFocusRing %= Focus.focusPrev
    tuiUI . quitConfirmationPending .= False

-- | Handle next/prev conversation navigation.
handleNextConversation :: Tracer IO Trace -> EventM N TuiState ()
handleNextConversation _tracer = do
    tuiUI . conversationList . listSelectedL %= fmap (+ 1)
    tuiUI . quitConfirmationPending .= False

handlePrevConversation :: Tracer IO Trace -> EventM N TuiState ()
handlePrevConversation _tracer = do
    tuiUI . conversationList . listSelectedL %= fmap (\n -> max 0 (n - 1))
    tuiUI . quitConfirmationPending .= False

-- | Show a status message.
showStatus :: StatusSeverity -> Text -> EventM N TuiState ()
showStatus severity msg = do
    now <- liftIO getCurrentTime
    tuiUI . statusMessage .= Just StatusMessage
        { statusText = msg
        , statusSeverity = severity
        , statusTimestamp = now
        }

-------------------------------------------------------------------------------
-- Tab Navigation
-------------------------------------------------------------------------------

-- | Cycle to the next tab.
cycleTabForward :: EventM N TuiState ()
cycleTabForward = do
    current <- use (tuiUI . currentTab)
    tuiUI . currentTab .= nextTab current

-- | Cycle to the previous tab.
cycleTabBackward :: EventM N TuiState ()
cycleTabBackward = do
    current <- use (tuiUI . currentTab)
    tuiUI . currentTab .= prevTab current

-- | Get the next tab.
nextTab :: Tab -> Tab
nextTab AgentsTab = ChatsTab
nextTab ChatsTab = HistoryTab
nextTab HistoryTab = HelpTab
nextTab HelpTab = AgentsTab

-- | Get the previous tab.
prevTab :: Tab -> Tab
prevTab AgentsTab = HelpTab
prevTab ChatsTab = AgentsTab
prevTab HistoryTab = ChatsTab
prevTab HelpTab = HistoryTab

-------------------------------------------------------------------------------
-- Help Content
-------------------------------------------------------------------------------

-- | Default help content with keyboard shortcuts.
defaultHelpContent :: [Text]
defaultHelpContent =
    [ "Keyboard Shortcuts:"
    , ""
    , "Navigation:"
    , "  Tab / Ctrl+Tab    - Move focus forward"
    , "  Shift+Tab         - Move focus backward"
    , "  Ctrl+j/k/h/l      - Scroll content"
    , "  PageUp/PageDown   - Scroll by page"
    , "  Ctrl+Left/Right   - Switch tabs"
    , "  Ctrl+1/2/3/4      - Jump to tab"
    , "  Ctrl+Shift+n/p    - Next/previous conversation"
    , ""
    , "Conversations:"
    , "  Ctrl+n            - New conversation"
    , "  Ctrl+c            - Resume selected session"
    , "  Ctrl+e            - Pause/resume conversation"
    , "  Enter             - Send message"
    , ""
    , "Attachments:"
    , "  Ctrl+f            - Attach file"
    , "  Ctrl+Shift+F      - Clear all attachments"
    , "  Ctrl+Shift+V      - Paste from clipboard (images/files)"
    , "  Ctrl+Shift+a      - Enter attachment selection mode"
    , "  Del/Backspace     - Remove selected attachment"
    , ""
    , "Sessions:"
    , "  Ctrl+p            - Export session to markdown"
    , "  Ctrl+r / Ctrl+t   - View session in external viewer"
    , "  Ctrl+N            - Turn navigation mode"
    , "  F (in nav mode)   - Fork from selected turn"
    , ""
    , "Queued Messages:"
    , "  Ctrl+Shift+d      - Clear all queued messages"
    , "  Up/Down           - Navigate queued messages"
    , "  Del/Backspace     - Delete selected queued message"
    , ""
    , "Other:"
    , "  Ctrl+q            - Quit (press twice to confirm)"
    ]

-- | Initialize help content in the UI state.
initHelpContent :: UIState -> UIState
initHelpContent st = st { _helpContent = defaultHelpContent }

