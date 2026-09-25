{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Event handling for the TUI application.

This module is a thin dispatcher that imports and re-exports from submodules.

Phase 3b-ii (@todos/os-as-standalone-server.md@): dispatch matches on the
runner-shaped 'AppEvent' constructors (Types.Core), most of them bridged
from 'System.Agents.Host.Client.subscribeAll' in "System.Agents.TUI.Core".
Handlers that reach into the runner (tool refresh, fork-at-turn, quit) go
through 'System.Agents.Host.Client'.
-}
module System.Agents.TUI.Event (
    -- * Main Event Handler
    tui_appHandleEvent,

    -- * Sub-module re-exports

    -- Dialog
    module System.Agents.TUI.Event.Dialog,
    -- Navigation
    module System.Agents.TUI.Event.Navigation,
    -- Conversation
    module System.Agents.TUI.Event.Conversation,
    -- Draft (§5, D3)
    module System.Agents.TUI.Event.Draft,
    -- Pending calls (Phase 3c)
    module System.Agents.TUI.Event.Pending,
    -- Attachment
    module System.Agents.TUI.Event.Attachment,

    -- * Types
    Trace (..),

    -- * Utility Functions
    defaultHelpContent,
    initHelpContent,
    handleQuit,
    resetQuitConfirmation,
    showStatus,

    -- * Internal Handlers (for sub-modules)
    handleNormalEvent,
    handleHeartbeat,
    handleShowStatus,
    handleClearStatus,
    getFocusedSession,
    getFocusedConversation,
    getFocusedConversationId,
    handleAgentListEvent,
    handleSessionsListEvent,
    handleConversationListEvent,
    handleMessageEditorEvent,
    handleConversationViewEvent,
    handleSessionViewEvent,
    handleAgentInfoEvent,
    handleBufferListEvent,
    checkTripleNewlineTrigger,

    -- * Markdown Export
    handleDumpSessionToMarkdown,
    handleViewSessionWithExternalViewer,

    -- * Buffer Operations
    handleSaveBuffer,
    handleResumeBuffer,
    handleDeleteSelectedBuffer,
    handleClearAllBuffers,
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Focus (focusGetCurrent, focusSetCurrent)
import Brick.Widgets.Edit (editContentsL, getEditContents, handleEditorEvent)
import Brick.Widgets.List (handleListEvent, listSelectedElement, listSelectedL)
import qualified Brick.Widgets.List as List
import Control.Concurrent.Async (async, poll)
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Lens (to, use, (%=), (.=), (^.))
import Control.Monad (filterM, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Zipper as TextZipper
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import Prod.Tracer (Tracer (..))
import System.Agents.Base (ConversationId (..))
import qualified System.Agents.Host.Client as Client
import System.Agents.Protocol (AgentDescriptor (..), runnerErrorMessage)
import System.Agents.Session.Base (
    ControlMsg (StopRun),
    MailBody (Control),
    Priority (Interrupt),
    Session (..),
    SessionStatus (StatusRunning),
 )
import System.Agents.SessionStore (
    SessionMeta (..),
    SessionQuery (..),
    allSessionsQuery,
    conversationIdToSessionId,
 )
import System.Timeout (timeout)
import System.Agents.SessionPrint (
    OrderPreference (..),
    PrintVisibility (..),
    SessionPrintOptions (..),
    formatSessionAsMarkdown,
    nTurns,
    noFunnyStamp,
    orderPreference,
    repeatSystemPrompt,
    repeatTools,
    sessionPrintFile,
    showToolCallArguments,
    showToolCallResults,
 )
import System.Agents.TUI.Buffer (bufferContent, newBufferWithContent)
import System.Agents.TUI.Event.Attachment
import System.Agents.TUI.Event.Conversation
import System.Agents.TUI.Event.Dialog
import System.Agents.TUI.Event.Draft
import System.Agents.TUI.Event.Navigation hiding (handleForkAtTurn)
import System.Agents.TUI.Event.Pending
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
    HistorySessionEntry (..),
    RunnerMode (..),
    N,
    StatusMessage (..),
    StatusSeverity (..),
    Tab (..),
    TuiAgent (..),
    TuiState,
    TurnNavigationState (..),
    WidgetName (..),
    agentList,
    attachmentDialogState,
    auxiliaryTasks,
    bufferFocus,
    buffers,
    buildFocusRingForTab,
    conversationId,
    conversationList,
    conversationSession,
    coreClient,
    coreConversations,
    coreOwnedSessions,
    coreRunnerMode,
    eventChan,
    historyDirty,
    historySessionCache,
    keyMapping,
    messageEditor,
    navSelectedTurnIndex,
    navSession,
    navTotalTurns,
    quitConfirmationPending,
    selectedAgentInfo,
    sessionConfig,
    sessionInputConfig,
    sessionList,
    statusMessage,
    tuiCore,
    tuiSlug,
    tuiUI,
    turnNavigation,
    uiFocusRing,
    unreadConversations,
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

{- | Default keyboard shortcuts help content.
Generated from the default key mapping.
-}
defaultHelpContent :: [Text.Text]
defaultHelpContent = generateHelpContent defaultKeyMapping

-- | Alias for defaultHelpContent for backward compatibility.
initHelpContent :: [Text.Text]
initHelpContent = defaultHelpContent

-------------------------------------------------------------------------------
-- Quit Confirmation
-------------------------------------------------------------------------------

-- | Handle Ctrl+Q with confirmation.
handleQuit :: EventM N TuiState ()
handleQuit = do
    pending <- use (tuiUI . quitConfirmationPending)
    if pending
        then stopConversations >> halt
        else do
            tuiUI . quitConfirmationPending .= True
            showStatus StatusWarning "Are you sure? Press Ctrl+Q again to quit"

{- | Stop every session this TUI started (embedded mode) before quitting:
'System.Agents.Host.Client.sendMail' a @StopRun@ 'Control' envelope to
every owned session that is still 'StatusRunning' (G1: quit used to be
@killThread@ on each conversation's run thread; the runner owns the run
loop now, so quitting the TUI asks it to stop instead). Bounded in time
(as the old thread-teardown was) so a runner call stuck in a foreign call
cannot keep the TUI from quitting; no thread is killed here, only the
runner is asked to stop. An attached TUI (Phase 4, @--attach@,
'AttachedRunner') skips this entirely, per the spec's Design §4: the
server keeps running its sessions after the TUI detaches.
-}
stopConversations :: EventM N TuiState ()
stopConversations = do
    coreRef <- use tuiCore
    core <- liftIO $ readTVarIO coreRef
    let client = core ^. coreClient
        owned = Set.toList (core ^. coreOwnedSessions)
    when (core ^. coreRunnerMode == EmbeddedRunner) $
        liftIO $ void $ timeout conversationShutdownMicros $ mapM_ (stopSession client) owned
  where
    stopSession client sid = do
        result <- Client.getSession client sid
        case result of
            Right (_, meta) | meta.smStatus == StatusRunning ->
                void $ Client.sendMail client sid Interrupt (Control StopRun)
            _ -> pure ()

-- | How long to wait for owned sessions to stop when quitting.
conversationShutdownMicros :: Int
conversationShutdownMicros = 2000000

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
                Just navState -> handleTurnNavigationEventWithSubcalls tracer navState ev
                Nothing -> handleNormalEvent tracer ev

-- | Wrapper for turn navigation that provides the subcall handlers.
handleTurnNavigationEventWithSubcalls :: Tracer IO Trace -> TurnNavigationState -> BrickEvent N AppEvent -> EventM N TuiState ()
handleTurnNavigationEventWithSubcalls tracer navState ev = do
    keymap <- use keyMapping
    case ev of
        AppEvent AppEvent_Heartbeat -> handleHeartbeat
        AppEvent (AppEvent_SessionUpdated convId sess meta) -> handleConversationUpdated convId sess meta >> cacheHistorySession convId sess >> markHistoryDirty
        AppEvent (AppEvent_RunStarted convId _mode) -> updateConversationStatus convId ConversationStatus_Active >> setConversationPending convId []
        AppEvent (AppEvent_RunStopped convId status) -> handleRunStopped convId status
        AppEvent (AppEvent_SessionFailed convId err) -> showStatus StatusError err >> updateConversationStatus convId ConversationStatus_WaitingForInput
        AppEvent (AppEvent_CallsDeferred convId calls) -> handleCallsDeferred convId calls
        AppEvent (AppEvent_SessionCreated meta) -> handleSessionCreated meta >> markHistoryDirty
        AppEvent (AppEvent_SessionDeleted convId) -> handleSessionDeletedEvent convId
        AppEvent (AppEvent_AgentTrace _) -> pure ()
        AppEvent (AppEvent_ShowStatus severity text) -> handleShowStatus severity text
        AppEvent AppEvent_ClearStatus -> handleClearStatus
        AppEvent (AppEvent_SubcallStarted parentId subcallId slug depth) ->
            handleSubcallStarted tracer parentId subcallId slug depth
        AppEvent (AppEvent_SubcallCompleted subcallId result) ->
            handleSubcallCompleted subcallId result
        AppEvent (AppEvent_SubcallFailed subcallId err) ->
            handleSubcallFailed subcallId err
        AppEvent (AppEvent_ToolCallActivity activity) ->
            handleToolCallActivity activity
        AppEvent (AppEvent_AgentsRefreshed agents) -> handleAgentsRefreshed agents
        AppEvent (AppEvent_SessionsRefreshed sessions) -> handleSessionsRefreshed sessions
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

-------------------------------------------------------------------------------
-- Normal Event Handler (Non-Navigation)
-------------------------------------------------------------------------------

-- | Handle normal (non-navigation) events.
handleNormalEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
handleNormalEvent tracer ev = do
    keymap <- use keyMapping
    case ev of
        AppEvent AppEvent_Heartbeat -> handleHeartbeat
        AppEvent (AppEvent_SessionUpdated convId sess meta) -> handleConversationUpdated convId sess meta >> cacheHistorySession convId sess >> markHistoryDirty
        AppEvent (AppEvent_RunStarted convId _mode) -> updateConversationStatus convId ConversationStatus_Active >> setConversationPending convId []
        AppEvent (AppEvent_RunStopped convId status) -> handleRunStopped convId status
        AppEvent (AppEvent_SessionFailed convId err) -> showStatus StatusError err >> updateConversationStatus convId ConversationStatus_WaitingForInput
        AppEvent (AppEvent_CallsDeferred convId calls) -> handleCallsDeferred convId calls
        AppEvent (AppEvent_SessionCreated meta) -> handleSessionCreated meta
        AppEvent (AppEvent_SessionDeleted _convId) -> pure ()
        AppEvent (AppEvent_AgentTrace _) -> pure ()
        AppEvent (AppEvent_ShowStatus severity text) -> handleShowStatus severity text
        AppEvent AppEvent_ClearStatus -> handleClearStatus
        AppEvent (AppEvent_SubcallStarted parentId subcallId slug depth) ->
            handleSubcallStarted tracer parentId subcallId slug depth
        AppEvent (AppEvent_SubcallCompleted subcallId result) ->
            handleSubcallCompleted subcallId result
        AppEvent (AppEvent_SubcallFailed subcallId err) ->
            handleSubcallFailed subcallId err
        AppEvent (AppEvent_ToolCallActivity activity) ->
            handleToolCallActivity activity
        AppEvent (AppEvent_AgentsRefreshed agents) -> handleAgentsRefreshed agents
        AppEvent (AppEvent_SessionsRefreshed _sessions) -> pure ()
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
                handleSendOrAnswer
        VtyEvent vtyEv
            | matchesEvent keymap EventTogglePause vtyEv -> do
                resetQuitConfirmation
                handleTogglePauseConversation
        VtyEvent vtyEv
            | matchesEvent keymap EventInterruptRun vtyEv -> do
                resetQuitConfirmation
                handleInterruptConversation
        VtyEvent vtyEv
            | matchesEvent keymap EventCancelAttached vtyEv -> do
                resetQuitConfirmation
                handleCancelAttachedConversation
        VtyEvent vtyEv
            | matchesEvent keymap EventPauseRun vtyEv -> do
                resetQuitConfirmation
                handlePauseRunConversation
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
            | matchesEvent keymap EventClearDraft vtyEv -> do
                resetQuitConfirmation
                handleClearDraft
        VtyEvent vtyEv
            | matchesEvent keymap EventEditDraft vtyEv -> do
                resetQuitConfirmation
                handleEditDraft
        VtyEvent vtyEv
            | matchesEvent keymap EventSendDraftNow vtyEv -> do
                resetQuitConfirmation
                handleSendDraftNow
        VtyEvent vtyEv
            | matchesEvent keymap EventAnswerPending vtyEv -> do
                resetQuitConfirmation
                handleAnswerPending
        VtyEvent vtyEv
            | matchesEvent keymap EventSelectPending vtyEv -> do
                resetQuitConfirmation
                handleSelectPending
        VtyEvent vtyEv
            | matchesEvent keymap EventFailPending vtyEv -> do
                resetQuitConfirmation
                handleFailPending
        VtyEvent vtyEv
            | matchesEvent keymap EventSaveBuffer vtyEv -> do
                resetQuitConfirmation
                handleSaveBuffer
        VtyEvent vtyEv
            | matchesEvent keymap EventClearBuffers vtyEv -> do
                resetQuitConfirmation
                handleClearAllBuffers
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
                Just DraftPanelWidget -> pure ()
                Just AttachmentListWidget -> handleAttachmentListEvent vtyEv keymap
                Just BufferListWidget -> handleBufferListEvent vtyEv keymap
                _ -> pure ()
        _ -> pure ()

-------------------------------------------------------------------------------
-- Turn Navigation Event Handler (uses Navigation module but needs subcall handlers)
-------------------------------------------------------------------------------

{- | Fork a new conversation at the selected turn: 'Client.forkSession' with
the newest-first turn index navigation already tracks (matches
'System.Agents.Host.Runner.forkSession': @drop atTurn session.turns@, the
same index the old local-fork code used), keeping the source's agent, then
open the fork as a new conversation.
-}
handleForkAtTurn :: Tracer IO Trace -> TurnNavigationState -> EventM N TuiState ()
handleForkAtTurn tracer navState = do
    let session = navState ^. navSession
        selectedIdx = navState ^. navSelectedTurnIndex
        sourceSessionId = session.sessionId
    coreRef <- use tuiCore
    core <- liftIO $ readTVarIO coreRef
    result <- liftIO $ Client.forkSession (core ^. coreClient) sourceSessionId (Just selectedIdx) Nothing
    case result of
        Left err -> showStatus StatusError (runnerErrorMessage err)
        Right meta -> do
            markOwnedSession meta.smSessionId
            runConversation tracer (fromMaybe "" meta.smAgent) meta.smSessionId
            tuiUI . turnNavigation .= Nothing
            showStatus StatusInfo $ "Forked conversation at turn " <> Text.pack (show (selectedIdx + 1))

-------------------------------------------------------------------------------
-- Widget-Specific Event Handlers
-------------------------------------------------------------------------------

-- | Handle agent list navigation.
handleAgentListEvent :: Vty.Event -> EventM N TuiState ()
handleAgentListEvent ev = do
    zoom (tuiUI . agentList) $ handleListEvent ev
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, agent) -> tuiUI . selectedAgentInfo .= Just agent
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

{- | Handle sessions list navigation (History tab): after moving the
selection, fetch the newly selected session's full 'Session' via
'Client.getSession' if it is not already cached ('historySessionCache'),
so 'render_sessionView' has turns to show without a fetch per frame.
-}
handleSessionsListEvent :: Vty.Event -> EventM N TuiState ()
handleSessionsListEvent ev = do
    zoom (tuiUI . sessionList) $ handleListEvent ev
    mSelected <- use (tuiUI . sessionList . to listSelectedElement)
    case mSelected of
        Just (_, meta) -> ensureHistorySessionCached meta
        Nothing -> pure ()

{- | Fetch and cache a history session if it is not already cached. Marks
the entry 'HistoryLoading' before the fetch so 'render_sessionView' has a
placeholder to show while it is in flight, and 'HistoryFailed' with the
runner's error text (as 'reportRunnerResult' shows it elsewhere) if it
errors, instead of silently leaving the session unrenderable.
-}
ensureHistorySessionCached :: SessionMeta -> EventM N TuiState ()
ensureHistorySessionCached meta = do
    cache <- use (tuiUI . historySessionCache)
    unless (Map.member meta.smSessionId cache) $ do
        tuiUI . historySessionCache %= Map.insert meta.smSessionId HistoryLoading
        coreRef <- use tuiCore
        core <- liftIO $ readTVarIO coreRef
        result <- liftIO $ Client.getSession (core ^. coreClient) meta.smSessionId
        case result of
            Right (sess, _) -> tuiUI . historySessionCache %= Map.insert meta.smSessionId (HistoryLoaded sess)
            Left err -> tuiUI . historySessionCache %= Map.insert meta.smSessionId (HistoryFailed (runnerErrorMessage err))

{- | Cache a fresh 'Session' pushed by @session.updated@, keyed by its own
id (D4: the same UUID as its 'ConversationId', so the caller's convId is
redundant here).
-}
cacheHistorySession :: ConversationId -> Session -> EventM N TuiState ()
cacheHistorySession _convId sess =
    tuiUI . historySessionCache %= Map.insert sess.sessionId (HistoryLoaded sess)

-- | Mark the History tab's session list as needing a refresh on the next heartbeat.
markHistoryDirty :: EventM N TuiState ()
markHistoryDirty = tuiUI . historyDirty .= True

{- | A session was deleted (@session.deleted@): drop it from the open
conversation list if it is there, drop it from the history cache, and mark
the History tab dirty so it drops out of 'sessionList' on the next
heartbeat refresh.
-}
handleSessionDeletedEvent :: ConversationId -> EventM N TuiState ()
handleSessionDeletedEvent convId = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{_coreConversations = filter (\conv -> conversationId conv /= convId) (c ^. coreConversations)}
    tuiUI . historySessionCache %= Map.delete (conversationIdToSessionId convId)
    markHistoryDirty

{- | Apply a freshly listed History tab roster ('AppEvent_SessionsRefreshed',
'Client.listSessions'), keeping the current selection by session id when
possible.
-}
handleSessionsRefreshed :: [SessionMeta] -> EventM N TuiState ()
handleSessionsRefreshed sessions = do
    mPrevId <- fmap (smSessionId . snd) <$> use (tuiUI . sessionList . to listSelectedElement)
    tuiUI . sessionList .= List.list SessionsListWidget (Vector.fromList sessions) 1
    case mPrevId >>= \sid -> Vector.findIndex (\m -> m.smSessionId == sid) (Vector.fromList sessions) of
        Just idx -> tuiUI . sessionList . listSelectedL .= Just idx
        Nothing -> pure ()

-- | Handle message editor events.
handleMessageEditorEvent :: BrickEvent N AppEvent -> EventM N TuiState ()
handleMessageEditorEvent ev = do
    zoom (tuiUI . messageEditor) $ handleEditorEvent ev
    case ev of
        VtyEvent (Vty.EvKey Vty.KEnter mods)
            | Vty.MCtrl `elem` mods -> handleSendOrAnswer
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
        handleSendOrAnswer

-- | Handle conversation view scrolling and turn navigation.
handleConversationViewEvent :: Tracer IO Trace -> Vty.Event -> KeyMapping -> EventM N TuiState ()
handleConversationViewEvent _tracer ev keymap =
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
        Vty.EvKey Vty.KUp _ -> vScrollBy (viewportScroll ConversationViewWidget) (-1)
        Vty.EvKey Vty.KDown _ -> vScrollBy (viewportScroll ConversationViewWidget) 1
        Vty.EvKey Vty.KLeft _ -> hScrollBy (viewportScroll ConversationViewWidget) (-1)
        Vty.EvKey Vty.KRight _ -> hScrollBy (viewportScroll ConversationViewWidget) 1
        Vty.EvKey Vty.KPageUp _ -> vScrollPage (viewportScroll ConversationViewWidget) Up
        Vty.EvKey Vty.KPageDown _ -> vScrollPage (viewportScroll ConversationViewWidget) Down
        _ -> pure ()

{- | Handle session view scrolling and turn navigation (History tab): the
entered turn navigation reads from 'historySessionCache' -- the full
'Session' fetched for the selected 'SessionMeta' by 'ensureHistorySessionCached'.
-}
handleSessionViewEvent :: Tracer IO Trace -> Vty.Event -> KeyMapping -> EventM N TuiState ()
handleSessionViewEvent _tracer ev keymap =
    case ev of
        Vty.EvKey key mods
            | matchesEvent keymap EventEnterTurnNavigation (Vty.EvKey key mods) -> do
                mSelected <- use (tuiUI . sessionList . to listSelectedElement)
                case mSelected of
                    Nothing -> showStatus StatusWarning "No session selected"
                    Just (_, meta) -> do
                        cache <- use (tuiUI . historySessionCache)
                        case Map.lookup meta.smSessionId cache of
                            Just (HistoryLoaded session) | not (null session.turns) -> do
                                let navState =
                                        TurnNavigationState
                                            { _navSession = session
                                            , _navSelectedTurnIndex = length session.turns - 1
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

-- | Handle buffer list navigation and actions.
handleBufferListEvent :: Vty.Event -> KeyMapping -> EventM N TuiState ()
handleBufferListEvent ev keymap = do
    bufs <- use (tuiUI . buffers)
    let count = length bufs
    case ev of
        Vty.EvKey key mods
            | matchesEvent keymap EventResumeBuffer (Vty.EvKey key mods) ->
                handleResumeBuffer
        Vty.EvKey key mods
            | matchesEvent keymap EventDeleteItem (Vty.EvKey key mods) ->
                handleDeleteSelectedBuffer
        Vty.EvKey key mods
            | matchesEvent keymap EventClearBuffers (Vty.EvKey key mods) ->
                handleClearAllBuffers
        Vty.EvKey key mods
            | matchesEvent keymap EventNavigateUp (Vty.EvKey key mods) ->
                tuiUI . bufferFocus %= \mIdx -> case mIdx of
                    Nothing -> Just (count - 1)
                    Just idx -> Just (max 0 (idx - 1))
        Vty.EvKey key mods
            | matchesEvent keymap EventNavigateDown (Vty.EvKey key mods) ->
                tuiUI . bufferFocus %= \mIdx -> case mIdx of
                    Nothing -> Just 0
                    Just idx -> Just (min (count - 1) (idx + 1))
        _ -> pure ()

-------------------------------------------------------------------------------
-- Buffer Operations
-------------------------------------------------------------------------------

-- | Save current message editor content as a new buffer.
handleSaveBuffer :: EventM N TuiState ()
handleSaveBuffer = do
    msgLines <- use (tuiUI . messageEditor . to getEditContents)
    let msgText = Text.strip $ Text.unlines msgLines
    if Text.null msgText
        then showStatus StatusWarning "No content to save"
        else do
            buffer <- liftIO $ newBufferWithContent msgText
            tuiUI . buffers %= (buffer :) -- Add to front
            tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing
            showStatus StatusInfo "Saved to buffer"

-- | Resume editing selected buffer (swap with current editor content).
handleResumeBuffer :: EventM N TuiState ()
handleResumeBuffer = do
    mIdx <- use (tuiUI . bufferFocus)
    bufs <- use (tuiUI . buffers)
    case mIdx of
        Nothing -> showStatus StatusWarning "Select a buffer first"
        Just idx
            | idx < 0 || idx >= length bufs ->
                showStatus StatusError "Invalid buffer selection"
        Just idx -> do
            let selectedBuffer = bufs List.!! idx
            -- 1. Save current editor content to a new buffer (swap)
            currentLines <- use (tuiUI . messageEditor . to getEditContents)
            let currentText = Text.strip $ Text.unlines currentLines
            unless (Text.null currentText) $ do
                newBuffer <- liftIO $ newBufferWithContent currentText
                tuiUI . buffers %= (newBuffer :)
            -- 2. Load selected buffer content into editor
            tuiUI . messageEditor . editContentsL
                .= TextZipper.textZipper (Text.lines $ selectedBuffer ^. bufferContent) Nothing
            -- 3. Remove selected buffer from list
            tuiUI . buffers %= deleteAt idx
            tuiUI . bufferFocus .= Nothing
            -- 4. Refocus message editor for immediate editing
            tuiUI . uiFocusRing
                .= focusSetCurrent
                    MessageEditorWidget
                    (buildFocusRingForTab ChatsTab)
            showStatus StatusInfo "Buffer resumed"

-- | Delete the currently selected buffer.
handleDeleteSelectedBuffer :: EventM N TuiState ()
handleDeleteSelectedBuffer = do
    mIdx <- use (tuiUI . bufferFocus)
    case mIdx of
        Nothing -> showStatus StatusWarning "Select a buffer first"
        Just idx -> do
            bufs <- use (tuiUI . buffers)
            if idx < 0 || idx >= length bufs
                then pure ()
                else do
                    tuiUI . buffers %= deleteAt idx
                    let newCount = length bufs - 1
                    tuiUI . bufferFocus
                        .= if newCount == 0
                            then Nothing
                            else Just (min idx (newCount - 1))
                    showStatus StatusInfo "Buffer deleted"

-- | Clear all buffers.
handleClearAllBuffers :: EventM N TuiState ()
handleClearAllBuffers = do
    tuiUI . buffers .= []
    tuiUI . bufferFocus .= Nothing
    showStatus StatusInfo "All buffers cleared"

-- | Delete an element at a specific index.
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt 0 (_ : xs) = xs
deleteAt n (x : xs) = x : deleteAt (n - 1) xs

-------------------------------------------------------------------------------
-- Status Messages
-------------------------------------------------------------------------------

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Show a status message with the given severity.
handleShowStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
handleShowStatus severity text = do
    now <- liftIO getCurrentTime
    tuiUI . statusMessage .= Just (StatusMessage text severity now)

-- | Clear the current status message.
handleClearStatus :: EventM N TuiState ()
handleClearStatus = tuiUI . statusMessage .= Nothing

-------------------------------------------------------------------------------
-- Session Helpers
-------------------------------------------------------------------------------

{- | Get the currently focused session, if any: the currently selected
conversation's own (possibly not-yet-loaded) 'Session' (Chats tab). The
History tab's own selected session is read separately, from
'historySessionCache' -- see 'handleSessionViewEvent'.
-}
getFocusedSession :: EventM N TuiState (Maybe Session)
getFocusedSession = do
    mConv <- use (tuiUI . conversationList . to listSelectedElement)
    pure $ mConv >>= conversationSession . snd

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

-------------------------------------------------------------------------------
-- Application Event Handlers
-------------------------------------------------------------------------------

-- | Handle heartbeat - refresh UI state and auto-clear expired status messages.
handleHeartbeat :: EventM N TuiState ()
handleHeartbeat = do
    mSelectedConvId <- getFocusedConversationId
    coreRef <- use tuiCore
    coreState <- liftIO $ readTVarIO coreRef
    let convs = coreState ^. coreConversations

    let sortedConvs = sortConversationsForNesting convs

    tuiUI . conversationList .= List.list ConversationListWidget (Vector.fromList sortedConvs) 1

    case mSelectedConvId of
        Just selectedConvId -> do
            let newConvs = Vector.fromList sortedConvs
            case Vector.findIndex (\c -> conversationId c == selectedConvId) newConvs of
                Just idx -> tuiUI . conversationList . listSelectedL .= Just idx
                Nothing -> pure ()
        Nothing -> pure ()
    mStatus <- use (tuiUI . statusMessage)
    case mStatus of
        Just status -> do
            now <- liftIO getCurrentTime
            when (diffUTCTime now status.statusTimestamp > 5) $
                tuiUI . statusMessage .= Nothing
        Nothing -> pure ()
    refreshHistoryIfDirty
    cleanupAuxiliaryTasks

{- | Refresh the History tab's 'sessionList' via 'Client.listSessions' if
anything marked it dirty since the last heartbeat ('markHistoryDirty'):
a burst of @session.created@\/@session.updated@\/@session.deleted@ events
during a run only costs one 'Client.listSessions' round trip per
heartbeat, not one per event.
-}
refreshHistoryIfDirty :: EventM N TuiState ()
refreshHistoryIfDirty = do
    dirty <- use (tuiUI . historyDirty)
    when dirty $ do
        tuiUI . historyDirty .= False
        coreRef <- use tuiCore
        core <- liftIO $ readTVarIO coreRef
        let query = allSessionsQuery{sqLimit = Just historyListLimit}
        result <- liftIO $ Client.listSessions (core ^. coreClient) query
        case result of
            Right sessions -> handleSessionsRefreshed sessions
            Left _ -> pure ()

-- | Default page size for the History tab's 'Client.listSessions' refresh
-- (matches the HTTP @GET \/v1\/sessions@ default limit).
historyListLimit :: Int
historyListLimit = 50

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

{- | Handle F5 key: refresh the agent roster from the client
('Client.listAgents'; tools live on 'AgentDescriptor' now, not a live
OS-native TVar). Posts 'AppEvent_AgentsRefreshed', handled below like any
other runner-driven event.
-}
handleRefreshTools :: EventM N TuiState ()
handleRefreshTools = do
    coreRef <- use tuiCore
    core <- liftIO $ readTVarIO coreRef
    result <- liftIO $ Client.listAgents (core ^. coreClient)
    case result of
        Left err -> showStatus StatusError (runnerErrorMessage err)
        Right agents -> do
            chan <- use eventChan
            liftIO $ writeBChan chan (AppEvent_AgentsRefreshed agents)

-- | Apply a fresh agent roster ('AppEvent_AgentsRefreshed'), keeping the current selection by slug when possible.
handleAgentsRefreshed :: [AgentDescriptor] -> EventM N TuiState ()
handleAgentsRefreshed agents = do
    let tuiAgents = Vector.fromList (map TuiAgent agents)
    mPrevSlug <- fmap (tuiSlug . snd) <$> use (tuiUI . agentList . to listSelectedElement)
    tuiUI . agentList .= List.list AgentListWidget tuiAgents 1
    case mPrevSlug >>= \slug -> Vector.findIndex (\a -> tuiSlug a == slug) tuiAgents of
        Just idx -> tuiUI . agentList . listSelectedL .= Just idx
        Nothing -> pure ()
    selected <- use (tuiUI . agentList . to listSelectedElement)
    tuiUI . selectedAgentInfo .= fmap snd selected
    showStatus StatusInfo "Agent roster refreshed"

-------------------------------------------------------------------------------
-- Markdown Export Handlers
-------------------------------------------------------------------------------

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
                    -- Create a temp file with a unique name based on session id
                    let tempFile = "/tmp/session-view-" ++ show session.sessionId ++ ".md"
                    liftIO $ writeFile tempFile (Text.unpack markdown)
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
