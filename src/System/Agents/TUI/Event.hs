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
import Brick.Focus (focusGetCurrent, focusNext, focusPrev, focusSetCurrent)
import Brick.Widgets.Edit (editContentsL, getEditContents, handleEditorEvent)
import Brick.Widgets.List (handleListEvent, listElements, listInsert, listSelectedElement, listSelectedL)
import qualified Brick.Widgets.List as List
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, poll)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Lens (to, use, (%=), (.=), (^.))
import Control.Monad (filterM, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Zipper as TextZipper
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (writeSystemTempFile)
import System.Process (readProcessWithExitCode)

import System.Agents.AgentTree (OSAgentNode (..), osNodeTools)
import System.Agents.Base (AgentId (..), ConversationId (..), newConversationId)
import System.Agents.Combinators.ProgressiveDisclosure (agentEvaluateActiveTools)
import System.Agents.OneShot (mapProgressiveDisclosureTrace, nodeToAgent)
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.Runtime.Trace as Runtime
import System.Agents.Session.Base (Action (..), Agent (..), MissingUserPrompt (..), OnSessionProgress, Session (..), SessionProgress (..), UserQuery (..), newSessionId, newTurnId)
import qualified System.Agents.Session.Loop as Loop
import System.Agents.SessionPrint (OrderPreference (..), PrintVisibility (..), SessionPrintOptions (..), formatSessionAsMarkdown)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.TUI.Types (
    AppEvent (..),
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
    UIState (..),
    WidgetName (..),
    agentList,
    auxiliaryTasks,
    conversationId,
    conversationList,
    conversationName,
    conversationSession,
    conversationStatus,
    coreAgentTools,
    coreBufferedMessages,
    coreConversations,
    corePausedConversations,
    currentTab,
    eventChan,
    messageEditor,
    navSelectedTurnIndex,
    navSession,
    navTotalTurns,
    ongoingConversations,
    quitConfirmationPending,
    selectedAgentInfo,
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
    uiBufferedMessages,
    uiFocusRing,
    unreadConversations,
    updateConversationSession,
    zoomed,
 )

-- Import Tracer for creating a no-op tracer
import Prod.Tracer (Tracer (..), contramap)

data Trace
    = RuntimeTrace !Runtime.Trace
    | OneShotTrace !OneShot.Trace
    deriving (Show)

-------------------------------------------------------------------------------
-- Help Content
-------------------------------------------------------------------------------

-- | Default keyboard shortcuts help content.
defaultHelpContent :: [Text.Text]
defaultHelpContent =
    [ "Keyboard Shortcuts:"
    , ""
    , "Navigation:"
    , "  Tab          - Cycle focus forward through widgets"
    , "  Shift+Tab    - Cycle focus backward through widgets"
    , "  Ctrl+Z       - Toggle zoom mode (based on current tab)"
    , ""
    , "Zoom Mode (Ctrl+Z):"
    , "  Agents  tab  - Zooms the agent description panel"
    , "  Chats   tab  - Zooms the conversation panel"
    , "  History tab  - Zooms the session panel"
    , "  Help    tab  - Zooms the help content"
    , ""
    , "Tabs:"
    , "  Ctrl+[       - Switch to previous tab"
    , "  Ctrl+]       - Switch to next tab"
    , "  Enter        - Open selected conversation (from Conversations list)"
    , ""
    , "Conversations:"
    , "  Ctrl+N       - Start new conversation with selected agent"
    , "  Ctrl+C       - Continue restored session"
    , "  Meta+Enter   - Send message"
    , "  Ctrl+E       - Pause/unpause conversation"
    , ""
    , "Session Navigation & Forking:"
    , "  Enter        - Enter turn navigation mode (when on conversation)"
    , "  Up/Down      - Navigate between turns (in navigation mode)"
    , "  F            - Fork conversation at selected turn"
    , "  Enter/Esc    - Exit turn navigation mode"
    , ""
    , "Session Export:"
    , "  Ctrl+P       - Export session to markdown file"
    , "  Ctrl+T       - View session in external viewer (chronological)"
    , "  Ctrl+R       - View session in external viewer (reverse)"
    , ""
    , "Other:"
    , "  F5           - Refresh tools for selected agent"
    , "  Ctrl+Q       - Quit application (press twice to confirm)"
    ]

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
    tuiUI . currentTab %= nextTab

-- | Cycle to the previous tab backward.
cycleTabBackward :: EventM N TuiState ()
cycleTabBackward = do
    tuiUI . currentTab %= prevTab

-------------------------------------------------------------------------------
-- Navigation Helpers
-------------------------------------------------------------------------------

{- | Switch to the Chats tab and focus the message editor.
This is used when starting or opening a conversation.
-}
switchToChatsAndFocusMessage :: EventM N TuiState ()
switchToChatsAndFocusMessage = do
    -- Switch to Chats tab
    tuiUI . currentTab .= ChatsTab
    -- Focus the message editor
    tuiUI . uiFocusRing %= focusSetCurrent MessageEditorWidget
    -- Ensure zoom mode is off for better visibility
    tuiUI . zoomed .= False

-------------------------------------------------------------------------------
-- Quit Confirmation
-------------------------------------------------------------------------------

{- | Handle Ctrl+Q with confirmation.
First press shows confirmation message, second press actually quits.
-}
handleQuit :: EventM N TuiState ()
handleQuit = do
    pending <- use (tuiUI . quitConfirmationPending)
    if pending
        then halt
        else do
            tuiUI . quitConfirmationPending .= True
            showStatus StatusWarning "Are you sure? Press Ctrl+Q again to quit"

-- | Reset quit confirmation state (call when user performs other actions).
resetQuitConfirmation :: EventM N TuiState ()
resetQuitConfirmation = do
    tuiUI . quitConfirmationPending .= False

-------------------------------------------------------------------------------
-- Main Event Handler
-------------------------------------------------------------------------------

-- | Main event handler for the TUI application.
tui_appHandleEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
tui_appHandleEvent tracer ev = do
    -- Check if we're in turn navigation mode
    mNavState <- use (tuiUI . turnNavigation)
    case mNavState of
        Just navState -> handleTurnNavigationEvent tracer navState ev
        Nothing -> handleNormalEvent tracer ev

-- | Handle events when in turn navigation mode.
handleTurnNavigationEvent :: Tracer IO Trace -> TurnNavigationState -> BrickEvent N AppEvent -> EventM N TuiState ()
handleTurnNavigationEvent tracer navState ev =
    case ev of
        -- Application events pass through
        AppEvent AppEvent_Heartbeat ->
            handleHeartbeat
        AppEvent (AppEvent_AgentStepProgrress convId sess) ->
            handleConversationUpdated convId sess
        AppEvent (AppEvent_AgentNeedsInput convId) ->
            handleConversationNeedsInput convId
        AppEvent (AppEvent_AgentTrace _) ->
            pure ()
        AppEvent (AppEvent_ShowStatus severity text) ->
            handleShowStatus severity text
        AppEvent AppEvent_ClearStatus ->
            handleClearStatus
        -- Exit navigation mode with Enter
        VtyEvent (Vty.EvKey Vty.KEnter []) -> do
            tuiUI . turnNavigation .= Nothing
            showStatus StatusInfo "Exited turn navigation"
            resetQuitConfirmation

        -- Also allow Esc to exit
        VtyEvent (Vty.EvKey Vty.KEsc []) -> do
            tuiUI . turnNavigation .= Nothing
            showStatus StatusInfo "Exited turn navigation"
            resetQuitConfirmation

        -- Navigate up (to earlier turns)
        VtyEvent (Vty.EvKey Vty.KUp []) -> do
            let currentIdx = navState ^. navSelectedTurnIndex
                newIdx = max 0 (currentIdx - 1)
            tuiUI . turnNavigation .= Just (navState{_navSelectedTurnIndex = newIdx})

        -- Navigate down (to later turns)
        VtyEvent (Vty.EvKey Vty.KDown []) -> do
            let currentIdx = navState ^. navSelectedTurnIndex
                maxIdx = (navState ^. navTotalTurns) - 1
                newIdx = min maxIdx (currentIdx + 1)
            tuiUI . turnNavigation .= Just (navState{_navSelectedTurnIndex = newIdx})

        -- Fork at current turn
        VtyEvent (Vty.EvKey (Vty.KChar 'f') []) -> do
            handleForkAtTurn tracer navState

        -- Fork at current turn (uppercase F)
        VtyEvent (Vty.EvKey (Vty.KChar 'F') []) -> do
            handleForkAtTurn tracer navState

        -- Ignore other events in navigation mode
        _ -> pure ()

-- | Fork a new conversation at the selected turn.
handleForkAtTurn :: Tracer IO Trace -> TurnNavigationState -> EventM N TuiState ()
handleForkAtTurn tracer navState = do
    let session = navState ^. navSession
        selectedIdx = navState ^. navSelectedTurnIndex
        originalSessionId = session.sessionId

    -- Create forked session with dropping unwanted turns
    let turnsToKeep = drop selectedIdx session.turns

    -- Generate new session ID and turn ID
    newSessionId' <- liftIO newSessionId
    newTurnId' <- liftIO newTurnId

    let forkedSession =
            Session
                { turns = turnsToKeep
                , sessionId = newSessionId'
                , forkedFromSessionId = Just originalSessionId
                , turnId = newTurnId'
                }

    -- Create a new conversation with this session
    mAgent <- use (tuiUI . agentList . to listSelectedElement)
    case mAgent of
        Just (_, baseTuiAgent) -> do
            -- Start conversation with forked session
            runConversation tracer baseTuiAgent forkedSession

            -- Show status
            showStatus StatusInfo $
                "Forked at turn "
                    <> Text.pack (show selectedIdx)
                    <> " - New conversation @"
                    <> tuiSlug baseTuiAgent

            -- Exit navigation mode
            tuiUI . turnNavigation .= Nothing
        Nothing ->
            showStatus StatusError "No agent selected for forked conversation"

-- | Handle normal (non-navigation) events.
handleNormalEvent :: Tracer IO Trace -> BrickEvent N AppEvent -> EventM N TuiState ()
handleNormalEvent tracer ev = do
    case ev of
        -- Application events
        AppEvent AppEvent_Heartbeat ->
            handleHeartbeat
        AppEvent (AppEvent_AgentStepProgrress convId sess) ->
            handleConversationUpdated convId sess
        AppEvent (AppEvent_AgentNeedsInput convId) ->
            handleConversationNeedsInput convId
        AppEvent (AppEvent_AgentTrace _) ->
            pure () -- todo: handle traces made by the app
        AppEvent (AppEvent_ShowStatus severity text) ->
            handleShowStatus severity text
        AppEvent AppEvent_ClearStatus ->
            handleClearStatus
        -- Tab switching
        -- Note: Ctrl+[ sends KEsc in Vty (ASCII 27), not KChar '[' with MCtrl
        VtyEvent (Vty.EvKey Vty.KEsc [Vty.MCtrl]) -> do
            resetQuitConfirmation
            cycleTabBackward
        VtyEvent (Vty.EvKey (Vty.KChar ']') [Vty.MCtrl]) -> do
            resetQuitConfirmation
            cycleTabForward
        -- VTY events
        VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) ->
            handleQuit
        VtyEvent (Vty.EvKey (Vty.KChar '\t') _) -> do
            resetQuitConfirmation
            cycleFocusForward
        VtyEvent (Vty.EvKey Vty.KBackTab _) -> do
            resetQuitConfirmation
            cycleFocusBackward
        VtyEvent (Vty.EvKey (Vty.KFun 5) _) -> do
            resetQuitConfirmation
            handleRefreshTools
        VtyEvent (Vty.EvKey (Vty.KChar 'z') [Vty.MCtrl]) -> do
            resetQuitConfirmation
            toggleZoom
        VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) -> do
            resetQuitConfirmation
            handleNewConversationFromEditor tracer
        VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> do
            resetQuitConfirmation
            handleRestoredConversation tracer
        VtyEvent (Vty.EvKey Vty.KEnter [Vty.MMeta]) -> do
            resetQuitConfirmation
            handleSendMessage
        VtyEvent (Vty.EvKey (Vty.KChar 'e') [Vty.MCtrl]) -> do
            resetQuitConfirmation
            handleTogglePauseConversation
        VtyEvent (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) -> do
            resetQuitConfirmation
            handleDumpSessionToMarkdown
        VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MCtrl]) -> do
            resetQuitConfirmation
            handleViewSessionWithExternalViewer Chronological
        VtyEvent (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]) -> do
            resetQuitConfirmation
            handleViewSessionWithExternalViewer Antichronological
        -- Delegate to focused widget
        VtyEvent vtyEv -> do
            resetQuitConfirmation
            currentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
            case currentFocus of
                Just AgentListWidget ->
                    handleAgentListEvent vtyEv
                Just SessionsListWidget ->
                    handleSessionsListEvent vtyEv
                Just ConversationListWidget ->
                    handleConversationListEvent vtyEv
                Just MessageEditorWidget ->
                    handleMessageEditorEvent ev
                Just ConversationViewWidget ->
                    handleConversationViewEvent tracer vtyEv
                Just SessionViewWidget ->
                    handleSessionViewEvent tracer vtyEv
                Just AgentInfoWidget ->
                    handleAgentInfoEvent vtyEv
                _ ->
                    pure ()
        _ -> pure ()

-------------------------------------------------------------------------------
-- Widget-Specific Event Handlers
-------------------------------------------------------------------------------

-- | Handle agent list navigation.
handleAgentListEvent :: Vty.Event -> EventM N TuiState ()
handleAgentListEvent ev = do
    zoom (tuiUI . agentList) $ handleListEvent ev
    -- Update selected agent info when selection changes
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, agent) -> do
            tuiUI . selectedAgentInfo .= Just agent
            -- Refresh tools for the newly selected agent
            refreshToolsForAgent agent
        Nothing -> pure ()

-- | Handle conversation list navigation.
handleConversationListEvent :: Vty.Event -> EventM N TuiState ()
handleConversationListEvent ev =
    case ev of
        -- Enter key opens the selected conversation (switches to Chats tab and focuses message)
        Vty.EvKey Vty.KEnter [] -> do
            mSelected <- use (tuiUI . conversationList . to listSelectedElement)
            case mSelected of
                Just (_, conv) -> do
                    -- Switch to Chats tab and focus message editor
                    switchToChatsAndFocusMessage
                    -- Mark conversation as read
                    tuiUI . unreadConversations %= Set.delete (conversationId conv)
                Nothing -> pure ()
        -- Normal navigation
        _ -> do
            zoom (tuiUI . conversationList) $ handleListEvent ev
            -- Mark conversation as read when selected
            selected <- use (tuiUI . conversationList . to listSelectedElement)
            case selected of
                Just (_, conv) ->
                    tuiUI . unreadConversations %= Set.delete (conversationId conv)
                Nothing -> pure ()

-- | Handle sessions list navigation.
handleSessionsListEvent :: Vty.Event -> EventM N TuiState ()
handleSessionsListEvent ev = do
    zoom (tuiUI . sessionList) $ handleListEvent ev

-- | Handle message editor events.
handleMessageEditorEvent :: BrickEvent N AppEvent -> EventM N TuiState ()
handleMessageEditorEvent ev = do
    zoom (tuiUI . messageEditor) $ handleEditorEvent ev
    -- Check for special key combinations
    case ev of
        VtyEvent (Vty.EvKey Vty.KEnter mods)
            | Vty.MCtrl `elem` mods -> handleSendMessage
        _ -> pure ()

-- | Handle conversation view scrolling.
handleSessionViewEvent :: Tracer IO Trace -> Vty.Event -> EventM N TuiState ()
handleSessionViewEvent _tracer ev =
    case ev of
        -- Enter key enters turn navigation mode
        Vty.EvKey Vty.KEnter [] -> do
            mSession <- getFocusedSession
            case mSession of
                Just session | not (null session.turns) -> do
                    let navState =
                            TurnNavigationState
                                { _navSession = session
                                , _navSelectedTurnIndex = 0 -- Start at most recent (turns are anti-chronological)
                                , _navTotalTurns = length session.turns
                                }
                    tuiUI . turnNavigation .= Just navState
                    showStatus StatusInfo "Navigation mode: Up/Down to navigate, F to fork, Enter/Esc to exit"
                _ ->
                    showStatus StatusWarning "No session or empty session to navigate"
        -- Normal scrolling
        Vty.EvKey Vty.KUp _ ->
            vScrollBy (viewportScroll SessionViewWidget) (-1)
        Vty.EvKey Vty.KDown _ ->
            vScrollBy (viewportScroll SessionViewWidget) 1
        Vty.EvKey Vty.KLeft _ ->
            hScrollBy (viewportScroll SessionViewWidget) (-1)
        Vty.EvKey Vty.KRight _ ->
            hScrollBy (viewportScroll SessionViewWidget) 1
        Vty.EvKey Vty.KPageUp _ ->
            vScrollPage (viewportScroll SessionViewWidget) Up
        Vty.EvKey Vty.KPageDown _ ->
            vScrollPage (viewportScroll SessionViewWidget) Down
        _ -> pure ()

-- | Handle conversation view scrolling.
handleConversationViewEvent :: Tracer IO Trace -> Vty.Event -> EventM N TuiState ()
handleConversationViewEvent _tracer ev =
    case ev of
        -- Enter key enters turn navigation mode
        Vty.EvKey Vty.KEnter [] -> do
            mSession <- getFocusedSession
            case mSession of
                Just session | not (null session.turns) -> do
                    let navState =
                            TurnNavigationState
                                { _navSession = session
                                , _navSelectedTurnIndex = length session.turns - 1 -- Start at most recent
                                , _navTotalTurns = length session.turns
                                }
                    tuiUI . turnNavigation .= Just navState
                    showStatus StatusInfo "Navigation mode: Up/Down to navigate, F to fork, Enter/Esc to exit"
                _ ->
                    showStatus StatusWarning "No session or empty session to navigate"
        -- Normal scrolling
        Vty.EvKey Vty.KUp _ ->
            vScrollBy (viewportScroll ConversationViewWidget) (-1)
        Vty.EvKey Vty.KDown _ ->
            vScrollBy (viewportScroll ConversationViewWidget) 1
        Vty.EvKey Vty.KLeft _ ->
            hScrollBy (viewportScroll ConversationViewWidget) (-1)
        Vty.EvKey Vty.KRight _ ->
            hScrollBy (viewportScroll ConversationViewWidget) 1
        Vty.EvKey Vty.KPageUp _ ->
            vScrollPage (viewportScroll ConversationViewWidget) Up
        Vty.EvKey Vty.KPageDown _ ->
            vScrollPage (viewportScroll ConversationViewWidget) Down
        _ -> pure ()

-- | Handle agent info scrolling.
handleAgentInfoEvent :: Vty.Event -> EventM N TuiState ()
handleAgentInfoEvent ev =
    case ev of
        Vty.EvKey Vty.KUp _ ->
            vScrollBy (viewportScroll AgentInfoWidget) (-1)
        Vty.EvKey Vty.KDown _ ->
            vScrollBy (viewportScroll AgentInfoWidget) 1
        Vty.EvKey Vty.KLeft _ ->
            hScrollBy (viewportScroll AgentInfoWidget) (-1)
        Vty.EvKey Vty.KRight _ ->
            hScrollBy (viewportScroll AgentInfoWidget) 1
        _ -> pure ()

-------------------------------------------------------------------------------
-- Status Message Helpers
-------------------------------------------------------------------------------

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-------------------------------------------------------------------------------
-- Markdown Export Handlers
-------------------------------------------------------------------------------

-- | Get the currently focused session, if any.
getFocusedSession :: EventM N TuiState (Maybe Session)
getFocusedSession = do
    mConv <- use (tuiUI . conversationList . to listSelectedElement)
    case mConv of
        Just (_, conv) -> do
            -- First try to get the session from the conversation's cached session
            case conversationSession conv of
                Just sess -> pure (Just sess)
                Nothing -> do
                    -- If not cached, try to read from session store
                    config <- use sessionConfig
                    liftIO $ SessionStore.readSession config.sessionStore (conversationId conv)
        Nothing -> do
            -- Try session list
            mSession <- use (tuiUI . sessionList . to listSelectedElement)
            pure $ fmap snd mSession

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
                { sessionPrintFile = "" -- Not used for in-memory formatting
                , showToolCallResults = ShownFull
                , showToolCallArguments = ShownFull
                , nTurns = Nothing
                , repeatSystemPrompt = False
                , repeatTools = False
                , orderPreference = orderPref
                , noFunnyStamp = True -- Skip ASCII art in TUI exports for cleaner output
                }
     in formatSessionAsMarkdown opts session

{- | Handle Ctrl+m: Dump the currently focused session to a markdown file.
The file is named `<conversation-id>.md`.
-}
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
        _ -> do
            showStatus StatusWarning "No session or conversation selected"

{- | Handle Ctrl+t or Ctrl+r: Display the currently focused session with an external viewer.
Uses the AGENT_MD_VIEWER environment variable if set.
The viewer is executed asynchronously and tracked as an auxiliary task.

Ctrl+t uses Chronological order (oldest first).
Ctrl+r uses Antichronological order (newest first).
-}
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
                    -- Create a temporary file with the markdown content
                    tempFile <- liftIO $ writeSystemTempFile "session-view-" (Text.unpack markdown)
                    -- Spawn the viewer process asynchronously
                    viewerAsync <- liftIO $ async $ do
                        result <- readProcessWithExitCode viewerCmd [tempFile] ""
                        case result of
                            (ExitFailure code, _, err) ->
                                hPutStrLn stderr $ "AGENT_MD_VIEWER failed with exit code " ++ show code ++ ": " ++ err
                            _ -> pure ()
                    -- Track the async task
                    let task = Viewer viewerAsync convId session.sessionId
                    tuiUI . auxiliaryTasks %= (task :)
                    showStatus StatusInfo $ "Opening with " <> Text.pack viewerCmd
                (Just _, Nothing) -> do
                    showStatus StatusWarning "No conversation selected"
                (Nothing, _) -> do
                    showStatus StatusWarning "No session selected"
        Nothing -> do
            showStatus StatusWarning "AGENT_MD_VIEWER not set"

-------------------------------------------------------------------------------
-- Focus Management
-------------------------------------------------------------------------------

-- | Get the corresponding Tab for a WidgetName.
-- Returns Nothing if the widget doesn't have an associated tab.
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
        Just tab -> tuiUI . currentTab .= tab
        Nothing -> pure ()

-- | Cycle focus forward through widgets.
cycleFocusForward :: EventM N TuiState ()
cycleFocusForward = do
    tuiUI . uiFocusRing %= focusNext
    tuiUI . zoomed .= False
    -- Also update the active tab based on the new focus
    updateTabFromFocus

-- | Cycle focus backward through widgets.
cycleFocusBackward :: EventM N TuiState ()
cycleFocusBackward = do
    tuiUI . uiFocusRing %= focusPrev
    tuiUI . zoomed .= False
    -- Also update the active tab based on the new focus
    updateTabFromFocus

-- | Toggle zoom mode for current widget.
toggleZoom :: EventM N TuiState ()
toggleZoom = tuiUI . zoomed %= not

-------------------------------------------------------------------------------
-- Application Event Handlers
-------------------------------------------------------------------------------

{- | Handle heartbeat - refresh UI state and auto-clear expired status messages.
Preserves the currently selected conversation when refreshing the list.

During migration, this also:
1. Refreshes tools for the selected agent via RuntimeBridge
2. Synchronizes tool state between legacy Runtime and OS Core
-}
handleHeartbeat :: EventM N TuiState ()
handleHeartbeat = do
    -- Save the currently selected conversation ID before refreshing
    mSelectedConvId <- getFocusedConversationId

    -- Refresh conversations from core
    coreRef <- use tuiCore
    coreState <- liftIO $ readTVarIO coreRef
    let convs = coreConversations coreState
    tuiUI . conversationList .= List.list ConversationListWidget (Vector.fromList convs) 1

    -- Restore the selection if the conversation still exists
    case mSelectedConvId of
        Just selectedConvId -> do
            let newConvs = Vector.fromList convs
            case Vector.findIndex (\c -> conversationId c == selectedConvId) newConvs of
                Just idx -> tuiUI . conversationList . listSelectedL .= Just idx
                Nothing -> pure () -- Conversation was removed, keep no selection
        Nothing -> pure ()

    -- Refresh tools for the currently selected agent
    selectedAgent <- use (tuiUI . selectedAgentInfo)
    case selectedAgent of
        Just agent -> refreshToolsForAgent agent
        Nothing -> pure ()

    -- Sync buffered messages from Core to UIState for rendering
    buffered <- liftIO $ readTVarIO (coreBufferedMessages coreState)
    tuiUI . uiBufferedMessages .= buffered

    -- Auto-clear status messages after 5 seconds
    mStatus <- use (tuiUI . statusMessage)
    case mStatus of
        Just status -> do
            now <- liftIO getCurrentTime
            when (diffUTCTime now status.statusTimestamp > 5) $
                tuiUI . statusMessage .= Nothing
        Nothing -> pure ()

    -- Cleanup completed auxiliary tasks
    cleanupAuxiliaryTasks

-- | Remove completed auxiliary tasks from the state.
cleanupAuxiliaryTasks :: EventM N TuiState ()
cleanupAuxiliaryTasks = do
    tasks <- use (tuiUI . auxiliaryTasks)
    -- Filter out completed tasks (poll returns Just)
    activeTasks <- liftIO $ filterM isTaskActive tasks
    tuiUI . auxiliaryTasks .= activeTasks
  where
    isTaskActive :: AuxiliaryTask -> IO Bool
    isTaskActive (Viewer asyncHandle _ _) = do
        mResult <- poll asyncHandle
        pure $ case mResult of
            Nothing -> True -- Still running
            Just _ -> False -- Completed (success or failure)

-- | Show a status message with the given severity.
handleShowStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
handleShowStatus severity text = do
    now <- liftIO getCurrentTime
    tuiUI . statusMessage .= Just (StatusMessage text severity now)

-- | Clear the current status message.
handleClearStatus :: EventM N TuiState ()
handleClearStatus =
    tuiUI . statusMessage .= Nothing

-- | Handle new conversation event.
handleNewConversation :: ConversationId -> EventM N TuiState ()
handleNewConversation convId = do
    -- Find and select the new conversation
    convs <- use (tuiUI . conversationList . to listElements)
    case Vector.findIndex (\c -> conversationId c == convId) convs of
        Just idx -> do
            tuiUI . conversationList . listSelectedL .= Just idx
            tuiUI . unreadConversations %= Set.delete convId
        Nothing -> pure ()

-- | Handle needs input update event.
handleConversationNeedsInput :: ConversationId -> EventM N TuiState ()
handleConversationNeedsInput convId = do
    tuiUI . ongoingConversations %= Set.delete convId

    -- Also update the conversation status in core
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
    -- Updates the conversation's view of the Session
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{coreConversations = updateConversationSession convId sess (coreConversations c)}

    -- Mark as unread if not currently selected
    selected <- use (tuiUI . conversationList . to listSelectedElement)
    case selected of
        Just (_, conv)
            | conversationId conv /= convId ->
                tuiUI . unreadConversations %= Set.insert convId
        _ -> pure ()
    -- Refresh from core
    handleHeartbeat

{- | Refresh tools for the given agent.

This function reads tools from the OS-native TVar and updates the UI state.
-}
refreshToolsForAgent :: TuiAgent -> EventM N TuiState ()
refreshToolsForAgent agent = do
    -- Read tools from the OS-native TVar
    tools <- liftIO $ readTVarIO (osNodeTools $ tuiNode agent)
    tuiUI . coreAgentTools %= updateAgentTools (tuiAgentId agent) tools
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
            -- Read tools from the OS-native TVar
            tools <- liftIO $ readTVarIO (osNodeTools $ tuiNode agent)
            tuiUI . coreAgentTools %= updateAgentTools (tuiAgentId agent) tools
            showStatus StatusInfo $ "Refreshed " <> Text.pack (show $ length tools) <> " tools"
        Nothing -> showStatus StatusWarning "No agent selected"
  where
    updateAgentTools :: AgentId -> [a] -> [(AgentId, [a])] -> [(AgentId, [a])]
    updateAgentTools aid newTools =
        ((aid, newTools) :) . filter ((/= aid) . fst)

-------------------------------------------------------------------------------
-- Conversation Management
-------------------------------------------------------------------------------

-- | Create a new conversation from the selected agent.
handleNewConversationFromEditor :: Tracer IO Trace -> EventM N TuiState ()
handleNewConversationFromEditor tracer = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, baseTuiAgent) -> do
            session <- liftIO (Session [] <$> newSessionId <*> pure Nothing <*> newTurnId)
            runConversation tracer baseTuiAgent session
        _ ->
            pure ()

{- | Continue a session restored from storage.
This starts the agent loop with the restored session.
-}
handleRestoredConversation :: Tracer IO Trace -> EventM N TuiState ()
handleRestoredConversation tracer = do
    mSession <- use (tuiUI . sessionList . to listSelectedElement)
    mAgent <- use (tuiUI . agentList . to listSelectedElement)
    case (,) <$> mSession <*> mAgent of
        Just ((_, session), (_, baseTuiAgent)) -> do
            runConversation tracer baseTuiAgent session
        _ ->
            pure ()

{- | Toggle pause/unpause for the currently selected conversation.
Ctrl+E pauses/unpauses the conversation, blocking the step iteration.
-}
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
                    -- Unpause: remove from paused set and update status
                    liftIO $ atomically $ modifyTVar coreRef $ \c ->
                        c{corePausedConversations = Set.delete convId (corePausedConversations c)}
                    updateConversationStatus convId ConversationStatus_WaitingForInput
                    showStatus StatusInfo $ "Unpaused: " <> conversationName conv
                else do
                    -- Pause: add to paused set and update status
                    liftIO $ atomically $ modifyTVar coreRef $ \c ->
                        c{corePausedConversations = Set.insert convId (corePausedConversations c)}
                    updateConversationStatus convId ConversationStatus_Paused
                    showStatus StatusInfo $ "Paused: " <> conversationName conv

-- | Check if a conversation is currently paused.
isConversationPaused :: ConversationId -> Core -> Bool
isConversationPaused convId core = Set.member convId (corePausedConversations core)

{- | Build the progress callback for a conversation.
Combines the global session config with TUI-specific notification needs.
-}
buildOnProgress :: ConversationId -> BChan AppEvent -> OnSessionProgress
buildOnProgress convId outChan progress = do
    -- Notify TUI of progress updates
    case progress of
        SessionUpdated sess -> do
            writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
        SessionCompleted sess -> do
            writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
        SessionStarted sess -> do
            writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
        SessionFailed sess _ -> do
            writeBChan outChan (AppEvent_AgentStepProgrress convId sess)

{- | STM operation to read and clear buffered messages for a conversation.
Returns the messages that were buffered (if any).
-}
readAndClearBufferedMessagesSTM :: ConversationId -> TVar (Map ConversationId [Text.Text]) -> STM [Text.Text]
readAndClearBufferedMessagesSTM convId bufferVar = do
    buffers <- readTVar bufferVar
    case Map.lookup convId buffers of
        Nothing -> pure []
        Just msgs -> do
            writeTVar bufferVar (Map.insert convId [] buffers)
            pure msgs

{- | Read and clear buffered messages for a conversation.
Returns the concatenated messages (if any) that were buffered.
-}
readAndClearBufferedMessages :: ConversationId -> Core -> IO (Maybe Text.Text)
readAndClearBufferedMessages convId core = do
    msgs <- atomically $ readAndClearBufferedMessagesSTM convId core.coreBufferedMessages
    pure $ case msgs of
        [] -> Nothing
        _ -> Just $ Text.unlines $ reverse msgs -- Reverse to maintain order (oldest first)

-- | Add a message to the buffer for a conversation.
addBufferedMessage :: ConversationId -> Core -> Text.Text -> IO ()
addBufferedMessage convId core msg =
    atomically $ modifyTVar core.coreBufferedMessages $ Map.insertWith (\new old -> new ++ old) convId [msg]

runConversation :: Tracer IO Trace -> TuiAgent -> Session -> EventM N TuiState ()
runConversation tracer baseTuiAgent session = do
    -- Get session configuration (includes API keys)
    config <- use sessionConfig

    -- Generate conversation ID
    convId <- liftIO $ newConversationId

    outChan <- use eventChan
    inChan <- liftIO $ newBChan 100

    -- Build the progress callback
    let notifyProgress = buildOnProgress convId outChan

    let node = tuiNode baseTuiAgent

    -- Create the agent with the progress callback
    -- Use the agent's OSAgentNode through the TuiAgent
    -- Pass API keys from the session config
    agent0 <- liftIO $ nodeToAgent config.sessionStore Nothing convId (contramap OneShotTrace tracer) config.sessionApiKeys node
    agent1 <- liftIO $ agentEvaluateActiveTools (contramap (OneShotTrace . mapProgressiveDisclosureTrace) tracer) (osNodeTools node) agent0

    -- Get reference to core state for pause checking and message buffering
    coreRef <- use tuiCore

    let notifyNeedInput = writeBChan outChan (AppEvent_AgentNeedsInput convId)
    let a =
            agent1
                { step = \sess -> do
                    -- Check if conversation is paused and block until unpaused
                    let waitIfPaused = do
                            core <- readTVarIO coreRef
                            when (isConversationPaused convId core) $ do
                                threadDelay 200000 -- Check every 200ms
                                waitIfPaused
                    waitIfPaused

                    notifyProgress (SessionUpdated sess)
                    ret <- agent1.step sess
                    case ret of
                        Stop _r ->
                            -- smoll hack to reuse the naive step from nodeToAgent
                            pure $ AskUserPrompt (MissingUserPrompt True [])
                        _ -> pure ret
                , usrQuery = do
                    -- First check for buffered messages
                    core <- readTVarIO coreRef
                    buffered <- readAndClearBufferedMessages convId core
                    case buffered of
                        Nothing -> notifyNeedInput >> readBChan inChan
                        Just buftxt -> pure (Just $ UserQuery buftxt)
                }

    -- \* wrap in Conversation
    let tuiAgent = TuiAgent (tuiAgentId baseTuiAgent) (tuiTree baseTuiAgent) (tuiNode baseTuiAgent) (tuiSlug baseTuiAgent)
    threadId <- liftIO $ forkIO $ do
        notifyProgress (SessionStarted session)
        -- Loop.run now requires convId as first parameter
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
                }

    -- Add to core
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{coreConversations = conv : coreConversations c}
    -- Update UI
    tuiUI . conversationList %= listInsert 0 conv
    tuiUI . conversationList . listSelectedL .= Just 0
    -- Switch to Chats tab and focus the message editor
    switchToChatsAndFocusMessage

{- | Send a message in the current conversation.
Messages are now buffered if the conversation is being processed by the agent,
allowing users to "interrupt" or provide additional context while the agent
is executing tool calls.
-}
handleSendMessage :: EventM N TuiState ()
handleSendMessage = do
    -- Get message text
    msgLines <- use (tuiUI . messageEditor . to getEditContents)
    let msgText = Text.strip $ Text.unlines msgLines

    -- Only send non-empty messages
    when (not $ Text.null msgText) $ do
        selected <- use (tuiUI . conversationList . to listSelectedElement)
        case selected of
            Just (_idx, conv) -> do
                -- Get core reference for message buffering
                coreRef <- use tuiCore
                core <- liftIO $ readTVarIO coreRef

                -- Check if conversation is already being processed
                ongoing <- use (tuiUI . ongoingConversations)
                let isOngoing = Set.member (conversationId conv) ongoing

                if isOngoing
                    then do
                        -- Buffer the message - agent will pick it up when collecting tool responses
                        liftIO $ addBufferedMessage (conversationId conv) core msgText
                        showStatus StatusInfo "Message buffered - will be sent with tool responses"
                    else do
                        -- Conversation is waiting for input - send directly via channel
                        liftIO $ writeBChan conv.conversationChan (Just $ UserQuery msgText)
                        -- Mark as ongoing since we're starting agent processing
                        tuiUI . ongoingConversations %= Set.insert (conversationId conv)

                -- Always clear the editor - user can type more messages
                tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing
            Nothing -> pure ()

-------------------------------------------------------------------------------
-- Help Content Initialization
-------------------------------------------------------------------------------

-- | Initialize help content in UIState.
initHelpContent :: UIState -> UIState
initHelpContent uiState = uiState{_helpContent = defaultHelpContent}

