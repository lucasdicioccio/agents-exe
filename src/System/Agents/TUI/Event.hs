{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Event handling for the TUI application.
module System.Agents.TUI.Event where

import Brick
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Focus (focusGetCurrent, focusNext, focusPrev, focusRingModify, focusSetCurrent)
import Brick.Widgets.Edit (editContentsL, getEditContents, handleEditorEvent)
import Brick.Widgets.List (handleListEvent, listElements, listInsert, listSelectedElement, listSelectedL)
import qualified Brick.Widgets.List as List
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, poll)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Lens (to, use, (%=), (.=))
import Control.Monad (filterM, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.CircularList as CList
import Data.List (find, findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
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

import System.Agents.AgentTree (AgentTree (..))
import System.Agents.Base (ConversationId (..), newConversationId)
import System.Agents.OneShot (runtimeToAgent)
import qualified System.Agents.Runtime as Runtime
import System.Agents.Session.Base (Action (..), Agent (..), MissingUserPrompt (..), OnSessionProgress, Session (..), SessionProgress (..), UserQuery (..), newSessionId, newTurnId)
import qualified System.Agents.Session.Loop as Loop
import System.Agents.SessionPrint (OrderPreference (..), PrintVisibility (..), SessionPrintOptions (..), formatSessionAsMarkdown)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.TUI.Types

-------------------------------------------------------------------------------
-- Main Event Handler
-------------------------------------------------------------------------------

-- | Main event handler for the TUI application.
tui_appHandleEvent :: BrickEvent N AppEvent -> EventM N TuiState ()
tui_appHandleEvent ev = do
    case ev of
        -- Application events
        AppEvent AppEvent_Heartbeat ->
            handleHeartbeat
        AppEvent (AppEvent_AgentStepProgrress convId sess) ->
            handleConversationUpdated convId sess
        AppEvent (AppEvent_AgentNeedsInput convId) ->
            handleConversationNeedsInput convId
        AppEvent (AppEvent_AgentTrace trace) ->
            handleAgentTrace trace
        AppEvent (AppEvent_ShowStatus severity text) ->
            handleShowStatus severity text
        AppEvent AppEvent_ClearStatus ->
            handleClearStatus
        -- Sub-agent session events
        AppEvent (AppEvent_SubAgentSessionStarted parentConvId childSession) ->
            handleSubAgentSessionStarted parentConvId childSession
        AppEvent (AppEvent_SubAgentSessionUpdated parentConvId childSession) ->
            handleSubAgentSessionUpdated parentConvId childSession
        AppEvent (AppEvent_SubAgentSessionCompleted parentConvId childSession) ->
            handleSubAgentSessionCompleted parentConvId childSession
        -- VTY events
        VtyEvent (Vty.EvKey Vty.KEsc _) ->
            halt
        VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) ->
            halt
        VtyEvent (Vty.EvKey (Vty.KChar '\t') _) ->
            cycleFocusForward
        VtyEvent (Vty.EvKey Vty.KBackTab _) ->
            cycleFocusBackward
        VtyEvent (Vty.EvKey (Vty.KFun 5) _) ->
            handleRefreshTools
        VtyEvent (Vty.EvKey (Vty.KChar 'z') [Vty.MCtrl]) ->
            toggleZoom
        VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) ->
            handleNewConversationFromEditor
        VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) ->
            handleRestoredConversation
        VtyEvent (Vty.EvKey Vty.KEnter [Vty.MMeta]) ->
            handleSendMessage
        VtyEvent (Vty.EvKey (Vty.KChar 'e') [Vty.MCtrl]) ->
            handleTogglePauseConversation
        VtyEvent (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) ->
            handleDumpSessionToMarkdown
        VtyEvent (Vty.EvKey (Vty.KChar 't') [Vty.MCtrl]) ->
            handleViewSessionWithExternalViewer Chronological
        VtyEvent (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]) ->
            handleViewSessionWithExternalViewer Antichronological
        -- Delegate to focused widget
        VtyEvent vtyEv -> do
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
                    handleConversationViewEvent vtyEv
                Just SessionViewWidget ->
                    handleSessionViewEvent vtyEv
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
        Nothing -> pure ()

-- | Handle conversation list navigation with tree support.
handleConversationListEvent :: Vty.Event -> EventM N TuiState ()
handleConversationListEvent ev =
    case ev of
        -- Tab to toggle expansion of selected conversation
        Vty.EvKey (Vty.KChar '\t') [] ->
            toggleSelectedConversation
        -- Enter to view selected conversation (focus conversation view)
        Vty.EvKey Vty.KEnter [] ->
            viewSelectedConversation
        -- Right arrow to expand
        Vty.EvKey Vty.KRight [] ->
            expandSelectedConversation
        -- Left arrow to collapse
        Vty.EvKey Vty.KLeft [] ->
            collapseSelectedConversation
        -- Up arrow to navigate up in the tree
        Vty.EvKey Vty.KUp [] ->
            navigateConversationUp
        -- Down arrow to navigate down in the tree
        Vty.EvKey Vty.KDown [] ->
            navigateConversationDown
        -- Regular list navigation (for backward compatibility)
        _ -> do
            zoom (tuiUI . conversationList) $ handleListEvent ev
            -- Update selected path when list selection changes
            selected <- use (tuiUI . conversationList . to listSelectedElement)
            case selected of
                Just (_, conv) -> do
                    tuiUI . selectedConversationPath .= [conversationId conv]
                    tuiUI . unreadConversations %= Set.delete (conversationId conv)
                Nothing -> pure ()

-- | Toggle expansion state of the selected conversation.
toggleSelectedConversation :: EventM N TuiState ()
toggleSelectedConversation = do
    mSelectedId <- getSelectedConversationId
    case mSelectedId of
        Nothing -> pure ()
        Just convId -> do
            expanded <- use (tuiUI . conversationTreeExpanded)
            if Set.member convId expanded
                then tuiUI . conversationTreeExpanded .= Set.delete convId expanded
                else tuiUI . conversationTreeExpanded .= Set.insert convId expanded

-- | Expand the selected conversation.
expandSelectedConversation :: EventM N TuiState ()
expandSelectedConversation = do
    mSelectedId <- getSelectedConversationId
    case mSelectedId of
        Nothing -> pure ()
        Just convId -> do
            tuiUI . conversationTreeExpanded %= Set.insert convId

-- | Collapse the selected conversation.
collapseSelectedConversation :: EventM N TuiState ()
collapseSelectedConversation = do
    mSelectedId <- getSelectedConversationId
    case mSelectedId of
        Nothing -> pure ()
        Just convId -> do
            tuiUI . conversationTreeExpanded %= Set.delete convId

-- | Navigate to the previous conversation in the tree.
navigateConversationUp :: EventM N TuiState ()
navigateConversationUp = do
    -- Get all visible conversations in tree order
    visibleConvIds <- getVisibleConversationsInOrder
    currentPath <- use (tuiUI . selectedConversationPath)

    case currentPath of
        [] -> pure ()  -- No selection
        (currentId : _) ->
            -- Find the current ID in the visible list and move to previous
            case findIndex (== currentId) visibleConvIds of
                Nothing -> pure ()
                Just 0 -> pure ()  -- Already at the top
                Just idx -> do
                    let prevId = visibleConvIds !! (idx - 1)
                    tuiUI . selectedConversationPath .= [prevId]
                    tuiUI . unreadConversations %= Set.delete prevId
                    -- Also update the list selection for consistency
                    updateListSelectionToConversation prevId

-- | Navigate to the next conversation in the tree.
navigateConversationDown :: EventM N TuiState ()
navigateConversationDown = do
    -- Get all visible conversations in tree order
    visibleConvIds <- getVisibleConversationsInOrder
    currentPath <- use (tuiUI . selectedConversationPath)

    case currentPath of
        [] ->
            -- No selection, select first if available
            case visibleConvIds of
                (firstId : _) -> do
                    tuiUI . selectedConversationPath .= [firstId]
                    tuiUI . unreadConversations %= Set.delete firstId
                    updateListSelectionToConversation firstId
                [] -> pure ()
        (currentId : _) ->
            -- Find the current ID in the visible list and move to next
            case findIndex (== currentId) visibleConvIds of
                Nothing -> pure ()
                Just idx ->
                    if idx >= length visibleConvIds - 1
                        then pure ()  -- Already at the bottom
                        else do
                            let nextId = visibleConvIds !! (idx + 1)
                            tuiUI . selectedConversationPath .= [nextId]
                            tuiUI . unreadConversations %= Set.delete nextId
                            updateListSelectionToConversation nextId

-- | Navigate to parent conversation.
navigateToParent :: EventM N TuiState ()
navigateToParent = do
    mSelectedId <- getSelectedConversationId
    coreRef <- use tuiCore
    core <- liftIO $ readTVarIO coreRef

    case mSelectedId of
        Nothing -> pure ()
        Just convId -> do
            -- Find the conversation and check if it has a parent
            case find ((== convId) . conversationId) (coreConversations core) of
                Nothing -> pure ()
                Just conv -> case conversationSession conv >>= parentConversationId of
                    Nothing -> pure ()  -- No parent
                    Just parentId -> do
                        -- Update selected path to parent
                        tuiUI . selectedConversationPath .= [parentId]
                        tuiUI . unreadConversations %= Set.delete parentId
                        updateListSelectionToConversation parentId

-- | Get all visible conversations in tree order (respecting expanded state).
getVisibleConversationsInOrder :: EventM N TuiState [ConversationId]
getVisibleConversationsInOrder = do
    coreRef <- use tuiCore
    core <- liftIO $ readTVarIO coreRef
    expanded <- use (tuiUI . conversationTreeExpanded)
    let tree = buildConversationTree (coreConversations core)
    pure $ collectVisibleConversations expanded tree

-- | Collect visible conversation IDs from the tree.
collectVisibleConversations :: Set ConversationId -> [ConversationNode] -> [ConversationId]
collectVisibleConversations expanded nodes = concatMap collectFromNode nodes
  where
    collectFromNode node =
        let convId = conversationId (nodeConversation node)
            children = if Set.member convId expanded
                       then collectVisibleConversations expanded (nodeChildren node)
                       else []
        in convId : children

-- | Update the list selection to match a specific conversation ID.
updateListSelectionToConversation :: ConversationId -> EventM N TuiState ()
updateListSelectionToConversation convId = do
    convs <- use (tuiUI . conversationList . to listElements)
    case Vector.findIndex (\c -> conversationId c == convId) convs of
        Just idx -> tuiUI . conversationList . listSelectedL .= Just idx
        Nothing -> pure ()

-- | Focus the conversation view widget.
viewSelectedConversation :: EventM N TuiState ()
viewSelectedConversation = do
    tuiUI . uiFocusRing %= focusSetCurrent ConversationViewWidget

-- | Get the currently selected conversation ID.
getSelectedConversationId :: EventM N TuiState (Maybe ConversationId)
getSelectedConversationId = do
    currentPath <- use (tuiUI . selectedConversationPath)
    pure $ listToMaybe currentPath

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
handleSessionViewEvent :: Vty.Event -> EventM N TuiState ()
handleSessionViewEvent ev =
    case ev of
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
handleConversationViewEvent :: Vty.Event -> EventM N TuiState ()
handleConversationViewEvent ev =
    case ev of
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
    mConvId <- getSelectedConversationId
    case mConvId of
        Just convId -> do
            -- First try to get the session from the conversation's cached session
            coreRef <- use tuiCore
            core <- liftIO $ readTVarIO coreRef
            case find ((== convId) . conversationId) (coreConversations core) of
                Just conv -> case conversationSession conv of
                    Just sess -> pure (Just sess)
                    Nothing -> do
                        -- If not cached, try to read from session store
                        config <- use sessionConfig
                        liftIO $ SessionStore.readSession config.sessionStore convId
                Nothing -> pure Nothing
        Nothing -> pure Nothing

-- | Get the conversation ID of the currently focused conversation.
getFocusedConversationId :: EventM N TuiState (Maybe ConversationId)
getFocusedConversationId = do
    getSelectedConversationId

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

-- | Cycle focus forward through widgets.
cycleFocusForward :: EventM N TuiState ()
cycleFocusForward = do
    tuiUI . uiFocusRing %= focusNext
    tuiUI . zoomed .= False

-- | Cycle focus backward through widgets.
cycleFocusBackward :: EventM N TuiState ()
cycleFocusBackward = do
    tuiUI . uiFocusRing %= focusPrev
    tuiUI . zoomed .= False

-- | Toggle zoom mode for current widget.
toggleZoom :: EventM N TuiState ()
toggleZoom = tuiUI . zoomed %= not

-------------------------------------------------------------------------------
-- Application Event Handlers
-------------------------------------------------------------------------------

{- | Handle heartbeat - refresh UI state and auto-clear expired status messages.
Preserves the currently selected conversation when refreshing the list.
-}
handleHeartbeat :: EventM N TuiState ()
handleHeartbeat = do
    -- Save the currently selected conversation ID before refreshing
    mSelectedConvId <- getSelectedConversationId

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

    -- Refresh tools.
    let itrees = fmap agentTree coreState.coreAgents
    agentTools <- liftIO $ traverse (\itree -> readTVarIO $ itree.agentRuntime.agentTools) itrees
    let toolz = zipWith (,) [itree.agentRuntime.agentId | itree <- itrees] agentTools
    tuiUI . coreAgentTools .= toolz

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

-- | Handle show status event.
handleShowStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
handleShowStatus severity text = do
    now <- liftIO getCurrentTime
    tuiUI . statusMessage .= Just (StatusMessage text severity now)

-- | Handle clear status event.
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
            tuiUI . selectedConversationPath .= [convId]
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
    selected <- getSelectedConversationId
    case selected of
        Just selId
            | selId /= convId ->
                tuiUI . unreadConversations %= Set.insert convId
        _ -> pure ()
    -- Refresh from core
    handleHeartbeat

{- | Handle agent trace events.
TODO: remove
-}
handleAgentTrace :: Runtime.Trace -> EventM N TuiState ()
handleAgentTrace _trace = do
    -- Currently a no-op, to be implemented
    pure ()

-- | Refresh tools for selected agent.
handleRefreshTools :: EventM N TuiState ()
handleRefreshTools = do
    selected <- use (tuiUI . selectedAgentInfo)
    case selected of
        Just agent ->
            liftIO $ void $ atomically $ agent.agentTree.agentRuntime.agentTriggerRefreshTools
        Nothing -> pure ()

-------------------------------------------------------------------------------
-- Sub-Agent Session Event Handlers
-------------------------------------------------------------------------------

-- | Handle sub-agent session started event.
handleSubAgentSessionStarted :: ConversationId -> Session -> EventM N TuiState ()
handleSubAgentSessionStarted parentConvId _childSession = do
    showStatus StatusInfo $ "Sub-agent started under conversation " <> Text.pack (show parentConvId)
    -- Auto-expand the parent conversation so the child is visible
    tuiUI . conversationTreeExpanded %= Set.insert parentConvId
    -- Refresh the conversation list to show the new session
    handleHeartbeat

-- | Handle sub-agent session updated event.
handleSubAgentSessionUpdated :: ConversationId -> Session -> EventM N TuiState ()
handleSubAgentSessionUpdated _parentConvId _childSession = do
    -- The session is stored in core, just refresh to show updates
    handleHeartbeat

-- | Handle sub-agent session completed event.
handleSubAgentSessionCompleted :: ConversationId -> Session -> EventM N TuiState ()
handleSubAgentSessionCompleted parentConvId _childSession = do
    showStatus StatusInfo $ "Sub-agent completed under conversation " <> Text.pack (show parentConvId)
    handleHeartbeat

-------------------------------------------------------------------------------
-- Conversation Management
-------------------------------------------------------------------------------

-- | Create a new root session with no parent.
mkRootSession :: IO Session
mkRootSession = do
    sessId <- newSessionId
    tId <- newTurnId
    pure $
        Session
            { turns = []
            , sessionId = sessId
            , forkedFromSessionId = Nothing
            , turnId = tId
            , parentSessionId = Nothing
            , parentConversationId = Nothing
            , parentAgentSlug = Nothing
            }

-- | Create a new conversation from the selected agent.
handleNewConversationFromEditor :: EventM N TuiState ()
handleNewConversationFromEditor = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, baseTuiAgent) -> do
            session <- liftIO mkRootSession
            runConversation baseTuiAgent session
        _ ->
            pure ()

{- | Continue a session restored from storage.
This starts the agent loop with the restored session.
-}
handleRestoredConversation :: EventM N TuiState ()
handleRestoredConversation = do
    mSession <- use (tuiUI . sessionList . to listSelectedElement)
    mAgent <- use (tuiUI . agentList . to listSelectedElement)
    case (,) <$> mSession <*> mAgent of
        Just ((_, session), (_, baseTuiAgent)) -> do
            runConversation baseTuiAgent session
        _ ->
            pure ()

{- | Toggle pause/unpause for the currently selected conversation.
Ctrl+E pauses/unpauses the conversation, blocking the step iteration.
-}
handleTogglePauseConversation :: EventM N TuiState ()
handleTogglePauseConversation = do
    mSelectedConvId <- getSelectedConversationId
    case mSelectedConvId of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just convId -> do
            coreRef <- use tuiCore
            isPaused <- Set.member convId . corePausedConversations <$> liftIO (readTVarIO coreRef)
            if isPaused
                then do
                    -- Unpause: remove from paused set and update status
                    liftIO $ atomically $ modifyTVar coreRef $ \c ->
                        c{corePausedConversations = Set.delete convId (corePausedConversations c)}
                    updateConversationStatus convId ConversationStatus_WaitingForInput
                    showStatus StatusInfo "Conversation unpaused"
                else do
                    -- Pause: add to paused set and update status
                    liftIO $ atomically $ modifyTVar coreRef $ \c ->
                        c{corePausedConversations = Set.insert convId (corePausedConversations c)}
                    updateConversationStatus convId ConversationStatus_Paused
                    showStatus StatusInfo "Conversation paused"

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

runConversation :: TuiAgent -> Session -> EventM N TuiState ()
runConversation baseTuiAgent session = do
    -- Get session configuration
    config <- use sessionConfig

    -- Generate conversation ID
    convId <- liftIO $ newConversationId

    outChan <- use eventChan
    inChan <- liftIO $ newBChan 100

    -- Build the progress callback
    let notifyProgress = buildOnProgress convId outChan

    -- Create the agent with the progress callback
    -- Note: runtimeToAgent now requires convId as a parameter
    agent0 <- liftIO $ runtimeToAgent config.sessionStore Nothing convId (baseTuiAgent.agentTree.agentRuntime)

    -- Get reference to core state for pause checking and message buffering
    coreRef <- use tuiCore

    let notifyNeedInput = writeBChan outChan (AppEvent_AgentNeedsInput convId)
    let a =
            agent0
                { step = \sess -> do
                    -- Check if conversation is paused and block until unpaused
                    let waitIfPaused = do
                            core <- readTVarIO coreRef
                            when (isConversationPaused convId core) $ do
                                threadDelay 200000 -- Check every 200ms
                                waitIfPaused
                    waitIfPaused

                    notifyProgress (SessionUpdated sess)
                    ret <- agent0.step sess
                    case ret of
                        Stop _r ->
                            -- smoll hack to reuse the naive step from runtimeToAgent
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
    let tuiAgent = TuiAgent a baseTuiAgent.agentTree
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
                , conversationName = "@" <> tuiAgent.agentTree.agentRuntime.agentSlug
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
    tuiUI . selectedConversationPath .= [convId]
    tuiUI . uiFocusRing %= focusRingModify (CList.insertR ConversationViewWidget)
    tuiUI . uiFocusRing %= focusSetCurrent MessageEditorWidget

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
        mConvId <- getSelectedConversationId
        case mConvId of
            Just convId -> do
                -- Get core reference for message buffering
                coreRef <- use tuiCore
                core <- liftIO $ readTVarIO coreRef

                -- Find the conversation
                let mConv = find ((== convId) . conversationId) (coreConversations core)
                case mConv of
                    Just conv -> do
                        -- Check if conversation is already being processed
                        ongoing <- use (tuiUI . ongoingConversations)
                        let isOngoing = Set.member convId ongoing

                        if isOngoing
                            then do
                                -- Buffer the message - agent will pick it up when collecting tool responses
                                liftIO $ addBufferedMessage convId core msgText
                                showStatus StatusInfo "Message buffered - will be sent with tool responses"
                            else do
                                -- Conversation is waiting for input - send directly via channel
                                liftIO $ writeBChan (conversationChan conv) (Just $ UserQuery msgText)
                                -- Mark as ongoing since we're starting agent processing
                                tuiUI . ongoingConversations %= Set.insert convId

                        -- Always clear the editor - user can type more messages
                        tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing
                    Nothing -> pure ()
            Nothing -> pure ()

