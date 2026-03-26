{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Event handling for the TUI application.

This module handles all user input and application events for the TUI.
During migration to the OS model, tool operations use the RuntimeBridge
which synchronizes tools between the legacy Runtime and OS Core.
-}
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
import System.Agents.OneShot (nodeToAgent)
import System.Agents.Runtime.Trace (SubAgentTrace (..), Trace (..))
import System.Agents.Session.Base (Action (..), Agent (..), MissingUserPrompt (..), OnSessionProgress, Session (..), SessionProgress (..), UserQuery (..), newSessionId, newTurnId)
import qualified System.Agents.Session.Loop as Loop
import System.Agents.SessionPrint (OrderPreference (..), PrintVisibility (..), SessionPrintOptions (..), formatSessionAsMarkdown)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.TUI.Types

-- Import Tracer for creating a no-op tracer
import Prod.Tracer (Tracer (..))

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
        -- Tree navigation shortcuts
        VtyEvent (Vty.EvKey Vty.KRight [Vty.MCtrl]) ->
            handleExpandConversation
        VtyEvent (Vty.EvKey Vty.KLeft [Vty.MCtrl]) ->
            handleCollapseConversation
        VtyEvent (Vty.EvKey (Vty.KChar 'e') [Vty.MCtrl, Vty.MShift]) ->
            handleExpandAllConversations
        VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl, Vty.MShift]) ->
            handleCollapseAllConversations
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
            -- Refresh tools for the newly selected agent
            refreshToolsForAgent agent
        Nothing -> pure ()

-- | Handle conversation list navigation.
handleConversationListEvent :: Vty.Event -> EventM N TuiState ()
handleConversationListEvent ev = do
    case ev of
        -- Enter key toggles expand/collapse for conversations with children
        Vty.EvKey Vty.KEnter [] ->
            handleToggleExpandConversation
        -- Ctrl+Down: jump to next sibling
        Vty.EvKey Vty.KDown [Vty.MCtrl] ->
            handleJumpToNextSibling
        -- Ctrl+Up: jump to previous sibling
        Vty.EvKey Vty.KUp [Vty.MCtrl] ->
            handleJumpToPreviousSibling
        -- Arrow keys for expand/collapse
        Vty.EvKey Vty.KRight [] ->
            handleExpandConversation
        Vty.EvKey Vty.KLeft [] ->
            handleCollapseConversation
        -- Otherwise handle normal list navigation
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
-- Tree Navigation Handlers
-------------------------------------------------------------------------------

-- | Toggle expand/collapse for the currently selected conversation.
handleToggleExpandConversation :: EventM N TuiState ()
handleToggleExpandConversation = do
    mSelected <- use (tuiUI . conversationList . to listSelectedElement)
    allConvs <- use (tuiUI . conversationList . to listElements)
    case mSelected of
        Just (_, conv) -> do
            coreRef <- use tuiCore
            liftIO $ atomically $ modifyTVar coreRef $ \c ->
                let treeState = coreConversationTreeState c
                    newTreeState = toggleExpanded (conversationId conv) treeState
                 in c{coreConversationTreeState = newTreeState}
            let convsList = Vector.toList allConvs
            showStatus StatusInfo $
                if hasChildConversations conv convsList
                    then "Toggled conversation expansion"
                    else "No children to expand"
            -- Refresh the conversation list to reflect changes
            handleHeartbeat
        Nothing -> pure ()

-- | Expand the currently selected conversation.
handleExpandConversation :: EventM N TuiState ()
handleExpandConversation = do
    mSelected <- use (tuiUI . conversationList . to listSelectedElement)
    case mSelected of
        Just (_, conv) -> do
            coreRef <- use tuiCore
            liftIO $ atomically $ modifyTVar coreRef $ \c ->
                let treeState = coreConversationTreeState c
                    newExpanded = Set.insert (conversationId conv) (_expandedConversations treeState)
                 in c{coreConversationTreeState = treeState{_expandedConversations = newExpanded}}
            handleHeartbeat
        Nothing -> pure ()

-- | Collapse the currently selected conversation.
handleCollapseConversation :: EventM N TuiState ()
handleCollapseConversation = do
    mSelected <- use (tuiUI . conversationList . to listSelectedElement)
    case mSelected of
        Just (_, conv) -> do
            coreRef <- use tuiCore
            liftIO $ atomically $ modifyTVar coreRef $ \c ->
                let treeState = coreConversationTreeState c
                    newExpanded = Set.delete (conversationId conv) (_expandedConversations treeState)
                 in c{coreConversationTreeState = treeState{_expandedConversations = newExpanded}}
            handleHeartbeat
        Nothing -> pure ()

-- | Expand all conversations.
handleExpandAllConversations :: EventM N TuiState ()
handleExpandAllConversations = do
    _convs <- use (tuiUI . conversationList . to listElements)
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        let treeState = coreConversationTreeState c
            convsList = c.coreConversations
            newTreeState = expandAll convsList treeState
         in c{coreConversationTreeState = newTreeState}
    showStatus StatusInfo "Expanded all conversations"
    handleHeartbeat

-- | Collapse all conversations.
handleCollapseAllConversations :: EventM N TuiState ()
handleCollapseAllConversations = do
    _convs <- use (tuiUI . conversationList . to listElements)
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        let treeState = coreConversationTreeState c
            convsList = c.coreConversations
            newTreeState = collapseAll convsList treeState
         in c{coreConversationTreeState = newTreeState}
    showStatus StatusInfo "Collapsed all conversations"
    handleHeartbeat

-- | Jump to next sibling conversation.
handleJumpToNextSibling :: EventM N TuiState ()
handleJumpToNextSibling = do
    mSelected <- use (tuiUI . conversationList . to listSelectedElement)
    allConvs <- use (tuiUI . conversationList . to listElements)
    case mSelected of
        Just (idx, conv) -> do
            let convsList = Vector.toList allConvs
                currentDepth = getConversationDepth conv convsList
                siblings =
                    [ (i, c)
                    | (i, c) <- zip [0 ..] convsList
                    , getConversationDepth c convsList == currentDepth
                    , i > idx
                    ]
            case siblings of
                ((nextIdx, _) : _) ->
                    tuiUI . conversationList . listSelectedL .= Just nextIdx
                [] -> pure () -- No more siblings
        Nothing -> pure ()

-- | Jump to previous sibling conversation.
handleJumpToPreviousSibling :: EventM N TuiState ()
handleJumpToPreviousSibling = do
    mSelected <- use (tuiUI . conversationList . to listSelectedElement)
    allConvs <- use (tuiUI . conversationList . to listElements)
    case mSelected of
        Just (idx, conv) -> do
            let convsList = Vector.toList allConvs
                currentDepth = getConversationDepth conv convsList
                siblings =
                    [ (i, c)
                    | (i, c) <- zip [0 ..] convsList
                    , getConversationDepth c convsList == currentDepth
                    , i < idx
                    ]
            case reverse siblings of
                ((prevIdx, _) : _) ->
                    tuiUI . conversationList . listSelectedL .= Just prevIdx
                [] -> pure () -- No previous siblings
        Nothing -> pure ()

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
        Nothing -> pure Nothing

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

During migration, this also:
1. Refreshes tools for the selected agent via RuntimeBridge
2. Synchronizes tool state between legacy Runtime and OS Core
3. Updates conversation tree cache for hierarchical display
-}
handleHeartbeat :: EventM N TuiState ()
handleHeartbeat = do
    -- Save the currently selected conversation ID before refreshing
    mSelectedConvId <- getFocusedConversationId

    -- Refresh conversations from core
    coreRef <- use tuiCore
    coreState <- liftIO $ readTVarIO coreRef
    let convs = coreConversations coreState

    -- Update tree cache
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{coreConversationTreeState = updateConversationTreeCache convs (coreConversationTreeState c)}

    -- Build display list respecting expanded state
    let visibleConvs = buildDisplayConversationList convs (coreConversationTreeState coreState)
    tuiUI . conversationList .= List.list ConversationListWidget (Vector.fromList visibleConvs) 1

    -- Restore the selection if the conversation still exists in visible list
    case mSelectedConvId of
        Just selectedConvId -> do
            let newConvs = Vector.fromList visibleConvs
            case Vector.findIndex (\c -> conversationId c == selectedConvId) newConvs of
                Just idx -> tuiUI . conversationList . listSelectedL .= Just idx
                Nothing -> pure () -- Conversation was removed or collapsed, keep no selection
        Nothing -> pure ()

    -- Refresh tools for the currently selected agent
    selectedAgent <- use (tuiUI . selectedAgentInfo)
    case selectedAgent of
        Just agent -> refreshToolsForAgent agent
        Nothing -> pure ()

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

{- | Handle agent trace events.

This function now properly handles sub-agent trace events, including:
- SubAgentStarted: When a sub-agent session begins
- SubAgentLLMTrace: LLM calls made by sub-agents
- SubAgentToolTrace: Tool calls made by sub-agents
- SubAgentCompleted: When a sub-agent finishes successfully
- SubAgentFailed: When a sub-agent encounters an error

These traces can be displayed in the TUI debug/log views or used for
monitoring recursive agent calls.
-}
handleAgentTrace :: Trace -> EventM N TuiState ()
handleAgentTrace trace = do
    case trace of
        AgentTrace_SubAgentCall{subAgentTrace} -> do
            -- Handle sub-agent traces
            case subAgentTrace of
                SubAgentStarted{} ->
                    -- Could add to a sub-agent activity log
                    pure ()
                SubAgentCompleted{} ->
                    -- Could display completion in status or log
                    pure ()
                SubAgentFailed{subAgentError} ->
                    -- Could display error in status or log
                    showStatus StatusWarning $ "Sub-agent failed: " <> Text.take 100 subAgentError
                SubAgentLLMTrace{} ->
                    -- Could display LLM activity in debug view
                    pure ()
                SubAgentToolTrace{} ->
                    -- Could display tool activity in debug view
                    pure ()
        _ ->
            -- Handle other trace types (existing behavior)
            pure ()

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
handleNewConversationFromEditor :: EventM N TuiState ()
handleNewConversationFromEditor = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, baseTuiAgent) -> do
            session <- liftIO (Session [] <$> newSessionId <*> pure Nothing <*> newTurnId)
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

runConversation :: TuiAgent -> Session -> EventM N TuiState ()
runConversation baseTuiAgent session = do
    -- Get session configuration (includes API keys)
    config <- use sessionConfig

    -- Generate conversation ID
    convId <- liftIO $ newConversationId

    outChan <- use eventChan
    inChan <- liftIO $ newBChan 100

    -- Build the progress callback
    let notifyProgress = buildOnProgress convId outChan

    -- Create a tracer that writes to the event channel for display
    let tracer = Tracer $ \tr -> writeBChan outChan (AppEvent_AgentTrace tr)

    -- Create the agent with the progress callback
    -- Use the agent's OSAgentNode through the TuiAgent
    -- Pass API keys from the session config
    agent0 <- liftIO $ nodeToAgent config.sessionStore Nothing convId tracer config.sessionApiKeys (tuiNode baseTuiAgent)

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
