{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Event handling for the TUI application.
module System.Agents.TUI2.Event where

import Brick
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Focus (focusGetCurrent, focusSetCurrent, focusNext, focusPrev, focusRingModify)
import Brick.Widgets.Edit (Editor, handleEditorEvent, editContentsL, getEditContents)
import Brick.Widgets.List (handleListEvent, listSelectedElement, listInsert, listSelectedL, listElements)
import qualified Brick.Widgets.List as List
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Lens (use, (%=), (.=), to)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.CircularList as CList
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as TextZipper
import qualified Data.Vector as Vector
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as Vty

import System.Agents.AgentTree (AgentTree(..), LoadAgentResult (..), Props)
import System.Agents.Base (ConversationId (..), newConversationId)
import System.Agents.OneShot (runtimeToAgent, storeSession)
import qualified System.Agents.Runtime as Runtime
import System.Agents.Session.Base (Session (..), UserQuery (..), newSessionId, newTurnId, Agent (..), Action (..), MissingUserPrompt (..))
import qualified System.Agents.Session.Loop as Loop
import System.Agents.TUI2.Types

-------------------------------------------------------------------------------
-- Main Event Handler
-------------------------------------------------------------------------------

-- | Main event handler for the TUI application.
tui_appHandleEvent :: FilePath -> BrickEvent N AppEvent -> EventM N TuiState ()
tui_appHandleEvent convPrefix ev = do
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
        VtyEvent (Vty.EvKey (Vty.KChar 'z') _) ->
            toggleZoom
        VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) ->
            handleNewConversationFromEditor convPrefix
        VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) ->
            handleContinueRestoredConversation convPrefix
        VtyEvent (Vty.EvKey Vty.KEnter [Vty.MMeta]) ->
            handleSendMessage

        -- Delegate to focused widget
        VtyEvent vtyEv -> do
            currentFocus <- use (tuiUI . uiFocusRing . to focusGetCurrent)
            case currentFocus of
                Just AgentListWidget ->
                    handleAgentListEvent vtyEv
                Just ConversationListWidget ->
                    handleConversationListEvent vtyEv
                Just MessageEditorWidget ->
                    handleMessageEditorEvent ev
                Just ConversationViewWidget ->
                    handleConversationViewEvent vtyEv
                Just AgentInfoWidget ->
                    handleAgentInfoEvent vtyEv
                Just AgentToolsWidget ->
                    handleAgentToolsEvent vtyEv
                Nothing ->
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

-- | Handle conversation list navigation.
handleConversationListEvent :: Vty.Event -> EventM N TuiState ()
handleConversationListEvent ev = do
    zoom (tuiUI . conversationList) $ handleListEvent ev
    -- Mark conversation as read when selected
    selected <- use (tuiUI . conversationList . to listSelectedElement)
    case selected of
        Just (_, conv) ->
            tuiUI . unreadConversations %= Set.delete (conversationId conv)
        Nothing -> pure ()

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

-- | Handle agent tools scrolling.
handleAgentToolsEvent :: Vty.Event -> EventM N TuiState ()
handleAgentToolsEvent ev =
    case ev of
        Vty.EvKey Vty.KUp _ ->
            vScrollBy (viewportScroll AgentToolsWidget) (-1)
        Vty.EvKey Vty.KDown _ ->
            vScrollBy (viewportScroll AgentToolsWidget) 1
        Vty.EvKey Vty.KLeft _ ->
            hScrollBy (viewportScroll AgentToolsWidget) (-1)
        Vty.EvKey Vty.KRight _ ->
            hScrollBy (viewportScroll AgentToolsWidget) 1
        _ -> pure ()

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

-- | Handle heartbeat - refresh UI state.
handleHeartbeat :: EventM N TuiState ()
handleHeartbeat = do
    -- Refresh conversations from core
    coreRef <- use tuiCore
    coreState <- liftIO $ readTVarIO coreRef
    let convs = coreConversations coreState
    tuiUI . conversationList .= List.list ConversationListWidget (Vector.fromList convs) 1

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
        c{coreConversations = map (\conv -> 
            if conversationId conv == convId 
            then conv{conversationStatus = newStatus}
            else conv) (coreConversations c)}

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

-- | Handle agent trace events.
-- TODO: remove
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
-- Conversation Management
-------------------------------------------------------------------------------

-- | Create a new conversation from the selected agent.
handleNewConversationFromEditor :: FilePath -> EventM N TuiState ()
handleNewConversationFromEditor convPrefix = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, baseTuiAgent) -> do
            -- * agent will have prompt, tools from the base agent, but will communicate via a pair of chans
            agent0 <- liftIO $ runtimeToAgent (baseTuiAgent.agentTree.agentRuntime)
            convId@(ConversationId cId) <- liftIO $ newConversationId
            let convFilePath = convPrefix <> "conv." <> show cId <> ".json"
            outChan <- use eventChan
            let notifyProgress sess = writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
            let notifyNeedInput = writeBChan outChan (AppEvent_AgentNeedsInput convId)
            inChan <- liftIO $ newBChan 100
            let a = agent0 {
                step = \sess -> do
                   storeSession sess convFilePath
                   notifyProgress sess
                   ret <- agent0.step sess
                   case ret of
                      Stop _r -> -- smoll hack to reuse the naive step from runtimeToAgent
                        pure $ AskUserPrompt (MissingUserPrompt True [])
                      _ -> pure ret
              , usrQuery = notifyNeedInput >> readBChan inChan
              }

            -- * wrap in Conversation
            let tuiAgent = TuiAgent a baseTuiAgent.agentTree
            session <- liftIO (Session [] <$> newSessionId <*> pure Nothing <*> newTurnId)
            threadId <- liftIO $ forkIO $ void $ Loop.run a session
            let conv =
                    Conversation
                        { conversationId = convId
                        , conversationAgent = tuiAgent
                        , conversationThreadId = Just threadId
                        , conversationSession = Nothing
                        , conversationName = "@" <> tuiAgent.agentTree.agentRuntime.agentSlug
                        , conversationChan = inChan
                        , conversationStatus = ConversationStatus_WaitingForInput
                        , conversationFilePath = convFilePath
                        }


            -- Add to core
            coreRef <- use tuiCore
            liftIO $ atomically $ modifyTVar coreRef $ \c ->
                c{coreConversations = conv : coreConversations c}
            -- Update UI
            tuiUI . conversationList %= listInsert 0 conv
            tuiUI . conversationList . listSelectedL .= Just 0
            tuiUI . uiFocusRing %= focusRingModify (CList.insertR ConversationViewWidget)
            tuiUI . uiFocusRing %= focusSetCurrent MessageEditorWidget
        Nothing -> pure ()

-- | Continue a restored conversation from disk.
-- This starts the agent loop with the restored session.
handleContinueRestoredConversation :: FilePath -> EventM N TuiState ()
handleContinueRestoredConversation convPrefix = do
    selected <- use (tuiUI . conversationList . to listSelectedElement)
    case selected of
        Just (idx, conv) -> do
            -- Only continue if the conversation is in restored state
            if conversationStatus conv /= ConversationStatus_Restored
                then pure ()  -- Already running or waiting for input
                else case conversationSession conv of
                    Nothing -> pure ()  -- No session to continue from
                    Just session -> do
                        outChan <- use eventChan
                        let convId = conversationId conv
                        
                        -- Create the agent with session storage and notification
                        let baseTuiAgent = conversationAgent conv
                        let convFilePath = conversationFilePath conv
                        
                        -- Create a new channel for this conversation
                        inChan <- liftIO $ newBChan 100
                        
                        -- Wrap the agent step function
                        let notifyProgress sess = writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
                        let notifyNeedInput = writeBChan outChan (AppEvent_AgentNeedsInput convId)
                        
                        -- Get the base agent's step function
                        let baseStep = (sessionAgent baseTuiAgent).step
                        
                        -- Create the decorated step function
                        let decoratedStep sess = do
                                storeSession sess convFilePath
                                notifyProgress sess
                                ret <- baseStep sess
                                case ret of
                                    Stop _r ->
                                        pure $ AskUserPrompt (MissingUserPrompt True [])
                                    _ -> pure ret
                        
                        -- Create the updated agent
                        let updatedAgent = (sessionAgent baseTuiAgent)
                                { step = decoratedStep
                                , usrQuery = notifyNeedInput >> readBChan inChan
                                }
                        
                        let updatedTuiAgent = TuiAgent updatedAgent (agentTree baseTuiAgent)
                        
                        -- Start the agent loop with the restored session
                        threadId <- liftIO $ forkIO $ void $ Loop.run updatedAgent session
                        
                        -- Update the conversation with new thread and status
                        let updatedConv = conv
                                { conversationAgent = updatedTuiAgent
                                , conversationThreadId = Just threadId
                                , conversationStatus = ConversationStatus_Active
                                , conversationChan = inChan
                                , conversationName = Text.replace " (restored)" "" (conversationName conv)
                                }
                        
                        -- Update core
                        coreRef <- use tuiCore
                        liftIO $ atomically $ modifyTVar coreRef $ \c ->
                            c{coreConversations = updateConversation updatedConv (coreConversations c)}
                        
                        -- Update UI
                        tuiUI . conversationList . listSelectedL .= Just idx
                        tuiUI . ongoingConversations %= Set.insert convId
                        tuiUI . uiFocusRing %= focusSetCurrent MessageEditorWidget
        Nothing -> pure ()

-- | Send a message in the current conversation.
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
                -- Check if conversation is restored - if so, we can't send messages until continued
                if conversationStatus conv == ConversationStatus_Restored
                    then pure ()  -- Can't send to restored conversation without continuing first
                    else do
                        -- Write message unconditionally
                        liftIO $ writeBChan conv.conversationChan (Just $ UserQuery msgText)

                        -- Check if conversation is already being processed
                        ongoing <- use (tuiUI . ongoingConversations)
                        when (not $ Set.member (conversationId conv) ongoing) $ do
                            -- Mark as ongoing
                            tuiUI . ongoingConversations %= Set.insert (conversationId conv)
                            -- Clear editor
                            tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing

            Nothing -> pure ()

