{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Conversation lifecycle event handlers for the TUI.

Phase 3b-i (@todos/os-as-standalone-server.md@): the TUI no longer builds
or runs an agent itself (G1). Every handler that used to spawn a
'System.Agents.Session.Loop.runUntilBlocked' thread, poll a 'MailRouter',
or read\/write @Core@'s buffered-message\/paused-conversation maps is
reduced to a compiling stub, tagged with the 'System.Agents.Host.Client'
helper 3b-ii will call. Handlers that only touch local UI state (selection,
focus, unread markers, the 'ConversationStatus' already on a 'Conversation')
keep working as before.
-}
module System.Agents.TUI.Event.Conversation (
    -- * New Conversation
    handleNewConversationFromEditor,
    handleNewConversation,

    -- * Restored Conversation
    handleRestoredConversation,

    -- * Run Conversation
    runConversation,

    -- * Types
    Trace (..),

    -- * Send Message
    handleSendMessage,

    -- * Subcall Management
    createSubcallConversationEntry,
    handleSubcallStarted,
    handleSubcallCompleted,
    handleSubcallFailed,
    findAgentBySlug,

    -- * Conversation Updates
    handleConversationUpdated,
    handleToolCallActivity,
    handleRunStopped,
    updateConversationStatus,

    -- * Pause/Resume
    handleTogglePauseConversation,
    isConversationPaused,

    -- * Interrupt
    handleInterruptConversation,
    handleCancelAttachedConversation,
    handlePauseRunConversation,

    -- * Core State Manipulation
    appendConversation,
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.List (listInsert, listSelectedElement)
import qualified Brick.Widgets.List as List
import Control.Concurrent.STM (atomically, modifyTVar)
import Control.Lens (to, use, (%=), (.=), (^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector

import Prod.Tracer (Tracer (..))

import System.Agents.Base (ConversationId (..))
import System.Agents.OS.Events (ToolCallActivity)
import System.Agents.Session.Base (Session (..), SessionStatus (..))
import System.Agents.SessionStore (SessionMeta, conversationIdToSessionId)
import System.Agents.TUI.ToolCallActivity (applyToolCallActivity)
import System.Agents.TUI.Types (
    AppEvent (..),
    Conversation (..),
    ConversationStatus (..),
    Core (..),
    Draft,
    N,
    StatusSeverity (..),
    TuiAgent (..),
    TuiState,
    agentList,
    conversationId,
    conversationList,
    conversationName,
    coreConversations,
    emptyDraft,
    eventChan,
    tuiCore,
    tuiSlug,
    toolCallViews,
    tuiUI,
    unreadConversations,
    updateConversationSession,
 )

-- | Trace type for TUI conversation events. Nothing to trace yet in 3b-i;
-- 3b-ii will carry 'RunnerClient' command\/reply pairs here.
newtype Trace = ConversationTrace Text.Text
    deriving (Show)

-- | Show a status message in the TUI.
showStatus :: StatusSeverity -> Text.Text -> EventM N TuiState ()
showStatus severity text = do
    chan <- use eventChan
    liftIO $ writeBChan chan (AppEvent_ShowStatus severity text)

-- | Get the currently focused conversation, if any.
getFocusedConversation :: EventM N TuiState (Maybe Conversation)
getFocusedConversation = do
    mConv <- use (tuiUI . conversationList . to listSelectedElement)
    pure $ fmap snd mConv

-------------------------------------------------------------------------------
-- New Conversation
-------------------------------------------------------------------------------

-- | Create a new conversation from the selected agent.
-- TODO(3b-ii): System.Agents.Host.Client.createSession
handleNewConversationFromEditor :: Tracer IO Trace -> EventM N TuiState ()
handleNewConversationFromEditor _tracer = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, baseTuiAgent) ->
            showStatus StatusWarning $
                "New conversation with @" <> tuiSlug baseTuiAgent <> " not wired yet (3b-ii: Client.createSession)"
        Nothing -> showStatus StatusWarning "No agent selected"

-- | Handle new conversation event: select it in the list and mark it read.
handleNewConversation :: ConversationId -> EventM N TuiState ()
handleNewConversation convId = do
    convs <- use (tuiUI . conversationList . to List.listElements)
    case Vector.findIndex (\c -> conversationId c == convId) convs of
        Just idx -> do
            tuiUI . conversationList . List.listSelectedL .= Just idx
            tuiUI . unreadConversations %= Set.delete convId
        Nothing -> pure ()

-------------------------------------------------------------------------------
-- Restored Conversation
-------------------------------------------------------------------------------

-- | Continue a session restored from the History tab.
-- TODO(3b-ii)/TODO(3b-iii): System.Agents.Host.Client.getSession, then
-- Client.postMessage or Client.resumeSession; the History tab itself is
-- only populated starting 3b-iii ('Client.listSessions').
handleRestoredConversation :: Tracer IO Trace -> EventM N TuiState ()
handleRestoredConversation _tracer =
    showStatus StatusWarning "Continuing a stored session is not wired yet (3b-ii/3b-iii)"

-------------------------------------------------------------------------------
-- Run Conversation
-------------------------------------------------------------------------------

{- | Build the client-side 'Conversation' view for a session and add it to
'Core' and the visible list.

TODO(3b-ii): this used to fork a 'System.Agents.Session.Loop.runUntilBlocked'
thread; the runner now owns every session's run loop
("System.Agents.Host.Runner"), so this becomes: call
'System.Agents.Host.Client.createSession' (or 'Client.getSession' for a
restored one), then insert the resulting 'Conversation' built from its
'SessionMeta'.
-}
runConversation :: Tracer IO Trace -> TuiAgent -> Session -> EventM N TuiState ()
runConversation _tracer baseTuiAgent _session =
    showStatus StatusWarning $
        "Starting @" <> tuiSlug baseTuiAgent <> " not wired yet (3b-ii: Client.createSession)"

-------------------------------------------------------------------------------
-- Send Message
-------------------------------------------------------------------------------

{- | Send (or draft) a message in the current conversation.

TODO(3b-ii): 'System.Agents.Host.Client.postMessage' when the session is
idle. TODO(3b-iii): append-to-draft semantics (§5, D3) when it is busy,
and the "send now" / draft editor behaviour.
-}
handleSendMessage :: EventM N TuiState ()
handleSendMessage = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv ->
            showStatus StatusWarning $
                "Sending to " <> conversationName conv <> " not wired yet (3b-ii: Client.postMessage)"

-------------------------------------------------------------------------------
-- Subcall Management
-------------------------------------------------------------------------------

{- | Handle a subcall-started event by creating its conversation entry.
TODO(3b-ii): this fires from the runner's event stream
('System.Agents.Host.Client.subscribeAll' -> 'AppEvent_SubcallStarted'),
not from a local OS event queue any more.
-}
handleSubcallStarted :: Tracer IO Trace -> ConversationId -> ConversationId -> Text.Text -> Int -> EventM N TuiState ()
handleSubcallStarted _tracer parentId subcallId slug depth = do
    agents <- use (tuiUI . agentList . to List.listElements)
    case findAgentBySlug slug agents of
        Just tuiAgent -> createSubcallConversationEntry tuiAgent subcallId parentId depth
        Nothing -> showStatus StatusWarning $ "Agent not found for subcall: " <> slug

-- | Find a TuiAgent by slug from the agent list.
findAgentBySlug :: Text.Text -> Vector.Vector TuiAgent -> Maybe TuiAgent
findAgentBySlug slug agents = Vector.find (\a -> tuiSlug a == slug) agents

-- | Handle subcall completed event: mark the conversation as no longer running.
handleSubcallCompleted :: ConversationId -> Text.Text -> EventM N TuiState ()
handleSubcallCompleted subcallId result = do
    showStatus StatusInfo $ "Subcall completed: " <> Text.take 30 result
    updateConversationStatus subcallId ConversationStatus_WaitingForInput

-- | Handle subcall failed event.
handleSubcallFailed :: ConversationId -> Text.Text -> EventM N TuiState ()
handleSubcallFailed subcallId err = do
    showStatus StatusError $ "Subcall failed: " <> err
    updateConversationStatus subcallId ConversationStatus_WaitingForInput

{- | Create a conversation entry for a sub-agent call, in the "started but
not yet updated" state. Progress reaches it later through
'handleConversationUpdated', once its own 'AppEvent_SessionUpdated's start
arriving (3b-ii).
-}
createSubcallConversationEntry ::
    TuiAgent ->
    ConversationId ->
    ConversationId ->
    Int ->
    EventM N TuiState ()
createSubcallConversationEntry tuiAgent convId parentId depth = do
    let conv =
            Conversation
                { conversationId = convId
                , conversationSessionId = conversationIdToSessionId convId
                , conversationAgentSlug = tuiSlug tuiAgent
                , conversationSession = Nothing
                , conversationMeta = Nothing
                , conversationName = "@" <> tuiSlug tuiAgent
                , conversationStatus = ConversationStatus_Active
                , conversationIsSubcall = True
                , conversationParentId = Just parentId
                , conversationSubcallDepth = depth
                , conversationDraft = emptyDraft :: Draft
                }
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ appendConversation conv
    tuiUI . conversationList %= listInsert 0 conv
    let convShort = shortConvId convId
        parentShort = shortConvId parentId
    showStatus StatusInfo $ "Created subcall d=" <> Text.pack (show depth) <> " cid=" <> convShort <> " pid=" <> parentShort

-------------------------------------------------------------------------------
-- Conversation Updates
-------------------------------------------------------------------------------

{- | Translate a run's stop status into the conversation's local status
(replaces the old @AppEvent_AgentNeedsInput@: there is no separate
"needs input" runner event, only @run.stopped@ with a 'SessionStatus').
-}
handleRunStopped :: ConversationId -> SessionStatus -> EventM N TuiState ()
handleRunStopped convId status = updateConversationStatus convId (fromSessionStatus status)
  where
    fromSessionStatus StatusPaused = ConversationStatus_Paused
    fromSessionStatus StatusWaitingExternal = ConversationStatus_BlockedOnDeferred
    fromSessionStatus StatusRunning = ConversationStatus_Active
    fromSessionStatus _ = ConversationStatus_WaitingForInput

-- | Update conversation status in core.
updateConversationStatus :: ConversationId -> ConversationStatus -> EventM N TuiState ()
updateConversationStatus convId newStatus = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c
            { _coreConversations =
                map
                    ( \conv ->
                        if conversationId conv == convId
                            then conv{conversationStatus = newStatus}
                            else conv
                    )
                    (c ^. coreConversations)
            }

-- | Record a background tool call event for rendering.
handleToolCallActivity :: ToolCallActivity -> EventM N TuiState ()
handleToolCallActivity activity =
    tuiUI . toolCallViews %= applyToolCallActivity activity

{- | Handle a session update: refresh the conversation's 'Session'\/'SessionMeta'
and mark it unread if it is not the focused one.
-}
handleConversationUpdated :: ConversationId -> Session -> SessionMeta -> EventM N TuiState ()
handleConversationUpdated convId sess meta = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{_coreConversations = updateConversationSession convId sess meta (c ^. coreConversations)}
    selected <- use (tuiUI . conversationList . to listSelectedElement)
    case selected of
        Just (_, conv)
            | conversationId conv /= convId ->
                tuiUI . unreadConversations %= Set.insert convId
        _ -> pure ()

-------------------------------------------------------------------------------
-- Pause/Resume
-------------------------------------------------------------------------------

{- | Toggle a purely local, TUI-side "don't send input yet" status -- this
never touches the kernel (see 'handlePauseRunConversation' for the real,
mail-backed pause).
-}
handleTogglePauseConversation :: EventM N TuiState ()
handleTogglePauseConversation = do
    mSelectedConv <- use (tuiUI . conversationList . to listSelectedElement)
    case mSelectedConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just (_, conv) -> do
            let convId = conversationId conv
            if conv.conversationStatus == ConversationStatus_Paused
                then do
                    updateConversationStatus convId ConversationStatus_WaitingForInput
                    showStatus StatusInfo $ "Unpaused: " <> conversationName conv
                else do
                    updateConversationStatus convId ConversationStatus_Paused
                    showStatus StatusInfo $ "Paused: " <> conversationName conv

-- | Whether a conversation is currently (locally) paused.
isConversationPaused :: ConversationId -> Core -> Bool
isConversationPaused convId core =
    any (\c -> conversationId c == convId && c.conversationStatus == ConversationStatus_Paused) (core ^. coreConversations)

-------------------------------------------------------------------------------
-- Interrupt
-------------------------------------------------------------------------------

-- | TODO(3b-ii): System.Agents.Host.Client.postMessage with 'nmInterrupt = True'.
handleInterruptConversation :: EventM N TuiState ()
handleInterruptConversation = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv ->
            showStatus StatusWarning $
                "Interrupt for " <> conversationName conv <> " not wired yet (3b-ii: Client.postMessage nmInterrupt)"

-- | TODO(3b-ii): System.Agents.Host.Client.cancelAttachedCalls.
handleCancelAttachedConversation :: EventM N TuiState ()
handleCancelAttachedConversation = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv ->
            showStatus StatusWarning $
                "Cancel-attached for " <> conversationName conv <> " not wired yet (3b-ii: Client.cancelAttachedCalls)"

-- | TODO(3b-ii): System.Agents.Host.Client.pauseSession.
handlePauseRunConversation :: EventM N TuiState ()
handlePauseRunConversation = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv ->
            showStatus StatusWarning $
                "Pause for " <> conversationName conv <> " not wired yet (3b-ii: Client.pauseSession)"

-------------------------------------------------------------------------------
-- Core State Manipulation
-------------------------------------------------------------------------------

-- | Append a conversation to the front of the core's conversation list.
appendConversation :: Conversation -> Core -> Core
appendConversation conv c = c{_coreConversations = conv : (c ^. coreConversations)}

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Extract short identifier from ConversationId for debugging.
shortConvId :: ConversationId -> Text.Text
shortConvId (ConversationId uuid) = Text.take 8 $ Text.pack $ UUID.toString uuid
