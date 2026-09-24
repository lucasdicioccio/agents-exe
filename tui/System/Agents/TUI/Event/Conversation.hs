{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Conversation lifecycle event handlers for the TUI.

Phase 3b-ii (@todos/os-as-standalone-server.md@): every handler here now
drives its work through 'System.Agents.Host.Client.RunnerClient' (Design
§4). A 'Conversation' is a client-side view of a runner session: creating
one calls 'Client.createSession', continuing a stored one calls
'Client.getSession', sending calls 'Client.postMessage' (interrupting sets
'System.Agents.Protocol.nmInterrupt'), pausing calls 'Client.pauseSession'
(unpausing 'Client.resumeSession'), and forking (elsewhere, "System.Agents.TUI.Event")
calls 'Client.forkSession'. Every session this TUI creates, forks, or
messages after restoring is added to 'coreOwnedSessions' so quitting can
ask the runner to stop it (see 'System.Agents.TUI.Event.stopConversations').
Handlers that only touch local UI state (selection, focus, unread markers)
are unchanged from 3b-i.
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
    handleSessionCreated,
    handleToolCallActivity,
    handleRunStopped,
    updateConversationStatus,
    statusFromSessionStatus,

    -- * Pause/Resume
    handleTogglePauseConversation,
    isConversationPaused,

    -- * Interrupt
    handleInterruptConversation,
    handleCancelAttachedConversation,
    handlePauseRunConversation,

    -- * Draft buffer (§5, D3)
    setConversationDraft,
    shipDraftIfAny,

    -- * Pending calls (Phase 3c)
    setConversationPending,
    handleCallsDeferred,

    -- * Core State Manipulation
    appendConversation,
    mkConversation,
    addConversationToCore,
    markOwnedSession,
    reportRunnerResult,
    readCore,
    clearEditorAndAttachments,
) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Widgets.Edit (editContentsL, getEditContents)
import Brick.Widgets.List (listInsert, listSelectedElement)
import qualified Brick.Widgets.List as List
import Control.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Control.Lens (to, use, (%=), (.=), (^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Zipper as TextZipper
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector

import Prod.Tracer (Tracer (..))

import System.Agents.Base (ConversationId (..))
import qualified System.Agents.Host.Client as Client
import System.Agents.OS.Events (ToolCallActivity)
import System.Agents.Protocol (NewMessage (..), RunMode (..), RunnerError, runnerErrorMessage)
import System.Agents.Session.Base (DeferredCallView, Session (..), SessionId, SessionStatus (..))
import System.Agents.SessionStore (SessionMeta (..), conversationIdToSessionId, sessionIdToConversationId)
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
    appendDraft,
    attachedFiles,
    conversationId,
    conversationList,
    conversationName,
    coreClient,
    coreConversations,
    coreOwnedSessions,
    coreParams,
    draftToMessage,
    emptyDraft,
    eventChan,
    messageEditor,
    selectedAttachmentIndex,
    sessionList,
    shouldShipDraft,
    tuiCore,
    tuiSlug,
    toolCallViews,
    tuiUI,
    unreadConversations,
    updateConversationSession,
 )

-- | Trace type for TUI conversation events. Nothing to trace yet.
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
-- Core / client helpers
-------------------------------------------------------------------------------

-- | Read the current 'Core' (client, params, owned sessions, conversations).
readCore :: EventM N TuiState Core
readCore = use tuiCore >>= liftIO . readTVarIO

-- | Build the client-side view of a session from its 'SessionMeta' (and, if
-- already fetched, its full 'Session').
mkConversation :: SessionMeta -> Maybe Session -> Bool -> Maybe ConversationId -> Int -> Conversation
mkConversation meta mSess isSubcall parentId depth =
    Conversation
        { conversationId = sessionIdToConversationId meta.smSessionId
        , conversationSessionId = meta.smSessionId
        , conversationAgentSlug = slug
        , conversationSession = mSess
        , conversationMeta = Just meta
        , conversationName = "@" <> slug
        , conversationStatus = statusFromSessionStatus meta.smStatus
        , conversationIsSubcall = isSubcall
        , conversationParentId = parentId
        , conversationSubcallDepth = depth
        , conversationDraft = emptyDraft :: Draft
        , conversationPending = []
        }
  where
    slug = fromMaybe "?" meta.smAgent

-- | Translate a run's stop status (or a fresh 'SessionMeta.smStatus') into
-- the conversation's local status.
statusFromSessionStatus :: SessionStatus -> ConversationStatus
statusFromSessionStatus StatusPaused = ConversationStatus_Paused
statusFromSessionStatus StatusWaitingExternal = ConversationStatus_BlockedOnDeferred
statusFromSessionStatus StatusRunning = ConversationStatus_Active
statusFromSessionStatus _ = ConversationStatus_WaitingForInput

-- | Add a conversation to 'Core' and to the visible list, at the front.
addConversationToCore :: Conversation -> EventM N TuiState ()
addConversationToCore conv = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ appendConversation conv
    tuiUI . conversationList %= listInsert 0 conv

-- | Record a session id as one this TUI started (embedded mode): due a
-- 'Client.sendMail' 'StopRun' on quit ("System.Agents.TUI.Event".stopConversations).
markOwnedSession :: SessionId -> EventM N TuiState ()
markOwnedSession sid = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{_coreOwnedSessions = Set.insert sid (c ^. coreOwnedSessions)}

{- | Surface a 'RunnerClient' result the way every command handler does:
an error never drops silently (it becomes a status-line error), a success
refreshes the conversation's status from the returned 'SessionMeta'.
-}
reportRunnerResult :: Text.Text -> Either RunnerError SessionMeta -> EventM N TuiState ()
reportRunnerResult _ (Left err) = showStatus StatusError (runnerErrorMessage err)
reportRunnerResult okMsg (Right meta) = do
    updateConversationStatus (sessionIdToConversationId meta.smSessionId) (statusFromSessionStatus meta.smStatus)
    showStatus StatusInfo okMsg

-------------------------------------------------------------------------------
-- New Conversation
-------------------------------------------------------------------------------

-- | Create a new (promptless) conversation with the selected agent.
handleNewConversationFromEditor :: Tracer IO Trace -> EventM N TuiState ()
handleNewConversationFromEditor _tracer = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Nothing -> showStatus StatusWarning "No agent selected"
        Just (_, baseTuiAgent) -> do
            let slug = tuiSlug baseTuiAgent
            core <- readCore
            result <- liftIO $ Client.createSession (core ^. coreClient) slug Nothing Nothing (core ^. coreParams)
            case result of
                Left err -> showStatus StatusError (runnerErrorMessage err)
                Right meta -> do
                    let conv = mkConversation meta Nothing False Nothing 0
                    addConversationToCore conv
                    markOwnedSession meta.smSessionId
                    handleNewConversation (conversationId conv)
                    showStatus StatusInfo $ "Started @" <> slug

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

{- | Continue a session restored from the History tab (the 'sessionList'
selection): if the currently selected agent differs from the stored
session's 'smAgent', fork first ('Client.forkSession' with no turn index,
i.e. the whole session, and the new agent slug -- "continue with another
agent") and open the fork; otherwise open the source session as-is
('runConversation'). Either way, the opened session only joins
'coreOwnedSessions' once the user actually sends it a message
('handleSendMessage'), except a fork, which this TUI created outright.
-}
handleRestoredConversation :: Tracer IO Trace -> EventM N TuiState ()
handleRestoredConversation tracer = do
    mSelectedSession <- use (tuiUI . sessionList . to listSelectedElement)
    case mSelectedSession of
        Nothing -> showStatus StatusWarning "No stored session selected"
        Just (_, sourceMeta) -> do
            mAgent <- use (tuiUI . agentList . to listSelectedElement)
            case mAgent of
                Nothing -> showStatus StatusWarning "No agent selected"
                Just (_, baseTuiAgent) -> do
                    let slug = tuiSlug baseTuiAgent
                        sid = sourceMeta.smSessionId
                    if sourceMeta.smAgent == Just slug
                        then runConversation tracer slug sid
                        else do
                            core <- readCore
                            forked <- liftIO $ Client.forkSession (core ^. coreClient) sid Nothing (Just slug)
                            case forked of
                                Left err -> showStatus StatusError (runnerErrorMessage err)
                                Right meta -> do
                                    markOwnedSession meta.smSessionId
                                    runConversation tracer slug meta.smSessionId
                                    showStatus StatusInfo $ "Forked to continue with @" <> slug

-------------------------------------------------------------------------------
-- Run Conversation
-------------------------------------------------------------------------------

{- | Open an existing session (a fresh fork, or a stored one from the
History tab) as a conversation: 'Client.getSession' for its full 'Session',
then add and select it.
-}
runConversation :: Tracer IO Trace -> Text.Text -> SessionId -> EventM N TuiState ()
runConversation _tracer slug sid = do
    core <- readCore
    result <- liftIO $ Client.getSession (core ^. coreClient) sid
    case result of
        Left err -> showStatus StatusError (runnerErrorMessage err)
        Right (sess, meta) -> do
            let conv = mkConversation meta (Just sess) False Nothing 0
            addConversationToCore conv
            handleNewConversation (conversationId conv)
            showStatus StatusInfo $ "Opened @" <> slug

-------------------------------------------------------------------------------
-- Send Message
-------------------------------------------------------------------------------

-- | Clear the message editor and the conversation's attachments.
clearEditorAndAttachments :: Conversation -> EventM N TuiState ()
clearEditorAndAttachments conv = do
    tuiUI . messageEditor . editContentsL .= TextZipper.textZipper [] Nothing
    tuiUI . attachedFiles %= Map.delete (conversationId conv)
    tuiUI . selectedAttachmentIndex .= Nothing

{- | Post the message editor's content (plus any attachments) to a
conversation's session, or append it to the conversation's draft (§5, D3)
when the session is busy: 'Client.postMessage' when it is idle
('ConversationStatus_WaitingForInput'), 'appendDraft' when a run is
active, paused, or blocked on deferred calls. An interrupt
('nmInterrupt', 'handleInterruptConversation') always bypasses the draft
and posts straight through, since it is meant to reach a running session
right away.
-}
sendMessageTo :: Conversation -> Bool -> EventM N TuiState ()
sendMessageTo conv interrupt = do
    msgLines <- use (tuiUI . messageEditor . to getEditContents)
    let msgText = Text.strip (Text.intercalate "\n" msgLines)
    atts <- use (tuiUI . attachedFiles)
    let attachments = Map.findWithDefault [] (conversationId conv) atts
    if Text.null msgText && null attachments
        then showStatus StatusWarning "Nothing to send"
        else
            if not interrupt && conversationStatus conv /= ConversationStatus_WaitingForInput
                then do
                    setConversationDraft (conversationId conv) (appendDraft msgText attachments (conversationDraft conv))
                    clearEditorAndAttachments conv
                    showStatus StatusInfo $ "Added to draft for " <> conversationName conv
                else do
                    core <- readCore
                    let nm = NewMessage{nmText = msgText, nmMedia = attachments, nmInterrupt = interrupt}
                    result <-
                        liftIO $
                            Client.postMessage
                                (core ^. coreClient)
                                (conversationSessionId conv)
                                nm
                                (Just UntilBlocked)
                                (core ^. coreParams)
                    case result of
                        Left err -> showStatus StatusError (runnerErrorMessage err)
                        Right meta -> do
                            clearEditorAndAttachments conv
                            updateConversationStatus (conversationId conv) (statusFromSessionStatus meta.smStatus)
                            markOwnedSession meta.smSessionId
                            showStatus StatusInfo $
                                if interrupt then "Interrupted " <> conversationName conv else "Sent to " <> conversationName conv

-- | Send (or draft) a message in the current conversation.
handleSendMessage :: EventM N TuiState ()
handleSendMessage = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> sendMessageTo conv False

-------------------------------------------------------------------------------
-- Subcall Management
-------------------------------------------------------------------------------

{- | Handle a subcall-started event by creating its conversation entry.
Fires from the runner's event stream ('System.Agents.Host.Client.subscribeAll'
-> 'AppEvent_SubcallStarted', bridged in "System.Agents.TUI.Core").
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
arriving.
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
                , conversationPending = []
                }
    addConversationToCore conv
    let convShort = shortConvId convId
        parentShort = shortConvId parentId
    showStatus StatusInfo $ "Created subcall d=" <> Text.pack (show depth) <> " cid=" <> convShort <> " pid=" <> parentShort

-------------------------------------------------------------------------------
-- Conversation Updates
-------------------------------------------------------------------------------

{- | Translate a run's stop status into the conversation's local status
(replaces the old @AppEvent_AgentNeedsInput@: there is no separate
"needs input" runner event, only @run.stopped@ with a 'SessionStatus'),
then ship the conversation's draft (§5) if the new status accepts input.
-}
handleRunStopped :: ConversationId -> SessionStatus -> EventM N TuiState ()
handleRunStopped convId status = do
    updateConversationStatus convId (statusFromSessionStatus status)
    when (shouldShipDraft status) $ shipDraftIfAny convId

{- | Replace a conversation's draft in 'Core'.
-}
setConversationDraft :: ConversationId -> Draft -> EventM N TuiState ()
setConversationDraft convId newDraft = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c
            { _coreConversations =
                map
                    ( \conv ->
                        if conversationId conv == convId
                            then conv{conversationDraft = newDraft}
                            else conv
                    )
                    (c ^. coreConversations)
            }

{- | Post a conversation's draft as one 'Client.postMessage', then clear it
(§5): fires from 'handleRunStopped' when the new status accepts input, and
from "System.Agents.TUI.Event.Draft"'s "Send now" action. A no-op when the
draft is empty.
-}
shipDraftIfAny :: ConversationId -> EventM N TuiState ()
shipDraftIfAny convId = do
    core <- readCore
    case [c | c <- core ^. coreConversations, conversationId c == convId] of
        (conv : _) | Just nm <- draftToMessage (conversationDraft conv) -> do
            result <-
                liftIO $
                    Client.postMessage
                        (core ^. coreClient)
                        (conversationSessionId conv)
                        nm
                        (Just UntilBlocked)
                        (core ^. coreParams)
            case result of
                Left err -> showStatus StatusError (runnerErrorMessage err)
                Right meta -> do
                    setConversationDraft convId emptyDraft
                    updateConversationStatus convId (statusFromSessionStatus meta.smStatus)
                    markOwnedSession meta.smSessionId
        _ -> pure ()

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

{- | Set a conversation's pending deferred calls (Phase 3c, Design §4): from
'AppEvent_CallsDeferred', or @[]@ on @run.started@
('System.Agents.TUI.Event.handleRunStarted').
-}
setConversationPending :: ConversationId -> [DeferredCallView] -> EventM N TuiState ()
setConversationPending convId calls = do
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c
            { _coreConversations =
                map
                    ( \conv ->
                        if conversationId conv == convId
                            then conv{conversationPending = calls}
                            else conv
                    )
                    (c ^. coreConversations)
            }

-- | @calls.deferred@: block the conversation and record its pending calls.
handleCallsDeferred :: ConversationId -> [DeferredCallView] -> EventM N TuiState ()
handleCallsDeferred convId calls = do
    updateConversationStatus convId ConversationStatus_BlockedOnDeferred
    setConversationPending convId calls

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

{- | A new session was created somewhere in this owner's tree
('AppEvent_SessionCreated', @session.created@): if it is a child of a
conversation we already know about, and we do not already have an entry
for it, add it -- this is how an agent-initiated @spawn-session@ shows up
without the TUI having started it itself.
-}
handleSessionCreated :: SessionMeta -> EventM N TuiState ()
handleSessionCreated meta = do
    core <- readCore
    let convs = core ^. coreConversations
        childConvId = sessionIdToConversationId meta.smSessionId
        alreadyKnown = any (\c -> conversationId c == childConvId) convs
    case (alreadyKnown, meta.smParent) of
        (False, Just parentSid) ->
            case [c | c <- convs, conversationSessionId c == parentSid] of
                (parentConv : _) -> do
                    let conv = mkConversation meta Nothing True (Just (conversationId parentConv)) (conversationSubcallDepth parentConv + 1)
                    addConversationToCore conv
                _ -> pure ()
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

-- | Interrupt the session with the message editor's content: 'Client.postMessage' with @nmInterrupt = True@.
handleInterruptConversation :: EventM N TuiState ()
handleInterruptConversation = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> sendMessageTo conv True

-- | Cancel every tool call currently attached to the focused session: 'Client.cancelAttachedCalls'.
handleCancelAttachedConversation :: EventM N TuiState ()
handleCancelAttachedConversation = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            core <- readCore
            result <- liftIO $ Client.cancelAttachedCalls (core ^. coreClient) (conversationSessionId conv)
            reportRunnerResult ("Cancelled attached calls for " <> conversationName conv) result

{- | Pause or unpause the focused session for real, over the mailbox:
'Client.pauseSession' when it is not paused yet, 'Client.resumeSession'
with 'UntilBlocked' (the mode the chat page's Resume button uses) when it
already is.
-}
handlePauseRunConversation :: EventM N TuiState ()
handlePauseRunConversation = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> do
            core <- readCore
            let client = core ^. coreClient
                sid = conversationSessionId conv
            if conv.conversationStatus == ConversationStatus_Paused
                then do
                    result <- liftIO $ Client.resumeSession client sid UntilBlocked mempty
                    reportRunnerResult ("Resumed " <> conversationName conv) result
                else do
                    result <- liftIO $ Client.pauseSession client sid
                    reportRunnerResult ("Paused " <> conversationName conv) result

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
