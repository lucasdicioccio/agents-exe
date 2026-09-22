{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Conversation lifecycle event handlers for the TUI.

This module handles conversation lifecycle, subcalls, new conversations,
and session management.
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
    handleSubcallProgress,
    handleSubcallCompleted,
    handleSubcallFailed,
    findAgentBySlug,

    -- * Conversation Updates
    handleConversationUpdated,
    handleToolCallActivity,
    handleConversationNeedsInput,
    updateConversationStatus,

    -- * Pause/Resume
    handleTogglePauseConversation,
    isConversationPaused,

    -- * Interrupt
    handleInterruptConversation,

    -- * Progress Callbacks
    buildOnProgress,
    readAndClearBufferedMessagesSTM,
    readAndClearBufferedMessages,
    addBufferedMessage,

    -- * Core State Manipulation
    appendConversation,
) where

import Brick
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Focus (focusSetCurrent)
import Brick.Widgets.Edit (editContentsL, getEditContents)
import Brick.Widgets.List (listInsert, listSelectedElement, listSelectedL)
import qualified Brick.Widgets.List as List
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVar, readTVarIO, writeTVar)
import Control.Lens (to, use, (%=), (.=), (^.))
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Zipper as TextZipper
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector

import Prod.Tracer (Tracer (..), contramap)

import System.Agents.Base (ConversationId (..), newConversationId)
import qualified System.Agents.OneShot as OneShot
import qualified System.Agents.Runtime.Trace as Runtime
import System.Agents.Session.Base (
    Action (..),
    Agent (..),
    MailBody (..),
    MissingUserPrompt (..),
    OnSessionProgress,
    Outgoing (..),
    Priority (..),
    Sender (..),
    Session (..),
    SessionId,
    SessionProgress (..),
    UserQuery (..),
    newSessionId,
    newTurnId,
 )
import qualified System.Agents.Session.Loop as Loop
import System.Agents.Session.Mailbox (MailRouter (..), MailScope (..), Mailbox (..), MailboxInfo (..), newInMemoryMailbox)
import System.Agents.OS.Events (ToolCallActivity)
import System.Agents.TUI.ToolCallActivity (applyToolCallActivity, pruneToolCallViews)
import System.Agents.TUI.Types (
    AppEvent (..),
    Conversation (..),
    ConversationStatus (..),
    Core (..),
    N,
    SessionConfig (..),
    StatusSeverity (..),
    Tab (..),
    TuiAgent (..),
    TuiState,
    WidgetName (..),
    agentList,
    attachedFiles,
    buildFocusRingForTab,
    conversationId,
    conversationList,
    conversationName,
    conversationParentId,
    conversationSession,
    conversationStatus,
    conversationSubcallDepth,
    coreBufferedMessages,
    coreConversations,
    coreMailRouter,
    coreOSEventQueue,
    corePausedConversations,
    coreWorld,
    currentTab,
    eventChan,
    messageEditor,
    queuedMessagesFocus,
    selectedAttachmentIndex,
    sessionConfig,
    sessionList,
    tuiCore,
    tuiNode,
    tuiSlug,
    toolCallViews,
    tuiUI,
    uiBufferedMessages,
    uiFocusRing,
    unreadConversations,
    updateConversationSession,
    zoomed,
 )
import System.Agents.Tools.Context (CallStackEntry (..))

-- | Trace type for TUI conversation events
data Trace
    = RuntimeTrace !Runtime.Trace
    | OneShotTrace !OneShot.Trace
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
handleNewConversationFromEditor :: Tracer IO Trace -> EventM N TuiState ()
handleNewConversationFromEditor tracer = do
    selected <- use (tuiUI . agentList . to listSelectedElement)
    case selected of
        Just (_, baseTuiAgent) -> do
            session <- liftIO (Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing <*> pure 0)
            runConversation tracer baseTuiAgent session
        _ -> pure ()

-- | Handle new conversation event.
handleNewConversation :: ConversationId -> EventM N TuiState ()
handleNewConversation convId = do
    convs <- use (tuiUI . conversationList . to List.listElements)
    case Vector.findIndex (\c -> conversationId c == convId) convs of
        Just idx -> do
            tuiUI . conversationList . listSelectedL .= Just idx
            tuiUI . unreadConversations %= Set.delete convId
        Nothing -> pure ()

-------------------------------------------------------------------------------
-- Restored Conversation
-------------------------------------------------------------------------------

-- | Continue a session restored from storage.
handleRestoredConversation :: Tracer IO Trace -> EventM N TuiState ()
handleRestoredConversation tracer = do
    mSession <- use (tuiUI . sessionList . to listSelectedElement)
    mAgent <- use (tuiUI . agentList . to listSelectedElement)
    case (,) <$> mSession <*> mAgent of
        Just ((_, session), (_, baseTuiAgent)) -> do
            runConversation tracer baseTuiAgent session
        _ -> pure ()

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
    outChan <- use eventChan
    coreRef <- use tuiCore
    roster <- Vector.toList . List.listElements <$> use (tuiUI . agentList)
    conv <- liftIO $ spawnConversationIO config tracer coreRef outChan roster baseTuiAgent session Nothing
    -- Insert the conversation at the top of the list and select it
    tuiUI . conversationList %= listInsert 0 conv
    tuiUI . conversationList . listSelectedL .= Just 0
    -- Mark as read since user just created it
    tuiUI . unreadConversations %= Set.delete conv.conversationId

    -- Switch to chats tab and focus message editor
    switchToChatsAndFocusMessage

{- | The plain-'IO' heart of 'runConversation' (Phase 4,
@todos/session-mailbox.md@ D10): builds the agent, its mailbox, and its
background thread, and appends the resulting 'Conversation' to 'Core'
(the shared 'TVar', not 'TuiState' -- see 'appendConversation'). Every
'EventM'-only concern (the visible widget list, focus, "unread" marking)
is deliberately left to the caller: 'handleHeartbeat' already rebuilds the
visible conversation list from 'Core' on every tick, so a conversation
appended here from outside 'EventM' (a @spawn-session@ hook, not a menu
action) still shows up, just one heartbeat later.

@mParentSid@ is the id of the session that caused this one to run, if any
(a @spawn-session@ call): recorded on the mailbox's own 'MailboxInfo' so
scope checks (@send-message@'s @subtree@, by default) see the lineage.
'Nothing' for a conversation the user started directly, exactly as before
this refactor.
-}
spawnConversationIO ::
    SessionConfig ->
    Tracer IO Trace ->
    TVar Core ->
    BChan AppEvent ->
    [TuiAgent] ->
    TuiAgent ->
    Session ->
    Maybe SessionId ->
    IO Conversation
spawnConversationIO config tracer coreRef outChan roster baseTuiAgent session mParentSid = do
    convId <- newConversationId
    inChan <- newBChan 100
    let notifyProgress = buildOnProgress convId outChan
    let node = tuiNode baseTuiAgent
    agent1 <- OneShot.nodeToAgent config.sessionStore Nothing convId (contramap OneShotTrace tracer) config.sessionApiKeys node

    -- Get the World and EventQueue from Core for subcall visibility
    coreState <- readTVarIO coreRef
    let mWorld = coreState ^. coreWorld
    let mEventQueue = coreState ^. coreOSEventQueue
    let router = coreState ^. coreMailRouter

    -- Phase 4 (@todos/session-mailbox.md@, D10/D12): every TUI conversation
    -- gets an in-memory mailbox (there is no @session_mail@ backend here,
    -- unlike the server) and registers it on the process's one 'MailRouter',
    -- keyed the same way the server keys a session -- so 'send-message'/
    -- 'watch-session' from another session (in this process, for now) can
    -- reach it. This is additive: 'usrQuery'/the conversation's own 'BChan'
    -- keep working exactly as they do today (Phase 1 kept the TUI on them
    -- deliberately); a mailbox only adds R1/R2's mail-folding on top.
    mailbox <- newInMemoryMailbox
    -- Keyed by the *session's* id ('ctxSessionId', what 'send-message'/
    -- 'spawn-session' address themselves and each other as -- see
    -- 'System.Agents.Session.Step.buildContext'), not 'convId': the two are
    -- independently-generated ids in this front-end (unlike the server,
    -- where a session's 'ConversationId' is always derived from its
    -- 'SessionId'), so registering under the wrong one would make this
    -- conversation unreachable by its own reported identity.
    let sid = session.sessionId
    -- The unregister action is intentionally discarded: this TUI has no
    -- "close conversation" action today, so a conversation's mailbox stays
    -- registered for the process's lifetime, same as its 'Conversation'
    -- entry in '_coreConversations' -- a conversation blocked on deferred
    -- calls, in particular, is still a valid send-message/watch-session
    -- target, so unregistering when its run loop merely stops looping
    -- would be wrong.
    _unregisterMail <-
        router.mrRegister
            sid
            MailboxInfo
                { miAgentSlug = Just (tuiSlug baseTuiAgent)
                , miParent = mParentSid
                , miStatus = "running"
                , miMailScope = MailScopeSubtree
                , miInterruptScope = MailScopeChildren
                }
            mailbox

    let notifyNeedInput = writeBChan outChan (AppEvent_AgentNeedsInput convId)

    -- Phase 4, §5: this conversation's own @spawn-session@ hook. @<slug>@
    -- is resolved against the roster of top-level agents the TUI already
    -- knows about (the same universe 'findAgentBySlug' searches for the
    -- agent-selection menu), not a strict child-of-this-node list -- the
    -- TUI, unlike @run@, has no single "root node" whose children define
    -- the caller's helpers. The spawned conversation runs the same way
    -- this one does (recursively, via 'spawnConversationIO'), so it too
    -- shows up in the conversation list at the next heartbeat and can
    -- itself spawn further descendants.
    let spawnHook slug text =
            case findAgentBySlug slug (Vector.fromList roster) of
                Nothing -> pure $ Left ("no such helper: " <> slug)
                Just childTuiAgent -> do
                    childSession <- Session [] <$> newSessionId <*> pure Nothing <*> newTurnId <*> pure (Just 1) <*> pure Nothing <*> pure 0
                    _childConv <- spawnConversationIO config tracer coreRef outChan roster childTuiAgent childSession (Just sid)
                    -- Post the initial message as ordinary mail: the
                    -- child's own R1 folds it into its first turn, the
                    -- same as any other 'send-message'.
                    mChildTarget <- router.mrLookup childSession.sessionId
                    forM_ mChildTarget $ \(_, childMailbox) ->
                        void $
                            childMailbox.mbSend
                                Outgoing
                                    { outId = Nothing
                                    , outFrom = FromSession sid (Just (tuiSlug baseTuiAgent))
                                    , outPriority = Normal
                                    , outHops = 0
                                    , outBody = UserMessage (UserQuery text [])
                                    }
                    pure $ Right childSession.sessionId

    -- Set the World and EventQueue on the agent for subcall visibility
    -- and initialize the call stack with a root entry
    let agentWithOS =
            agent1
                { ctxWorld = mWorld
                , ctxEventQueue = mEventQueue
                , ctxCallStack = [CallStackEntry "root" convId 0]
                , ctxMailbox = Just mailbox
                , ctxMailRouter = Just router
                , ctxSpawnSession = Just spawnHook
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
                        -- The LLM is done but background tool calls still run:
                        -- let the user talk meanwhile. The stepper races the
                        -- input against the calls and delivers their results
                        -- as soon as they finish.
                        AskUserPrompt (MissingUserPrompt False []) -> pure $ AskUserPrompt (MissingUserPrompt True [])
                        _ -> pure ret
                , usrQuery = do
                    core <- readTVarIO coreRef
                    buffered <- readAndClearBufferedMessages convId core
                    case buffered of
                        Nothing -> notifyNeedInput >> readBChan inChan
                        Just buftxt -> pure (Just $ UserQuery buftxt [])
                }
    threadId <- forkIO $ do
        notifyProgress (SessionStarted session)
        -- 'runUntilBlocked' rather than 'run': a turn waiting only on deferred
        -- calls cannot progress here, and looping on it would spin.
        outcome <- Loop.runUntilBlocked convId a session
        case outcome of
            Left _ -> pure ()
            Right blocked -> do
                notifyProgress (SessionUpdated blocked)
                writeBChan
                    outChan
                    ( AppEvent_ShowStatus
                        StatusWarning
                        "Conversation stopped: waiting for deferred tool calls. Complete them with the 'session' commands."
                    )
        notifyProgress (SessionCompleted session)
    let conv =
            Conversation
                { conversationId = convId
                , conversationAgent = baseTuiAgent
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
    atomically $ modifyTVar coreRef $ appendConversation conv
    pure conv

-- | Switch to the Chats tab and focus the message editor.
switchToChatsAndFocusMessage :: EventM N TuiState ()
switchToChatsAndFocusMessage = do
    tuiUI . currentTab .= ChatsTab
    tuiUI . uiFocusRing .= focusSetCurrent MessageEditorWidget (buildFocusRingForTab ChatsTab)
    tuiUI . zoomed .= False

-------------------------------------------------------------------------------
-- Send Message
-------------------------------------------------------------------------------

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
                isPaused <- Set.member convId . (^. corePausedConversations) <$> liftIO (readTVarIO coreRef)
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

-------------------------------------------------------------------------------
-- Subcall Management
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
    agents <- use (tuiUI . agentList . to List.listElements)
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
    let exists = any (\c -> conversationId c == subcallId) (coreState ^. coreConversations)
    if exists
        then do
            -- Update existing conversation
            liftIO $ atomically $ modifyTVar coreRef $ \c ->
                c{_coreConversations = updateConversationSession subcallId sess (c ^. coreConversations)}
        else do
            -- Conversation doesn't exist yet - heartbeat will pick it up
            let subcallShort = Text.take 8 (Text.pack $ show subcallId)
            showStatus StatusWarning $ "Subcall progress for unknown conv: " <> subcallShort

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
-- Conversation Updates
-------------------------------------------------------------------------------

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

-- | Handle conversation update event.
handleConversationUpdated :: ConversationId -> Session -> EventM N TuiState ()
handleConversationUpdated convId sess = do
    tuiUI . toolCallViews %= pruneToolCallViews sess
    coreRef <- use tuiCore
    liftIO $ atomically $ modifyTVar coreRef $ \c ->
        c{_coreConversations = updateConversationSession convId sess (c ^. coreConversations)}
    selected <- use (tuiUI . conversationList . to listSelectedElement)
    case selected of
        Just (_, conv)
            | conversationId conv /= convId ->
                tuiUI . unreadConversations %= Set.insert convId
        _ -> pure ()

-------------------------------------------------------------------------------
-- Pause/Resume
-------------------------------------------------------------------------------

-- | Toggle pause/unpause for the currently selected conversation.
handleTogglePauseConversation :: EventM N TuiState ()
handleTogglePauseConversation = do
    mSelectedConv <- use (tuiUI . conversationList . to listSelectedElement)
    case mSelectedConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just (_, conv) -> do
            let convId = conversationId conv
            coreRef <- use tuiCore
            isPaused <- Set.member convId . (^. corePausedConversations) <$> liftIO (readTVarIO coreRef)
            if isPaused
                then do
                    liftIO $ atomically $ modifyTVar coreRef $ \c ->
                        c{_corePausedConversations = Set.delete convId (c ^. corePausedConversations)}
                    updateConversationStatus convId ConversationStatus_WaitingForInput
                    tuiUI . queuedMessagesFocus .= Nothing
                    showStatus StatusInfo $ "Unpaused: " <> conversationName conv
                else do
                    liftIO $ atomically $ modifyTVar coreRef $ \c ->
                        c{_corePausedConversations = Set.insert convId (c ^. corePausedConversations)}
                    updateConversationStatus convId ConversationStatus_Paused
                    showStatus StatusInfo $ "Paused: " <> conversationName conv

-- | Check if a conversation is currently paused.
isConversationPaused :: ConversationId -> Core -> Bool
isConversationPaused convId core = Set.member convId (core ^. corePausedConversations)

-------------------------------------------------------------------------------
-- Interrupt
-------------------------------------------------------------------------------

{- | Interrupt the focused conversation's currently-attached tool calls
(@todos/session-mailbox.md@, R3a): posts an 'Interrupt'-priority envelope
to its own mailbox, which the stepper reacts to the next time it is
waiting on attached calls -- all of them are detached at once ("^Z then
talk") and the LLM is asked again with the interrupting mail and the
detached calls' placeholders folded into the next turn.

Only has an effect for a conversation whose agent runs in asynchronous
execution mode: a call in the (default) synchronous mode runs inline, with
no attached-call state for R3a to detach, so the mail is simply queued and
picked up (rendered, not acted on) at the conversation's next turn
boundary. This is a real, spec-inherent limitation, not a bug -- told to
the user via 'showStatus' rather than silently doing nothing.
-}
handleInterruptConversation :: EventM N TuiState ()
handleInterruptConversation = do
    mConv <- getFocusedConversation
    case mConv of
        Nothing -> showStatus StatusWarning "No conversation selected"
        Just conv -> case conv.conversationSession of
            Nothing -> showStatus StatusWarning "Conversation has no session yet"
            Just sess -> do
                coreRef <- use tuiCore
                core <- liftIO $ readTVarIO coreRef
                let router = core ^. coreMailRouter
                mTarget <- liftIO $ router.mrLookup sess.sessionId
                case mTarget of
                    Nothing -> showStatus StatusWarning "Conversation has no mailbox to interrupt"
                    Just (_, mailbox) -> do
                        sent <-
                            liftIO $
                                mailbox.mbSend
                                    Outgoing
                                        { outId = Nothing
                                        , outFrom = FromUser Nothing
                                        , outPriority = Interrupt
                                        , outHops = 0
                                        , outBody =
                                            UserMessage
                                                (UserQuery "(interrupted by user; any attached tool calls have been detached)" [])
                                        }
                        case sent of
                            Left _ -> showStatus StatusWarning "Could not send interrupt (mailbox full)"
                            Right _ ->
                                showStatus StatusInfo $
                                    "Interrupt sent to " <> conversationName conv <> ": attached tool calls will be detached shortly"

-------------------------------------------------------------------------------
-- Progress Callbacks
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
readAndClearBufferedMessagesSTM :: ConversationId -> TVar (Map.Map ConversationId [Text.Text]) -> STM [Text.Text]
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
    msgs <- atomically $ readAndClearBufferedMessagesSTM convId (core ^. coreBufferedMessages)
    pure $ case msgs of
        [] -> Nothing
        _ -> Just $ Text.unlines $ reverse msgs

-- | Add a message to the buffer for a conversation.
addBufferedMessage :: ConversationId -> Core -> Text.Text -> IO ()
addBufferedMessage convId core msg =
    atomically $ modifyTVar (core ^. coreBufferedMessages) $ Map.insertWith (\new old -> new ++ old) convId [msg]

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

