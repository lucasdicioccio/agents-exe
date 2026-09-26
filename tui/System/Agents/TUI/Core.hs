{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- | Main entry point for the TUI application.

This module re-exports functionality from the submodules and provides
the main initialization and application runner.

Phase 3b-ii (@todos/os-as-standalone-server.md@): the TUI is a client of
'System.Agents.Host.Client.RunnerClient'. There is no more agent-tree
loading or OS 'World' here (G1): 'System.Agents.CLI.TUI' builds a
'RunnerClient' over a 'System.Agents.Host.Runner.SessionRunner' (embedded
mode) or, eventually, an HTTP\/socket client (Phase 4), and hands it to
'runTUIInternal'. The event bridge ('bridgeRunnerEvents', Design §4) is the
one place left that reaches into the runner's own event stream: it turns
'System.Agents.Protocol.Event's from 'Client.subscribeAll' into 'AppEvent's
on the Brick 'Brick.BChan.BChan'; the heartbeat only re-reads the TUI's own
'Core' state, never the runner (no polling).
-}
module System.Agents.TUI.Core (
    -- Re-export Trace from Runtime.Trace
    Trace,

    -- * Re-exports from Types
    WidgetName (..),
    N,
    AppEvent (..),
    AttachmentDialogState (..),
    TuiAgent (..),
    Conversation (..),
    ConversationStatus (..),
    AuxiliaryTask (..),
    Core,
    UIState (..),
    TuiState,
    SessionConfig (..),
    Tab (..),
    mkSessionConfig,
    initUIState,
    initCore,
    updateConversationSession,

    -- * Lens accessors
    uiFocusRing,
    zoomed,
    agentList,
    conversationList,
    messageEditor,
    selectedAgentInfo,
    unreadConversations,
    auxiliaryTasks,
    attachedFiles,
    attachmentDialogState,
    filePathInput,
    fileBrowser,
    selectedAttachmentIndex,
    coreConversations,
    coreClient,
    coreParams,
    coreOwnedSessions,
    coreRunnerMode,
    RunnerMode (..),
    tuiCore,
    tuiUI,
    eventChan,
    currentTab,
    helpContent,
    keyMapping,
    sessionConfig,
    buffers,
    bufferFocus,

    -- * Re-exports from Buffer
    Buffer,
    BufferId,
    newBuffer,
    newBufferWithContent,
    updateBufferContent,

    -- * Re-exports from KeyMapping
    EventName (..),
    KeyName (..),
    Modifiers (..),
    KeyBinding (..),
    KeyMapping,
    TUIUserConfig (..),
    defaultKeyMapping,
    defaultTUIUserConfig,
    matchesEvent,
    generateHelpContent,
    loadKeymapFromFile,

    -- * Re-exports from MessageComposer
    SendTrigger (..),
    InputConfig (..),
    defaultInputConfig,
    shouldSendMessage,
    willSendOnNextNewline,
    stripSendTrigger,

    -- * Re-exports from Render
    tui_appDraw,
    tui_appAttrMap,
    focusedAttr,
    userMessageAttr,
    llmMessageAttr,

    -- * Re-exports from Event
    tui_appHandleEvent,
    cycleTabForward,
    cycleTabBackward,
    nextTab,
    prevTab,
    defaultHelpContent,
    initHelpContent,

    -- * Main entry points
    fileSessionConfig,
    runTUIWithUserConfig,
    runTUIInternal,
) where

import Brick hiding (Down)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Focus (focusGetCurrent)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, try)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Prod.Tracer (Tracer)

import System.Agents.Base (ConversationId)
import System.Agents.Host.Client (RunnerClient)
import qualified System.Agents.Host.Client as Client
import System.Agents.Host.Runner (ReplayUnavailable (..))
import qualified System.Agents.Protocol as Protocol
import System.Agents.Session.Base (SessionId)
import System.Agents.SessionStore (sessionIdToConversationId)
import System.Agents.Tools.Params.Types (ParamName)

-- Import from submodules
import System.Agents.TUI.Buffer (
    Buffer,
    BufferId,
    newBuffer,
    newBufferWithContent,
    updateBufferContent,
 )
import System.Agents.TUI.Event (
    Trace (..),
    cycleTabBackward,
    cycleTabForward,
    defaultHelpContent,
    initHelpContent,
    nextTab,
    prevTab,
    tui_appHandleEvent,
 )
import System.Agents.TUI.KeyMapping (
    EventName (..),
    KeyBinding (..),
    KeyMapping,
    KeyName (..),
    Modifiers (..),
    TUIUserConfig (..),
    defaultKeyMapping,
    defaultTUIUserConfig,
    generateHelpContent,
    loadKeymapFromFile,
    matchesEvent,
 )
import System.Agents.TUI.MessageComposer (
    InputConfig (..),
    SendTrigger (..),
    defaultInputConfig,
    shouldSendMessage,
    stripSendTrigger,
    willSendOnNextNewline,
 )
import System.Agents.TUI.Render
import System.Agents.TUI.Types

-------------------------------------------------------------------------------
-- Session Configuration
-------------------------------------------------------------------------------

-- | Create a session configuration from a user config: keymap and input config.
fileSessionConfig :: TUIUserConfig -> SessionConfig
fileSessionConfig userConfig =
    mkSessionConfig userConfig.userConfigKeymap userConfig.userConfigInput

-------------------------------------------------------------------------------
-- Application Setup
-------------------------------------------------------------------------------

-- | Choose cursor based on focus.
tui_appChooseCursor :: TuiState -> [CursorLocation N] -> Maybe (CursorLocation N)
tui_appChooseCursor st locs =
    case focusGetCurrent (st ^. tuiUI . uiFocusRing) of
        Just MessageEditorWidget -> showCursorNamed MessageEditorWidget locs
        Just FilePathInputWidget -> showCursorNamed FilePathInputWidget locs
        _ -> Nothing

-- | Start event (no-op).
tui_appStartEvent :: EventM N TuiState ()
tui_appStartEvent = pure ()

-------------------------------------------------------------------------------
-- Main Entry Points
-------------------------------------------------------------------------------

{- | Build the full session configuration and run the TUI over a
'RunnerClient' (Design §3): embedded mode wraps a 'SessionRunner' this
process started itself ('System.Agents.CLI.TUI.handleTUI'); a future
@--attach@ mode (Phase 4) hands in an @httpClient@\/@socketClient@
instead. Neither this function nor anything it calls can tell which.
-}
runTUIWithUserConfig :: Tracer IO Trace -> RunnerMode -> RunnerClient -> TUIUserConfig -> Map ParamName Aeson.Value -> IO ()
runTUIWithUserConfig tracer mode client userConfig params = do
    let config = fileSessionConfig userConfig
    runTUIInternal tracer mode client config params

{- | Fetch the agent roster from the client (G6: 'Client.listAgents', which
already carries model, prompt and tool activation -- no live OS-native
handle needed) and start Brick.

The History tab's session list starts empty: it is only populated
starting 3b-iii, via 'Client.listSessions'.
-}
runTUIInternal :: Tracer IO Trace -> RunnerMode -> RunnerClient -> SessionConfig -> Map ParamName Aeson.Value -> IO ()
runTUIInternal tracer mode client config params = do
    descriptors <- either (const []) id <$> Client.listAgents client
    let tuiAgents = map TuiAgent descriptors :: [TuiAgent]

    core0 <- initCore mode client params
    coreTVar <- newTVarIO core0

    let helpText = generateHelpContent (sessionKeyMapping config)
        ui0 = initUIState helpText tuiAgents []

    evChan <- newBChan 100
    let st = TuiState coreTVar ui0 evChan config (sessionKeyMapping config)

    let app =
            App
                { appDraw = tui_appDraw
                , appChooseCursor = tui_appChooseCursor
                , appHandleEvent = tui_appHandleEvent tracer
                , appStartEvent = tui_appStartEvent
                , appAttrMap = tui_appAttrMap
                }

    subResult <- Client.subscribeAll client
    case subResult of
        Left ReplayUnavailable -> pure ()
        Right sub -> void $ forkIO $ bridgeRunnerEvents client evChan sub

    void $ forkIO $ forever $ do
        writeBChan evChan AppEvent_Heartbeat
        threadDelay 1000000
    void $ customMainWithDefaultVty (Just evChan) app st

-------------------------------------------------------------------------------
-- The runner event bridge (Design §4)
-------------------------------------------------------------------------------

{- | Bridge every 'System.Agents.Protocol.Event' the runner emits, across
every session, into an 'AppEvent' on the TUI's own 'BChan'. This is the
only place the TUI still reaches into the runner's live stream: everything
else reacts to the 'AppEvent's this produces
("System.Agents.TUI.Event"\/"System.Agents.TUI.Event.Conversation").

* @session.updated@ needs a follow-up 'Client.getSession' (the event
  itself only carries the fresh 'System.Agents.SessionStore.SessionMeta'
  and, optionally, the head turn -- not the full 'System.Agents.Session.Base.Session'
  a 'Conversation' renders from).
* @subcall.started@\/@completed@\/@failed@ and @tool.progressed@ map
  straight onto their 'AppEvent' counterparts, translating every
  'System.Agents.Session.Base.SessionId' to its
  'System.Agents.Base.ConversationId' (D4: they are the same UUID).
* @text.delta@ is ignored: the TUI does not stream tokens.
* Any exception from a single iteration (e.g. the session was deleted
  between the event and the follow-up 'Client.getSession') ends this loop
  rather than crashing the TUI; the bridge simply stops delivering events
  from then on -- a future @--attach@ client (Phase 4) would reconnect,
  which this in-process bridge has no need to.

The thread this runs on is never explicitly killed: 'Client.subClose' on
'inProcessClient' is a no-op (nothing to release, its subscription is a
'Control.Concurrent.STM.TChan' duplicate), and quitting the TUI ends the
process, which ends every thread with it -- the same way the old heartbeat
thread was never explicitly stopped either.
-}
bridgeRunnerEvents :: RunnerClient -> BChan AppEvent -> Client.Subscription -> IO ()
bridgeRunnerEvents client chan sub = loop
  where
    loop = do
        result <- try (Client.subNext sub) :: IO (Either SomeException Protocol.Event)
        case result of
            Left _ -> pure ()
            Right ev -> do
                deliver ev
                loop

    deliver :: Protocol.Event -> IO ()
    deliver ev = case ev.evBody of
        Protocol.SessionUpdated _meta _headTurn -> withSid ev $ \sid -> do
            fresh <- Client.getSession client sid
            case fresh of
                Right (sess, meta) -> writeBChan chan (AppEvent_SessionUpdated (sessionIdToConversationId sid) sess meta)
                Left _ -> pure ()
        Protocol.RunStarted mode -> withConvId ev $ \cid -> AppEvent_RunStarted cid mode
        Protocol.RunStopped status -> withConvId ev $ \cid -> AppEvent_RunStopped cid status
        Protocol.SessionFailed msg -> withConvId ev $ \cid -> AppEvent_SessionFailed cid msg
        Protocol.CallsDeferred calls -> withConvId ev $ \cid -> AppEvent_CallsDeferred cid calls
        Protocol.SessionCreated meta -> writeBChan chan (AppEvent_SessionCreated meta)
        Protocol.SessionDeleted sid -> writeBChan chan (AppEvent_SessionDeleted (sessionIdToConversationId sid))
        Protocol.SubcallStarted parentSid childSid slug depth ->
            writeBChan chan $
                AppEvent_SubcallStarted
                    { appSubcallParentId = sessionIdToConversationId parentSid
                    , appSubcallId = sessionIdToConversationId childSid
                    , appSubcallAgentSlug = slug
                    , appSubcallDepth = depth
                    }
        Protocol.SubcallCompleted childSid result ->
            writeBChan chan (AppEvent_SubcallCompleted (sessionIdToConversationId childSid) (fromMaybe "" result))
        Protocol.SubcallFailed childSid msg ->
            writeBChan chan (AppEvent_SubcallFailed (sessionIdToConversationId childSid) msg)
        Protocol.ToolCallProgressed activity -> writeBChan chan (AppEvent_ToolCallActivity activity)
        Protocol.TextDelta _ -> pure ()
        Protocol.ToolCallStarted{} -> pure ()
        Protocol.ToolCallCompleted{} -> pure ()
        -- A hook failure is not a session failure -- the run continues --
        -- so it is surfaced as a warning in the status bar.
        Protocol.HookFailed msg ->
            writeBChan chan (AppEvent_ShowStatus StatusWarning (hookFailedStatusText msg))

    withSid :: Protocol.Event -> (SessionId -> IO ()) -> IO ()
    withSid ev f = maybe (pure ()) f ev.evSession

    withConvId :: Protocol.Event -> (ConversationId -> AppEvent) -> IO ()
    withConvId ev f = withSid ev (writeBChan chan . f . sessionIdToConversationId)
