{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- | Main entry point for the TUI application.

This module re-exports functionality from the submodules and provides
the main initialization and application runner.

Phase 3b-i (@todos/os-as-standalone-server.md@): the TUI is a client of
'System.Agents.Host.Client.RunnerClient'. There is no more agent-tree
loading, OS 'World', or OS-event bridge here (G1): 'System.Agents.CLI.TUI'
builds a 'RunnerClient' over a 'System.Agents.Host.Runner.SessionRunner'
(embedded mode) or, eventually, an HTTP\/socket client (Phase 4), and hands
it to 'runTUIInternal'.
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
import Brick.BChan (newBChan, writeBChan)
import Brick.Focus (focusGetCurrent)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import Prod.Tracer (Tracer)

import System.Agents.Host.Client (RunnerClient)
import qualified System.Agents.Host.Client as Client
import System.Agents.SessionStore (SessionStore)
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

{- | Create a session configuration from a user config: the legacy file
store (a read fallback for history), plus the keymap and input config.
-}
fileSessionConfig :: SessionStore -> TUIUserConfig -> SessionConfig
fileSessionConfig store userConfig =
    mkSessionConfig store userConfig.userConfigKeymap userConfig.userConfigInput

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
runTUIWithUserConfig :: Tracer IO Trace -> RunnerClient -> SessionStore -> TUIUserConfig -> Map ParamName Aeson.Value -> IO ()
runTUIWithUserConfig tracer client store userConfig params = do
    let config = fileSessionConfig store userConfig
    runTUIInternal tracer client config params

{- | Fetch the agent roster from the client (G6: 'Client.listAgents', which
already carries model, prompt and tool activation -- no live OS-native
handle needed) and start Brick.

The History tab's session list starts empty: it is only populated
starting 3b-iii, via 'Client.listSessions'.
-}
runTUIInternal :: Tracer IO Trace -> RunnerClient -> SessionConfig -> Map ParamName Aeson.Value -> IO ()
runTUIInternal tracer client config params = do
    descriptors <- either (const []) id <$> Client.listAgents client
    let tuiAgents = map TuiAgent descriptors :: [TuiAgent]

    core0 <- initCore client params
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

    void $ forkIO $ forever $ do
        writeBChan evChan AppEvent_Heartbeat
        threadDelay 1000000
    void $ customMainWithDefaultVty (Just evChan) app st
