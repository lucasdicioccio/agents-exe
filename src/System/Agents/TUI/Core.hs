{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- | Main entry point for the TUI application.

This module re-exports functionality from the submodules and provides
the main initialization and application runner using OS-native structures.
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
    uiAgentTools,
    attachedFiles,
    attachmentDialogState,
    filePathInput,
    fileBrowser,
    selectedAttachmentIndex,
    coreConversations,
    corePausedConversations,
    coreBufferedMessages,
    tuiCore,
    tuiUI,
    eventChan,
    currentTab,
    helpContent,

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

    -- * Session loading
    loadSessionFiles,

    -- * Main entry point
    runTUI,
    runTUIWithConfig,
    fileSessionConfig,

    -- * OS-native helpers
    createTuiAgent,
    refreshAgentTools,
    getAgentTools,
) where

import Brick hiding (Down)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Focus (focusGetCurrent)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, TQueue, atomically, newTQueueIO, newTVarIO, readTQueue, readTVarIO)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import Data.Proxy (Proxy (..))
import Prod.Tracer (Tracer)

import System.Agents.AgentTree (
    LoadAgentResult (..),
    LoadedApiKeys,
    OSAgentNode (..),
    OSAgentTree (..),
    Props,
    loadAgentTree,
 )
import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Base (AgentId (..))
import System.Agents.OS.Conversation (
    ConversationConfig,
    ConversationState,
    Lineage (..),
 )
import System.Agents.OS.Core.World (World, newWorld, registerComponentStore)
import System.Agents.OS.Events (OSEvent (..))
import System.Agents.Session.Base (Session (..))
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (ToolRegistration)

-- Import from submodules
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
import System.Agents.TUI.Render
import System.Agents.TUI.Types

-------------------------------------------------------------------------------
-- Cursor and Start Event
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
-- Session File Loading
-------------------------------------------------------------------------------

{- | Load all sessions from files matching the prefix pattern.
Returns a list of (FilePath, Maybe Session) pairs.

This function uses 'SessionStore' internally to discover and load sessions.
-}
loadSessionFiles :: SessionStore -> IO [(FilePath, Maybe Session)]
loadSessionFiles store = do
    sessions <- SessionStore.listSessions store
    -- Convert to the legacy format (filepath, Maybe Session)
    pure [(path, mSess) | (path, mSess, _) <- sessions]

-------------------------------------------------------------------------------
-- Session Configuration Helpers
-------------------------------------------------------------------------------

{- | Create a session configuration with file-based persistence.
| Create a session configuration with file-based persistence.
-}
fileSessionConfig :: SessionStore -> LoadedApiKeys -> SessionConfig
fileSessionConfig store apiKeys =
    SessionConfig
        { sessionStore = store
        , sessionApiKeys = apiKeys
        }

{- | Initialize the TUI with props and optional conversation prefix (legacy API).
For more control, use 'runTUIWithConfig' instead.

This function:
1. Loads agent trees from the provided props
2. Creates TuiAgents with OS-native structures
3. Initializes the TUI with the agents and loaded sessions
-}
runTUI :: Tracer IO Trace -> SessionStore -> LoadedApiKeys -> [Props] -> IO ()
runTUI tracer store apiKeys props = do
    let config = fileSessionConfig store apiKeys
    runTUIWithConfig tracer config props

-- | Initialize the TUI with a custom session configuration.
runTUIWithConfig :: Tracer IO Trace -> SessionConfig -> [Props] -> IO ()
runTUIWithConfig tracer config props = do
    -- Load agent trees and create TuiAgents
    trees <- traverse loadAgentTree props
    let itrees = [tree | Initialized tree <- trees]

    -- Create TUI agents from OS-native trees
    let tuiAgents = map createTuiAgent itrees

    -- Load existing session files
    loadedSessions <- loadSessionFiles config.sessionStore

    -- Extract successfully loaded sessions
    let sessions = [sess | (_, Just sess) <- loadedSessions]

    -- Collect tools from all agents (read from their TVars)
    agentTools <- collectAgentTools tuiAgents

    -- Create event channel (needed for conversations)
    evChan <- newBChan 100

    -- Create OS event queue for subcall visibility
    osEventQueue <- newTQueueIO

    -- Start the event bridge
    startOSEventBridge osEventQueue evChan

    -- Create and initialize the OS World
    world <- atomically initWorld

    -- Create core state with World and EventQueue for subcall visibility
    core0 <- initCore (Just world) (Just osEventQueue)
    coreTVar <- newTVarIO core0

    -- Create UI state with loaded sessions and collected tools
    -- Also initialize help content with keyboard shortcuts
    let ui0 =
            (initUIState initHelpContent tuiAgents sessions)
                { _uiAgentTools = agentTools
                }

    -- Create TUI state with session configuration
    let st = TuiState coreTVar ui0 evChan config

    -- Build and run the app
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

{- | Create a TuiAgent from an OSAgentTree.

This function extracts the root agent from the tree and creates
a TuiAgent that provides direct access to OS-native structures.
-}
createTuiAgent :: OSAgentTree -> TuiAgent
createTuiAgent tree =
    let rootNode = osTreeRoot tree
     in TuiAgent
            { tuiAgentId = rootNode.osNodeAgentId
            , tuiTree = tree
            , tuiNode = rootNode
            , tuiSlug = rootNode.osNodeConfig.slug
            }

{- | Refresh tools for a TuiAgent.

This function reads the current tools from the OS-native TVar.
-}
refreshAgentTools :: TuiAgent -> IO [ToolRegistration]
refreshAgentTools agent =
    readTVarIO (osNodeTools agent.tuiNode)

-- | Get tools for a TuiAgent from the OS-native TVar.
getAgentTools :: TuiAgent -> IO [ToolRegistration]
getAgentTools = refreshAgentTools

-- | Collect tools from all TuiAgents.
collectAgentTools :: [TuiAgent] -> IO [(AgentId, [ToolRegistration])]
collectAgentTools agents = mapM collectTools agents
  where
    collectTools agent = do
        tools <- getAgentTools agent
        pure (tuiAgentId agent, tools)

-------------------------------------------------------------------------------
-- OSEvent to AppEvent Bridge
-------------------------------------------------------------------------------

{- | Convert an OSEvent to an AppEvent.

This function bridges OS events to TUI application events, enabling
subcall visibility and lifecycle tracking in the TUI.
-}
convertOSEvent :: OSEvent -> Maybe AppEvent
convertOSEvent (OSEvent_SubcallStarted parentId convId slug depth) =
    Just $ AppEvent_SubcallStarted parentId convId slug depth
convertOSEvent (OSEvent_SubcallProgress convId session) =
    Just $ AppEvent_SubcallProgress convId session
convertOSEvent (OSEvent_SubcallCompleted convId result) =
    Just $ AppEvent_SubcallCompleted convId result
convertOSEvent (OSEvent_SubcallFailed convId err) =
    Just $ AppEvent_SubcallFailed convId err
convertOSEvent _ = Nothing

{- | Start a background thread that bridges OSEvents to AppEvents.

This thread reads from the OSEvent queue and writes converted AppEvents
to the Brick event channel, enabling the TUI to respond to subcall events.
-}
startOSEventBridge :: TQueue OSEvent -> BChan AppEvent -> IO ()
startOSEventBridge osEventQueue appEventChan = void $ forkIO $ forever $ do
    osEvent <- atomically $ readTQueue osEventQueue
    case convertOSEvent osEvent of
        Just appEvent -> writeBChan appEventChan appEvent
        Nothing -> pure () -- Ignore non-TUI events

-------------------------------------------------------------------------------
-- World Initialization
-------------------------------------------------------------------------------

{- | Initialize the OS World with required component stores.

This creates a World and registers the component types needed for
subcall conversation tracking.
-}
initWorld :: STM World
initWorld = do
    world <- newWorld
    -- Register component stores for conversation tracking
    world1 <- registerComponentStore world (Proxy @ConversationConfig)
    world2 <- registerComponentStore world1 (Proxy @ConversationState)
    world3 <- registerComponentStore world2 (Proxy @Lineage)
    pure world3
