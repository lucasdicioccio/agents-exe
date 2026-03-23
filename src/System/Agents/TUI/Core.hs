{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{- | Main entry point for the TUI application.
This module re-exports functionality from the submodules and provides
the main initialization and application runner.

This version uses the RuntimeBridge from the OS compatibility layer
to work with the new OS model while maintaining backward compatibility.
-}
module System.Agents.TUI.Core (
    -- * Re-exports from Types
    WidgetName (..),
    N,
    AppEvent (..),
    TuiAgent (..),
    Conversation (..),
    ConversationStatus (..),
    AuxiliaryTask (..),
    Core,
    UIState (..),
    TuiState (..),
    SessionConfig (..),
    initUIState,
    initCore,
    updateConversationSession,
    updateConversation,

    -- * Lens accessors
    uiFocusRing,
    zoomed,
    agentList,
    conversationList,
    messageEditor,
    selectedAgentInfo,
    unreadConversations,
    ongoingConversations,
    auxiliaryTasks,
    coreAgents,
    coreConversations,
    corePausedConversations,
    coreBufferedMessages,
    tuiCore,
    tuiUI,
    eventChan,

    -- * Re-exports from Render
    tui_appDraw,
    tui_appAttrMap,
    focusedAttr,
    userMessageAttr,
    llmMessageAttr,

    -- * Re-exports from Event
    tui_appHandleEvent,

    -- * Session loading
    loadSessionFiles,

    -- * Main entry point
    runTUI,
    runTUIWithConfig,
    fileSessionConfig,

    -- * OS Bridge helpers
    createTuiAgentWithBridge,
) where

import Brick hiding (Down)
import Brick.BChan (newBChan, writeBChan)
import Brick.Focus (focusGetCurrent)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.))
import Control.Monad (forever, void)

import System.Agents.AgentTree (AgentTree (..), LoadAgentResult (..), Props, loadAgentTreeRuntime)
import System.Agents.Base (newAgentId)
import System.Agents.OS.Compat.Runtime (
    initializeOS,
    newRuntimeBridge,
 )
import System.Agents.Session.Base (Session (..))
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore

-- Import from submodules

import System.Agents.TUI.Event
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

-- | Create a session configuration with file-based persistence.
fileSessionConfig :: SessionStore -> SessionConfig
fileSessionConfig store =
    SessionConfig
        { sessionStore = store
        }

-------------------------------------------------------------------------------
-- OS Bridge Integration
-------------------------------------------------------------------------------

{- | Create a TuiAgent with a RuntimeBridge for OS compatibility.

This function initializes the OS compatibility layer and creates
a RuntimeBridge that allows the TUI to work with the new OS model
while maintaining backward compatibility with existing code.
-}
createTuiAgentWithBridge :: AgentTree -> IO TuiAgent
createTuiAgentWithBridge tree = do
    -- Initialize a minimal OS instance
    os <- initializeOS

    -- Create a new agent ID
    agentId <- newAgentId

    -- Create the RuntimeBridge
    let bridge = newRuntimeBridge agentId os

    -- Return the TUI agent
    pure $
        TuiAgent
            { tuiAgentId = agentId
            , tuiBridge = bridge
            , tuiTree = tree
            }

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

{- | Initialize the TUI with props and optional conversation prefix (legacy API).

For more control, use 'runTUIWithConfig' instead.
-}
runTUI :: SessionStore -> [Props] -> IO ()
runTUI store props =
    let config = fileSessionConfig store
     in runTUIWithConfig config props

-- | Initialize the TUI with a custom session configuration.
runTUIWithConfig :: SessionConfig -> [Props] -> IO ()
runTUIWithConfig config props = do
    -- Load agent trees and create TuiAgents with RuntimeBridge
    trees <- traverse loadAgentTreeRuntime props
    let itrees = [rt | Initialized rt <- trees]

    -- Create TUI agents with OS bridges
    tuiAgents <- traverse createTuiAgentWithBridge itrees

    -- Load existing session files (only if file prefix is provided)
    loadedSessions <- loadSessionFiles config.sessionStore

    -- Create event channel (needed for conversations)
    evChan <- newBChan 100

    -- Create core state with loaded conversations
    core0 <- initCore tuiAgents
    coreTVar <- newTVarIO core0

    -- Create UI state
    let ui0 = initUIState tuiAgents [s | (_, Just s) <- loadedSessions]

    -- Create TUI state with session configuration
    let st = TuiState coreTVar ui0 evChan config

    -- Build and run the app
    let app =
            App
                { appDraw = tui_appDraw
                , appChooseCursor = tui_appChooseCursor
                , appHandleEvent = tui_appHandleEvent
                , appStartEvent = tui_appStartEvent
                , appAttrMap = tui_appAttrMap
                }

    void $ forkIO $ forever $ do
        writeBChan evChan AppEvent_Heartbeat
        threadDelay 1000000
    void $ customMainWithDefaultVty (Just evChan) app st
