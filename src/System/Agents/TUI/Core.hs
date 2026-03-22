{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{- | Main entry point for the TUI application.
This module re-exports functionality from the submodules and provides
the main initialization and application runner.
-}
module System.Agents.TUI.Core (
    -- * Re-exports from Types
    WidgetName (..),
    N,
    AppEvent (..),
    TuiAgent (..),
    Conversation (..),
    ConversationStatus (..),
    ConversationNode (..),
    buildConversationTree,
    makeSubAgentCallback,
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
    coreSubAgentCallbacks,
    conversationTreeExpanded,
    selectedConversationPath,
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
) where

import Brick hiding (Down)
import Brick.BChan (newBChan, writeBChan)
import Brick.Focus (focusGetCurrent)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.))
import Control.Monad (forever, void)

import System.Agents.AgentTree (AgentTree (..), LoadAgentResult (..), Props, agentRuntime, loadAgentTreeRuntime)
import System.Agents.Base (newConversationId)
import System.Agents.OneShot (runtimeToAgent)
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
    -- Load agent trees and create TuiAgents
    trees <- traverse loadAgentTreeRuntime props
    let itrees = [rt | Initialized rt <- trees]
    -- Generate a conversation ID for each agent and create session agents
    sessionAgents <- traverse createAgentForTree itrees
    let tuiAgents = zipWith TuiAgent sessionAgents itrees

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
  where
    -- Create an agent for a given tree with a fresh conversation ID
    createAgentForTree itree = do
        convId <- newConversationId
        runtimeToAgent config.sessionStore Nothing convId (agentRuntime itree)
