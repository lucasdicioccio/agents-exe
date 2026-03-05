{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Main entry point for the TUI application.
-- This module re-exports functionality from the submodules and provides
-- the main initialization and application runner.
module System.Agents.TUI2.Core (
    -- * Re-exports from Types
    WidgetName(..),
    N,
    AppEvent(..),
    TuiAgent(..),
    Conversation(..),
    ConversationStatus(..),
    Core(..),
    UIState(..),
    TuiState(..),
    SessionConfig(..),
    defaultSessionConfig,
    initUIState,
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
import Control.Monad (void, forever)

import System.Agents.AgentTree (AgentTree(..), LoadAgentResult(..), Props, loadAgentTreeRuntime, agentRuntime)
import System.Agents.OneShot (runtimeToAgent)
import System.Agents.Session.Base (Session(..), ignoreSessionProgress)
import qualified System.Agents.SessionStore as SessionStore

-- Import from submodules
import System.Agents.TUI2.Types
import System.Agents.TUI2.Render
import System.Agents.TUI2.Event

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

-- | Load all sessions from files matching the prefix pattern.
-- Returns a list of (FilePath, Maybe Session) pairs.
--
-- This function uses 'SessionStore' internally to discover and load sessions.
loadSessionFiles :: FilePath -> IO [(FilePath, Maybe Session)]
loadSessionFiles convPrefix = do
    let store = SessionStore.mkSessionStore convPrefix
    sessions <- SessionStore.listSessions store
    -- Convert to the legacy format (filepath, Maybe Session)
    pure [(path, mSess) | (path, mSess, _) <- sessions]

-------------------------------------------------------------------------------
-- Session Configuration Helpers
-------------------------------------------------------------------------------

-- | Create a session configuration with file-based persistence.
fileSessionConfig :: FilePath -> SessionConfig
fileSessionConfig prefix = SessionConfig
    { sessionOnProgress = ignoreSessionProgress  -- Will be set per-conversation with specific path
    , sessionFilePrefix = Just prefix
    }

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Initialize the TUI with props and optional conversation prefix (legacy API).
-- 
-- For more control, use 'runTUIWithConfig' instead.
runTUI :: Maybe FilePath -> [Props] -> IO ()
runTUI mConvPrefix props = 
    let config = case mConvPrefix of
          Nothing -> defaultSessionConfig
          Just prefix -> fileSessionConfig prefix
    in runTUIWithConfig config props

-- | Initialize the TUI with a custom session configuration.
runTUIWithConfig :: SessionConfig -> [Props] -> IO ()
runTUIWithConfig config props = do
    -- Load agent trees and create TuiAgents
    trees <- traverse loadAgentTreeRuntime props
    let itrees = [rt | Initialized rt <- trees]
    sessionAgents <- traverse (runtimeToAgent . agentRuntime) itrees
    let tuiAgents = zipWith TuiAgent sessionAgents itrees

    -- Load existing session files (only if file prefix is provided)
    loadedSessions <- case config.sessionFilePrefix of
        Just prefix -> loadSessionFiles prefix
        Nothing -> pure []
    
    -- Create event channel (needed for conversations)
    evChan <- newBChan 100

    -- Create core state with loaded conversations
    core0 <- newTVarIO (Core tuiAgents mempty)

    -- Create UI state
    let ui0 = initUIState tuiAgents [s | (_,Just s) <- loadedSessions]

    -- Create TUI state with session configuration
    let st = TuiState core0 ui0 evChan config

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

