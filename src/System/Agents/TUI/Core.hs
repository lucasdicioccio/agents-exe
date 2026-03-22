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
    runTUIWithCallback,
    fileSessionConfig,
) where

import Brick hiding (Down)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Focus (focusGetCurrent)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import Prod.Tracer (Tracer)

import System.Agents.AgentTree (AgentTree (..), LoadAgentResult (..), Props (..), agentRuntime, loadAgentTreeRuntime, agentToTool)
import System.Agents.Base (AgentId, AgentSlug, newConversationId)
import System.Agents.OneShot (runtimeToAgent)
import System.Agents.Runtime (Runtime)
import System.Agents.Runtime.Trace (Trace)
import System.Agents.Session.Base (Session (..))
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.ToolRegistration (ToolRegistration)

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

For more control, use 'runTUIWithConfig' or 'runTUIWithCallback' instead.
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

{- | Initialize the TUI with a callback for creating the agentToTool function.

This version allows the agentToTool function to be created with access to the
event channel, enabling sub-agent progress callbacks that emit TUI events.

The callback receives the event channel and returns the agentToTool function
that will be used when wiring up tool references for sub-agents.
-}
runTUIWithCallback ::
    -- | Session store for persistence
    SessionStore ->
    -- | Callback that receives the event channel and returns the agentToTool function
    (BChan AppEvent -> Tracer IO Trace -> Runtime -> AgentSlug -> AgentId -> ToolRegistration) ->
    -- | List of agent props (will be modified with the callback-based agentToTool)
    [Props] ->
    IO ()
runTUIWithCallback store mkAgentToTool props = do
    -- Create event channel first so we can pass it to the callback
    evChan <- newBChan 100

    -- Create the agentToTool function using the callback
    let newAgentToTool = mkAgentToTool evChan

    -- Modify props to use the callback-based agentToTool
    let propsWithCallback = map (setAgentToTool newAgentToTool) props

    -- Load agent trees and create TuiAgents
    trees <- traverse loadAgentTreeRuntime propsWithCallback
    let itrees = [rt | Initialized rt <- trees]
    -- Generate a conversation ID for each agent and create session agents
    sessionAgents <- traverse createAgentForTree itrees
    let tuiAgents = zipWith TuiAgent sessionAgents itrees

    -- Load existing session files
    loadedSessions <- loadSessionFiles store

    -- Create core state with loaded conversations
    core0 <- initCore tuiAgents
    coreTVar <- newTVarIO core0

    -- Create UI state
    let ui0 = initUIState tuiAgents [s | (_, Just s) <- loadedSessions]

    -- Create session config
    let config = fileSessionConfig store

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
        runtimeToAgent store Nothing convId (agentRuntime itree)

    -- Update the agentToTool field in Props
    setAgentToTool :: (Tracer IO Trace -> Runtime -> AgentSlug -> AgentId -> ToolRegistration) -> Props -> Props
    setAgentToTool f p = p{agentToTool = f}

