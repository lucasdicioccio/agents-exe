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
) where

import Brick hiding (Down)
import Brick.BChan (BChan, newBChan, readBChan, writeBChan)
import Brick.Focus (focusGetCurrent)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.))
import Control.Monad (void, forever)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down(..))
import Data.Time (UTCTime)
import System.Directory (listDirectory, doesFileExist, getModificationTime)
import System.FilePath ((</>))

import System.Agents.AgentTree (AgentTree(..), LoadAgentResult(..), Props, loadAgentTreeRuntime, agentRuntime)
import System.Agents.Base (ConversationId(..), newConversationId)
import System.Agents.OneShot (runtimeToAgent, readSession, storeSession)
import qualified System.Agents.Runtime as Runtime
import System.Agents.Session.Base (Session(..))

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

-- | Session file info with metadata for sorting.
data SessionFileInfo = SessionFileInfo
    { sessionFilePath :: FilePath
    , sessionModTime :: UTCTime
    }

-- | Check if a filename matches the session file pattern.
-- Pattern: conv.<uuid>.json
isSessionFile :: String -> Bool
isSessionFile name =
    "conv." `isPrefixOf` name && ".json" `isSuffixOf` name
  where
    isSuffixOf suffix str0 = reverse suffix `isPrefixOf` reverse str0

-- | Find all session files in the given directory matching the pattern.
findSessionFiles :: FilePath -> IO [SessionFileInfo]
findSessionFiles dir = do
    entries <- listDirectory dir
    let candidates = [dir </> entry | entry <- entries, isSessionFile entry]
    -- Filter to only existing files and get modification times
    existing <- filterM doesFileExist candidates
    mapM (\path -> SessionFileInfo path <$> getModificationTime path) existing

-- | Load all sessions from files matching the prefix pattern.
-- Returns a list of (FilePath, Maybe Session) pairs.
loadSessionFiles :: FilePath -> IO [(FilePath, Maybe Session)]
loadSessionFiles convPrefix = do
    -- Get directory from prefix (if prefix is a path, use its directory)
    let dir = if '/' `elem` convPrefix || '\\' `elem` convPrefix
                then case reverse $ dropWhile (\c -> c /= '/' && c /= '\\') $ reverse convPrefix of
                       "" -> "."
                       d -> d
                else "."
    
    sessionFiles <- findSessionFiles dir
    -- Sort by modification time (most recent first)
    let sortedFiles = map sessionFilePath $ sortOn (Data.Ord.Down . sessionModTime) sessionFiles
    
    -- Load each session file
    mapM (\path -> (path,) <$> readSession path) sortedFiles

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\x acc -> do
    b <- p x
    if b then (x :) <$> acc else acc) (return [])

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Initialize the TUI with props.
runTUI :: Maybe FilePath -> [Props] -> IO ()
runTUI mConvPrefix props = do
    let convPrefix = fromMaybe "./" mConvPrefix
    -- Load agent trees and create TuiAgents
    trees <- traverse loadAgentTreeRuntime props
    let itrees = [rt | Initialized rt <- trees]
    sessionAgents <- traverse (runtimeToAgent . agentRuntime) itrees
    let tuiAgents = zipWith TuiAgent sessionAgents itrees

    -- Load existing session files
    loadedSessions <- loadSessionFiles convPrefix
    
    -- Create event channel (needed for conversations)
    evChan <- newBChan 100

    -- Create core state with loaded conversations
    core0 <- newTVarIO (Core tuiAgents mempty)

    -- Create UI state
    let ui0 = initUIState tuiAgents [s | (_,Just s) <- loadedSessions]

    -- Create TUI state
    let st = TuiState core0 ui0 evChan

    -- Build and run the app
    let app =
            App
                { appDraw = tui_appDraw
                , appChooseCursor = tui_appChooseCursor
                , appHandleEvent = tui_appHandleEvent convPrefix
                , appStartEvent = tui_appStartEvent
                , appAttrMap = tui_appAttrMap
                }

    void $ forkIO $ forever $ do
      writeBChan evChan AppEvent_Heartbeat
      threadDelay 1000000
    void $ customMainWithDefaultVty (Just evChan) app st

