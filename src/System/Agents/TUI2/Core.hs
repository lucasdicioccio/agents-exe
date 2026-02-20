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
    isConversationRestored,
    canContinueConversation,
    
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
import Brick.Widgets.List (list)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Directory (listDirectory, doesFileExist, getModificationTime)
import System.FilePath (takeFileName, (</>))

import System.Agents.AgentTree (AgentTree(..), LoadAgentResult(..), Props, loadAgentTreeRuntime, agentRuntime)
import System.Agents.Base (ConversationId(..), newConversationId)
import System.Agents.OneShot (runtimeToAgent, readSession)
import System.Agents.Runtime (Runtime)
import qualified System.Agents.Runtime as Runtime
import System.Agents.Session.Base (Session(..), Agent(..))

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
    isSuffixOf suffix str = reverse suffix `isPrefixOf` reverse str

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
    eventChan <- newBChan 100
    
    -- Get the selected agent (first one in the list, if any)
    let mSelectedAgent = case tuiAgents of
            [] -> Nothing
            (a:_) -> Just a
    
    -- Create restored conversations from loaded sessions
    -- Uses the selected agent and forks conversations with new IDs
    restoredConversations <- concat <$> mapM (makeRestoredConversation eventChan mSelectedAgent convPrefix) loadedSessions

    -- Create core state with loaded conversations
    core0 <- newTVarIO (Core tuiAgents restoredConversations)

    -- Create UI state
    let ui0 = (initUIState tuiAgents)
            { _conversationList = list ConversationListWidget (Vector.fromList restoredConversations) 1
            }

    -- Create TUI state
    let st = TuiState core0 ui0 eventChan

    -- Build and run the app
    let app =
            App
                { appDraw = tui_appDraw
                , appChooseCursor = tui_appChooseCursor
                , appHandleEvent = tui_appHandleEvent convPrefix
                , appStartEvent = tui_appStartEvent
                , appAttrMap = tui_appAttrMap
                }

    void $ customMainWithDefaultVty (Just eventChan) app st

-- | Create a restored conversation from a loaded session.
-- Uses the provided selected agent (or returns empty if none available).
-- Forks the conversation by generating a new conversation ID.
makeRestoredConversation :: BChan AppEvent -> Maybe TuiAgent -> FilePath -> (FilePath, Maybe Session) -> IO [Conversation]
makeRestoredConversation _ Nothing _ _ = pure []  -- No agent available
makeRestoredConversation _ _ _ (_, Nothing) = pure []  -- Failed to load session
makeRestoredConversation outChan (Just selectedAgent) _convPrefix (path, Just session) = do
    -- Create a channel for this conversation
    inChan <- newBChan 100
    
    -- Generate a new conversation ID to fork the conversation
    convId <- newConversationId
    
    -- Create agent with session restoration capability using the selected agent
    let baseAgentTree = agentTree selectedAgent
    agent0 <- runtimeToAgent (agentRuntime baseAgentTree)
    
    let notifyProgress sess = writeBChan outChan (AppEvent_AgentStepProgrress convId sess)
    let notifyNeedInput = writeBChan outChan (AppEvent_AgentNeedsInput convId)
    
    let a = agent0
            { usrQuery = notifyNeedInput >> readBChan inChan
            }
    
    let tuiAgent = TuiAgent a baseAgentTree
    
    -- Get agent slug from the runtime
    let agentSlug = Runtime.agentSlug (agentRuntime baseAgentTree)
    
    -- Create the restored conversation
    let conv = Conversation
            { conversationId = convId
            , conversationAgent = tuiAgent
            , conversationThreadId = Nothing  -- Not started yet
            , conversationSession = Just session
            , conversationName = "@" <> agentSlug <> " (restored)"
            , conversationChan = inChan
            , conversationStatus = ConversationStatus_Restored
            , conversationFilePath = path
            }
    
    pure [conv]

