{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'session-index' command handler.

The session-index command manages the search index for session files.
It supports building, updating, and checking the status of the index.
-}
module System.Agents.CLI.SessionIndex (
    -- * Types
    SessionIndexOptions (..),

    -- * Handler
    handleSessionIndex,
) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)

import System.Agents.Session.Search.Index
import System.Agents.Session.Search.Types
import qualified System.Agents.SessionStore as SessionStore

-- | Options for the session-index command
data SessionIndexOptions = SessionIndexOptions
    { optIndexOperation :: IndexOperation
    , optIndexDbPath :: Maybe FilePath
    , optIndexSessionStore :: SessionStore.SessionStore
    , optIndexIncludeToolOutputs :: Bool
    }
    deriving (Show, Eq)

-- | Handle the session-index command
handleSessionIndex :: SessionIndexOptions -> IO ()
handleSessionIndex opts = do
    let config = buildIndexConfig opts

    case opts.optIndexOperation of
        IndexBuild -> do
            Text.putStrLn "Building search index..."
            createSearchIndex config
            Text.putStrLn "Index built successfully."
        IndexUpdate -> do
            Text.putStrLn "Updating search index..."
            updateSearchIndex config
            Text.putStrLn "Index updated successfully."
        IndexStatus -> do
            status <- checkIndexStatus config
            displayStatus status
        IndexClean -> do
            Text.putStrLn "Removing search index..."
            removeSearchIndex config
            Text.putStrLn "Index removed."

-- | Build the search index configuration from options.
buildIndexConfig :: SessionIndexOptions -> SearchIndexConfig
buildIndexConfig opts =
    SearchIndexConfig
        { indexDbPath = fromMaybe ".agents-search.db" opts.optIndexDbPath
        , indexSessionStore = opts.optIndexSessionStore
        , indexIncludeToolOutputs = opts.optIndexIncludeToolOutputs
        }

-- | Display index status to the user.
displayStatus :: SearchIndexStatus -> IO ()
displayStatus status = case status of
    IndexCurrent ->
        Text.putStrLn "Index status: Current (up to date)"
    IndexStale stale total -> do
        Text.putStrLn $ "Index status: Stale (" <> Text.pack (show stale) <> " of " <> Text.pack (show total) <> " sessions need update)"
        Text.putStrLn "Run 'session-index --update' to refresh the index."
    IndexMissing -> do
        Text.putStrLn "Index status: Missing"
        Text.putStrLn "Run 'session-index --build' to create the index."
    IndexError err -> do
        Text.hPutStrLn stderr $ "Error checking index status: " <> err
        exitFailure
