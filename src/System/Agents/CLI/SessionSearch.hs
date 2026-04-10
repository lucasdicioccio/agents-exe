{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'session-search' command handler.

The session-search command performs fuzzy text search across session files
using an SQLite FTS5 index with optional metadata filtering.
-}
module System.Agents.CLI.SessionSearch (
    -- * Types
    SessionSearchOptions (..),

    -- * Handler
    handleSessionSearch,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)

import System.Agents.Session.Search.Index (checkIndexStatus, createSearchIndex)
import System.Agents.Session.Search.Query (executeSearchWithOptions, formatResults)
import qualified System.Agents.Session.Search.Types as SearchTypes
import qualified System.Agents.SessionStore as SessionStore

-- | Options for the session-search command
data SessionSearchOptions = SessionSearchOptions
    { searchQueryText :: Text
    , searchDbPath :: Maybe FilePath
    , searchSessionStore :: SessionStore.SessionStore
    , searchDateFilter :: Maybe SearchTypes.DateFilter
    , searchTools :: [Text]
    , searchAgent :: Maybe Text
    , searchIncludeToolOutputs :: Bool
    , searchJsonOutput :: Bool
    , searchPreviewLines :: Int
    , searchLimit :: Maybe Int
    , searchAutoUpdate :: Bool
    }
    deriving (Show, Eq)

-- | Handle the session-search command
handleSessionSearch :: SessionSearchOptions -> IO ()
handleSessionSearch opts = do
    -- Check if index exists
    let config = buildIndexConfig opts
    status <- checkIndexStatus config

    case status of
        SearchTypes.IndexMissing -> do
            Text.putStrLn "Search index not found. Building index..."
            createSearchIndex config
        SearchTypes.IndexStale stale total -> do
            when opts.searchAutoUpdate $ do
                Text.putStrLn $ "Index is stale (" <> Text.pack (show stale) <> " of " <> Text.pack (show total) <> " sessions need update). Updating..."
        _ -> pure ()

    -- Build search options
    let searchOpts = buildSearchOptions opts

    -- Execute search
    result <- executeSearchWithOptions config searchOpts

    -- Display results
    formatResults searchOpts result

-- | Build the search index configuration from options.
buildIndexConfig :: SessionSearchOptions -> SearchTypes.SearchIndexConfig
buildIndexConfig opts =
    SearchTypes.SearchIndexConfig
        { SearchTypes.indexDbPath = fromMaybe ".agents-search.db" opts.searchDbPath
        , SearchTypes.indexSessionStore = opts.searchSessionStore
        , SearchTypes.indexIncludeToolOutputs = opts.searchIncludeToolOutputs
        }

-- | Build search options from CLI options.
buildSearchOptions :: SessionSearchOptions -> SearchTypes.SearchOptions
buildSearchOptions opts =
    SearchTypes.SearchOptions
        { SearchTypes.searchQuery = opts.searchQueryText
        , SearchTypes.searchDateFilter = opts.searchDateFilter
        , SearchTypes.searchTools = opts.searchTools
        , SearchTypes.searchAgent = opts.searchAgent
        , SearchTypes.searchIncludeToolOutputs = opts.searchIncludeToolOutputs
        , SearchTypes.searchJsonOutput = opts.searchJsonOutput
        , SearchTypes.searchPreviewLines = opts.searchPreviewLines
        , SearchTypes.searchLimit = opts.searchLimit
        , SearchTypes.searchAutoUpdate = opts.searchAutoUpdate
        }

-- | Parse a date string in various formats.
_parseDate :: String -> Maybe UTCTime
_parseDate str =
    -- Try various date formats
    parseTimeM True defaultTimeLocale "%Y-%m-%d" str
        <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" str
        <|> _parseRelativeDate str

-- | Parse relative date strings like "2 weeks ago".
_parseRelativeDate :: String -> Maybe UTCTime
_parseRelativeDate _str =
    -- TODO: Implement relative date parsing
    -- For now, return Nothing to indicate not supported
    Nothing

