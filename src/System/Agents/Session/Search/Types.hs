{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Core types for session search functionality.

This module defines the types used for fuzzy text search across session files,
including index configuration, search options, and result types.
-}
module System.Agents.Session.Search.Types (
    -- * Index Configuration
    SearchIndexConfig (..),
    defaultSearchIndexConfig,
    SearchIndexStatus (..),

    -- * Search Options
    SearchOptions (..),
    defaultSearchOptions,
    DateFilter (..),

    -- * Search Results
    SearchResult (..),
    SearchResultItem (..),
    SearchResultMetadata (..),

    -- * Index Operations
    IndexOperation (..),
    IndexBuildMode (..),
) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Agents.SessionStore (SessionStore)

-------------------------------------------------------------------------------
-- Index Configuration
-------------------------------------------------------------------------------

{- | Configuration for the search index.

The index is stored as a SQLite database alongside session files.
-}
data SearchIndexConfig = SearchIndexConfig
    { indexDbPath :: FilePath
    -- ^ Path to the SQLite index database (default: .agents-search.db)
    , indexSessionStore :: SessionStore
    -- ^ Session store to index
    , indexIncludeToolOutputs :: Bool
    -- ^ Whether to include tool outputs in the index
    }
    deriving (Show, Eq)

-- | Default search index configuration.
defaultSearchIndexConfig :: SessionStore -> SearchIndexConfig
defaultSearchIndexConfig store =
    SearchIndexConfig
        { indexDbPath = ".agents-search.db"
        , indexSessionStore = store
        , indexIncludeToolOutputs = False
        }

-- | Status of the search index.
data SearchIndexStatus
    = IndexCurrent
    -- ^ Index is up to date
    | IndexStale Int Int
    -- ^ Index is stale: (sessions needing update, total sessions)
    | IndexMissing
    -- ^ Index does not exist
    | IndexError Text
    -- ^ Error checking index status
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Search Options
-------------------------------------------------------------------------------

-- | Date filter for search queries.
data DateFilter
    = AfterDate UTCTime
    -- ^ Sessions after a specific date
    | BeforeDate UTCTime
    -- ^ Sessions before a specific date
    | BetweenDates UTCTime UTCTime
    -- ^ Sessions between two dates
    deriving (Show, Eq)

{- | Options for session search queries.

All filters are combined with AND logic (intersection).
-}
data SearchOptions = SearchOptions
    { searchQuery :: Text
    -- ^ Fuzzy search query text
    , searchDateFilter :: Maybe DateFilter
    -- ^ Optional date filter
    , searchTools :: [Text]
    -- ^ Filter by tools used (any of these)
    , searchAgent :: Maybe Text
    -- ^ Filter by agent slug
    , searchIncludeToolOutputs :: Bool
    -- ^ Include tool outputs in search
    , searchJsonOutput :: Bool
    -- ^ Output results as JSON
    , searchPreviewLines :: Int
    -- ^ Number of context lines to show (0 for none)
    , searchLimit :: Maybe Int
    -- ^ Maximum number of results
    , searchAutoUpdate :: Bool
    -- ^ Auto-update index if stale before searching
    }
    deriving (Show, Eq)

-- | Default search options.
defaultSearchOptions :: Text -> SearchOptions
defaultSearchOptions query =
    SearchOptions
        { searchQuery = query
        , searchDateFilter = Nothing
        , searchTools = []
        , searchAgent = Nothing
        , searchIncludeToolOutputs = False
        , searchJsonOutput = False
        , searchPreviewLines = 0
        , searchLimit = Nothing
        , searchAutoUpdate = False
        }

-------------------------------------------------------------------------------
-- Search Results
-------------------------------------------------------------------------------

-- | Metadata about a search result.
data SearchResultMetadata = SearchResultMetadata
    { resultSessionId :: Text
    -- ^ Session ID (conversation ID)
    , resultFilePath :: FilePath
    -- ^ Path to the session file
    , resultAgentSlug :: Maybe Text
    -- ^ Agent slug if available
    , resultTurnCount :: Int
    -- ^ Number of turns in the session
    , resultFirstTurnAt :: Maybe UTCTime
    -- ^ Timestamp of first turn
    , resultLastTurnAt :: Maybe UTCTime
    -- ^ Timestamp of last turn
    , resultRank :: Double
    -- ^ Search relevance score (lower is better)
    }
    deriving (Show, Eq, Generic)

instance ToJSON SearchResultMetadata

-- | A single search result item.
data SearchResultItem = SearchResultItem
    { resultMetadata :: SearchResultMetadata
    -- ^ Metadata about the matching session
    , resultPreview :: Maybe Text
    -- ^ Preview text showing the match context
    , resultMatchedTerms :: [Text]
    -- ^ Terms that matched in this result
    }
    deriving (Show, Eq, Generic)

instance ToJSON SearchResultItem

-- | Complete search results.
data SearchResult = SearchResult
    { resultItems :: [SearchResultItem]
    -- ^ List of matching sessions
    , resultTotalMatches :: Int
    -- ^ Total number of matches
    , resultQueryTimeMs :: Double
    -- ^ Query execution time in milliseconds
    , resultIndexWasUpdated :: Bool
    -- ^ Whether the index was auto-updated before search
    }
    deriving (Show, Eq, Generic)

instance ToJSON SearchResult

-------------------------------------------------------------------------------
-- Index Operations
-------------------------------------------------------------------------------

-- | Mode for building/updating the index.
data IndexBuildMode
    = BuildFull
    -- ^ Full rebuild of the index
    | BuildIncremental
    -- ^ Incremental update of changed sessions only
    deriving (Show, Eq)

-- | Operations for the session-index command.
data IndexOperation
    = IndexBuild
    -- ^ Build the search index from scratch
    | IndexUpdate
    -- ^ Incrementally update the search index
    | IndexStatus
    -- ^ Show index status
    | IndexClean
    -- ^ Remove the search index
    deriving (Show, Eq)

