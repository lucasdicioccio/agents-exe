{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Search query execution for session search.

This module provides functionality to execute fuzzy text search queries
against the SQLite FTS5 index with optional metadata filtering.
-}
module System.Agents.Session.Search.Query (
    -- * Search Execution
    executeSearch,
    executeSearchWithOptions,

    -- * Result Formatting
    formatResults,
    formatResultAsText,
) where

import Control.Exception (handle)
import Control.Monad (when)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (diffUTCTime, getCurrentTime)
import Database.SQLite.Simple (
    Connection,
    Only (..),
    query,
 )
import Database.SQLite.Simple.QQ (sql)
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Exit (exitFailure)
import System.IO (stderr)

import System.Agents.Session.Search.Types
import System.Agents.Session.Search.Index

-------------------------------------------------------------------------------
-- Search Execution
-------------------------------------------------------------------------------

-- | Execute a search query with the given options.
executeSearchWithOptions :: SearchIndexConfig -> SearchOptions -> IO SearchResult
executeSearchWithOptions config opts = do
    startTime <- getCurrentTime

    -- Auto-update index if requested and stale
    wasUpdated <-
        if opts.searchAutoUpdate
            then do
                status <- checkIndexStatus config
                case status of
                    IndexStale _ _ -> do
                        updateSearchIndex config
                        pure True
                    IndexMissing -> do
                        createSearchIndex config
                        pure True
                    _ -> pure False
            else pure False

    -- Execute search
    result <- executeSearch config opts

    endTime <- getCurrentTime
    let queryTimeMs = realToFrac (diffUTCTime endTime startTime) * 1000

    pure result{resultQueryTimeMs = queryTimeMs, resultIndexWasUpdated = wasUpdated}

-- | Execute a search query against the index.
executeSearch :: SearchIndexConfig -> SearchOptions -> IO SearchResult
executeSearch config opts = do
    handle (\e -> do
        let err = Text.pack $ "Search error: " ++ show (e :: IOError)
        Text.hPutStrLn stderr err
        exitFailure) $ do

        withSearchIndex config $ \conn -> do
            -- Build and execute FTS5 query
            rawResults <- querySearchContent conn opts

            -- Apply metadata filters
            filteredResults <- filterResults conn opts rawResults

            -- Build result items with previews
            items <- mapM (buildResultItem conn opts) filteredResults

            -- Apply limit
            let limitedItems = case opts.searchLimit of
                    Nothing -> items
                    Just n -> take n items

            pure $ SearchResult
                { resultItems = limitedItems
                , resultTotalMatches = length filteredResults
                , resultQueryTimeMs = 0 -- Will be set by executeSearchWithOptions
                , resultIndexWasUpdated = False
                }

-------------------------------------------------------------------------------
-- Query Building
-------------------------------------------------------------------------------

-- | Query the FTS5 search_content table.
querySearchContent ::
    Connection ->
    SearchOptions ->
    IO [(Text, Text, Double)] -- ^ (session_id, content, rank)
querySearchContent conn opts = do
    let searchTerm = sanitizeSearchTerm opts.searchQuery

    if Text.null searchTerm
        then pure [] -- Empty query returns no results
        else do
            results <-
                query
                    conn
                    [sql|
                        SELECT session_id, content, rank
                        FROM search_content
                        WHERE search_content MATCH ?
                        ORDER BY rank
                    |]
                    [searchTerm]
            pure $ map (\(sid, content, rank) -> (sid, content, rank :: Double)) results

-- | Sanitize a search term for FTS5.
sanitizeSearchTerm :: Text -> Text
sanitizeSearchTerm =
    Text.unwords
        . map escapeSpecialChars
        . Text.words
        . Text.toLower
        . Text.strip
  where
    escapeSpecialChars word =
        -- Escape FTS5 special characters by wrapping in quotes if needed
        if Text.any isSpecialChar word
            then quote <> Text.replace quote (quote <> quote) word <> quote
            else word

    quote = Text.pack "\""
    
    isSpecialChar c = c `elem` ['"', '*', '(', ')', '-', '~', '^']

-- | Filter results by tool usage.
filterByTools :: Connection -> [Text] -> [(Text, Text, Double)] -> IO [(Text, Text, Double)]
filterByTools conn tools results = do
    let sessionIds = map (\(sid, _, _) -> sid) results

    -- Query tool_index for each tool and collect matching sessions
    matchingPerTool <- mapM (queryToolSessions conn sessionIds) tools
    
    -- A session matches if it has ANY of the requested tools (OR logic within tools)
    let allMatching = concat matchingPerTool
    let matchingSet = map (\(Only sid) -> sid :: Text) allMatching
    
    pure $ filter (\(sid, _, _) -> sid `elem` matchingSet) results

-- | Query sessions that have a specific tool.
queryToolSessions :: Connection -> [Text] -> Text -> IO [Only Text]
queryToolSessions conn sessionIds tool = do
    -- Query for this tool across all candidate sessions
    -- We use individual queries to avoid IN clause complexity with lists
    concat <$> mapM (queryOneSession tool) sessionIds
  where
    queryOneSession t sid = 
        query conn [sql| SELECT session_id FROM tool_index WHERE tool_name = ? AND session_id = ? |] (t, sid)

-- | Filter results by date.
filterByDate :: DateFilter -> [(Text, Text, Double)] -> [(Text, Text, Double)]
filterByDate _dateFilter results =
    -- Note: Date filtering would require timestamp data in session_index
    -- This is a placeholder for when timestamp data is available
    results

-------------------------------------------------------------------------------
-- Result Processing
-------------------------------------------------------------------------------

-- | Build a SearchResultItem from raw query result.
buildResultItem :: Connection -> SearchOptions -> (Text, Text, Double) -> IO SearchResultItem
buildResultItem conn opts (sessionId, content, rank) = do
    -- Get session metadata
    metadata <- getSessionMetadata conn sessionId content rank

    -- Generate preview if requested
    preview <-
        if opts.searchPreviewLines > 0
            then generatePreview opts content opts.searchQuery
            else pure Nothing

    -- Extract matched terms
    let matchedTerms = extractMatchedTerms opts.searchQuery content

    pure $ SearchResultItem
        { resultMetadata = metadata
        , resultPreview = preview
        , resultMatchedTerms = matchedTerms
        }

-- | Get metadata for a session.
getSessionMetadata :: Connection -> Text -> Text -> Double -> IO SearchResultMetadata
getSessionMetadata conn sessionId _content rank = do
    results <-
        query
            conn
            [sql|
                SELECT file_path, agent_slug, turn_count, first_turn_at, last_turn_at
                FROM session_index
                WHERE session_id = ?
            |]
            [sessionId]

    case results of
        [(path, agent, turns, first, lastTurn)] ->
            pure $ SearchResultMetadata
                { resultSessionId = sessionId
                , resultFilePath = path
                , resultAgentSlug = agent
                , resultTurnCount = turns
                , resultFirstTurnAt = first
                , resultLastTurnAt = lastTurn
                , resultRank = rank
                }
        _ ->
            pure $ SearchResultMetadata
                { resultSessionId = sessionId
                , resultFilePath = ""
                , resultAgentSlug = Nothing
                , resultTurnCount = 0
                , resultFirstTurnAt = Nothing
                , resultLastTurnAt = Nothing
                , resultRank = rank
                }

-- | Generate a preview showing context around matches.
generatePreview :: SearchOptions -> Text -> Text -> IO (Maybe Text)
generatePreview opts content queryText = do
    let queryTerms = Text.words $ Text.toLower queryText
    let contentLower = Text.toLower content

    -- Find positions of matches
    let matchPositions = findMatches queryTerms contentLower

    case matchPositions of
        [] -> pure Nothing
        positions -> do
            let previewLines = opts.searchPreviewLines
            let contextSize = previewLines * 40 -- Approximate characters per line

            -- Build preview from match contexts
            let preview = buildPreview content positions contextSize
            pure $ Just $ Text.take 1000 preview -- Limit preview length

-- | Find match positions in content.
findMatches :: [Text] -> Text -> [Int]
findMatches terms content =
    concatMap (\term -> findTermMatches term content 0) terms
  where
    findTermMatches :: Text -> Text -> Int -> [Int]
    findTermMatches term txt offset =
        case Text.breakOn term txt of
            (_, "") -> [] -- Not found
            (prefix, rest) ->
                let pos = offset + Text.length prefix
                 in pos : findTermMatches term (Text.drop (Text.length term) rest) (pos + Text.length term)

-- | Build preview text from match positions.
buildPreview :: Text -> [Int] -> Int -> Text
buildPreview content positions contextSize =
    Text.intercalate " ... " $ map (extractContext content contextSize) positions

-- | Extract context around a position.
extractContext :: Text -> Int -> Int -> Text
extractContext content contextSize pos =
    let start = max 0 (pos - contextSize `div` 2)
        end = min (Text.length content) (pos + contextSize `div` 2)
     in Text.strip $ Text.take (end - start) $ Text.drop start content

-- | Extract terms that matched in the content.
extractMatchedTerms :: Text -> Text -> [Text]
extractMatchedTerms queryText content =
    let queryTerms = Text.words $ Text.toLower queryText
        contentLower = Text.toLower content
     in filter (\term -> term `Text.isInfixOf` contentLower) queryTerms

-------------------------------------------------------------------------------
-- Result Formatting
-------------------------------------------------------------------------------

-- | Format search results for display.
formatResults :: SearchOptions -> SearchResult -> IO ()
formatResults opts result
    | opts.searchJsonOutput = do
        BSL.putStrLn $ encodePretty result
    | otherwise = do
        -- Human-readable output
        Text.putStrLn $ "Found " <> Text.pack (show $ resultTotalMatches result) <> " matches"
        Text.putStrLn $ "Query time: " <> Text.pack (show (round (resultQueryTimeMs result) :: Integer)) <> "ms"
        when (resultIndexWasUpdated result) $
            Text.putStrLn "(Index was auto-updated)"
        Text.putStrLn ""

        mapM_ (Text.putStrLn . formatResultAsText opts) $ resultItems result

-- | Format a single result as human-readable text.
formatResultAsText :: SearchOptions -> SearchResultItem -> Text
formatResultAsText _opts item =
    let meta = resultMetadata item
     in Text.unlines $
            filter (not . Text.null)
                [ "─────────────────────────────────────────"
                , "Session: " <> resultSessionId meta
                , "File: " <> Text.pack (resultFilePath meta)
                , maybe "" ("Agent: " <>) (resultAgentSlug meta)
                , "Turns: " <> Text.pack (show $ resultTurnCount meta)
                , "Rank: " <> Text.pack (show $ resultRank meta)
                , case resultPreview item of
                    Nothing -> ""
                    Just preview -> "Preview:\n" <> preview
                , ""
                ]

-- | Apply all metadata filters to search results.
filterResults :: Connection -> SearchOptions -> [(Text, Text, Double)] -> IO [(Text, Text, Double)]
filterResults conn opts results = do
    -- Get metadata for all sessions first
    allMetadata <- queryAllSessionMetadata conn (map (\(sid, _, _) -> sid) results)
    
    -- Apply date filters
    let dateFiltered = case opts.searchDateFilter of
            Nothing -> results
            Just df -> filterByDate df results

    -- Apply tool filters
    toolFiltered <- case opts.searchTools of
        [] -> pure dateFiltered
        tools -> filterByTools conn tools dateFiltered

    -- Apply agent filter (in-memory)
    let agentFiltered = case opts.searchAgent of
            Nothing -> toolFiltered
            Just agent -> filter (\(sid, _, _) -> hasAgent sid agent allMetadata) toolFiltered

    pure agentFiltered

-- | Check if a session has a specific agent.
hasAgent :: Text -> Text -> [(Text, Maybe Text, Int)] -> Bool
hasAgent sessionId agent metadata =
    case lookup sessionId (map (\(s, a, _t) -> (s, a)) metadata) of
        Nothing -> False
        Just mAgent -> mAgent == Just agent

-- | Query metadata for all sessions.
queryAllSessionMetadata :: Connection -> [Text] -> IO [(Text, Maybe Text, Int)]
queryAllSessionMetadata conn sessionIds = do
    -- For simplicity, query each session individually
    concat <$> mapM queryOne sessionIds
  where
    queryOne sid = do
        results <- query conn [sql| SELECT session_id, agent_slug, turn_count FROM session_index WHERE session_id = ? |] [sid]
        pure $ map (\(s, a, t) -> (s :: Text, a :: Maybe Text, t :: Int)) results

-- | Build SQL WHERE clause from search options.
_buildWhereClause :: SearchOptions -> Text
_buildWhereClause opts =
    Text.intercalate " AND " $ catMaybes
        [ dateFilterClause opts.searchDateFilter
        , agentFilterClause opts.searchAgent
        ]
  where
    dateFilterClause :: Maybe DateFilter -> Maybe Text
    dateFilterClause Nothing = Nothing
    dateFilterClause (Just (AfterDate _dt)) =
        -- Note: Would need proper timestamp handling
        Nothing
    dateFilterClause (Just (BeforeDate _dt)) =
        -- Note: Would need proper timestamp handling
        Nothing
    dateFilterClause (Just (BetweenDates _start _end)) =
        -- Note: Would need proper timestamp handling
        Nothing

    agentFilterClause :: Maybe Text -> Maybe Text
    agentFilterClause Nothing = Nothing
    agentFilterClause (Just agent) = Just $ "agent_slug = '" <> agent <> "'"

