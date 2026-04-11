{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Index building and maintenance for session search.

This module provides functionality to build and maintain an SQLite FTS5-based
search index for session files. The index uses trigram tokenization for
fuzzy matching capabilities.
-}
module System.Agents.Session.Search.Index (
    -- * Index Management
    createSearchIndex,
    updateSearchIndex,
    checkIndexStatus,
    removeSearchIndex,

    -- * Index Operations
    withSearchIndex,
    ensureIndexExists,

    -- * Content Extraction
    extractSearchableContent,
    extractToolNames,
) where

import Control.Exception (bracket, handle)
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.SQLite.Simple (
    Connection,
    Query (..),
    close,
    execute,
    execute_,
    open,
    query_,
 )
import Database.SQLite.Simple.QQ (sql)
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, removeFile)
import System.FilePath (takeDirectory)

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Base (LlmTurnContent (..), Session (..), Turn (..), UserTurnContent (..))
import System.Agents.Session.Search.Types
import System.Agents.Session.Types (
    LlmResponse (..),
    LlmToolCall (..),
    SystemPrompt (..),
    UserQuery (..),
    UserToolResponse (..),
 )
import qualified System.Agents.SessionStore as SessionStore

-------------------------------------------------------------------------------
-- Index Creation and Schema
-------------------------------------------------------------------------------

-- | Schema statements for the search index.
searchIndexSchema :: [Query]
searchIndexSchema =
    [ -- Session metadata cache
      [sql|
        CREATE TABLE IF NOT EXISTS session_index (
            session_id TEXT PRIMARY KEY,
            file_path TEXT NOT NULL,
            mtime INTEGER NOT NULL,
            agent_slug TEXT,
            turn_count INTEGER,
            first_turn_at TIMESTAMP,
            last_turn_at TIMESTAMP
        )
    |]
    , -- FTS5 virtual table for trigram search
      [sql|
        CREATE VIRTUAL TABLE IF NOT EXISTS search_content USING fts5(
            session_id,
            content,
            tokenize='trigram'
        )
    |]
    , -- Tool call index for filtering
      [sql|
        CREATE TABLE IF NOT EXISTS tool_index (
            session_id TEXT REFERENCES session_index(session_id),
            tool_name TEXT NOT NULL,
            call_count INTEGER DEFAULT 1,
            PRIMARY KEY (session_id, tool_name)
        )
    |]
    , -- Index metadata
      [sql|
        CREATE TABLE IF NOT EXISTS index_metadata (
            key TEXT PRIMARY KEY,
            value TEXT
        )
    |]
    , -- Indexes for faster lookups
      [sql|
        CREATE INDEX IF NOT EXISTS idx_tool_index_session ON tool_index(session_id)
    |]
    , [sql|
        CREATE INDEX IF NOT EXISTS idx_tool_index_name ON tool_index(tool_name)
    |]
    , [sql|
        CREATE INDEX IF NOT EXISTS idx_session_index_mtime ON session_index(mtime)
    |]
    ]

-- | Initialize the search index database with schema.
initializeIndex :: FilePath -> IO Connection
initializeIndex dbPath = do
    -- Ensure directory exists
    let dir = takeDirectory dbPath
    unless (null dir || dir == ".") $ do
        createDirectoryIfMissing True dir

    conn <- open dbPath
    -- Enable foreign keys and WAL mode
    execute_ conn "PRAGMA foreign_keys = ON"
    execute_ conn "PRAGMA journal_mode = WAL"

    -- Create schema
    forM_ searchIndexSchema $ \stmt -> do
        execute_ conn stmt

    -- Store creation time
    now <- getCurrentTime
    execute
        conn
        [sql|
            INSERT OR REPLACE INTO index_metadata (key, value)
            VALUES ('created_at', ?)
        |]
        [show now]

    pure conn

-------------------------------------------------------------------------------
-- Index Management
-------------------------------------------------------------------------------

-- | Create a new search index, replacing any existing index.
createSearchIndex :: SearchIndexConfig -> IO ()
createSearchIndex config = do
    -- Remove existing index if present
    removeSearchIndex config

    -- Create new index
    bracket (initializeIndex config.indexDbPath) close $ \conn -> do
        -- Get all sessions from store
        sessions <- SessionStore.listSessions config.indexSessionStore

        -- Index each session
        forM_ sessions $ \(path, mSession, convId) -> do
            case mSession of
                Nothing -> pure () -- Skip unparseable sessions
                Just session -> do
                    mtime <- getMtime path
                    indexSession conn config path mtime convId session

        -- Update metadata
        now <- getCurrentTime
        execute
            conn
            [sql|
                INSERT OR REPLACE INTO index_metadata (key, value)
                VALUES ('last_full_build', ?)
            |]
            [show now]

-- | Incrementally update the search index.
updateSearchIndex :: SearchIndexConfig -> IO ()
updateSearchIndex config = do
    exists <- doesFileExist config.indexDbPath
    if not exists
        then createSearchIndex config
        else do
            bracket (open config.indexDbPath) close $ \conn -> do
                -- Enable foreign keys and WAL mode
                execute_ conn "PRAGMA foreign_keys = ON"

                -- Get all sessions from store
                sessions <- SessionStore.listSessions config.indexSessionStore

                -- Get indexed mtimes
                indexedMtimes <- getIndexedMtimes conn

                -- Process each session
                forM_ sessions $ \(path, mSession, convId) -> do
                    mtime <- getMtime path
                    let sessionIdText = conversationIdToText convId

                    case mSession of
                        Nothing -> pure () -- Skip unparseable
                        Just session -> do
                            case lookup sessionIdText indexedMtimes of
                                Nothing ->
                                    -- New session, add to index
                                    indexSession conn config path mtime convId session
                                Just indexedMtime
                                    | mtime > indexedMtime ->
                                        -- Modified session, re-index
                                        reindexSession conn config path mtime convId session
                                    | otherwise ->
                                        -- Unchanged, skip
                                        pure ()

                -- Remove sessions that no longer exist
                let currentIds = map (\(_, _, cid) -> conversationIdToText cid) sessions
                removeStaleSessions conn currentIds

                -- Update metadata
                now <- getCurrentTime
                execute
                    conn
                    [sql|
                        INSERT OR REPLACE INTO index_metadata (key, value)
                        VALUES ('last_incremental_update', ?)
                    |]
                    [show now]

-- | Check the status of the search index.
checkIndexStatus :: SearchIndexConfig -> IO SearchIndexStatus
checkIndexStatus config = do
    exists <- doesFileExist config.indexDbPath
    if not exists
        then pure IndexMissing
        else handle (\e -> pure $ IndexError $ Text.pack $ show (e :: IOError)) $ do
            bracket (open config.indexDbPath) close $ \conn -> do
                -- Get all sessions from store
                sessions <- SessionStore.listSessions config.indexSessionStore
                let totalSessions = length sessions

                -- Get indexed mtimes
                indexedMtimes <- getIndexedMtimes conn

                -- Count stale sessions
                staleCount <- countStaleSessions sessions indexedMtimes

                if staleCount == 0
                    then pure IndexCurrent
                    else pure $ IndexStale staleCount totalSessions

-- | Remove the search index.
removeSearchIndex :: SearchIndexConfig -> IO ()
removeSearchIndex config = do
    exists <- doesFileExist config.indexDbPath
    when exists $ do
        removeFile config.indexDbPath

-------------------------------------------------------------------------------
-- Session Indexing
-------------------------------------------------------------------------------

{- | Index a single session.

Note: FTS5 virtual tables don't support ON CONFLICT DO UPDATE, so we use
INSERT OR REPLACE which deletes the old row and inserts a new one.
-}
indexSession ::
    Connection ->
    SearchIndexConfig ->
    FilePath ->
    UTCTime ->
    ConversationId ->
    Session ->
    IO ()
indexSession conn config path mtime convId session = do
    let sessionIdText = conversationIdToText convId
    let content = extractSearchableContent config.indexIncludeToolOutputs session
    let (agentSlug, turnCount, firstTurn, lastTurn) = extractSessionMetadata session

    -- Insert into session_index (regular table supports ON CONFLICT)
    execute
        conn
        [sql|
            INSERT INTO session_index 
                (session_id, file_path, mtime, agent_slug, turn_count, first_turn_at, last_turn_at)
            VALUES (?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(session_id) DO UPDATE SET
                file_path = excluded.file_path,
                mtime = excluded.mtime,
                agent_slug = excluded.agent_slug,
                turn_count = excluded.turn_count,
                first_turn_at = excluded.first_turn_at,
                last_turn_at = excluded.last_turn_at
        |]
        (sessionIdText, path, utcToEpoch mtime, agentSlug, turnCount, firstTurn, lastTurn)

    -- Insert into FTS5 table - use INSERT OR REPLACE since FTS5 doesn't support ON CONFLICT DO UPDATE
    -- INSERT OR REPLACE works because session_id is the primary key (rowid) of the FTS5 table
    execute
        conn
        [sql|
            INSERT OR REPLACE INTO search_content (session_id, content)
            VALUES (?, ?)
        |]
        (sessionIdText, content)

    -- Index tools
    indexTools conn sessionIdText session

-- | Re-index a modified session (removes old data first).
reindexSession ::
    Connection ->
    SearchIndexConfig ->
    FilePath ->
    UTCTime ->
    ConversationId ->
    Session ->
    IO ()
reindexSession conn config path mtime convId session = do
    let sessionIdText = conversationIdToText convId

    -- Remove old tool entries
    execute
        conn
        [sql|
            DELETE FROM tool_index WHERE session_id = ?
        |]
        [sessionIdText]

    -- Re-index
    indexSession conn config path mtime convId session

-- | Index tools used in a session.
indexTools :: Connection -> Text -> Session -> IO ()
indexTools conn sessionIdText session = do
    let toolCounts = countToolCalls session
    forM_ toolCounts $ \(toolName, count) -> do
        execute
            conn
            [sql|
                INSERT INTO tool_index (session_id, tool_name, call_count)
                VALUES (?, ?, ?)
                ON CONFLICT(session_id, tool_name) DO UPDATE SET
                    call_count = excluded.call_count
            |]
            (sessionIdText, toolName, count)

{- | Remove sessions that no longer exist.

This function deletes all index entries for sessions whose IDs are not
in the provided currentIds list. Since SQLite doesn't support array
parameters directly, we use a temporary table approach for efficiency.
-}
removeStaleSessions :: Connection -> [Text] -> IO ()
removeStaleSessions _conn [] = do
    -- If no current sessions, nothing to do (all sessions would be stale,
    -- but this is handled by createSearchIndex which removes the entire DB)
    pure ()
removeStaleSessions conn currentIds = do
    -- Create temporary table to hold current session IDs
    execute_ conn "CREATE TEMP TABLE IF NOT EXISTS temp_current_ids (session_id TEXT PRIMARY KEY)"
    execute_ conn "DELETE FROM temp_current_ids"

    -- Insert current IDs into temp table
    forM_ currentIds $ \sid -> do
        execute conn "INSERT OR IGNORE INTO temp_current_ids (session_id) VALUES (?)" [sid]

    -- Delete stale entries from all index tables
    execute_
        conn
        [sql|
        DELETE FROM tool_index 
        WHERE session_id NOT IN (SELECT session_id FROM temp_current_ids)
    |]

    execute_
        conn
        [sql|
        DELETE FROM search_content 
        WHERE session_id NOT IN (SELECT session_id FROM temp_current_ids)
    |]

    execute_
        conn
        [sql|
        DELETE FROM session_index 
        WHERE session_id NOT IN (SELECT session_id FROM temp_current_ids)
    |]

    -- Clean up temp table
    execute_ conn "DROP TABLE IF EXISTS temp_current_ids"

-------------------------------------------------------------------------------
-- Content Extraction
-------------------------------------------------------------------------------

-- | Extract searchable text content from a session.
extractSearchableContent :: Bool -> Session -> Text
extractSearchableContent includeToolOutputs session =
    Text.unlines $ concatMap (extractTurnContent includeToolOutputs) session.turns

-- | Extract content from a single turn.
extractTurnContent :: Bool -> Turn -> [Text]
extractTurnContent includeToolOutputs turn = case turn of
    UserTurn content _ ->
        catMaybes
            [ Just $ extractSystemPrompt content.userPrompt
            , extractUserQuery <$> content.userQuery
            , if includeToolOutputs
                then Just $ extractToolResponses content.userToolResponses
                else Nothing
            ]
    LlmTurn content _ ->
        catMaybes
            [ responseText content.llmResponse
            , if includeToolOutputs
                then Just $ extractLlmToolCalls content.llmToolCalls
                else Nothing
            ]

-- | Extract system prompt text.
extractSystemPrompt :: SystemPrompt -> Text
extractSystemPrompt (SystemPrompt txt) = txt

-- | Extract user query text.
extractUserQuery :: UserQuery -> Text
extractUserQuery (UserQuery txt) = txt

-- | Extract tool responses.
extractToolResponses :: [(LlmToolCall, UserToolResponse)] -> Text
extractToolResponses responses =
    Text.unlines $ map extractResponse responses
  where
    extractResponse (_, UserToolResponse val) =
        jsonToText val

-- | Extract LLM tool calls.
extractLlmToolCalls :: [LlmToolCall] -> Text
extractLlmToolCalls calls =
    Text.unlines $ map extractCall calls
  where
    extractCall (LlmToolCall val) = jsonToText val

-- | Convert JSON value to searchable text.
jsonToText :: Value -> Text
jsonToText =
    Text.replace "\\n" " "
        . Text.replace "\\t" " "
        . Text.decodeUtf8
        . LByteString.toStrict
        . Aeson.encode

-------------------------------------------------------------------------------
-- Metadata Extraction
-------------------------------------------------------------------------------

-- | Extract metadata from a session.
extractSessionMetadata :: Session -> (Maybe Text, Int, Maybe UTCTime, Maybe UTCTime)
extractSessionMetadata session =
    let turnCount = length session.turns
        timestamps = extractTimestamps session
        firstTurn = safeHead timestamps
        lastTurn = safeLast timestamps
     in (Nothing, turnCount, firstTurn, lastTurn)
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just x
    safeLast [] = Nothing
    safeLast xs = Just $ last xs

-- | Extract timestamps from session turns (if available).
extractTimestamps :: Session -> [UTCTime]
extractTimestamps _session =
    -- Note: Session doesn't currently have timestamps per turn
    -- This would need to be added to Session types for full support
    []

-- | Count tool calls in a session.
countToolCalls :: Session -> [(Text, Int)]
countToolCalls session =
    let allTools = concatMap extractToolsFromTurn session.turns
     in foldr insertTool ([] :: [(Text, Int)]) allTools
  where
    extractToolsFromTurn turn = case turn of
        UserTurn content _ -> map (\(LlmToolCall val, _) -> extractToolName val) content.userToolResponses
        LlmTurn content _ -> map (\(LlmToolCall val) -> extractToolName val) content.llmToolCalls

    extractToolName :: Value -> Text
    extractToolName val = case val of
        Object obj -> fromMaybe "unknown" $ do
            nameVal <- KeyMap.lookup "name" obj
            case nameVal of
                String n -> Just n
                _ -> Nothing
        _ -> "unknown"

    insertTool :: Text -> [(Text, Int)] -> [(Text, Int)]
    insertTool toolName acc = case lookup toolName acc of
        Nothing -> (toolName, 1) : acc
        Just count -> (toolName, count + 1) : filter (\(n, _) -> n /= toolName) acc

-- | Extract tool names from a session.
extractToolNames :: Session -> [Text]
extractToolNames session =
    let allTools = concatMap extractToolsFromTurn session.turns
     in map fst $ foldr insertTool ([] :: [(Text, Int)]) allTools
  where
    extractToolsFromTurn turn = case turn of
        UserTurn content _ -> map (\(LlmToolCall val, _) -> extractToolName val) content.userToolResponses
        LlmTurn content _ -> map (\(LlmToolCall val) -> extractToolName val) content.llmToolCalls

    extractToolName :: Value -> Text
    extractToolName val = case val of
        Object obj -> fromMaybe "unknown" $ do
            nameVal <- KeyMap.lookup "name" obj
            case nameVal of
                String n -> Just n
                _ -> Nothing
        _ -> "unknown"

    insertTool :: Text -> [(Text, Int)] -> [(Text, Int)]
    insertTool toolName acc = case lookup toolName acc of
        Nothing -> (toolName, 1) : acc
        Just count -> (toolName, count + 1) : filter (\(n, _) -> n /= toolName) acc

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Run an action with a database connection.
withSearchIndex :: SearchIndexConfig -> (Connection -> IO a) -> IO a
withSearchIndex config action = do
    bracket (open config.indexDbPath) close action

-- | Ensure the index exists, creating it if necessary.
ensureIndexExists :: SearchIndexConfig -> IO ()
ensureIndexExists config = do
    exists <- doesFileExist config.indexDbPath
    unless exists $ do
        createSearchIndex config

-- | Get file modification time.
getMtime :: FilePath -> IO UTCTime
getMtime path = do
    getModificationTime path

-- | Convert UTC time to epoch seconds.
utcToEpoch :: UTCTime -> Int
utcToEpoch = round . utcTimeToPOSIXSeconds

-- | Convert ConversationId to Text.
conversationIdToText :: ConversationId -> Text
conversationIdToText (ConversationId uuid) = Text.pack $ show uuid

-- | Get all indexed sessions with their mtimes.
getIndexedMtimes :: Connection -> IO [(Text, UTCTime)]
getIndexedMtimes conn = do
    results <- query_ conn [sql| SELECT session_id, mtime FROM session_index |]
    pure $ map (\(sid, mtime :: Int) -> (sid, epochToUtc mtime)) results
  where
    epochToUtc :: Int -> UTCTime
    epochToUtc = posixSecondsToUTCTime . fromIntegral

-- | Count stale sessions.
countStaleSessions :: [(FilePath, Maybe Session, ConversationId)] -> [(Text, UTCTime)] -> IO Int
countStaleSessions sessions indexedMtimes = do
    let indexedMap = indexedMtimes
    countStale 0 sessions indexedMap
  where
    countStale acc [] _ = pure acc
    countStale acc ((path, _, convId) : rest) indexedMap = do
        mtime <- getMtime path
        let sessionIdText = conversationIdToText convId
        case lookup sessionIdText indexedMap of
            Nothing -> countStale (acc + 1) rest indexedMap
            Just indexedMtime
                | mtime > indexedMtime -> countStale (acc + 1) rest indexedMap
                | otherwise -> countStale acc rest indexedMap
