{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provides a runtime for executing SQL queries against SQLite databases.

This module implements the SQLite toolbox functionality, including:

* Database connection management
* Query execution with access control (read-only vs read-write vs snapshot)
* Concurrent access protection (within-agent serialization, cross-agent busy handling)
* Snapshot mode for isolated conversation-scoped changes
* Result formatting for LLM consumption
* Tracing for debugging and monitoring

The toolbox can be configured to run in either read-only, read-write, or snapshot mode:
* 'ReadOnly': Only SELECT queries are allowed
* 'ReadWrite': All SQL operations are allowed on the original database
* 'Snapshot': Creates a copy of the database for each conversation, allowing
  isolated read-write access that doesn't affect the original database

Concurrent Access:

* Within a single agent: Tool calls are serialized via an MVar lock, ensuring
  only one query executes at a time per toolbox.
* Across multiple agents: Each agent has its own connection. SQLite's WAL mode
  allows concurrent readers, but writers are serialized. A busy timeout of
  5 seconds prevents immediate failures when another agent is writing.

Snapshot Mode:

* When in snapshot mode, the original database is copied on first use
* The copy is named with the conversation ID as suffix: @original.{uuid}.snapshot.sqlite@
* Each conversation gets its own isolated copy
* Changes are persisted to the snapshot file but don't affect the original

Example usage:

@
import System.Agents.Tools.SqliteToolbox as SQLite
import System.Agents.Base (SqliteAccessMode(..))

main :: IO ()
main = do
    let desc = SqliteToolboxDescription
            { sqliteToolboxName = "analytics"
            , sqliteToolboxDescription = "Analytics database"
            , sqliteToolboxPath = "/path/to/analytics.db"
            , sqliteToolboxAccess = SqliteReadWrite
            }
    result <- SQLite.initializeToolbox tracer desc
    case result of
        Right toolbox -> do
            -- Execute a read-only query
            result <- SQLite.executeQuery toolbox "SELECT * FROM users LIMIT 10"
            case result of
                Right queryResult -> print queryResult
                Left err -> print err
        Left err -> putStrLn $ "Failed to initialize: " ++ err
@
-}
module System.Agents.Tools.SqliteToolbox (
    -- * Core types
    Trace (..),
    Toolbox (..),
    ToolDescription (..),
    QueryError (..),
    QueryResult (..),
    SnapshotState (..),

    -- * Access control
    AccessMode (..),
    SqlOperation (..),
    classifyQuery,
    allowsOperation,
    validateAccess,

    -- * Initialization
    initializeToolbox,

    -- * Query execution
    executeQuery,
    executeReadOnlyQuery,
    executeWriteQuery,
    executeQueryWithContext,

    -- * Result formatting
    formatResults,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite3 as Direct
import Prod.Tracer (Tracer (..), runTracer)
import System.Directory (copyFile, doesFileExist)
import System.FilePath ((-<.>))

import System.Agents.Base (
    ConversationId (..),
    SqliteAccessMode (..),
    SqliteToolboxDescription (..),
 )

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

{- | Trace events for monitoring SQLite toolbox operations.

These events allow tracking of:
* Query execution progress
* Access violations
* Database errors
* Connection lifecycle
* Concurrent access contention
* Snapshot creation
-}
data Trace
    = -- | Database connection opened
      ConnectionOpenedTrace !FilePath
    | -- | Database connection closed
      ConnectionClosedTrace
    | -- | Query execution started
      QueryStartedTrace !Text
    | -- | Query execution completed successfully
      QueryCompletedTrace !Text !NominalDiffTime !Int
    | -- | Access violation detected
      AccessViolationTrace !Text !SqlOperation !AccessMode
    | -- | Query execution error
      QueryErrorTrace !Text !Text
    | -- | Database busy, waiting for lock
      WaitingForLockTrace !Text
    | -- | Snapshot created from original database
      SnapshotCreatedTrace !FilePath !FilePath
    | -- | Snapshot creation failed
      SnapshotErrorTrace !FilePath !Text
    deriving (Show)

{- | State for snapshot mode connections.

In snapshot mode, the connection is created lazily on first use.
This type tracks whether the snapshot has been initialized.
-}
data SnapshotState
    = -- | Snapshot not yet created, stores the original database path
      SnapshotUninitialized !FilePath
    | -- | Snapshot created and connection established
      SnapshotInitialized
        { snapshotConnection :: Connection
        , snapshotDirectDb :: Direct.Database
        , snapshotPath :: FilePath
        }

-- | Custom Show instance since Connection doesn't have one
instance Show SnapshotState where
    show (SnapshotUninitialized path) =
        "SnapshotUninitialized {originalPath = " ++ show path ++ "}"
    show (SnapshotInitialized _conn _directDb path) =
        "SnapshotInitialized {snapshotPath = " ++ show path ++ ", connection = <connection>}"

{- | Runtime state for a SQLite toolbox.

The toolbox maintains:
* A database connection (from sqlite-simple) - for read-only and read-write modes
* A direct database handle for metadata access - for read-only and read-write modes
* An MVar lock for serializing access within this toolbox instance
* The configured access mode (read-only, read-write, or snapshot)
* Toolbox metadata (name, description)
* For snapshot mode: an MVar tracking the lazy connection state

The MVar ensures that even if multiple tool calls are made concurrently
from the same agent, they execute sequentially. Combined with SQLite's
built-in locking and busy timeout, this provides robust concurrent access
control.
-}
data Toolbox = Toolbox
    { toolboxConnection :: Maybe Connection
    -- ^ Database connection (Nothing in snapshot mode until first use)
    , toolboxDirectDb :: Maybe Direct.Database
    -- ^ Direct database handle (Nothing in snapshot mode until first use)
    , toolboxLock :: MVar ()
    -- ^ Lock for serializing access within this toolbox instance
    , toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxPath :: FilePath
    -- ^ Original database path (for read-only/read-write) or snapshot template (for snapshot mode)
    , toolboxAccessMode :: AccessMode
    , toolboxSnapshotState :: Maybe (MVar SnapshotState)
    -- ^ Just for snapshot mode, Nothing for other modes
    }

{- | Description of a SQLite tool.

Contains metadata about a specific SQLite query tool, including
its name, description, and associated database information.
-}
data ToolDescription = ToolDescription
    { toolDescriptionName :: Text
    -- ^ Name of the tool (e.g., "query_users")
    , toolDescriptionDescription :: Text
    -- ^ Human-readable description of what the tool does
    , toolDescriptionToolboxName :: Text
    -- ^ Name of the toolbox this tool belongs to
    , toolDescriptionDatabasePath :: FilePath
    -- ^ Path to the SQLite database file
    }
    deriving (Show)

{- | Result of a SQL query execution.

Contains:
* Column names
* Row data (as JSON values)
* Row count
* Execution time
-}
data QueryResult = QueryResult
    { resultColumns :: [Text]
    , resultRows :: [[Value]]
    , resultRowCount :: Int
    , resultExecutionTime :: NominalDiffTime
    }
    deriving (Show)

-- | JSON serialization for QueryResult.
instance ToJSON QueryResult where
    toJSON result =
        Aeson.object
            [ "columns" .= resultColumns result
            , "rows" .= resultRows result
            , "rowCount" .= resultRowCount result
            , "executionTime" .= formatExecutionTime (resultExecutionTime result)
            ]

-- | Errors that can occur during query execution.
data QueryError
    = -- | SQL syntax error or database error
      SqlError !Text
    | -- | Access denied due to mode restrictions
      AccessDeniedError !Text
    | -- | Connection error
      ConnectionError !Text
    | -- | Database is locked/busy (timeout exceeded waiting for lock)
      DatabaseLockedError !Text
    | -- | Snapshot creation error
      SnapshotError !Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Access Control
-------------------------------------------------------------------------------

{- | Access mode for the toolbox.

Controls what operations are allowed:
* 'ReadOnly': Only SELECT queries are allowed
* 'ReadWrite': All SQL operations are allowed
* 'Snapshot': Creates a per-conversation copy, all operations allowed on the copy
-}
data AccessMode = ReadOnly | ReadWrite | Snapshot
    deriving (Show, Eq)

{- | SQL operation types for classification.

These are the common SQL statement types that can be identified
from the first keyword of a query.
-}
data SqlOperation
    = Select
    | Insert
    | Update
    | Delete
    | Create
    | Drop
    | Alter
    | Other
    deriving (Show, Eq)

-- | Convert the base 'SqliteAccessMode' to our internal 'AccessMode'.
fromBaseAccessMode :: SqliteAccessMode -> AccessMode
fromBaseAccessMode SqliteReadOnly = ReadOnly
fromBaseAccessMode SqliteReadWrite = ReadWrite
fromBaseAccessMode SqliteSnapshot = Snapshot

{- | Classify a SQL query by examining its first keyword.

This function performs a simple classification by looking at the first
non-whitespace, non-comment token in the query. It normalizes the query by:
1. Removing leading whitespace
2. Removing single-line comments (-- ...)
3. Removing multi-line comments (/* ... */)
4. Extracting the first alphanumeric word

Examples:

>>> classifyQuery "SELECT * FROM users"
Select

>>> classifyQuery "  -- comment\\nINSERT INTO logs VALUES (1)"
Insert

>>> classifyQuery "/* multi-line\\ncomment */ UPDATE users SET x=1"
Update
-}
classifyQuery :: Text -> SqlOperation
classifyQuery query =
    let
        -- Remove leading whitespace
        stripped = Text.dropWhile (`elem` [' ', '\t', '\n', '\r']) query
        -- Remove single-line comments
        noSingleLine = removeSingleLineComments stripped
        -- Remove multi-line comments
        noComments = removeMultiLineComments noSingleLine
        -- Get first word (alphanumeric only)
        firstWord = Text.toUpper $ Text.takeWhile (`elem` (['A' .. 'Z'] ++ ['a' .. 'z'])) noComments
     in
        case firstWord of
            "SELECT" -> Select
            "INSERT" -> Insert
            "UPDATE" -> Update
            "DELETE" -> Delete
            "CREATE" -> Create
            "DROP" -> Drop
            "ALTER" -> Alter
            "WITH" -> Select -- CTEs are read operations (usually)
            _ -> Other
  where
    removeSingleLineComments :: Text -> Text
    removeSingleLineComments t =
        case Text.breakOn "--" t of
            (before, _) -> before

    removeMultiLineComments :: Text -> Text
    removeMultiLineComments t =
        case Text.breakOn "/*" t of
            (before, rest)
                | Text.null rest -> before
                | otherwise ->
                    case Text.breakOn "*/" (Text.drop 2 rest) of
                        (_, end) -> before <> removeMultiLineComments (Text.drop 2 end)

{- | Check if an operation is allowed given an access mode.

* 'ReadOnly' mode allows only 'Select' operations
* 'ReadWrite' mode allows all operations
* 'Snapshot' mode allows all operations (it's read-write on a copy)

Examples:

>>> allowsOperation ReadOnly Select
True

>>> allowsOperation ReadOnly Insert
False

>>> allowsOperation ReadWrite Delete
True

>>> allowsOperation Snapshot Insert
True
-}
allowsOperation :: AccessMode -> SqlOperation -> Bool
allowsOperation ReadOnly Select = True
allowsOperation ReadOnly _ = False
allowsOperation ReadWrite _ = True
allowsOperation Snapshot _ = True

{- | Validate that a query is allowed given an access mode.

Returns 'Right ()' if the query is allowed, or 'Left QueryError'
if access is denied.
-}
validateAccess :: AccessMode -> Text -> Either QueryError ()
validateAccess mode query =
    let operation = classifyQuery query
     in if allowsOperation mode operation
            then Right ()
            else Left $ AccessDeniedError $ "Operation " <> Text.pack (show operation) <> " not allowed in " <> Text.pack (show mode) <> " mode"

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Default busy timeout in milliseconds (5 seconds)
defaultBusyTimeoutMs :: Int
defaultBusyTimeoutMs = 5000

{- | Initialize a SQLite toolbox from a description.

This function:
1. Opens a connection to the SQLite database using both sqlite-simple and direct-sqlite
2. Configures the database for efficient writes (WAL mode, foreign keys, busy timeout)
3. Sets up the connection based on the access mode
4. Creates an MVar lock for serializing access within this toolbox
5. Returns a 'Toolbox' ready for query execution

For snapshot mode:
- The original database is NOT copied during initialization
- The copy is deferred until the first query with a conversation context
- This allows the conversation ID to be used as the snapshot suffix

The access mode determines:
* Whether the database is opened in read-only mode
* What SQL operations will be allowed
* Whether a snapshot copy is created

Concurrent Access Protection:

* The toolbox includes an MVar lock that serializes all queries within this
  toolbox instance. This ensures that even if multiple tool calls are triggered
  concurrently, they execute sequentially.
* A busy timeout of 5 seconds is set on the SQLite connection. If another
  agent (with a different connection) is writing to the same database, this
  agent will wait up to 5 seconds for the lock instead of failing immediately.

Returns an error if the database cannot be opened.
-}
initializeToolbox ::
    Tracer IO Trace ->
    SqliteToolboxDescription ->
    IO (Either String Toolbox)
initializeToolbox tracer desc = do
    runTracer tracer (ConnectionOpenedTrace desc.sqliteToolboxPath)

    let dbPath = desc.sqliteToolboxPath
    let accessMode = fromBaseAccessMode desc.sqliteToolboxAccess

    case accessMode of
        Snapshot -> do
            -- For snapshot mode, we don't create the connection yet
            -- It will be created on first use with the conversation ID
            lock <- newMVar ()
            snapshotState <- newMVar (SnapshotUninitialized dbPath)
            pure $
                Right $
                    Toolbox
                        { toolboxConnection = Nothing
                        , toolboxDirectDb = Nothing
                        , toolboxLock = lock
                        , toolboxName = desc.sqliteToolboxName
                        , toolboxDescription = desc.sqliteToolboxDescription
                        , toolboxPath = dbPath
                        , toolboxAccessMode = accessMode
                        , toolboxSnapshotState = Just snapshotState
                        }
        _ -> do
            -- For read-only and read-write modes, open connection immediately
            result <- try $ openConnection tracer dbPath
            case result of
                Left (e :: SomeException) -> do
                    let errMsg = "Failed to open database: " <> show e
                    runTracer tracer (QueryErrorTrace "initialization" (Text.pack errMsg))
                    pure $ Left errMsg
                Right (conn, directDb) -> do
                    lock <- newMVar ()
                    pure $
                        Right
                            Toolbox
                                { toolboxConnection = Just conn
                                , toolboxDirectDb = Just directDb
                                , toolboxLock = lock
                                , toolboxName = desc.sqliteToolboxName
                                , toolboxDescription = desc.sqliteToolboxDescription
                                , toolboxPath = dbPath
                                , toolboxAccessMode = accessMode
                                , toolboxSnapshotState = Nothing
                                }

-- | Open a database connection with standard configuration.
openConnection :: Tracer IO Trace -> FilePath -> IO (Connection, Direct.Database)
openConnection tracer dbPath = do
    runTracer tracer (ConnectionOpenedTrace dbPath)
    -- Open sqlite-simple connection
    conn <- SQLite.open dbPath
    -- Open direct connection for metadata access
    directDb <- Direct.open (Text.pack dbPath)
    -- Configure database for efficient writes and data integrity
    _ <- SQLite.execute_ conn "PRAGMA foreign_keys = ON"
    _ <- SQLite.execute_ conn "PRAGMA journal_mode = WAL"
    _ <- SQLite.execute_ conn "PRAGMA synchronous = NORMAL"
    _ <- SQLite.execute_ conn $ "PRAGMA busy_timeout = " <> SQLite.Query (Text.pack $ show defaultBusyTimeoutMs)
    pure (conn, directDb)

{- | Create a snapshot copy of the database for the given conversation.
Returns the path to the snapshot database.
-}
createSnapshot ::
    Tracer IO Trace ->
    FilePath ->
    -- | Original database path
    ConversationId ->
    -- | Conversation ID for the suffix
    IO (Either Text FilePath)
createSnapshot tracer originalPath (ConversationId uuid) = do
    let suffix = Text.unpack $ Text.replace "-" "" $ Text.pack $ show uuid
    -- Create snapshot path: original.{uuid}.snapshot.sqlite
    let newSnapshotPath = originalPath -<.> (suffix ++ ".snapshot.sqlite")

    result <- try $ do
        -- Check if original exists
        exists <- doesFileExist originalPath
        when (not exists) $ do
            error $ "Original database does not exist: " ++ originalPath
        -- Copy the database
        copyFile originalPath newSnapshotPath
        pure newSnapshotPath

    case result of
        Left (e :: SomeException) -> do
            let errMsg = Text.pack $ "Failed to create snapshot: " ++ show e
            runTracer tracer (SnapshotErrorTrace newSnapshotPath errMsg)
            pure $ Left errMsg
        Right path -> do
            runTracer tracer (SnapshotCreatedTrace originalPath path)
            pure $ Right path

{- | Ensure snapshot is initialized for snapshot mode.
This is called within the toolbox lock, so it's thread-safe.
-}
ensureSnapshotInitialized ::
    Tracer IO Trace ->
    MVar SnapshotState ->
    ConversationId ->
    IO (Either QueryError (Connection, Direct.Database, FilePath))
ensureSnapshotInitialized tracer stateVar convId = do
    -- Use modifyMVar to atomically check and update state
    modifyMVar stateVar $ \state ->
        case state of
            SnapshotInitialized conn directDb path ->
                -- Already initialized, return existing connection
                pure (state, Right (conn, directDb, path))
            SnapshotUninitialized originalPath -> do
                -- Need to create the snapshot
                snapshotResult <- createSnapshot tracer originalPath convId
                case snapshotResult of
                    Left err ->
                        -- Keep uninitialized state on error
                        pure (state, Left $ SnapshotError err)
                    Right newPath -> do
                        -- Open connection to the snapshot
                        connResult <- try $ openConnection tracer newPath
                        case connResult of
                            Left (e :: SomeException) ->
                                pure (state, Left $ ConnectionError $ Text.pack $ show e)
                            Right (conn, directDb) -> do
                                -- Update state to initialized
                                let newState = SnapshotInitialized conn directDb newPath
                                pure (newState, Right (conn, directDb, newPath))

{- | Close a toolbox and release its resources.

This function should be called when the toolbox is no longer needed
to properly close the database connection.
-}
_closeToolbox :: Tracer IO Trace -> Toolbox -> IO ()
_closeToolbox tracer toolbox = do
    case toolboxConnection toolbox of
        Just conn -> SQLite.close conn
        Nothing -> pure ()
    case toolboxDirectDb toolbox of
        Just db -> Direct.close db
        Nothing -> pure ()
    -- Also close snapshot connection if initialized
    case toolboxSnapshotState toolbox of
        Just stateVar -> do
            state <- withMVar stateVar pure
            case state of
                SnapshotInitialized conn directDb _ -> do
                    SQLite.close conn
                    Direct.close directDb
                _ -> pure ()
        Nothing -> pure ()
    runTracer tracer ConnectionClosedTrace

-------------------------------------------------------------------------------
-- Query Execution
-------------------------------------------------------------------------------

{- | Execute a SQL query and return results.

This is the general-purpose query execution function that respects
the toolbox's configured access mode. Use 'executeReadOnlyQuery'
or 'executeWriteQuery' for more explicit control.

This function acquires the toolbox lock before executing the query,
ensuring that only one query runs at a time within this toolbox instance.

Returns:
* 'Right QueryResult' on successful execution
* 'Left QueryError' on access violation or database error
* 'Left DatabaseLockedError' if the database is busy and timeout is exceeded

Note: For snapshot mode without a conversation context, this function
will return an error. Use 'executeQueryWithContext' instead.
-}
executeQuery :: Toolbox -> Text -> IO (Either QueryError QueryResult)
executeQuery toolbox query =
    executeQueryWithContext toolbox Nothing query

{- | Execute a SQL query with an optional conversation context.

This is the main query execution function that handles all access modes
including snapshot mode which requires a conversation ID.

For snapshot mode:
* If 'Just ConversationId' is provided, creates/uses the snapshot for that conversation
* If 'Nothing' is provided, returns an error (snapshot mode requires context)

For read-only and read-write modes, the conversation context is ignored.
-}
executeQueryWithContext ::
    Toolbox ->
    Maybe ConversationId ->
    Text ->
    IO (Either QueryError QueryResult)
executeQueryWithContext toolbox mConvId query = do
    -- Validate access based on toolbox mode
    case validateAccess (toolboxAccessMode toolbox) query of
        Left err -> do
            runTracer (Tracer (const (pure ()))) (AccessViolationTrace query (classifyQuery query) (toolboxAccessMode toolbox))
            pure $ Left err
        Right () -> do
            -- Acquire lock to serialize access within this toolbox instance
            withMVar (toolboxLock toolbox) $ \() -> do
                case toolboxAccessMode toolbox of
                    Snapshot ->
                        case mConvId of
                            Nothing ->
                                pure $ Left $ SnapshotError "Snapshot mode requires conversation context but none was provided"
                            Just convId ->
                                executeSnapshotQueryInternal toolbox convId query
                    _ ->
                        executeStandardQueryInternal toolbox query

-- | Execute query on a standard (non-snapshot) toolbox.
executeStandardQueryInternal :: Toolbox -> Text -> IO (Either QueryError QueryResult)
executeStandardQueryInternal toolbox query = do
    case (toolboxConnection toolbox, toolboxDirectDb toolbox) of
        (Just conn, Just directDb) ->
            executeQueryInternal conn directDb query
        _ ->
            pure $ Left $ ConnectionError "Database connection not initialized"

-- | Execute query on a snapshot toolbox, ensuring snapshot is created first.
executeSnapshotQueryInternal :: Toolbox -> ConversationId -> Text -> IO (Either QueryError QueryResult)
executeSnapshotQueryInternal toolbox convId query =
    case toolboxSnapshotState toolbox of
        Nothing ->
            pure $ Left $ SnapshotError "Snapshot state not initialized"
        Just stateVar -> do
            -- Ensure snapshot is initialized (thread-safe via MVar)
            initResult <- ensureSnapshotInitialized (Tracer (const (pure ()))) stateVar convId
            case initResult of
                Left err -> pure $ Left err
                Right (conn, directDb, _snapshotPath) ->
                    executeQueryInternal conn directDb query

{- | Execute a read-only (SELECT) query.

This function ensures that only SELECT queries are executed,
regardless of the toolbox's configured access mode. Use this
for additional safety when you only need to read data.

Like 'executeQuery', this function acquires the toolbox lock before
executing the query.

Returns:
* 'Right QueryResult' on successful execution
* 'Left QueryError' if the query is not a SELECT or on database error
-}
executeReadOnlyQuery :: Toolbox -> Text -> IO (Either QueryError QueryResult)
executeReadOnlyQuery toolbox query =
    case classifyQuery query of
        Select ->
            -- Acquire lock to serialize access within this toolbox instance
            withMVar (toolboxLock toolbox) $ \() -> do
                case toolboxAccessMode toolbox of
                    Snapshot ->
                        -- For snapshot mode, we still need conversation context
                        pure $ Left $ SnapshotError "Use executeQueryWithContext for snapshot mode"
                    _ ->
                        executeStandardQueryInternal toolbox query
        other -> do
            let err = AccessDeniedError $ "Expected SELECT query, got: " <> Text.pack (show other)
            pure $ Left err

{- | Execute a write query (INSERT, UPDATE, DELETE, etc.).

This function ensures that the toolbox is in read-write or snapshot mode
before executing the query. Use this for explicit write operations.

Like 'executeQuery', this function acquires the toolbox lock before
executing the query.

Returns:
* 'Right QueryResult' on successful execution
* 'Left QueryError' if the toolbox is read-only or on database error
-}
executeWriteQuery :: Toolbox -> Text -> IO (Either QueryError QueryResult)
executeWriteQuery toolbox query =
    case toolboxAccessMode toolbox of
        ReadOnly -> do
            let err = AccessDeniedError "Write operations not allowed in read-only mode"
            pure $ Left err
        ReadWrite ->
            withMVar (toolboxLock toolbox) $ \() ->
                executeStandardQueryInternal toolbox query
        Snapshot ->
            -- Snapshot mode requires conversation context
            pure $ Left $ SnapshotError "Use executeQueryWithContext for snapshot mode"

{- | Internal function to execute a query and extract results with column names.
This function does NOT acquire the lock - callers must hold the lock.
-}
executeQueryInternal :: Connection -> Direct.Database -> Text -> IO (Either QueryError QueryResult)
executeQueryInternal _conn directDb query = do
    startTime <- getCurrentTime

    result <- try $ do
        let db = directDb

        -- Prepare the statement
        stmt <- Direct.prepare db query

        -- Get column names from the statement
        colCount <- Direct.columnCount stmt
        columnNames <-
            mapM
                ( \i -> do
                    mName <- Direct.columnName stmt i
                    case mName of
                        Just name -> return name
                        Nothing -> return (Text.pack $ "column_" ++ show i)
                )
                [0 .. fromIntegral colCount - 1]

        -- Execute and collect rows
        rows <- collectRows stmt colCount

        Direct.finalize stmt

        let rowCount = length rows
        endTime <- getCurrentTime
        let execTime = diffUTCTime endTime startTime

        pure $
            QueryResult
                { resultColumns = columnNames
                , resultRows = rows
                , resultRowCount = rowCount
                , resultExecutionTime = execTime
                }

    case result of
        Left (e :: SomeException) -> do
            let errMsg = Text.pack $ show e
            let errStr = show e
            -- Check if this is a busy/locked error by examining the error message
            let lowerErr = Text.toLower errMsg
            if "busy" `Text.isInfixOf` lowerErr
                || "locked" `Text.isInfixOf` lowerErr
                || "SQLITE_BUSY" `isInfixOf` errStr
                then pure $ Left $ DatabaseLockedError $ "Database is locked by another process. Try again later. Original error: " <> errMsg
                else pure $ Left $ SqlError errMsg
        Right qr -> pure $ Right qr
  where
    collectRows :: Direct.Statement -> Direct.ColumnCount -> IO [[Value]]
    collectRows stmt colCount = do
        stepResult <- Direct.step stmt
        case stepResult of
            Direct.Row -> do
                row <- mapM (\i -> sqlColumnToJson stmt (fromIntegral i)) [0 .. colCount - 1]
                rest <- collectRows stmt colCount
                pure (row : rest)
            Direct.Done -> pure []

-- | Convert a SQL column value to a JSON value.
sqlColumnToJson :: Direct.Statement -> Direct.ColumnIndex -> IO Value
sqlColumnToJson stmt colIdx = do
    colType <- Direct.columnType stmt colIdx
    case colType of
        Direct.IntegerColumn -> do
            val <- Direct.columnInt64 stmt colIdx
            pure $ Number (fromIntegral val)
        Direct.FloatColumn -> do
            val <- Direct.columnDouble stmt colIdx
            pure $ Number (fromRational (toRational val))
        Direct.TextColumn -> do
            val <- Direct.columnText stmt colIdx
            pure $ String val
        Direct.BlobColumn -> do
            val <- Direct.columnBlob stmt colIdx
            pure $ String (Text.decodeUtf8 val)
        Direct.NullColumn -> pure Null

-------------------------------------------------------------------------------
-- Result Formatting
-------------------------------------------------------------------------------

{- | Format query results as JSON.

Returns a ByteString containing a JSON object with:
* "columns": Array of column names
* "rows": Array of row arrays, or array of row objects (if columns available)
* "rowCount": Number of rows returned
* "executionTime": Execution time in seconds (optional)

Example output:

> {
>   "columns": ["id", "name", "email"],
>   "rows": [
>     [1, "Alice", "alice@example.com"],
>     [2, "Bob", "bob@example.com"]
>   ],
>   "rowCount": 2,
>   "executionTime": 0.003
> }
-}
formatResults :: QueryResult -> ByteString
formatResults result =
    LByteString.toStrict $ Aeson.encode jsonObj
  where
    jsonObj =
        Aeson.object
            [ "columns" Aeson..= resultColumns result
            , "rows" Aeson..= resultRows result
            , "rowCount" Aeson..= resultRowCount result
            , "executionTime" Aeson..= formatExecutionTime (resultExecutionTime result)
            ]

-- | Format execution time as seconds with 3 decimal places.
formatExecutionTime :: NominalDiffTime -> Double
formatExecutionTime = realToFrac

{- | Format query results as compact JSON (single line).

Useful for logging or when space is constrained.
-}
_formatResultsCompact :: QueryResult -> ByteString
_formatResultsCompact result =
    LByteString.toStrict $ Aeson.encode jsonObj
  where
    jsonObj =
        Aeson.object
            [ "columns" Aeson..= resultColumns result
            , "rows" Aeson..= resultRows result
            , "rowCount" Aeson..= resultRowCount result
            ]

{- | Format query results with rows as objects instead of arrays.

Example output:

> {
>   "columns": ["id", "name", "email"],
>   "rows": [
>     {"id": 1, "name": "Alice", "email": "alice@example.com"},
>     {"id": 2, "name": "Bob", "email": "bob@example.com"}
>   ],
>   "rowCount": 2
> }
-}
_formatResultsAsObjects :: QueryResult -> ByteString
_formatResultsAsObjects result =
    LByteString.toStrict $ Aeson.encode jsonObj
  where
    jsonObj =
        Aeson.object
            [ "columns" Aeson..= resultColumns result
            , "rows" Aeson..= map (_rowToObject (resultColumns result)) (resultRows result)
            , "rowCount" Aeson..= resultRowCount result
            , "executionTime" Aeson..= formatExecutionTime (resultExecutionTime result)
            ]

-- | Convert a row (list of values) to an object using column names.
_rowToObject :: [Text] -> [Value] -> Value
_rowToObject cols values =
    Object $ KeyMap.fromList $ zip (map AesonKey.fromText cols) values
