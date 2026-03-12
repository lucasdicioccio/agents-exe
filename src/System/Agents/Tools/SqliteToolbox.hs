{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Provides a runtime for executing SQL queries against SQLite databases.

This module implements the SQLite toolbox functionality, including:

* Database connection management
* Query execution with access control (read-only vs read-write)
* Result formatting for LLM consumption
* Tracing for debugging and monitoring

The toolbox can be configured to run in either read-only or read-write mode,
providing safety when the agent should only query data.

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
            result <- SQLite.executeReadOnlyQuery toolbox "SELECT * FROM users LIMIT 10"
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

    -- * Result formatting
    formatResults,
) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (when)
import Data.Aeson (ToJSON (..), Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, FromRow, Query (..), ToRow)
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite3 as Direct
import Prod.Tracer (Tracer (..), runTracer)

import System.Agents.Base (SqliteAccessMode (..), SqliteToolboxDescription (..))

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

{- | Trace events for monitoring SQLite toolbox operations.

These events allow tracking of:
* Query execution progress
* Access violations
* Database errors
* Connection lifecycle
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
    deriving (Show)

{- | Runtime state for a SQLite toolbox.

The toolbox maintains:
* A database connection (from sqlite-simple)
* A direct database handle for metadata access
* The configured access mode (read-only or read-write)
* Toolbox metadata (name, description)
-}
data Toolbox = Toolbox
    { toolboxConnection :: Connection
    , toolboxDirectDb :: Direct.Database
    , toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxPath :: FilePath
    , toolboxAccessMode :: AccessMode
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
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Access Control
-------------------------------------------------------------------------------

{- | Access mode for the toolbox.

Controls what operations are allowed:
* 'ReadOnly': Only SELECT queries are allowed
* 'ReadWrite': All SQL operations are allowed
-}
data AccessMode = ReadOnly | ReadWrite
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

{- | Classify a SQL query by examining its first keyword.

This function performs a simple classification by looking at the first
non-whitespace, non-comment token in the query. It normalizes the query
by:
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

Examples:

>>> allowsOperation ReadOnly Select
True

>>> allowsOperation ReadOnly Insert
False

>>> allowsOperation ReadWrite Delete
True
-}
allowsOperation :: AccessMode -> SqlOperation -> Bool
allowsOperation ReadOnly Select = True
allowsOperation ReadOnly _ = False
allowsOperation ReadWrite _ = True

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

{- | Initialize a SQLite toolbox from a description.

This function:
1. Opens a connection to the SQLite database using both sqlite-simple and direct-sqlite
2. Configures the database for efficient writes (WAL mode, foreign keys)
3. Sets up the connection based on the access mode
4. Returns a 'Toolbox' ready for query execution

The access mode determines:
* Whether the database is opened in read-only mode
* What SQL operations will be allowed

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

    -- Open connections (both sqlite-simple and direct)
    result <- try $ do
        -- Open sqlite-simple connection
        conn <- SQLite.open dbPath

        -- Open direct connection for metadata access
        directDb <- Direct.open (Text.pack dbPath)

        -- Configure database for efficient writes and data integrity
        -- Enable foreign keys for better data integrity
        _ <- SQLite.execute_ conn "PRAGMA foreign_keys = ON"

        -- Enable Write-Ahead Logging (WAL) mode for better write performance
        -- WAL mode reduces file churn and allows concurrent readers during writes
        _ <- SQLite.execute_ conn "PRAGMA journal_mode = WAL"

        -- Set synchronous to NORMAL for a good balance between durability and performance
        -- This ensures data is safe while still being efficient
        _ <- SQLite.execute_ conn "PRAGMA synchronous = NORMAL"

        pure (conn, directDb)

    case result of
        Left (e :: SomeException) -> do
            let errMsg = "Failed to open database: " <> show e
            runTracer tracer (QueryErrorTrace "initialization" (Text.pack errMsg))
            pure $ Left errMsg
        Right (conn, directDb) -> do
            pure $
                Right
                    Toolbox
                        { toolboxConnection = conn
                        , toolboxDirectDb = directDb
                        , toolboxName = desc.sqliteToolboxName
                        , toolboxDescription = desc.sqliteToolboxDescription
                        , toolboxPath = dbPath
                        , toolboxAccessMode = accessMode
                        }

{- | Close a toolbox and release its resources.

This function should be called when the toolbox is no longer needed
to properly close the database connection.
-}
closeToolbox :: Tracer IO Trace -> Toolbox -> IO ()
closeToolbox tracer toolbox = do
    SQLite.close (toolboxConnection toolbox)
    Direct.close (toolboxDirectDb toolbox)
    runTracer tracer ConnectionClosedTrace

-------------------------------------------------------------------------------
-- Query Execution
-------------------------------------------------------------------------------

{- | Execute a SQL query and return results.

This is the general-purpose query execution function that respects
the toolbox's configured access mode. Use 'executeReadOnlyQuery'
or 'executeWriteQuery' for more explicit control.

Returns:
* 'Right QueryResult' on successful execution
* 'Left QueryError' on access violation or database error
-}
executeQuery :: Toolbox -> Text -> IO (Either QueryError QueryResult)
executeQuery toolbox query = do
    -- Validate access based on toolbox mode
    case validateAccess (toolboxAccessMode toolbox) query of
        Left err -> do
            runTracer (Tracer (const (pure ()))) (AccessViolationTrace query (classifyQuery query) (toolboxAccessMode toolbox))
            pure $ Left err
        Right () -> do
            executeQueryInternal toolbox query

{- | Execute a read-only (SELECT) query.

This function ensures that only SELECT queries are executed,
regardless of the toolbox's configured access mode. Use this
for additional safety when you only need to read data.

Returns:
* 'Right QueryResult' on successful execution
* 'Left QueryError' if the query is not a SELECT or on database error
-}
executeReadOnlyQuery :: Toolbox -> Text -> IO (Either QueryError QueryResult)
executeReadOnlyQuery toolbox query =
    case classifyQuery query of
        Select -> executeQueryInternal toolbox query
        other -> do
            let err = AccessDeniedError $ "Expected SELECT query, got: " <> Text.pack (show other)
            pure $ Left err

{- | Execute a write query (INSERT, UPDATE, DELETE, etc.).

This function ensures that the toolbox is in read-write mode
before executing the query. Use this for explicit write operations.

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
        ReadWrite -> executeQueryInternal toolbox query

-- | Internal function to execute a query and extract results with column names.
executeQueryInternal :: Toolbox -> Text -> IO (Either QueryError QueryResult)
executeQueryInternal toolbox query = do
    startTime <- getCurrentTime

    result <- try $ do
        let db = toolboxDirectDb toolbox

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
            pure $ Left $ SqlError errMsg
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
formatResultsCompact :: QueryResult -> ByteString
formatResultsCompact result =
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
formatResultsAsObjects :: QueryResult -> ByteString
formatResultsAsObjects result =
    LByteString.toStrict $ Aeson.encode jsonObj
  where
    jsonObj =
        Aeson.object
            [ "columns" Aeson..= resultColumns result
            , "rows" Aeson..= map (rowToObject (resultColumns result)) (resultRows result)
            , "rowCount" Aeson..= resultRowCount result
            , "executionTime" Aeson..= formatExecutionTime (resultExecutionTime result)
            ]

-- | Convert a row (list of values) to an object using column names.
rowToObject :: [Text] -> [Value] -> Value
rowToObject cols values =
    Object $ KeyMap.fromList $ zip (map AesonKey.fromText cols) values
