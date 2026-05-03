{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Versioned SQLite database with automatic snapshotting after each write operation.

This module provides:

* Automatic snapshots after each write transaction
* Version history tracking with metadata
* Rollback capability to any previous version
* Configurable retention policy for version cleanup
* Storage-efficient versioning with complete database copies

== Example Usage

>>> import System.Agents.OS.Persistence.VersionedSqlite
>>>
>>> -- Initialize a versioned database with max 10 versions
>>> vdb <- initializeVersionedDB "mydb.sqlite" defaultVersionedDBConfig
>>>
>>> -- Execute a write operation (automatically creates snapshot)
>>> result <- executeWrite vdb "INSERT INTO users (name) VALUES ('Alice')"
>>>
>>> -- List all versions
>>> versions <- listVersions vdb
>>> print versions
[VersionInfo {versionId = 3, createdAt = ..., description = "Auto-snapshot after write"}]
>>>
>>> -- Rollback to previous version
>>> rollbackToVersion vdb 2

== Version Management

Versions are created automatically after each write operation (INSERT, UPDATE, DELETE,
CREATE, DROP, ALTER). Each version includes:

* Unique version ID (sequential integer)
* Timestamp of creation
* Description (auto-generated or custom)
* Complete database snapshot

== Storage Strategy

Versions are stored as complete SQLite database copies in a dedicated directory
next to the original database. While this uses more storage than incremental
deltas, it provides:

* Fast rollback (simple file copy)
* Full data integrity (no delta reconstruction)
* Easy inspection of any version
* Independence from SQLite journaling

The retention policy automatically cleans up old versions when the maximum is exceeded.
-}
module System.Agents.OS.Persistence.VersionedSqlite (
    -- * Types
    VersionedDB,
    VersionId,
    VersionInfo (..),
    VersionedDBConfig (..),
    defaultVersionedDBConfig,
    WriteResult (..),

    -- * Initialization
    initializeVersionedDB,
    closeVersionedDB,

    -- * Query Execution
    executeQuery,
    executeWrite,

    -- * Version Management
    listVersions,
    getCurrentVersion,
    rollbackToVersion,
    rollbackToPrevious,
    createSnapshot,
    deleteVersion,

    -- * Configuration
    setMaxVersions,
    getMaxVersions,
    cleanupOldVersions,

    -- * Storage Info
    getStorageStats,
    StorageStats (..),
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (isPrefixOf, tails)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, Query (..))
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite3 as Direct
import GHC.Generics (Generic)
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getFileSize,
    listDirectory,
    removeFile,
    renameFile,
 )
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Unique identifier for a version.
type VersionId = Int

-- | Information about a database version.
data VersionInfo = VersionInfo
    { versionId :: !VersionId
    -- ^ Unique version identifier (sequential)
    , versionCreatedAt :: !UTCTime
    -- ^ When this version was created
    , versionDescription :: !Text
    -- ^ Description of what created this version
    , versionSizeBytes :: !Integer
    -- ^ Size of the snapshot in bytes
    }
    deriving (Show, Eq, Generic)

instance ToJSON VersionInfo
instance FromJSON VersionInfo

-- | Configuration for versioned database behavior.
data VersionedDBConfig = VersionedDBConfig
    { maxVersions :: !Int
    -- ^ Maximum number of versions to retain (default: 10)
    , autoCleanup :: !Bool
    -- ^ Automatically cleanup old versions when max is exceeded (default: True)
    , versionsDirectory :: !(Maybe FilePath)
    -- ^ Custom directory for version storage (default: adjacent to main DB)
    , snapshotOnWrite :: !Bool
    -- ^ Create snapshot after every write (default: True)
    }
    deriving (Show, Eq, Generic)

instance ToJSON VersionedDBConfig
instance FromJSON VersionedDBConfig

-- | Default configuration for versioned database.
defaultVersionedDBConfig :: VersionedDBConfig
defaultVersionedDBConfig =
    VersionedDBConfig
        { maxVersions = 10
        , autoCleanup = True
        , versionsDirectory = Nothing
        , snapshotOnWrite = True
        }

-- | Internal state tracking for version management.
data VersionState = VersionState
    { currentVersion :: !VersionId
    -- ^ Current version ID (0 = initial, no snapshots yet)
    , versions :: ![VersionInfo]
    -- ^ List of all available versions (newest first)
    , currentConnection :: !(Maybe Connection)
    -- ^ Current active database connection
    , currentDirectDb :: !(Maybe Direct.Database)
    -- ^ Direct database handle for metadata
    }

-- | Versioned database handle.
data VersionedDB = VersionedDB
    { vdbOriginalPath :: !FilePath
    -- ^ Path to the original database file
    , vdbVersionsDir :: !FilePath
    -- ^ Directory where version snapshots are stored
    , vdbConfig :: !VersionedDBConfig
    -- ^ Configuration settings
    , vdbState :: !(MVar VersionState)
    -- ^ Mutable state (protected by MVar)
    , vdbLock :: !(MVar ())
    -- ^ Lock for serializing write operations
    }

-- | Result of a write operation.
data WriteResult = WriteResult
    { writeRowsAffected :: !Int
    -- ^ Number of rows affected by the write
    , writeNewVersion :: !(Maybe VersionInfo)
    -- ^ New version created (if snapshotOnWrite is enabled)
    , writeExecutionTime :: !Double
    -- ^ Execution time in seconds
    }
    deriving (Show, Eq, Generic)

instance ToJSON WriteResult

-- | Storage statistics for the versioned database.
data StorageStats = StorageStats
    { statsOriginalSize :: !Integer
    -- ^ Size of original database in bytes
    , statsVersionsCount :: !Int
    -- ^ Number of stored versions
    , statsVersionsTotalSize :: !Integer
    -- ^ Total size of all versions in bytes
    , statsAverageVersionSize :: !Double
    -- ^ Average version size in bytes
    , statsOldestVersion :: !(Maybe VersionId)
    -- ^ ID of oldest retained version
    , statsNewestVersion :: !(Maybe VersionId)
    -- ^ ID of newest version
    }
    deriving (Show, Eq, Generic)

instance ToJSON StorageStats

-- | SQL operation types for classification.
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

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | Initialize a versioned SQLite database.
initializeVersionedDB :: FilePath -> VersionedDBConfig -> IO (Either Text VersionedDB)
initializeVersionedDB dbPath config = do
    -- Ensure parent directory exists
    createDirectoryIfMissing True (takeDirectory dbPath)

    -- Create database if it doesn't exist
    dbExists <- doesFileExist dbPath
    unless dbExists $ do
        conn <- SQLite.open dbPath
        SQLite.close conn

    -- Determine versions directory
    let versionsDir = case config.versionsDirectory of
            Just dir -> dir
            Nothing -> dbPath ++ ".versions"

    -- Create versions directory
    createDirectoryIfMissing True versionsDir

    -- Load existing versions
    existingVersions <- loadExistingVersions versionsDir

    -- Open connection to original database
    (conn, directDb) <- openConnection dbPath

    -- Create initial state
    let initialVersion = case existingVersions of
            (v : _) -> versionId v
            [] -> 0
    state <-
        newMVar
            VersionState
                { currentVersion = initialVersion
                , versions = existingVersions
                , currentConnection = Just conn
                , currentDirectDb = Just directDb
                }

    lock <- newMVar ()

    pure $ Right VersionedDB{vdbOriginalPath = dbPath, vdbVersionsDir = versionsDir, vdbConfig = config, vdbState = state, vdbLock = lock}

-- | Close a versioned database and release resources.
closeVersionedDB :: VersionedDB -> IO ()
closeVersionedDB vdb = do
    withMVar vdb.vdbState $ \state -> do
        case state.currentConnection of
            Just conn -> SQLite.close conn
            Nothing -> pure ()
        case state.currentDirectDb of
            Just db -> Direct.close db
            Nothing -> pure ()

-------------------------------------------------------------------------------
-- Query Execution
-------------------------------------------------------------------------------

-- | Execute a read-only query against the current database state.
executeQuery :: (FromJSON a, SQLite.FromRow a) => VersionedDB -> Text -> IO (Either Text [a])
executeQuery vdb query = do
    withMVar vdb.vdbLock $ \_ -> do
        withMVar vdb.vdbState $ \state -> do
            case state.currentConnection of
                Just conn -> do
                    result <- try $ SQLite.query_ conn (Query query)
                    case result of
                        Left (e :: SomeException) -> pure $ Left $ Text.pack $ show e
                        Right rows -> pure $ Right rows
                Nothing -> pure $ Left "Database connection not available"

-- | Execute a write operation and optionally create a snapshot.
executeWrite :: VersionedDB -> Text -> IO (Either Text WriteResult)
executeWrite vdb query = do
    -- First classify the query to ensure it's a write operation
    let sqlOp = classifyQuery query
    if isWriteOperation sqlOp
        then executeWriteInternal vdb query
        else pure $ Left $ "Not a write operation: " <> Text.pack (show sqlOp)

executeWriteInternal :: VersionedDB -> Text -> IO (Either Text WriteResult)
executeWriteInternal vdb query =
    withMVar vdb.vdbLock $ \_ -> do
        startTime <- getCurrentTime

        -- Execute the write operation
        writeResult <-
            withMVar vdb.vdbState $ \state -> do
                case state.currentConnection of
                    Just conn -> do
                        result <- try $ SQLite.execute_ conn (Query query)
                        case result of
                            Left (e :: SomeException) -> pure $ Left $ Text.pack $ show e
                            Right () -> do
                                -- Get rows affected
                                rowsAffected <- SQLite.changes conn
                                pure $ Right rowsAffected
                    Nothing -> pure $ Left "Database connection not available"

        case writeResult of
            Left err -> pure $ Left err
            Right rowsAffected -> do
                -- Create snapshot if enabled
                mVersionInfo <-
                    if vdb.vdbConfig.snapshotOnWrite
                        then do
                            eVersion <- createSnapshotInternal vdb ("Auto-snapshot after write: " <> Text.take 100 query)
                            case eVersion of
                                Left _ -> pure Nothing
                                Right version -> pure $ Just version
                        else pure Nothing

                endTime <- getCurrentTime
                let execTime = realToFrac (diffUTCTime endTime startTime)

                pure $ Right WriteResult{writeRowsAffected = rowsAffected, writeNewVersion = mVersionInfo, writeExecutionTime = execTime}

isWriteOperation :: SqlOperation -> Bool
isWriteOperation Insert = True
isWriteOperation Update = True
isWriteOperation Delete = True
isWriteOperation Create = True
isWriteOperation Drop = True
isWriteOperation Alter = True
isWriteOperation _ = False

-- | Classify a SQL query by examining its first keyword.
classifyQuery :: Text -> SqlOperation
classifyQuery query =
    let stripped = Text.dropWhile (`elem` [' ', '\t', '\n', '\r']) query
        noSingleLine = case Text.breakOn "--" stripped of
            (before, _) -> before
        noComments = removeMultiLineComments noSingleLine
        firstWord = Text.toUpper $ Text.takeWhile (`elem` (['A' .. 'Z'] ++ ['a' .. 'z'])) noComments
     in case firstWord of
            "SELECT" -> Select
            "INSERT" -> Insert
            "UPDATE" -> Update
            "DELETE" -> Delete
            "CREATE" -> Create
            "DROP" -> Drop
            "ALTER" -> Alter
            "WITH" -> Select
            _ -> Other

removeMultiLineComments :: Text -> Text
removeMultiLineComments t =
    case Text.breakOn "/*" t of
        (before, rest)
            | Text.null rest -> before
            | otherwise -> case Text.breakOn "*/" (Text.drop 2 rest) of
                (_, end) -> before <> removeMultiLineComments (Text.drop 2 end)

-------------------------------------------------------------------------------
-- Version Management
-------------------------------------------------------------------------------

-- | List all available versions, newest first.
listVersions :: VersionedDB -> IO [VersionInfo]
listVersions vdb = do
    withMVar vdb.vdbState $ \state -> do
        pure state.versions

-- | Get the current version ID.
getCurrentVersion :: VersionedDB -> IO VersionId
getCurrentVersion vdb = do
    withMVar vdb.vdbState $ \state -> do
        pure state.currentVersion

-- | Create a manual snapshot with a custom description.
createSnapshot :: VersionedDB -> Text -> IO (Either Text VersionInfo)
createSnapshot vdb description =
    withMVar vdb.vdbLock $ \_ -> createSnapshotInternal vdb description

createSnapshotInternal :: VersionedDB -> Text -> IO (Either Text VersionInfo)
createSnapshotInternal vdb description = do
    modifyMVar vdb.vdbState $ \state -> do
        let newVersionId = state.currentVersion + 1
        let snapshotPath = versionPath vdb newVersionId

        -- Copy current database to version snapshot
        result <- try $ do
            copyFile vdb.vdbOriginalPath snapshotPath
            -- Also copy WAL files if they exist
            copyWalFiles vdb.vdbOriginalPath snapshotPath

        case result of
            Left (e :: SomeException) -> do
                let err = Text.pack $ "Failed to create snapshot: " ++ show e
                pure (state, Left err)
            Right () -> do
                now <- getCurrentTime
                size <- getFileSize snapshotPath

                let versionInfo =
                        VersionInfo
                            { versionId = newVersionId
                            , versionCreatedAt = now
                            , versionDescription = description
                            , versionSizeBytes = size
                            }

                let newVersions = versionInfo : state.versions
                let newState =
                        state
                            { currentVersion = newVersionId
                            , versions = newVersions
                            }

                -- Cleanup old versions if needed
                when (vdb.vdbConfig.autoCleanup && length newVersions > vdb.vdbConfig.maxVersions) $ do
                    cleanupOldVersionsInternal vdb newState

                pure (newState, Right versionInfo)

copyWalFiles :: FilePath -> FilePath -> IO ()
copyWalFiles originalPath snapshotPath = do
    let originalWal = originalPath ++ "-wal"
    let snapshotWal = snapshotPath ++ "-wal"
    walExists <- doesFileExist originalWal
    when walExists $ copyFile originalWal snapshotWal

    let originalShm = originalPath ++ "-shm"
    let snapshotShm = snapshotPath ++ "-shm"
    shmExists <- doesFileExist originalShm
    when shmExists $ copyFile originalShm snapshotShm

-- | Rollback to a specific version.
rollbackToVersion :: VersionedDB -> VersionId -> IO (Either Text VersionInfo)
rollbackToVersion vdb targetVersionId =
    withMVar vdb.vdbLock $ \_ -> do
        -- Find the version
        mVersion <- findVersion vdb targetVersionId
        case mVersion of
            Nothing -> pure $ Left $ "Version " <> Text.pack (show targetVersionId) <> " not found"
            Just versionInfo -> do
                let snapshotPath = versionPath vdb targetVersionId

                -- Verify snapshot exists
                snapshotExists <- doesFileExist snapshotPath
                unless snapshotExists $ do
                    error $ "Snapshot file missing: " ++ snapshotPath

                -- Perform atomic rollback using temp file
                result <- try $ atomicReplace vdb.vdbOriginalPath snapshotPath

                case result of
                    Left (e :: SomeException) -> do
                        pure $ Left $ Text.pack $ "Rollback failed: " ++ show e
                    Right () -> do
                        -- Reopen connection to new database
                        modifyMVar_ vdb.vdbState $ \state -> do
                            -- Close old connection
                            case state.currentConnection of
                                Just conn -> SQLite.close conn
                                Nothing -> pure ()
                            case state.currentDirectDb of
                                Just db -> Direct.close db
                                Nothing -> pure ()

                            -- Open new connection
                            (newConn, newDirectDb) <- openConnection vdb.vdbOriginalPath

                            pure $
                                state
                                    { currentVersion = targetVersionId
                                    , currentConnection = Just newConn
                                    , currentDirectDb = Just newDirectDb
                                    }

                        pure $ Right versionInfo

-- | Rollback to the previous version (if any).
rollbackToPrevious :: VersionedDB -> IO (Either Text VersionInfo)
rollbackToPrevious vdb = do
    versions <- listVersions vdb
    current <- getCurrentVersion vdb

    -- Find the version immediately before current
    let olderVersions = filter (\v -> versionId v < current) versions
    case listToMaybe olderVersions of
        Just prevVersion -> rollbackToVersion vdb (versionId prevVersion)
        Nothing -> pure $ Left "No previous version to rollback to"

-- | Delete a specific version.
deleteVersion :: VersionedDB -> VersionId -> IO (Either Text ())
deleteVersion vdb vid = do
    let snapshotPath = versionPath vdb vid
    exists <- doesFileExist snapshotPath
    if not exists
        then pure $ Left $ "Version " <> Text.pack (show vid) <> " not found"
        else do
            result <- try $ removeFile snapshotPath
            case result of
                Left (e :: SomeException) -> pure $ Left $ Text.pack $ show e
                Right () -> do
                    -- Update state to remove from list
                    modifyMVar_ vdb.vdbState $ \state -> do
                        let newVersions = filter (\v -> versionId v /= vid) state.versions
                        pure $ state{versions = newVersions}
                    pure $ Right ()

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Update the maximum number of versions to retain.
setMaxVersions :: VersionedDB -> Int -> IO ()
setMaxVersions vdb newMax = do
    modifyMVar_ vdb.vdbState $ \state -> do
        when (length state.versions > newMax) $ do
            cleanupOldVersionsInternal vdb state{versions = take newMax state.versions}
        pure state

-- | Get the current maximum versions setting.
getMaxVersions :: VersionedDB -> IO Int
getMaxVersions vdb = pure vdb.vdbConfig.maxVersions

-- | Manually trigger cleanup of old versions.
cleanupOldVersions :: VersionedDB -> IO Int
cleanupOldVersions vdb = do
    withMVar vdb.vdbLock $ \_ -> do
        modifyMVar vdb.vdbState $ \state -> do
            let toDelete = drop vdb.vdbConfig.maxVersions state.versions
            deletedCount <- cleanupVersions vdb toDelete
            let newVersions = take vdb.vdbConfig.maxVersions state.versions
            pure (state{versions = newVersions}, deletedCount)

cleanupOldVersionsInternal :: VersionedDB -> VersionState -> IO ()
cleanupOldVersionsInternal vdb state = do
    let toDelete = drop vdb.vdbConfig.maxVersions state.versions
    _ <- cleanupVersions vdb toDelete
    pure ()

cleanupVersions :: VersionedDB -> [VersionInfo] -> IO Int
cleanupVersions vdb versionsToDelete = do
    results <- mapM deleteVersionFile versionsToDelete
    pure $ length $ filter id results
  where
    deleteVersionFile versionInfo = do
        let path = versionPath vdb (versionId versionInfo)
        result <- try $ removeFile path
        case result of
            Left (_ :: SomeException) -> pure False
            Right () -> pure True

-------------------------------------------------------------------------------
-- Storage Info
-------------------------------------------------------------------------------

-- | Get storage statistics for the versioned database.
getStorageStats :: VersionedDB -> IO StorageStats
getStorageStats vdb = do
    versions <- listVersions vdb

    originalSize <- do
        exists <- doesFileExist vdb.vdbOriginalPath
        if exists
            then getFileSize vdb.vdbOriginalPath
            else pure 0

    let versionSizes = map versionSizeBytes versions
    let totalVersionSize = sum versionSizes
    let avgSize = if null versions then 0.0 else fromIntegral totalVersionSize / fromIntegral (length versions)

    let oldestId = case versions of
            [] -> Nothing
            vs -> Just $ versionId $ last vs
    let newestId = case versions of
            [] -> Nothing
            (v : _) -> Just $ versionId v

    pure $
        StorageStats
            { statsOriginalSize = originalSize
            , statsVersionsCount = length versions
            , statsVersionsTotalSize = totalVersionSize
            , statsAverageVersionSize = avgSize
            , statsOldestVersion = oldestId
            , statsNewestVersion = newestId
            }

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Open a database connection with standard configuration.
openConnection :: FilePath -> IO (Connection, Direct.Database)
openConnection dbPath = do
    conn <- SQLite.open dbPath
    directDb <- Direct.open (Text.pack dbPath)
    _ <- SQLite.execute_ conn "PRAGMA foreign_keys = ON"
    _ <- SQLite.execute_ conn "PRAGMA journal_mode = WAL"
    _ <- SQLite.execute_ conn "PRAGMA synchronous = NORMAL"
    _ <- SQLite.execute_ conn "PRAGMA busy_timeout = 5000"
    pure (conn, directDb)

-- | Generate the file path for a specific version.
versionPath :: VersionedDB -> VersionId -> FilePath
versionPath vdb vid =
    let baseName = takeBaseName vdb.vdbOriginalPath
        ext = takeExtension vdb.vdbOriginalPath
     in vdb.vdbVersionsDir </> baseName <> ".v" <> show vid <> ext

-- | Find a version by ID.
findVersion :: VersionedDB -> VersionId -> IO (Maybe VersionInfo)
findVersion vdb vid = do
    withMVar vdb.vdbState $ \state -> do
        pure $ listToMaybe $ filter (\v -> versionId v == vid) state.versions

-- | Load existing versions from the versions directory.
loadExistingVersions :: FilePath -> IO [VersionInfo]
loadExistingVersions versionsDir = do
    exists <- doesDirectoryExist versionsDir
    if not exists
        then pure []
        else do
            files <- listDirectory versionsDir
            let versionFiles = filter (".v" `isInfixOf`) files
            mapM parseVersionFile versionFiles
  where
    parseVersionFile :: FilePath -> IO VersionInfo
    parseVersionFile fileName = do
        let path = versionsDir </> fileName
        -- Extract version ID from filename (e.g., "db.v42.sqlite" -> 42)
        let vid = extractVersionId fileName
        size <- getFileSize path
        -- Use file modification time as creation time
        now <- getCurrentTime
        pure $
            VersionInfo
                { versionId = vid
                , versionCreatedAt = now
                , versionDescription = "Restored from existing snapshot"
                , versionSizeBytes = size
                }

    extractVersionId :: FilePath -> Int
    extractVersionId fileName =
        case break (== '.') fileName of
            (_, '.' : 'v' : rest) ->
                case reads (takeWhile (/= '.') rest) of
                    [(n, _)] -> n
                    _ -> 0
            _ -> 0

-- | Atomically replace a file with another.
atomicReplace :: FilePath -> FilePath -> IO ()
atomicReplace target source = do
    let tempPath = target ++ ".tmp"
    copyFile source tempPath
    renameFile tempPath target

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
