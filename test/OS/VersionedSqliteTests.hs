{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OS.VersionedSqliteTests (versionedSqliteTests) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.List (sortOn)
import Data.Maybe (isJust, isNothing)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (FromRow (..))
import Database.SQLite.Simple.FromRow (field)
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase, (@?=))

import System.Agents.OS.Persistence.VersionedSqlite

newtype NameRow = NameRow { getName :: Text } deriving (Show, Eq)
instance FromRow NameRow where fromRow = NameRow <$> field
instance FromJSON NameRow where parseJSON v = NameRow <$> parseJSON v
instance ToJSON NameRow where toJSON (NameRow n) = toJSON n

withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action = do
    tmpDir <- createTempDirectory "/tmp" "versioned-sqlite-test-"
    bracket (pure tmpDir) removeDirectoryRecursive action

withVersionedDB :: FilePath -> (VersionedDB -> IO ()) -> IO ()
withVersionedDB dir action = do
    let dbPath = dir </> "test.db"
    result <- initializeVersionedDB dbPath defaultVersionedDBConfig
    case result of
        Left err -> assertFailure $ "Failed to initialize database: " ++ Text.unpack err
        Right vdb -> bracket (pure vdb) closeVersionedDB action

withVersionedDBConfig :: FilePath -> VersionedDBConfig -> (VersionedDB -> IO ()) -> IO ()
withVersionedDBConfig dir config action = do
    let dbPath = dir </> "test.db"
    result <- initializeVersionedDB dbPath config
    case result of
        Left err -> assertFailure $ "Failed to initialize database: " ++ Text.unpack err
        Right vdb -> bracket (pure vdb) closeVersionedDB action

versionedSqliteTests :: TestTree
versionedSqliteTests = testGroup "Versioned SQLite Tests"
    [ testGroup "Initialization"
        [ testCase "Initialize new database" $ withTempDir $ \dir -> do
            let dbPath = dir </> "new.db"
            result <- initializeVersionedDB dbPath defaultVersionedDBConfig
            case result of
                Left err -> assertFailure $ "Should initialize: " ++ Text.unpack err
                Right vdb -> do
                    exists <- doesFileExist dbPath
                    assertBool "Database file should exist" exists
                    closeVersionedDB vdb

        , testCase "Versions directory is created" $ withTempDir $ \dir -> do
            let dbPath = dir </> "test.db"
            result <- initializeVersionedDB dbPath defaultVersionedDBConfig
            case result of
                Left err -> assertFailure $ "Init failed: " ++ Text.unpack err
                Right vdb -> do
                    let versionsDir = dbPath ++ ".versions"
                    exists <- doesFileExist versionsDir
                    assertBool "Versions directory should exist" exists
                    closeVersionedDB vdb

        , testCase "Initial version is 0" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                current <- getCurrentVersion vdb
                current @?= 0

        , testCase "Initial versions list is empty" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                versions <- listVersions vdb
                versions @?= []
        ]

    , testGroup "Write Operations"
        [ testCase "INSERT creates a version" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                _ <- executeWrite vdb "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)"
                versionsBefore <- listVersions vdb
                
                result <- executeWrite vdb "INSERT INTO users (name) VALUES ('Alice')"
                case result of
                    Left err -> assertFailure $ Text.unpack err
                    Right writeResult -> do
                        assertBool "Should have new version" (isJust $ writeNewVersion writeResult)
                        assertEqual "Rows affected" 1 (writeRowsAffected writeResult)
                
                versionsAfter <- listVersions vdb
                assertEqual "Should have one more version" (length versionsBefore + 1) (length versionsAfter)

        , testCase "SELECT does not create a version" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                _ <- executeWrite vdb "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)"
                _ <- executeWrite vdb "INSERT INTO users (name) VALUES ('Alice')"
                
                versionsBefore <- listVersions vdb
                
                result <- executeWrite vdb "SELECT * FROM users"
                case result of
                    Left _ -> pure () -- Expected error
                    Right _ -> assertFailure "SELECT should not be allowed as write"
                
                versionsAfter <- listVersions vdb
                assertEqual "Version count should not change" (length versionsBefore) (length versionsAfter)
        ]

    , testGroup "Version Management"
        [ testCase "Manual snapshot creation" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                _ <- executeWrite vdb "CREATE TABLE test (id INTEGER PRIMARY KEY)"
                
                result <- createSnapshot vdb "Manual backup before migration"
                case result of
                    Left err -> assertFailure $ Text.unpack err
                    Right versionInfo -> do
                        versionDescription versionInfo @?= "Manual backup before migration"
                        assertBool "Version ID should be positive" (versionId versionInfo > 0)

        , testCase "Delete version removes it from list" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                _ <- executeWrite vdb "CREATE TABLE test (id INTEGER PRIMARY KEY)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (1)"
                
                versionsBefore <- listVersions vdb
                let vid = versionId $ head versionsBefore
                
                result <- deleteVersion vdb vid
                case result of
                    Left err -> assertFailure $ Text.unpack err
                    Right () -> do
                        versionsAfter <- listVersions vdb
                        assertEqual "Should have one less version" (length versionsBefore - 1) (length versionsAfter)
        ]

    , testGroup "Rollback"
        [ testCase "Rollback to previous version restores data" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                _ <- executeWrite vdb "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)"
                _ <- executeWrite vdb "INSERT INTO users (name) VALUES ('Alice')"
                _ <- executeWrite vdb "INSERT INTO users (name) VALUES ('Bob')"
                
                versions <- listVersions vdb
                let versionBeforeBob = versionId $ versions !! 1
                
                result <- rollbackToVersion vdb versionBeforeBob
                case result of
                    Left err -> assertFailure $ Text.unpack err
                    Right _ -> do
                        queryResult <- executeQuery vdb "SELECT name FROM users ORDER BY id" :: IO (Either Text [NameRow])
                        case queryResult of
                            Left err -> assertFailure $ Text.unpack err
                            Right rows -> do
                                length rows @?= 1
                                getName (head rows) @?= "Alice"

        , testCase "Rollback to non-existent version fails" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                result <- rollbackToVersion vdb 999
                case result of
                    Left _ -> pure () -- Expected error
                    Right _ -> assertFailure "Should fail for non-existent version"
        ]

    , testGroup "Retention Policy"
        [ testCase "Auto-cleanup removes old versions" $ withTempDir $ \dir ->
            withVersionedDBConfig dir defaultVersionedDBConfig{maxVersions = 3} $ \vdb -> do
                _ <- executeWrite vdb "CREATE TABLE test (id INTEGER PRIMARY KEY)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (1)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (2)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (3)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (4)"
                
                versions <- listVersions vdb
                assertBool "Should have at most 3 versions" (length versions <= 3)

        , testCase "SetMaxVersions triggers cleanup" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                _ <- executeWrite vdb "CREATE TABLE test (id INTEGER PRIMARY KEY)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (1)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (2)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (3)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (4)"
                
                versionsBefore <- listVersions vdb
                setMaxVersions vdb 2
                versionsAfter <- listVersions vdb
                
                assertBool "Should have fewer versions after setting max" 
                    (length versionsAfter < length versionsBefore)
                assertEqual "Should have exactly 2 versions" 2 (length versionsAfter)
        ]

    , testGroup "Storage Statistics"
        [ testCase "getStorageStats returns valid stats" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                _ <- executeWrite vdb "CREATE TABLE test (id INTEGER PRIMARY KEY)"
                _ <- executeWrite vdb "INSERT INTO test VALUES (1)"
                
                stats <- getStorageStats vdb
                assertBool "Original size should be positive" (statsOriginalSize stats > 0)
                assertEqual "Should have 2 versions" 2 (statsVersionsCount stats)
                assertBool "Total size should be positive" (statsVersionsTotalSize stats > 0)
                assertBool "Average size should be positive" (statsAverageVersionSize stats > 0)
                assertBool "Should have oldest version" (isJust $ statsOldestVersion stats)
                assertBool "Should have newest version" (isJust $ statsNewestVersion stats)
        ]

    , testGroup "Configuration"
        [ testCase "Custom versions directory" $ withTempDir $ \dir -> do
            let customDir = dir </> "custom_versions"
            let config = defaultVersionedDBConfig{versionsDirectory = Just customDir}
            let dbPath = dir </> "test.db"
            
            result <- initializeVersionedDB dbPath config
            case result of
                Left err -> assertFailure $ Text.unpack err
                Right vdb -> do
                    closeVersionedDB vdb
                    exists <- doesFileExist customDir
                    assertBool "Custom versions directory should exist" exists

        , testCase "getMaxVersions returns config value" $ withTempDir $ \dir ->
            withVersionedDBConfig dir defaultVersionedDBConfig{maxVersions = 42} $ \vdb -> do
                maxV <- getMaxVersions vdb
                maxV @?= 42
        ]

    , testGroup "Error Handling"
        [ testCase "Invalid SQL returns error" $ withTempDir $ \dir ->
            withVersionedDB dir $ \vdb -> do
                result <- executeWrite vdb "INVALID SQL SYNTAX"
                case result of
                    Left _ -> pure () -- Expected
                    Right _ -> assertFailure "Should return error for invalid SQL"

        , testCase "Database persists across re-initialization" $ withTempDir $ \dir -> do
            let dbPath = dir </> "persistent.db"
            
            result1 <- initializeVersionedDB dbPath defaultVersionedDBConfig
            case result1 of
                Left err -> assertFailure $ Text.unpack err
                Right vdb1 -> do
                    _ <- executeWrite vdb1 "CREATE TABLE data (value TEXT)"
                    _ <- executeWrite vdb1 "INSERT INTO data VALUES ('persistent')"
                    closeVersionedDB vdb1
            
            result2 <- initializeVersionedDB dbPath defaultVersionedDBConfig
            case result2 of
                Left err -> assertFailure $ Text.unpack err
                Right vdb2 -> do
                    queryResult <- executeQuery vdb2 "SELECT value FROM data" :: IO (Either Text [NameRow])
                    case queryResult of
                        Left err -> assertFailure $ Text.unpack err
                        Right rows -> do
                            getName (head rows) @?= "persistent"
                    closeVersionedDB vdb2
        ]
    ]

