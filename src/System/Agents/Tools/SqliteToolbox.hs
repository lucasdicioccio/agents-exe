{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module System.Agents.Tools.SqliteToolbox where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Database.SQLite3 as Direct
import Prod.Tracer (Tracer (..), runTracer)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))

import System.Agents.Base (
    SqliteSharing (..),
    SqliteToolboxDescription (..),
    SqliteVersionHandle (..),
    SqliteVersioningConfig (..),
 )
import System.Agents.Session.Types (SessionId (..), TurnId (..))
import System.Agents.Tools.Context (ToolExecutionContext (..))

-- Types
data Trace = ConnectionOpenedTrace FilePath | QueryStartedTrace Text
    deriving (Show)

data SqlOperation = Select | Insert | Update | Delete | Create | Drop | Alter | Other
    deriving (Show, Eq)

data SqliteQueryInput = SqliteQueryInput
    { inputSql :: Text
    , inputRestoreFrom :: Maybe SqliteVersionHandle
    }
    deriving (Show)

instance FromJSON SqliteQueryInput where
    parseJSON = Aeson.withObject "SqliteQueryInput" $ \v ->
        SqliteQueryInput <$> v .: "sql" <*> v .:? "restore_from"

data PromoteInput = PromoteInput
    { promoteRestoreFrom :: Maybe SqliteVersionHandle
    }
    deriving (Show)

instance FromJSON PromoteInput where
    parseJSON = Aeson.withObject "PromoteInput" $ \v ->
        PromoteInput <$> v .:? "restore_from"

data PromoteResult = PromoteResult
    { promotedFrom :: FilePath
    , promotedTo   :: FilePath
    }
    deriving (Show)

instance ToJSON PromoteResult where
    toJSON r = Aeson.object
        [ "promoted_from" .= promotedFrom r
        , "promoted_to"   .= promotedTo r
        ]

data QueryResult = QueryResult
    { resultColumns :: [Text]
    , resultRows :: [[Aeson.Value]]
    , resultRowCount :: Int
    , resultExecutionTime :: NominalDiffTime
    , resultVersionHandle :: Maybe SqliteVersionHandle
    }
    deriving (Show)

instance ToJSON QueryResult where
    toJSON result =
        Aeson.object $
            [ "columns" .= resultColumns result
            , "rows" .= resultRows result
            , "rowCount" .= resultRowCount result
            , "executionTime" .= resultExecutionTime result
            ]
                ++ ["versionHandle" .= h | Just h <- [resultVersionHandle result]]

data QueryError = SqlError Text | ConnectionError Text | RestoreError Text
    deriving (Show, Eq)

-- | Key identifying the current head within a sharing scope.
data HeadKey
    = GlobalHead
    | ConversationHead !SessionId
    | TurnHead !SessionId !TurnId
    deriving (Eq, Ord)

data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxDescription :: Text
    , toolboxConfig :: SqliteToolboxDescription
    , toolboxLock :: MVar ()
    , toolboxWriteCounter :: TVar Int
    , toolboxDirectDb :: Maybe Direct.Database
    -- ^ Persistent connection for ReadOnly/ReadWrite modes; Nothing for Versioned
    , toolboxVersionedHeads :: TVar (Map HeadKey FilePath)
    -- ^ Current head path per sharing scope for SqliteVersioned; empty for other modes
    }

data ToolDescription = ToolDescription
    { toolDescriptionName :: Text
    , toolDescriptionDescription :: Text
    , toolDescriptionToolboxName :: Text
    }
    deriving (Show)

-- Path resolution
getDefaultStorageRoot :: IO FilePath
getDefaultStorageRoot = do
    mXdgData <- lookupEnv "XDG_DATA_HOME"
    case mXdgData of
        Just xdg -> pure $ xdg </> "agents" </> "sqlite-versions"
        Nothing -> do
            mHome <- lookupEnv "HOME"
            case mHome of
                Just home -> pure $ home </> ".local" </> "share" </> "agents" </> "sqlite-versions"
                Nothing -> pure "/tmp/agents-sqlite-versions"

getStorageRoot :: SqliteVersioningConfig -> IO FilePath
getStorageRoot (SqliteReadOnly _)  = fail "Cannot get storage root for non-versioned toolbox"
getStorageRoot (SqliteReadWrite _) = fail "Cannot get storage root for non-versioned toolbox"
getStorageRoot (SqliteVersioned _ mRoot _) = case mRoot of
    Just path -> pure path
    Nothing   -> getDefaultStorageRoot

sessionIdToPath :: SessionId -> String
sessionIdToPath (SessionId uuid) = map (\c -> if c == '-' then '_' else c) (show uuid)

turnIdToPath :: TurnId -> String
turnIdToPath (TurnId uuid) = map (\c -> if c == '-' then '_' else c) (show uuid)

-- | Unique path for snapshot N produced by a specific session/turn.
buildSnapshotPath :: FilePath -> SessionId -> TurnId -> Int -> Text -> FilePath
buildSnapshotPath root session turn idx name =
    root
        </> sessionIdToPath session
        </> turnIdToPath turn
        </> ("snap-" ++ show idx)
        </> (Text.unpack name ++ ".sqlite")

-- | HeadKey for a given sharing scope.
sharingToHeadKey :: SqliteSharing -> SessionId -> TurnId -> Maybe HeadKey
sharingToHeadKey SharingGlobal       _       _    = Just GlobalHead
sharingToHeadKey SharingConversation session _    = Just (ConversationHead session)
sharingToHeadKey SharingTurn         session turn = Just (TurnHead session turn)
sharingToHeadKey SharingToolCall     _       _    = Nothing

resolveVersionPath :: SqliteVersioningConfig -> Text -> SqliteVersionHandle -> IO (Either QueryError FilePath)
resolveVersionPath config expectedName handle =
    if vhToolboxName handle /= expectedName
        then pure $ Left $ RestoreError "Version handle toolbox name mismatch"
        else do
            root <- getStorageRoot config
            case (parseUuidText (vhSessionId handle), parseUuidText (vhTurnId handle)) of
                (Just sessionUuid, Just turnUuid) -> do
                    let path = buildSnapshotPath root (SessionId sessionUuid) (TurnId turnUuid) (vhToolCallIndex handle) (vhToolboxName handle)
                    exists <- doesFileExist path
                    if exists
                        then pure $ Right path
                        else pure $ Left $ RestoreError $ "Version not found: " <> Text.pack path
                _ -> pure $ Left $ RestoreError "Invalid UUID in version handle"
  where
    parseUuidText txt = UUID.fromText (Text.pack (map (\c -> if c == '_' then '-' else c) (Text.unpack txt)))

copyFileIfExists :: FilePath -> FilePath -> IO ()
copyFileIfExists src dst = do
    exists <- doesFileExist src
    when exists $ copyFile src dst
copyDatabaseWithWAL :: FilePath -> FilePath -> IO ()
copyDatabaseWithWAL src dst = do
    createDirectoryIfMissing True (takeDirectory dst)
    copyFile src dst
    copyFileIfExists (src ++ "-wal") (dst ++ "-wal")
    copyFileIfExists (src ++ "-shm") (dst ++ "-shm")

-- Query classification
classifyQuery :: Text -> SqlOperation
classifyQuery query =
    let stripped = Text.dropWhile (`elem` [' ', '\t', '\n', '\r']) query
        firstWord = Text.toUpper $ Text.takeWhile (`elem` (['A' .. 'Z'] ++ ['a' .. 'z'])) stripped
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

isWriteOperation :: SqlOperation -> Bool
isWriteOperation Select = False
isWriteOperation _ = True

openDirectDb :: Tracer IO Trace -> FilePath -> IO Direct.Database
openDirectDb tracer path = do
    runTracer tracer (ConnectionOpenedTrace path)
    db <- Direct.open (Text.pack path)
    Direct.exec db "PRAGMA journal_mode = WAL"
    Direct.exec db "PRAGMA foreign_keys = ON"
    Direct.exec db "PRAGMA busy_timeout = 5000"
    pure db

-- Initialization
initializeToolbox :: Tracer IO Trace -> SqliteToolboxDescription -> IO (Either String Toolbox)
initializeToolbox tracer desc = do
    lock    <- newMVar ()
    counter <- newTVarIO 0
    heads   <- newTVarIO Map.empty
    mDbResult <- case sqliteToolboxVersioning desc of
        SqliteReadOnly path -> do
            r <- try @SomeException $ openDirectDb tracer path
            pure $ case r of
                Left e   -> Left $ "Failed to open read-only database: " ++ show e
                Right db -> Right (Just db)
        SqliteReadWrite path -> do
            r <- try @SomeException $ openDirectDb tracer path
            pure $ case r of
                Left e   -> Left $ "Failed to open read-write database: " ++ show e
                Right db -> Right (Just db)
        SqliteVersioned _ _ basePath -> do
            exists <- doesFileExist basePath
            unless exists $ do
                db <- Direct.open (Text.pack basePath)
                Direct.close db
            pure $ Right Nothing
    case mDbResult of
        Left err -> pure $ Left err
        Right mDb ->
            pure $
                Right $
                    Toolbox
                        { toolboxName          = sqliteToolboxName desc
                        , toolboxDescription   = sqliteToolboxDescription desc
                        , toolboxConfig        = desc
                        , toolboxLock          = lock
                        , toolboxWriteCounter  = counter
                        , toolboxDirectDb      = mDb
                        , toolboxVersionedHeads = heads
                        }

-- Query execution
executeQuery :: Tracer IO Trace -> Toolbox -> ToolExecutionContext -> SqliteQueryInput -> IO (Either QueryError QueryResult)
executeQuery tracer toolbox ctx input =
    withMVar (toolboxLock toolbox) $ \() -> do
        case sqliteToolboxVersioning (toolboxConfig toolbox) of
            SqliteReadOnly  _ -> executeDirectQuery tracer toolbox False input
            SqliteReadWrite _ -> executeDirectQuery tracer toolbox True  input
            SqliteVersioned sharing _ basePath -> executeVersionedQuery tracer toolbox ctx sharing basePath input

executeDirectQuery :: Tracer IO Trace -> Toolbox -> Bool -> SqliteQueryInput -> IO (Either QueryError QueryResult)
executeDirectQuery tracer toolbox allowWrites input = do
    let op = classifyQuery (inputSql input)
    if not allowWrites && isWriteOperation op
        then pure $ Left $ SqlError "Write operation not allowed in read-only mode"
        else case toolboxDirectDb toolbox of
            Nothing -> pure $ Left $ ConnectionError "No persistent connection for direct-access toolbox"
            Just db -> do
                runTracer tracer (QueryStartedTrace (inputSql input))
                startTime <- getCurrentTime
                result <- try @SomeException $ executeWithDb db (inputSql input) startTime
                pure $ case result of
                    Left e  -> Left $ SqlError $ Text.pack $ show e
                    Right r -> Right r

executeVersionedQuery :: Tracer IO Trace -> Toolbox -> ToolExecutionContext -> SqliteSharing -> FilePath -> SqliteQueryInput -> IO (Either QueryError QueryResult)
executeVersionedQuery tracer toolbox ctx sharing basePath input = do
    runTracer tracer (QueryStartedTrace (inputSql input))
    startTime <- getCurrentTime

    let config  = sqliteToolboxVersioning (toolboxConfig toolbox)
    let headKey = sharingToHeadKey sharing (ctxSessionId ctx) (ctxTurnId ctx)

    -- Resolve source: restore_from > current head > basePath
    sourcePath <- case inputRestoreFrom input of
        Just handle -> do
            result <- resolveVersionPath config (toolboxName toolbox) handle
            pure $ case result of
                Right path -> path
                Left _     -> basePath
        Nothing -> case headKey of
            Nothing  -> pure basePath
            Just key -> Map.findWithDefault basePath key <$> readTVarIO (toolboxVersionedHeads toolbox)

    let operation = classifyQuery (inputSql input)

    if isWriteOperation operation
        then do
            -- Allocate a unique snapshot index
            callIdx <- atomically $ do
                idx <- readTVar (toolboxWriteCounter toolbox)
                modifyTVar' (toolboxWriteCounter toolbox) (+ 1)
                pure idx

            root <- getStorageRoot config
            let snapshotPath = buildSnapshotPath root (ctxSessionId ctx) (ctxTurnId ctx) callIdx (toolboxName toolbox)

            -- Copy source → snapshot, run write on snapshot
            copyDatabaseWithWAL sourcePath snapshotPath
            result <- executeOnPath tracer snapshotPath (inputSql input) startTime

            -- Advance head for this sharing scope
            case headKey of
                Nothing  -> pure ()
                Just key -> atomically $ modifyTVar' (toolboxVersionedHeads toolbox) (Map.insert key snapshotPath)

            -- Build version handle so the LLM can reference this snapshot
            timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime
            let handle = SqliteVersionHandle
                    { vhSessionId      = Text.pack $ sessionIdToPath (ctxSessionId ctx)
                    , vhTurnId         = Text.pack $ turnIdToPath (ctxTurnId ctx)
                    , vhToolCallIndex  = callIdx
                    , vhToolboxName    = toolboxName toolbox
                    , vhTimestamp      = Text.pack timestamp
                    }

            pure $ case result of
                Left err -> Left err
                Right r  -> Right $ r{resultVersionHandle = Just handle}
        else do
            -- Read: run on current source, no snapshot, no head update
            executeOnPath tracer sourcePath (inputSql input) startTime

executeWithDb :: Direct.Database -> Text -> UTCTime -> IO QueryResult
executeWithDb db query startTime = do
    stmt <- Direct.prepare db query
    colCount <- Direct.columnCount stmt
    columnNames <-
        mapM
            ( \i -> do
                mName <- Direct.columnName stmt i
                pure $ maybe (Text.pack $ "column_" ++ show i) id mName
            )
            [0 .. fromIntegral colCount - 1]
    rows <- collectRows stmt colCount
    Direct.finalize stmt
    endTime <- getCurrentTime
    let execTime = diffUTCTime endTime startTime
    pure $
        QueryResult
            { resultColumns = columnNames
            , resultRows = rows
            , resultRowCount = length rows
            , resultExecutionTime = execTime
            , resultVersionHandle = Nothing
            }
  where
    collectRows stmt colCount = do
        stepResult <- Direct.step stmt
        case stepResult of
            Direct.Row -> do
                row <- mapM (\i -> sqlColumnToJson stmt (fromIntegral i)) [0 .. colCount - 1]
                rest <- collectRows stmt colCount
                pure (row : rest)
            Direct.Done -> pure []

executeOnPath :: Tracer IO Trace -> FilePath -> Text -> UTCTime -> IO (Either QueryError QueryResult)
executeOnPath tracer path query startTime = do
    result <- try @SomeException $ do
        db <- openDirectDb tracer path
        r <- executeWithDb db query startTime
        Direct.close db
        pure r
    pure $ case result of
        Left e  -> Left $ SqlError $ Text.pack $ show e
        Right r -> Right r

sqlColumnToJson :: Direct.Statement -> Direct.ColumnIndex -> IO Aeson.Value
sqlColumnToJson stmt colIdx = do
    colType <- Direct.columnType stmt colIdx
    case colType of
        Direct.IntegerColumn -> do
            val <- Direct.columnInt64 stmt colIdx
            pure $ Aeson.Number (fromIntegral val)
        Direct.FloatColumn -> do
            val <- Direct.columnDouble stmt colIdx
            pure $ Aeson.Number (fromRational (toRational val))
        Direct.TextColumn -> do
            val <- Direct.columnText stmt colIdx
            pure $ Aeson.String val
        Direct.BlobColumn -> do
            val <- Direct.columnBlob stmt colIdx
            pure $ Aeson.String (TextEnc.decodeUtf8 val)
        Direct.NullColumn -> pure Aeson.Null

-- | Promote a snapshot (or the current head) to basePath, making it the new baseline.
promoteSnapshot :: Toolbox -> ToolExecutionContext -> PromoteInput -> IO (Either QueryError PromoteResult)
promoteSnapshot toolbox ctx input =
    withMVar (toolboxLock toolbox) $ \() ->
        case sqliteToolboxVersioning (toolboxConfig toolbox) of
            SqliteReadOnly  _ -> pure $ Left $ SqlError "Cannot promote a read-only toolbox"
            SqliteReadWrite _ -> pure $ Left $ SqlError "Cannot promote a read-write toolbox (writes go directly to the file)"
            SqliteVersioned sharing _ basePath -> do
                let config  = sqliteToolboxVersioning (toolboxConfig toolbox)
                let headKey = sharingToHeadKey sharing (ctxSessionId ctx) (ctxTurnId ctx)
                sourcePath <- case promoteRestoreFrom input of
                    Just handle -> do
                        result <- resolveVersionPath config (toolboxName toolbox) handle
                        pure $ case result of
                            Right path -> path
                            Left _     -> basePath
                    Nothing -> case headKey of
                        Nothing  -> pure basePath
                        Just key -> Map.findWithDefault basePath key <$> readTVarIO (toolboxVersionedHeads toolbox)
                result <- try @SomeException $ copyDatabaseWithWAL sourcePath basePath
                pure $ case result of
                    Left e  -> Left $ SqlError $ Text.pack $ show e
                    Right _ -> Right $ PromoteResult { promotedFrom = sourcePath, promotedTo = basePath }

makeToolDescription :: Toolbox -> ToolDescription
makeToolDescription toolbox =
    ToolDescription
        { toolDescriptionName = toolboxName toolbox <> "_query"
        , toolDescriptionDescription = "Execute SQL on " <> toolboxDescription toolbox
        , toolDescriptionToolboxName = toolboxName toolbox
        }

buildToolSchema :: Toolbox -> Aeson.Value
buildToolSchema toolbox =
    Aeson.object
        [ "name" .= ("sqlite_" <> toolboxName toolbox <> "_query")
        , "description" .= (toolboxDescription toolbox <> " Supports restore_from for versioned rollbacks.")
        , "parameters"
            .= Aeson.object
                [ "type" .= ("object" :: Text)
                , "properties"
                    .= Aeson.object
                        [ "sql" .= Aeson.object ["type" .= ("string" :: Text), "description" .= ("SQL query to execute" :: Text)]
                        , "restore_from" .= Aeson.object ["type" .= ("object" :: Text), "description" .= ("Version handle for restoration" :: Text)]
                        ]
                , "required" .= (["sql"] :: [Text])
                ]
        ]
