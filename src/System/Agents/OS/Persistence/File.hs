{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- |
File-based persistence backend for the OS persistence layer.

This module provides a file-based implementation of the Persistence
typeclass for compatibility with existing session storage.

== Storage Layout

@
<basePath>/
  entities/
    <entity-id>.json
  events/
    <timestamp>-<uuid>.json
  conversations/
    <conversation-id>.json
@

Each entity is stored as a separate JSON file containing all its
components. This provides human-readable storage suitable for
debugging and version control.
-}
module System.Agents.OS.Persistence.File (
    -- * File Backend
    FilePersistenceBackend,
    newFileBackend,
    closeFileBackend,

    -- * Low-level operations
    readEntityFile,
    writeEntityFile,
    deleteEntityFile,
    listEntityFiles,

    -- * Migration support
    migrateFromSessionStore,
    SessionStoreInfo (..),
    
    -- * Event operations
    writeEventFile,
    readEventFiles,
) where

import Control.Exception (catch)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    listDirectory,
    removeFile,
 )
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO.Error (IOError)

import System.Agents.OS.Core.Types (ComponentTypeId (..), EntityId (..))
import System.Agents.OS.Persistence.Types (
    ComponentType,
    FileFormat (..),
    FilePersistence (..),
    Persistable (..),
    PersistenceEvent (..),
    entityIdToText,
    textToEntityId,
 )

-------------------------------------------------------------------------------
-- File Backend State
-------------------------------------------------------------------------------

-- | State for the file-based persistence backend.
data FilePersistenceBackend = FilePersistenceBackend
    { fpbConfig :: FilePersistence
    , fpbEventLog :: [PersistenceEvent]
    }
    deriving (Show)

-- | Create a new file-based persistence backend.
newFileBackend :: FilePersistence -> IO FilePersistenceBackend
newFileBackend config = do
    -- Ensure base directory exists
    createDirectoryIfMissing True config.fpBasePath
    createDirectoryIfMissing True (getEntitiesPath config)
    createDirectoryIfMissing True (getEventsPath config)
    pure $ FilePersistenceBackend config []

-- | Close the file backend (no-op for file backend).
closeFileBackend :: FilePersistenceBackend -> IO ()
closeFileBackend _ = pure ()

-------------------------------------------------------------------------------
-- Storage Paths
-------------------------------------------------------------------------------

-- | Get the path for entity files.
getEntitiesPath :: FilePersistence -> FilePath
getEntitiesPath config = config.fpBasePath </> "entities"

-- | Get the path for event files.
getEventsPath :: FilePersistence -> FilePath
getEventsPath config = config.fpBasePath </> "events"

-- | Get the file extension based on format.
getExtension :: FilePersistence -> String
getExtension config = case config.fpFormat of
    JsonFormat -> ".json"
    BinaryFormat -> ".bin"

-- | Get the full path for an entity file.
entityFilePath :: FilePersistence -> EntityId -> FilePath
entityFilePath config eid =
    getEntitiesPath config </> Text.unpack (entityIdToText eid) ++ getExtension config

-- | Get the full path for an event file.
eventFilePath :: FilePersistence -> UTCTime -> FilePath
eventFilePath config timestamp =
    getEventsPath config </> show timestamp ++ getExtension config

-------------------------------------------------------------------------------
-- Entity Storage
-------------------------------------------------------------------------------

-- | Data stored in an entity file.
data EntityFileData = EntityFileData
    { efdEntityId :: Text
    , efdEntityType :: Text
    , efdComponents :: HashMap Text Aeson.Value
    , efdCreatedAt :: Maybe UTCTime
    , efdUpdatedAt :: Maybe UTCTime
    }
    deriving (Show)

instance Aeson.FromJSON EntityFileData where
    parseJSON = Aeson.withObject "EntityFileData" $ \obj ->
        EntityFileData
            <$> obj Aeson..: "entityId"
            <*> obj Aeson..: "entityType"
            <*> obj Aeson..:? "components" Aeson..!= HashMap.empty
            <*> obj Aeson..:? "createdAt"
            <*> obj Aeson..:? "updatedAt"

instance Aeson.ToJSON EntityFileData where
    toJSON efd =
        Aeson.object
            [ "entityId" Aeson..= efd.efdEntityId
            , "entityType" Aeson..= efd.efdEntityType
            , "components" Aeson..= efd.efdComponents
            , "createdAt" Aeson..= efd.efdCreatedAt
            , "updatedAt" Aeson..= efd.efdUpdatedAt
            ]

-- | Read an entity file.
readEntityFile :: FilePersistence -> EntityId -> IO (Maybe EntityFileData)
readEntityFile config eid = do
    let path = entityFilePath config eid
    exists <- doesFileExist path
    if exists
        then
            catch
                ( do
                    content <- LBS.readFile path
                    case config.fpFormat of
                        JsonFormat -> pure $ Aeson.decode content
                        BinaryFormat -> pure Nothing -- Not implemented
                )
                (\(_ :: IOError) -> pure Nothing)
        else pure Nothing

-- | Write an entity file.
writeEntityFile ::
    forall a. (Persistable a) =>
    FilePersistence ->
    EntityId ->
    a ->
    IO ()
writeEntityFile config eid component = do
    now <- getCurrentTime
    let existingData =
            EntityFileData
                { efdEntityId = entityIdToText eid
                , efdEntityType = persistenceTable (Proxy :: Proxy a)
                , efdComponents = HashMap.empty
                , efdCreatedAt = Just now
                , efdUpdatedAt = Just now
                }
    -- Read existing or create new
    mExisting <- readEntityFile config eid
    let baseData = fromMaybe existingData mExisting
    -- Serialize component
    let componentJson = Aeson.toJSON component
    let compType = persistenceKey (Proxy :: Proxy a) eid
    let newComponents = HashMap.insert compType componentJson baseData.efdComponents
    let newData =
            baseData
                { efdComponents = newComponents
                , efdUpdatedAt = Just now
                }
    let path = entityFilePath config eid
    createDirectoryIfMissing True (takeDirectory path)
    case config.fpFormat of
        JsonFormat ->
            LBS.writeFile path (Aeson.encode newData <> "\n")
        BinaryFormat ->
            LBS.writeFile path (Aeson.encode newData) -- Use JSON for now

-- | Delete an entity file.
deleteEntityFile :: FilePersistence -> EntityId -> IO ()
deleteEntityFile config eid = do
    let path = entityFilePath config eid
    exists <- doesFileExist path
    when exists $ removeFile path

-- | List all entity files.
listEntityFiles :: FilePersistence -> IO [(EntityId, UTCTime)]
listEntityFiles config = do
    let entitiesPath = getEntitiesPath config
    exists <- doesDirectoryExist entitiesPath
    if not exists
        then pure []
        else do
            files <- listDirectory entitiesPath
            let validFiles = filter (\f -> takeExtension f == getExtension config) files
            mapM parseEntityFile validFiles
  where
    parseEntityFile fileName = do
        let path = getEntitiesPath config </> fileName
        mtime <- getModificationTime path
        let base = Text.pack $ takeBaseName fileName
        case textToEntityId base of
            Just eid -> pure (eid, mtime)
            Nothing -> error $ "Invalid entity ID in filename: " ++ fileName

-------------------------------------------------------------------------------
-- Event Storage
-------------------------------------------------------------------------------

-- | Data stored in an event file.
data EventFileData = EventFileData
    { evdTimestamp :: UTCTime
    , evdEventType :: Text
    , evdEventData :: Aeson.Value
    , evdEntityId :: Maybe Text
    }
    deriving (Show)

instance Aeson.FromJSON EventFileData where
    parseJSON = Aeson.withObject "EventFileData" $ \obj ->
        EventFileData
            <$> obj Aeson..: "timestamp"
            <*> obj Aeson..: "eventType"
            <*> obj Aeson..: "eventData"
            <*> obj Aeson..:? "entityId"

instance Aeson.ToJSON EventFileData where
    toJSON evd =
        Aeson.object
            [ "timestamp" Aeson..= evd.evdTimestamp
            , "eventType" Aeson..= evd.evdEventType
            , "eventData" Aeson..= evd.evdEventData
            , "entityId" Aeson..= evd.evdEntityId
            ]

-- | Write an event to a file.
writeEventFile ::
    FilePersistence ->
    UTCTime ->
    Text ->
    Aeson.Value ->
    Maybe EntityId ->
    IO ()
writeEventFile config timestamp eventType eventData mEid = do
    let eventData' =
            EventFileData
                { evdTimestamp = timestamp
                , evdEventType = eventType
                , evdEventData = eventData
                , evdEntityId = fmap entityIdToText mEid
                }
    let path = eventFilePath config timestamp
    createDirectoryIfMissing True (takeDirectory path)
    case config.fpFormat of
        JsonFormat ->
            LBS.writeFile path (Aeson.encode eventData' <> "\n")
        BinaryFormat ->
            LBS.writeFile path (Aeson.encode eventData')

-- | Read all event files.
readEventFiles :: FilePersistence -> IO [(UTCTime, Text, Aeson.Value)]
readEventFiles config = do
    let eventsPath = getEventsPath config
    exists <- doesDirectoryExist eventsPath
    if not exists
        then pure []
        else do
            files <- listDirectory eventsPath
            events <- mapM readEventFile files
            pure $ concat events
  where
    readEventFile fileName = do
        let path = getEventsPath config </> fileName
        catch
            ( do
                content <- LBS.readFile path
                case Aeson.decode content of
                    Just evd -> pure [(evdTimestamp evd, evdEventType evd, evdEventData evd)]
                    Nothing -> pure []
            )
            (\(_ :: IOError) -> pure [])

-------------------------------------------------------------------------------
-- Migration Support
-------------------------------------------------------------------------------

-- | Information about a session store file to migrate.
data SessionStoreInfo = SessionStoreInfo
    { ssiSourcePath :: FilePath
    , ssiConversationId :: Text
    , ssiModifiedAt :: UTCTime
    }
    deriving (Show)

-- | Migrate from existing session store to file-based persistence.
migrateFromSessionStore ::
    FilePersistence ->
    [SessionStoreInfo] ->
    IO [Either Text EntityId]
migrateFromSessionStore _config sessions = do
    mapM migrateSession sessions
  where
    migrateSession session = do
        content <- LBS.readFile (ssiSourcePath session)
        case parseSessionContent content of
            Nothing -> pure $ Left $ "Failed to parse: " <> Text.pack (ssiSourcePath session)
            Just _sessionData -> do
                -- Write to new format - would need actual conversion logic
                pure $ Left "Migration not fully implemented"

    parseSessionContent :: LBS.ByteString -> Maybe LBS.ByteString
    parseSessionContent = Just -- Placeholder

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Entity type from component type.
entityTypeFromComponentType :: ComponentType -> Text
entityTypeFromComponentType ct = case ct of
    _ -> "unknown"

