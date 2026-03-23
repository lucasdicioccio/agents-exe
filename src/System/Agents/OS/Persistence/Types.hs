{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Core types for the OS persistence layer.

This module defines the persistence interface and configuration types
for storing ECS entities, components, and events in various backends.
-}
module System.Agents.OS.Persistence.Types (
    -- * Persistence Typeclass
    Persistence (..),
    EntityQuery (..),
    mkEntityQuery,

    -- * Persistence Backend Types
    PersistenceBackend (..),
    FilePersistence (..),
    FileFormat (..),
    SqlitePersistence (..),
    PostgresPersistence (..),

    -- * Persistable Components
    Persistable (..),
    entityIdToText,
    textToEntityId,

    -- * Persistence Events
    PersistenceEvent (..),

    -- * Persistence Configuration
    PersistenceConfig (..),
    PersistenceBackendType (..),
    defaultPersistenceConfig,

    -- * Component Type Mapping
    ComponentType (..),
    componentTypeIdToType,
    componentTypeToTypeId,
) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Data (Proxy (..))
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime)
import qualified Data.UUID as UUID
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)

import System.Agents.OS.Core.Types (
    Component (..),
    ComponentTypeId (..),
    EntityId (..),
 )

-------------------------------------------------------------------------------
-- Component Type Mapping
-------------------------------------------------------------------------------

-- | Enumeration of known component types for database storage.
-- This provides a stable mapping between ComponentTypeId and database values.
data ComponentType
    = ComponentTypeAgentConfig
    | ComponentTypeAgentState
    | ComponentTypeToolboxConfig
    | ComponentTypeToolboxState
    | ComponentTypeToolboxBinding
    | ComponentTypeConversationConfig
    | ComponentTypeConversationState
    | ComponentTypeAgentConversation
    | ComponentTypeTurnConfig
    | ComponentTypeTurnState
    | ComponentTypeToolCallConfig
    | ComponentTypeToolCallState
    | ComponentTypeMessage
    | ComponentTypeUnknown Int
    deriving (Show, Eq, Generic)

instance FromJSON ComponentType
instance ToJSON ComponentType

-- | Convert ComponentTypeId to ComponentType for storage.
componentTypeIdToType :: ComponentTypeId -> ComponentType
componentTypeIdToType (ComponentTypeId n) = case n of
    1 -> ComponentTypeAgentConfig
    2 -> ComponentTypeAgentState
    3 -> ComponentTypeToolboxConfig
    4 -> ComponentTypeToolboxState
    5 -> ComponentTypeToolboxBinding
    30 -> ComponentTypeConversationConfig
    31 -> ComponentTypeConversationState
    32 -> ComponentTypeAgentConversation
    33 -> ComponentTypeTurnConfig
    34 -> ComponentTypeTurnState
    35 -> ComponentTypeToolCallConfig
    36 -> ComponentTypeToolCallState
    38 -> ComponentTypeMessage
    _ -> ComponentTypeUnknown n

-- | Convert ComponentType back to ComponentTypeId.
componentTypeToTypeId :: ComponentType -> ComponentTypeId
componentTypeToTypeId ct = ComponentTypeId $ case ct of
    ComponentTypeAgentConfig -> 1
    ComponentTypeAgentState -> 2
    ComponentTypeToolboxConfig -> 3
    ComponentTypeToolboxState -> 4
    ComponentTypeToolboxBinding -> 5
    ComponentTypeConversationConfig -> 30
    ComponentTypeConversationState -> 31
    ComponentTypeAgentConversation -> 32
    ComponentTypeTurnConfig -> 33
    ComponentTypeTurnState -> 34
    ComponentTypeToolCallConfig -> 35
    ComponentTypeToolCallState -> 36
    ComponentTypeMessage -> 38
    ComponentTypeUnknown n -> n

-------------------------------------------------------------------------------
-- Persistence Typeclass
-------------------------------------------------------------------------------

{- | Typeclass for persistence operations.

This defines the interface for storing and retrieving entities,
components, and events. Implementations can use different backends
(SQLite, PostgreSQL, files, etc.).
-}
class (Monad m) => Persistence m where
    -- | Persist a component attached to an entity.
    persistEntity ::
        (Persistable a) =>
        EntityId ->
        a ->
        m ()

    -- | Load a component for an entity.
    loadEntity ::
        (Persistable a) =>
        EntityId ->
        m (Maybe a)

    -- | Query entities based on a filter.
    queryEntities ::
        (Persistable a) =>
        EntityQuery a ->
        m [EntityId]

    -- | Delete an entity and all its components.
    deleteEntity ::
        EntityId ->
        m ()

    -- | Persist an OS event to the event log.
    persistEvent ::
        Text -> -- Event type
        Value -> -- Event data
        Maybe EntityId -> -- Associated entity (optional)
        m ()

    -- | Get events for a specific entity.
    getEntityEvents ::
        EntityId ->
        m [(UTCTime, Text, Value)]

-- | Query specification for filtering entities.
data EntityQuery a = EntityQuery
    { eqFilter :: Maybe (a -> Bool)
    -- ^ Optional filter function (not available for SQL backends)
    , eqLimit :: Maybe Int
    -- ^ Maximum number of results
    , eqOrder :: Maybe (a -> a -> Ordering)
    -- ^ Optional ordering (not available for SQL backends)
    , eqType :: ComponentType
    -- ^ Component type to query
    }

-- | Create a basic entity query for a component type.
mkEntityQuery :: forall a. (Persistable a) => EntityQuery a
mkEntityQuery =
    EntityQuery
        { eqFilter = Nothing
        , eqLimit = Nothing
        , eqOrder = Nothing
        , eqType = componentTypeIdToType (componentId (Proxy :: Proxy a))
        }

-------------------------------------------------------------------------------
-- Persistence Backend Types
-------------------------------------------------------------------------------

-- | Supported persistence backends.
data PersistenceBackend
    = FileBackend FilePersistence
    | SqliteBackend SqlitePersistence
    | PostgresBackend PostgresPersistence
    deriving (Show)

-- | File-based persistence configuration.
data FilePersistence = FilePersistence
    { fpBasePath :: FilePath
    -- ^ Base directory for storing files
    , fpFormat :: FileFormat
    -- ^ Serialization format
    }
    deriving (Show, Eq)

-- | Supported file formats.
data FileFormat
    = JsonFormat
    -- ^ Human-readable JSON
    | BinaryFormat
    -- ^ Compact binary format
    deriving (Show, Eq, Generic)

instance FromJSON FileFormat
instance ToJSON FileFormat

-- | SQLite persistence configuration.
data SqlitePersistence = SqlitePersistence
    { spConnectionPool :: Pool Connection
    -- ^ Connection pool for database access
    , spSchemaVersion :: Int
    -- ^ Current schema version
    }

instance Show SqlitePersistence where
    show sp =
        "SqlitePersistence { spConnectionPool = <Pool>, spSchemaVersion = "
            ++ show sp.spSchemaVersion
            ++ " }"

-- | PostgreSQL persistence configuration.
data PostgresPersistence = PostgresPersistence
    { pgConnectionPool :: Pool Connection
    -- ^ Connection pool for database access
    , pgSchemaVersion :: Int
    -- ^ Current schema version
    }

instance Show PostgresPersistence where
    show pg =
        "PostgresPersistence { pgConnectionPool = <Pool>, pgSchemaVersion = "
            ++ show pg.pgSchemaVersion
            ++ " }"

-------------------------------------------------------------------------------
-- Persistable Components
-------------------------------------------------------------------------------

{- | Marker class for components that can be persisted.

This class combines Component with JSON serialization and provides
table/key information for persistence backends.
-}
class (Component a, FromJSON a, ToJSON a) => Persistable a where
    -- | Get the table name for this component type.
    persistenceTable :: Proxy a -> Text

    -- | Get the storage key for an entity's component.
    persistenceKey :: Proxy a -> EntityId -> Text

    -- | Get the component type for queries.
    persistenceComponentType :: Proxy a -> ComponentType

-- | Convert an EntityId to Text representation.
entityIdToText :: EntityId -> Text
entityIdToText (EntityId uuid) = Text.pack $ UUID.toString uuid

-- | Convert Text to EntityId (may fail).
textToEntityId :: Text -> Maybe EntityId
textToEntityId txt = EntityId <$> UUID.fromString (Text.unpack txt)

-------------------------------------------------------------------------------
-- Persistence Events
-------------------------------------------------------------------------------

-- | Events emitted by the persistence layer.
data PersistenceEvent
    = EntityPersisted EntityId ComponentType
    | -- ^ An entity component was persisted
      EntityLoaded EntityId (Maybe ComponentType)
    | -- ^ An entity component was loaded
      EntityDeleted EntityId
    | -- ^ An entity was deleted
      BatchPersistCompleted Int
    | -- ^ A batch persist operation completed (count)
      PersistenceError Text
    | -- ^ A persistence error occurred
      MigrationStarted Int Int
    | -- ^ Migration started (from, to)
      MigrationCompleted Int
    | -- ^ Migration completed (new version)
      MigrationFailed Text
    deriving (Show, Eq, Generic)

instance FromJSON PersistenceEvent
instance ToJSON PersistenceEvent

-------------------------------------------------------------------------------
-- Persistence Configuration
-------------------------------------------------------------------------------

-- | Configuration for the persistence layer.
data PersistenceConfig = PersistenceConfig
    { persistenceBackend :: PersistenceBackendType
    -- ^ Backend type and connection info
    , persistenceSyncInterval :: NominalDiffTime
    -- ^ How often to sync to disk (for async modes)
    , persistenceBatchSize :: Int
    -- ^ Number of operations to batch before writing
    , persistenceEnableEvents :: Bool
    -- ^ Whether to persist events to the event log
    }
    deriving (Show)

-- | Backend type selection.
data PersistenceBackendType
    = InMemory
    | -- ^ No persistence (volatile)
      FileBackendType FilePath FileFormat
    | -- ^ File-based storage
      SqliteBackendType FilePath
    | -- ^ SQLite database
      PostgresBackendType Text
    -- ^ PostgreSQL connection string
    deriving (Show, Eq, Generic)

instance FromJSON PersistenceBackendType
instance ToJSON PersistenceBackendType

-- | Default persistence configuration (SQLite).
defaultPersistenceConfig :: PersistenceConfig
defaultPersistenceConfig =
    PersistenceConfig
        { persistenceBackend = SqliteBackendType ".agents/os.db"
        , persistenceSyncInterval = 5 -- 5 seconds
        , persistenceBatchSize = 100
        , persistenceEnableEvents = True
        }

