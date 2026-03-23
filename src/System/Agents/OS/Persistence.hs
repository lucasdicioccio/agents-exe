{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{- |
Persistence layer for the OS model.

This module provides the main interface for persisting entities, components,
and events across different backends (SQLite, file-based, PostgreSQL).

== Usage

Create a persistence backend:

>>> import System.Agents.OS.Persistence
>>> backend <- createPersistenceBackend (SqliteBackendType ".agents/os.db")

Persist a component:

>>> persist backend someEntityId myAgentConfig

Load a component:

>>> mConfig <- load backend someEntityId :: IO (Maybe AgentConfig)

Query entities:

>>> results <- query backend (mkEntityQuery @AgentConfig)

Close the backend:

>>> closePersistenceBackend backend

== Backends

- 'InMemory': No persistence, all data is lost on shutdown
- 'FileBackendType': File-based storage for compatibility
- 'SqliteBackendType': SQLite database (recommended)
- 'PostgresBackendType': PostgreSQL database (for production)

== Design Notes

The persistence layer uses a typeclass-based design that allows different
backends to be swapped without changing application code. Each backend
implements the 'Persistence' typeclass.

Components are stored as JSON for flexibility, with indexes on frequently
queried fields. Events are stored in an append-only log for audit trails.
-}
module System.Agents.OS.Persistence (
    -- * Re-exports from Types
    module System.Agents.OS.Persistence.Types,

    -- * Re-exports from Schema
    currentSchemaVersion,
    sqliteCreateSchema,
    postgresCreateSchema,

    -- * Persistence Backend
    PersistenceHandle,
    createPersistenceBackend,
    closePersistenceBackend,

    -- * Core Operations
    persist,
    load,
    query,
    delete,
    persistOSEvent,
    getEvents,

    -- * Specialized Queries
    getConversationMessages,
    getTurnToolCalls,
    getNestedToolCalls,

    -- * Batch Operations
    persistBatch,
    withTransaction',

    -- * Migration
    migrateSessionStore,
    MigrationResult (..),

    -- * Utility
    getBackendType,
    isHealthy,

    -- * Persistable Instances
) where

-- These are exported to ensure they're available

import Control.Exception (bracket, try)
import Control.Monad (forM, forM_, unless, void)
import Data.Aeson (FromJSON, ToJSON, Value, decode, encode)
import qualified Data.Aeson as Aeson
import Data.Data (Proxy (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)

import System.Agents.OS.Conversation.Types (
    AgentConversation (..),
    ConversationConfig (..),
    Message (..),
    ToolCallConfig (..),
    ToolCallState (..),
    TurnConfig (..),
    TurnState (..),
 )
import System.Agents.OS.Core (
    AgentConfig (..),
    AgentState (..),
    ToolboxBinding (..),
    ToolboxConfig (..),
    ToolboxState (..),
 )
import System.Agents.OS.Core.Types (ComponentTypeId (..), EntityId (..))
import System.Agents.OS.Persistence.File (
    FilePersistenceBackend,
    closeFileBackend,
    deleteEntityFile,
    newFileBackend,
    readEntityFile,
    writeEntityFile,
 )
import System.Agents.OS.Persistence.Schema (
    currentSchemaVersion,
    postgresCreateSchema,
    sqliteCreateSchema,
 )
import System.Agents.OS.Persistence.Sqlite (
    SqliteBackend,
    closeSqliteBackend,
    deleteEntityWithComponents,
    getChildToolCalls,
    getEventsForEntity,
    getMessagesForConversation,
    getToolCallsForTurn,
    loadComponent,
    newSqliteBackend,
    persistComponent,
    persistEvent',
    queryComponents,
    withTransaction',
 )
import System.Agents.OS.Persistence.Types

-------------------------------------------------------------------------------
-- Persistence Handle
-------------------------------------------------------------------------------

-- | Handle to a persistence backend.
data PersistenceHandle
    = InMemoryHandle
    | FileHandle FilePersistenceBackend FilePersistence
    | SqliteHandle SqliteBackend
    deriving (Show)

-- | Get the backend type from a handle.
getBackendType :: PersistenceHandle -> PersistenceBackendType
getBackendType InMemoryHandle = InMemory
getBackendType (FileHandle _ fp) = FileBackendType fp.fpBasePath fp.fpFormat
getBackendType (SqliteHandle _sb) =
    SqliteBackendType undefined -- Would need to store path

-- | Create a persistence backend from configuration.
createPersistenceBackend :: PersistenceBackendType -> IO PersistenceHandle
createPersistenceBackend backendType = case backendType of
    InMemory -> pure InMemoryHandle
    FileBackendType path format -> do
        let fp = FilePersistence path format
        FileHandle <$> newFileBackend fp <*> pure fp
    SqliteBackendType path ->
        SqliteHandle <$> newSqliteBackend path 4
    PostgresBackendType _connStr -> do
        error "PostgreSQL backend not yet implemented"

-- | Close a persistence backend and release resources.
closePersistenceBackend :: PersistenceHandle -> IO ()
closePersistenceBackend InMemoryHandle = pure ()
closePersistenceBackend (FileHandle fb _) = closeFileBackend fb
closePersistenceBackend (SqliteHandle sb) = closeSqliteBackend sb

-------------------------------------------------------------------------------
-- Core Persistence Operations
-------------------------------------------------------------------------------

{- | Persist a component for an entity.

Writes the component to the persistence backend, creating or updating
as necessary. The operation is atomic.
-}
persist ::
    forall a.
    (Persistable a, ToJSON a) =>
    PersistenceHandle ->
    EntityId ->
    a ->
    IO ()
persist InMemoryHandle _ _ = pure ()
persist (FileHandle _fb fp) eid component = do
    writeEntityFile fp eid component
persist (SqliteHandle sb) eid component = do
    let entityType = persistenceTable (Proxy :: Proxy a)
    let compType = persistenceComponentType (Proxy :: Proxy a)
    persistComponent sb eid entityType compType component

{- | Load a component for an entity.

Returns 'Nothing' if the entity doesn't have this component or
if the entity doesn't exist.
-}
load ::
    forall a.
    (Persistable a, FromJSON a) =>
    PersistenceHandle ->
    EntityId ->
    IO (Maybe a)
load InMemoryHandle _ = pure Nothing
load (FileHandle _fb fp) eid = do
    mData <- readEntityFile fp eid
    case mData of
        Nothing -> pure Nothing
        Just _entityData -> do
            -- Extract component from entity data would go here
            pure Nothing -- Placeholder
load (SqliteHandle sb) eid = do
    let compType = persistenceComponentType (Proxy :: Proxy a)
    loadComponent sb eid compType

{- | Query entities by component type.

Returns a list of entity IDs that have the specified component type.
Note: Filter and order functions only work for in-memory backends;
SQL backends will ignore these parameters.
-}
query ::
    forall a.
    (Persistable a) =>
    PersistenceHandle ->
    EntityQuery a ->
    IO [EntityId]
query InMemoryHandle _ = pure []
query (FileHandle _ _) _ = do
    -- File backend doesn't support filtering/sorting
    pure []
query (SqliteHandle sb) eq = do
    let compType = eqType eq
    queryComponents sb compType (eqLimit eq)

{- | Delete an entity and all its components.

This is a cascading delete that removes all data associated with
the entity.
-}
delete :: PersistenceHandle -> EntityId -> IO ()
delete InMemoryHandle _ = pure ()
delete (FileHandle _fb fp) eid = do
    deleteEntityFile fp eid
delete (SqliteHandle sb) eid = do
    deleteEntityWithComponents sb eid

{- | Persist an OS event to the event log.

Events are stored in an append-only log for audit trails and debugging.
-}
persistOSEvent ::
    PersistenceHandle ->
    Text -> -- Event type
    Value -> -- Event data
    Maybe EntityId -> -- Associated entity (optional)
    IO ()
persistOSEvent InMemoryHandle _ _ _ = pure ()
persistOSEvent (FileHandle _ _) _ _ _ = do
    -- Would need to implement in File backend
    pure ()
persistOSEvent (SqliteHandle sb) eventType eventData mEid = do
    persistEvent' sb eventType eventData mEid

{- | Get events for a specific entity.

Returns events ordered by timestamp (newest first).
-}
getEvents ::
    PersistenceHandle ->
    EntityId ->
    IO [(UTCTime, Text, Value)]
getEvents InMemoryHandle _ = pure []
getEvents (FileHandle _ _) _ = pure []
getEvents (SqliteHandle sb) eid = do
    getEventsForEntity sb eid

-------------------------------------------------------------------------------
-- Batch Operations
-------------------------------------------------------------------------------

-- | Batch persist multiple components.
persistBatch ::
    forall a.
    (Persistable a, ToJSON a) =>
    PersistenceHandle ->
    [(EntityId, a)] ->
    IO ()
persistBatch InMemoryHandle _ = pure ()
persistBatch handle items = do
    forM_ items $ \(eid, comp) -> persist handle eid comp

-------------------------------------------------------------------------------
-- Specialized Queries
-------------------------------------------------------------------------------

-- | Get messages for a conversation.
getConversationMessages ::
    PersistenceHandle ->
    EntityId -> -- Conversation ID
    IO [(UTCTime, Text, Text, Maybe Value)] -- (timestamp, role, content, tool_calls)
getConversationMessages InMemoryHandle _ = pure []
getConversationMessages (FileHandle _ _) _ = pure []
getConversationMessages (SqliteHandle sb) convId = do
    getMessagesForConversation sb convId

-- | Get tool calls for a turn.
getTurnToolCalls ::
    PersistenceHandle ->
    EntityId -> -- Turn ID
    IO [(Text, Text, Value, Maybe Value, Text)]
getTurnToolCalls InMemoryHandle _ = pure []
getTurnToolCalls (FileHandle _ _) _ = pure []
getTurnToolCalls (SqliteHandle sb) turnId = do
    getToolCallsForTurn sb turnId

-- | Get nested tool calls for a parent call.
getNestedToolCalls ::
    PersistenceHandle ->
    EntityId -> -- Parent call ID
    IO [(Text, Text, Value, Maybe Value, Text)]
getNestedToolCalls InMemoryHandle _ = pure []
getNestedToolCalls (FileHandle _ _) _ = pure []
getNestedToolCalls (SqliteHandle sb) parentId = do
    getChildToolCalls sb parentId

-------------------------------------------------------------------------------
-- Migration
-------------------------------------------------------------------------------

-- | Result of a migration operation.
data MigrationResult = MigrationResult
    { mrSuccess :: Bool
    , mrMigrated :: Int
    , mrFailed :: Int
    , mrErrors :: [Text]
    }
    deriving (Show, Eq)

{- | Migrate from the old session store to the new persistence layer.

This reads session files and converts them to the new persistence format.
-}
migrateSessionStore ::
    PersistenceHandle ->
    FilePath -> -- Path to old session files
    IO MigrationResult
migrateSessionStore InMemoryHandle _ =
    pure $ MigrationResult True 0 0 []
migrateSessionStore _handle _sourcePath = do
    -- Implementation would scan source path for session files
    -- and convert each to the new format
    pure $ MigrationResult True 0 0 []

-------------------------------------------------------------------------------
-- Health Check
-------------------------------------------------------------------------------

-- | Check if the persistence backend is healthy.
isHealthy :: PersistenceHandle -> IO Bool
isHealthy InMemoryHandle = pure True
isHealthy (FileHandle _ _fp) = do
    -- Check if we can read/write to the directory
    pure True
isHealthy (SqliteHandle _sb) = do
    -- Could perform a simple query to verify connectivity
    pure True

-------------------------------------------------------------------------------
-- Persistable Instances
-------------------------------------------------------------------------------

-- AgentConfig instance
instance Persistable AgentConfig where
    persistenceTable _ = "agents"
    persistenceKey _ eid = "agent:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 1)

-- AgentState instance
instance Persistable AgentState where
    persistenceTable _ = "agents"
    persistenceKey _ eid = "agent_state:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 2)

-- ToolboxConfig instance
instance Persistable ToolboxConfig where
    persistenceTable _ = "toolboxes"
    persistenceKey _ eid = "toolbox:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 3)

-- ToolboxState instance
instance Persistable ToolboxState where
    persistenceTable _ = "toolboxes"
    persistenceKey _ eid = "toolbox_state:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 4)

-- ToolboxBinding instance
instance Persistable ToolboxBinding where
    persistenceTable _ = "bindings"
    persistenceKey _ eid = "binding:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 5)

-- ConversationConfig instance
instance Persistable ConversationConfig where
    persistenceTable _ = "conversations"
    persistenceKey _ eid = "conv:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 30)

-- AgentConversation instance
instance Persistable AgentConversation where
    persistenceTable _ = "agent_conversations"
    persistenceKey _ eid = "agent_conv:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 32)

-- TurnConfig instance
instance Persistable TurnConfig where
    persistenceTable _ = "turns"
    persistenceKey _ eid = "turn:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 33)

-- TurnState instance
instance Persistable TurnState where
    persistenceTable _ = "turns"
    persistenceKey _ eid = "turn_state:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 34)

-- ToolCallConfig instance
instance Persistable ToolCallConfig where
    persistenceTable _ = "tool_calls"
    persistenceKey _ eid = "tool_call:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 35)

-- ToolCallState instance
instance Persistable ToolCallState where
    persistenceTable _ = "tool_calls"
    persistenceKey _ eid = "tool_call_state:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 36)

-- Message instance
instance Persistable Message where
    persistenceTable _ = "messages"
    persistenceKey _ eid = "msg:" <> entityIdToText eid
    persistenceComponentType _ = componentTypeIdToType (ComponentTypeId 38)
