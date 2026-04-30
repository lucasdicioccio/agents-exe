{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Database schema definitions for the OS persistence layer.

This module provides SQL schema definitions for SQLite and PostgreSQL
backends, along with migration support.

Schema Version History:
* Version 1 - Initial schema with entities, components, events, messages, tool_calls
* Version 2 - Added tool_call_cache and tool_continuations tables for async execution
-}
module System.Agents.OS.Persistence.Schema (
    -- * Schema Version
    currentSchemaVersion,

    -- * SQLite Schema
    sqliteCreateSchema,
    sqliteDropSchema,
    sqliteMigrationScript,
    executeSqliteSchema,

    -- * PostgreSQL Schema
    postgresCreateSchema,
    postgresDropSchema,
    postgresMigrationScript,

    -- * Async Execution Schema
    sqliteCreateAsyncSchema,
    postgresCreateAsyncSchema,

    -- * Migration
    Migration (..),
    migrateSchema,
) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, execute_, query, query_, withTransaction)
import Database.SQLite.Simple.QQ (sql)

-------------------------------------------------------------------------------
-- Schema Version
-------------------------------------------------------------------------------

{- | Current schema version number.
Increment this when schema changes require migrations.

Version history:
* 1 - Initial schema (entities, components, events, messages, tool_calls)
* 2 - Added async execution support (tool_call_cache, tool_continuations)
-}
currentSchemaVersion :: Int
currentSchemaVersion = 2

-------------------------------------------------------------------------------
-- SQLite Schema - Version 1 (Base)
-------------------------------------------------------------------------------

-- | Individual SQLite schema statements for proper execution.
sqliteSchemaV1Statements :: [Query]
sqliteSchemaV1Statements =
    [ [sql| CREATE TABLE IF NOT EXISTS schema_version (
            version INTEGER PRIMARY KEY,
            applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        ) |]
    , [sql| INSERT INTO schema_version (version) VALUES (1) 
            ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP |]
    , [sql| CREATE TABLE IF NOT EXISTS entities (
            entity_id TEXT PRIMARY KEY,
            entity_type TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        ) |]
    , [sql| CREATE TABLE IF NOT EXISTS components (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            entity_id TEXT NOT NULL REFERENCES entities(entity_id) ON DELETE CASCADE,
            component_type INTEGER NOT NULL,
            component_data TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            UNIQUE(entity_id, component_type)
        ) |]
    , [sql| CREATE TABLE IF NOT EXISTS events (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            event_type TEXT NOT NULL,
            event_data TEXT NOT NULL,
            entity_id TEXT REFERENCES entities(entity_id) ON DELETE SET NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        ) |]
    , [sql| CREATE TABLE IF NOT EXISTS messages (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            conversation_id TEXT NOT NULL,
            turn_id TEXT NOT NULL,
            role TEXT NOT NULL,
            content TEXT NOT NULL,
            tool_calls TEXT,
            timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        ) |]
    , [sql| CREATE TABLE IF NOT EXISTS tool_calls (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            tool_call_id TEXT NOT NULL UNIQUE,
            turn_id TEXT NOT NULL,
            tool_name TEXT NOT NULL,
            tool_input TEXT NOT NULL,
            tool_result TEXT,
            parent_call_id TEXT,
            started_at TIMESTAMP,
            completed_at TIMESTAMP,
            status TEXT NOT NULL
        ) |]
    , -- Indexes
      [sql| CREATE INDEX IF NOT EXISTS idx_components_entity ON components(entity_id) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_components_type ON components(component_type) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_events_entity ON events(entity_id) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_events_created ON events(created_at) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_messages_conversation ON messages(conversation_id) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_messages_turn ON messages(turn_id) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_messages_timestamp ON messages(timestamp) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_tool_calls_turn ON tool_calls(turn_id) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_tool_calls_parent ON tool_calls(parent_call_id) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_tool_calls_status ON tool_calls(status) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_entities_type ON entities(entity_type) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_entities_updated ON entities(updated_at) |]
    ]

-------------------------------------------------------------------------------
-- SQLite Schema - Version 2 (Async Execution)
-------------------------------------------------------------------------------

-- | SQLite schema statements for version 2 (async execution support).
sqliteSchemaV2Statements :: [Query]
sqliteSchemaV2Statements =
    [ -- Tool call cache table
      [sql| CREATE TABLE IF NOT EXISTS tool_call_cache (
            cache_key TEXT PRIMARY KEY,
            tool_name TEXT NOT NULL,
            arguments_hash TEXT NOT NULL,
            result_json TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            expires_at TIMESTAMP
        ) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_cache_tool_name ON tool_call_cache(tool_name) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_cache_expires ON tool_call_cache(expires_at) |]
    , -- Tool continuations table
      [sql| CREATE TABLE IF NOT EXISTS tool_continuations (
            token TEXT PRIMARY KEY,
            session_id TEXT NOT NULL,
            tool_call_json TEXT NOT NULL,
            context_json TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            expires_at TIMESTAMP,
            completed_at TIMESTAMP,
            result_json TEXT
        ) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_continuations_session ON tool_continuations(session_id) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_continuations_expires ON tool_continuations(expires_at) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_continuations_completed ON tool_continuations(completed_at) |]
    , -- Update schema version
      [sql| INSERT INTO schema_version (version) VALUES (2) 
            ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP |]
    ]

-- | Execute SQLite schema statements individually.
executeSqliteSchema :: Connection -> IO ()
executeSqliteSchema conn = do
    forM_ sqliteSchemaV1Statements $ \stmt ->
        execute_ conn stmt
    forM_ sqliteSchemaV2Statements $ \stmt ->
        execute_ conn stmt

-- | Complete SQLite schema creation script (for reference/documentation).
sqliteCreateSchema :: Query
sqliteCreateSchema =
    [sql|
-- OS Persistence Schema v2

-- Schema version tracking
CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert initial version (only if not exists)
INSERT INTO schema_version (version) VALUES (1) ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP;

-- Entities table (sparse representation)
CREATE TABLE IF NOT EXISTS entities (
    entity_id TEXT PRIMARY KEY,
    entity_type TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Components table (JSON storage for flexibility)
CREATE TABLE IF NOT EXISTS components (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    entity_id TEXT NOT NULL REFERENCES entities(entity_id) ON DELETE CASCADE,
    component_type INTEGER NOT NULL,
    component_data JSON NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(entity_id, component_type)
);

-- Events table (append-only log)
CREATE TABLE IF NOT EXISTS events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    event_type TEXT NOT NULL,
    event_data JSON NOT NULL,
    entity_id TEXT REFERENCES entities(entity_id) ON DELETE SET NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Messages table (optimized for conversation queries)
CREATE TABLE IF NOT EXISTS messages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    conversation_id TEXT NOT NULL,
    turn_id TEXT NOT NULL,
    role TEXT NOT NULL,
    content TEXT NOT NULL,
    tool_calls JSON,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Tool calls table (lineage tracking)
CREATE TABLE IF NOT EXISTS tool_calls (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    tool_call_id TEXT NOT NULL UNIQUE,
    turn_id TEXT NOT NULL,
    tool_name TEXT NOT NULL,
    tool_input JSON NOT NULL,
    tool_result JSON,
    parent_call_id TEXT,
    started_at TIMESTAMP,
    completed_at TIMESTAMP,
    status TEXT NOT NULL
);

-- Tool call cache table (async execution)
CREATE TABLE IF NOT EXISTS tool_call_cache (
    cache_key TEXT PRIMARY KEY,
    tool_name TEXT NOT NULL,
    arguments_hash TEXT NOT NULL,
    result_json TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP
);

-- Tool continuations table (async execution)
CREATE TABLE IF NOT EXISTS tool_continuations (
    token TEXT PRIMARY KEY,
    session_id TEXT NOT NULL,
    tool_call_json TEXT NOT NULL,
    context_json TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    completed_at TIMESTAMP,
    result_json TEXT
);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_components_entity ON components(entity_id);
CREATE INDEX IF NOT EXISTS idx_components_type ON components(component_type);
CREATE INDEX IF NOT EXISTS idx_events_entity ON events(entity_id);
CREATE INDEX IF NOT EXISTS idx_events_created ON events(created_at);
CREATE INDEX IF NOT EXISTS idx_messages_conversation ON messages(conversation_id);
CREATE INDEX IF NOT EXISTS idx_messages_turn ON messages(turn_id);
CREATE INDEX IF NOT EXISTS idx_messages_timestamp ON messages(timestamp);
CREATE INDEX IF NOT EXISTS idx_tool_calls_turn ON tool_calls(turn_id);
CREATE INDEX IF NOT EXISTS idx_tool_calls_parent ON tool_calls(parent_call_id);
CREATE INDEX IF NOT EXISTS idx_tool_calls_status ON tool_calls(status);
CREATE INDEX IF NOT EXISTS idx_entities_type ON entities(entity_type);
CREATE INDEX IF NOT EXISTS idx_entities_updated ON entities(updated_at);
CREATE INDEX IF NOT EXISTS idx_cache_tool_name ON tool_call_cache(tool_name);
CREATE INDEX IF NOT EXISTS idx_cache_expires ON tool_call_cache(expires_at);
CREATE INDEX IF NOT EXISTS idx_continuations_session ON tool_continuations(session_id);
CREATE INDEX IF NOT EXISTS idx_continuations_expires ON tool_continuations(expires_at);
CREATE INDEX IF NOT EXISTS idx_continuations_completed ON tool_continuations(completed_at);
|]

-- | SQLite schema destruction script.
sqliteDropSchema :: Query
sqliteDropSchema =
    [sql|
DROP INDEX IF EXISTS idx_continuations_completed;
DROP INDEX IF EXISTS idx_continuations_expires;
DROP INDEX IF EXISTS idx_continuations_session;
DROP INDEX IF EXISTS idx_cache_expires;
DROP INDEX IF EXISTS idx_cache_tool_name;
DROP INDEX IF EXISTS idx_entities_updated;
DROP INDEX IF EXISTS idx_entities_type;
DROP INDEX IF EXISTS idx_tool_calls_status;
DROP INDEX IF EXISTS idx_tool_calls_parent;
DROP INDEX IF EXISTS idx_tool_calls_turn;
DROP INDEX IF EXISTS idx_messages_timestamp;
DROP INDEX IF EXISTS idx_messages_turn;
DROP INDEX IF EXISTS idx_messages_conversation;
DROP INDEX IF EXISTS idx_events_created;
DROP INDEX IF EXISTS idx_events_entity;
DROP INDEX IF EXISTS idx_components_type;
DROP INDEX IF EXISTS idx_components_entity;
DROP TABLE IF EXISTS tool_continuations;
DROP TABLE IF EXISTS tool_call_cache;
DROP TABLE IF EXISTS tool_calls;
DROP TABLE IF EXISTS messages;
DROP TABLE IF EXISTS events;
DROP TABLE IF EXISTS components;
DROP TABLE IF EXISTS entities;
DROP TABLE IF EXISTS schema_version;
|]

-- | SQLite migration script.
sqliteMigrationScript :: Int -> Int -> Maybe (Connection -> IO ())
sqliteMigrationScript fromVer toVer
    | fromVer == 0 && toVer == 1 = Just migrateV0ToV1
    | fromVer == 1 && toVer == 2 = Just migrateV1ToV2
    | fromVer == 0 && toVer == 2 = Just migrateV0ToV2
    | fromVer == toVer = Nothing -- No migration needed
    | otherwise = Nothing -- Unknown migration path
  where
    migrateV0ToV1 :: Connection -> IO ()
    migrateV0ToV1 conn =
        forM_ sqliteSchemaV1Statements $ \stmt ->
            execute_ conn stmt

    migrateV1ToV2 :: Connection -> IO ()
    migrateV1ToV2 conn =
        forM_ sqliteSchemaV2Statements $ \stmt ->
            execute_ conn stmt

    migrateV0ToV2 :: Connection -> IO ()
    migrateV0ToV2 conn = do
        migrateV0ToV1 conn
        migrateV1ToV2 conn

-------------------------------------------------------------------------------
-- PostgreSQL Schema
-------------------------------------------------------------------------------

-- | Complete PostgreSQL schema creation script.
postgresCreateSchema :: Query
postgresCreateSchema =
    [sql|
-- OS Persistence Schema v2

-- Enable UUID extension if not already enabled
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Schema version tracking
CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert initial version
INSERT INTO schema_version (version) VALUES (2) ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP;

-- Entities table (sparse representation)
CREATE TABLE IF NOT EXISTS entities (
    entity_id UUID PRIMARY KEY,
    entity_type TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Components table (JSONB storage for flexibility and indexing)
CREATE TABLE IF NOT EXISTS components (
    id SERIAL PRIMARY KEY,
    entity_id UUID NOT NULL REFERENCES entities(entity_id) ON DELETE CASCADE,
    component_type INTEGER NOT NULL,
    component_data JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(entity_id, component_type)
);

-- Events table (append-only log)
CREATE TABLE IF NOT EXISTS events (
    id SERIAL PRIMARY KEY,
    event_type TEXT NOT NULL,
    event_data JSONB NOT NULL,
    entity_id UUID REFERENCES entities(entity_id) ON DELETE SET NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Messages table (optimized for conversation queries)
CREATE TABLE IF NOT EXISTS messages (
    id SERIAL PRIMARY KEY,
    conversation_id UUID NOT NULL,
    turn_id UUID NOT NULL,
    role TEXT NOT NULL,
    content TEXT NOT NULL,
    tool_calls JSONB,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Tool calls table (lineage tracking)
CREATE TABLE IF NOT EXISTS tool_calls (
    id SERIAL PRIMARY KEY,
    tool_call_id UUID NOT NULL UNIQUE,
    turn_id UUID NOT NULL,
    tool_name TEXT NOT NULL,
    tool_input JSONB NOT NULL,
    tool_result JSONB,
    parent_call_id UUID,
    started_at TIMESTAMP,
    completed_at TIMESTAMP,
    status TEXT NOT NULL
);

-- Tool call cache table (async execution)
CREATE TABLE IF NOT EXISTS tool_call_cache (
    cache_key TEXT PRIMARY KEY,
    tool_name TEXT NOT NULL,
    arguments_hash TEXT NOT NULL,
    result_json JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP
);

-- Tool continuations table (async execution)
CREATE TABLE IF NOT EXISTS tool_continuations (
    token TEXT PRIMARY KEY,
    session_id UUID NOT NULL,
    tool_call_json JSONB NOT NULL,
    context_json JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    completed_at TIMESTAMP,
    result_json JSONB
);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_components_entity ON components(entity_id);
CREATE INDEX IF NOT EXISTS idx_components_type ON components(component_type);
CREATE INDEX IF NOT EXISTS idx_components_data ON components USING GIN(component_data);
CREATE INDEX IF NOT EXISTS idx_events_entity ON events(entity_id);
CREATE INDEX IF NOT EXISTS idx_events_created ON events(created_at);
CREATE INDEX IF NOT EXISTS idx_messages_conversation ON messages(conversation_id);
CREATE INDEX IF NOT EXISTS idx_messages_turn ON messages(turn_id);
CREATE INDEX IF NOT EXISTS idx_messages_timestamp ON messages(timestamp);
CREATE INDEX IF NOT EXISTS idx_tool_calls_turn ON tool_calls(turn_id);
CREATE INDEX IF NOT EXISTS idx_tool_calls_parent ON tool_calls(parent_call_id);
CREATE INDEX IF NOT EXISTS idx_tool_calls_status ON tool_calls(status);
CREATE INDEX IF NOT EXISTS idx_entities_type ON entities(entity_type);
CREATE INDEX IF NOT EXISTS idx_entities_updated ON entities(updated_at);
CREATE INDEX IF NOT EXISTS idx_cache_tool_name ON tool_call_cache(tool_name);
CREATE INDEX IF NOT EXISTS idx_cache_expires ON tool_call_cache(expires_at);
CREATE INDEX IF NOT EXISTS idx_continuations_session ON tool_continuations(session_id);
CREATE INDEX IF NOT EXISTS idx_continuations_expires ON tool_continuations(expires_at);
CREATE INDEX IF NOT EXISTS idx_continuations_completed ON tool_continuations(completed_at);
|]

-- | PostgreSQL schema destruction script.
postgresDropSchema :: Query
postgresDropSchema =
    [sql|
DROP INDEX IF EXISTS idx_continuations_completed;
DROP INDEX IF EXISTS idx_continuations_expires;
DROP INDEX IF EXISTS idx_continuations_session;
DROP INDEX IF EXISTS idx_cache_expires;
DROP INDEX IF EXISTS idx_cache_tool_name;
DROP INDEX IF EXISTS idx_entities_updated;
DROP INDEX IF EXISTS idx_entities_type;
DROP INDEX IF EXISTS idx_tool_calls_status;
DROP INDEX IF EXISTS idx_tool_calls_parent;
DROP INDEX IF EXISTS idx_tool_calls_turn;
DROP INDEX IF EXISTS idx_messages_timestamp;
DROP INDEX IF EXISTS idx_messages_turn;
DROP INDEX IF EXISTS idx_messages_conversation;
DROP INDEX IF EXISTS idx_events_created;
DROP INDEX IF EXISTS idx_events_entity;
DROP INDEX IF EXISTS idx_components_data;
DROP INDEX IF EXISTS idx_components_type;
DROP INDEX IF EXISTS idx_components_entity;
DROP TABLE IF EXISTS tool_continuations;
DROP TABLE IF EXISTS tool_call_cache;
DROP TABLE IF EXISTS tool_calls;
DROP TABLE IF EXISTS messages;
DROP TABLE IF EXISTS events;
DROP TABLE IF EXISTS components;
DROP TABLE IF EXISTS entities;
DROP TABLE IF EXISTS schema_version;
|]

-- | PostgreSQL migration script.
postgresMigrationScript :: Int -> Int -> Maybe Query
postgresMigrationScript fromVer toVer
    | fromVer == 0 && toVer == 1 = Just postgresCreateV1Schema
    | fromVer == 1 && toVer == 2 = Just postgresCreateV2Schema
    | fromVer == 0 && toVer == 2 = Just postgresCreateSchema
    | fromVer == toVer = Nothing -- No migration needed
    | otherwise = Nothing -- Unknown migration path
  where
    postgresCreateV1Schema =
        [sql|
-- V1 schema (legacy)
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
INSERT INTO schema_version (version) VALUES (1) ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP;
-- (rest of V1 schema omitted for brevity)
|]

    postgresCreateV2Schema =
        [sql|
-- V2 additions
CREATE TABLE IF NOT EXISTS tool_call_cache (
    cache_key TEXT PRIMARY KEY,
    tool_name TEXT NOT NULL,
    arguments_hash TEXT NOT NULL,
    result_json JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP
);
CREATE INDEX IF NOT EXISTS idx_cache_tool_name ON tool_call_cache(tool_name);
CREATE INDEX IF NOT EXISTS idx_cache_expires ON tool_call_cache(expires_at);

CREATE TABLE IF NOT EXISTS tool_continuations (
    token TEXT PRIMARY KEY,
    session_id UUID NOT NULL,
    tool_call_json JSONB NOT NULL,
    context_json JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    completed_at TIMESTAMP,
    result_json JSONB
);
CREATE INDEX IF NOT EXISTS idx_continuations_session ON tool_continuations(session_id);
CREATE INDEX IF NOT EXISTS idx_continuations_expires ON tool_continuations(expires_at);
CREATE INDEX IF NOT EXISTS idx_continuations_completed ON tool_continuations(completed_at);

INSERT INTO schema_version (version) VALUES (2) ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP;
|]

-------------------------------------------------------------------------------
-- Async Execution Schema Helpers
-------------------------------------------------------------------------------

{- | Create only the async execution schema tables (SQLite).

Useful when you want to add async support to an existing database
without recreating the entire schema.
-}
sqliteCreateAsyncSchema :: Query
sqliteCreateAsyncSchema =
    [sql|
-- Tool call cache table
CREATE TABLE IF NOT EXISTS tool_call_cache (
    cache_key TEXT PRIMARY KEY,
    tool_name TEXT NOT NULL,
    arguments_hash TEXT NOT NULL,
    result_json TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_cache_tool_name ON tool_call_cache(tool_name);
CREATE INDEX IF NOT EXISTS idx_cache_expires ON tool_call_cache(expires_at);

-- Tool continuations table
CREATE TABLE IF NOT EXISTS tool_continuations (
    token TEXT PRIMARY KEY,
    session_id TEXT NOT NULL,
    tool_call_json TEXT NOT NULL,
    context_json TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    completed_at TIMESTAMP,
    result_json TEXT
);

CREATE INDEX IF NOT EXISTS idx_continuations_session ON tool_continuations(session_id);
CREATE INDEX IF NOT EXISTS idx_continuations_expires ON tool_continuations(expires_at);
CREATE INDEX IF NOT EXISTS idx_continuations_completed ON tool_continuations(completed_at);

-- Update schema version
INSERT INTO schema_version (version) VALUES (2) 
    ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP;
|]

{- | Create only the async execution schema tables (PostgreSQL).

Useful when you want to add async support to an existing database
without recreating the entire schema.
-}
postgresCreateAsyncSchema :: Query
postgresCreateAsyncSchema =
    [sql|
-- Tool call cache table
CREATE TABLE IF NOT EXISTS tool_call_cache (
    cache_key TEXT PRIMARY KEY,
    tool_name TEXT NOT NULL,
    arguments_hash TEXT NOT NULL,
    result_json JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_cache_tool_name ON tool_call_cache(tool_name);
CREATE INDEX IF NOT EXISTS idx_cache_expires ON tool_call_cache(expires_at);

-- Tool continuations table
CREATE TABLE IF NOT EXISTS tool_continuations (
    token TEXT PRIMARY KEY,
    session_id UUID NOT NULL,
    tool_call_json JSONB NOT NULL,
    context_json JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    completed_at TIMESTAMP,
    result_json JSONB
);

CREATE INDEX IF NOT EXISTS idx_continuations_session ON tool_continuations(session_id);
CREATE INDEX IF NOT EXISTS idx_continuations_expires ON tool_continuations(expires_at);
CREATE INDEX IF NOT EXISTS idx_continuations_completed ON tool_continuations(completed_at);

-- Update schema version
INSERT INTO schema_version (version) VALUES (2) 
    ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP;
|]

-------------------------------------------------------------------------------
-- Migration
-------------------------------------------------------------------------------

-- | Migration operation.
data Migration = Migration
    { migrationFromVersion :: Int
    , migrationToVersion :: Int
    , migrationName :: Text
    , migrationSql :: Query
    }
    deriving (Show, Eq)

{- | Get the current schema version from the database.
Returns 0 if the schema_version table doesn't exist (fresh database).
-}
getSchemaVersion :: Connection -> IO Int
getSchemaVersion conn = do
    -- Check if schema_version table exists
    tableExists <-
        query
            conn
            [sql|
                SELECT name FROM sqlite_master 
                WHERE type='table' AND name='schema_version'
            |]
            () ::
            IO [Only Text]

    case tableExists of
        [] -> pure 0 -- Table doesn't exist, return version 0
        _ -> do
            -- Get current version from the table
            result <- query_ conn "SELECT version FROM schema_version ORDER BY version DESC LIMIT 1"
            case result of
                [Only v] -> pure v
                _ -> pure 0

-- | Apply migrations to reach the target schema version.
migrateSchema ::
    Connection ->
    (Int -> Int -> Maybe (Connection -> IO ())) -> -- Migration script lookup
    IO (Either Text Int)
migrateSchema conn getMigration = do
    currentVer <- getSchemaVersion conn
    if currentVer == currentSchemaVersion
        then pure $ Right currentVer
        else applyMigrations conn currentVer currentSchemaVersion getMigration

-- | Apply migrations from current version to target version.
applyMigrations ::
    Connection ->
    Int ->
    Int ->
    (Int -> Int -> Maybe (Connection -> IO ())) ->
    IO (Either Text Int)
applyMigrations conn fromVer toVer getMigration
    | fromVer >= toVer = pure $ Right fromVer
    | otherwise =
        case getMigration fromVer toVer of
            Nothing ->
                pure $ Left $ Text.pack $ "No migration path from " ++ show fromVer ++ " to " ++ show toVer
            Just migrationAction -> do
                -- Execute migration within a transaction
                withTransaction conn $ do
                    migrationAction conn
                    -- Update schema version
                    execute conn "INSERT INTO schema_version (version) VALUES (?) ON CONFLICT(version) DO UPDATE SET applied_at = CURRENT_TIMESTAMP" (Only toVer)
                pure $ Right toVer
