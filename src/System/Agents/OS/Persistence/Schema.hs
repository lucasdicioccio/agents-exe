{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Database schema definitions for the OS persistence layer.

This module provides SQL schema definitions for SQLite and PostgreSQL
backends, along with migration support.
-}
module System.Agents.OS.Persistence.Schema (
    -- * Schema Version
    currentSchemaVersion,

    -- * SQLite Schema
    sqliteCreateSchema,
    sqliteDropSchema,
    sqliteMigrationScript,

    -- * PostgreSQL Schema
    postgresCreateSchema,
    postgresDropSchema,
    postgresMigrationScript,

    -- * Migration
    Migration (..),
    migrateSchema,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, execute_, query_)
import Database.SQLite.Simple.QQ (sql)

-------------------------------------------------------------------------------
-- Schema Version
-------------------------------------------------------------------------------

{- | Current schema version number.
Increment this when schema changes require migrations.
-}
currentSchemaVersion :: Int
currentSchemaVersion = 1

-------------------------------------------------------------------------------
-- SQLite Schema
-------------------------------------------------------------------------------

-- | Complete SQLite schema creation script.
sqliteCreateSchema :: Query
sqliteCreateSchema =
    [sql|
-- OS Persistence Schema v1

-- Schema version tracking
CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert initial version
INSERT OR IGNORE INTO schema_version (version) VALUES (1);

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
|]

-- | SQLite schema destruction script.
sqliteDropSchema :: Query
sqliteDropSchema =
    [sql|
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
DROP TABLE IF EXISTS tool_calls;
DROP TABLE IF EXISTS messages;
DROP TABLE IF EXISTS events;
DROP TABLE IF EXISTS components;
DROP TABLE IF EXISTS entities;
DROP TABLE IF EXISTS schema_version;
|]

-- | SQLite migration script (version 0 -> 1).
sqliteMigrationScript :: Int -> Int -> Maybe Query
sqliteMigrationScript fromVer toVer
    | fromVer == 0 && toVer == 1 = Just sqliteCreateSchema
    | fromVer == toVer = Nothing -- No migration needed
    | otherwise = Nothing -- Unknown migration path

-------------------------------------------------------------------------------
-- PostgreSQL Schema
-------------------------------------------------------------------------------

-- | Complete PostgreSQL schema creation script.
postgresCreateSchema :: Query
postgresCreateSchema =
    [sql|
-- OS Persistence Schema v1

-- Enable UUID extension if not already enabled
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Schema version tracking
CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert initial version
INSERT INTO schema_version (version) VALUES (1) ON CONFLICT DO NOTHING;

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
|]

-- | PostgreSQL schema destruction script.
postgresDropSchema :: Query
postgresDropSchema =
    [sql|
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
DROP TABLE IF EXISTS tool_calls;
DROP TABLE IF EXISTS messages;
DROP TABLE IF EXISTS events;
DROP TABLE IF EXISTS components;
DROP TABLE IF EXISTS entities;
DROP TABLE IF EXISTS schema_version;
|]

-- | PostgreSQL migration script (version 0 -> 1).
postgresMigrationScript :: Int -> Int -> Maybe Query
postgresMigrationScript fromVer toVer
    | fromVer == 0 && toVer == 1 = Just postgresCreateSchema
    | fromVer == toVer = Nothing -- No migration needed
    | otherwise = Nothing -- Unknown migration path

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

-- | Get the current schema version from the database.
getSchemaVersion :: Connection -> IO Int
getSchemaVersion conn = do
    -- Ensure schema_version table exists
    execute_ conn "CREATE TABLE IF NOT EXISTS schema_version (version INTEGER PRIMARY KEY)"
    -- Get current version
    result <- query_ conn "SELECT version FROM schema_version ORDER BY version DESC LIMIT 1"
    case result of
        [Only v] -> pure v
        _ -> pure 0

-- | Apply migrations to reach the target schema version.
migrateSchema ::
    Connection ->
    (Int -> Int -> Maybe Query) -> -- Migration script lookup
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
    (Int -> Int -> Maybe Query) ->
    IO (Either Text Int)
applyMigrations conn fromVer toVer getMigration
    | fromVer >= toVer = pure $ Right fromVer
    | otherwise =
        case getMigration fromVer toVer of
            Nothing ->
                pure $ Left $ Text.pack $ "No migration path from " ++ show fromVer ++ " to " ++ show toVer
            Just migrationQuery -> do
                -- Execute migration
                execute_ conn migrationQuery
                -- Update schema version
                execute conn "INSERT OR REPLACE INTO schema_version (version) VALUES (?)" (Only toVer)
                pure $ Right toVer
