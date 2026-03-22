{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
SQLite resource component for the OS resource management system.

This module provides:
* Component storage for SQLite connections
* Resource accessor type for type-safe access
* Integration with the ECS component system

== Example Usage

@
import System.Agents.OS.Resources.Sqlite
import qualified Database.SQLite.Simple as SQLite

-- Access a SQLite resource through the handle
handleAccess handle $ \accessor -> do
    let conn = sqliteConnection accessor
    SQLite.query_ conn "SELECT * FROM users"
@
-}
module System.Agents.OS.Resources.Sqlite (
    -- * Component
    SqliteResourceData (..),
    SqliteAccessor (..),
    sqliteResourceComponentId,

    -- * Operations
    newSqliteAccessor,
    closeSqliteAccessor,
    withSqliteAccessor,
) where

import Control.Exception (bracket, try)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite3 as Direct

import System.Agents.OS.Core.Types (Component (..), ComponentTypeId (..))
import System.Agents.OS.Resources.Types (SqliteAccessMode (..), SqliteConfig (..))

-------------------------------------------------------------------------------
-- Component Definition
-------------------------------------------------------------------------------

{- | SQLite resource data stored as a Component.

This holds the actual database connection and access mode information.
It is stored in the ECS World as a component attached to a ResourceId entity.
-}
data SqliteResourceData = SqliteResourceData
    { sqliteResourceConnection :: Connection
    -- ^ The SQLite database connection (from sqlite-simple)
    , sqliteResourceDirectDb :: Direct.Database
    -- ^ Direct database handle for metadata access
    , sqliteResourceAccessMode :: SqliteAccessMode
    -- ^ Access mode (serial or concurrent reads)
    }

instance Show SqliteResourceData where
    show _ = "SqliteResourceData{..}"

-- | Component ID for SqliteResourceData (allocated as ComponentTypeId 11 per spec)
sqliteResourceComponentId :: ComponentTypeId
sqliteResourceComponentId = ComponentTypeId 11

instance Component SqliteResourceData where
    componentId _ = sqliteResourceComponentId

{- | Accessor type for SQLite resources.

This provides a type-safe way to access SQLite resources through the
resource handle's access function.
-}
newtype SqliteAccessor = SqliteAccessor
    { sqliteConn :: Connection
    }

-- | Create a new SQLite accessor from resource data.
newSqliteAccessor :: SqliteResourceData -> SqliteAccessor
newSqliteAccessor data_ = SqliteAccessor{sqliteConn = sqliteResourceConnection data_}

-- | Close a SQLite accessor (and its underlying connection).
closeSqliteAccessor :: SqliteAccessor -> IO ()
closeSqliteAccessor accessor = do
    SQLite.close (sqliteConn accessor)

{- | Execute an action with a SQLite accessor.

This is a helper function that handles opening and closing the connection
based on the configuration. In practice, the accessor is created once
and reused through the resource handle.

Example:
@
withSqliteAccessor config $ \accessor -> do
    result <- SQLite.query_ (sqliteConn accessor) "SELECT * FROM users"
    process result
@
-}
withSqliteAccessor ::
    SqliteConfig ->
    (SqliteAccessor -> IO a) ->
    IO (Either String a)
withSqliteAccessor config action = do
    result <- try $ bracket open close (action . newSqliteAccessor)
    case result of
        Left (e :: IOError) -> pure $ Left $ show e
        Right val -> pure $ Right val
  where
    open = do
        conn <- SQLite.open (sqlitePath config)
        directDb <- Direct.open mempty -- Note: This would need the actual path
        pure $
            SqliteResourceData
                { sqliteResourceConnection = conn
                , sqliteResourceDirectDb = directDb
                , sqliteResourceAccessMode = sqliteAccessMode config
                }
    close data_ = do
        SQLite.close (sqliteResourceConnection data_)
        Direct.close (sqliteResourceDirectDb data_)

