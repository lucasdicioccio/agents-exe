{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- | Agent configurations kept in a database rather than in files.

A stored agent is the @contents@ of an agent file (the 'Base.Agent'
record), keyed by slug. Only configurations that need no files can be
stored: see 'fileBasedFields'.
-}
module System.Agents.AgentStore (
    StoredAgent (..),
    AgentStore (..),
    mkSqliteAgentStore,
    fileBasedFields,
) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, Only (..), execute, execute_, query, query_)
import Database.SQLite.Simple.QQ (sql)

import qualified System.Agents.Base as Base
import System.Agents.SessionStore (Migration (..), runMigrations)

data StoredAgent = StoredAgent
    { saConfig :: Base.Agent
    , saUpdatedAt :: UTCTime
    , saUpdatedBy :: Maybe Text
    -- ^ The owner who stored it, when the host authenticates callers.
    }
    deriving (Show)

data AgentStore = AgentStore
    { asList :: IO [StoredAgent]
    -- ^ Every stored agent whose configuration still parses.
    , asPut :: Maybe Text -> Base.Agent -> IO StoredAgent
    -- ^ Insert or replace an agent, by slug.
    , asDelete :: Text -> IO Bool
    -- ^ Remove an agent; 'False' when there was none.
    }

{- | Fields of a configuration that refer to files or directories, and that
a stored agent therefore cannot use, with their names.
-}
fileBasedFields :: Base.Agent -> [Text]
fileBasedFields agent =
    [ name
    | (name, used) <-
        [ ("toolDirectory", isSet (Base.toolDirectory agent))
        , ("bashToolboxes", nonEmpty (Base.bashToolboxes agent))
        , ("openApiToolboxes", nonEmpty (Base.openApiToolboxes agent))
        , ("postgrestToolboxes", nonEmpty (Base.postgrestToolboxes agent))
        , ("extraAgents", nonEmpty (Base.extraAgents agent))
        , ("skillSources", nonEmpty (Base.skillSources agent))
        , ("autoEnableSkills", nonEmpty (Base.autoEnableSkills agent))
        ]
    , used
    ]
  where
    isSet = maybe False (const True)
    nonEmpty = maybe False (not . null)

-- | An agent store in the host's SQLite database (table @agents@).
mkSqliteAgentStore :: Connection -> IO AgentStore
mkSqliteAgentStore conn = do
    runMigrations conn "agents" agentMigrations
    pure
        AgentStore
            { asList = do
                rows <- query_ conn [sql| SELECT json, updated_at, updated_by FROM agents ORDER BY slug |] :: IO [(Text, UTCTime, Maybe Text)]
                pure $ mapMaybe fromRow rows
            , asPut = \by agent -> do
                now <- getCurrentTime
                execute
                    conn
                    [sql| INSERT INTO agents (slug, json, updated_at, updated_by) VALUES (?, ?, ?, ?)
                          ON CONFLICT(slug) DO UPDATE SET
                            json = excluded.json, updated_at = excluded.updated_at, updated_by = excluded.updated_by |]
                    (Base.slug agent, encode agent, now, by)
                pure $ StoredAgent agent now by
            , asDelete = \slug -> do
                rows <- query conn [sql| DELETE FROM agents WHERE slug = ? RETURNING slug |] (Only slug) :: IO [Only Text]
                pure $ not (null rows)
            }
  where
    fromRow (json, updated, by) = do
        agent <- Aeson.decodeStrict' (TextEnc.encodeUtf8 json)
        pure $ StoredAgent agent updated by
    encode = TextEnc.decodeUtf8 . LByteString.toStrict . Aeson.encode

agentMigrations :: [Migration]
agentMigrations =
    [ Migration 1 $ \conn ->
        execute_
            conn
            [sql| CREATE TABLE IF NOT EXISTS agents (
                slug TEXT PRIMARY KEY,
                json TEXT NOT NULL,
                updated_at TIMESTAMP NOT NULL,
                updated_by TEXT
            ) |]
    ]
