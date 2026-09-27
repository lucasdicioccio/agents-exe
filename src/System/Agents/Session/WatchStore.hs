{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- | The durable, SQLite-backed 'WatchStore' (@todos/os-as-standalone-server.md@
G11): a @session_watches@ table next to @session_mail@, mirroring its
schema/migration pattern ("System.Agents.Session.MailStore"), so an active
@watch-session@ registration (@todos/session-mailbox.md@ §7) survives a
restart: 'System.Agents.Host.Runner.recoverOnStartup' re-registers every
row here whose deadline has not yet passed; a 'WatchRequest' carries no
secret, so unlike session parameters (D7) there is nothing here that must
stay volatile.

A row is written when a watch is registered and removed when it is
unregistered or its forwarding loop drops it (TTL elapsed, or the watcher's
mailbox is gone) -- the table always mirrors "currently active watches",
never a log.
-}
module System.Agents.Session.WatchStore (
    WatchStore (..),
    PersistedWatch (..),
    mkSqliteWatchStore,
    initializeWatchSchema,

    -- * Shared with other backends (e.g. "System.Agents.Postgres")
    encodeWatchRequest,
    decodeWatchRequest,
) where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import Database.SQLite.Simple (Connection, Only (..), Query, execute, execute_, query_)
import Database.SQLite.Simple.QQ (sql)

import System.Agents.Session.Mailbox (WatchRequest (..))
import System.Agents.Session.Types (SessionId (..))
import System.Agents.SessionStore (Migration (..), runMigrations)

-- | One @watch-session@ registration, as persisted.
data PersistedWatch = PersistedWatch
    { pwWatchId :: Text
    , pwWatcher :: SessionId
    , pwRequest :: WatchRequest
    , pwDeadline :: UTCTime
    }

-- | Durable storage for active watches, so a restart can re-register them.
data WatchStore = WatchStore
    { wsSave :: PersistedWatch -> IO ()
    -- ^ Insert or replace a watch's row (idempotent on 'pwWatchId').
    , wsDelete :: Text -> IO ()
    -- ^ Remove a watch's row (stopped, expired, or its loop ended).
    , wsLoadAll :: IO [PersistedWatch]
    -- ^ Every still-persisted watch, in no particular order. A row whose
    -- 'WatchRequest' or watcher id fails to decode is skipped rather than
    -- failing the whole load (it can never have been written by this
    -- module, so it is not this process's to recover).
    }

-- | Create a SQLite-backed 'WatchStore', creating/migrating its table first.
mkSqliteWatchStore :: Connection -> IO WatchStore
mkSqliteWatchStore conn = do
    initializeWatchSchema conn
    pure
        WatchStore
            { wsSave = sqliteSaveWatch conn
            , wsDelete = sqliteDeleteWatch conn
            , wsLoadAll = sqliteLoadAllWatches conn
            }

-- | Create or migrate the @session_watches@ schema.
initializeWatchSchema :: Connection -> IO ()
initializeWatchSchema conn = runMigrations conn "session_watches" watchMigrations

watchMigrations :: [Migration]
watchMigrations =
    [Migration 1 $ \c -> mapM_ (execute_ c) watchSchemaStatements]

watchSchemaStatements :: [Query]
watchSchemaStatements =
    [ [sql| CREATE TABLE IF NOT EXISTS session_watches (
            watch_id TEXT PRIMARY KEY,
            watcher_session_id TEXT NOT NULL,
            request_json TEXT NOT NULL,
            deadline TIMESTAMP NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        ) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_session_watches_watcher ON session_watches(watcher_session_id) |]
    ]

sqliteSaveWatch :: Connection -> PersistedWatch -> IO ()
sqliteSaveWatch conn pw = do
    let SessionId watcherUuid = pw.pwWatcher
        bodyJson = TextEnc.decodeUtf8 $ LBS.toStrict $ Aeson.encode (encodeWatchRequest pw.pwRequest)
    execute
        conn
        [sql| INSERT INTO session_watches (watch_id, watcher_session_id, request_json, deadline)
              VALUES (?, ?, ?, ?)
              ON CONFLICT(watch_id) DO UPDATE SET
                watcher_session_id = excluded.watcher_session_id,
                request_json = excluded.request_json,
                deadline = excluded.deadline |]
        (pw.pwWatchId, UUID.toText watcherUuid, bodyJson, pw.pwDeadline)

sqliteDeleteWatch :: Connection -> Text -> IO ()
sqliteDeleteWatch conn watchId =
    execute conn [sql| DELETE FROM session_watches WHERE watch_id = ? |] (Only watchId)

sqliteLoadAllWatches :: Connection -> IO [PersistedWatch]
sqliteLoadAllWatches conn = do
    rows <-
        query_
            conn
            [sql| SELECT watch_id, watcher_session_id, request_json, deadline FROM session_watches |] ::
            IO [(Text, Text, Text, UTCTime)]
    pure $ mapMaybe decodeRow rows
  where
    decodeRow :: (Text, Text, Text, UTCTime) -> Maybe PersistedWatch
    decodeRow (watchId, watcherText, bodyJson, deadline) = do
        watcherUuid <- UUID.fromText watcherText
        value <- Aeson.decode (LBS.fromStrict (TextEnc.encodeUtf8 bodyJson))
        req <- Aeson.parseMaybe decodeWatchRequest value
        pure $ PersistedWatch watchId (SessionId watcherUuid) req deadline

encodeWatchRequest :: WatchRequest -> Aeson.Value
encodeWatchRequest req =
    Aeson.object
        [ "target" .= (let SessionId t = req.wrTarget in UUID.toText t)
        , "events" .= req.wrEvents
        , "tool" .= req.wrTool
        , "ttlSeconds" .= req.wrTtlSeconds
        ]

decodeWatchRequest :: Aeson.Value -> Aeson.Parser WatchRequest
decodeWatchRequest = Aeson.withObject "WatchRequest" $ \o -> do
    targetText <- o .: "target"
    target <- maybe (fail "bad target uuid") pure (UUID.fromText targetText)
    events <- o .:? "events"
    tool <- o .:? "tool"
    ttl <- o .:? "ttlSeconds"
    pure WatchRequest{wrTarget = SessionId target, wrEvents = events, wrTool = tool, wrTtlSeconds = ttl}
