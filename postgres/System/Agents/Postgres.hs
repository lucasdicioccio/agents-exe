{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- | Sessions and continuations in Postgres.

The schema and the semantics match the SQLite stores in @agents-lib@:
versioned compare-and-store, metadata columns, and component-scoped
migrations in @schema_migrations@. Connections come from a pool, so the
stores can be used from many threads.

Several processes may run sessions on one database. 'withPostgresStores'
gives each a 'Coordination': a lease per run in the @run_owner@ and
@run_lease_until@ columns of @sessions@, measured on the database's clock,
and a @NOTIFY@ on the @agents_sessions@ channel for each session written
and each mail accepted, which the other processes @LISTEN@ to.

@
withPostgresStores "postgresql://localhost/agents" $ \\stores ->
    withHostStores cfg stores tracer $ \\host -> …
@
-}
module System.Agents.Postgres (
    -- * Stores
    withPostgresStores,
    openPostgresPool,
    mkPostgresSessionStore,
    mkPostgresContinuationStore,
    mkPostgresMailStore,
    mkPostgresWatchStore,
    mkPostgresAgentStore,
    isPostgresUrl,

    -- * Several processes on one database
    PgInstance (..),
    newPgInstance,
    mkPostgresSessionStoreAs,
    mkPostgresMailStoreAs,
    mkPostgresCoordination,
    PgListener,
    withPgListener,

    -- * Migrations
    PgMigration (..),
    runPostgresMigrations,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, stateTVar)
import Control.Exception (SomeAsyncException, SomeException, bracket, fromException, throwIO, try)
import Control.Monad (forM_, forever, unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (NominalDiffTime, UTCTime (..), getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple (
    Connection,
    In (..),
    Only (..),
    Query,
    close,
    connectPostgreSQL,
    execute,
    execute_,
    query,
    query_,
    withTransaction,
    (:.) (..),
 )
import Database.PostgreSQL.Simple.Notification (Notification (..), getNotification)
import Database.PostgreSQL.Simple.ToField (Action, ToField (..))
import System.Timeout (timeout)

import System.Agents.AgentStore (AgentStore (..), StoredAgent (..))
import qualified System.Agents.Base as Base
import System.Agents.Host (HostStores (..))
import System.Agents.Host.Coordination (Coordination (..), LeaseResult (..), SessionSignal (..))
import System.Agents.Session.Async (ContinuationStore (..), ContinuationToken (..), ToolContinuationSnapshot (..))
import System.Agents.Session.Base (Envelope (..), Session, SessionId (..), SessionStatus (..), UserToolResponse, messageIdText, parseSessionStatus, sessionStatusOf, sessionStatusText)
import System.Agents.Session.Mailbox (MailStore (..))
import System.Agents.Session.WatchStore (PersistedWatch (..), WatchStore (..), decodeWatchRequest, encodeWatchRequest)
import System.Agents.SessionStore (
    SessionBackend (..),
    SessionLabels (..),
    SessionMeta (..),
    SessionQuery (..),
    VersionConflict (..),
    decodeSecurity,
    encodeSecurity,
    noLabels,
 )

-------------------------------------------------------------------------------
-- Stores
-------------------------------------------------------------------------------

{- | Open a pool on a connection string (a @postgresql://@ URL or
@key=value@ pairs), migrate, and run the action with the stores.

The stores come with the 'Coordination' that lets other processes use the
same database: this process gets a fresh 'PgInstance', and one more
connection, outside the pool, listens to what the others write.
-}
withPostgresStores :: ByteString -> (HostStores -> IO a) -> IO a
withPostgresStores url action =
    bracket (openPostgresPool url 10) destroyAllResources $ \pool -> do
        me <- newPgInstance
        sessions <- mkPostgresSessionStoreAs me pool
        continuations <- mkPostgresContinuationStore pool
        mail <- mkPostgresMailStoreAs me pool
        watches <- mkPostgresWatchStore pool
        agents <- mkPostgresAgentStore pool
        withPgListener url me $ \listener ->
            action (HostStores sessions continuations mail watches (Just agents) (mkPostgresCoordination me pool listener))

-- | A pool of at most the given number of connections, idle ones closed after a minute.
openPostgresPool :: ByteString -> Int -> IO (Pool Connection)
openPostgresPool url size = newPool (defaultPoolConfig (connectPostgreSQL url) close 60 size)

-- | Whether a @--db@ value names a Postgres database rather than a SQLite file.
isPostgresUrl :: String -> Bool
isPostgresUrl db = any (`isPrefixOf` db) ["postgres://", "postgresql://"]

-------------------------------------------------------------------------------
-- Migrations
-------------------------------------------------------------------------------

data PgMigration = PgMigration
    { pgMigrationVersion :: Int
    , pgMigrationApply :: Connection -> IO ()
    }

{- | Apply the migrations of a component that were not applied yet, in one
transaction holding an advisory lock, so that processes starting together
do not race.
-}
runPostgresMigrations :: Pool Connection -> Text -> [PgMigration] -> IO ()
runPostgresMigrations pool component migrations =
    withResource pool $ \conn -> withTransaction conn $ do
        _ <- query_ conn "SELECT pg_advisory_xact_lock(7236871069110195828)::text" :: IO [Only Text]
        -- "already exists, skipping" notices would land on stderr.
        void $ execute_ conn "SET LOCAL client_min_messages = warning"
        void $
            execute_
                conn
                "CREATE TABLE IF NOT EXISTS schema_migrations (\
                \ component TEXT NOT NULL,\
                \ version INTEGER NOT NULL,\
                \ applied_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
                \ PRIMARY KEY (component, version))"
        applied <-
            map fromOnly
                <$> (query conn "SELECT version FROM schema_migrations WHERE component = ?" (Only component) :: IO [Only Int])
        forM_ (sortOn (.pgMigrationVersion) migrations) $ \m ->
            unless (m.pgMigrationVersion `elem` applied) $ do
                m.pgMigrationApply conn
                void $ execute conn "INSERT INTO schema_migrations (component, version) VALUES (?, ?)" (component, m.pgMigrationVersion)

statements :: [Query] -> Connection -> IO ()
statements qs conn = mapM_ (execute_ conn) qs

sessionMigrations :: [PgMigration]
sessionMigrations =
    [ PgMigration 1 $
        statements
            [ "CREATE TABLE IF NOT EXISTS sessions (\
              \ session_id TEXT PRIMARY KEY,\
              \ created_at TIMESTAMPTZ NOT NULL,\
              \ updated_at TIMESTAMPTZ NOT NULL,\
              \ json TEXT NOT NULL,\
              \ agent_slug TEXT,\
              \ parent_session_id TEXT,\
              \ owner TEXT,\
              \ status TEXT NOT NULL DEFAULT 'ready',\
              \ status_detail TEXT,\
              \ version INTEGER NOT NULL DEFAULT 0)"
            , "CREATE INDEX IF NOT EXISTS idx_sessions_updated ON sessions(updated_at)"
            , "CREATE INDEX IF NOT EXISTS idx_sessions_status ON sessions(status, updated_at)"
            , "CREATE INDEX IF NOT EXISTS idx_sessions_parent ON sessions(parent_session_id)"
            , "CREATE INDEX IF NOT EXISTS idx_sessions_owner ON sessions(owner, updated_at)"
            ]
    , PgMigration 2 $
        statements
            [ "ALTER TABLE sessions ADD COLUMN IF NOT EXISTS params TEXT NOT NULL DEFAULT '{}'"
            ]
    , PgMigration 3 $
        statements
            [ "ALTER TABLE sessions ADD COLUMN IF NOT EXISTS security TEXT NOT NULL DEFAULT '{}'"
            ]
    , -- The run lease: which process runs the session, and until when
      -- without a renewal. Both NULL when no run holds the session.
      PgMigration 4 $
        statements
            [ "ALTER TABLE sessions ADD COLUMN IF NOT EXISTS run_owner TEXT"
            , "ALTER TABLE sessions ADD COLUMN IF NOT EXISTS run_lease_until TIMESTAMPTZ"
            , "CREATE INDEX IF NOT EXISTS idx_sessions_run_lease ON sessions(run_lease_until) WHERE run_owner IS NOT NULL"
            ]
    ]

continuationMigrations :: [PgMigration]
continuationMigrations =
    [ PgMigration 1 $
        statements
            [ "CREATE TABLE IF NOT EXISTS tool_continuations (\
              \ token TEXT PRIMARY KEY,\
              \ session_id TEXT NOT NULL,\
              \ tool_call_json TEXT NOT NULL,\
              \ context_json TEXT NOT NULL,\
              \ created_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
              \ expires_at TIMESTAMPTZ,\
              \ completed_at TIMESTAMPTZ,\
              \ result_json TEXT)"
            , "CREATE INDEX IF NOT EXISTS idx_continuations_session_completed ON tool_continuations(session_id, completed_at)"
            , "CREATE INDEX IF NOT EXISTS idx_continuations_expires ON tool_continuations(expires_at)"
            ]
    ]

-------------------------------------------------------------------------------
-- Sessions
-------------------------------------------------------------------------------

{- | A session backend on the pool, after migrating its table. It signals
nothing: for a database only this process runs sessions on. See
'mkPostgresSessionStoreAs'.
-}
mkPostgresSessionStore :: Pool Connection -> IO SessionBackend
mkPostgresSessionStore = mkPostgresSessionStoreAs silentInstance

{- | Like 'mkPostgresSessionStore', signalling each write and each deletion
to the other processes on the database, as coming from the given instance.
-}
mkPostgresSessionStoreAs :: PgInstance -> Pool Connection -> IO SessionBackend
mkPostgresSessionStoreAs me pool = do
    runPostgresMigrations pool "sessions" sessionMigrations
    let with = withResource pool
        stored c sid = signal c me SessionStored sid
    pure
        SessionBackend
            { sbStore = \sid sess -> with $ \c -> storeLabelled c noLabels sid sess >> stored c sid
            , sbLoad = \sid -> fmap fst <$> with (\c -> loadMeta c sid)
            , sbList = with listSessions
            , sbDelete = \sid -> with $ \c -> do
                void $ execute c "DELETE FROM sessions WHERE session_id = ?" (Only (sessionIdText sid))
                stored c sid
            , sbStoreLabelled = \labels sid sess -> with $ \c -> storeLabelled c labels sid sess >> stored c sid
            , sbLoadMeta = \sid -> with $ \c -> loadMeta c sid
            , sbCompareAndStore = \meta sess -> with $ \c -> do
                result <- compareAndStore c meta sess
                when (either (const False) (const True) result) $ stored c meta.smSessionId
                pure result
            , sbQuery = \q -> with $ \c -> querySessions c q
            }

sessionIdText :: SessionId -> Text
sessionIdText (SessionId uuid) = UUID.toText uuid

encodeJson :: (Aeson.ToJSON a) => a -> Text
encodeJson = TextEnc.decodeUtf8 . LByteString.toStrict . Aeson.encode

decodeJson :: (Aeson.FromJSON a) => Text -> Maybe a
decodeJson = Aeson.decodeStrict' . TextEnc.encodeUtf8

-- | Postgres keeps microseconds; so do the times this backend hands out.
nowMicros :: IO UTCTime
nowMicros = do
    t <- getCurrentTime
    pure t{utctDayTime = fromInteger (floor (utctDayTime t * 1000000)) / 1000000}

metaColumns :: Query
metaColumns = "session_id, agent_slug, parent_session_id, owner, status, status_detail, version, created_at, updated_at, params, security"

type MetaRow = (Text, Maybe Text, Maybe Text, Maybe Text, Text) :. (Maybe Text, Int, UTCTime, UTCTime, Text, Text)

metaFromRow :: MetaRow -> Maybe SessionMeta
metaFromRow ((sid, agent, parent, owner, status) :. (detail, version, created, updated, params, security)) = do
    uuid <- UUID.fromText sid
    pure
        SessionMeta
            { smSessionId = SessionId uuid
            , smAgent = agent
            , smParent = SessionId <$> (UUID.fromText =<< parent)
            , smOwner = owner
            , smStatus = fromMaybe StatusReady (parseSessionStatus status)
            , smStatusDetail = detail
            , smVersion = version
            , smCreatedAt = created
            , smUpdatedAt = updated
            , smParams = fromMaybe mempty (decodeJson params)
            , smSecurity = decodeSecurity security
            }

storeLabelled :: Connection -> SessionLabels -> SessionId -> Session -> IO ()
storeLabelled conn labels sid sess = do
    now <- nowMicros
    void $
        execute
            conn
            "INSERT INTO sessions\
            \ (session_id, created_at, updated_at, json, agent_slug, parent_session_id, owner, status, version)\
            \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1)\
            \ ON CONFLICT (session_id) DO UPDATE SET\
            \ updated_at = excluded.updated_at,\
            \ json = excluded.json,\
            \ agent_slug = COALESCE(excluded.agent_slug, sessions.agent_slug),\
            \ parent_session_id = COALESCE(excluded.parent_session_id, sessions.parent_session_id),\
            \ owner = COALESCE(excluded.owner, sessions.owner),\
            \ status = CASE WHEN sessions.status = 'running' THEN sessions.status ELSE excluded.status END,\
            \ status_detail = CASE WHEN sessions.status = 'running' THEN sessions.status_detail ELSE NULL END,\
            \ version = sessions.version + 1"
            ( (sessionIdText sid, now, now, encodeJson sess, labels.slAgent)
                :. (sessionIdText <$> labels.slParent, labels.slOwner, sessionStatusText (sessionStatusOf sess))
            )

loadMeta :: Connection -> SessionId -> IO (Maybe (Session, SessionMeta))
loadMeta conn sid = do
    rows <-
        query conn ("SELECT json, " <> metaColumns <> " FROM sessions WHERE session_id = ?") (Only (sessionIdText sid)) ::
            IO [Only Text :. MetaRow]
    pure $ case rows of
        [Only json :. row] -> (,) <$> decodeJson json <*> metaFromRow row
        _ -> Nothing

compareAndStore :: Connection -> SessionMeta -> Session -> IO (Either VersionConflict SessionMeta)
compareAndStore conn meta sess = do
    now <- nowMicros
    let sid = sessionIdText meta.smSessionId
        columns =
            (now, encodeJson sess, meta.smAgent, sessionIdText <$> meta.smParent)
                :. (meta.smOwner, sessionStatusText meta.smStatus, meta.smStatusDetail, encodeJson meta.smParams, encodeSecurity meta.smSecurity)
    rows <-
        if meta.smVersion == 0
            then
                query
                    conn
                    "INSERT INTO sessions\
                    \ (session_id, created_at, updated_at, json, agent_slug, parent_session_id, owner, status, status_detail, version, params, security)\
                    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?, ?)\
                    \ ON CONFLICT (session_id) DO UPDATE SET\
                    \ updated_at = excluded.updated_at,\
                    \ json = excluded.json,\
                    \ agent_slug = excluded.agent_slug,\
                    \ parent_session_id = excluded.parent_session_id,\
                    \ owner = excluded.owner,\
                    \ status = excluded.status,\
                    \ status_detail = excluded.status_detail,\
                    \ version = sessions.version + 1,\
                    \ params = excluded.params,\
                    \ security = excluded.security\
                    \ WHERE sessions.version = 0\
                    \ RETURNING version, created_at"
                    ((sid, now) :. columns)
            else
                query
                    conn
                    "UPDATE sessions SET\
                    \ updated_at = ?, json = ?, agent_slug = ?, parent_session_id = ?,\
                    \ owner = ?, status = ?, status_detail = ?, version = version + 1, params = ?, security = ?\
                    \ WHERE session_id = ? AND version = ?\
                    \ RETURNING version, created_at"
                    (columns :. (sid, meta.smVersion))
    case rows of
        [(version, created)] ->
            pure $ Right meta{smVersion = version, smCreatedAt = created, smUpdatedAt = now}
        _ -> do
            current <- query conn "SELECT version FROM sessions WHERE session_id = ?" (Only sid) :: IO [Only Int]
            pure $ Left $ VersionConflict meta.smSessionId meta.smVersion (maybe 0 fromOnly (listToMaybe current))

querySessions :: Connection -> SessionQuery -> IO [SessionMeta]
querySessions conn q
    | q.sqStatuses == Just [] = pure []
    | otherwise = do
        let criteria :: [(Text, [Action])]
            criteria =
                catMaybes
                    [ (\a -> ("agent_slug = ?", [toField a])) <$> q.sqAgent
                    , (\ss -> ("status IN (" <> Text.intercalate ", " ("?" <$ ss) <> ")", map (toField . sessionStatusText) ss)) <$> q.sqStatuses
                    , (\p -> ("parent_session_id = ?", [toField (sessionIdText p)])) <$> q.sqParent
                    , (\o -> ("owner = ?", [toField o])) <$> q.sqOwner
                    , (\t -> ("updated_at < ?", [toField t])) <$> q.sqUpdatedBefore
                    ]
            whereClause
                | null criteria = ""
                | otherwise = " WHERE " <> Text.intercalate " AND " (map fst criteria)
            (limitClause, limitArgs) = case q.sqLimit of
                Just n -> (" LIMIT ?", [toField n])
                Nothing -> ("", [])
            statement =
                "SELECT " <> metaColumns <> " FROM sessions" <> fromString (Text.unpack (whereClause <> " ORDER BY updated_at DESC" <> limitClause))
        rows <- query conn statement (concatMap snd criteria <> limitArgs) :: IO [MetaRow]
        pure $ mapMaybe metaFromRow rows

listSessions :: Connection -> IO [(SessionId, UTCTime)]
listSessions conn = do
    rows <- query_ conn "SELECT session_id, updated_at FROM sessions" :: IO [(Text, UTCTime)]
    pure [(SessionId uuid, t) | (sid, t) <- rows, Just uuid <- [UUID.fromText sid]]

-------------------------------------------------------------------------------
-- Continuations
-------------------------------------------------------------------------------

-- | A continuation store on the pool, after migrating its table.
mkPostgresContinuationStore :: Pool Connection -> IO ContinuationStore
mkPostgresContinuationStore pool = do
    runPostgresMigrations pool "continuations" continuationMigrations
    let with = withResource pool
    pure
        ContinuationStore
            { csStore = \snap -> with $ \c -> storeContinuation c snap
            , csLoad = \token -> with $ \c -> loadContinuation c token
            , csFindSession = \token -> with $ \c -> findSession c token
            , csComplete = \token result -> with $ \c -> completeContinuation c token result
            , csDelete = \token -> with $ \c -> void $ execute c "DELETE FROM tool_continuations WHERE token = ?" (Only (tokenText token))
            , csListPending = \sid -> with $ \c -> listPending c sid
            , csCleanupExpired = with $ \c -> do
                now <- getCurrentTime
                void $ execute c "DELETE FROM tool_continuations WHERE expires_at IS NOT NULL AND expires_at < ?" (Only now)
            , csCountSession = \sid -> with $ \c -> do
                rows <- query c "SELECT COUNT(*)::int FROM tool_continuations WHERE session_id = ?" (Only (sessionIdText sid)) :: IO [Only Int]
                pure $ maybe 0 fromOnly (listToMaybe rows)
            , csDeleteSession = \sid -> with $ \c -> fromIntegral <$> execute c "DELETE FROM tool_continuations WHERE session_id = ?" (Only (sessionIdText sid))
            }

tokenText :: ContinuationToken -> Text
tokenText (ContinuationToken uuid) = UUID.toText uuid

-- | The whole snapshot goes in @context_json@, as in the SQLite store.
storeContinuation :: Connection -> ToolContinuationSnapshot -> IO ()
storeContinuation conn snap =
    void $
        execute
            conn
            "INSERT INTO tool_continuations\
            \ (token, session_id, tool_call_json, context_json, created_at, expires_at)\
            \ VALUES (?, ?, ?, ?, ?, ?)\
            \ ON CONFLICT (token) DO UPDATE SET\
            \ session_id = excluded.session_id,\
            \ tool_call_json = excluded.tool_call_json,\
            \ context_json = excluded.context_json,\
            \ expires_at = excluded.expires_at"
            (tokenText snap.tcsToken, sessionIdText snap.tcsSessionId, encodeJson snap.tcsToolCall, encodeJson snap, snap.tcsCreatedAt, snap.tcsExpiresAt)

loadContinuation :: Connection -> ContinuationToken -> IO (Maybe ToolContinuationSnapshot)
loadContinuation conn token = do
    rows <-
        query conn "SELECT context_json FROM tool_continuations WHERE token = ? AND completed_at IS NULL" (Only (tokenText token)) ::
            IO [Only Text]
    pure $ case rows of
        [Only json] -> decodeJson json
        _ -> Nothing

completeContinuation :: Connection -> ContinuationToken -> UserToolResponse -> IO Bool
completeContinuation conn token result = do
    now <- getCurrentTime
    rows <-
        query
            conn
            "UPDATE tool_continuations SET completed_at = ?, result_json = ?\
            \ WHERE token = ? AND completed_at IS NULL\
            \ RETURNING token"
            (now, encodeJson result, tokenText token) ::
            IO [Only Text]
    pure $ not (null rows)

findSession :: Connection -> ContinuationToken -> IO (Maybe SessionId)
findSession conn token = do
    rows <- query conn "SELECT session_id FROM tool_continuations WHERE token = ?" (Only (tokenText token)) :: IO [Only Text]
    pure $ case rows of
        [Only sid] -> SessionId <$> UUID.fromText sid
        _ -> Nothing

listPending :: Connection -> SessionId -> IO [(ContinuationToken, ToolContinuationSnapshot)]
listPending conn sid = do
    rows <-
        query conn "SELECT token, context_json FROM tool_continuations WHERE session_id = ? AND completed_at IS NULL" (Only (sessionIdText sid)) ::
            IO [(Text, Text)]
    pure [(ContinuationToken uuid, snap) | (t, json) <- rows, Just uuid <- [UUID.fromText t], Just snap <- [decodeJson json]]

-------------------------------------------------------------------------------
-- Mail (@todos/session-mailbox.md@, Phase 3)
-------------------------------------------------------------------------------

mailMigrations :: [PgMigration]
mailMigrations =
    [ PgMigration 1 $
        statements
            [ "CREATE TABLE IF NOT EXISTS session_mail (\
              \ session_id TEXT NOT NULL,\
              \ seq INTEGER NOT NULL,\
              \ id TEXT NOT NULL,\
              \ body_json TEXT NOT NULL,\
              \ accepted_at TIMESTAMPTZ NOT NULL DEFAULT now(),\
              \ PRIMARY KEY (session_id, id))"
            , "CREATE INDEX IF NOT EXISTS idx_session_mail_session_seq ON session_mail(session_id, seq)"
            ]
    ]

-- | A durable 'MailStore' on the pool, after migrating its table. Mirrors
-- "System.Agents.Session.MailStore"'s SQLite implementation: one row per
-- envelope, the envelope itself serialized whole into @body_json@,
-- idempotent on @(session_id, id)@. It signals nothing; see
-- 'mkPostgresMailStoreAs'.
mkPostgresMailStore :: Pool Connection -> IO MailStore
mkPostgresMailStore = mkPostgresMailStoreAs silentInstance

{- | Like 'mkPostgresMailStore', signalling each accepted envelope to the
other processes on the database, as coming from the given instance.
-}
mkPostgresMailStoreAs :: PgInstance -> Pool Connection -> IO MailStore
mkPostgresMailStoreAs me pool = do
    runPostgresMigrations pool "session_mail" mailMigrations
    let with = withResource pool
    pure
        MailStore
            { msAppend = \sid envelope -> with $ \c -> do
                stored <- appendMail c sid envelope
                signal c me MailAccepted sid
                pure stored
            , msLoad = \sid -> with $ \c -> loadMail c sid
            }

{- | Append an envelope, and answer with it as stored.

Several processes may append to one session, each numbering from what it
last loaded. So the sequence number is settled here, under a lock on the
session's mail held for the transaction: the envelope keeps its number when
that is past every stored one, and takes the next free one otherwise. An id
that is already stored answers with the envelope stored under it.
-}
appendMail :: Connection -> SessionId -> Envelope -> IO Envelope
appendMail conn sid envelope = withTransaction conn $ do
    _ <- query conn "SELECT pg_advisory_xact_lock(hashtextextended(?, 7236871069110195829))::text" (Only (sessionIdText sid)) :: IO [Only Text]
    existing <-
        query conn "SELECT body_json FROM session_mail WHERE session_id = ? AND id = ?" (sessionIdText sid, messageIdText envelope.envId) ::
            IO [Only Text]
    case mapMaybe (decodeJson . fromOnly) existing of
        (found : _) -> pure found
        [] -> do
            latest <- query conn "SELECT COALESCE(MAX(seq), 0) FROM session_mail WHERE session_id = ?" (Only (sessionIdText sid)) :: IO [Only Int]
            let stored = envelope{envSeq = max envelope.envSeq (1 + maybe 0 fromOnly (listToMaybe latest))}
            void $
                execute
                    conn
                    "INSERT INTO session_mail (session_id, seq, id, body_json)\
                    \ VALUES (?, ?, ?, ?)\
                    \ ON CONFLICT (session_id, id) DO NOTHING"
                    (sessionIdText sid, stored.envSeq, messageIdText stored.envId, encodeJson stored)
            pure stored

loadMail :: Connection -> SessionId -> IO [Envelope]
loadMail conn sid = do
    rows <-
        query conn "SELECT body_json FROM session_mail WHERE session_id = ? ORDER BY seq ASC" (Only (sessionIdText sid)) ::
            IO [Only Text]
    pure $ mapMaybe (decodeJson . fromOnly) rows

-------------------------------------------------------------------------------
-- Watches (@todos/os-as-standalone-server.md@ G11)
-------------------------------------------------------------------------------

watchMigrations :: [PgMigration]
watchMigrations =
    [ PgMigration 1 $
        statements
            [ "CREATE TABLE IF NOT EXISTS session_watches (\
              \ watch_id TEXT PRIMARY KEY,\
              \ watcher_session_id TEXT NOT NULL,\
              \ request_json TEXT NOT NULL,\
              \ deadline TIMESTAMPTZ NOT NULL,\
              \ created_at TIMESTAMPTZ NOT NULL DEFAULT now())"
            , "CREATE INDEX IF NOT EXISTS idx_session_watches_watcher ON session_watches(watcher_session_id)"
            ]
    ]

-- | A durable 'WatchStore' on the pool, after migrating its table. Mirrors
-- "System.Agents.Session.WatchStore"'s SQLite implementation, reusing its
-- 'WatchRequest' JSON shape ('encodeWatchRequest'\/'decodeWatchRequest') so
-- the two backends agree on the wire format.
mkPostgresWatchStore :: Pool Connection -> IO WatchStore
mkPostgresWatchStore pool = do
    runPostgresMigrations pool "session_watches" watchMigrations
    let with = withResource pool
    pure
        WatchStore
            { wsSave = \pw -> with $ \c -> saveWatch c pw
            , wsDelete = \watchId -> with $ \c -> void $ execute c "DELETE FROM session_watches WHERE watch_id = ?" (Only watchId)
            , wsLoadAll = with loadAllWatches
            }

saveWatch :: Connection -> PersistedWatch -> IO ()
saveWatch conn pw =
    void $
        execute
            conn
            "INSERT INTO session_watches (watch_id, watcher_session_id, request_json, deadline)\
            \ VALUES (?, ?, ?, ?)\
            \ ON CONFLICT (watch_id) DO UPDATE SET\
            \ watcher_session_id = excluded.watcher_session_id,\
            \ request_json = excluded.request_json,\
            \ deadline = excluded.deadline"
            (pw.pwWatchId, sessionIdText pw.pwWatcher, encodeJson (encodeWatchRequest pw.pwRequest), pw.pwDeadline)

loadAllWatches :: Connection -> IO [PersistedWatch]
loadAllWatches conn = do
    rows <-
        query_ conn "SELECT watch_id, watcher_session_id, request_json, deadline FROM session_watches" ::
            IO [(Text, Text, Text, UTCTime)]
    pure $ mapMaybe decodeRow rows
  where
    decodeRow (watchId, watcherText, bodyJson, deadline) = do
        watcherUuid <- UUID.fromText watcherText
        value <- decodeJson bodyJson :: Maybe Aeson.Value
        req <- either (const Nothing) Just (parseEither decodeWatchRequest value)
        pure $ PersistedWatch watchId (SessionId watcherUuid) req deadline

-------------------------------------------------------------------------------
-- Agents
-------------------------------------------------------------------------------

agentMigrations :: [PgMigration]
agentMigrations =
    [ PgMigration 1 $
        statements
            [ "CREATE TABLE IF NOT EXISTS agents (\
              \ slug TEXT PRIMARY KEY,\
              \ json TEXT NOT NULL,\
              \ updated_at TIMESTAMPTZ NOT NULL,\
              \ updated_by TEXT)"
            ]
    ]

-- | An agent store on the pool, after migrating its table.
mkPostgresAgentStore :: Pool Connection -> IO AgentStore
mkPostgresAgentStore pool = do
    runPostgresMigrations pool "agents" agentMigrations
    let with = withResource pool
    pure
        AgentStore
            { asList = with $ \c -> do
                rows <- query_ c "SELECT json, updated_at, updated_by FROM agents ORDER BY slug" :: IO [(Text, UTCTime, Maybe Text)]
                pure [StoredAgent agent updated by | (json, updated, by) <- rows, Just agent <- [decodeJson json]]
            , asPut = \by agent -> with $ \c -> do
                now <- nowMicros
                void $
                    execute
                        c
                        "INSERT INTO agents (slug, json, updated_at, updated_by) VALUES (?, ?, ?, ?)\
                        \ ON CONFLICT (slug) DO UPDATE SET\
                        \ json = excluded.json, updated_at = excluded.updated_at, updated_by = excluded.updated_by"
                        (Base.slug agent, encodeJson agent, now, by)
                pure $ StoredAgent agent now by
            , asDelete = \slug -> with $ \c -> (> 0) <$> execute c "DELETE FROM agents WHERE slug = ?" (Only slug)
            }

-------------------------------------------------------------------------------
-- Several processes on one database
-------------------------------------------------------------------------------

{- | The name of one process among those sharing a database: the owner it
writes in @run_owner@, and the origin of the signals it sends, which is how
a process recognises (and skips) its own.
-}
newtype PgInstance = PgInstance {pgInstanceName :: Text}
    deriving (Show, Eq)

-- | A fresh, unique instance name.
newPgInstance :: IO PgInstance
newPgInstance = PgInstance . UUID.toText <$> UUID.nextRandom

-- | The instance of a store that signals nothing.
silentInstance :: PgInstance
silentInstance = PgInstance ""

-- | The channel every process on the database listens to.
signalChannel :: Text
signalChannel = "agents_sessions"

signalName :: SessionSignal -> Text
signalName = \case
    MailAccepted -> "mail"
    SessionStored -> "stored"

-- | Tell the other processes about a session. The payload is @origin kind session@.
signal :: Connection -> PgInstance -> SessionSignal -> SessionId -> IO ()
signal conn me kind sid =
    unless (me == silentInstance) $ do
        let payload = Text.unwords [me.pgInstanceName, signalName kind, sessionIdText sid]
        _ <- query conn "SELECT pg_notify(?, ?)::text" (signalChannel, payload) :: IO [Only Text]
        pure ()

parseSignal :: ByteString -> Maybe (PgInstance, SessionSignal, SessionId)
parseSignal payload = case Text.words (TextEnc.decodeUtf8Lenient payload) of
    [origin, kind, sid] -> do
        uuid <- UUID.fromText sid
        parsed <- case kind of
            "mail" -> Just MailAccepted
            "stored" -> Just SessionStored
            _ -> Nothing
        pure (PgInstance origin, parsed, SessionId uuid)
    _ -> Nothing

-- | The handlers called for each signal from another process.
newtype PgListener = PgListener (TVar (Map.Map Int (SessionId -> SessionSignal -> IO ())))

{- | Listen to the other processes' signals for as long as the action runs,
on a connection of its own. A connection that breaks is opened again after a
second; the signals sent in between are lost, which is why a runner also
looks at the store on a timer.

Waits (at most five seconds) for the first @LISTEN@, so that a signal sent
right after this returns is heard.
-}
withPgListener :: ByteString -> PgInstance -> (PgListener -> IO a) -> IO a
withPgListener url me action = do
    handlers <- newTVarIO Map.empty
    listening <- newEmptyMVar
    let dispatch :: Notification -> IO ()
        dispatch notification =
            forM_ (parseSignal notification.notificationData) $ \(origin, kind, sid) ->
                unless (origin == me) $ do
                    current <- readTVarIO handlers
                    forM_ (Map.elems current) $ \handler -> handler sid kind
        session = bracket (connectPostgreSQL url) close $ \conn -> do
            void $ execute_ conn (fromString ("LISTEN " <> Text.unpack signalChannel))
            void $ tryPutMVar listening ()
            forever $ getNotification conn >>= dispatch
        loop =
            forever $
                try session >>= \case
                    Left (e :: SomeException)
                        | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
                        | otherwise -> threadDelay 1000000
                    Right () -> pure ()
    withAsync loop $ \_ -> do
        _ <- timeout 5000000 (readMVar listening)
        action (PgListener handlers)

{- | Run leases on the @sessions@ table, and the listener's signals.

Every time is the database's (@now()@), so the processes need not agree on
the clock.
-}
mkPostgresCoordination :: PgInstance -> Pool Connection -> PgListener -> Coordination
mkPostgresCoordination me pool (PgListener handlers) =
    Coordination
        { coEnabled = True
        , coInstance = owner
        , coAcquire = \sid ttl -> with $ \c -> do
            taken <-
                query
                    c
                    "UPDATE sessions SET run_owner = ?, run_lease_until = now() + (? * interval '1 second')\
                    \ WHERE session_id = ?\
                    \ AND (run_owner IS NULL OR run_owner = ? OR run_lease_until IS NULL OR run_lease_until < now())\
                    \ RETURNING session_id"
                    (owner, seconds ttl, sessionIdText sid, owner) ::
                    IO [Only Text]
            if not (null taken)
                then pure LeaseAcquired
                else do
                    holder <- query c "SELECT run_owner FROM sessions WHERE session_id = ?" (Only (sessionIdText sid)) :: IO [Only (Maybe Text)]
                    pure $ case holder of
                        [] -> LeaseNoSession
                        (Only by : _) -> LeaseHeldBy (fromMaybe "" by)
        , coRenew = \sids ttl ->
            if null sids
                then pure []
                else with $ \c -> do
                    rows <-
                        query
                            c
                            "UPDATE sessions SET run_lease_until = now() + (? * interval '1 second')\
                            \ WHERE run_owner = ? AND session_id IN ?\
                            \ RETURNING session_id"
                            (seconds ttl, owner, In (map sessionIdText sids)) ::
                            IO [Only Text]
                    pure (sessionIds rows)
        , coRelease = \sid -> with $ \c ->
            void $
                execute
                    c
                    "UPDATE sessions SET run_owner = NULL, run_lease_until = NULL WHERE session_id = ? AND run_owner = ?"
                    (sessionIdText sid, owner)
        , coHolder = \sid -> with $ \c -> do
            rows <-
                query
                    c
                    "SELECT run_owner FROM sessions\
                    \ WHERE session_id = ? AND run_owner IS NOT NULL AND run_owner <> ? AND run_lease_until >= now()"
                    (sessionIdText sid, owner) ::
                    IO [Only Text]
            pure (fromOnly <$> listToMaybe rows)
        , coExpired = with $ \c ->
            sessionIds
                <$> query
                    c
                    "SELECT session_id FROM sessions\
                    \ WHERE status = 'running' AND run_owner IS NOT NULL AND run_owner <> ? AND run_lease_until < now()"
                    (Only owner)
        , coAbandoned = with $ \c ->
            sessionIds
                <$> query_
                    c
                    "SELECT session_id FROM sessions\
                    \ WHERE status = 'running' AND (run_owner IS NULL OR run_lease_until IS NULL OR run_lease_until < now())"
        , coListen = \handler -> do
            key <- atomically $ stateTVar handlers $ \current ->
                let k = maybe 0 ((+ 1) . fst) (Map.lookupMax current) in (k, Map.insert k handler current)
            pure $ atomically $ modifyTVar' handlers (Map.delete key)
        }
  where
    with :: (Connection -> IO a) -> IO a
    with = withResource pool
    owner = me.pgInstanceName
    seconds :: NominalDiffTime -> Double
    seconds = realToFrac
    sessionIds :: [Only Text] -> [SessionId]
    sessionIds rows = [SessionId uuid | Only sid <- rows, Just uuid <- [UUID.fromText sid]]
