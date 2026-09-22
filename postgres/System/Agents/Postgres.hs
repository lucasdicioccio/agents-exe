{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | Sessions and continuations in Postgres.

The schema and the semantics match the SQLite stores in @agents-lib@:
versioned compare-and-store, metadata columns, and component-scoped
migrations in @schema_migrations@. Connections come from a pool, so the
stores can be used from many threads.

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
    mkPostgresAgentStore,
    isPostgresUrl,

    -- * Migrations
    PgMigration (..),
    runPostgresMigrations,
) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime (..), getCurrentTime)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (
    Connection,
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
import Database.PostgreSQL.Simple.ToField (Action, ToField (..))

import System.Agents.AgentStore (AgentStore (..), StoredAgent (..))
import qualified System.Agents.Base as Base
import System.Agents.Host (HostStores (..))
import System.Agents.Session.Async (ContinuationStore (..), ContinuationToken (..), ToolContinuationSnapshot (..))
import System.Agents.Session.Base (Envelope (..), Session, SessionId (..), SessionStatus (..), UserToolResponse, messageIdText, parseSessionStatus, sessionStatusOf, sessionStatusText)
import System.Agents.Session.Mailbox (MailStore (..))
import System.Agents.SessionStore (
    SessionBackend (..),
    SessionLabels (..),
    SessionMeta (..),
    SessionQuery (..),
    VersionConflict (..),
    noLabels,
 )

-------------------------------------------------------------------------------
-- Stores
-------------------------------------------------------------------------------

{- | Open a pool on a connection string (a @postgresql://@ URL or
@key=value@ pairs), migrate, and run the action with the two stores.
-}
withPostgresStores :: ByteString -> (HostStores -> IO a) -> IO a
withPostgresStores url action =
    bracket (openPostgresPool url 10) destroyAllResources $ \pool -> do
        sessions <- mkPostgresSessionStore pool
        continuations <- mkPostgresContinuationStore pool
        mail <- mkPostgresMailStore pool
        agents <- mkPostgresAgentStore pool
        action (HostStores sessions continuations mail (Just agents))

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

-- | A session backend on the pool, after migrating its table.
mkPostgresSessionStore :: Pool Connection -> IO SessionBackend
mkPostgresSessionStore pool = do
    runPostgresMigrations pool "sessions" sessionMigrations
    let with = withResource pool
    pure
        SessionBackend
            { sbStore = \sid sess -> with $ \c -> storeLabelled c noLabels sid sess
            , sbLoad = \sid -> fmap fst <$> with (\c -> loadMeta c sid)
            , sbList = with listSessions
            , sbDelete = \sid -> with $ \c -> void $ execute c "DELETE FROM sessions WHERE session_id = ?" (Only (sessionIdText sid))
            , sbStoreLabelled = \labels sid sess -> with $ \c -> storeLabelled c labels sid sess
            , sbLoadMeta = \sid -> with $ \c -> loadMeta c sid
            , sbCompareAndStore = \meta sess -> with $ \c -> compareAndStore c meta sess
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
metaColumns = "session_id, agent_slug, parent_session_id, owner, status, status_detail, version, created_at, updated_at, params"

type MetaRow = (Text, Maybe Text, Maybe Text, Maybe Text, Text) :. (Maybe Text, Int, UTCTime, UTCTime, Text)

metaFromRow :: MetaRow -> Maybe SessionMeta
metaFromRow ((sid, agent, parent, owner, status) :. (detail, version, created, updated, params)) = do
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
                :. (meta.smOwner, sessionStatusText meta.smStatus, meta.smStatusDetail, encodeJson meta.smParams)
    rows <-
        if meta.smVersion == 0
            then
                query
                    conn
                    "INSERT INTO sessions\
                    \ (session_id, created_at, updated_at, json, agent_slug, parent_session_id, owner, status, status_detail, version, params)\
                    \ VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1, ?)\
                    \ ON CONFLICT (session_id) DO UPDATE SET\
                    \ updated_at = excluded.updated_at,\
                    \ json = excluded.json,\
                    \ agent_slug = excluded.agent_slug,\
                    \ parent_session_id = excluded.parent_session_id,\
                    \ owner = excluded.owner,\
                    \ status = excluded.status,\
                    \ status_detail = excluded.status_detail,\
                    \ version = sessions.version + 1,\
                    \ params = excluded.params\
                    \ WHERE sessions.version = 0\
                    \ RETURNING version, created_at"
                    ((sid, now) :. columns)
            else
                query
                    conn
                    "UPDATE sessions SET\
                    \ updated_at = ?, json = ?, agent_slug = ?, parent_session_id = ?,\
                    \ owner = ?, status = ?, status_detail = ?, version = version + 1, params = ?\
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
-- idempotent on @(session_id, id)@.
mkPostgresMailStore :: Pool Connection -> IO MailStore
mkPostgresMailStore pool = do
    runPostgresMigrations pool "session_mail" mailMigrations
    let with = withResource pool
    pure
        MailStore
            { msAppend = \sid envelope -> with $ \c -> appendMail c sid envelope
            , msLoad = \sid -> with $ \c -> loadMail c sid
            }

appendMail :: Connection -> SessionId -> Envelope -> IO ()
appendMail conn sid envelope =
    void $
        execute
            conn
            "INSERT INTO session_mail (session_id, seq, id, body_json)\
            \ VALUES (?, ?, ?, ?)\
            \ ON CONFLICT (session_id, id) DO NOTHING"
            (sessionIdText sid, envelope.envSeq, messageIdText envelope.envId, encodeJson envelope)

loadMail :: Connection -> SessionId -> IO [Envelope]
loadMail conn sid = do
    rows <-
        query conn "SELECT body_json FROM session_mail WHERE session_id = ? ORDER BY seq ASC" (Only (sessionIdText sid)) ::
            IO [Only Text]
    pure $ mapMaybe (decodeJson . fromOnly) rows

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
