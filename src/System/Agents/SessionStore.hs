{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{- | Session storage management with multi-location support.

This module provides functionality to store and list sessions
using configurable file prefixes. Sessions can be read from multiple
locations while always writing to a single designated location.

The file naming pattern is @conv.<uuid>.json@.

Multi-location support allows:
- Reading sessions from multiple directories (e.g., project-local, global)
- Writing all new sessions to a single unified location
- Deduplication by ConversationId with priority based on location order

In addition to the legacy file-based 'SessionStore', this module defines a
pluggable 'SessionBackend' interface with SQLite and composite fallback
implementations for durable workflow support.
-}
module System.Agents.SessionStore (
    -- * Session Backend Interface
    SessionBackend (..),
    sessionIdToConversationId,
    conversationIdToSessionId,

    -- * Session Metadata
    SessionLabels (..),
    noLabels,
    SessionMeta (..),
    freshSessionMeta,
    SessionQuery (..),
    allSessionsQuery,
    matchesQuery,
    VersionConflict (..),

    -- * Session Catalog
    SessionCatalog (..),
    CatalogEntry (..),
    fileCatalog,
    backendCatalog,
    isFileBusy,

    -- * File Backend
    FileSessionStore (..),
    mkFileSessionStore,
    fileSessionBackend,

    -- * SQLite Backend
    SqliteSessionStore (..),
    mkSqliteSessionStore,
    initializeSessionSchema,
    Migration (..),
    runMigrations,

    -- * Composite Backend
    CompositeSessionStore (..),
    mkCompositeSessionStore,

    -- * Legacy Session Store
    SessionStore (..),
    defaultSessionStore,
    mkSessionStore,
    mkSimpleSessionStore,

    -- * Path resolution
    resolveSessionPath,

    -- * Low-level operations, mostly to implement command-line bypasses
    readSessionFromFile,
    storeSessionToFile,

    -- * File path operations
    sessionFilePath,
    sessionWritePath,

    -- * Session storage operations
    storeSession,
    readSession,

    -- * Listing sessions
    listSessions,
    SessionFileInfo (..),
    findSessionFiles,
    isSessionFile,
) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, bracket, catch, try)
import Control.Monad (filterM, forM, forM_, unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (foldl', isInfixOf, isPrefixOf, sortOn)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, execute_, query, query_, withTransaction, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (toField)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, getModificationTime, listDirectory, removeFile)
import System.FilePath (takeFileName, (</>))
import System.IO (IOMode (..), hClose, openBinaryFile)
import System.IO.Error (ioeGetErrorString)

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Types (
    Session (..),
    SessionId (..),
    SessionStatus (..),
    parseSessionStatus,
    sessionStatusOf,
    sessionStatusText,
 )

-------------------------------------------------------------------------------
-- Session Backend Interface
-------------------------------------------------------------------------------

{- | Abstract backend interface for durable session storage.

A 'SessionBackend' hides the details of where and how sessions are persisted,
allowing agents to store sessions in files, SQLite, or a composite of multiple
backends without changing the caller.

Every stored session carries a 'SessionMeta'. Each write increments its
version: 'sbCompareAndStore' only writes when the version is the one the
caller loaded, so concurrent writers detect each other.
-}
data SessionBackend = SessionBackend
    { sbStore :: SessionId -> Session -> IO ()
    {- ^ Persist a session under the given session id, unconditionally. The
    status is refreshed from 'sessionStatusOf', unless it is 'StatusRunning'.
    -}
    , sbLoad :: SessionId -> IO (Maybe Session)
    -- ^ Load a session by id. Returns 'Nothing' if not found or unreadable.
    , sbList :: IO [(SessionId, UTCTime)]
    -- ^ List all stored sessions with their last update time.
    , sbDelete :: SessionId -> IO ()
    -- ^ Delete a session by id.
    , sbStoreLabelled :: SessionLabels -> SessionId -> Session -> IO ()
    -- ^ Like 'sbStore', also recording the given labels.
    , sbLoadMeta :: SessionId -> IO (Maybe (Session, SessionMeta))
    -- ^ Load a session with its metadata.
    , sbCompareAndStore :: SessionMeta -> Session -> IO (Either VersionConflict SessionMeta)
    {- ^ Store the session with exactly the given metadata, if the stored
    version still equals 'smVersion' (0 when the session was never stored).
    Returns the metadata as written, with the incremented version.
    -}
    , sbQuery :: SessionQuery -> IO [SessionMeta]
    -- ^ Metadata of matching sessions, most recently updated first.
    }

-- | Convert a 'SessionId' to a 'ConversationId'.
sessionIdToConversationId :: SessionId -> ConversationId
sessionIdToConversationId (SessionId uuid) = ConversationId uuid

-- | Convert a 'ConversationId' to a 'SessionId'.
conversationIdToSessionId :: ConversationId -> SessionId
conversationIdToSessionId (ConversationId uuid) = SessionId uuid

-------------------------------------------------------------------------------
-- Session Metadata
-------------------------------------------------------------------------------

-- | Labels recorded next to a session. 'Nothing' keeps the stored value.
data SessionLabels = SessionLabels
    { slAgent :: Maybe Text
    -- ^ Slug of the agent running the session.
    , slParent :: Maybe SessionId
    -- ^ Session of the agent that called this one as a tool.
    , slOwner :: Maybe Text
    -- ^ Reserved for multi-tenancy.
    }
    deriving (Show, Eq)

noLabels :: SessionLabels
noLabels = SessionLabels Nothing Nothing Nothing

-- | What a backend records next to each session.
data SessionMeta = SessionMeta
    { smSessionId :: SessionId
    , smAgent :: Maybe Text
    , smParent :: Maybe SessionId
    , smOwner :: Maybe Text
    , smStatus :: SessionStatus
    , smStatusDetail :: Maybe Text
    -- ^ Why the session failed, for 'StatusFailed'.
    , smVersion :: Int
    -- ^ Incremented on every write; 0 for a session never stored.
    , smCreatedAt :: UTCTime
    , smUpdatedAt :: UTCTime
    }
    deriving (Show, Eq)

instance Aeson.ToJSON SessionMeta where
    toJSON m =
        Aeson.object
            [ "session_id" Aeson..= m.smSessionId
            , "agent" Aeson..= m.smAgent
            , "parent_session_id" Aeson..= m.smParent
            , "owner" Aeson..= m.smOwner
            , "status" Aeson..= m.smStatus
            , "status_detail" Aeson..= m.smStatusDetail
            , "version" Aeson..= m.smVersion
            , "created_at" Aeson..= m.smCreatedAt
            , "updated_at" Aeson..= m.smUpdatedAt
            ]

instance Aeson.FromJSON SessionMeta where
    parseJSON = Aeson.withObject "SessionMeta" $ \v ->
        SessionMeta
            <$> v Aeson..: "session_id"
            <*> v Aeson..:? "agent"
            <*> v Aeson..:? "parent_session_id"
            <*> v Aeson..:? "owner"
            <*> v Aeson..: "status"
            <*> v Aeson..:? "status_detail"
            <*> v Aeson..: "version"
            <*> v Aeson..: "created_at"
            <*> v Aeson..: "updated_at"

-- | Metadata for a session that was never stored (version 0).
freshSessionMeta :: SessionId -> UTCTime -> SessionMeta
freshSessionMeta sid now =
    SessionMeta
        { smSessionId = sid
        , smAgent = Nothing
        , smParent = Nothing
        , smOwner = Nothing
        , smStatus = StatusReady
        , smStatusDetail = Nothing
        , smVersion = 0
        , smCreatedAt = now
        , smUpdatedAt = now
        }

{- | The metadata an unconditional store ('sbStoreLabelled') writes, given the
stored metadata.
-}
storedMeta :: UTCTime -> SessionLabels -> Session -> SessionMeta -> SessionMeta
storedMeta now labels sess old =
    old
        { smAgent = labels.slAgent <|> old.smAgent
        , smParent = labels.slParent <|> old.smParent
        , smOwner = labels.slOwner <|> old.smOwner
        , smStatus = if running then StatusRunning else sessionStatusOf sess
        , smStatusDetail = if running then old.smStatusDetail else Nothing
        , smVersion = old.smVersion + 1
        , smUpdatedAt = now
        }
  where
    running = old.smStatus == StatusRunning

-- | Which sessions 'sbQuery' returns. Absent criteria match everything.
data SessionQuery = SessionQuery
    { sqAgent :: Maybe Text
    , sqStatuses :: Maybe [SessionStatus]
    , sqParent :: Maybe SessionId
    , sqUpdatedBefore :: Maybe UTCTime
    , sqLimit :: Maybe Int
    }
    deriving (Show, Eq)

-- | A query matching every session.
allSessionsQuery :: SessionQuery
allSessionsQuery = SessionQuery Nothing Nothing Nothing Nothing Nothing

-- | Whether metadata matches a query's criteria (ignoring its limit).
matchesQuery :: SessionQuery -> SessionMeta -> Bool
matchesQuery q m =
    maybe True (\a -> m.smAgent == Just a) q.sqAgent
        && maybe True (m.smStatus `elem`) q.sqStatuses
        && maybe True (\p -> m.smParent == Just p) q.sqParent
        && maybe True (m.smUpdatedAt <) q.sqUpdatedBefore

-- | Apply a query to metadata listed from a backend that cannot filter itself.
applyQuery :: SessionQuery -> [SessionMeta] -> [SessionMeta]
applyQuery q =
    maybe id take q.sqLimit . sortOn (Down . (.smUpdatedAt)) . filter (matchesQuery q)

-- | A compare-and-store found another version than the one expected.
data VersionConflict = VersionConflict
    { vcSessionId :: SessionId
    , vcExpected :: Int
    , vcActual :: Int
    }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- File Backend
-------------------------------------------------------------------------------

{- | File-backed session backend.

Stores sessions as JSON files in a single directory using the same
@conv.<uuid>.json@ naming scheme as the legacy 'SessionStore', and their
metadata in @meta.<uuid>.json@ next to them. A session file without metadata
(e.g. written by the legacy store) reads as version 0.

Compare-and-store is not atomic across processes: it reads, compares, then
writes. Use SQLite when several processes write the same sessions.
-}
newtype FileSessionStore = FileSessionStore FilePath
{- | Create a file-backed session backend, ensuring the directory exists.

Example:

> backend <- mkFileSessionStore "./sessions/"
-}
mkFileSessionStore :: FilePath -> IO SessionBackend
mkFileSessionStore path = do
    createDirectoryIfMissing True path
    pure $ fileSessionBackend (FileSessionStore path)

-- | Build a 'SessionBackend' from an existing 'FileSessionStore'.
fileSessionBackend :: FileSessionStore -> SessionBackend
fileSessionBackend (FileSessionStore path) =
    SessionBackend
        { sbStore = storeLabelled noLabels
        , sbLoad = \sid -> readSession simpleStore (sessionIdToConversationId sid)
        , sbList = do
            files <- findSessionFiles simpleStore
            pure $ map (\info -> (conversationIdToSessionId info.sessionInfoConversationId, info.sessionInfoModTime)) files
        , sbDelete = \sid -> do
            removeIfPresent $ sessionWritePath simpleStore (sessionIdToConversationId sid)
            removeIfPresent $ metaPath sid
        , sbStoreLabelled = storeLabelled
        , sbLoadMeta = loadMeta
        , sbCompareAndStore = \meta sess -> do
            current <- maybe 0 (.smVersion) <$> readMeta meta.smSessionId
            if current /= meta.smVersion
                then pure $ Left $ VersionConflict meta.smSessionId meta.smVersion current
                else do
                    now <- getCurrentTime
                    let written = meta{smVersion = current + 1, smUpdatedAt = now}
                    write written sess
                    pure $ Right written
        , sbQuery = \q -> do
            files <- findSessionFiles simpleStore
            metas <- mapM (fmap (fmap snd) . loadMeta . conversationIdToSessionId . (.sessionInfoConversationId)) files
            pure $ applyQuery q (catMaybes metas)
        }
  where
    simpleStore = mkSimpleSessionStore path

    metaPath :: SessionId -> FilePath
    metaPath (SessionId uuid) = path </> ("meta." <> UUID.toString uuid <> ".json")

    readMeta :: SessionId -> IO (Maybe SessionMeta)
    readMeta sid = do
        let p = metaPath sid
        exists <- doesFileExist p
        if exists then Aeson.decodeStrict' <$> ByteString.readFile p else pure Nothing

    write :: SessionMeta -> Session -> IO ()
    write meta sess = do
        storeSession simpleStore (sessionIdToConversationId meta.smSessionId) sess
        LByteString.writeFile (metaPath meta.smSessionId) (Aeson.encode meta)

    storeLabelled :: SessionLabels -> SessionId -> Session -> IO ()
    storeLabelled labels sid sess = do
        now <- getCurrentTime
        old <- fromMaybe (freshSessionMeta sid now) <$> readMeta sid
        write (storedMeta now labels sess old) sess

    loadMeta :: SessionId -> IO (Maybe (Session, SessionMeta))
    loadMeta sid = do
        mSess <- readSession simpleStore (sessionIdToConversationId sid)
        case mSess of
            Nothing -> pure Nothing
            Just sess -> do
                mMeta <- readMeta sid
                meta <- case mMeta of
                    Just m -> pure m
                    Nothing -> do
                        mtime <- getModificationTime (sessionWritePath simpleStore (sessionIdToConversationId sid))
                        pure (freshSessionMeta sid mtime){smStatus = sessionStatusOf sess}
                pure $ Just (sess, meta)

    removeIfPresent :: FilePath -> IO ()
    removeIfPresent p = removeFile p `catch` \(_ :: IOException) -> pure ()

-------------------------------------------------------------------------------
-- SQLite Backend
-------------------------------------------------------------------------------

{- | SQLite-backed session backend.

Stores sessions as JSON rows in a @sessions@ table, with their metadata in
columns. Compare-and-store is a single conditional statement, so it is
atomic across threads and processes sharing the database.
-}
newtype SqliteSessionStore = SqliteSessionStore Connection

{- | A schema migration: applied once, in version order, inside a transaction,
and recorded in @schema_migrations@ under a component name.
-}
data Migration = Migration
    { migrationVersion :: Int
    , migrationApply :: Connection -> IO ()
    }

-- | Apply the migrations of a component that were not applied yet.
runMigrations :: Connection -> Text -> [Migration] -> IO ()
runMigrations conn component migrations = do
    execute_
        conn
        [sql| CREATE TABLE IF NOT EXISTS schema_migrations (
            component TEXT NOT NULL,
            version INTEGER NOT NULL,
            applied_at TIMESTAMP NOT NULL,
            PRIMARY KEY (component, version)
        ) |]
    applied <-
        map fromOnly
            <$> (query conn [sql| SELECT version FROM schema_migrations WHERE component = ? |] (Only component) :: IO [Only Int])
    forM_ (sortOn (.migrationVersion) migrations) $ \m ->
        unless (m.migrationVersion `elem` applied) $
            withTransaction conn $ do
                m.migrationApply conn
                now <- getCurrentTime
                execute
                    conn
                    [sql| INSERT INTO schema_migrations (component, version, applied_at) VALUES (?, ?, ?) |]
                    (component, m.migrationVersion, now)

-- | Migrations of the @sessions@ table.
sessionMigrations :: [Migration]
sessionMigrations =
    [ Migration 1 $ \conn -> do
        execute_
            conn
            [sql| CREATE TABLE IF NOT EXISTS sessions (
                session_id TEXT PRIMARY KEY,
                created_at TIMESTAMP NOT NULL,
                updated_at TIMESTAMP NOT NULL,
                json TEXT NOT NULL
            ) |]
        execute_ conn [sql| CREATE INDEX IF NOT EXISTS idx_sessions_updated ON sessions(updated_at) |]
    , Migration 2 $ \conn -> do
        mapM_
            (execute_ conn)
            [ [sql| ALTER TABLE sessions ADD COLUMN agent_slug TEXT |]
            , [sql| ALTER TABLE sessions ADD COLUMN parent_session_id TEXT |]
            , [sql| ALTER TABLE sessions ADD COLUMN owner TEXT |]
            , [sql| ALTER TABLE sessions ADD COLUMN status TEXT NOT NULL DEFAULT 'ready' |]
            , [sql| ALTER TABLE sessions ADD COLUMN status_detail TEXT |]
            , [sql| ALTER TABLE sessions ADD COLUMN version INTEGER NOT NULL DEFAULT 0 |]
            , [sql| CREATE INDEX IF NOT EXISTS idx_sessions_status ON sessions(status, updated_at) |]
            , [sql| CREATE INDEX IF NOT EXISTS idx_sessions_parent ON sessions(parent_session_id) |]
            ]
        -- Derive the status of sessions stored before this migration.
        rows <- query_ conn [sql| SELECT session_id, json FROM sessions |] :: IO [(Text, Text)]
        forM_ rows $ \(sid, json) ->
            forM_ (Aeson.decodeStrict' (TextEnc.encodeUtf8 json)) $ \sess ->
                execute
                    conn
                    [sql| UPDATE sessions SET status = ? WHERE session_id = ? |]
                    (sessionStatusText (sessionStatusOf sess), sid)
    ]

-- | Create or migrate the SQLite session schema.
initializeSessionSchema :: Connection -> IO ()
initializeSessionSchema conn = runMigrations conn "sessions" sessionMigrations

{- | Create a SQLite-backed session backend and initialize its schema.

Example:

> conn <- open ":memory:"
> backend <- mkSqliteSessionStore conn
-}
mkSqliteSessionStore :: Connection -> IO SessionBackend
mkSqliteSessionStore conn = do
    initializeSessionSchema conn
    pure $ sqliteSessionBackend conn

-- | Build a 'SessionBackend' from an existing SQLite connection.
sqliteSessionBackend :: Connection -> SessionBackend
sqliteSessionBackend conn =
    SessionBackend
        { sbStore = sqliteStoreSession conn noLabels
        , sbLoad = fmap (fmap fst) . sqliteLoadMeta conn
        , sbList = sqliteListSessions conn
        , sbDelete = sqliteDeleteSession conn
        , sbStoreLabelled = sqliteStoreSession conn
        , sbLoadMeta = sqliteLoadMeta conn
        , sbCompareAndStore = sqliteCompareAndStore conn
        , sbQuery = sqliteQuerySessions conn
        }

sessionIdText :: SessionId -> Text
sessionIdText (SessionId uuid) = UUID.toText uuid

encodeSession :: Session -> Text
encodeSession = TextEnc.decodeUtf8 . LByteString.toStrict . Aeson.encode

-- | Columns read into a 'SessionMeta', in 'MetaRow' order.
metaColumns :: Text
metaColumns = "session_id, agent_slug, parent_session_id, owner, status, status_detail, version, created_at, updated_at"

type MetaRow = (Text, Maybe Text, Maybe Text, Maybe Text, Text) :. (Maybe Text, Int, UTCTime, UTCTime)

metaFromRow :: MetaRow -> Maybe SessionMeta
metaFromRow ((sid, agent, parent, owner, status) :. (detail, version, created, updated)) = do
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
            }

sqliteStoreSession :: Connection -> SessionLabels -> SessionId -> Session -> IO ()
sqliteStoreSession conn labels sid sess = do
    now <- getCurrentTime
    execute
        conn
        [sql| INSERT INTO sessions
                (session_id, created_at, updated_at, json, agent_slug, parent_session_id, owner, status, version)
              VALUES (?, ?, ?, ?, ?, ?, ?, ?, 1)
              ON CONFLICT(session_id) DO UPDATE SET
                updated_at = excluded.updated_at,
                json = excluded.json,
                agent_slug = COALESCE(excluded.agent_slug, sessions.agent_slug),
                parent_session_id = COALESCE(excluded.parent_session_id, sessions.parent_session_id),
                owner = COALESCE(excluded.owner, sessions.owner),
                status = CASE WHEN sessions.status = 'running' THEN sessions.status ELSE excluded.status END,
                status_detail = CASE WHEN sessions.status = 'running' THEN sessions.status_detail ELSE NULL END,
                version = sessions.version + 1 |]
        ( (sessionIdText sid, now, now, encodeSession sess, labels.slAgent)
            :. (sessionIdText <$> labels.slParent, labels.slOwner, sessionStatusText (sessionStatusOf sess))
        )

sqliteLoadMeta :: Connection -> SessionId -> IO (Maybe (Session, SessionMeta))
sqliteLoadMeta conn sid = do
    rows <-
        query
            conn
            (Query $ "SELECT json, " <> metaColumns <> " FROM sessions WHERE session_id = ?")
            (Only $ sessionIdText sid) ::
            IO [Only Text :. MetaRow]
    pure $ case rows of
        [Only json :. row] -> (,) <$> Aeson.decodeStrict' (TextEnc.encodeUtf8 json) <*> metaFromRow row
        _ -> Nothing

sqliteCompareAndStore :: Connection -> SessionMeta -> Session -> IO (Either VersionConflict SessionMeta)
sqliteCompareAndStore conn meta sess = do
    now <- getCurrentTime
    let sid = sessionIdText meta.smSessionId
        columns =
            (now, encodeSession sess, meta.smAgent, sessionIdText <$> meta.smParent)
                :. (meta.smOwner, sessionStatusText meta.smStatus, meta.smStatusDetail)
    rows <-
        if meta.smVersion == 0
            then
                query
                    conn
                    [sql| INSERT INTO sessions
                            (session_id, created_at, updated_at, json, agent_slug, parent_session_id, owner, status, status_detail, version)
                          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 1)
                          ON CONFLICT(session_id) DO UPDATE SET
                            updated_at = excluded.updated_at,
                            json = excluded.json,
                            agent_slug = excluded.agent_slug,
                            parent_session_id = excluded.parent_session_id,
                            owner = excluded.owner,
                            status = excluded.status,
                            status_detail = excluded.status_detail,
                            version = sessions.version + 1
                          WHERE sessions.version = 0
                          RETURNING version, created_at |]
                    ((sid, now) :. columns)
            else
                query
                    conn
                    [sql| UPDATE sessions SET
                            updated_at = ?, json = ?, agent_slug = ?, parent_session_id = ?,
                            owner = ?, status = ?, status_detail = ?, version = version + 1
                          WHERE session_id = ? AND version = ?
                          RETURNING version, created_at |]
                    (columns :. (sid, meta.smVersion))
    case rows of
        [(version, created)] ->
            pure $ Right meta{smVersion = version, smCreatedAt = created, smUpdatedAt = now}
        _ -> do
            current <-
                query conn [sql| SELECT version FROM sessions WHERE session_id = ? |] (Only sid) ::
                    IO [Only Int]
            pure $ Left $ VersionConflict meta.smSessionId meta.smVersion (maybe 0 fromOnly (listToMaybe current))

sqliteQuerySessions :: Connection -> SessionQuery -> IO [SessionMeta]
sqliteQuerySessions conn q
    | q.sqStatuses == Just [] = pure []
    | otherwise = do
        let criteria =
                catMaybes
                    [ (\a -> ("agent_slug = ?", [toField a])) <$> q.sqAgent
                    , (\ss -> ("status IN (" <> Text.intercalate ", " ("?" <$ ss) <> ")", map (toField . sessionStatusText) ss)) <$> q.sqStatuses
                    , (\p -> ("parent_session_id = ?", [toField (sessionIdText p)])) <$> q.sqParent
                    , (\t -> ("updated_at < ?", [toField t])) <$> q.sqUpdatedBefore
                    ]
            whereClause
                | null criteria = ""
                | otherwise = " WHERE " <> Text.intercalate " AND " (map fst criteria)
            statement =
                "SELECT " <> metaColumns <> " FROM sessions" <> whereClause <> " ORDER BY updated_at DESC LIMIT ?"
        rows <-
            query conn (Query statement) (concatMap snd criteria <> [toField (fromMaybe (-1) q.sqLimit)]) ::
                IO [MetaRow]
        pure $ mapMaybe metaFromRow rows

sqliteListSessions :: Connection -> IO [(SessionId, UTCTime)]
sqliteListSessions conn =
    map (\m -> (m.smSessionId, m.smUpdatedAt)) <$> sqliteQuerySessions conn allSessionsQuery

sqliteDeleteSession :: Connection -> SessionId -> IO ()
sqliteDeleteSession conn sid =
    execute conn [sql| DELETE FROM sessions WHERE session_id = ? |] (Only $ sessionIdText sid)

-------------------------------------------------------------------------------
-- Composite Backend
-------------------------------------------------------------------------------

{- | Composite session backend.

Reads fall back across all provided backends in order. Writes (store/delete)
and metadata queries go to the first backend only, which acts as the primary
target.

Listing aggregates entries from all backends and deduplicates by 'SessionId',
keeping the first occurrence (highest-priority backend).
-}
newtype CompositeSessionStore = CompositeSessionStore [SessionBackend]

-- | Build a composite backend from a non-empty list of backends.
mkCompositeSessionStore :: [SessionBackend] -> SessionBackend
mkCompositeSessionStore backends =
    SessionBackend
        { sbStore = \sid sess -> onPrimary () $ \p -> sbStore p sid sess
        , sbLoad = fallback sbLoad
        , sbList = dedupeBy fst . concat <$> mapM sbList backends
        , sbDelete = \sid -> onPrimary () $ \p -> sbDelete p sid
        , sbStoreLabelled = \labels sid sess -> onPrimary () $ \p -> sbStoreLabelled p labels sid sess
        , sbLoadMeta = fallback sbLoadMeta
        , sbCompareAndStore = \meta sess ->
            onPrimary (Left $ VersionConflict meta.smSessionId meta.smVersion 0) $ \p -> sbCompareAndStore p meta sess
        , sbQuery = \q -> onPrimary [] $ \p -> sbQuery p q
        }
  where
    onPrimary :: a -> (SessionBackend -> IO a) -> IO a
    onPrimary none f = case backends of
        (primary : _) -> f primary
        [] -> pure none

    -- Try each backend in order until one has the session.
    fallback :: (SessionBackend -> SessionId -> IO (Maybe a)) -> SessionId -> IO (Maybe a)
    fallback get sid = go backends
      where
        go [] = pure Nothing
        go (b : bs) = get b sid >>= maybe (go bs) (pure . Just)

-------------------------------------------------------------------------------
-- Legacy Session Store Configuration
-------------------------------------------------------------------------------

{- | The default session file pattern prefix.
Files are named: @conv.<uuid>.json@
-}
defaultSessionPattern :: String
defaultSessionPattern = "conv."

-- | The default session file suffix.
defaultSessionSuffix :: String
defaultSessionSuffix = ".json"

{- | Session store with multi-location support.

The store supports reading from multiple locations while writing
to a single designated location. This enables:

1. Unified view of sessions scattered across directories
2. Project-local sessions overriding global ones
3. Archived or shared sessions in separate directories

Deduplication is done by ConversationId, with the first location
in 'sessionReadPrefixes' having the highest priority.
-}
data SessionStore = SessionStore
    { sessionWritePrefix :: FilePath
    {- ^ Directory where new sessions are written.
    This path is used for all write operations.
    -}
    , sessionReadPrefixes :: [FilePath]
    {- ^ Directories to search for existing sessions, in priority order.
    Earlier locations take precedence when the same session ID exists
    in multiple locations.
    -}
    }
    deriving (Show, Eq)

-- | Create an ineffective session store (no read or write locations).
defaultSessionStore :: SessionStore
defaultSessionStore = SessionStore "" []

{- | Create a new session store with separate write and read locations.

If the write location is not in the read locations list, it is automatically
prepended to ensure newly written sessions are immediately readable.

Example:

> mkSessionStore "./sessions/" ["./sessions/", "~/.config/agents-exe/sessions/"]
-}
mkSessionStore :: FilePath -> [FilePath] -> SessionStore
mkSessionStore writePrefix readPrefixes =
    SessionStore
        { sessionWritePrefix = writePrefix
        , sessionReadPrefixes =
            if writePrefix `elem` readPrefixes
                then readPrefixes
                else writePrefix : readPrefixes
        }

{- | Create a simple session store with a single location for both reading and writing.

This is the backwards-compatible constructor for single-location stores.

Example:

> mkSimpleSessionStore "./sessions/"
-}
mkSimpleSessionStore :: FilePath -> SessionStore
mkSimpleSessionStore prefix = mkSessionStore prefix [prefix]

-------------------------------------------------------------------------------
-- Path Resolution
-------------------------------------------------------------------------------

{- | Resolve a session path, expanding tilde (~) to the user's home directory.

Examples:

> resolveSessionPath "~/.config/agents-exe/sessions/"
> -- Returns: "/home/user/.config/agents-exe/sessions/"

> resolveSessionPath "./sessions/"
> -- Returns: "./sessions/" (unchanged)
-}
resolveSessionPath :: FilePath -> IO FilePath
resolveSessionPath path =
    if "~" `isPrefixOf` path
        then do
            home <- getHomeDirectory
            pure $ home ++ drop 1 path
        else pure path

{- | Resolve all paths in a SessionStore.

This expands tilde (~) prefixes in both write and read locations.
-}
resolveSessionStorePaths :: SessionStore -> IO SessionStore
resolveSessionStorePaths store = do
    writePath <- resolveSessionPath store.sessionWritePrefix
    readPaths <- mapM resolveSessionPath store.sessionReadPrefixes
    pure $ SessionStore writePath readPaths

-------------------------------------------------------------------------------
-- File Path Operations
-------------------------------------------------------------------------------

{- | Generate the file path for a session given a ConversationId.
The pattern is: @<prefix>conv.<uuid>.json@

Note: This uses the first read prefix to construct the path.
For write operations, use 'sessionWritePath' instead.
-}
sessionFilePath :: SessionStore -> ConversationId -> FilePath
sessionFilePath store convId =
    case sessionReadPrefixes store of
        [] -> conversationIdToFileName convId
        (prefix : _) -> prefix </> conversationIdToFileName convId

{- | Generate the write file path for a session.

This always uses the 'sessionWritePrefix' location, ensuring
new sessions are written to the correct directory.
-}
sessionWritePath :: SessionStore -> ConversationId -> FilePath
sessionWritePath store convId =
    store.sessionWritePrefix </> conversationIdToFileName convId

-- | Convert a ConversationId to a file name using the default pattern.
conversationIdToFileName :: ConversationId -> FilePath
conversationIdToFileName (ConversationId cid) =
    defaultSessionPattern ++ show cid ++ defaultSessionSuffix

{- | Parse a ConversationId from a file name using the default pattern.
Returns 'Nothing' if the file name doesn't match the expected pattern.
-}
parseConversationIdFromFileName :: FilePath -> Maybe ConversationId
parseConversationIdFromFileName name =
    let baseName = takeFileName name
        prefixLen = length defaultSessionPattern
        suffixLen = length defaultSessionSuffix
     in if defaultSessionPattern `isPrefixOf` baseName
            && defaultSessionSuffix `isSuffixOf` baseName
            then
                let uuidStr =
                        take (length baseName - prefixLen - suffixLen) $
                            drop prefixLen baseName
                 in case reads uuidStr of
                        [(uuid, "")] -> Just (ConversationId uuid)
                        _ -> Nothing
            else Nothing
  where
    isSuffixOf suffix str = reverse suffix `isInfixOf` reverse str

-------------------------------------------------------------------------------
-- Low-level File Operations
-------------------------------------------------------------------------------

{- | Check if an IOException indicates a "resource busy" (file locked) condition.
This checks the error message for common patterns indicating a locked file.
-}
isResourceBusyError :: IOException -> Bool
isResourceBusyError e =
    let errStr = ioeGetErrorString e
        errTxt = Text.toLower $ Text.pack errStr
     in "resource busy" `Text.isInfixOf` errTxt
            || "file is locked" `Text.isInfixOf` errTxt
            || "locked" `Text.isInfixOf` errTxt

{- | Read a session from a file path.
Returns 'Nothing' if:
- The file doesn't exist
- The file is locked (resource busy)
- The file can't be parsed

Note: This function forces strict evaluation of the file content to ensure
the file handle is closed promptly, preventing "resource busy" errors when
listing sessions concurrently.
-}
readSessionFromFile :: FilePath -> IO (Maybe Session)
readSessionFromFile path = do
    fileExists <- doesFileExist path
    if fileExists
        then do
            -- Try to read the file, handling locked files gracefully
            result <- try $ BSL.readFile path
            case result of
                Left (e :: IOException)
                    | isResourceBusyError e -> pure Nothing -- File is locked, return Nothing
                    | otherwise -> pure Nothing -- Other IO errors, return Nothing
                Right dat ->
                    -- Force strict evaluation by using `seq` on the length.
                    -- This ensures the file handle is closed before we return,
                    -- preventing "resource busy" errors when listing sessions.
                    LByteString.length dat `seq`
                        pure $
                            Aeson.decode =<< lastLine dat
        else do
            pure Nothing
  where
    lastLine :: LByteString.ByteString -> Maybe LByteString.ByteString
    lastLine dat = case BSL.lines dat of [] -> Nothing; rows -> Just (last rows)

-- | Store a session to a file path.
storeSessionToFile :: Session -> FilePath -> IO ()
storeSessionToFile sess path = do
    BSL.writeFile path (Aeson.encode sess <> "\n")

-------------------------------------------------------------------------------
-- Session Storage Operations
-------------------------------------------------------------------------------

{- | Store a session to disk at the appropriate path for the given ConversationId.
Always writes to the 'sessionWritePrefix' location.
-}
storeSession :: SessionStore -> ConversationId -> Session -> IO ()
storeSession store convId sess =
    storeSessionToFile sess (sessionWritePath store convId)

{- | Read a session from disk for the given ConversationId.

Searches through all read locations in priority order.
Returns 'Nothing' if the session file doesn't exist in any location,
is locked, or can't be parsed.
-}
readSession :: SessionStore -> ConversationId -> IO (Maybe Session)
readSession store convId = go (sessionReadPrefixes store)
  where
    go [] = pure Nothing
    go (prefix : rest) = do
        let path = prefix </> conversationIdToFileName convId
        mSession <- readSessionFromFile path
        case mSession of
            Just session -> pure (Just session)
            Nothing -> go rest

-------------------------------------------------------------------------------
-- Session File Discovery
-------------------------------------------------------------------------------

-- | Information about a discovered session file.
data SessionFileInfo = SessionFileInfo
    { sessionInfoPath :: FilePath
    -- ^ Full path to the session file
    , sessionInfoModTime :: UTCTime
    -- ^ Last modification time of the file
    , sessionInfoConversationId :: ConversationId
    -- ^ The ConversationId extracted from the file name
    }
    deriving (Show, Eq)

{- | Check if a filename matches the session file pattern.
Pattern: @conv.<uuid>.json@
-}
isSessionFile :: String -> Bool
isSessionFile name =
    let baseName = takeFileName name
     in defaultSessionPattern `isPrefixOf` baseName
            && defaultSessionSuffix `isSuffixOf` baseName
  where
    isSuffixOf suffix str = reverse suffix `isInfixOf` reverse str

{- | Find all session files in a single directory.
Returns files matching the @conv.<uuid>.json@ pattern with their metadata.
-}
findSessionsInDir :: FilePath -> IO [SessionFileInfo]
findSessionsInDir dir = do
    exists <- doesFileExist dir
    if exists
        then -- If the path is a file, return empty list
            pure []
        else do
            entries <- listDirectory dir
            let candidates = [dir </> entry | entry <- entries, isSessionFile entry]
            -- Filter to only existing files and get modification times
            existing <- filterM doesFileExist candidates
            -- Build session info, handling locked files gracefully
            catMaybes <$> mapM mkSessionInfo existing
  where
    mkSessionInfo :: FilePath -> IO (Maybe SessionFileInfo)
    mkSessionInfo path = do
        -- Try to get modification time, handling locked files
        mtimeResult <- try $ getModificationTime path
        case mtimeResult of
            Left (_ :: IOException) -> do
                -- File is locked or inaccessible, skip it
                pure Nothing
            Right mtime ->
                case parseConversationIdFromFileName (takeFileName path) of
                    Just cid -> pure $ Just $ SessionFileInfo path mtime cid
                    Nothing -> error $ "Unexpected: file passed isSessionFile but failed parse: " ++ path

{- | Find all session files across all read locations.

This function:
1. Collects sessions from all read locations
2. Deduplicates by ConversationId (first location wins)
3. Returns sessions sorted by modification time (most recent first)

The deduplication strategy ensures that if the same session exists
in multiple locations, the one from the earliest location in
'sessionReadPrefixes' is kept.
-}
findSessionFiles :: SessionStore -> IO [SessionFileInfo]
findSessionFiles store = do
    -- Resolve all paths first (expand tildes)
    resolvedStore <- resolveSessionStorePaths store

    -- Collect sessions from all read locations, preserving order
    allSessions <- concat <$> mapM findSessionsInDir resolvedStore.sessionReadPrefixes

    -- Deduplicate by ConversationId, keeping first occurrence (highest priority)
    let deduplicated = dedupeBy sessionInfoConversationId allSessions

    -- Sort by modification time (most recent first)
    pure $ sortOn (Down . sessionInfoModTime) deduplicated

{- | Deduplicate a list by a key function, keeping the first occurrence.

The first occurrence of each key is kept, subsequent duplicates are discarded.
This maintains the priority order of the input list.
-}
dedupeBy :: (Eq k) => (a -> k) -> [a] -> [a]
dedupeBy keyFn = foldl' addItem []
  where
    addItem acc item =
        let k = keyFn item
         in if k `elem` map keyFn acc
                then acc
                else acc ++ [item]

{- | List all sessions from files matching the store's prefix pattern.
Returns a list of @(FilePath, Maybe Session, ConversationId)@ triples,
sorted by file modification time (most recent first).

The 'Maybe Session' is 'Nothing' if:
- The session file couldn't be parsed
- The session file is locked (resource busy)
- The session file is inaccessible

This function aggregates sessions from all read locations and deduplicates
by ConversationId. The first location in 'sessionReadPrefixes' has the
highest priority for resolving duplicates.
-}
listSessions :: SessionStore -> IO [(FilePath, Maybe Session, ConversationId)]
listSessions store = do
    sessionFiles <- findSessionFiles store
    -- Load each session file (locked/inaccessible files will return Nothing)
    mapM (\info -> (sessionInfoPath info,,sessionInfoConversationId info) <$> readSessionFromFile (sessionInfoPath info)) sessionFiles


-------------------------------------------------------------------------------
-- Session Catalog
-------------------------------------------------------------------------------

-- | One session as listed by a 'SessionCatalog'.
data CatalogEntry = CatalogEntry
    { ceConversationId :: ConversationId
    , ceUpdatedAt :: Maybe UTCTime
    , ceSession :: Maybe Session
    -- ^ 'Nothing' when the session cannot be read, e.g. a locked file.
    , ceBusy :: Bool
    -- ^ Being written: a locked file, or a running session.
    }

{- | Read-only access to stored sessions, for tools that inspect them.

Sessions are identified by conversation ID: the file name in the file store,
and the same UUID as the session ID in a 'SessionBackend'.
-}
data SessionCatalog = SessionCatalog
    { catList :: IO [CatalogEntry]
    -- ^ All sessions, most recently updated first.
    , catRead :: ConversationId -> IO (Maybe Session)
    }

instance Show SessionCatalog where
    show _ = "SessionCatalog"

-- | The sessions of a file 'SessionStore', across all its read locations.
fileCatalog :: SessionStore -> SessionCatalog
fileCatalog store =
    SessionCatalog
        { catList = do
            files <- findSessionFiles store
            forM files $ \info -> do
                mSess <- readSessionFromFile info.sessionInfoPath
                busy <- isFileBusy info.sessionInfoPath
                pure
                    CatalogEntry
                        { ceConversationId = info.sessionInfoConversationId
                        , ceUpdatedAt = Just info.sessionInfoModTime
                        , ceSession = mSess
                        , ceBusy = busy
                        }
        , catRead = readSession store
        }

-- | The sessions of a 'SessionBackend'.
backendCatalog :: SessionBackend -> SessionCatalog
backendCatalog backend =
    SessionCatalog
        { catList = do
            metas <- sbQuery backend allSessionsQuery
            forM metas $ \meta -> do
                mSess <- sbLoad backend meta.smSessionId
                pure
                    CatalogEntry
                        { ceConversationId = sessionIdToConversationId meta.smSessionId
                        , ceUpdatedAt = Just meta.smUpdatedAt
                        , ceSession = mSess
                        , ceBusy = meta.smStatus == StatusRunning
                        }
        , catRead = sbLoad backend . conversationIdToSessionId
        }

-- | Whether a file is locked by a writer, so that it cannot be opened now.
isFileBusy :: FilePath -> IO Bool
isFileBusy path = do
    result <- try $ bracket (openBinaryFile path ReadMode) hClose (\_ -> pure ())
    case result of
        Left (ioe :: IOException) -> pure $ isResourceBusyError ioe
        Right _ -> pure False
