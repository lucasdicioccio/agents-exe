{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- | The durable, SQLite-backed 'MailStore' (Phase 3 of
@todos/session-mailbox.md@, §1): a @session_mail@ table next to the
continuations table, mirroring its schema/migration pattern
("System.Agents.Session.Async").

Each envelope is persisted whole, as JSON, in @body_json@ (its 'ToJSON'
instance already covers 'envFrom'\/'envPriority'\/'envHops'\/'envBody'), with
@session_id@, @seq@, and @id@ broken out as real columns so the table can be
indexed and 'msAppend' can be idempotent on @(session_id, id)@ per §1's
"accept is atomic and idempotent".
-}
module System.Agents.Session.MailStore (
    mkSqliteMailStore,
    initializeMailSchema,
) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEnc
import qualified Data.UUID as UUID
import Database.SQLite.Simple (Connection, Only (..), Query, execute, execute_, fromOnly, query)
import Database.SQLite.Simple.QQ (sql)

import System.Agents.Session.Mailbox (MailStore (..))
import System.Agents.Session.Types (Envelope (..), MessageId (..), SessionId (..))
import System.Agents.SessionStore (Migration (..), runMigrations)

-- | Create a SQLite-backed 'MailStore', creating/migrating its table first.
mkSqliteMailStore :: Connection -> IO MailStore
mkSqliteMailStore conn = do
    initializeMailSchema conn
    pure
        MailStore
            { msAppend = sqliteAppendMail conn
            , msLoad = sqliteLoadMail conn
            }

-- | Create or migrate the @session_mail@ schema.
initializeMailSchema :: Connection -> IO ()
initializeMailSchema conn = runMigrations conn "session_mail" mailMigrations

mailMigrations :: [Migration]
mailMigrations =
    [Migration 1 $ \c -> mapM_ (execute_ c) mailSchemaStatements]

mailSchemaStatements :: [Query]
mailSchemaStatements =
    [ [sql| CREATE TABLE IF NOT EXISTS session_mail (
            session_id TEXT NOT NULL,
            seq INTEGER NOT NULL,
            id TEXT NOT NULL,
            body_json TEXT NOT NULL,
            accepted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            PRIMARY KEY (session_id, id)
        ) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_session_mail_session_seq ON session_mail(session_id, seq) |]
    ]

-- | Append one envelope. A no-op if @(session_id, id)@ already exists
-- (idempotent accept, per §1).
sqliteAppendMail :: Connection -> SessionId -> Envelope -> IO ()
sqliteAppendMail conn (SessionId sid) envelope = do
    let MessageId mid = envelope.envId
        bodyJson = TextEnc.decodeUtf8 $ LBS.toStrict $ Aeson.encode envelope
    execute
        conn
        [sql| INSERT INTO session_mail (session_id, seq, id, body_json)
              VALUES (?, ?, ?, ?)
              ON CONFLICT(session_id, id) DO NOTHING |]
        (UUID.toText sid, envelope.envSeq, UUID.toText mid, bodyJson)

-- | Load a session's envelopes, in ascending 'envSeq' order.
sqliteLoadMail :: Connection -> SessionId -> IO [Envelope]
sqliteLoadMail conn (SessionId sid) = do
    rows <-
        query
            conn
            [sql| SELECT body_json FROM session_mail WHERE session_id = ? ORDER BY seq ASC |]
            (Only $ UUID.toText sid) ::
            IO [Only Text]
    pure $ mapMaybe (Aeson.decode . LBS.fromStrict . TextEnc.encodeUtf8 . fromOnly) rows
