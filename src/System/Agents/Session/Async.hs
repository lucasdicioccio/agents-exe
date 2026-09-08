{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Asynchronous/resumable session execution support.

This module provides types and utilities for executing agent sessions
asynchronously with the ability to pause after partial tool execution
and resume later (potentially on a different machine).

Key concepts:

* 'AsyncToolResponse' - Tool execution can complete immediately or yield
* 'ContinuationToken' - Unique identifier for a yielded tool call
* 'ToolContinuation' - Stored state needed to resume a yielded call
* 'ToolContinuationSnapshot' - Serializable subset of a continuation
* 'ContinuationStore' - Persistence interface for continuation snapshots
* 'AsyncExecutionState' - Tracks progress through partial execution

Usage:

@
-- Execute a single tool call with async support
result <- executeAsyncToolCall agent ctx call cache

-- Handle the result
case result of
    ToolComplete response -> do
        -- Continue with next tool
        processNextTool ...
    ToolYield token cacheKey -> do
        -- Store continuation and pause session
        storeContinuation store snapshot
        saveSession session  -- Persist partial state
@
-}
module System.Agents.Session.Async (
    -- * Async Tool Response
    AsyncToolResponse (..),

    -- * Continuation Token
    ContinuationToken (..),
    newContinuationToken,

    -- * Tool Continuation
    ToolContinuation (..),
    ToolContinuationSnapshot (..),
    mkToolContinuationSnapshot,

    -- * Continuation Store
    ContinuationStore (..),

    -- * Execution State
    AsyncExecutionState (..),
    initialAsyncState,
    isComplete,
    remainingCalls,

    -- * Core Functions
    executeAsyncToolCall,
    resumeAsyncToolCall,
    storeContinuation,
    loadContinuation,

    -- * SQLite Continuation Store
    mkSqliteContinuationStore,
    initializeContinuationSchema,
    cleanupExpiredContinuations,

    -- * Session State Queries
    getPendingContinuation,
    hasPendingContinuations,
) where

import Control.Monad (forM, forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, execute_, query)
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)

import System.Agents.Session.Types (
    AppliedPolicy (..),
    ContinuationToken (..),
    LlmToolCall (..),
    PartialUserTurnContent (..),
    Session (..),
    SessionId (..),
    ToolCallDisposition (..),
    ToolCallId (..),
    ToolCallState (..),
    TrackedToolCall (..),
    Turn (..),
    UserToolResponse (..),
    newContinuationToken,
 )
import System.Agents.Tools.Cache (CacheKey (..), CachedResult (..), ToolCache (..), computeCacheKey)
import System.Agents.Tools.Context (ToolExecutionContext (..), ToolExecutionContextSnapshot, contextSnapshot)
-------------------------------------------------------------------------------
-- Async Tool Response
-------------------------------------------------------------------------------

{- | The result of executing a tool call in async mode.

A tool call can either:
* Complete immediately with a 'ToolComplete' response
* Yield for external completion with 'ToolYield', providing a continuation token

The 'ToolYield' case allows sessions to pause mid-execution and resume
later when the external result is available.
-}
data AsyncToolResponse
    = -- | Normal synchronous completion
      ToolComplete UserToolResponse
    | -- | Paused, waiting for external completion
      ToolYield ContinuationToken CacheKey
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON AsyncToolResponse
instance Aeson.FromJSON AsyncToolResponse

-------------------------------------------------------------------------------
-- Continuation Token
-------------------------------------------------------------------------------

-- | Convert token to text for storage.
tokenToText :: ContinuationToken -> Text
tokenToText (ContinuationToken uuid) = UUID.toText uuid

-------------------------------------------------------------------------------
-- Tool Continuation
-------------------------------------------------------------------------------

{- | Stored state for a yielded tool call.

This record contains everything needed to resume a tool call that
yielded for external completion. It is intended for in-memory use; for
persistence use 'ToolContinuationSnapshot'.
-}
data ToolContinuation = ToolContinuation
    { tcToken :: ContinuationToken
    -- ^ Unique token for resuming
    , tcSessionId :: SessionId
    -- ^ Session this continuation belongs to
    , tcToolCall :: LlmToolCall
    -- ^ The original tool call
    , tcContext :: ToolExecutionContext
    -- ^ Execution context at yield time
    , tcCreatedAt :: UTCTime
    -- ^ When the continuation was created
    , tcExpiresAt :: Maybe UTCTime
    -- ^ Optional expiration time
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON ToolContinuation where
    toJSON tc =
        Aeson.object
            [ "token" Aeson..= tc.tcToken
            , "sessionId" Aeson..= tc.tcSessionId
            , "toolCall" Aeson..= tc.tcToolCall
            , "context" Aeson..= tc.tcContext
            , "createdAt" Aeson..= tc.tcCreatedAt
            , "expiresAt" Aeson..= tc.tcExpiresAt
            ]

instance Aeson.FromJSON ToolContinuation where
    parseJSON = Aeson.withObject "ToolContinuation" $ \v ->
        ToolContinuation
            <$> v Aeson..: "token"
            <*> v Aeson..: "sessionId"
            <*> v Aeson..: "toolCall"
            <*> v Aeson..: "context"
            <*> v Aeson..: "createdAt"
            <*> v Aeson..:? "expiresAt"

{- | Serializable snapshot of a yielded tool call.

This is the durable representation that is stored in a 'ContinuationStore'.
It omits the non-serializable runtime fields of the execution context
('ctxToolPortal', 'ctxWorld', 'ctxEventQueue') and includes only the
fields needed to re-hydrate the call later.
-}
data ToolContinuationSnapshot = ToolContinuationSnapshot
    { tcsToken :: ContinuationToken
    -- ^ Unique token for resuming
    , tcsSessionId :: SessionId
    -- ^ Session this continuation belongs to
    , tcsToolCallId :: ToolCallId
    -- ^ Stable identifier for the call within its turn
    , tcsToolCall :: LlmToolCall
    -- ^ The original tool call
    , tcsCacheKey :: CacheKey
    -- ^ Cache key derived from the tool call
    , tcsPolicy :: AppliedPolicy
    -- ^ Policy decision that produced the continuation
    , tcsContextSnapshot :: ToolExecutionContextSnapshot
    -- ^ Serializable subset of the execution context
    , tcsCreatedAt :: UTCTime
    -- ^ When the continuation was created
    , tcsExpiresAt :: Maybe UTCTime
    -- ^ Optional expiration time
    }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON ToolContinuationSnapshot
instance Aeson.FromJSON ToolContinuationSnapshot

{- | Build a serializable snapshot for a deferred tool call.

The snapshot captures everything needed to persist the continuation and
later re-hydrate it in a different process.
-}
mkToolContinuationSnapshot ::
    ContinuationToken ->
    UTCTime ->
    TrackedToolCall ->
    ToolCallDisposition ->
    ToolExecutionContext ->
    ToolContinuationSnapshot
mkToolContinuationSnapshot token now tc disp ctx =
    ToolContinuationSnapshot
        { tcsToken = token
        , tcsSessionId = ctx.ctxSessionId
        , tcsToolCallId = tc.tcId
        , tcsToolCall = tc.tcCall
        , tcsCacheKey = computeCacheKey tc.tcCall
        , tcsPolicy = AppliedPolicy disp Nothing
        , tcsContextSnapshot = contextSnapshot ctx
        , tcsCreatedAt = now
        , tcsExpiresAt = Nothing
        }

-------------------------------------------------------------------------------
-- Continuation Store Interface
-------------------------------------------------------------------------------

{- | Interface for storing and retrieving continuation snapshots.

This abstracts over different storage backends (SQLite, Redis, etc.).
-}
data ContinuationStore = ContinuationStore
    { csStore :: ToolContinuationSnapshot -> IO ()
    -- ^ Store a new continuation snapshot
    , csLoad :: ContinuationToken -> IO (Maybe ToolContinuationSnapshot)
    -- ^ Load a continuation snapshot by token
    , csComplete :: ContinuationToken -> UserToolResponse -> IO Bool
    -- ^ Mark a continuation as completed with the result
    , csDelete :: ContinuationToken -> IO ()
    -- ^ Delete a continuation
    , csListPending :: SessionId -> IO [(ContinuationToken, ToolContinuationSnapshot)]
    -- ^ List all pending continuation snapshots for a session
    , csCleanupExpired :: IO ()
    -- ^ Remove expired continuations
    }

-------------------------------------------------------------------------------
-- Async Execution State
-------------------------------------------------------------------------------

{- | Tracks the state of an async execution.

Maintains lists of completed and pending tool calls, allowing
resumption from any point in the execution.
-}
data AsyncExecutionState = AsyncExecutionState
    { aesCompleted :: [(LlmToolCall, UserToolResponse)]
    -- ^ Tool calls that have completed
    , aesPending :: [LlmToolCall]
    -- ^ Tool calls that still need to be executed
    , aesSessionId :: SessionId
    -- ^ The session being executed
    }
    deriving (Show, Eq, Generic)

-- | Create initial async state from a list of tool calls.
initialAsyncState :: SessionId -> [LlmToolCall] -> AsyncExecutionState
initialAsyncState sid calls =
    AsyncExecutionState
        { aesCompleted = []
        , aesPending = calls
        , aesSessionId = sid
        }

-- | Check if all tool calls have completed.
isComplete :: AsyncExecutionState -> Bool
isComplete state = null $ aesPending state

-- | Get the number of remaining tool calls.
remainingCalls :: AsyncExecutionState -> Int
remainingCalls = length . aesPending

-------------------------------------------------------------------------------
-- Core Async Functions
-------------------------------------------------------------------------------

{- | Execute a single tool call in async mode.

First checks the cache for a cached result. If found, returns it immediately.
If not in cache, executes the tool normally and returns the result.

This is the basic async execution without yielding - tools complete immediately
but benefit from caching.
-}
executeAsyncToolCall ::
    -- | Function to execute the tool call
    (ToolExecutionContext -> LlmToolCall -> IO UserToolResponse) ->
    -- | Execution context
    ToolExecutionContext ->
    -- | The tool call to execute
    LlmToolCall ->
    -- | Optional cache for lookup
    Maybe ToolCache ->
    -- | Result (always completes in this basic version)
    IO AsyncToolResponse
executeAsyncToolCall exec ctx call mCache = do
    -- Check cache first
    case mCache of
        Just cache -> do
            let cacheKey = computeCacheKey call
            mCached <- cache.cacheLookup cacheKey
            case mCached of
                Just cached -> pure $ ToolComplete $ crResult cached
                Nothing -> do
                    -- Not in cache, execute normally
                    result <- exec ctx call
                    pure $ ToolComplete result
        Nothing -> do
            -- No cache configured, execute directly
            result <- exec ctx call
            pure $ ToolComplete result

{- | Resume a yielded tool call with an external result.

This is called when an external process completes a previously-yielded
tool call. The result is stored in cache and the continuation is marked
as complete.
-}
resumeAsyncToolCall ::
    ContinuationStore ->
    ToolCache ->
    ContinuationToken ->
    UserToolResponse ->
    IO Bool
resumeAsyncToolCall store cache token result = do
    -- Mark continuation as complete
    completed <- csComplete store token result
    -- Store result in cache for future lookups
    if completed
        then do
            mSnap <- csLoad store token
            case mSnap of
                Just snap -> do
                    now <- getCurrentTime
                    cache.cacheStore (tcsCacheKey snap) $ CachedResult result now Nothing
                Nothing -> pure ()
        else pure ()
    pure completed

{- | Store a continuation snapshot for later resumption.

This is called when a tool yields instead of completing immediately.
The snapshot is stored persistently so the session can be resumed later
(potentially on a different machine).
-}
storeContinuation ::
    ContinuationStore ->
    ToolContinuationSnapshot ->
    IO ()
storeContinuation store = csStore store

-- | Load a continuation snapshot by its token.
loadContinuation ::
    ContinuationStore ->
    ContinuationToken ->
    IO (Maybe ToolContinuationSnapshot)
loadContinuation store = csLoad store

-------------------------------------------------------------------------------
-- SQLite Continuation Store Implementation
-------------------------------------------------------------------------------

-- | Create a SQLite-backed continuation store.
mkSqliteContinuationStore :: Connection -> IO ContinuationStore
mkSqliteContinuationStore conn = do
    initializeContinuationSchema conn
    pure $
        ContinuationStore
            { csStore = sqliteStoreContinuation conn
            , csLoad = sqliteLoadContinuation conn
            , csComplete = sqliteCompleteContinuation conn
            , csDelete = sqliteDeleteContinuation conn
            , csListPending = sqliteListPending conn
            , csCleanupExpired = cleanupExpiredContinuations conn
            }

-- | Schema for the tool continuations table.
continuationSchemaStatements :: [Query]
continuationSchemaStatements =
    [ [sql| CREATE TABLE IF NOT EXISTS tool_continuations (
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
    ]

-- | Initialize the continuation schema.
initializeContinuationSchema :: Connection -> IO ()
initializeContinuationSchema conn =
    forM_ continuationSchemaStatements $ \stmt ->
        execute_ conn stmt

-- | Remove expired continuations.
cleanupExpiredContinuations :: Connection -> IO ()
cleanupExpiredContinuations conn = do
    now <- getCurrentTime
    execute conn [sql| DELETE FROM tool_continuations WHERE expires_at IS NOT NULL AND expires_at < ? |] (Only now)

-- | Store a continuation snapshot in SQLite.
--
-- The whole 'ToolContinuationSnapshot' is serialized into the @context_json@
-- column so that the load/list functions can round-trip it without relying on
-- the non-serializable runtime context fields.
sqliteStoreContinuation :: Connection -> ToolContinuationSnapshot -> IO ()
sqliteStoreContinuation conn snap = do
    let tokenStr = tokenToText snap.tcsToken
    let SessionId sid = snap.tcsSessionId
    let toolCallJson = TextEnc.decodeUtf8 $ LBS.toStrict $ Aeson.encode snap.tcsToolCall
    let snapshotJson = TextEnc.decodeUtf8 $ LBS.toStrict $ Aeson.encode snap
    execute
        conn
        [sql| INSERT INTO tool_continuations
              (token, session_id, tool_call_json, context_json, created_at, expires_at)
              VALUES (?, ?, ?, ?, ?, ?)
              ON CONFLICT(token) DO UPDATE SET
                session_id = excluded.session_id,
                tool_call_json = excluded.tool_call_json,
                context_json = excluded.context_json,
                expires_at = excluded.expires_at |]
        (tokenStr, UUID.toText sid, toolCallJson, snapshotJson, snap.tcsCreatedAt, snap.tcsExpiresAt)

-- | Load a continuation snapshot from SQLite.
sqliteLoadContinuation :: Connection -> ContinuationToken -> IO (Maybe ToolContinuationSnapshot)
sqliteLoadContinuation conn token = do
    let tokenStr = tokenToText token
    rows <-
        query
            conn
            [sql| SELECT context_json FROM tool_continuations
                  WHERE token = ? AND completed_at IS NULL |]
            (Only tokenStr) ::
            IO [Only Text]
    case rows of
        [Only snapshotJson] ->
            pure $ Aeson.decode $ LBS.fromStrict $ TextEnc.encodeUtf8 snapshotJson
        _ -> pure Nothing

-- | Mark a continuation as completed.
sqliteCompleteContinuation :: Connection -> ContinuationToken -> UserToolResponse -> IO Bool
sqliteCompleteContinuation conn token result = do
    let tokenStr = tokenToText token
    let resultJson = TextEnc.decodeUtf8 $ LBS.toStrict $ Aeson.encode result
    now <- getCurrentTime
    -- Only update if not already completed
    execute
        conn
        [sql| UPDATE tool_continuations
              SET completed_at = ?, result_json = ?
              WHERE token = ? AND completed_at IS NULL |]
        (now, resultJson, tokenStr)
    -- Check if any row was updated
    rows <- query conn [sql| SELECT changes() |] () :: IO [Only Int]
    case rows of
        [Only n] -> pure $ n > 0
        _ -> pure False

-- | Delete a continuation.
sqliteDeleteContinuation :: Connection -> ContinuationToken -> IO ()
sqliteDeleteContinuation conn token = do
    let tokenStr = tokenToText token
    execute conn [sql| DELETE FROM tool_continuations WHERE token = ? |] (Only tokenStr)

-- | List pending continuation snapshots for a session.
sqliteListPending :: Connection -> SessionId -> IO [(ContinuationToken, ToolContinuationSnapshot)]
sqliteListPending conn sid = do
    let SessionId uuid = sid
    rows <-
        query
            conn
            [sql| SELECT token, context_json FROM tool_continuations
                  WHERE session_id = ? AND completed_at IS NULL |]
            (Only $ UUID.toText uuid) ::
            IO [(Text, Text)]
    fmap catMaybes $ forM rows $ \(tokenText, snapshotJson) ->
        case UUID.fromText tokenText of
            Nothing -> pure Nothing
            Just uuid' -> do
                let mSnapshot = Aeson.decode $ LBS.fromStrict $ TextEnc.encodeUtf8 snapshotJson
                pure $ fmap (\snap -> (ContinuationToken uuid', snap)) mSnapshot
-- Session State Queries
-------------------------------------------------------------------------------

{- | Get the first pending continuation from a session.

Checks the session's turns for any 'PartialUserTurn' with deferred calls
that have associated continuation tokens.
-}
getPendingContinuation :: Session -> Maybe (ContinuationToken, CacheKey)
getPendingContinuation session =
    listToMaybe $ concatMap extractFromTurn $ turns session
  where
    extractFromTurn :: Turn -> [(ContinuationToken, CacheKey)]
    extractFromTurn turn = case turn of
        PartialUserTurn content _ ->
            [ (token, computeCacheKey tc.tcCall)
            | tc <- content.pTrackedToolCalls
            , tc.tcState == Deferred
            , Just token <- [tc.tcContinuation]
            ]
        _ -> []

-- | Check if a session has any pending continuations.
hasPendingContinuations :: Session -> Bool
hasPendingContinuations = maybe False (const True) . getPendingContinuation

