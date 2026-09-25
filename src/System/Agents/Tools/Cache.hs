{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Tool result cache for asynchronous/resumable session execution.

This module provides:

* 'CacheKey' - A unique identifier derived from tool call name and arguments
* 'CachedResult' - Stored result with metadata (creation time, TTL)
* 'ToolCache' - Pluggable cache storage interface
* 'mkSqliteToolCache' - SQLite-backed cache implementation

The cache is designed to support:
* Partial execution resumption (check cache before yielding)
* Cross-process persistence (SQLite storage)
* TTL-based expiration (optional)
* Content-addressable storage (hash of normalized arguments)
-}
module System.Agents.Tools.Cache (
    -- * Cache Key
    CacheKey (..),
    computeCacheKey,
    CacheScope (..),
    cacheScopeOf,
    scopedCacheKey,
    computeScopedCacheKey,
    uncacheableKey,
    isUncacheableKey,
    hashArguments,
    extractToolInfo,

    -- * Cached Result
    CachedResult (..),

    -- * Cache Interface
    ToolCache (..),

    -- * SQLite Implementation
    mkSqliteToolCache,
    initializeCacheSchema,
    cleanupExpiredCache,

    -- * Utilities
    normalizeArguments,
) where

import Control.Monad (forM_)
import Crypto.Hash (Digest, SHA256 (..), hashWith)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, execute_, open, query)
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)

import System.Agents.Session.Types (LlmToolCall (..), UserToolResponse)
import System.Agents.Tools.Bindings.Types (ScopedBinding (..))
import System.Agents.Tools.Params.Types (ParamValue (..), Params)

-------------------------------------------------------------------------------
-- Cache Key
-------------------------------------------------------------------------------

{- | Cache key derived from tool call.

The key uniquely identifies a tool call based on:
* Tool name (e.g., "bash", "sqlite_query")
* Arguments hash (normalized JSON representation)

This content-addressable approach ensures that identical tool calls
(with the same arguments) map to the same cache key, regardless of
the order of keys in the arguments object.
-}
data CacheKey = CacheKey
    { ckToolName :: Text
    -- ^ The name of the tool (e.g., "bash_command")
    , ckArgumentsHash :: Text
    -- ^ Hash of normalized arguments (hex-encoded)
    }
    deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CacheKey where
    toJSON key =
        Aeson.object
            [ "toolName" Aeson..= key.ckToolName
            , "argumentsHash" Aeson..= key.ckArgumentsHash
            ]

instance Aeson.FromJSON CacheKey where
    parseJSON = Aeson.withObject "CacheKey" $ \v ->
        CacheKey
            <$> v Aeson..: "toolName"
            <*> v Aeson..: "argumentsHash"

{- | Compute a cache key from an LLM tool call.

Extracts the tool name and arguments from the JSON structure,
normalizes the arguments, and computes a hash.
-}
computeCacheKey :: LlmToolCall -> CacheKey
computeCacheKey (LlmToolCall val) =
    let (toolName, args) = extractToolInfo val
        normalized = normalizeArguments args
        hash = hashArguments normalized
     in CacheKey toolName hash

{- | Extract tool name and arguments from the LLM tool call JSON.

Handles both OpenAI-style and native tool call formats.
-}
extractToolInfo :: Aeson.Value -> (Text, Aeson.Value)
extractToolInfo val = case val of
    Aeson.Object obj ->
        -- Try OpenAI format first: { "function": { "name": ..., "arguments": ... } }
        case KeyMap.lookup "function" obj of
            Just (Aeson.Object funcObj) ->
                let name = case KeyMap.lookup "name" funcObj of
                        Just (Aeson.String n) -> n
                        _ -> "unknown"
                    args = case KeyMap.lookup "arguments" funcObj of
                        Just a -> a
                        _ -> Aeson.Object mempty
                 in (name, args)
            _ ->
                -- Try native format: { "toolName": ..., "arguments": ... }
                let name = case KeyMap.lookup "toolName" obj of
                        Just (Aeson.String n) -> n
                        _ -> case KeyMap.lookup "name" obj of
                            Just (Aeson.String n) -> n
                            _ -> "unknown"
                    args = case KeyMap.lookup "arguments" obj of
                        Just a -> a
                        _ -> Aeson.Object mempty
                 in (name, args)
    _ -> ("unknown", Aeson.Object mempty)

{- | Normalize arguments for consistent hashing.

* Sorts object keys alphabetically
* Removes insignificant whitespace
* Normalizes number formats
-}
normalizeArguments :: Aeson.Value -> Aeson.Value
normalizeArguments val = case val of
    Aeson.Object obj ->
        -- Sort keys and recursively normalize values
        let sorted = sortByKey $ KeyMap.toList obj
         in Aeson.Object $ KeyMap.fromList $ map (\(k, v) -> (k, normalizeArguments v)) sorted
    Aeson.Array arr ->
        Aeson.Array $ fmap normalizeArguments arr
    other -> other
  where
    sortByKey :: [(KeyMap.Key, Aeson.Value)] -> [(KeyMap.Key, Aeson.Value)]
    sortByKey = sortBy (\(a, _) (b, _) -> compare a b)

{- | Compute hash of arguments: the SHA-256 (hex) of their canonical JSON.

(It used to be the length and a 60-character prefix of the bytes, which two
different calls could share.)
-}
hashArguments :: Aeson.Value -> Text
hashArguments = hexDigest . LBS.toStrict . Aeson.encode

hexDigest :: BS.ByteString -> Text
hexDigest = Text.pack . show . (hashWith SHA256 :: BS.ByteString -> Digest SHA256)

-------------------------------------------------------------------------------
-- Scoping by bound values (@todos/tool-partial-application.md@, G8)
-------------------------------------------------------------------------------

{- | What a call's cache key must additionally depend on. The LLM's arguments
are not all a tool receives: parameter bindings merge session values into
the call (tenant, project...) just before dispatch, so a cached result is
only valid for the values it was computed with.
-}
data CacheScope
    = -- | No bound values at all: the key depends on the call alone, as before.
      Unscoped
    | -- | The digest of the (non-secret) bound values in play.
      Scoped Text
    | {- | A secret value is in play: results obtained with one credential must
      not be replayed for another, and a hash of a secret in a persisted key
      is an oracle, so the call is not cached at all.
      -}
      Uncacheable
    deriving (Show, Eq)

{- | The scope of a call made under these parameter values and inherited
narrowing bindings (the context's 'ctxParams' and 'ctxInheritedBindings').

This looks at every parameter of the agent, not only those a tool binds:
the registrations' bindings are closed over inside the tools and are not
visible where the cache is consulted. That can only cost hits (two sessions
differing in an unrelated parameter do not share results), never share a
result across values a tool binds.
-}
cacheScopeOf :: Params -> [ScopedBinding] -> CacheScope
cacheScopeOf params inherited
    | any (.pvSecret) (Map.elems params) || any (.sbSecret) inherited = Uncacheable
    | Map.null params && null inherited = Unscoped
    | otherwise =
        Scoped $
            hexDigest $
                LBS.toStrict $
                    Aeson.encode $
                        Aeson.object
                            [ "params" Aeson..= Map.map (.pvValue) params
                            , "inherited" Aeson..= [(Aeson.toJSON (sbAddress b), sbTool b, sbArg b, sbValue b) | b <- inherited]
                            ]

-- | A tool call's key under a scope; 'Nothing' when it must not be cached.
computeScopedCacheKey :: CacheScope -> LlmToolCall -> Maybe CacheKey
computeScopedCacheKey scope call = case scope of
    Uncacheable -> Nothing
    _ -> Just (scopedCacheKey scope call)

{- | Like 'computeScopedCacheKey' for callers that need a key even when the
call is not cached (a continuation snapshot): an 'Uncacheable' call gets a
key that no other call can have and that 'isUncacheableKey' recognizes, so
its result is never stored.
-}
scopedCacheKey :: CacheScope -> LlmToolCall -> CacheKey
scopedCacheKey scope call@(LlmToolCall val) = case scope of
    Uncacheable -> uncacheableKey (fst (extractToolInfo val)) call
    Unscoped -> computeCacheKey call
    Scoped digest ->
        let (toolName, args) = extractToolInfo val
         in CacheKey toolName (hashArguments (Aeson.toJSON [Aeson.String digest, normalizeArguments args]))

-- | The placeholder key of a call that is not cached.
uncacheableKey :: Text -> LlmToolCall -> CacheKey
uncacheableKey toolName (LlmToolCall val) =
    CacheKey toolName ("uncacheable:" <> hashArguments val)

-- | Whether a key stands for a call that must not be cached ('uncacheableKey').
isUncacheableKey :: CacheKey -> Bool
isUncacheableKey key = "uncacheable:" `Text.isPrefixOf` key.ckArgumentsHash

-------------------------------------------------------------------------------
-- Cached Result
-------------------------------------------------------------------------------

{- | A cached tool result with metadata.

Includes the actual result, creation timestamp, and optional expiration.
-}
data CachedResult = CachedResult
    { crResult :: UserToolResponse
    -- ^ The cached tool response
    , crCreatedAt :: UTCTime
    -- ^ When this result was cached
    , crExpiresAt :: Maybe UTCTime
    -- ^ Optional expiration time (TTL support)
    }
    deriving (Show, Eq, Generic)

-- JSON instances for CachedResult
instance Aeson.ToJSON CachedResult where
    toJSON cr =
        Aeson.object
            [ "result" Aeson..= cr.crResult
            , "createdAt" Aeson..= cr.crCreatedAt
            , "expiresAt" Aeson..= cr.crExpiresAt
            ]

instance Aeson.FromJSON CachedResult where
    parseJSON = Aeson.withObject "CachedResult" $ \v ->
        CachedResult
            <$> v Aeson..: "result"
            <*> v Aeson..: "createdAt"
            <*> v Aeson..:? "expiresAt"

-------------------------------------------------------------------------------
-- Cache Interface
-------------------------------------------------------------------------------

{- | Pluggable cache storage interface.

This record abstracts over different cache backends (SQLite, in-memory,
Redis, etc.). The interface is designed for synchronous use from within
the agent execution loop.

All operations are in IO since cache access typically involves I/O.
-}
data ToolCache = ToolCache
    { cacheLookup :: CacheKey -> IO (Maybe CachedResult)
    {- ^ Look up a cached result by key.
    Returns Nothing if not found or expired.
    -}
    , cacheStore :: CacheKey -> CachedResult -> IO ()
    -- ^ Store a result in the cache.
    , cacheInvalidate :: CacheKey -> IO ()
    -- ^ Remove a specific entry from the cache.
    , cacheInvalidateTool :: Text -> IO ()
    -- ^ Remove all entries for a specific tool name.
    , cacheClear :: IO ()
    -- ^ Clear all entries from the cache.
    }

-------------------------------------------------------------------------------
-- SQLite Implementation
-------------------------------------------------------------------------------

{- | Create a SQLite-backed tool cache.

The cache is stored in a file at the given path. The database schema
is initialized automatically if it doesn't exist.

Example:

@
cache <- mkSqliteToolCache ".agents-cache.db"
result <- cacheLookup cache key
@
-}
mkSqliteToolCache :: FilePath -> IO ToolCache
mkSqliteToolCache dbPath = do
    conn <- open dbPath
    initializeCacheSchema conn
    pure $ mkCacheFromConnection conn

{- | Create a cache interface from an existing database connection.

Useful when the connection is managed externally (e.g., connection pool).
-}
mkCacheFromConnection :: Connection -> ToolCache
mkCacheFromConnection conn =
    ToolCache
        { cacheLookup = sqliteLookup conn
        , cacheStore = sqliteStore conn
        , cacheInvalidate = sqliteInvalidate conn
        , cacheInvalidateTool = sqliteInvalidateTool conn
        , cacheClear = sqliteClear conn
        }

-- | Schema for the tool call cache table.
cacheSchemaStatements :: [Query]
cacheSchemaStatements =
    [ [sql| CREATE TABLE IF NOT EXISTS tool_call_cache (
            cache_key TEXT PRIMARY KEY,
            tool_name TEXT NOT NULL,
            arguments_hash TEXT NOT NULL,
            result_json TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            expires_at TIMESTAMP
        ) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_cache_tool_name ON tool_call_cache(tool_name) |]
    , [sql| CREATE INDEX IF NOT EXISTS idx_cache_expires ON tool_call_cache(expires_at) |]
    ]

-- | Initialize the cache schema in the database.
initializeCacheSchema :: Connection -> IO ()
initializeCacheSchema conn =
    forM_ cacheSchemaStatements $ \stmt ->
        execute_ conn stmt

-- | Remove expired entries from the cache.
cleanupExpiredCache :: Connection -> IO ()
cleanupExpiredCache conn = do
    now <- getCurrentTime
    execute conn [sql| DELETE FROM tool_call_cache WHERE expires_at IS NOT NULL AND expires_at < ? |] (Only now)

-- | Build the cache key string from components.
buildCacheKeyString :: CacheKey -> Text
buildCacheKeyString key = key.ckToolName <> ":" <> key.ckArgumentsHash

-- | SQLite lookup implementation.
sqliteLookup :: Connection -> CacheKey -> IO (Maybe CachedResult)
sqliteLookup conn key = do
    -- First, clean up expired entries
    cleanupExpiredCache conn
    -- Then perform lookup
    let keyStr = buildCacheKeyString key
    results <-
        query
            conn
            [sql| SELECT result_json, created_at, expires_at FROM tool_call_cache WHERE cache_key = ? |]
            (Only keyStr)
    case results of
        [(jsonStr, created, mExpires)] ->
            case Aeson.decode (LBS.fromStrict $ TextEnc.encodeUtf8 jsonStr) of
                Just result -> pure $ Just $ CachedResult result created mExpires
                Nothing -> pure Nothing
        _ -> pure Nothing

-- | SQLite store implementation.
sqliteStore :: Connection -> CacheKey -> CachedResult -> IO ()
sqliteStore conn key result = do
    let keyStr = buildCacheKeyString key
    let jsonStr = TextEnc.decodeUtf8 $ LBS.toStrict $ Aeson.encode result.crResult
    execute
        conn
        [sql| INSERT INTO tool_call_cache (cache_key, tool_name, arguments_hash, result_json, created_at, expires_at)
              VALUES (?, ?, ?, ?, ?, ?)
              ON CONFLICT(cache_key) DO UPDATE SET
                result_json = excluded.result_json,
                created_at = excluded.created_at,
                expires_at = excluded.expires_at |]
        (keyStr, key.ckToolName, key.ckArgumentsHash, jsonStr, result.crCreatedAt, result.crExpiresAt)

-- | SQLite invalidate implementation.
sqliteInvalidate :: Connection -> CacheKey -> IO ()
sqliteInvalidate conn key = do
    let keyStr = buildCacheKeyString key
    execute conn [sql| DELETE FROM tool_call_cache WHERE cache_key = ? |] (Only keyStr)

-- | SQLite invalidate-by-tool implementation.
sqliteInvalidateTool :: Connection -> Text -> IO ()
sqliteInvalidateTool conn toolName =
    execute conn [sql| DELETE FROM tool_call_cache WHERE tool_name = ? |] (Only toolName)

-- | SQLite clear implementation.
sqliteClear :: Connection -> IO ()
sqliteClear conn =
    execute_ conn [sql| DELETE FROM tool_call_cache |]
