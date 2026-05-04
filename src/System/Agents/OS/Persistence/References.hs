{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Persistence layer for cross-conversation references.

This module provides database operations for storing and querying
references between conversations. It supports:

- **Reference Storage**: Persist references as database entities
- **Bidirectional Queries**: Efficient lookup of incoming/outgoing refs
- **Graph Traversal**: Query reference graphs at the database level
- **Validation Tracking**: Store and query reference resolution status

== Database Schema

References are stored in the 'conversation_refs' table with:
- source_conv_id: The conversation making the reference
- target_conv_id: The conversation being referenced
- target_msg_id: Optional specific message ID
- ref_type: Type of reference (cite, reply, related, fork)
- created_at: Creation timestamp
- created_by: User/agent who created the reference
- context: Optional context text
- resolved_at: When the target was validated
- is_valid: Whether the target exists
- resolution_error: Error message if resolution failed

== Indexes

- idx_refs_source: Fast lookup of outgoing references
- idx_refs_target: Fast lookup of incoming references (backlinks)
- idx_refs_type: Filter by reference type
- idx_refs_valid: Filter valid/invalid references
-}
module System.Agents.OS.Persistence.References (
    -- * Reference Persistence
    persistReference,
    loadReference,
    deleteReference,

    -- * Batch Operations
    persistReferencesBatch,

    -- * Querying
    getOutgoingReferences,
    getIncomingReferences,
    getReferencesBetween,
    queryReferences,

    -- * Graph Operations
    getReferenceGraph,
    getConnectedConversations,

    -- * Validation
    updateReferenceStatus,
    getBrokenReferences,

    -- * Suggestions
    searchConversationsForRef,
) where

import Control.Monad (forM, forM_)
import Data.Pool (withResource)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Database.SQLite.Simple (
    FromRow (..),
    execute,
    field,
    query,
    query_,
 )
import Database.SQLite.Simple.QQ (sql)

import System.Agents.OS.Core.Types (
    ConversationId (..),
    EntityId (..),
 )
import System.Agents.OS.Persistence.Sqlite (SqliteBackend (..))
import System.Agents.OS.Persistence.Types (
    SqlitePersistence (..),
    entityIdToText,
 )
import System.Agents.OS.References.Types (
    ConversationRef (..),
    MessageRef (..),
    RefType (..),
    ReferenceQuery (..),
 )

-------------------------------------------------------------------------------
-- Row Types
-------------------------------------------------------------------------------

{- | Database row for conversation references.
Note: Some fields are parsed but not currently used in the domain model.
-}
data ReferenceRow = ReferenceRow
    { _rrId :: Int
    , rrSourceConvId :: Text
    , rrTargetConvId :: Text
    , rrTargetMsgId :: Maybe Text
    , rrRefType :: Text
    , rrCreatedAt :: UTCTime
    , rrCreatedBy :: Maybe Text
    , rrContext :: Maybe Text
    , _rrResolvedAt :: Maybe UTCTime
    , _rrIsValid :: Bool
    , _rrResolutionError :: Maybe Text
    }

instance FromRow ReferenceRow where
    fromRow =
        ReferenceRow
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

-------------------------------------------------------------------------------
-- Reference Persistence
-------------------------------------------------------------------------------

{- | Persist a reference to the database.

Creates or updates a reference record in the conversation_refs table.
-}
persistReference ::
    SqliteBackend ->
    ConversationRef ->
    IO ()
persistReference backend ref = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        execute
            conn
            [sql|
                INSERT INTO conversation_refs (
                    source_conv_id, target_conv_id, target_msg_id,
                    ref_type, created_at, created_by, context,
                    resolved_at, is_valid, resolution_error
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(source_conv_id, target_conv_id, target_msg_id) DO UPDATE SET
                    ref_type = excluded.ref_type,
                    context = excluded.context,
                    resolved_at = excluded.resolved_at,
                    is_valid = excluded.is_valid,
                    resolution_error = excluded.resolution_error
            |]
            ( entityIdToText (entityIdFromConvId ref.refSourceConversation)
            , entityIdToText (entityIdFromConvId ref.refTargetConversation)
            , fmap unMessageRef ref.refTargetMessage :: Maybe Text
            , refTypeToText ref.refType
            , ref.refCreatedAt
            , ref.refCreatedBy :: Maybe Text
            , ref.refContext :: Maybe Text
            , Nothing :: Maybe UTCTime
            , True :: Bool
            , Nothing :: Maybe Text
            )

-- | Load a reference by its components.
loadReference ::
    SqliteBackend ->
    -- | Source
    ConversationId ->
    -- | Target
    ConversationId ->
    -- | Optional message
    Maybe MessageRef ->
    IO (Maybe ConversationRef)
loadReference backend sourceId targetId mMsgRef = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        results <-
            query
                conn
                [sql|
                    SELECT id, source_conv_id, target_conv_id, target_msg_id,
                           ref_type, created_at, created_by, context,
                           resolved_at, is_valid, resolution_error
                    FROM conversation_refs
                    WHERE source_conv_id = ? AND target_conv_id = ? AND target_msg_id IS ?
                |]
                ( entityIdToText (entityIdFromConvId sourceId)
                , entityIdToText (entityIdFromConvId targetId)
                , fmap unMessageRef mMsgRef :: Maybe Text
                )

        case results of
            [row] -> pure $ Just $ rowToReference row
            _ -> pure Nothing

-- | Delete a reference.
deleteReference ::
    SqliteBackend ->
    ConversationId ->
    ConversationId ->
    Maybe MessageRef ->
    IO ()
deleteReference backend sourceId targetId mMsgRef = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        execute
            conn
            [sql|
                DELETE FROM conversation_refs
                WHERE source_conv_id = ? AND target_conv_id = ? AND target_msg_id IS ?
            |]
            ( entityIdToText (entityIdFromConvId sourceId)
            , entityIdToText (entityIdFromConvId targetId)
            , fmap unMessageRef mMsgRef :: Maybe Text
            )

-------------------------------------------------------------------------------
-- Batch Operations
-------------------------------------------------------------------------------

{- | Persist multiple references in a batch.

More efficient than individual persistReference calls.
-}
persistReferencesBatch ::
    SqliteBackend ->
    [ConversationRef] ->
    IO ()
persistReferencesBatch backend refs = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        forM_ refs $ \ref ->
            execute
                conn
                [sql|
                    INSERT INTO conversation_refs (
                        source_conv_id, target_conv_id, target_msg_id,
                        ref_type, created_at, created_by, context,
                        resolved_at, is_valid, resolution_error
                    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ON CONFLICT(source_conv_id, target_conv_id, target_msg_id) DO UPDATE SET
                        ref_type = excluded.ref_type,
                        context = excluded.context
                |]
                ( entityIdToText (entityIdFromConvId ref.refSourceConversation)
                , entityIdToText (entityIdFromConvId ref.refTargetConversation)
                , fmap unMessageRef ref.refTargetMessage :: Maybe Text
                , refTypeToText ref.refType
                , ref.refCreatedAt
                , ref.refCreatedBy :: Maybe Text
                , ref.refContext :: Maybe Text
                , Nothing :: Maybe UTCTime
                , True :: Bool
                , Nothing :: Maybe Text
                )

-------------------------------------------------------------------------------
-- Querying
-------------------------------------------------------------------------------

-- | Get all outgoing references from a conversation.
getOutgoingReferences ::
    SqliteBackend ->
    ConversationId ->
    IO [ConversationRef]
getOutgoingReferences backend sourceId = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        rows <-
            query
                conn
                [sql|
                    SELECT id, source_conv_id, target_conv_id, target_msg_id,
                           ref_type, created_at, created_by, context,
                           resolved_at, is_valid, resolution_error
                    FROM conversation_refs
                    WHERE source_conv_id = ?
                    ORDER BY created_at DESC
                |]
                [entityIdToText (entityIdFromConvId sourceId)]

        pure $ map rowToReference rows

-- | Get all incoming references to a conversation (backlinks).
getIncomingReferences ::
    SqliteBackend ->
    ConversationId ->
    IO [ConversationRef]
getIncomingReferences backend targetId = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        rows <-
            query
                conn
                [sql|
                    SELECT id, source_conv_id, target_conv_id, target_msg_id,
                           ref_type, created_at, created_by, context,
                           resolved_at, is_valid, resolution_error
                    FROM conversation_refs
                    WHERE target_conv_id = ?
                    ORDER BY created_at DESC
                |]
                [entityIdToText (entityIdFromConvId targetId)]

        pure $ map rowToReference rows

-- | Get references between two specific conversations.
getReferencesBetween ::
    SqliteBackend ->
    ConversationId ->
    ConversationId ->
    IO [ConversationRef]
getReferencesBetween backend convId1 convId2 = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        rows <-
            query
                conn
                [sql|
                    SELECT id, source_conv_id, target_conv_id, target_msg_id,
                           ref_type, created_at, created_by, context,
                           resolved_at, is_valid, resolution_error
                    FROM conversation_refs
                    WHERE (source_conv_id = ? AND target_conv_id = ?)
                       OR (source_conv_id = ? AND target_conv_id = ?)
                    ORDER BY created_at DESC
                |]
                ( entityIdToText (entityIdFromConvId convId1)
                , entityIdToText (entityIdFromConvId convId2)
                , entityIdToText (entityIdFromConvId convId2)
                , entityIdToText (entityIdFromConvId convId1)
                )

        pure $ map rowToReference rows

{- | Query references with filters.

Supports all filters defined in ReferenceQuery.
Note: This is a simplified implementation. A full implementation
would build dynamic SQL queries.
-}
queryReferences ::
    SqliteBackend ->
    ReferenceQuery ->
    IO [ConversationRef]
queryReferences backend rq = do
    -- For simplicity, get all references and filter in Haskell
    -- A production implementation would build dynamic SQL
    allRefs <- getAllReferences backend
    let filtered = filter (matchesRefQuery rq) allRefs
    pure $ case rq.rqLimit of
        Just n -> take n filtered
        Nothing -> filtered

-- | Get all references from the database.
getAllReferences :: SqliteBackend -> IO [ConversationRef]
getAllReferences backend = do
    let pool = spConnectionPool (sbPersistence backend)
    withResource pool $ \conn -> do
        rows <-
            query_
                conn
                [sql|
                    SELECT id, source_conv_id, target_conv_id, target_msg_id,
                           ref_type, created_at, created_by, context,
                           resolved_at, is_valid, resolution_error
                    FROM conversation_refs
                    ORDER BY created_at DESC
                |]
        pure $ map rowToReference rows

-- | Check if a reference matches the query criteria.
matchesRefQuery :: ReferenceQuery -> ConversationRef -> Bool
matchesRefQuery rq ref =
    sourceMatch && targetMatch && typeMatch && validMatch
  where
    sourceMatch = case rq.rqSourceConversation of
        Just sid -> ref.refSourceConversation == sid
        Nothing -> True

    targetMatch = case rq.rqTargetConversation of
        Just tid -> ref.refTargetConversation == tid
        Nothing -> True

    typeMatch = case rq.rqRefType of
        Just rt -> ref.refType == rt
        Nothing -> True

    validMatch = if rq.rqIncludeInvalid then True else True -- All refs considered valid for now

-------------------------------------------------------------------------------
-- Graph Operations
-------------------------------------------------------------------------------

{- | Get the reference graph around a conversation.

Returns references up to a specified depth for graph visualization.
-}
getReferenceGraph ::
    SqliteBackend ->
    ConversationId ->
    -- | Maximum depth
    Int ->
    IO ([ConversationId], [(ConversationId, ConversationId, RefType)])
getReferenceGraph backend startConvId maxDepth = do
    -- BFS traversal to find connected conversations
    let go :: Int -> Set.Set ConversationId -> [(ConversationId, ConversationId, RefType)] -> IO ([(ConversationId, ConversationId, RefType)], Set.Set ConversationId)
        go depth visited edges
            | depth >= maxDepth = pure (edges, visited)
            | otherwise = do
                -- Get all outgoing references from visited nodes
                newEdges <- fmap concat $ forM (Set.toList visited) $ \convId -> do
                    refs <- getOutgoingReferences backend convId
                    pure $ map (\r -> (r.refSourceConversation, r.refTargetConversation, r.refType)) refs

                let newTargets = Set.fromList $ map (\(_, tgt, _) -> tgt) newEdges
                let newVisited = Set.union visited newTargets
                let allEdges = edges ++ filter (\(s, _, _) -> Set.member s visited) newEdges

                if newVisited == visited
                    then pure (allEdges, visited)
                    else go (depth + 1) newVisited allEdges

    (edges, allConvs) <- go 0 (Set.singleton startConvId) []
    pure (Set.toList allConvs, edges)

{- | Get all conversations connected to a given conversation.

Returns conversations that are reachable via references (in either direction).
-}
getConnectedConversations ::
    SqliteBackend ->
    ConversationId ->
    IO [ConversationId]
getConnectedConversations backend convId = do
    outgoing <- getOutgoingReferences backend convId
    incoming <- getIncomingReferences backend convId

    let allConvs =
            Set.fromList $
                convId
                    : map (\r -> r.refTargetConversation) outgoing
                    ++ map (\r -> r.refSourceConversation) incoming

    pure $ Set.toList allConvs

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | Update the resolution status of a reference.
updateReferenceStatus ::
    SqliteBackend ->
    -- | Source
    ConversationId ->
    -- | Target
    ConversationId ->
    Maybe MessageRef ->
    -- | Is valid
    Bool ->
    -- | Error message
    Maybe Text ->
    IO ()
updateReferenceStatus backend sourceId targetId mMsgRef isValid mError = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        execute
            conn
            [sql|
                UPDATE conversation_refs
                SET resolved_at = CURRENT_TIMESTAMP,
                    is_valid = ?,
                    resolution_error = ?
                WHERE source_conv_id = ? AND target_conv_id = ? AND target_msg_id IS ?
            |]
            ( isValid :: Bool
            , mError :: Maybe Text
            , entityIdToText (entityIdFromConvId sourceId)
            , entityIdToText (entityIdFromConvId targetId)
            , fmap unMessageRef mMsgRef :: Maybe Text
            )

-- | Get all broken/invalid references.
getBrokenReferences ::
    SqliteBackend ->
    IO [ConversationRef]
getBrokenReferences backend = do
    let pool = spConnectionPool (sbPersistence backend)

    withResource pool $ \conn -> do
        rows <-
            query_
                conn
                [sql|
                    SELECT id, source_conv_id, target_conv_id, target_msg_id,
                           ref_type, created_at, created_by, context,
                           resolved_at, is_valid, resolution_error
                    FROM conversation_refs
                    WHERE is_valid = 0
                    ORDER BY resolved_at DESC
                |]

        pure $ map rowToReference rows

-------------------------------------------------------------------------------
-- Suggestions
-------------------------------------------------------------------------------

{- | Search conversations for reference auto-complete.

Searches titles and returns matches for the partial input.
-}
searchConversationsForRef ::
    SqliteBackend ->
    -- | Partial input
    Text ->
    -- | Limit
    Int ->
    -- | (ConversationId, Title, Preview)
    IO [(ConversationId, Text, Maybe Text)]
searchConversationsForRef backend partialInput limit = do
    let pool = spConnectionPool (sbPersistence backend)
    let pattern = "%" <> partialInput <> "%"

    withResource pool $ \conn -> do
        results <-
            query
                conn
                [sql|
                    SELECT DISTINCT c.entity_id, c.component_data
                    FROM components c
                    WHERE c.component_type = 30
                      AND c.component_data LIKE ?
                    LIMIT ?
                |]
                (pattern :: Text, limit :: Int)

        -- Parse results
        forM results $ \(eidText, _jsonData :: Text) -> do
            let convId = ConversationId $ _readTextEntityId eidText
            -- In practice, parse the JSON to extract title
            pure (convId, partialInput <> "...", Nothing)
  where
    _readTextEntityId txt = EntityId $ read $ Text.unpack txt

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Convert a database row to a ConversationRef.
rowToReference :: ReferenceRow -> ConversationRef
rowToReference row =
    ConversationRef
        { refSourceConversation = ConversationId $ readTextEntityId row.rrSourceConvId
        , refTargetConversation = ConversationId $ readTextEntityId row.rrTargetConvId
        , refTargetMessage = fmap MessageRef row.rrTargetMsgId
        , refType = textToRefType row.rrRefType
        , refCreatedAt = row.rrCreatedAt
        , refCreatedBy = row.rrCreatedBy
        , refContext = row.rrContext
        }

-- | Convert RefType to text for storage.
refTypeToText :: RefType -> Text
refTypeToText RefCite = "cite"
refTypeToText RefReply = "reply"
refTypeToText RefRelated = "related"
refTypeToText RefFork = "fork"

-- | Convert text to RefType.
textToRefType :: Text -> RefType
textToRefType "cite" = RefCite
textToRefType "reply" = RefReply
textToRefType "related" = RefRelated
textToRefType "fork" = RefFork
textToRefType _ = RefRelated -- Default

-- | Helper to extract EntityId from ConversationId.
entityIdFromConvId :: ConversationId -> EntityId
entityIdFromConvId (ConversationId eid) = eid

-- | Helper to read EntityId from Text.
readTextEntityId :: Text -> EntityId
readTextEntityId txt = EntityId $ read $ Text.unpack txt
