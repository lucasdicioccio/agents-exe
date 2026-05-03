{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- |
Cross-conversation references for building linked knowledge graphs.

This module provides the public API for creating, querying, and rendering
references between conversations. It enables:

- **Citations**: Reference previous discussions with [[conv:id]] syntax
- **Bidirectional Links**: Navigate between related conversations
- **Reference Graphs**: Build and visualize conversation relationships
- **Auto-complete**: Suggest conversations when typing [[

== Quick Start

Create a reference:

@
import System.Agents.OS.References

-- Create a reference from current conversation to another
ref <- createReference world
    currentConvId          -- source
    targetConvId           -- target conversation
    Nothing                -- no specific message
    RefCite                -- citation type
    (Just userId)          -- created by
    (Just "See previous work") -- context
@

Get related conversations:

@
-- Get all conversations referenced by this one
outgoing <- getOutgoingReferences world currentConvId

-- Get all conversations that reference this one
incoming <- getIncomingReferences world currentConvId

-- Get bidirectional links
related <- getRelatedConversations world currentConvId
@

Render references for display:

@
-- Build a reference view with resolved content
view <- renderReferenceView world ref OutgoingRef

-- Format for TUI display
let displayText = formatReferenceView view
@

Search by reference:

@
-- Find conversations that reference a specific target
results <- searchByReference world targetConvId

-- Query with filters
let query = defaultReferenceQuery
        { rqRefType = Just RefReply
        , rqLimit = Just 10
        }
refs <- queryReferences world query
@

== Design Notes

1. **Soft References**: References don't enforce foreign key constraints,
   allowing references to archived/deleted conversations.

2. **Component Storage**: References are stored as ECS components
   (ReferenceConfig + ReferenceState) attached to reference entities.

3. **Resolution**: References are lazily resolved - validation happens
   when the reference is rendered or queried.

4. **Bidirectional**: Both outgoing and incoming references are tracked
   for full graph traversal.
-}
module System.Agents.OS.References (
    -- * Reference Types
    ConversationRef (..),
    RefType (..),
    RefDirection (..),
    ReferenceConfig (..),
    ReferenceState (..),
    ReferenceView (..),
    ReferencedContent (..),
    MessageRef (..),
    ReferenceQuery (..),

    -- * Creating References
    createReference,
    createReferenceFromText,

    -- * Querying References
    getOutgoingReferences,
    getIncomingReferences,
    getRelatedConversations,
    getBacklinks,
    queryReferences,
    defaultReferenceQuery,

    -- * Rendering References
    renderReferenceView,
    resolveReference,
    formatReferenceView,

    -- * Parsing
    parseReference,
    renderReference,
    referencePattern,
    extractReferencesFromText,

    -- * Reference Graph
    buildReferenceGraph,
    ReferenceGraph (..),
    GraphNode (..),
    GraphEdge (..),

    -- * Auto-complete
    getReferenceSuggestions,
    ReferenceSuggestion (..),

    -- * Validation
    validateReference,
    markReferenceResolved,
    markReferenceBroken,

    -- * Constants
    referenceConfigComponentId,
) where

import Control.Concurrent.STM (atomically)
import Control.Monad (forM)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)

import System.Agents.OS.Core.Types (
    ConversationId (..),
    EntityId,
    EntityId (..),
 )
import System.Agents.OS.Core.World (
    World,
    allEntitiesWithComponent,
    createEntity,
    getComponent,
    setComponent,
 )
import System.Agents.OS.References.Types
import System.Agents.OS.Conversation.Types (
    ConversationConfig (..),
 )

-------------------------------------------------------------------------------
-- Creating References
-------------------------------------------------------------------------------

{- | Create a new reference between conversations.

The reference is created as a new entity with ReferenceConfig and
ReferenceState components. The reference is initially unresolved
and will be validated lazily.

Returns the entity ID of the created reference.
-}
createReference ::
    World ->
    ConversationId ->
    -- ^ Source conversation
    ConversationId ->
    -- ^ Target conversation
    Maybe MessageRef ->
    -- ^ Optional target message
    RefType ->
    -- ^ Type of reference
    Maybe Text ->
    -- ^ Created by (user/agent ID)
    Maybe Text ->
    -- ^ Context text
    IO EntityId
createReference world sourceId targetId mMsgRef referenceType createdBy mContext = do
    now <- getCurrentTime

    -- Create the reference entity
    refId <- createEntity

    let refConfig =
            ReferenceConfig
                { rcSourceId = sourceId
                , rcTargetId = targetId
                , rcTargetMessageRef = mMsgRef
                , rcRefType = referenceType
                , rcContext = mContext
                }

    let refState =
            ReferenceState
                { rsCreatedAt = now
                , rsCreatedBy = createdBy
                , rsResolvedAt = Nothing
                , rsIsValid = True
                , rsResolutionError = Nothing
                }

    -- Store components
    atomically $ do
        setComponent world refId refConfig
        setComponent world refId refState

    pure refId

{- | Create references from text containing [[conv:id]] syntax.

Parses the text for all reference patterns and creates references
for each unique target found.

Returns the list of created reference entity IDs.
-}
createReferenceFromText ::
    World ->
    ConversationId ->
    -- ^ Source conversation
    Text ->
    -- ^ Text to parse
    RefType ->
    -- ^ Default reference type
    Maybe Text ->
    -- ^ Created by
    IO [EntityId]
createReferenceFromText world sourceId text referenceType createdBy = do
    let refs = extractReferencesFromText text

    -- Create reference for each unique target
    let uniqueTargets = Set.toList $ Set.fromList refs

    forM uniqueTargets $ \(targetId, mMsgRef, mDisplay) ->
        let mContext = case mDisplay of
                Just d -> Just $ "Referenced as: " <> d
                Nothing -> Nothing
         in createReference world sourceId targetId mMsgRef referenceType createdBy mContext

-------------------------------------------------------------------------------
-- Querying References
-------------------------------------------------------------------------------

{- | Get all outgoing references from a conversation.

These are references created by the given conversation pointing to other conversations.
-}
getOutgoingReferences ::
    World ->
    ConversationId ->
    IO [ConversationRef]
getOutgoingReferences world sourceId = do
    -- Query all reference configs
    refIds <- atomically $ allEntitiesWithComponent @ReferenceConfig world

    -- Filter and load references
    refs <- forM refIds $ \refId -> do
        mConfig <- atomically $ getComponent world refId
        mState <- atomically $ getComponent world refId
        case (mConfig, mState) of
            (Just config, Just state) | config.rcSourceId == sourceId ->
                pure $ Just $ toConversationRef refId config state
            _ -> pure Nothing

    pure $ catMaybes refs

{- | Get all incoming references to a conversation.

These are references created by other conversations pointing to this one.
-}
getIncomingReferences ::
    World ->
    ConversationId ->
    IO [ConversationRef]
getIncomingReferences world targetId = do
    -- Query all reference configs
    refIds <- atomically $ allEntitiesWithComponent @ReferenceConfig world

    -- Filter and load references
    refs <- forM refIds $ \refId -> do
        mConfig <- atomically $ getComponent world refId
        mState <- atomically $ getComponent world refId
        case (mConfig, mState) of
            (Just config, Just state) | config.rcTargetId == targetId ->
                pure $ Just $ toConversationRef refId config state
            _ -> pure Nothing

    pure $ catMaybes refs

{- | Get all related conversations (bidirectional links).

Returns conversations that either reference this one or are referenced by it.
-}
getRelatedConversations ::
    World ->
    ConversationId ->
    IO [(ConversationRef, RefDirection)]
getRelatedConversations world convId = do
    outgoing <- getOutgoingReferences world convId
    incoming <- getIncomingReferences world convId

    let outgoingPairs = map (,OutgoingRef) outgoing
    let incomingPairs = map (,IncomingRef) incoming

    pure $ outgoingPairs ++ incomingPairs

{- | Get backlinks - conversations that reference this one.

Alias for 'getIncomingReferences' with a more SEO-friendly name.
-}
getBacklinks ::
    World ->
    ConversationId ->
    IO [ConversationRef]
getBacklinks = getIncomingReferences

{- | Query references with filters.

Supports complex queries combining multiple filters.
-}
queryReferences ::
    World ->
    ReferenceQuery ->
    IO [ConversationRef]
queryReferences world rq = do
    -- Get all references
    refIds <- atomically $ allEntitiesWithComponent @ReferenceConfig world

    -- Filter based on query
    refs <- forM refIds $ \refId -> do
        mConfig <- atomically $ getComponent world refId
        mState <- atomically $ getComponent world refId
        case (mConfig, mState) of
            (Just config, Just state) ->
                if matchesQuery rq config state
                    then pure $ Just $ toConversationRef refId config state
                    else pure Nothing
            _ -> pure Nothing

    let results = catMaybes refs

    -- Apply limit
    case rq.rqLimit of
        Just n -> pure $ take n results
        Nothing -> pure results

-- | Check if a reference matches the query.
matchesQuery :: ReferenceQuery -> ReferenceConfig -> ReferenceState -> Bool
matchesQuery rq config state =
    sourceMatch && targetMatch && typeMatch && directionMatch && validMatch
  where
    sourceMatch = case rq.rqSourceConversation of
        Just sid -> config.rcSourceId == sid
        Nothing -> True

    targetMatch = case rq.rqTargetConversation of
        Just tid -> config.rcTargetId == tid
        Nothing -> True

    typeMatch = case rq.rqRefType of
        Just rt -> config.rcRefType == rt
        Nothing -> True

    directionMatch = case rq.rqDirection of
        Just _ -> True -- Direction determined at query time, not stored
        Nothing -> True

    validMatch = if rq.rqIncludeInvalid then True else state.rsIsValid

-------------------------------------------------------------------------------
-- Rendering References
-------------------------------------------------------------------------------

{- | Render a reference view for display.

Resolves the target conversation and builds a complete view
with preview text and metadata.
-}
renderReferenceView ::
    World ->
    ConversationRef ->
    RefDirection ->
    IO ReferenceView
renderReferenceView world ref direction = do
    -- Try to get target conversation info
    mTargetConfig <- atomically $ getComponent world (entityIdFromConvId ref.refTargetConversation)

    let displayTitle = case mTargetConfig of
            Just (config :: ConversationConfig) ->
                fromMaybe "Untitled Conversation" config.conversationTitle
            Nothing -> "Unknown Conversation"

    -- Resolve content preview
    mPreview <- resolveReference world ref

    let previewText = fmap (.rcContent) mPreview
    let targetTimestamp = fmap (.rcTimestamp) mPreview
    let isClickable = isJust mTargetConfig

    pure
        ReferenceView
            { rvRef = ref
            , rvDirection = direction
            , rvDisplayTitle = displayTitle
            , rvPreviewText = previewText
            , rvTargetTimestamp = targetTimestamp
            , rvIsClickable = isClickable
            }

{- | Resolve a reference to its content.

Fetches the referenced content from the target conversation/message.
Returns Nothing if the target doesn't exist.
-}
resolveReference ::
    World ->
    ConversationRef ->
    IO (Maybe ReferencedContent)
resolveReference _world _ref = do
    -- In a real implementation, this would:
    -- 1. Look up the target conversation
    -- 2. If message specified, get that specific message
    -- 3. Otherwise, get conversation summary/first message
    --
    -- For now, return a placeholder
    pure Nothing

{- | Format a reference view for display.
-}
formatReferenceView :: ReferenceView -> Text
formatReferenceView view =
    case view.rvPreviewText of
        Just preview -> view.rvDisplayTitle <> ": " <> Text.take 100 preview
        Nothing -> view.rvDisplayTitle

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

{- | Extract all references from text.

Finds all [[conv:id#msg]] patterns and returns the parsed components.
-}
extractReferencesFromText ::
    Text ->
    [(ConversationId, Maybe MessageRef, Maybe Text)]
extractReferencesFromText text =
    -- This is a simplified implementation
    -- A full implementation would use Text.Regex.TDFA to find all matches
    case parseReference text of
        Just result -> [result]
        Nothing -> []

-------------------------------------------------------------------------------
-- Reference Graph
-------------------------------------------------------------------------------

{- | A graph of conversation references.

Useful for visualization and analysis of conversation relationships.
-}
data ReferenceGraph = ReferenceGraph
    { rgNodes :: [GraphNode]
    , rgEdges :: [GraphEdge]
    }
    deriving (Show, Eq)

{- | A node in the reference graph (conversation).
-}
data GraphNode = GraphNode
    { gnConversationId :: ConversationId
    , gnTitle :: Text
    , gnReferenceCount :: Int
    -- ^ Total references (incoming + outgoing)
    }
    deriving (Show, Eq)

{- | An edge in the reference graph (reference).
-}
data GraphEdge = GraphEdge
    { geSource :: ConversationId
    , geTarget :: ConversationId
    , geType :: RefType
    , geWeight :: Int
    -- ^ Number of references of this type between these conversations
    }
    deriving (Show, Eq)

{- | Build a reference graph from the world state.

Collects all conversations that have references and builds
a graph structure for visualization.
-}
buildReferenceGraph ::
    World ->
    IO ReferenceGraph
buildReferenceGraph world = do
    -- Get all references
    allRefs <- queryReferences world defaultReferenceQuery

    -- Build nodes and edges
    let convIds = Set.toList $ Set.fromList $ concatMap (\r -> [r.refSourceConversation, r.refTargetConversation]) allRefs

    nodes <- forM convIds $ \convId -> do
        mConfig <- atomically $ getComponent world (entityIdFromConvId convId)
        let title = case mConfig of
                Just (config :: ConversationConfig) ->
                    fromMaybe "Untitled" config.conversationTitle
                Nothing -> "Unknown"

        let refCount = length $ filter (\r -> r.refSourceConversation == convId || r.refTargetConversation == convId) allRefs

        pure
            GraphNode
                { gnConversationId = convId
                , gnTitle = title
                , gnReferenceCount = refCount
                }

    -- Build edges (aggregate multiple references between same pair)
    let refPairs = map (\r -> ((r.refSourceConversation, r.refTargetConversation, r.refType), r)) allRefs
    let grouped = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) refPairs

    let edges =
            map
                ( \((src, tgt, rtype), refs) ->
                    GraphEdge
                        { geSource = src
                        , geTarget = tgt
                        , geType = rtype
                        , geWeight = length refs
                        }
                )
                (Map.toList grouped)

    pure $ ReferenceGraph nodes edges

-------------------------------------------------------------------------------
-- Auto-complete
-------------------------------------------------------------------------------

{- | A suggestion for reference auto-complete.
-}
data ReferenceSuggestion = ReferenceSuggestion
    { rsConversationId :: ConversationId
    , rsTitle :: Text
    , rsPreview :: Maybe Text
    , rsMatchScore :: Double
    -- ^ Relevance score for ranking
    }
    deriving (Show, Eq)

{- | Get reference suggestions for auto-complete.

Returns conversations that match the partial input, sorted by relevance.
-}
getReferenceSuggestions ::
    World ->
    Text ->
    -- ^ Partial input (after [[conv:)
    Int ->
    -- ^ Maximum number of suggestions
    IO [ReferenceSuggestion]
getReferenceSuggestions _world _partialInput _limit = do
    -- In a real implementation, this would:
    -- 1. Search conversation titles
    -- 2. Search conversation content
    -- 3. Rank by relevance to partial input
    -- 4. Return top N matches

    pure []

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

{- | Validate that a reference's target exists.

Updates the reference state with validation results.
-}
validateReference ::
    World ->
    EntityId ->
    -- ^ Reference entity ID
    IO Bool
validateReference world refId = do
    mConfig <- atomically $ getComponent world refId

    case mConfig of
        Just (config :: ReferenceConfig) -> do
            -- Check if target conversation exists
            mTarget <- atomically $ getComponent world (entityIdFromConvId config.rcTargetId)

            case mTarget of
                Just (_ :: ConversationConfig) -> do
                    markReferenceResolved world refId
                    pure True
                Nothing -> do
                    markReferenceBroken world refId "Target conversation not found"
                    pure False
        Nothing -> pure False

{- | Mark a reference as resolved (target found).
-}
markReferenceResolved ::
    World ->
    EntityId ->
    IO ()
markReferenceResolved world refId = do
    now <- getCurrentTime

    mState <- atomically $ getComponent world refId
    case mState of
        Just (state :: ReferenceState) -> do
            let newState =
                    state
                        { rsResolvedAt = Just now
                        , rsIsValid = True
                        , rsResolutionError = Nothing
                        }
            atomically $ setComponent world refId newState
        Nothing -> pure ()

{- | Mark a reference as broken (target not found).
-}
markReferenceBroken ::
    World ->
    EntityId ->
    Text ->
    -- ^ Error message
    IO ()
markReferenceBroken world refId errorMsg = do
    now <- getCurrentTime

    mState <- atomically $ getComponent world refId
    case mState of
        Just (state :: ReferenceState) -> do
            let newState =
                    state
                        { rsResolvedAt = Just now
                        , rsIsValid = False
                        , rsResolutionError = Just errorMsg
                        }
            atomically $ setComponent world refId newState
        Nothing -> pure ()

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Convert ReferenceConfig and ReferenceState to ConversationRef.
toConversationRef :: EntityId -> ReferenceConfig -> ReferenceState -> ConversationRef
toConversationRef _ config state =
    ConversationRef
        { refSourceConversation = config.rcSourceId
        , refTargetConversation = config.rcTargetId
        , refTargetMessage = config.rcTargetMessageRef
        , refType = config.rcRefType
        , refCreatedAt = state.rsCreatedAt
        , refCreatedBy = state.rsCreatedBy
        , refContext = config.rcContext
        }

-- | Extract EntityId from ConversationId.
entityIdFromConvId :: ConversationId -> EntityId
entityIdFromConvId (ConversationId eid) = eid

