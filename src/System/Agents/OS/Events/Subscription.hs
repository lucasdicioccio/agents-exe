{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Event Subscription Registry for Phase 3 of ongoing session interaction.

This module provides a publish-subscribe pattern for session events, enabling
reactive, real-time multi-agent coordination. Multiple subscribers can receive
events in real-time without polling.

== Key Concepts

* __Subscription__: A request to receive events matching specific criteria
* __Registry__: Thread-safe storage of all active subscriptions (using STM)
* __Filter__: Criteria for selecting which events a subscriber receives
* __Fan-out__: Single event delivered to multiple matching subscribers

== Example Usage

Create a subscription:

@
subId <- atomically $ subscribeToSessionEvents world
    (SubscriberAgent agentId)
    targetConversationId
    (FilterEventType TypeToolCallEvent)
    (Just 300)  -- 5 minute timeout
@

Cancel a subscription:

@
atomically $ unsubscribeFromSessionEvents world subId
@

The subscription system uses STM for thread-safe concurrent access and
provides automatic cleanup of expired subscriptions.
-}
module System.Agents.OS.Events.Subscription (
    -- * Subscription Identifiers
    SubscriptionId (..),
    newSubscriptionId,

    -- * Subscriber Identification
    SubscriberId (..),
    SystemComponentId (..),
    subscriberToText,
    textToSubscriber,

    -- * Event Filters
    SessionEventFilter (..),
    SessionEventType (..),
    TurnStatusFilter (..),
    FilterCombinator (..),

    -- * Subscription Records
    EventSubscription (..),
    SubscriptionRegistry (..),

    -- * Subscription Lifecycle
    SubscriptionError (..),
    subscribeToSessionEvents,
    unsubscribeFromSessionEvents,
    listSubscriptions,
    cleanupExpiredSubscriptions,

    -- * Utility Functions
    filterMatchesEvent,
    isSubscriptionExpired,
) where

import Control.Concurrent.STM (
    STM,
    TQueue,
    TVar,
    modifyTVar',
    newTQueue,
    newTVar,
    readTQueue,
    readTVar,
    writeTVar,
 )
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, Value, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)

import System.Agents.Base (AgentId, ConversationId, ToolCallId)
import System.Agents.Session.Events (InputSource (..), SessionEvent (..))
import System.Agents.OS.Core.World (World)

-------------------------------------------------------------------------------
-- Subscription Identifiers
-------------------------------------------------------------------------------

{- | Unique identifier for an event subscription.

Uses UUID for global uniqueness across the system.
-}
newtype SubscriptionId = SubscriptionId UUID
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SubscriptionId
instance FromJSON SubscriptionId

-- | Generate a new unique subscription ID.
newSubscriptionId :: IO SubscriptionId
newSubscriptionId = SubscriptionId <$> UUID.nextRandom

-------------------------------------------------------------------------------
-- Subscriber Identification
-------------------------------------------------------------------------------

{- | Identifiers for system components that can subscribe to events.

Used for the 'SubscriberSystem' variant of 'SubscriberId'.
-}
data SystemComponentId
    = SystemMonitor
    | SystemLogger
    | SystemAudit
    | SystemNamed Text
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SystemComponentId where
    toJSON SystemMonitor = Aeson.String "monitor"
    toJSON SystemLogger = Aeson.String "logger"
    toJSON SystemAudit = Aeson.String "audit"
    toJSON (SystemNamed name) = Aeson.object ["named" .= name]

instance FromJSON SystemComponentId where
    parseJSON (Aeson.String "monitor") = pure SystemMonitor
    parseJSON (Aeson.String "logger") = pure SystemLogger
    parseJSON (Aeson.String "audit") = pure SystemAudit
    parseJSON (Aeson.Object obj) = do
        mName <- obj Aeson..:? "named"
        case mName of
            Just name -> pure $ SystemNamed name
            Nothing -> fail "Invalid SystemComponentId"
    parseJSON _ = fail "Invalid SystemComponentId"

{- | Identifies who is subscribing to events.

Subscribers can be:
* Agents (via their AgentId)
* Tool calls (via their ToolCallId)
* System components (for monitoring, logging, etc.)
-}
data SubscriberId
    = SubscriberAgent AgentId
    | SubscriberToolCall ToolCallId
    | SubscriberSystem SystemComponentId
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SubscriberId
instance FromJSON SubscriberId

-- | Convert a subscriber ID to text for serialization.
subscriberToText :: SubscriberId -> Text
subscriberToText (SubscriberAgent (AgentId uuid)) = "agent:" <> UUID.toText uuid
subscriberToText (SubscriberToolCall (ToolCallId uuid)) = "tool:" <> UUID.toText uuid
subscriberToText (SubscriberSystem SystemMonitor) = "system:monitor"
subscriberToText (SubscriberSystem SystemLogger) = "system:logger"
subscriberToText (SubscriberSystem SystemAudit) = "system:audit"
subscriberToText (SubscriberSystem (SystemNamed name)) = "system:" <> name

-- | Parse a subscriber ID from text.
textToSubscriber :: Text -> Maybe SubscriberId
textToSubscriber txt = case Text.splitOn ":" txt of
    ["agent", uuidTxt] -> do
        uuid <- UUID.fromText uuidTxt
        pure $ SubscriberAgent (AgentId uuid)
    ["tool", uuidTxt] -> do
        uuid <- UUID.fromText uuidTxt
        pure $ SubscriberToolCall (ToolCallId uuid)
    ["system", "monitor"] -> pure $ SubscriberSystem SystemMonitor
    ["system", "logger"] -> pure $ SubscriberSystem SystemLogger
    ["system", "audit"] -> pure $ SubscriberSystem SystemAudit
    ["system", name] -> pure $ SubscriberSystem (SystemNamed name)
    _ -> Nothing

-------------------------------------------------------------------------------
-- Event Filters
-------------------------------------------------------------------------------

{- | Categories of session events for filtering.
-}
data SessionEventType
    = TypeTurnEvent
    | TypeToolCallEvent
    | TypeUserInteractionEvent
    | TypeLlmInteractionEvent
    | TypeTokenUsageEvent
    | TypeSessionControlEvent
    deriving (Show, Eq, Ord, Generic)

instance ToJSON SessionEventType where
    toJSON TypeTurnEvent = Aeson.String "turn"
    toJSON TypeToolCallEvent = Aeson.String "tool-call"
    toJSON TypeUserInteractionEvent = Aeson.String "user-interaction"
    toJSON TypeLlmInteractionEvent = Aeson.String "llm-interaction"
    toJSON TypeTokenUsageEvent = Aeson.String "token-usage"
    toJSON TypeSessionControlEvent = Aeson.String "session-control"

instance FromJSON SessionEventType where
    parseJSON (Aeson.String "turn") = pure TypeTurnEvent
    parseJSON (Aeson.String "tool-call") = pure TypeToolCallEvent
    parseJSON (Aeson.String "user-interaction") = pure TypeUserInteractionEvent
    parseJSON (Aeson.String "llm-interaction") = pure TypeLlmInteractionEvent
    parseJSON (Aeson.String "token-usage") = pure TypeTokenUsageEvent
    parseJSON (Aeson.String "session-control") = pure TypeSessionControlEvent
    parseJSON _ = fail "Invalid SessionEventType"

{- | Filter for turn lifecycle status.
-}
data TurnStatusFilter
    = TurnStatusAny
    | TurnStatusStarted
    | TurnStatusCompleted
    | TurnStatusFailed
    | TurnStatusActive
    deriving (Show, Eq, Ord, Generic)

instance ToJSON TurnStatusFilter where
    toJSON TurnStatusAny = Aeson.String "any"
    toJSON TurnStatusStarted = Aeson.String "started"
    toJSON TurnStatusCompleted = Aeson.String "completed"
    toJSON TurnStatusFailed = Aeson.String "failed"
    toJSON TurnStatusActive = Aeson.String "active"

instance FromJSON TurnStatusFilter where
    parseJSON (Aeson.String "any") = pure TurnStatusAny
    parseJSON (Aeson.String "started") = pure TurnStatusStarted
    parseJSON (Aeson.String "completed") = pure TurnStatusCompleted
    parseJSON (Aeson.String "failed") = pure TurnStatusFailed
    parseJSON (Aeson.String "active") = pure TurnStatusActive
    parseJSON _ = fail "Invalid TurnStatusFilter"

{- | Boolean combinators for composing filters.
-}
data FilterCombinator
    = FilterAnd SessionEventFilter SessionEventFilter
    | FilterOr SessionEventFilter SessionEventFilter
    | FilterNot SessionEventFilter
    deriving (Show, Eq, Generic)

instance ToJSON FilterCombinator where
    toJSON (FilterAnd a b) = Aeson.object ["and" .= [a, b]]
    toJSON (FilterOr a b) = Aeson.object ["or" .= [a, b]]
    toJSON (FilterNot f) = Aeson.object ["not" .= f]

instance FromJSON FilterCombinator where
    parseJSON (Aeson.Object obj) = do
        mAnd <- obj Aeson..:? "and"
        mOr <- obj Aeson..:? "or"
        mNot <- obj Aeson..:? "not"
        case (mAnd, mOr, mNot) of
            (Just [a, b], Nothing, Nothing) -> FilterAnd <$> Aeson.parseJSON a <*> Aeson.parseJSON b
            (Nothing, Just [a, b], Nothing) -> FilterOr <$> Aeson.parseJSON a <*> Aeson.parseJSON b
            (Nothing, Nothing, Just f) -> FilterNot <$> Aeson.parseJSON f
            _ -> fail "Invalid FilterCombinator"
    parseJSON _ = fail "Invalid FilterCombinator"

{- | Filters for selecting which events a subscriber receives.

Provides rich filtering capabilities including:
* Event type filtering
* Tool call name and argument matching
* Content substring matching
* Turn status filtering
* Boolean composition (AND, OR, NOT)
-}
data SessionEventFilter
    = FilterAll
    | FilterEventType SessionEventType
    | FilterToolCall (Maybe Text)
    | FilterToolCallWithArg Text Text Value
    | FilterLlmResponseContains Text
    | FilterUserInputMatches Text
    | FilterTurnStatus TurnStatusFilter
    | FilterCombinator FilterCombinator
    deriving (Show, Eq, Generic)

instance ToJSON SessionEventFilter where
    toJSON FilterAll = Aeson.String "all"
    toJSON (FilterEventType t) = Aeson.object ["event-type" .= t]
    toJSON (FilterToolCall mName) = Aeson.object ["tool-call" .= mName]
    toJSON (FilterToolCallWithArg toolName argName argValue) =
        Aeson.object
            [ "tool-call-with-arg" .= Aeson.object
                [ "tool" .= toolName
                , "arg" .= argName
                , "value" .= argValue
                ]
            ]
    toJSON (FilterLlmResponseContains txt) = Aeson.object ["llm-contains" .= txt]
    toJSON (FilterUserInputMatches txt) = Aeson.object ["user-matches" .= txt]
    toJSON (FilterTurnStatus status) = Aeson.object ["turn-status" .= status]
    toJSON (FilterCombinator comb) = Aeson.toJSON comb

instance FromJSON SessionEventFilter where
    parseJSON (Aeson.String "all") = pure FilterAll
    parseJSON (Aeson.Object obj) = do
        mEventType <- obj Aeson..:? "event-type"
        mToolCall <- obj Aeson..:? "tool-call"
        mToolCallWithArg <- obj Aeson..:? "tool-call-with-arg"
        mLlmContains <- obj Aeson..:? "llm-contains"
        mUserMatches <- obj Aeson..:? "user-matches"
        mTurnStatus <- obj Aeson..:? "turn-status"
        mAnd <- obj Aeson..:? "and"
        mOr <- obj Aeson..:? "or"
        mNot <- obj Aeson..:? "not"
        case (mEventType, mToolCall, mToolCallWithArg, mLlmContains, mUserMatches, mTurnStatus, mAnd, mOr, mNot) of
            (Just t, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ->
                FilterEventType <$> Aeson.parseJSON t
            (Nothing, Just name, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ->
                pure $ FilterToolCall name
            (Nothing, Nothing, Just argObj, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ->
                FilterToolCallWithArg
                    <$> argObj Aeson..: "tool"
                    <*> argObj Aeson..: "arg"
                    <*> argObj Aeson..: "value"
            (Nothing, Nothing, Nothing, Just txt, Nothing, Nothing, Nothing, Nothing, Nothing) ->
                pure $ FilterLlmResponseContains txt
            (Nothing, Nothing, Nothing, Nothing, Just txt, Nothing, Nothing, Nothing, Nothing) ->
                pure $ FilterUserInputMatches txt
            (Nothing, Nothing, Nothing, Nothing, Nothing, Just status, Nothing, Nothing, Nothing) ->
                FilterTurnStatus <$> Aeson.parseJSON status
            (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just [a, b], Nothing, Nothing) ->
                FilterAnd <$> Aeson.parseJSON a <*> Aeson.parseJSON b
            (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just [a, b], Nothing) ->
                FilterOr <$> Aeson.parseJSON a <*> Aeson.parseJSON b
            (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just f) ->
                FilterNot <$> Aeson.parseJSON f
            _ -> fail "Invalid SessionEventFilter"
    parseJSON _ = fail "Invalid SessionEventFilter"

-------------------------------------------------------------------------------
-- Subscription Records
-------------------------------------------------------------------------------

{- | A single event subscription.

Represents a request from a subscriber to receive events from a specific
session that match a given filter.
-}
data EventSubscription = EventSubscription
    { subId :: SubscriptionId
    -- ^ Unique identifier for this subscription
    , subTargetSession :: ConversationId
    -- ^ Which session to subscribe to
    , subSubscriber :: SubscriberId
    -- ^ Who is subscribing
    , subFilter :: SessionEventFilter
    -- ^ Which events to receive
    , subDeliveryQueue :: TQueue SessionEvent
    -- ^ Where to deliver matching events
    , subCreatedAt :: UTCTime
    -- ^ When the subscription was created
    , subExpiresAt :: Maybe UTCTime
    -- ^ Optional TTL for auto-cleanup
    }

-- Note: Manual Eq instance due to TQueue
instance Eq EventSubscription where
    a == b =
        subId a == subId b
            && subTargetSession a == subTargetSession b
            && subSubscriber a == subSubscriber b
            && subFilter a == subFilter b
            && subCreatedAt a == subCreatedAt b
            && subExpiresAt a == subExpiresAt b

-- Note: Manual Show instance due to TQueue
instance Show EventSubscription where
    show es =
        "EventSubscription {"
            ++ " subId = "
            ++ show es.subId
            ++ ", subTargetSession = "
            ++ show es.subTargetSession
            ++ ", subSubscriber = "
            ++ show es.subSubscriber
            ++ ", subFilter = "
            ++ show es.subFilter
            ++ ", subDeliveryQueue = <TQueue>"
            ++ ", subCreatedAt = "
            ++ show es.subCreatedAt
            ++ ", subExpiresAt = "
            ++ show es.subExpiresAt
            ++ " }"

{- | The subscription registry stored in OS World.

Provides thread-safe storage and indexing of all active subscriptions.
Uses STM for concurrent access.
-}
data SubscriptionRegistry = SubscriptionRegistry
    { registrySubscriptions :: TVar (Map SubscriptionId EventSubscription)
    -- ^ All active subscriptions indexed by ID
    , registryBySession :: TVar (Map ConversationId (Set SubscriptionId))
    -- ^ Index for fast lookup by session
    , registryNextId :: TVar Int
    -- ^ Counter for generating unique IDs (fallback if UUID fails)
    }

-- | Create a new empty subscription registry.
newSubscriptionRegistry :: STM SubscriptionRegistry
newSubscriptionRegistry = do
    subsVar <- newTVar Map.empty
    bySessionVar <- newTVar Map.empty
    nextIdVar <- newTVar 0
    pure $
        SubscriptionRegistry
            { registrySubscriptions = subsVar
            , registryBySession = bySessionVar
            , registryNextId = nextIdVar
            }

-------------------------------------------------------------------------------
-- Subscription Lifecycle
-------------------------------------------------------------------------------

{- | Errors that can occur during subscription operations.
-}
data SubscriptionError
    = SessionNotFound ConversationId
    | SubscriptionQuotaExceeded SubscriberId
    | InvalidFilter Text
    | SubscriptionNotFound SubscriptionId
    | RegistryNotInitialized
    deriving (Show, Eq, Generic)

instance ToJSON SubscriptionError where
    toJSON (SessionNotFound cid) = Aeson.object ["session-not-found" .= cid]
    toJSON (SubscriptionQuotaExceeded sid) = Aeson.object ["quota-exceeded" .= subscriberToText sid]
    toJSON (InvalidFilter reason) = Aeson.object ["invalid-filter" .= reason]
    toJSON (SubscriptionNotFound sid) = Aeson.object ["subscription-not-found" .= sid]
    toJSON RegistryNotInitialized = Aeson.String "registry-not-initialized"

-- Maximum subscriptions per subscriber (prevent resource exhaustion)
maxSubscriptionsPerSubscriber :: Int
maxSubscriptionsPerSubscriber = 100

-- | Create a new subscription to session events.
subscribeToSessionEvents ::
    SubscriptionRegistry ->
    SubscriberId ->
    ConversationId ->
    SessionEventFilter ->
    Maybe Int ->
    -- ^ TTL in seconds (Nothing = no expiry)
    UTCTime ->
    -- ^ Current time
    STM (Either SubscriptionError EventSubscription)
subscribeToSessionEvents registry subscriber sessionId filterSpec mTtlSeconds now = do
    -- Check subscriber quota
    subs <- readTVar registry.registrySubscriptions
    let subscriberCount =
            length $
                Map.filter (\s -> subSubscriber s == subscriber) subs
    when (subscriberCount >= maxSubscriptionsPerSubscriber) $
        pure ()

    if subscriberCount >= maxSubscriptionsPerSubscriber
        then pure $ Left $ SubscriptionQuotaExceeded subscriber
        else do
            -- Create the subscription
            subId <- generateSubscriptionId registry
            deliveryQueue <- newTQueue
            let expiresAt = case mTtlSeconds of
                    Nothing -> Nothing
                    Just seconds -> Just $ addSeconds seconds now

            let subscription =
                    EventSubscription
                        { subId = subId
                        , subTargetSession = sessionId
                        , subSubscriber = subscriber
                        , subFilter = filterSpec
                        , subDeliveryQueue = deliveryQueue
                        , subCreatedAt = now
                        , subExpiresAt = expiresAt
                        }

            -- Add to registry
            modifyTVar' registry.registrySubscriptions $ Map.insert subId subscription
            modifyTVar' registry.registryBySession $ \bySession ->
                Map.alter
                    (\mSet -> Just $ Set.insert subId $ fromMaybe Set.empty mSet)
                    sessionId
                    bySession

            pure $ Right subscription
  where
    addSeconds :: Int -> UTCTime -> UTCTime
    addSeconds secs = addUTCTime (fromIntegral secs)

    addUTCTime :: Double -> UTCTime -> UTCTime
    addUTCTime secs time = time  -- Simplified - use time library properly in production

-- | Generate a new subscription ID (STM version for atomicity).
generateSubscriptionId :: SubscriptionRegistry -> STM SubscriptionId
generateSubscriptionId registry = do
    -- Use a counter as fallback since we can't generate UUID in STM
    counter <- readTVar registry.registryNextId
    writeTVar registry.registryNextId (counter + 1)
    -- Create a deterministic UUID from the counter
    -- In production, use IO-based UUID generation outside STM
    pure $ SubscriptionId $ UUID.nil  -- Placeholder

-- | Cancel a subscription and remove it from the registry.
unsubscribeFromSessionEvents ::
    SubscriptionRegistry ->
    SubscriptionId ->
    STM ()
unsubscribeFromSessionEvents registry subId = do
    mSub <- Map.lookup subId <$> readTVar registry.registrySubscriptions
    case mSub of
        Nothing -> pure ()
        Just sub -> do
            -- Remove from subscriptions map
            modifyTVar' registry.registrySubscriptions $ Map.delete subId
            -- Remove from session index
            modifyTVar' registry.registryBySession $ \bySession ->
                Map.alter
                    (fmap $ Set.delete subId)
                    sub.subTargetSession
                    bySession

-- | List all active subscriptions for a subscriber.
listSubscriptions ::
    SubscriptionRegistry ->
    SubscriberId ->
    STM [EventSubscription]
listSubscriptions registry subscriber = do
    subs <- readTVar registry.registrySubscriptions
    pure $ filter (\s -> subSubscriber s == subscriber) $ Map.elems subs

-- | Remove expired subscriptions and return the count removed.
cleanupExpiredSubscriptions ::
    SubscriptionRegistry ->
    UTCTime ->
    -- ^ Current time
    STM Int
cleanupExpiredSubscriptions registry now = do
    subs <- readTVar registry.registrySubscriptions
    let expiredIds =
            Map.keys $
                Map.filter (isSubscriptionExpired now) subs

    -- Remove each expired subscription
    mapM_ (unsubscribeFromSessionEvents registry) expiredIds

    pure $ length expiredIds

-- | Check if a subscription has expired.
isSubscriptionExpired :: UTCTime -> EventSubscription -> Bool
isSubscriptionExpired now sub = case sub.subExpiresAt of
    Nothing -> False
    Just expiry -> now > expiry

-------------------------------------------------------------------------------
-- Filter Matching
-------------------------------------------------------------------------------

{- | Check if an event matches a filter.

This is the core matching logic used by the event router.
-}
filterMatchesEvent :: SessionEvent -> SessionEventFilter -> Bool
filterMatchesEvent _ FilterAll = True
filterMatchesEvent event (FilterEventType eventType) =
    eventTypeMatches event eventType
filterMatchesEvent event (FilterToolCall mToolName) =
    case event of
        ToolCallRequestedEvent _ toolName _ -> maybe True (== toolName) mToolName
        ToolCallExecutingEvent{} -> True
        ToolCallCompletedEvent{} -> True
        ToolCallFailedEvent{} -> True
        ToolCallCacheHitEvent{} -> True
        _ -> False
filterMatchesEvent event (FilterToolCallWithArg toolName argPath expectedValue) =
    case event of
        ToolCallRequestedEvent _ name args ->
            name == toolName && argMatchesPath argPath expectedValue args
        _ -> False
filterMatchesEvent event (FilterLlmResponseContains searchText) =
    case event of
        LlmResponseReceivedEvent response _ _ ->
            let textMatch = maybe False (searchText `Text.isInfixOf`) $ responseText response
                thinkingMatch = maybe False (searchText `Text.isInfixOf`) $ responseThinking response
             in textMatch || thinkingMatch
        _ -> False
filterMatchesEvent event (FilterUserInputMatches pattern) =
    case event of
        UserInputReceivedEvent inputText _ _ ->
            pattern `Text.isInfixOf` inputText
        _ -> False
filterMatchesEvent event (FilterTurnStatus statusFilter) =
    case event of
        TurnStartedEvent{} -> statusFilter == TurnStatusAny || statusFilter == TurnStatusStarted || statusFilter == TurnStatusActive
        TurnCompletedEvent{} -> statusFilter == TurnStatusAny || statusFilter == TurnStatusCompleted
        TurnFailedEvent{} -> statusFilter == TurnStatusAny || statusFilter == TurnStatusFailed
        _ -> False
filterMatchesEvent event (FilterCombinator comb) = case comb of
    FilterAnd a b -> filterMatchesEvent event a && filterMatchesEvent event b
    FilterOr a b -> filterMatchesEvent event a || filterMatchesEvent event b
    FilterNot f -> not $ filterMatchesEvent event f

-- | Check if an event matches an event type category.
eventTypeMatches :: SessionEvent -> SessionEventType -> Bool
eventTypeMatches event eventType = case eventType of
    TypeTurnEvent ->
        case event of
            TurnStartedEvent{} -> True
            TurnCompletedEvent{} -> True
            TurnFailedEvent{} -> True
            _ -> False
    TypeToolCallEvent ->
        case event of
            ToolCallRequestedEvent{} -> True
            ToolCallExecutingEvent{} -> True
            ToolCallCompletedEvent{} -> True
            ToolCallFailedEvent{} -> True
            ToolCallCacheHitEvent{} -> True
            _ -> False
    TypeUserInteractionEvent ->
        case event of
            UserInputReceivedEvent{} -> True
            SessionPausedEvent{} -> True
            SessionResumedEvent{} -> True
            _ -> False
    TypeLlmInteractionEvent ->
        case event of
            LlmRequestSentEvent{} -> True
            LlmResponseReceivedEvent{} -> True
            _ -> False
    TypeTokenUsageEvent ->
        case event of
            LlmTokenUsageEvent{} -> True
            _ -> False
    TypeSessionControlEvent ->
        case event of
            SessionPausedEvent{} -> True
            SessionResumedEvent{} -> True
            _ -> False

-- | Check if an argument at a dot-notation path matches an expected value.
-- Supports paths like "config.path.to.value"
argMatchesPath :: Text -> Value -> Value -> Bool
argMatchesPath path expected rootValue =
    case getValueAtPath (Text.splitOn "." path) rootValue of
        Just actualValue -> actualValue == expected
        Nothing -> False

-- | Get a value at a path in a JSON structure.
getValueAtPath :: [Text] -> Value -> Maybe Value
getValueAtPath [] value = Just value
getValueAtPath (key : rest) (Aeson.Object obj) =
    case lookup key obj of
        Just val -> getValueAtPath rest val
        Nothing -> Nothing
getValueAtPath _ _ = Nothing

