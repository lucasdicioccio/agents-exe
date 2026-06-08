{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Event Router for Phase 3 of ongoing session interaction.

This module provides the event routing and fan-out functionality for the
subscription system. When a session emits an event, the router delivers it
to all matching subscribers.

== Key Concepts

* __Routing__: Directing events to appropriate subscribers based on filters
* __Fan-out__: One event delivered to many subscribers
* __Non-blocking__: Delivery via TQueue never blocks the emitter

== Example Usage

Route an event to all subscribers:

@
import System.Agents.OS.Events.Router

-- In the session loop after emitting an event:
atomically $ routeEvent registry conversationId sessionEvent
@

Check if a specific event matches a filter:

@
let matches = eventMatchesFilter event filterSpec
@

== Design Notes

The router is designed for minimal latency and maximum throughput:

1. Events are delivered via TQueue (non-blocking STM)
2. The session index allows O(1) lookup of subscribers by conversation
3. Filter matching is pure and can be parallelized if needed
4. Failed deliveries are silently dropped (no backpressure on emitter)
-}
module System.Agents.OS.Events.Router (
    -- * Event Routing
    routeEvent,
    routeEventToSubscriber,
    getMatchingSubscribers,
    countMatchingSubscribers,

    -- * Filter Matching
    eventMatchesFilter,
) where

import Control.Concurrent.STM (
    STM,
    writeTQueue,
 )
import Control.Monad (forM_, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Base (ConversationId)
import System.Agents.OS.Events.Subscription (
    EventSubscription (..),
    FilterCombinator (..),
    SessionEventFilter (..),
    SessionEventType (..),
    SubscriptionId,
    SubscriptionRegistry (..),
    TurnStatusFilter (..),
    filterMatchesEvent,
 )
import System.Agents.Session.Events (SessionEvent (..))

-------------------------------------------------------------------------------
-- Event Routing
-------------------------------------------------------------------------------

{- | Route an event to all matching subscribers for a session.

Looks up subscribers for the given conversation and delivers the event
to each subscriber whose filter matches the event.

Returns the number of subscribers notified.

This function is non-blocking - it uses TQueue write which never blocks
in STM (the queue grows if consumers are slow).
-}
routeEvent ::
    SubscriptionRegistry ->
    ConversationId ->
    SessionEvent ->
    STM Int
routeEvent registry sessionId event = do
    -- Get all subscriber IDs for this session
    bySession <- readTVar registry.registryBySession
    let subscriberIds = fromMaybe Set.empty $ Map.lookup sessionId bySession

    -- Get the full subscription records
    allSubs <- readTVar registry.registrySubscriptions
    let subscriptions = mapMaybe (`Map.lookup` allSubs) $ Set.toList subscriberIds

    -- Filter to matching subscriptions and deliver
    let matchingSubs = filter (\s -> filterMatchesEvent event s.subFilter) subscriptions
    forM_ matchingSubs $ \sub -> do
        writeTQueue sub.subDeliveryQueue event

    pure $ length matchingSubs
  where
    fromMaybe def = maybe def id

{- | Route an event to a specific subscriber.

Useful for targeted delivery or testing. The event is delivered regardless
of whether it matches the subscriber's filter.

Note: This does not check if the subscription exists or is for the correct
session - caller is responsible for validation.
-}
routeEventToSubscriber ::
    SubscriptionRegistry ->
    SubscriptionId ->
    SessionEvent ->
    STM Bool
routeEventToSubscriber registry subId event = do
    subs <- readTVar registry.registrySubscriptions
    case Map.lookup subId subs of
        Nothing -> pure False
        Just sub -> do
            writeTQueue sub.subDeliveryQueue event
            pure True

{- | Get all subscriptions that match an event for a session.

Returns the matching subscription records. Useful for debugging or
when you need to inspect matching subscriptions before delivery.
-}
getMatchingSubscribers ::
    SubscriptionRegistry ->
    ConversationId ->
    SessionEvent ->
    STM [EventSubscription]
getMatchingSubscribers registry sessionId event = do
    bySession <- readTVar registry.registryBySession
    let subscriberIds = fromMaybe Set.empty $ Map.lookup sessionId bySession

    allSubs <- readTVar registry.registrySubscriptions
    let subscriptions = mapMaybe (`Map.lookup` allSubs) $ Set.toList subscriberIds

    pure $ filter (\s -> filterMatchesEvent event s.subFilter) subscriptions
  where
    fromMaybe def = maybe def id

{- | Count how many subscribers would receive an event.

More efficient than 'getMatchingSubscribers' when you only need the count.
Useful for metrics and monitoring.
-}
countMatchingSubscribers ::
    SubscriptionRegistry ->
    ConversationId ->
    SessionEvent ->
    STM Int
countMatchingSubscribers registry sessionId event = do
    bySession <- readTVar registry.registryBySession
    let subscriberIds = fromMaybe Set.empty $ Map.lookup sessionId bySession

    allSubs <- readTVar registry.registrySubscriptions
    let subscriptions = mapMaybe (`Map.lookup` allSubs) $ Set.toList subscriberIds

    pure $ length $ filter (\s -> filterMatchesEvent event s.subFilter) subscriptions
  where
    fromMaybe def = maybe def id

-------------------------------------------------------------------------------
-- Filter Matching (Re-exported with Documentation)
-------------------------------------------------------------------------------

{- | Check if a session event matches a filter.

This function is the heart of the routing system. It evaluates all filter
types and combinators to determine if an event should be delivered.

=== Filter Types

* 'FilterAll' - Matches every event
* 'FilterEventType' - Matches events of a specific category (turn, tool, etc.)
* 'FilterToolCall' - Matches tool call events (optionally for a specific tool)
* 'FilterToolCallWithArg' - Matches tool calls with specific arguments
* 'FilterLlmResponseContains' - Matches when LLM response contains text
* 'FilterUserInputMatches' - Matches when user input matches a pattern
* 'FilterTurnStatus' - Matches turn lifecycle events
* 'FilterCombinator' - Combines filters with AND/OR/NOT

=== Examples

Match any tool call:

>>> eventMatchesFilter (ToolCallRequestedEvent{...}) (FilterToolCall Nothing)
True

Match specific tool:

>>> eventMatchesFilter (ToolCallRequestedEvent{seToolName = "bash"} _) (FilterToolCall (Just "bash"))
True

Match LLM response containing "error":

>>> eventMatchesFilter (LlmResponseReceivedEvent{...}) (FilterLlmResponseContains "error")
True

Match with AND combinator:

>>> let filter = FilterCombinator $ FilterAnd (FilterEventType TypeToolCallEvent) (FilterToolCall (Just "bash"))
>>> eventMatchesFilter (ToolCallRequestedEvent{seToolName = "bash"} _) filter
True
-}
eventMatchesFilter :: SessionEvent -> SessionEventFilter -> Bool
eventMatchesFilter = filterMatchesEvent

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- Note: These helper functions are available from the Subscription module
-- but are included here for documentation purposes:
--

-- * filterMatchesEvent - Core matching logic

-- * eventTypeMatches - Match event to event type category

-- * argMatchesPath - Match JSON argument at a path

-- * getValueAtPath - Get value from JSON at a dot-notation path
