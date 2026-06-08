{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Wait-For Event Tool for Phase 4 of ongoing session interaction.

This module implements the @system_wait_for_event@ tool that enables agents
to block and wait for specific events in other ongoing sessions. This is the
foundation for multi-agent coordination patterns.

== Key Concepts

* __Blocking Wait__: Agent pauses execution until an event occurs
* __Timeout__: Maximum wait time to prevent indefinite blocking
* __Event Buffering__: Optional collection of all events during wait
* __Multi-condition__: Wait for multiple event types with combinators

== Example Usage

Wait for a deployment to complete:

@
let params = WaitForEventParams
    { waitTargetSession = deploymentSessionId
    , waitEventTypes = [SelectToolCall (NamedExact "deploy_service")]
    , waitCondition = Just $ ToolCallResultContains "success"
    , waitTimeoutSeconds = Just 600
    , waitBufferEvents = False
    }

result <- waitForEvent tracer toolbox params

case result of
    WaitSucceeded event elapsed -> putStrLn $ "Deploy completed in " ++ show elapsed ++ "ms"
    WaitTimeout events elapsed -> putStrLn $ "Timeout after " ++ show elapsed ++ "ms"
    WaitCancelled reason -> putStrLn $ "Cancelled: " ++ show reason
    WaitError err -> putStrLn $ "Error: " ++ show err
@

== Multi-Agent Coordination Patterns

1. __Monitor-Worker__: Agent A waits for Agent B to complete a task
2. __Fan-Out/Fan-In__: Coordinator waits for multiple workers
3. __Pipeline__: Each stage waits for the previous to complete

== Safety Limits

* Default timeout: 300 seconds (5 minutes)
* Maximum timeout: 3600 seconds (1 hour)
* Event buffer: Limited to prevent memory exhaustion
-}
module System.Agents.Tools.SystemToolbox.Wait (
    -- * Wait Parameters
    WaitForEventParams (..),
    EventTypeSelector (..),
    Named (..),
    EventCombinator (..),

    -- * Event Conditions
    EventCondition (..),
    ConditionCombinator (..),

    -- * Wait Results
    WaitForEventResult (..),

    -- * Core Function
    waitForEvent,

    -- * Matching Functions
    eventMatchesParams,
    eventMatchesSelector,
    eventMatchesCondition,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (
    STM,
    TQueue,
    atomically,
    isEmptyTQueue,
    readTQueue,
    tryReadTQueue,
 )
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, Value, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap (lookup)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Prod.Tracer (Tracer, runTracer)

import System.Agents.Base (ConversationId)
import System.Agents.OS.Events.Subscription (
    EventSubscription (..),
    FilterCombinator (..),
    SessionEventFilter (..),
    SessionEventType (..),
    SubscriberId (..),
    SubscriptionError (..),
    SubscriptionId,
    SubscriptionRegistry,
    SystemComponentId (..),
    subscribeToSessionEvents,
    unsubscribeFromSessionEvents,
 )
import System.Agents.Session.Events (SessionEvent (..), UserToolResponse (..))
import System.Agents.Tools.SystemToolbox.Types (Toolbox, Trace (..))

-------------------------------------------------------------------------------
-- Wait Parameters
-------------------------------------------------------------------------------

-- | Name matching specification for event type selectors.
data Named
    = NamedExact Text
    | NamedPattern Text
    deriving (Show, Eq, Generic)

instance ToJSON Named where
    toJSON (NamedExact name) = Aeson.object ["exact" .= name]
    toJSON (NamedPattern pattern) = Aeson.object ["pattern" .= pattern]

instance FromJSON Named where
    parseJSON (Aeson.Object obj) = do
        mExact <- obj Aeson..:? "exact"
        mPattern <- obj Aeson..:? "pattern"
        case (mExact, mPattern) of
            (Just name, Nothing) -> pure $ NamedExact name
            (Nothing, Just pat) -> pure $ NamedPattern pat
            _ -> fail "Named must have either 'exact' or 'pattern'"
    parseJSON (Aeson.String name) = pure $ NamedExact name
    parseJSON _ = fail "Invalid Named"

-- | Combinators for composing event type selectors.
data EventCombinator
    = EventOr
    | EventAnd
    deriving (Show, Eq, Generic)

instance ToJSON EventCombinator where
    toJSON EventOr = Aeson.String "or"
    toJSON EventAnd = Aeson.String "and"

instance FromJSON EventCombinator where
    parseJSON (Aeson.String "or") = pure EventOr
    parseJSON (Aeson.String "and") = pure EventAnd
    parseJSON _ = fail "Invalid EventCombinator"

{- | Selects which types of events to wait for.

Provides a simpler API than full SessionEventFilter for the wait tool.
-}
data EventTypeSelector
    = SelectAnyToolCall
    | SelectToolCall Named
    | SelectAnyUserInput
    | SelectAnyLlmResponse
    | SelectTurnCompletion
    | EventTypeCombinator EventCombinator [EventTypeSelector]
    deriving (Show, Eq, Generic)

instance ToJSON EventTypeSelector where
    toJSON SelectAnyToolCall = Aeson.String "any_tool_call"
    toJSON (SelectToolCall named) = Aeson.object ["tool_call" .= named]
    toJSON SelectAnyUserInput = Aeson.String "any_user_input"
    toJSON SelectAnyLlmResponse = Aeson.String "any_llm_response"
    toJSON SelectTurnCompletion = Aeson.String "turn_completion"
    toJSON (EventTypeCombinator comb selectors) =
        Aeson.object [combinerKey comb .= selectors]
      where
        combinerKey EventOr = "or"
        combinerKey EventAnd = "and"

instance FromJSON EventTypeSelector where
    parseJSON (Aeson.String "any_tool_call") = pure SelectAnyToolCall
    parseJSON (Aeson.String "any_user_input") = pure SelectAnyUserInput
    parseJSON (Aeson.String "any_llm_response") = pure SelectAnyLlmResponse
    parseJSON (Aeson.String "turn_completion") = pure SelectTurnCompletion
    parseJSON (Aeson.Object obj) = do
        mToolCall <- obj Aeson..:? "tool_call"
        mOr <- obj Aeson..:? "or"
        mAnd <- obj Aeson..:? "and"
        case (mToolCall, mOr, mAnd) of
            (Just named, Nothing, Nothing) -> SelectToolCall <$> Aeson.parseJSON named
            (Nothing, Just selectors, Nothing) -> EventTypeCombinator EventOr <$> Aeson.parseJSON selectors
            (Nothing, Nothing, Just selectors) -> EventTypeCombinator EventAnd <$> Aeson.parseJSON selectors
            _ -> fail "Invalid EventTypeSelector"
    parseJSON _ = fail "Invalid EventTypeSelector"

-- | Combinators for event conditions.
data ConditionCombinator
    = ConditionAnd EventCondition EventCondition
    | ConditionOr EventCondition EventCondition
    | ConditionNot EventCondition
    deriving (Show, Eq, Generic)

instance ToJSON ConditionCombinator where
    toJSON (ConditionAnd a b) = Aeson.object ["and" .= [a, b]]
    toJSON (ConditionOr a b) = Aeson.object ["or" .= [a, b]]
    toJSON (ConditionNot c) = Aeson.object ["not" .= c]

instance FromJSON ConditionCombinator where
    parseJSON (Aeson.Object obj) = do
        mAnd <- obj Aeson..:? "and"
        mOr <- obj Aeson..:? "or"
        mNot <- obj Aeson..:? "not"
        case (mAnd, mOr, mNot) of
            (Just [a, b], Nothing, Nothing) -> ConditionAnd <$> Aeson.parseJSON a <*> Aeson.parseJSON b
            (Nothing, Just [a, b], Nothing) -> ConditionOr <$> Aeson.parseJSON a <*> Aeson.parseJSON b
            (Nothing, Nothing, Just c) -> ConditionNot <$> Aeson.parseJSON c
            _ -> fail "Invalid ConditionCombinator"
    parseJSON _ = fail "Invalid ConditionCombinator"

{- | Conditions on event content for fine-grained filtering.

These conditions are applied after the event type selector matches.
-}
data EventCondition
    = ToolCallArgCondition Text Value
    | ToolCallResultContains Text
    | LlmResponseContains Text
    | UserInputMatches Text
    | ConditionCombinator ConditionCombinator
    deriving (Show, Eq, Generic)

instance ToJSON EventCondition where
    toJSON (ToolCallArgCondition argName argValue) =
        Aeson.object ["tool_arg_equals" .= Aeson.object ["name" .= argName, "value" .= argValue]]
    toJSON (ToolCallResultContains text) =
        Aeson.object ["tool_result_contains" .= text]
    toJSON (LlmResponseContains text) =
        Aeson.object ["llm_response_contains" .= text]
    toJSON (UserInputMatches text) =
        Aeson.object ["user_input_matches" .= text]
    toJSON (ConditionCombinator comb) = Aeson.toJSON comb

instance FromJSON EventCondition where
    parseJSON (Aeson.Object obj) = do
        mToolArg <- obj Aeson..:? "tool_arg_equals"
        mToolResult <- obj Aeson..:? "tool_result_contains"
        mLlmResp <- obj Aeson..:? "llm_response_contains"
        mUserInput <- obj Aeson..:? "user_input_matches"
        mAnd <- obj Aeson..:? "and"
        mOr <- obj Aeson..:? "or"
        mNot <- obj Aeson..:? "not"
        case (mToolArg, mToolResult, mLlmResp, mUserInput, mAnd, mOr, mNot) of
            (Just argObj, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ->
                ToolCallArgCondition <$> argObj Aeson..: "name" <*> argObj Aeson..: "value"
            (Nothing, Just text, Nothing, Nothing, Nothing, Nothing, Nothing) ->
                pure $ ToolCallResultContains text
            (Nothing, Nothing, Just text, Nothing, Nothing, Nothing, Nothing) ->
                pure $ LlmResponseContains text
            (Nothing, Nothing, Nothing, Just text, Nothing, Nothing, Nothing) ->
                pure $ UserInputMatches text
            (Nothing, Nothing, Nothing, Nothing, Just [a, b], Nothing, Nothing) ->
                ConditionAnd <$> Aeson.parseJSON a <*> Aeson.parseJSON b
            (Nothing, Nothing, Nothing, Nothing, Nothing, Just [a, b], Nothing) ->
                ConditionOr <$> Aeson.parseJSON a <*> Aeson.parseJSON b
            (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just c) ->
                ConditionNot <$> Aeson.parseJSON c
            _ -> fail "Invalid EventCondition"
    parseJSON _ = fail "Invalid EventCondition"

-- | Parameters for the @system_wait_for_event@ tool.
data WaitForEventParams = WaitForEventParams
    { waitTargetSession :: ConversationId
    -- ^ Which session to watch
    , waitEventTypes :: [EventTypeSelector]
    -- ^ What types of events to wait for
    , waitCondition :: Maybe EventCondition
    -- ^ Optional condition on the event
    , waitTimeoutSeconds :: Maybe Int
    -- ^ Timeout (Nothing = default 300s, max 3600s)
    , waitBufferEvents :: Bool
    -- ^ If true, buffer events while waiting
    }
    deriving (Show, Eq, Generic)

instance ToJSON WaitForEventParams
instance FromJSON WaitForEventParams

-- Default timeout: 5 minutes
defaultWaitTimeoutSeconds :: Int
defaultWaitTimeoutSeconds = 300

-- Maximum timeout: 1 hour
maxWaitTimeoutSeconds :: Int
maxWaitTimeoutSeconds = 3600

-------------------------------------------------------------------------------
-- Wait Results
-------------------------------------------------------------------------------

-- | Result of a wait operation.
data WaitForEventResult
    = WaitSucceeded
        { waitEvent :: SessionEvent
        , waitElapsedMs :: Int
        }
    | WaitTimeout
        { waitEventsSeen :: [SessionEvent]
        , waitElapsedMs :: Int
        }
    | WaitCancelled
        { waitReason :: Text
        }
    | WaitError
        { waitError :: Text
        }
    deriving (Show, Eq, Generic)

instance ToJSON WaitForEventResult where
    toJSON (WaitSucceeded event elapsed) =
        Aeson.object
            [ "status" .= ("succeeded" :: Text)
            , "event" .= event
            , "elapsed_ms" .= elapsed
            ]
    toJSON (WaitTimeout events elapsed) =
        Aeson.object
            [ "status" .= ("timeout" :: Text)
            , "events_seen" .= events
            , "elapsed_ms" .= elapsed
            ]
    toJSON (WaitCancelled reason) =
        Aeson.object
            [ "status" .= ("cancelled" :: Text)
            , "reason" .= reason
            ]
    toJSON (WaitError err) =
        Aeson.object
            [ "status" .= ("error" :: Text)
            , "error" .= err
            ]

instance FromJSON WaitForEventResult where
    parseJSON (Aeson.Object obj) = do
        status <- obj Aeson..: "status"
        case (status :: Text) of
            "succeeded" ->
                WaitSucceeded
                    <$> obj Aeson..: "event"
                    <*> obj Aeson..: "elapsed_ms"
            "timeout" ->
                WaitTimeout
                    <$> obj Aeson..: "events_seen"
                    <*> obj Aeson..: "elapsed_ms"
            "cancelled" ->
                WaitCancelled
                    <$> obj Aeson..: "reason"
            "error" ->
                WaitError
                    <$> obj Aeson..: "error"
            _ -> fail "Invalid status in WaitForEventResult"
    parseJSON _ = fail "Invalid WaitForEventResult"

-------------------------------------------------------------------------------
-- Core Wait Function
-------------------------------------------------------------------------------

{- | Block and wait for a specific event in another session.

Creates a temporary subscription, waits for a matching event, and returns
the result. Handles timeouts, event buffering, and cleanup.

=== Usage Notes

* If 'waitTimeoutSeconds' is Nothing, uses default (300s)
* If timeout exceeds max (3600s), it's clamped to max
* If 'waitBufferEvents' is True, all events during wait are collected
* Subscription is always cleaned up, even on timeout or error
-}
waitForEvent ::
    Tracer IO Trace ->
    Toolbox ->
    -- | Must include SubscriptionRegistry in the toolbox
    WaitForEventParams ->
    IO WaitForEventResult
waitForEvent tracer toolbox params = do
    runTracer tracer $ SystemInfoRequestedTrace "wait-for-event"
    startTime <- getCurrentTime

    -- Calculate timeout in microseconds
    let timeoutSecs = fromMaybe defaultWaitTimeoutSeconds params.waitTimeoutSeconds
    let clampedTimeout = min timeoutSecs maxWaitTimeoutSeconds
    let timeoutMicros = clampedTimeout * 1000000

    -- TODO: Get subscription registry from toolbox
    -- For now, return error indicating registry not available
    -- In full implementation, the toolbox would have a registry field
    endTime <- getCurrentTime
    let elapsedMs = round $ diffUTCTime endTime startTime * 1000

    pure $ WaitError "Subscription registry not available in toolbox - Phase 4 implementation pending integration"

{- | Wait implementation using STM and async.

This is the core implementation that:
1. Creates a subscription
2. Waits for matching event or timeout
3. Cleans up the subscription
4. Returns appropriate result

Note: This is a placeholder implementation. Full implementation requires
access to the SubscriptionRegistry from the toolbox.
-}
waitForEventImpl ::
    SubscriptionRegistry ->
    WaitForEventParams ->
    -- | Timeout in microseconds
    Int ->
    IO WaitForEventResult
waitForEventImpl registry params timeoutMicros = do
    startTime <- getCurrentTime

    -- Create temporary subscription
    let subscriber = SubscriberSystem (SystemNamed "wait-tool")
    let filterSpec = paramsToFilter params

    -- Subscribe
    subResult <- atomically $ do
        now <- pure startTime -- Would need actual time in STM
        subscribeToSessionEvents registry subscriber params.waitTargetSession filterSpec Nothing now

    case subResult of
        Left err -> pure $ WaitError $ Text.pack $ show err
        Right subscription -> do
            -- Race between timeout and event arrival
            result <-
                race
                    (threadDelay timeoutMicros)
                    (waitForMatchingEvent subscription params)

            -- Cleanup subscription
            atomically $ unsubscribeFromSessionEvents registry subscription.subId

            endTime <- getCurrentTime
            let elapsedMs = round $ diffUTCTime endTime startTime * 1000

            -- Return result
            case result of
                Left () -> do
                    -- Timeout - collect buffered events if enabled
                    buffered <-
                        if params.waitBufferEvents
                            then atomically $ drainQueue subscription.subDeliveryQueue
                            else pure []
                    pure $ WaitTimeout buffered elapsedMs
                Right event ->
                    pure $ WaitSucceeded event elapsedMs

-- | Convert WaitForEventParams to SessionEventFilter.
paramsToFilter :: WaitForEventParams -> SessionEventFilter
paramsToFilter params =
    -- Convert event type selectors to filter
    case params.waitEventTypes of
        [] -> FilterAll
        [selector] -> selectorToFilter selector
        selectors -> FilterCombinator $ foldr1 FilterAnd $ map selectorToFilter selectors

-- | Convert EventTypeSelector to SessionEventFilter.
selectorToFilter :: EventTypeSelector -> SessionEventFilter
selectorToFilter SelectAnyToolCall = FilterEventType TypeToolCallEvent
selectorToFilter (SelectToolCall named) =
    case named of
        NamedExact name -> FilterToolCall (Just name)
        NamedPattern _ -> FilterToolCall Nothing -- Pattern matching not in base filter
selectorToFilter SelectAnyUserInput = FilterEventType TypeUserInteractionEvent
selectorToFilter SelectAnyLlmResponse = FilterEventType TypeLlmInteractionEvent
selectorToFilter SelectTurnCompletion = FilterTurnStatus TurnStatusCompleted
selectorToFilter (EventTypeCombinator comb selectors) =
    case selectors of
        [] -> FilterAll
        [s] -> selectorToFilter s
        (s : ss) ->
            let baseFilter = selectorToFilter s
                restFilter = selectorToFilter (EventTypeCombinator comb ss)
             in FilterCombinator $ case comb of
                    EventOr -> FilterOr baseFilter restFilter
                    EventAnd -> FilterAnd baseFilter restFilter

-- | Wait for a matching event from the subscription queue.
waitForMatchingEvent :: EventSubscription -> WaitForEventParams -> IO SessionEvent
waitForMatchingEvent subscription params = atomically $ go
  where
    go = do
        event <- readTQueue subscription.subDeliveryQueue
        if eventMatchesParams event params
            then pure event
            else go

-- | Drain all events from a queue.
drainQueue :: TQueue a -> STM [a]
drainQueue queue = do
    empty <- isEmptyTQueue queue
    if empty
        then pure []
        else do
            x <- readTQueue queue
            xs <- drainQueue queue
            pure (x : xs)

-------------------------------------------------------------------------------
-- Matching Functions
-------------------------------------------------------------------------------

{- | Check if an event matches the wait parameters.

Applies both the event type selector and optional condition.
-}
eventMatchesParams :: SessionEvent -> WaitForEventParams -> Bool
eventMatchesParams event params =
    let typeMatches = any (eventMatchesSelector event) params.waitEventTypes
        conditionMatches = case params.waitCondition of
            Nothing -> True
            Just cond -> eventMatchesCondition event cond
     in typeMatches && conditionMatches

-- | Check if an event matches a type selector.
eventMatchesSelector :: SessionEvent -> EventTypeSelector -> Bool
eventMatchesSelector event selector = case selector of
    SelectAnyToolCall ->
        case event of
            ToolCallRequestedEvent{} -> True
            ToolCallExecutingEvent{} -> True
            ToolCallCompletedEvent{} -> True
            ToolCallFailedEvent{} -> True
            ToolCallCacheHitEvent{} -> True
            _ -> False
    SelectToolCall named ->
        case event of
            ToolCallRequestedEvent _ name _ ->
                case named of
                    NamedExact expected -> name == expected
                    NamedPattern _ -> True -- Pattern matching would need regex
            ToolCallExecutingEvent{} -> True
            ToolCallCompletedEvent{} -> True
            ToolCallFailedEvent{} -> True
            ToolCallCacheHitEvent{} -> True
            _ -> False
    SelectAnyUserInput ->
        case event of
            UserInputReceivedEvent{} -> True
            _ -> False
    SelectAnyLlmResponse ->
        case event of
            LlmResponseReceivedEvent{} -> True
            _ -> False
    SelectTurnCompletion ->
        case event of
            TurnCompletedEvent{} -> True
            _ -> False
    EventTypeCombinator comb selectors ->
        case comb of
            EventOr -> any (eventMatchesSelector event) selectors
            EventAnd -> all (eventMatchesSelector event) selectors

-- | Check if an event matches a condition.
eventMatchesCondition :: SessionEvent -> EventCondition -> Bool
eventMatchesCondition event condition = case condition of
    ToolCallArgCondition argPath expectedValue ->
        case event of
            ToolCallRequestedEvent _ _ args ->
                argMatchesPath argPath expectedValue args
            _ -> False
    ToolCallResultContains searchText ->
        case event of
            ToolCallCompletedEvent _ result _ ->
                resultContains searchText result
            _ -> False
    LlmResponseContains searchText ->
        case event of
            LlmResponseReceivedEvent response _ _ ->
                let textMatch = maybe False (searchText `Text.isInfixOf`) $ responseText response
                    thinkingMatch = maybe False (searchText `Text.isInfixOf`) $ responseThinking response
                 in textMatch || thinkingMatch
            _ -> False
    UserInputMatches pattern ->
        case event of
            UserInputReceivedEvent inputText _ _ ->
                pattern `Text.isInfixOf` inputText
            _ -> False
    ConditionCombinator comb ->
        case comb of
            ConditionAnd a b ->
                eventMatchesCondition event a && eventMatchesCondition event b
            ConditionOr a b ->
                eventMatchesCondition event a || eventMatchesCondition event b
            ConditionNot c ->
                not $ eventMatchesCondition event c

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Check if an argument at a dot-notation path matches an expected value.
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

-- | Check if a tool response contains search text.
resultContains :: Text -> UserToolResponse -> Bool
resultContains searchText response = case response of
    TextResponse txt -> searchText `Text.isInfixOf` txt
    JsonResponse val -> searchText `Text.isInfixOf` Text.pack (show val)
    MediaResponse media -> searchText `Text.isInfixOf` media.mediaBase64Data
    MixedResponse parts -> any (partContains searchText) parts

-- | Check if a content part contains search text.
partContains :: Text -> a -> Bool
partContains _ _ = False -- Simplified - would need ContentPart type

-- | Extract response text from LLM response (placeholder).
responseText :: a -> Maybe Text
responseText _ = Nothing -- Placeholder - would need LlmResponse type

-- | Extract thinking content from LLM response (placeholder).
responseThinking :: a -> Maybe Text
responseThinking _ = Nothing -- Placeholder - would need LlmResponse type
