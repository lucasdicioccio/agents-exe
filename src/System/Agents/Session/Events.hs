{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Session lifecycle events for ongoing session observation.

This module defines fine-grained session events that are emitted during
session execution. These events enable external observation and interaction
with ongoing (in-memory) sessions.

== Event History

Events are stored in a bounded queue (TBQueue) within the ConversationInbox
component. When the queue is full, old events are dropped to make room for
new ones (circular buffer behavior).

== Event Types

Events cover the full session lifecycle:
* Turn lifecycle (started, completed, failed)
* Tool call lifecycle (requested, executing, completed, failed, cache hit)
* User interaction (input received, paused, resumed)
* LLM interaction (request sent, response received)

== Usage

Emit events during session execution:

@
emitEvent mEventQueue (TurnStartedEvent turnId now)
@

Read events from the SystemToolbox:

@
events <- getSessionEvents world convId (Just 100)
@

-}
module System.Agents.Session.Events (
    -- * Event Types
    SessionEvent (..),
    InputSource (..),
    EventSubscriber (..),

    -- * Event Emission
    emitEvent,
    emitEventSTM,

    -- * Event History Operations
    readEvents,
    drainEvents,

    -- * Utility Functions
    isTurnEvent,
    isToolCallEvent,
    isUserInteractionEvent,
    isLlmInteractionEvent,
    eventTimestamp,
) where

import Control.Concurrent.STM (
    STM,
    TBQueue,
    isFullTBQueue,
    readTBQueue,
    tryReadTBQueue,
    writeTBQueue,
 )
import Control.Monad (void, when)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import System.Agents.Base (AgentId, ConversationId, ToolCallId)
import System.Agents.LLMs.OpenAI (TokenUsage)
import System.Agents.Media.Types (ContentPart)
import System.Agents.Session.Types (
    LlmResponse,
    TurnId,
    UserToolResponse,
 )

-------------------------------------------------------------------------------
-- Input Source
-------------------------------------------------------------------------------

{- | Source of user input.

Distinguishes between genuine user input and injected messages from
other agents or the system.
-}
data InputSource
    = -- | Input from the actual user
      FromUser
    | -- | Input injected by another agent
      FromAgent AgentId
    | -- | Input injected by the system
      FromSystem
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON InputSource
instance Aeson.FromJSON InputSource

-------------------------------------------------------------------------------
-- Event Subscriber (for Phase 3+)
-------------------------------------------------------------------------------

{- | A subscriber to session events.

For now, this is a placeholder for future real-time subscription support.
In Phase 3, this will enable agents to subscribe to events from other
sessions and receive them via callback or queue.
-}
data EventSubscriber = EventSubscriber
    { subAgentId :: AgentId
    -- ^ Agent that subscribed
    , subCreatedAt :: UTCTime
    -- ^ When subscription was created
    , subFilter :: Maybe EventFilter
    -- ^ Optional filter for event types
    }
    deriving (Show, Eq, Generic)

{- | Filter for event subscription.

Allows subscribers to receive only specific event types.
-}
data EventFilter = EventFilter
    { efTurnEvents :: Bool
    , efToolCallEvents :: Bool
    , efUserInteractionEvents :: Bool
    , efLlmInteractionEvents :: Bool
    }
    deriving (Show, Eq, Generic)

-- | Default event filter (all events enabled).
defaultEventFilter :: EventFilter
defaultEventFilter =
    EventFilter
        { efTurnEvents = True
        , efToolCallEvents = True
        , efUserInteractionEvents = True
        , efLlmInteractionEvents = True
        }

instance Aeson.ToJSON EventSubscriber
instance Aeson.FromJSON EventSubscriber
instance Aeson.ToJSON EventFilter
instance Aeson.FromJSON EventFilter

-------------------------------------------------------------------------------
-- Session Events
-------------------------------------------------------------------------------

{- | Fine-grained session lifecycle events.

These events capture the full lifecycle of a session, from turn start
to tool call completion. Each event includes a timestamp for accurate
history reconstruction.
-}
data SessionEvent
    = -- Turn lifecycle
      TurnStartedEvent
        { seTurnId :: TurnId
        , seTimestamp :: UTCTime
        }
    | TurnCompletedEvent
        { seTurnId :: TurnId
        , seDurationMs :: Int
        -- ^ Turn duration in milliseconds
        }
    | TurnFailedEvent
        { seTurnId :: TurnId
        , seError :: Text
        }
    | -- Tool call lifecycle
      ToolCallRequestedEvent
        { seToolCallId :: ToolCallId
        , seToolName :: Text
        , seToolArgs :: Value
        }
    | ToolCallExecutingEvent
        { seToolCallId :: ToolCallId
        , seTimestamp :: UTCTime
        }
    | ToolCallCompletedEvent
        { seToolCallId :: ToolCallId
        , seResult :: UserToolResponse
        , seTimestamp :: UTCTime
        }
    | ToolCallFailedEvent
        { seToolCallId :: ToolCallId
        , seError :: Text
        , seTimestamp :: UTCTime
        }
    | ToolCallCacheHitEvent
        { seToolCallId :: ToolCallId
        , seCacheKey :: Text
        -- ^ Simplified cache key representation
        }
    | -- User interaction
      UserInputReceivedEvent
        { seInputText :: Text
        , seInputSource :: InputSource
        , seTimestamp :: UTCTime
        }
    | SessionPausedEvent
        { sePauseReason :: Text
        , seTimestamp :: UTCTime
        }
    | SessionResumedEvent
        { seTimestamp :: UTCTime
        }
    | -- LLM interaction
      LlmRequestSentEvent
        { seRequestContent :: [ContentPart]
        , seTimestamp :: UTCTime
        }
    | LlmResponseReceivedEvent
        { seResponse :: LlmResponse
        , seToolCalls :: [Text]
        -- ^ Names of tools requested
        , seTimestamp :: UTCTime
        }
    | LlmTokenUsageEvent
        { seTurnId :: TurnId
        , seTokenUsage :: TokenUsage
        , seTimestamp :: UTCTime
        }
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON SessionEvent
instance Aeson.FromJSON SessionEvent

-------------------------------------------------------------------------------
-- Event Classification
-------------------------------------------------------------------------------

-- | Check if an event is a turn lifecycle event.
isTurnEvent :: SessionEvent -> Bool
isTurnEvent TurnStartedEvent{} = True
isTurnEvent TurnCompletedEvent{} = True
isTurnEvent TurnFailedEvent{} = True
isTurnEvent _ = False

-- | Check if an event is a tool call lifecycle event.
isToolCallEvent :: SessionEvent -> Bool
isToolCallEvent ToolCallRequestedEvent{} = True
isToolCallEvent ToolCallExecutingEvent{} = True
isToolCallEvent ToolCallCompletedEvent{} = True
isToolCallEvent ToolCallFailedEvent{} = True
isToolCallEvent ToolCallCacheHitEvent{} = True
isToolCallEvent _ = False

-- | Check if an event is a user interaction event.
isUserInteractionEvent :: SessionEvent -> Bool
isUserInteractionEvent UserInputReceivedEvent{} = True
isUserInteractionEvent SessionPausedEvent{} = True
isUserInteractionEvent SessionResumedEvent{} = True
isUserInteractionEvent _ = False

-- | Check if an event is an LLM interaction event.
isLlmInteractionEvent :: SessionEvent -> Bool
isLlmInteractionEvent LlmRequestSentEvent{} = True
isLlmInteractionEvent LlmResponseReceivedEvent{} = True
isLlmInteractionEvent LlmTokenUsageEvent{} = True
isLlmInteractionEvent _ = False

-- | Get the timestamp from an event, if present.
eventTimestamp :: SessionEvent -> Maybe UTCTime
eventTimestamp (TurnStartedEvent _ ts) = Just ts
eventTimestamp (TurnCompletedEvent{}) = Nothing -- No timestamp, has duration
eventTimestamp (TurnFailedEvent{}) = Nothing
eventTimestamp (ToolCallRequestedEvent{}) = Nothing
eventTimestamp (ToolCallExecutingEvent _ ts) = Just ts
eventTimestamp (ToolCallCompletedEvent _ _ ts) = Just ts
eventTimestamp (ToolCallFailedEvent _ _ ts) = Just ts
eventTimestamp (ToolCallCacheHitEvent{}) = Nothing
eventTimestamp (UserInputReceivedEvent _ _ ts) = Just ts
eventTimestamp (SessionPausedEvent _ ts) = Just ts
eventTimestamp (SessionResumedEvent ts) = Just ts
eventTimestamp (LlmRequestSentEvent _ ts) = Just ts
eventTimestamp (LlmResponseReceivedEvent _ _ ts) = Just ts
eventTimestamp (LlmTokenUsageEvent _ _ ts) = Just ts

-------------------------------------------------------------------------------
-- Event Emission
-------------------------------------------------------------------------------

{- | Emit an event to the event history queue (IO version).

Non-blocking: if the queue is full, the oldest event is dropped.
This ensures event emission never blocks the session execution.
-}
emitEvent :: Maybe (TBQueue SessionEvent) -> SessionEvent -> IO ()
emitEvent Nothing _ = pure () -- No event queue configured
emitEvent (Just queue) event = do
    -- Non-blocking write; drop oldest if full
    atomically $ emitEventSTM queue event

{- | Emit an event to the event history queue (STM version).

Non-blocking: if the queue is full, the oldest event is dropped.
-}
emitEventSTM :: TBQueue SessionEvent -> SessionEvent -> STM ()
emitEventSTM queue event = do
    full <- isFullTBQueue queue
    when full $ void $ tryReadTBQueue queue
    writeTBQueue queue event

-------------------------------------------------------------------------------
-- Event History Operations
-------------------------------------------------------------------------------

{- | Read all available events from the queue without draining.

Returns events in chronological order (oldest first).
-}
readEvents :: TBQueue SessionEvent -> STM [SessionEvent]
readEvents queue = do
    -- Note: This is a simplified implementation
    -- In practice, we'd need to peek at the queue without removing
    -- For now, this returns an empty list as a placeholder
    -- Full implementation would require a different queue structure
    pure []

{- | Drain all events from the queue.

Returns events in chronological order (oldest first) and clears the queue.
-}
drainEvents :: TBQueue SessionEvent -> STM [SessionEvent]
drainEvents queue = go []
  where
    go acc = do
        mEvent <- tryReadTBQueue queue
        case mEvent of
            Nothing -> pure $ reverse acc
            Just event -> go (event : acc)

-- STM import helper
atomically :: STM a -> IO a
atomically = Control.Concurrent.STM.atomically

