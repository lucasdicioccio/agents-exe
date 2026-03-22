{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Conversation, Turn, and Tool Call components for the ECS architecture.

This module defines types for tracking:
- Conversations: Top-level interactions between users and agents
- Turns: Individual steps within a conversation
- Tool Calls: Individual tool invocations, potentially nested
- Messages: Communication between users and agents

The types support:
- Conversation branching (exploring multiple responses from the same point)
- Nested tool calls with proper depth tracking
- Complete audit trails for debugging

== Tradeoffs

1. **Branching Conversations**: Tree-structured conversations add complexity
   but enable exploring multiple responses from the same point.

2. **Nested Tool Calls**: Maximum depth limits should be enforced to prevent
   infinite recursion. Default: 10 levels.

3. **Message Storage**: Storing all messages in components may consume
   significant memory for long conversations. Consider pagination or
   external storage for production use.

4. **TVar in ConversationState**: The 'conversationLastActivity' field uses
   a TVar for thread-safe updates without full conversation lock.
-}
module System.Agents.OS.Conversation.Types (
    -- * Conversation Components
    ConversationConfig (..),
    ConversationState (..),
    ConversationStatus (..),
    AgentConversation (..),
    AgentRole (..),

    -- * Turn Components
    TurnConfig (..),
    TurnState (..),
    TurnStatus (..),

    -- * Tool Call Components
    ToolCallConfig (..),
    ToolCallState (..),
    ToolCallStatus (..),

    -- * Message Components
    Message (..),
    MessageRole (..),

    -- * Utility Functions
    isConversationActive,
    isTurnCompleted,
    isToolCallCompleted,
    getToolCallResult,
    defaultMaxToolCallDepth,
) where

import Control.Concurrent.STM (TVar)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import System.Agents.OS.Core.Types (
    AgentId,
    Component (..),
    ComponentTypeId (..),
    ConversationId,
    ToolCallId,
    TurnId,
 )

-------------------------------------------------------------------------------
-- Conversation Components
-------------------------------------------------------------------------------

{- | Configuration component for conversations.

This is static configuration that doesn't change during the conversation lifetime.
-}
data ConversationConfig = ConversationConfig
    { conversationTitle :: Maybe Text
    -- ^ Optional human-readable title for the conversation
    , conversationMetadata :: Map Text Value
    -- ^ Arbitrary metadata for extensibility
    }
    deriving (Show, Eq, Generic)

instance FromJSON ConversationConfig
instance ToJSON ConversationConfig

instance Component ConversationConfig where
    componentId _ = ComponentTypeId 30

{- | Runtime state component for conversations.

This changes during conversation execution and tracks the current status.
Uses a TVar for 'conversationLastActivity' to allow thread-safe updates
without locking the entire conversation state.
-}
data ConversationState = ConversationState
    { conversationAgentId :: AgentId
    -- ^ The primary agent for this conversation
    , conversationStatus :: ConversationStatus
    -- ^ Current status of the conversation
    , conversationStartedAt :: UTCTime
    -- ^ When the conversation was created
    , conversationLastActivity :: TVar UTCTime
    -- ^ Last activity timestamp (TVar for thread-safe updates)
    }

-- Note: Manual Show instance due to TVar
instance Show ConversationState where
    show cs =
        "ConversationState {"
            ++ " conversationAgentId = "
            ++ show cs.conversationAgentId
            ++ ", conversationStatus = "
            ++ show cs.conversationStatus
            ++ ", conversationStartedAt = "
            ++ show cs.conversationStartedAt
            ++ ", conversationLastActivity = <TVar>"
            ++ " }"

-- Note: Eq instance compares everything except TVar
instance Eq ConversationState where
    a == b =
        conversationAgentId a == conversationAgentId b
            && conversationStatus a == conversationStatus b
            && conversationStartedAt a == conversationStartedAt b

instance Component ConversationState where
    componentId _ = ComponentTypeId 31

-- | Status of a conversation.
data ConversationStatus
    = -- | Conversation is ongoing and accepting turns
      ConversationActive
    | -- | Conversation is temporarily paused
      ConversationPaused
    | -- | Conversation has ended and is archived
      ConversationArchived
    | -- | Conversation encountered an error with message
      ConversationError Text
    deriving (Show, Eq, Generic)

instance FromJSON ConversationStatus
instance ToJSON ConversationStatus

{- | Links an agent to a conversation with a specific role.

This enables multiple agents to participate in the same conversation
with different responsibilities (primary, assistant, observer).
-}
data AgentConversation = AgentConversation
    { acAgentId :: AgentId
    -- ^ The agent participating in the conversation
    , acConversationId :: ConversationId
    -- ^ The conversation they're participating in
    , acRole :: AgentRole
    -- ^ Their role in the conversation
    }
    deriving (Show, Eq, Generic)

instance FromJSON AgentConversation
instance ToJSON AgentConversation

instance Component AgentConversation where
    componentId _ = ComponentTypeId 32

-- | Role of an agent in a conversation.
data AgentRole
    = -- | Main agent responsible for the conversation
      PrimaryAgent
    | -- | Supporting agent that assists the primary
      AssistantAgent
    | -- | Agent that observes but doesn't actively participate
      ObserverAgent
    deriving (Show, Eq, Generic)

instance FromJSON AgentRole
instance ToJSON AgentRole

-------------------------------------------------------------------------------
-- Turn Components
-------------------------------------------------------------------------------

{- | Configuration component for turns.

Turns represent individual steps within a conversation. The parent-turn
reference enables branching conversations where multiple turns can
spawn from the same parent.
-}
data TurnConfig = TurnConfig
    { turnConversationId :: ConversationId
    -- ^ Which conversation this turn belongs to
    , turnParentTurnId :: Maybe TurnId
    -- ^ Parent turn for branching (Nothing for root turns)
    }
    deriving (Show, Eq, Generic)

instance FromJSON TurnConfig
instance ToJSON TurnConfig

instance Component TurnConfig where
    componentId _ = ComponentTypeId 33

{- | Runtime state component for turns.

Tracks the lifecycle of a turn from creation to completion.
-}
data TurnState = TurnState
    { turnStatus :: TurnStatus
    -- ^ Current status of the turn
    , turnStartedAt :: UTCTime
    -- ^ When the turn was created
    , turnCompletedAt :: Maybe UTCTime
    -- ^ When the turn completed (if it has)
    }
    deriving (Show, Eq, Generic)

instance FromJSON TurnState
instance ToJSON TurnState

instance Component TurnState where
    componentId _ = ComponentTypeId 34

-- | Status of a turn.
data TurnStatus
    = -- | Turn is being initialized
      TurnStarting
    | -- | Turn is actively processing
      TurnRunning
    | -- | Waiting for tool calls to complete (with IDs of pending calls)
      TurnToolCallsPending [ToolCallId]
    | -- | Turn completed successfully with final response
      TurnCompleted Text
    | -- | Turn failed with error message
      TurnFailed Text
    deriving (Show, Eq, Generic)

instance FromJSON TurnStatus
instance ToJSON TurnStatus

-------------------------------------------------------------------------------
-- Tool Call Components
-------------------------------------------------------------------------------

{- | Configuration component for tool calls.

Tool calls represent individual tool invocations. The parent-call
reference enables nested tool calls for recursive agent invocations.
-}
data ToolCallConfig = ToolCallConfig
    { tcTurnId :: TurnId
    -- ^ Which turn this tool call belongs to
    , tcToolName :: Text
    -- ^ Name of the tool being called
    , tcToolInput :: Value
    -- ^ Input arguments for the tool (JSON)
    , tcParentCallId :: Maybe ToolCallId
    -- ^ Parent tool call for nested calls (Nothing for top-level)
    }
    deriving (Show, Eq, Generic)

instance FromJSON ToolCallConfig
instance ToJSON ToolCallConfig

instance Component ToolCallConfig where
    componentId _ = ComponentTypeId 35

{- | Runtime state component for tool calls.

Tracks the execution lifecycle of a tool call.
-}
data ToolCallState = ToolCallState
    { tcStatus :: ToolCallStatus
    -- ^ Current status of the tool call
    , tcStartedAt :: UTCTime
    -- ^ When the tool call started
    , tcCompletedAt :: Maybe UTCTime
    -- ^ When the tool call completed (if it has)
    , tcResult :: Maybe Value
    -- ^ Result of the tool call (if completed)
    }
    deriving (Show, Eq, Generic)

instance FromJSON ToolCallState
instance ToJSON ToolCallState

instance Component ToolCallState where
    componentId _ = ComponentTypeId 36

-- | Status of a tool call.
data ToolCallStatus
    = -- | Tool call is queued but not started
      TcPending
    | -- | Tool call is currently executing
      TcExecuting
    | -- | Tool call completed with result
      TcCompleted Value
    | -- | Tool call failed with error message
      TcFailed Text
    | -- | Tool call was cancelled before completion
      TcCancelled
    deriving (Show, Eq, Generic)

instance FromJSON ToolCallStatus
instance ToJSON ToolCallStatus

-------------------------------------------------------------------------------
-- Message Components
-------------------------------------------------------------------------------

{- | A message in a conversation.

Messages represent communication between users, agents, and tools.
They are separate from turns to allow for flexible message organization.
-}
data Message = Message
    { msgConversationId :: ConversationId
    -- ^ Which conversation this message belongs to
    , msgTurnId :: TurnId
    -- ^ Which turn this message was created in
    , msgRole :: MessageRole
    -- ^ Role of the message sender
    , msgContent :: Text
    -- ^ Message content
    , msgToolCalls :: [ToolCallId]
    -- ^ Tool calls associated with this message (for assistant messages)
    , msgTimestamp :: UTCTime
    -- ^ When the message was created
    }
    deriving (Show, Eq, Generic)

instance FromJSON Message
instance ToJSON Message

instance Component Message where
    componentId _ = ComponentTypeId 38

-- | Role of a message sender.
data MessageRole
    = -- | Message from the user
      UserRole
    | -- | Message from the AI assistant
      AssistantRole
    | -- | System message (instructions, etc.)
      SystemRole
    | -- | Message from a tool (results, etc.)
      ToolRole
    deriving (Show, Eq, Generic)

instance FromJSON MessageRole
instance ToJSON MessageRole

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Check if a conversation is currently active.
isConversationActive :: ConversationState -> Bool
isConversationActive cs = case cs.conversationStatus of
    ConversationActive -> True
    _ -> False

-- | Check if a turn has completed (successfully or with failure).
isTurnCompleted :: TurnState -> Bool
isTurnCompleted ts = case ts.turnStatus of
    TurnCompleted _ -> True
    TurnFailed _ -> True
    _ -> False

-- | Check if a tool call has completed (successfully or with failure).
isToolCallCompleted :: ToolCallState -> Bool
isToolCallCompleted tcs = case tcs.tcStatus of
    TcCompleted _ -> True
    TcFailed _ -> True
    TcCancelled -> True
    _ -> False

-- | Get the result of a completed tool call, if available.
getToolCallResult :: ToolCallState -> Maybe Value
getToolCallResult tcs = case tcs.tcStatus of
    TcCompleted val -> Just val
    _ -> tcs.tcResult

-- | Default maximum depth for nested tool calls.
defaultMaxToolCallDepth :: Int
defaultMaxToolCallDepth = 10
