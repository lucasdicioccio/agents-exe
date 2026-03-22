{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

{- |
Conversation tracking and lineage for the OS architecture.

This module provides the public API for conversation, turn, and tool call
tracking using the ECS (Entity-Component-System) pattern. It enables:

- **Conversation Management**: Track top-level user-agent interactions
- **Turn Tracking**: Follow individual steps within conversations
- **Tool Call Lineage**: Trace nested tool invocations
- **Branching Support**: Fork conversations to explore multiple responses
- **Audit Trails**: Complete history for debugging and accounting

== Quick Start

Create a conversation:

@
import System.Agents.OS.Core
import System.Agents.OS.Conversation

-- Create a new conversation
let config = ConversationConfig
        { conversationTitle = Just "My Chat"
        , conversationMetadata = Map.empty
        }

-- In STM:
convId <- createEntity
now <- getCurrentTime
lastActivity <- newTVar now
let state = ConversationState
        { conversationAgentId = agentId
        , conversationStatus = ConversationActive
        , conversationStartedAt = now
        , conversationLastActivity = lastActivity
        }
setComponent world convId config
setComponent world convId state
@

Track a turn:

@
turnId <- createEntity
let turnConfig = TurnConfig
        { turnConversationId = convId
        , turnParentTurnId = Nothing  -- Root turn
        }
let turnState = TurnState
        { turnStatus = TurnStarting
        , turnStartedAt = now
        , turnCompletedAt = Nothing
        }
setComponent world turnId turnConfig
setComponent world turnId turnState
@

Build lineage context:

@
let lineage = pushLineage ConversationFrame convId now $
              pushLineage TurnFrame turnId now emptyLineage
let context = buildLineageContext lineage
-- context contains: depth, path, frame types, IDs
@

== Design Notes

1. **Component Pattern**: All types are ECS components that can be attached
   to entities. This enables flexible querying and composition.

2. **Phantom Types**: Entity IDs are phantom-typed (ConversationId, TurnId,
   etc.) for type safety.

3. **Branching**: Turns have optional parent references enabling tree
   structures for exploring multiple responses.

4. **Nested Calls**: Tool calls have parent references for tracking
   recursive/nested invocations.

5. **Thread Safety**: ConversationState uses a TVar for lastActivity to
   allow concurrent updates.
-}
module System.Agents.OS.Conversation (
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

    -- * Lineage Types
    Lineage (..),
    LineageFrame (..),
    FrameType (..),

    -- * Lineage Operations
    emptyLineage,
    pushLineage,
    lineageDepth,
    lineageHead,
    lineageRoot,
    buildLineageContext,
    buildLineagePath,
    findFramesByType,
    findConversationFrames,
    findTurnFrames,
    findToolCallFrames,
    isInConversation,
    isInTurn,
    currentFrameType,

    -- * Utility Functions
    isConversationActive,
    isTurnCompleted,
    isToolCallCompleted,
    getToolCallResult,
    defaultMaxToolCallDepth,

    -- * Constants
    conversationConfigComponentId,
    conversationStateComponentId,
    agentConversationComponentId,
    turnConfigComponentId,
    turnStateComponentId,
    toolCallConfigComponentId,
    toolCallStateComponentId,
    lineageComponentId,
    messageComponentId,
) where

import Data.Proxy (Proxy (..))

import System.Agents.OS.Conversation.Lineage
import System.Agents.OS.Conversation.Types
import System.Agents.OS.Core.Types (Component (..), ComponentTypeId (..))

-------------------------------------------------------------------------------
-- Component ID Constants
-------------------------------------------------------------------------------

-- | ComponentTypeId for ConversationConfig (30)
conversationConfigComponentId :: ComponentTypeId
conversationConfigComponentId = componentId (Proxy @ConversationConfig)

-- | ComponentTypeId for ConversationState (31)
conversationStateComponentId :: ComponentTypeId
conversationStateComponentId = componentId (Proxy @ConversationState)

-- | ComponentTypeId for AgentConversation (32)
agentConversationComponentId :: ComponentTypeId
agentConversationComponentId = componentId (Proxy @AgentConversation)

-- | ComponentTypeId for TurnConfig (33)
turnConfigComponentId :: ComponentTypeId
turnConfigComponentId = componentId (Proxy @TurnConfig)

-- | ComponentTypeId for TurnState (34)
turnStateComponentId :: ComponentTypeId
turnStateComponentId = componentId (Proxy @TurnState)

-- | ComponentTypeId for ToolCallConfig (35)
toolCallConfigComponentId :: ComponentTypeId
toolCallConfigComponentId = componentId (Proxy @ToolCallConfig)

-- | ComponentTypeId for ToolCallState (36)
toolCallStateComponentId :: ComponentTypeId
toolCallStateComponentId = componentId (Proxy @ToolCallState)

-- | ComponentTypeId for Lineage (37)
lineageComponentId :: ComponentTypeId
lineageComponentId = componentId (Proxy @Lineage)

-- | ComponentTypeId for Message (38)
messageComponentId :: ComponentTypeId
messageComponentId = componentId (Proxy @Message)

