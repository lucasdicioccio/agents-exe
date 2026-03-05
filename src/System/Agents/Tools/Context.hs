{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tool execution context for passing session metadata to tools.
--
-- This module provides a structured record that encapsulates all session-related
-- metadata that tools might need access to during execution. The context is
-- intentionally kept separate from LLM-facing types to maintain a clean
-- separation of concerns.
--
-- Design decisions:
--
-- * Structured record (not generic Aeson.Value) for type safety and clarity
-- * Optional 'Session' allows performance optimization when full context isn't needed
-- * Separate module to maintain clean import hierarchy
--
module System.Agents.Tools.Context (
    -- * Core type
    ToolExecutionContext (..),

    -- * Construction
    mkToolExecutionContext,
    mkMinimalContext,

    -- * Access helpers
    hasFullSession,
    hasAgentId,
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import System.Agents.Base (AgentId, ConversationId)
import System.Agents.Session.Base (Session, SessionId, TurnId)

-------------------------------------------------------------------------------
-- Tool Execution Context
-------------------------------------------------------------------------------

-- | Context containing session metadata passed to tools during execution.
--
-- This record encapsulates identifiers and optional full session data that
-- tools may need to access without exposing these details to the LLM.
--
-- The context is designed to be:
--
-- * Serializable (ToJSON/FromJSON instances) for potential persistence or
--   transmission
-- * Optional where appropriate (e.g., 'ctxFullSession', 'ctxAgentId') to
--   support different use cases
-- * LLM-agnostic - no types from LLM interfaces are included
--
-- === Fields
--
-- * 'ctxSessionId' - Unique identifier for the current session. Always present.
-- * 'ctxConversationId' - Identifier for the conversation this session belongs to.
-- * 'ctxTurnId' - Identifier for the current turn within the session.
-- * 'ctxAgentId' - Optional identifier for the agent executing the tool.
--   May be 'Nothing' for anonymous or system-level tool executions.
-- * 'ctxFullSession' - Optional complete serialized session. Tools that need
--   access to conversation history or other session state can use this.
--   Left as 'Nothing' for performance when only identifiers are needed.
--
data ToolExecutionContext = ToolExecutionContext
    { ctxSessionId :: SessionId
    -- ^ The current session identifier. This uniquely identifies the
    -- session within which the tool is being executed.
    , ctxConversationId :: ConversationId
    -- ^ The conversation identifier that groups related sessions together.
    -- Multiple sessions may belong to the same conversation (e.g., forked sessions).
    , ctxTurnId :: TurnId
    -- ^ The current turn identifier within the session. This allows tools
    -- to understand their position in the conversation flow.
    , ctxAgentId :: Maybe AgentId
    -- ^ Optional agent identifier. Present when a specific agent is executing
    -- the tool, 'Nothing' for system-level or anonymous executions.
    , ctxFullSession :: Maybe Session
    -- ^ Optional full serialized session. When present, tools can access
    -- complete conversation history and session state. This is made optional
    -- to avoid performance overhead when not needed and to prevent circular
    -- dependencies in the module hierarchy.
    }
    deriving (Show, Eq, Generic)

-- | JSON serialization support for 'ToolExecutionContext'.
-- Enables persistence and transmission of context across boundaries.
instance ToJSON ToolExecutionContext
instance FromJSON ToolExecutionContext

-------------------------------------------------------------------------------
-- Construction Helpers
-------------------------------------------------------------------------------

-- | Create a complete 'ToolExecutionContext' with all fields populated.
--
-- This is the preferred constructor when you have all context information
-- available and want to provide tools with full session access.
--
-- Example:
--
-- @
-- context <- mkToolExecutionContext
--     sessionId
--     conversationId
--     turnId
--     (Just agentId)
--     (Just fullSession)
-- @
mkToolExecutionContext
    :: SessionId
    -> ConversationId
    -> TurnId
    -> Maybe AgentId
    -> Maybe Session
    -> ToolExecutionContext
mkToolExecutionContext = ToolExecutionContext

-- | Create a minimal 'ToolExecutionContext' with only required identifiers.
--
-- Use this when you don't have (or don't need) agent identification or
-- full session data. This is appropriate for simple tools that only need
-- to log or track their execution context.
--
-- Example:
--
-- @
-- context <- mkMinimalContext sessionId conversationId turnId
-- @
mkMinimalContext
    :: SessionId
    -> ConversationId
    -> TurnId
    -> ToolExecutionContext
mkMinimalContext sessId convId tId =
    ToolExecutionContext
        { ctxSessionId = sessId
        , ctxConversationId = convId
        , ctxTurnId = tId
        , ctxAgentId = Nothing
        , ctxFullSession = Nothing
        }

-------------------------------------------------------------------------------
-- Query Helpers
-------------------------------------------------------------------------------

-- | Check if the context contains a full session.
--
-- Useful for tools that need to decide whether they can access conversation
-- history or other session state.
--
-- @
-- if hasFullSession ctx
--     then processWithHistory (fromJust $ ctxFullSession ctx)
--     else processWithoutHistory
-- @
hasFullSession :: ToolExecutionContext -> Bool
hasFullSession = maybe False (const True) . ctxFullSession

-- | Check if the context has an associated agent identifier.
--
-- Tools can use this to determine if they're running in an agent context
-- or as a system-level operation.
hasAgentId :: ToolExecutionContext -> Bool
hasAgentId = maybe False (const True) . ctxAgentId

