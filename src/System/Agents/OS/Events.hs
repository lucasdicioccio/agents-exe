{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | OS Event types for subcall visibility and TUI integration.

This module provides event types that are used for communication between
the OS layer and the TUI, particularly for tracking subcall lifecycle
(started, progress, completed, failed).

By placing these types in a separate module, we avoid circular dependencies
between OS.Interfaces, Session.Base, and Tools.Context.
-}
module System.Agents.OS.Events (
    -- * OS Event Types
    OSEvent (..),
) where

import Data.Aeson (Value)
import Data.Text (Text)

import System.Agents.Base (AgentId, ConversationId)
import System.Agents.Session.Types (Session, SessionProgress)

-- | Events that can be emitted by the OS.
data OSEvent
    = OSEvent_AgentStarted AgentId
    | OSEvent_AgentStopped AgentId
    | OSEvent_ConversationStarted ConversationId AgentId
    | OSEvent_ConversationProgress ConversationId SessionProgress
    | OSEvent_ConversationCompleted ConversationId
    | OSEvent_ConversationFailed ConversationId Text
    | OSEvent_ToolCalled AgentId Text Value
    | OSEvent_ToolCompleted AgentId Text Value
    | OSEvent_Error Text
    | OSEvent_Shutdown
    | -- ** Subcall Events for TUI visibility
      -- | Emitted when a subcall (helper agent invocation) starts
      OSEvent_SubcallStarted
        { subcallParentConversationId :: ConversationId
        -- ^ The parent conversation that initiated the subcall
        , subcallConversationId :: ConversationId
        -- ^ The new conversation ID for the subcall
        , subcallAgentSlug :: Text
        -- ^ The slug of the agent being called
        , subcallDepth :: Int
        -- ^ The recursion depth of this subcall (0 = root)
        }
    | -- | Emitted when a subcall makes progress
      OSEvent_SubcallProgress
        { subcallProgressConversationId :: ConversationId
        -- ^ The subcall conversation ID
        , subcallProgressSession :: Session
        -- ^ The current session state
        }
    | -- | Emitted when a subcall completes successfully
      OSEvent_SubcallCompleted
        { subcallCompletedConversationId :: ConversationId
        -- ^ The subcall conversation ID
        , subcallCompletedResult :: Text
        -- ^ The result/response text from the subcall
        }
    | -- | Emitted when a subcall fails
      OSEvent_SubcallFailed
        { subcallFailedConversationId :: ConversationId
        -- ^ The subcall conversation ID
        , subcallFailedError :: Text
        -- ^ The error message
        }
    deriving (Show)

