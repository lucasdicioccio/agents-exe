{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.Agents.TUI.Types.Conversation
Description : Types for conversation management in the TUI

This module defines types related to chat conversations and message handling,
including conversation status, the Conversation type itself, and utility functions.
-}
module System.Agents.TUI.Types.Conversation
    ( -- * Conversation Status
      ConversationStatus (..)

      -- * Conversation
    , Conversation (..)

      -- * Utility Functions
    , updateConversationSession
    ) where

import Brick.BChan (BChan)
import Control.Concurrent (ThreadId)
import Data.Text (Text)

import System.Agents.Base (ConversationId (..))
import System.Agents.Session.Base (OnSessionProgress, Session, UserQuery)
import System.Agents.TUI.Types.Core (TuiAgent)

-------------------------------------------------------------------------------
-- Conversation Types
-------------------------------------------------------------------------------

-- | Status of a conversation regarding its execution state.
data ConversationStatus
    = -- | Conversation is currently running (agent is processing)
      ConversationStatus_Active
    | -- | Conversation is waiting for user input
      ConversationStatus_WaitingForInput
    | -- | Conversation is paused (blocks step iteration until unpaused)
      ConversationStatus_Paused
    deriving (Show, Eq)

{- | A conversation with an agent.

Conversations can be either root-level user-initiated conversations
or subcalls (nested agent invocations). Subcall conversations have
additional metadata for TUI visibility and lineage tracking.
-}
data Conversation = Conversation
    { conversationId :: ConversationId
    , conversationAgent :: TuiAgent
    , conversationThreadId :: Maybe ThreadId
    -- ^ Nothing for restored conversations that haven't been continued yet
    , conversationSession :: Maybe Session
    , conversationName :: Text
    , conversationChan :: BChan (Maybe UserQuery)
    , conversationStatus :: ConversationStatus
    -- ^ Current status of the conversation
    , conversationOnProgress :: OnSessionProgress
    -- ^ Callback for session progress updates
    , conversationIsSubcall :: Bool
    -- ^ Whether this is a subcall (nested agent invocation)
    , conversationParentId :: Maybe ConversationId
    -- ^ Parent conversation ID for subcalls (Nothing for root conversations)
    , conversationSubcallDepth :: Int
    -- ^ Subcall nesting depth (0 = root, 1+ = nested)
    }

-- | Manual Show instance for Conversation.
instance Show Conversation where
    show conv =
        "Conversation {conversationId = "
            ++ show conv.conversationId
            ++ ", conversationAgent = "
            ++ show conv.conversationAgent
            ++ ", conversationName = "
            ++ show conv.conversationName
            ++ ", conversationIsSubcall = "
            ++ show conv.conversationIsSubcall
            ++ ", conversationParentId = "
            ++ show conv.conversationParentId
            ++ ", ...}"

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Update a conversation's session in a list of conversations.
updateConversationSession :: ConversationId -> Session -> [Conversation] -> [Conversation]
updateConversationSession targetConvId newSession =
    map
        ( \conv ->
            if conversationId conv == targetConvId
                then conv{conversationSession = Just newSession}
                else conv
        )

