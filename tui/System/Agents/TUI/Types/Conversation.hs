{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.Agents.TUI.Types.Conversation
Description : Types for conversation management in the TUI

This module defines types related to chat conversations and message handling,
including conversation status, the Conversation type itself, and utility functions.

Phase 3b-i (@todos/os-as-standalone-server.md@, Design §4): a 'Conversation'
is a client-side view of a runner session -- its 'SessionId' (D4: the
'ConversationId' is always 'sessionIdToConversationId' of it), the latest
'Session'\/'SessionMeta' pulled from 'Client.getSession' or pushed by a
'AppEvent_SessionUpdated', and a local, unsent 'Draft' (§5, D3). No
'ThreadId', no 'BChan', no callback, no 'TuiAgent' handle.
-}
module System.Agents.TUI.Types.Conversation (
    -- * Conversation Status
    ConversationStatus (..),

    -- * Draft buffer
    Draft (..),
    emptyDraft,

    -- * Conversation
    Conversation (..),

    -- * Utility Functions
    updateConversationSession,
) where

import Data.Text (Text)

import System.Agents.Base (ConversationId (..))
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.Session.Base (Session, SessionId)
import System.Agents.SessionStore (SessionMeta)

-------------------------------------------------------------------------------
-- Conversation Types
-------------------------------------------------------------------------------

-- | Status of a conversation regarding its execution state.
data ConversationStatus
    = -- | Conversation is currently running (agent is processing)
      ConversationStatus_Active
    | -- | Conversation is waiting for user input
      ConversationStatus_WaitingForInput
    | -- | Conversation is paused (@Control Pause@ mail, or a TUI-local
      -- "don't send input yet" toggle -- see
      -- 'System.Agents.TUI.Event.Conversation.handleTogglePauseConversation').
      ConversationStatus_Paused
    | -- | The run stopped on deferred calls: an external worker must
      -- complete them ('System.Agents.Host.Client.completeCall') before
      -- the session can run again.
      ConversationStatus_BlockedOnDeferred
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Draft buffer (§5, D3)
-------------------------------------------------------------------------------

{- | One editable, unsent draft per conversation, with append semantics
(@todos/os-as-standalone-server.md@ §5). Never crosses the wire and is
never persisted: it is TUI-local state, like the contents of the message
editor, posted whole as a single 'System.Agents.Protocol.NewMessage' once
the session accepts input again.
-}
data Draft = Draft
    { draftText :: Text
    , draftMedia :: [MediaAttachment]
    }
    deriving (Show, Eq)

-- | The draft a new conversation starts with.
emptyDraft :: Draft
emptyDraft = Draft{draftText = "", draftMedia = []}

{- | A conversation with an agent: the client-side view of one runner
session.

Conversations can be either root-level user-initiated conversations
or subcalls (nested agent invocations). Subcall conversations have
additional metadata for TUI visibility and lineage tracking.
-}
data Conversation = Conversation
    { conversationId :: ConversationId
    -- ^ Always 'System.Agents.SessionStore.sessionIdToConversationId'
    -- 'conversationSessionId' (D4).
    , conversationSessionId :: SessionId
    , conversationAgentSlug :: Text
    , conversationSession :: Maybe Session
    -- ^ 'Nothing' until the first 'Client.getSession'\/'AppEvent_SessionUpdated'.
    , conversationMeta :: Maybe SessionMeta
    , conversationName :: Text
    , conversationStatus :: ConversationStatus
    -- ^ Current status of the conversation
    , conversationIsSubcall :: Bool
    -- ^ Whether this is a subcall (nested agent invocation)
    , conversationParentId :: Maybe ConversationId
    -- ^ Parent conversation ID for subcalls (Nothing for root conversations)
    , conversationSubcallDepth :: Int
    -- ^ Subcall nesting depth (0 = root, 1+ = nested)
    , conversationDraft :: Draft
    -- ^ The unsent draft (§5); never posted until the session's turn.
    }
    deriving (Show)

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Update a conversation's session and metadata in a list of conversations.
updateConversationSession :: ConversationId -> Session -> SessionMeta -> [Conversation] -> [Conversation]
updateConversationSession targetConvId newSession newMeta =
    map
        ( \conv ->
            if conversationId conv == targetConvId
                then conv{conversationSession = Just newSession, conversationMeta = Just newMeta}
                else conv
        )
