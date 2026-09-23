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
    appendDraft,
    draftToMessage,
    shouldShipDraft,
    draftIsEmpty,
    draftParagraphCount,
    draftFirstLine,

    -- * Conversation
    Conversation (..),

    -- * Utility Functions
    updateConversationSession,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Base (ConversationId (..))
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.Protocol (NewMessage (..))
import System.Agents.Session.Base (Session, SessionId, SessionStatus (..))
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

{- | Append text (as a new paragraph, blank-line separated) and\/or media to
a draft. Sending while the model is busy calls this instead of posting
(@todos/os-as-standalone-server.md@ §5): three messages typed in a row
while the run is active become one elaborated draft. Appending empty text
with no media is a no-op.
-}
appendDraft :: Text -> [MediaAttachment] -> Draft -> Draft
appendDraft newText newMedia d =
    d{draftText = joinedText, draftMedia = draftMedia d ++ newMedia}
  where
    trimmed = Text.strip newText
    joinedText
        | Text.null trimmed = draftText d
        | Text.null (Text.strip (draftText d)) = trimmed
        | otherwise = draftText d <> "\n\n" <> trimmed

-- | Whether a draft has nothing to post.
draftIsEmpty :: Draft -> Bool
draftIsEmpty d = Text.null (Text.strip (draftText d)) && null (draftMedia d)

{- | Turn a non-empty draft into the single 'NewMessage' it ships as (§5):
paragraphs already joined by 'appendDraft', media concatenated, never an
interrupt (a draft is always ordinary input, never 'nmInterrupt'). Nothing
for an empty draft.
-}
draftToMessage :: Draft -> Maybe NewMessage
draftToMessage d
    | draftIsEmpty d = Nothing
    | otherwise = Just NewMessage{nmText = draftText d, nmMedia = draftMedia d, nmInterrupt = False}

{- | Whether a 'SessionStatus' accepts input right now, i.e. whether a
pending draft should ship on @run.stopped@ (§5): ready and idle sessions
do; a paused, deferred-blocked, still-running or failed one keeps its
draft until it is resumed and stops again.
-}
shouldShipDraft :: SessionStatus -> Bool
shouldShipDraft StatusIdle = True
shouldShipDraft StatusReady = True
shouldShipDraft StatusRunning = False
shouldShipDraft StatusWaitingExternal = False
shouldShipDraft StatusPaused = False
shouldShipDraft StatusFailed = False

-- | Number of paragraphs in a draft (blank-line separated), 0 for an empty one.
draftParagraphCount :: Draft -> Int
draftParagraphCount d
    | Text.null (Text.strip (draftText d)) = 0
    | otherwise = length (paragraphsOf (draftText d))
  where
    paragraphsOf =
        filter (not . Text.null) . map Text.strip . splitOnBlankLines

    splitOnBlankLines :: Text -> [Text]
    splitOnBlankLines = Text.splitOn "\n\n"

-- | The draft's first line, for the collapsed Draft tab view.
draftFirstLine :: Draft -> Text
draftFirstLine d = case Text.lines (draftText d) of
    (l : _) -> l
    [] -> ""

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
