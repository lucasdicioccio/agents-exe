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

    -- * Pending calls (Phase 3c)
    pendingSummaryLine,
    selectedPendingCall,
    nextPendingToken,
    failedCallText,
    hookFailedStatusText,

    -- * Utility Functions
    updateConversationSession,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Base (ConversationId (..))
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.Protocol (NewMessage (..))
import System.Agents.Session.Base (ContinuationToken, DeferredCallView (..), Session, SessionId, SessionStatus (..))
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
    , conversationPending :: [DeferredCallView]
    {- ^ The run's deferred calls (Phase 3c, @todos/os-as-standalone-server.md@
    Design §4), set from 'AppEvent_CallsDeferred' and cleared on
    @run.started@ -- the Pending panel's source of truth. Answered through
    'System.Agents.Host.Client.completeCall' by
    'System.Agents.TUI.Event.Pending.handleAnswerPending'.
    -}
    }
    deriving (Show)

{- | The Pending panel's collapsed summary line: @"N pending calls: tool_a,
tool_b"@, or @""@ for none (the panel is hidden then, like the Draft one).
-}
pendingSummaryLine :: [DeferredCallView] -> Text
pendingSummaryLine [] = ""
pendingSummaryLine calls =
    Text.pack (show n) <> (if n == 1 then " pending call: " else " pending calls: ") <> names
  where
    n = length calls
    names = Text.intercalate ", " (map dcvToolName calls)

{- | The pending call the Pending panel acts on: the one holding the selected
continuation token, else the first call that has a token (a call without
one cannot be completed from the TUI).
-}
selectedPendingCall :: Maybe ContinuationToken -> [DeferredCallView] -> Maybe DeferredCallView
selectedPendingCall selected calls =
    case [c | c <- completable, selected /= Nothing, dcvToken c == selected] of
        (c : _) -> Just c
        [] -> case completable of
            (c : _) -> Just c
            [] -> Nothing
  where
    completable = [c | c <- calls, dcvToken c /= Nothing]

{- | The token after the selected one, wrapping around, among the calls that
can be completed. 'Nothing' when there are none. With no (or a vanished)
selection the first call is the current one, so the next is the second.
-}
nextPendingToken :: Maybe ContinuationToken -> [DeferredCallView] -> Maybe ContinuationToken
nextPendingToken selected calls =
    case tokens of
        [] -> Nothing
        _ ->
            let current = dcvToken =<< selectedPendingCall selected calls
                idx = maybe 0 id (current >>= \t -> lookup t (zip tokens [0 ..]))
             in Just (tokens !! ((idx + 1) `mod` length tokens))
  where
    tokens = [t | c <- calls, Just t <- [dcvToken c]]

{- | The text a failed call reports to the model: what a failing tool's
error text reads ('Error: ...'), carrying the user's reason.
-}
failedCallText :: Text -> Text
failedCallText reason
    | Text.null (Text.strip reason) = "Error: the user declined this call"
    | otherwise = "Error: " <> Text.strip reason

{- | The status-bar line for a @hook.failed@ event: the run continues (a
hook failure is not a session failure), so this is only a notification.
-}
hookFailedStatusText :: Text -> Text
hookFailedStatusText msg = "Hook failed: " <> Text.strip msg

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
