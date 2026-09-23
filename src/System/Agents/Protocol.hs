{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | The wire protocol: every type a client and the runner exchange, with
its JSON encoding, in one place.

@AgentsServer.Api@ (the HTTP layer), the in-process runner client, and any
future socket client all speak these types; nothing else in the codebase
defines a JSON shape for a command, an event, or a runner error. The
encodings match what @agents-server@ already emitted before this module
existed, field for field, so nothing that already parses these events (the
chat page, a script watching @tool.started@\/@tool.completed@) breaks: this
module only *adds* fields (@kind@, @seq@) that were implicit in the SSE
@event:@ line before.

This module must not depend on @wai@\/@warp@\/@servant@: it is used from
@agents-lib@ itself (the runner stamps 'Event's), not only from the HTTP
example.
-}
module System.Agents.Protocol (
    -- * Sequence numbers
    EventSeq (..),
    firstEventSeq,
    nextEventSeq,

    -- * Events
    Event (..),
    EventBody (..),
    eventKind,

    -- * Commands' supporting types
    RunMode (..),
    runModeText,
    runModeFromText,
    NewMessage (..),
    RunnerError (..),
    runnerErrorCode,
    runnerErrorMessage,
    DeleteMode (..),
    DeletionPlan (..),
    SubscribeScope (..),
) where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID

import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.Session.Base (
    ContinuationToken (..),
    DeferredCallView,
    SessionId (..),
    SessionStatus (..),
    ToolCallId,
    Turn,
    sessionStatusText,
 )
import System.Agents.SessionStore (SessionMeta, VersionConflict (..))
import System.Agents.Tools.Params.Types (ParamName)

-------------------------------------------------------------------------------
-- Sequence numbers
-------------------------------------------------------------------------------

-- | A total order of events on one server, assigned by the runner at 'emit'.
newtype EventSeq = EventSeq Int64
    deriving (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)

firstEventSeq :: EventSeq
firstEventSeq = EventSeq 1

nextEventSeq :: EventSeq -> EventSeq
nextEventSeq (EventSeq n) = EventSeq (n + 1)

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

{- | What happened, in the order it happened. 'evSeq' is unique and
increasing per server; 'evSession' names the session the event is about,
when it is about one (every current kind but 'SessionCreated' and
'SessionDeleted', which are server\/owner-wide); 'evOwner' is that
session's recorded owner at the time of the event (or the deleted/created
session's owner for those two), used to filter an owner-scoped
subscription ('SubscribeScope').
-}
data Event = Event
    { evSeq :: EventSeq
    , evSession :: Maybe SessionId
    , evOwner :: Maybe Text
    , evBody :: EventBody
    }
    deriving (Show, Eq)

-- | The event's kind, unchanged from the 'kind' the old @SessionEvent@ used
-- as its SSE @event:@ line.
eventKind :: EventBody -> Text
eventKind = \case
    RunStarted{} -> "run.started"
    SessionUpdated{} -> "session.updated"
    CallsDeferred{} -> "calls.deferred"
    RunStopped{} -> "run.stopped"
    SessionFailed{} -> "session.failed"
    TextDelta{} -> "text.delta"
    ToolCallStarted{} -> "tool.started"
    ToolCallCompleted{} -> "tool.completed"
    SessionCreated{} -> "session.created"
    SessionDeleted{} -> "session.deleted"

{- | The body of an 'Event'. Every constructor that used to carry a
'SessionId' (the old @SessionEvent@) no longer does: it is 'evSession'
instead.

Phase 2c (@todos/os-as-standalone-server.md@) adds @SubcallStarted@,
@SubcallCompleted@, @SubcallFailed@ and @ToolCallProgressed@ here, once
'ctxEmit' replaces 'ctxEventQueue'. Not added yet.
-}
data EventBody
    = RunStarted RunMode
    | -- | A new version was stored; carries the head turn.
      SessionUpdated SessionMeta (Maybe Turn)
    | -- | The run stopped on deferred calls, which an external worker must complete.
      CallsDeferred [DeferredCallView]
    | RunStopped SessionStatus
    | SessionFailed Text
    | -- | A piece of the LLM's answer, when the host streams tokens.
      TextDelta Text
    | -- | A tracked tool call started running.
      ToolCallStarted ToolCallId Text
    | -- | A tracked tool call reached a final state.
      ToolCallCompleted ToolCallId Text Bool
    | -- | A session was created (server\/owner-wide feed only).
      SessionCreated SessionMeta
    | -- | A session was deleted (server\/owner-wide feed only).
      SessionDeleted SessionId
    deriving (Show, Eq)

instance Aeson.ToJSON Event where
    toJSON ev = withFields (Aeson.object (bodyPairs ev.evBody)) extra
      where
        extra =
            ["kind" .= eventKind ev.evBody, "seq" .= ev.evSeq]
                <> ["session_id" .= sid | Just sid <- [ev.evSession]]
                <> ["owner" .= o | Just o <- [ev.evOwner]]

instance Aeson.FromJSON Event where
    parseJSON = Aeson.withObject "Event" $ \o -> do
        seq_ <- o .: "seq"
        sid <- o .:? "session_id"
        owner <- o .:? "owner"
        kind <- o .: "kind"
        body <- bodyFromKindAndObject kind o
        pure $ Event seq_ sid owner body

{- | Merge extra fields into a JSON object, the extra fields taking priority
over a same-named field the object already had (used for every event kind:
'SessionMeta' has its own @session_id@\/@owner@ fields, which agree with
'Event'\'s own in practice -- both name the same session -- but the
'Event'-level ones are authoritative).
-}
withFields :: Aeson.Value -> [Aeson.Pair] -> Aeson.Value
withFields (Aeson.Object o) pairs = Aeson.Object (KeyMap.fromList pairs <> o)
withFields other _ = other

bodyPairs :: EventBody -> [Aeson.Pair]
bodyPairs = \case
    RunStarted mode -> ["mode" .= mode]
    SessionUpdated meta turn -> metaPairs meta <> ["head_turn" .= turn]
    CallsDeferred calls -> ["calls" .= calls]
    RunStopped status -> ["status" .= status]
    SessionFailed msg -> ["message" .= msg]
    TextDelta text -> ["text" .= text]
    ToolCallStarted callId toolName -> ["tool_call_id" .= callId, "tool" .= toolName]
    ToolCallCompleted callId toolName ok -> ["tool_call_id" .= callId, "tool" .= toolName, "succeeded" .= ok]
    SessionCreated meta -> metaPairs meta
    SessionDeleted sid -> ["session_id" .= sid]
  where
    metaPairs meta = case Aeson.toJSON meta of
        Aeson.Object o -> KeyMap.toList o
        _ -> []

bodyFromKindAndObject :: Text -> Aeson.Object -> Aeson.Parser EventBody
bodyFromKindAndObject kind o = case kind of
    "run.started" -> RunStarted <$> o .: "mode"
    "session.updated" -> SessionUpdated <$> Aeson.parseJSON (Aeson.Object o) <*> o .:? "head_turn"
    "calls.deferred" -> CallsDeferred <$> o .: "calls"
    "run.stopped" -> RunStopped <$> o .: "status"
    "session.failed" -> SessionFailed <$> o .: "message"
    "text.delta" -> TextDelta <$> o .: "text"
    "tool.started" -> ToolCallStarted <$> o .: "tool_call_id" <*> o .: "tool"
    "tool.completed" -> ToolCallCompleted <$> o .: "tool_call_id" <*> o .: "tool" <*> o .: "succeeded"
    "session.created" -> SessionCreated <$> Aeson.parseJSON (Aeson.Object o)
    "session.deleted" -> SessionDeleted <$> o .: "session_id"
    other -> fail ("unknown event kind: " <> Text.unpack other)

-------------------------------------------------------------------------------
-- RunMode
-------------------------------------------------------------------------------

-- | How far a run goes.
data RunMode
    = -- | One step, e.g. one LLM call or one batch of tool calls.
      StepOnce
    | -- | Until the LLM answers, or only deferred calls remain.
      UntilBlocked
    deriving (Show, Eq)

runModeText :: RunMode -> Text
runModeText = \case
    StepOnce -> "step"
    UntilBlocked -> "until_blocked"

runModeFromText :: Text -> Maybe RunMode
runModeFromText = \case
    "step" -> Just StepOnce
    "until_blocked" -> Just UntilBlocked
    _ -> Nothing

instance Aeson.ToJSON RunMode where
    toJSON = Aeson.String . runModeText

instance Aeson.FromJSON RunMode where
    parseJSON = Aeson.withText "RunMode" $ \txt ->
        maybe (fail ("run mode must be \"step\" or \"until_blocked\": " <> Text.unpack txt)) pure (runModeFromText txt)

-------------------------------------------------------------------------------
-- NewMessage
-------------------------------------------------------------------------------

-- | A user message.
data NewMessage = NewMessage
    { nmText :: Text
    , nmMedia :: [MediaAttachment]
    , nmInterrupt :: Bool
    {- ^ Post this as 'Interrupt'-priority mail rather than 'Normal'
    (@todos/session-mailbox.md@ R3a\/R4): a running session's attached tool
    calls are detached and, if @interruptCompletions@ is on, an in-flight
    LLM completion is cancelled, either way asked again with this message
    folded in. Only meaningful for 'acceptAsMail' (a busy session); ignored
    on the idle path, where there is nothing to interrupt.
    -}
    }
    deriving (Show, Eq)

{- | @{prompt, media: [{mime, base64, filename?}], interrupt}@ -- the wire
shape @POST .../messages@ and @POST /v1/sessions@ already used. Note this
is not 'MediaAttachment'\'s own JSON (@mimeType@\/@base64Data@, used when a
'MediaAttachment' is nested in a stored 'Turn'): the request body has
always spelled it @mime@\/@base64@, so the field names here are kept
distinct on purpose.
-}
instance Aeson.ToJSON NewMessage where
    toJSON m =
        Aeson.object
            [ "prompt" .= m.nmText
            , "media" .= map mediaToJSON m.nmMedia
            , "interrupt" .= m.nmInterrupt
            ]

instance Aeson.FromJSON NewMessage where
    parseJSON = Aeson.withObject "NewMessage" $ \o ->
        NewMessage
            <$> o .: "prompt"
            <*> (maybe (pure []) (mapM mediaFromJSON) =<< o .:? "media")
            <*> (maybe (pure False) pure =<< o .:? "interrupt")

mediaToJSON :: MediaAttachment -> Aeson.Value
mediaToJSON m =
    Aeson.object $
        ["mime" .= m.mediaMimeType, "base64" .= m.mediaBase64Data]
            <> ["filename" .= fname | Just fname <- [m.mediaFilename]]

mediaFromJSON :: Aeson.Value -> Aeson.Parser MediaAttachment
mediaFromJSON = Aeson.withObject "media" $ \m ->
    MediaAttachment <$> m .: "mime" <*> m .: "base64" <*> m .:? "filename"

-------------------------------------------------------------------------------
-- RunnerError
-------------------------------------------------------------------------------

data RunnerError
    = UnknownAgent Text
    | UnknownSession SessionId
    | UnknownToken ContinuationToken
    | TokenAlreadyCompleted ContinuationToken
    | -- | A run owns the session (on delete: a session of the tree, or an ancestor).
      RunInProgress SessionId
    | NoActiveRun SessionId
    | NotAcceptingMessages SessionId SessionStatus
    | Conflict VersionConflict
    | -- | Supplied parameter names the agent does not declare.
      UnknownParams [ParamName]
    | -- | Supplied parameter names that are process-scope, or pinned by the operator.
      ForbiddenParams [ParamName]
    | -- | Supplied values for @secret@ parameters that are not JSON strings.
      InvalidParams [ParamName]
    | -- | Required parameters still unbound once caller-supplied values are merged in.
      MissingRequiredParams [ParamName]
    | -- | The session's mailbox refused the message (full, per 'mailboxMaxUnread').
      MailboxRejected SessionId
    deriving (Show, Eq)

-- | The @error@ code an HTTP answer gives for a 'RunnerError' (matches
-- @AgentsServer.Api.fromRunnerError@'s codes exactly).
runnerErrorCode :: RunnerError -> Text
runnerErrorCode = \case
    UnknownAgent _ -> "unknown_agent"
    UnknownSession _ -> "unknown_session"
    UnknownToken _ -> "unknown_token"
    TokenAlreadyCompleted _ -> "token_already_completed"
    RunInProgress _ -> "run_in_progress"
    NoActiveRun _ -> "no_active_run"
    NotAcceptingMessages{} -> "not_accepting_messages"
    Conflict _ -> "conflict"
    UnknownParams _ -> "unknown_params"
    ForbiddenParams _ -> "forbidden_params"
    InvalidParams _ -> "invalid_params"
    MissingRequiredParams _ -> "params_required"
    MailboxRejected _ -> "mailbox_full"

-- | The human-readable @message@ an HTTP answer gives for a 'RunnerError'.
runnerErrorMessage :: RunnerError -> Text
runnerErrorMessage = \case
    UnknownAgent slug -> "no agent named " <> slug
    UnknownSession sid -> "no session " <> showId sid
    UnknownToken _ -> "no pending call has this continuation token"
    TokenAlreadyCompleted _ -> "this call already has a result"
    RunInProgress sid -> "a run is active on session " <> showId sid
    NoActiveRun sid -> "no run is active on session " <> showId sid
    NotAcceptingMessages _ status -> "the session is " <> sessionStatusText status <> ", not idle"
    Conflict _ -> "the session was modified by another writer; retry"
    UnknownParams names -> "unknown parameter(s): " <> Text.intercalate ", " names
    ForbiddenParams names -> "process-scope or pinned parameter(s) cannot be set here: " <> Text.intercalate ", " names
    InvalidParams names -> "secret parameter(s) must be given as strings: " <> Text.intercalate ", " names
    MissingRequiredParams names -> "required parameter(s) not bound: " <> Text.intercalate ", " names
    MailboxRejected sid -> "session " <> showId sid <> " has too much unread mail; try again later"
  where
    showId = Text.pack . show

{- | @{error, message}@, exactly what @errorResponse@ used to build by hand.
'Aeson.FromJSON' is necessarily lossy: a caller decoding a server's error
answer gets the code and message back, but not the original structured
payload (a 'SessionId', a 'VersionConflict', a parameter list) -- only the
message says what it was. Every reconstructed value uses an empty\/dummy
payload of the right shape; compare by 'runnerErrorCode', not '(==)', after
a round trip.
-}
instance Aeson.ToJSON RunnerError where
    toJSON e = Aeson.object ["error" .= runnerErrorCode e, "message" .= runnerErrorMessage e]

instance Aeson.FromJSON RunnerError where
    parseJSON = Aeson.withObject "RunnerError" $ \o -> do
        code <- o .: "error"
        msg <- o .: "message"
        pure $ runnerErrorFromCode code msg

-- | Reconstruct a placeholder 'RunnerError' from an @{error, message}@ pair.
runnerErrorFromCode :: Text -> Text -> RunnerError
runnerErrorFromCode code msg = case code of
    "unknown_agent" -> UnknownAgent msg
    "unknown_session" -> UnknownSession placeholderSessionId
    "unknown_token" -> UnknownToken placeholderToken
    "token_already_completed" -> TokenAlreadyCompleted placeholderToken
    "run_in_progress" -> RunInProgress placeholderSessionId
    "no_active_run" -> NoActiveRun placeholderSessionId
    "not_accepting_messages" -> NotAcceptingMessages placeholderSessionId placeholderStatus
    "conflict" -> Conflict (VersionConflict placeholderSessionId 0 0)
    "unknown_params" -> UnknownParams []
    "forbidden_params" -> ForbiddenParams []
    "invalid_params" -> InvalidParams []
    "params_required" -> MissingRequiredParams []
    "mailbox_full" -> MailboxRejected placeholderSessionId
    _ -> UnknownAgent msg
  where
    placeholderSessionId = SessionId UUID.nil
    placeholderToken = ContinuationToken UUID.nil
    placeholderStatus = StatusFailed

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

data DeleteMode = DryRun | DeleteForReal
    deriving (Show, Eq)

instance Aeson.ToJSON DeleteMode where
    toJSON DryRun = Aeson.String "dry_run"
    toJSON DeleteForReal = Aeson.String "delete"

instance Aeson.FromJSON DeleteMode where
    parseJSON = Aeson.withText "DeleteMode" $ \case
        "dry_run" -> pure DryRun
        "delete" -> pure DeleteForReal
        other -> fail ("unknown delete mode: " <> Text.unpack other)

data DeletionPlan = DeletionPlan
    { dpSessions :: [SessionId]
    -- ^ The session and all its descendants, deepest first.
    , dpContinuations :: Int
    -- ^ Continuation rows removed, or that would be.
    , dpDryRun :: Bool
    }
    deriving (Show, Eq)

instance Aeson.ToJSON DeletionPlan where
    toJSON p =
        Aeson.object
            [ "sessions" .= p.dpSessions
            , "continuations" .= p.dpContinuations
            , "dry_run" .= p.dpDryRun
            ]

instance Aeson.FromJSON DeletionPlan where
    parseJSON = Aeson.withObject "DeletionPlan" $ \o ->
        DeletionPlan
            <$> o .: "sessions"
            <*> o .: "continuations"
            <*> o .: "dry_run"

-------------------------------------------------------------------------------
-- Subscriptions
-------------------------------------------------------------------------------

-- | What a subscriber sees.
data SubscribeScope
    = -- | Events of one session.
      OneSession SessionId
    | -- | Events of every session of one owner (or ownerless sessions, when 'Nothing').
      Owner (Maybe Text)
    | -- | Every session's events, server-wide.
      AllSessions
    deriving (Show, Eq)

instance Aeson.ToJSON SubscribeScope where
    toJSON = \case
        OneSession sid -> Aeson.object ["scope" .= ("one_session" :: Text), "session_id" .= sid]
        Owner o -> Aeson.object ["scope" .= ("owner" :: Text), "owner" .= o]
        AllSessions -> Aeson.object ["scope" .= ("all" :: Text)]

instance Aeson.FromJSON SubscribeScope where
    parseJSON = Aeson.withObject "SubscribeScope" $ \o -> do
        scope <- o .: "scope"
        case (scope :: Text) of
            "one_session" -> OneSession <$> o .: "session_id"
            "owner" -> Owner <$> o .:? "owner"
            "all" -> pure AllSessions
            other -> fail ("unknown subscribe scope: " <> Text.unpack other)
