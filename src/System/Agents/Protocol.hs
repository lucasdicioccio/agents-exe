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

    -- * Runner stats
    RunnerStats (..),

    -- * Agent descriptors (G6, Phase 3a)
    AgentDescriptor (..),
    ToolDescriptor (..),
    AgentParameter (..),

    -- * Commands and replies (Phase 3a)
    Command (..),
    Reply (..),
) where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime)
import qualified Data.UUID as UUID

import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.OS.Events (ToolCallActivity)
import System.Agents.Session.Base (
    ContinuationToken (..),
    DeferredCallView,
    Envelope,
    MailBody,
    Priority,
    Receipt,
    Session,
    SessionId (..),
    SessionStatus (..),
    ToolCallId,
    Turn,
    UserToolResponse,
    sessionStatusText,
 )
import System.Agents.SessionStore (SessionMeta, SessionQuery, VersionConflict (..))
import System.Agents.Tools.Activation (Activation)
import System.Agents.Tools.Params.Types (ParamName, ParamScope)

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
    SubcallStarted{} -> "subcall.started"
    SubcallCompleted{} -> "subcall.completed"
    SubcallFailed{} -> "subcall.failed"
    ToolCallProgressed{} -> "tool.progressed"
    SessionCreated{} -> "session.created"
    SessionDeleted{} -> "session.deleted"

{- | The body of an 'Event'. Every constructor that used to carry a
'SessionId' (the old @SessionEvent@) no longer does: it is 'evSession'
instead.

Phase 2c (@todos/os-as-standalone-server.md@) adds @SubcallStarted@,
@SubcallCompleted@, @SubcallFailed@ and @ToolCallProgressed@: subcall
lifecycle and tool-call activity, forwarded through 'ctxEmit' alongside the
TUI's 'System.Agents.OS.Events.OSEvent' queue (which is not retired yet).
A sub-agent run is not (yet, G10) its own runner session, so these carry
'evSession' set to the *parent*'s id -- the session whose event stream a
client is actually subscribed to -- and name the child by the 'SessionId'
'System.Agents.AgentTree.OneShotTool' already generates for it.
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
    | -- | A @prompt_agent_*@ call started a sub-agent run: the parent
      -- session id, the child's session id, the helper's slug, and the
      -- call depth (0 = root).
      SubcallStarted SessionId SessionId Text Int
    | -- | A sub-agent run finished: the child's session id and its result
      -- text, when it produced one.
      SubcallCompleted SessionId (Maybe Text)
    | -- | A sub-agent run failed: the child's session id and the error message.
      SubcallFailed SessionId Text
    | -- | A background tool call started, reported progress, or reached a
      -- final state.
      ToolCallProgressed ToolCallActivity
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
    -- 'evSession' (the top-level "session_id") is the *parent* -- the
    -- session a 'OneSession' subscriber actually watches, per
    -- 'System.Agents.Host.Runner.matchesScope' -- so the child gets its
    -- own key here rather than colliding with it.
    SubcallStarted parent child slug depth ->
        ["parent_session_id" .= parent, "child_session_id" .= child, "agent" .= slug, "depth" .= depth]
    SubcallCompleted child result -> ["child_session_id" .= child, "result" .= result]
    SubcallFailed child msg -> ["child_session_id" .= child, "message" .= msg]
    ToolCallProgressed activity -> activityPairs activity
    SessionCreated meta -> metaPairs meta
    SessionDeleted sid -> ["session_id" .= sid]
  where
    metaPairs meta = case Aeson.toJSON meta of
        Aeson.Object o -> KeyMap.toList o
        _ -> []
    activityPairs activity = case Aeson.toJSON activity of
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
    "subcall.started" -> SubcallStarted <$> o .: "parent_session_id" <*> o .: "child_session_id" <*> o .: "agent" <*> o .: "depth"
    "subcall.completed" -> SubcallCompleted <$> o .: "child_session_id" <*> o .:? "result"
    "subcall.failed" -> SubcallFailed <$> o .: "child_session_id" <*> o .: "message"
    "tool.progressed" -> ToolCallProgressed <$> Aeson.parseJSON (Aeson.Object o)
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
    | {- | 'forkSession' (G6): the turn index does not name a turn of this
      session. The sketch in @todos/os-as-standalone-server.md@ Design §1
      spells this as a @TurnId@, but 'Turn' carries no id of its own --
      only 'Session.turnId' does, and that names the latest turn, not a
      historical one. 'forkSession' addresses a turn the same way the
      TUI's own @handleForkAtTurn@ does: a 0-based index into 'turns'
      (newest first), which this carries instead.
      -}
      UnknownTurn SessionId Int
    | {- | A 'RunnerClient' helper ('System.Agents.Host.Client') got a
      'Reply' of the wrong shape for the 'Command' it sent -- a bug in the
      dispatch table ('inProcessClient'\/a future @httpClient@), not
      something a caller can provoke.
      -}
      UnexpectedReply
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
    UnknownTurn{} -> "unknown_turn"
    UnexpectedReply -> "unexpected_reply"

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
    UnknownTurn sid idx -> "session " <> showId sid <> " has no turn at index " <> Text.pack (show idx)
    UnexpectedReply -> "the runner answered with a reply of the wrong shape for this command"
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
    "unknown_turn" -> UnknownTurn placeholderSessionId 0
    "unexpected_reply" -> UnexpectedReply
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

-------------------------------------------------------------------------------
-- Runner stats
-------------------------------------------------------------------------------

-- | A snapshot of a 'System.Agents.Host.Runner.SessionRunner''s load,
-- as @GET \/healthz@ and 'Stats' report it.
data RunnerStats = RunnerStats
    { rsLiveSessions :: Int
    , rsActiveRuns :: Int
    }
    deriving (Show, Eq)

instance Aeson.ToJSON RunnerStats where
    toJSON s = Aeson.object ["live_sessions" .= s.rsLiveSessions, "active_runs" .= s.rsActiveRuns]

instance Aeson.FromJSON RunnerStats where
    parseJSON = Aeson.withObject "RunnerStats" $ \o ->
        RunnerStats <$> o .: "live_sessions" <*> o .: "active_runs"

-------------------------------------------------------------------------------
-- Agent descriptors (G6)
-------------------------------------------------------------------------------

{- | One declared parameter of an agent, as the Agents tab and @GET
\/v1\/agents@ show it: never the value (a secret must not leak), only
whether the process already has one bound and whether a client could
override it.
-}
data AgentParameter = AgentParameter
    { apName :: ParamName
    , apDescription :: Maybe Text
    , apSecret :: Bool
    , apScope :: ParamScope
    , apRequired :: Bool
    , apBound :: Bool
    , apPinned :: Bool
    }
    deriving (Show, Eq)

instance Aeson.ToJSON AgentParameter where
    toJSON p =
        Aeson.object $
            [ "name" .= p.apName
            , "secret" .= p.apSecret
            , "scope" .= p.apScope
            , "required" .= p.apRequired
            , "bound" .= p.apBound
            , "pinned" .= p.apPinned
            ]
                <> ["description" .= d | Just d <- [p.apDescription]]

instance Aeson.FromJSON AgentParameter where
    parseJSON = Aeson.withObject "AgentParameter" $ \o ->
        AgentParameter
            <$> o .: "name"
            <*> o .:? "description"
            <*> o .: "secret"
            <*> o .: "scope"
            <*> o .: "required"
            <*> o .: "bound"
            <*> o .: "pinned"

{- | One tool an agent's node currently registers, with the activation the
Agents tab renders as @[A]@\/@[D:group]@\/@[a]@ (Widgets.hs):
'Nothing' is the "no override" default (rendered @[a]@), @Just
'System.Agents.Tools.Activation.AlwaysActivated'@ is @[A]@, and @Just
('System.Agents.Tools.Activation.OnDemandActivated' g)@ is @[D:g]@.
Reusing 'Activation' (rather than a fresh sum type) keeps this in sync
with the toolbox configs that set it.
-}
data ToolDescriptor = ToolDescriptor
    { tdName :: Text
    , tdDescription :: Text
    , tdActivation :: Maybe Activation
    }
    deriving (Show, Eq)

instance Aeson.ToJSON ToolDescriptor where
    toJSON t =
        Aeson.object
            [ "name" .= t.tdName
            , "description" .= t.tdDescription
            , "activation" .= t.tdActivation
            ]

instance Aeson.FromJSON ToolDescriptor where
    parseJSON = Aeson.withObject "ToolDescriptor" $ \o ->
        ToolDescriptor
            <$> o .: "name"
            <*> o .: "description"
            <*> o .:? "activation"

{- | A root agent, as the Agents tab (G6, the "agent details" gap row) and
@GET \/v1\/agents@\/@GET \/v1\/agents\/:slug@ need it: everything
'System.Agents.AgentTree.OSAgentNode' currently only exposes live, in
this process's memory, made serializable. A superset of the old
@AgentBody@ (@examples\/agents-server\/src\/AgentsServer\/Types.hs@): every
field that shape had is still here under the same name, plus 'adModel',
'adSystemPrompt' and 'adHelpers'. The one shape change is 'adTools':
formerly a bare list of tool names, now a list of 'ToolDescriptor' (name,
description, activation) -- nothing in the codebase (chat page, tests)
read the old shape, so this widens it rather than adding a parallel field.
-}
data AgentDescriptor = AgentDescriptor
    { adSlug :: Text
    , adDescription :: Text
    -- ^ What the agent announces about itself ('Base.announce').
    , adModel :: Text
    , adSystemPrompt :: [Text]
    , adSource :: Text
    -- ^ @file@ or @database@, exactly as the old @AgentBody.abSource@.
    , adTools :: [ToolDescriptor]
    , adParameters :: [AgentParameter]
    , adHelpers :: [Text]
    -- ^ Slugs of this node's sub-agents (helpers), if any.
    , adUpdatedAt :: Maybe UTCTime
    -- ^ 'Just' only for a @database@ agent.
    , adUpdatedBy :: Maybe Text
    -- ^ 'Just' only for a @database@ agent, when it has an owner.
    , adConfig :: Maybe Aeson.Value
    -- ^ The stored configuration, for a @database@ agent.
    }
    deriving (Show, Eq)

instance Aeson.ToJSON AgentDescriptor where
    toJSON a =
        Aeson.object $
            [ "slug" .= a.adSlug
            , "description" .= a.adDescription
            , "model" .= a.adModel
            , "system_prompt" .= a.adSystemPrompt
            , "source" .= a.adSource
            , "tools" .= a.adTools
            , "parameters" .= a.adParameters
            , "helpers" .= a.adHelpers
            ]
                <> ["updated_at" .= t | Just t <- [a.adUpdatedAt]]
                <> ["updated_by" .= t | Just t <- [a.adUpdatedBy]]
                <> ["config" .= c | Just c <- [a.adConfig]]

instance Aeson.FromJSON AgentDescriptor where
    parseJSON = Aeson.withObject "AgentDescriptor" $ \o ->
        AgentDescriptor
            <$> o .: "slug"
            <*> o .: "description"
            <*> o .: "model"
            <*> o .: "system_prompt"
            <*> o .: "source"
            <*> o .: "tools"
            <*> o .: "parameters"
            <*> o .: "helpers"
            <*> o .:? "updated_at"
            <*> o .:? "updated_by"
            <*> o .:? "config"

-------------------------------------------------------------------------------
-- Commands and replies (Phase 3a)
-------------------------------------------------------------------------------

{- | What a client ('System.Agents.Host.Client.RunnerClient') asks the
runner to do. Mirrors "System.Agents.Host.Runner"'s public operations
field for field and argument for argument (so 'inProcessClient' is a
one-to-one dispatch table), plus 'ListAgents'\/'GetAgent' (G6). No
constructor carries an explicit owner for who is /calling/: that is the
client's own identity ('System.Agents.Host.Client.inProcessClient'\'s
first argument), not something a command can spoof. This differs from
the sketch in @todos/os-as-standalone-server.md@ Design §1 in a few
places the runner's actual signatures forced:

* 'ForkSession' carries a 0-based, newest-first turn index ('Maybe Int'),
  not a @TurnId@ -- see 'UnknownTurn'.
* 'SendMail' carries a 'Priority' and a 'MailBody', not the sketch's
  informal @MailBody@ shorthand.
* 'ListMail', 'ListSessions', 'GetSession', 'ListAgents', 'GetAgent' and
  'AwaitRun' are additions the sketch's prose called for but its type
  sketch omitted.
-}
data Command
    = -- | Parent (lineage only, G6), agent slug, first message (G2:
      -- 'Nothing' creates an idle session), run mode, and caller-supplied
      -- parameter values.
      CreateSession (Maybe SessionId) Text (Maybe NewMessage) (Maybe RunMode) (Map ParamName Aeson.Value)
    | -- | @spawn-session@: parent, helper slug, first message.
      SpawnSession SessionId Text NewMessage
    | PostMessage SessionId NewMessage (Maybe RunMode) (Map ParamName Aeson.Value)
    | Resume SessionId RunMode (Map ParamName Aeson.Value)
    | CompleteCall ContinuationToken UserToolResponse Bool (Map ParamName Aeson.Value)
    | CancelRun SessionId
    | CancelAttached SessionId
    | Pause SessionId
    | SendMail SessionId Priority MailBody
    | ListMail SessionId Bool
    | -- | Source session, turn index (newest-first, 'Nothing' = whole
      -- session), new agent slug ('Nothing' keeps the source's).
      ForkSession SessionId (Maybe Int) (Maybe Text)
    | ListSessions SessionQuery
    | GetSession SessionId
    | ListAgents
    | GetAgent Text
    | DeleteSession SessionId DeleteMode
    | AwaitRun SessionId NominalDiffTime
    | Stats
    deriving (Show, Eq)

-- | The @cmd@ tag a 'Command' encodes and decodes by.
commandTag :: Command -> Text
commandTag = \case
    CreateSession{} -> "create_session"
    SpawnSession{} -> "spawn_session"
    PostMessage{} -> "post_message"
    Resume{} -> "resume"
    CompleteCall{} -> "complete_call"
    CancelRun{} -> "cancel_run"
    CancelAttached{} -> "cancel_attached"
    Pause{} -> "pause"
    SendMail{} -> "send_mail"
    ListMail{} -> "list_mail"
    ForkSession{} -> "fork_session"
    ListSessions{} -> "list_sessions"
    GetSession{} -> "get_session"
    ListAgents -> "list_agents"
    GetAgent{} -> "get_agent"
    DeleteSession{} -> "delete_session"
    AwaitRun{} -> "await_run"
    Stats -> "stats"

instance Aeson.ToJSON Command where
    toJSON cmd = withFields (Aeson.object (commandPairs cmd)) ["cmd" .= commandTag cmd]

commandPairs :: Command -> [Aeson.Pair]
commandPairs = \case
    CreateSession parent agent message mode params ->
        ["parent" .= parent, "agent" .= agent, "message" .= message, "run" .= mode, "params" .= params]
    SpawnSession parent agent message ->
        ["parent" .= parent, "agent" .= agent, "message" .= message]
    PostMessage sid message mode params ->
        ["session_id" .= sid, "message" .= message, "run" .= mode, "params" .= params]
    Resume sid mode params ->
        ["session_id" .= sid, "run" .= mode, "params" .= params]
    CompleteCall token result autoResume params ->
        ["token" .= token, "result" .= result, "resume" .= autoResume, "params" .= params]
    CancelRun sid -> ["session_id" .= sid]
    CancelAttached sid -> ["session_id" .= sid]
    Pause sid -> ["session_id" .= sid]
    SendMail sid priority body -> ["session_id" .= sid, "priority" .= priority, "body" .= body]
    ListMail sid unreadOnly -> ["session_id" .= sid, "unread" .= unreadOnly]
    ForkSession sid atTurn newAgent -> ["session_id" .= sid, "at_turn" .= atTurn, "agent" .= newAgent]
    ListSessions query -> ["query" .= query]
    GetSession sid -> ["session_id" .= sid]
    ListAgents -> []
    GetAgent slug -> ["agent" .= slug]
    DeleteSession sid mode -> ["session_id" .= sid, "mode" .= mode]
    AwaitRun sid limit -> ["session_id" .= sid, "timeout" .= limit]
    Stats -> []

instance Aeson.FromJSON Command where
    parseJSON = Aeson.withObject "Command" $ \o -> do
        tag <- o .: "cmd"
        case (tag :: Text) of
            "create_session" ->
                CreateSession <$> o .:? "parent" <*> o .: "agent" <*> o .:? "message" <*> o .:? "run" <*> paramsField o
            "spawn_session" -> SpawnSession <$> o .: "parent" <*> o .: "agent" <*> o .: "message"
            "post_message" -> PostMessage <$> o .: "session_id" <*> o .: "message" <*> o .:? "run" <*> paramsField o
            "resume" -> Resume <$> o .: "session_id" <*> o .: "run" <*> paramsField o
            "complete_call" -> CompleteCall <$> o .: "token" <*> o .: "result" <*> o .: "resume" <*> paramsField o
            "cancel_run" -> CancelRun <$> o .: "session_id"
            "cancel_attached" -> CancelAttached <$> o .: "session_id"
            "pause" -> Pause <$> o .: "session_id"
            "send_mail" -> SendMail <$> o .: "session_id" <*> o .: "priority" <*> o .: "body"
            "list_mail" -> ListMail <$> o .: "session_id" <*> o .: "unread"
            "fork_session" -> ForkSession <$> o .: "session_id" <*> o .:? "at_turn" <*> o .:? "agent"
            "list_sessions" -> ListSessions <$> o .: "query"
            "get_session" -> GetSession <$> o .: "session_id"
            "list_agents" -> pure ListAgents
            "get_agent" -> GetAgent <$> o .: "agent"
            "delete_session" -> DeleteSession <$> o .: "session_id" <*> o .: "mode"
            "await_run" -> AwaitRun <$> o .: "session_id" <*> o .: "timeout"
            "stats" -> pure Stats
            other -> fail ("unknown command: " <> Text.unpack other)
      where
        paramsField o = maybe Map.empty id <$> o .:? "params"

{- | What a 'Command' answers with. 'inProcessClient' builds one of these
from every runner call's own return shape; 'System.Agents.Host.Client'\'s
typed helpers unwrap the one they expect, failing with
'UnexpectedReply' otherwise.
-}
data Reply
    = RSessionMeta SessionMeta
    | RSession Session SessionMeta
    | RSessions [SessionMeta]
    | RMail [Envelope]
    | RReceipt Receipt
    | RAgents [AgentDescriptor]
    | RAgent AgentDescriptor
    | RDeletion DeletionPlan
    | RUnit
    | RStats RunnerStats
    | -- | 'awaitRun': the latest metadata, and whether a run is still active.
      RAwait SessionMeta Bool
    deriving (Show, Eq)

replyTag :: Reply -> Text
replyTag = \case
    RSessionMeta{} -> "session_meta"
    RSession{} -> "session"
    RSessions{} -> "sessions"
    RMail{} -> "mail"
    RReceipt{} -> "receipt"
    RAgents{} -> "agents"
    RAgent{} -> "agent"
    RDeletion{} -> "deletion"
    RUnit -> "unit"
    RStats{} -> "stats"
    RAwait{} -> "await"

instance Aeson.ToJSON Reply where
    toJSON reply = withFields (Aeson.object (replyPairs reply)) ["reply" .= replyTag reply]

replyPairs :: Reply -> [Aeson.Pair]
replyPairs = \case
    RSessionMeta meta -> ["session" .= meta]
    RSession sess meta -> ["turns" .= sess, "session" .= meta]
    RSessions metas -> ["sessions" .= metas]
    RMail envelopes -> ["mail" .= envelopes]
    RReceipt receipt -> ["receipt" .= receipt]
    RAgents agents -> ["agents" .= agents]
    RAgent agent -> ["agent" .= agent]
    RDeletion plan -> ["deletion" .= plan]
    RUnit -> []
    RStats stats -> ["stats" .= stats]
    RAwait meta active -> ["session" .= meta, "active" .= active]

instance Aeson.FromJSON Reply where
    parseJSON = Aeson.withObject "Reply" $ \o -> do
        tag <- o .: "reply"
        case (tag :: Text) of
            "session_meta" -> RSessionMeta <$> o .: "session"
            "session" -> RSession <$> o .: "turns" <*> o .: "session"
            "sessions" -> RSessions <$> o .: "sessions"
            "mail" -> RMail <$> o .: "mail"
            "receipt" -> RReceipt <$> o .: "receipt"
            "agents" -> RAgents <$> o .: "agents"
            "agent" -> RAgent <$> o .: "agent"
            "deletion" -> RDeletion <$> o .: "deletion"
            "unit" -> pure RUnit
            "stats" -> RStats <$> o .: "stats"
            "await" -> RAwait <$> o .: "session" <*> o .: "active"
            other -> fail ("unknown reply: " <> Text.unpack other)
