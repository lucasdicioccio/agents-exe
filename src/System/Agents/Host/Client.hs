{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | 'RunnerClient': one interface over a 'SessionRunner', for anything
that only wants to send 'Command's and get back 'Reply's -- the TUI
(Phase 3b), "System.Agents.Host.Client.Http"'s @httpClient@ over
"System.Agents.Protocol"'s wire types, and this module's own typed
helpers.

@todos/os-as-standalone-server.md@ Design §3 sketches 'RunnerClient' with
two implementations: 'inProcessClient' here (Phase 3a), and
"System.Agents.Host.Client.Http"'s @httpClient@ over HTTP or a Unix
socket (Phase 4). Nothing here depends on @wai@\/@warp@\/@http-client@:
'inProcessClient' only ever calls "System.Agents.Host.Runner" functions
directly.
-}
module System.Agents.Host.Client (
    -- * The client interface
    RunnerClient (..),
    Subscription (..),
    inProcessClient,

    -- * Typed helpers
    createSession,
    createSessionAsChild,
    spawnSession,
    postMessage,
    resumeSession,
    completeCall,
    cancelRun,
    cancelAttachedCalls,
    pauseSession,
    sendMail,
    listMail,
    forkSession,
    listSessions,
    getSession,
    listAgents,
    getAgent,
    deleteSession,
    awaitRun,
    stats,
    subscribeAll,
) where

import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (NominalDiffTime)

import System.Agents.Host.Runner (SessionRunner, ReplayUnavailable)
import qualified System.Agents.Host.Runner as Runner
import System.Agents.Protocol
import System.Agents.Session.Base (
    ContinuationToken,
    Envelope,
    MailBody,
    Priority,
    Receipt,
    Session,
    SessionId,
    UserToolResponse,
 )
import System.Agents.SessionStore (SessionMeta, SessionQuery)
import System.Agents.Tools.Params.Types (ParamName)

-------------------------------------------------------------------------------
-- The client interface
-------------------------------------------------------------------------------

{- | One interface over a runner, for a 'Command' in and a 'Reply' (or a
'RunnerError') out, plus a live event feed. "System.Agents.Protocol"
Design §3: the TUI (Phase 3b) depends only on this, never on
"System.Agents.Host.Runner" directly, so it does not care whether it runs
against a runner it started itself ('inProcessClient') or one attached
over HTTP\/a socket (Phase 4).
-}
data RunnerClient = RunnerClient
    { rcCommand :: Command -> IO (Either RunnerError Reply)
    , rcSubscribe :: SubscribeScope -> Maybe EventSeq -> IO (Either ReplayUnavailable Subscription)
    }

-- | A live event stream: block for the next matching 'Event', or stop watching.
data Subscription = Subscription
    { subNext :: IO Event
    , subClose :: IO ()
    }

{- | A 'RunnerClient' that dispatches every 'Command' straight to a
'SessionRunner' in this process. @owner@ is this client's own identity
(e.g. the TUI's local user, or 'Nothing' for an unauthenticated embedded
run): it is used for every operation that needs one ('CreateSession',
'SpawnSession', 'SendMail', 'ForkSession') since, unlike an HTTP request, a
'Command' does not carry a caller-asserted owner of its own to trust.

The @case@ below is total over every 'Command' constructor on purpose: a
constructor added to "System.Agents.Protocol" without a matching case here
is a compile error, not a silent 'UnexpectedReply' at runtime.
-}
inProcessClient :: Maybe Text -> SessionRunner -> RunnerClient
inProcessClient owner runner =
    RunnerClient
        { rcCommand = dispatch
        , rcSubscribe = \scope after -> fmap toSubscription <$> Runner.subscribe runner scope after
        }
  where
    toSubscription next = Subscription{subNext = next, subClose = pure ()}

    dispatch :: Command -> IO (Either RunnerError Reply)
    dispatch = \case
        CreateSession parent agent message mode params ->
            wrap RSessionMeta <$> Runner.createSessionAsWithParent runner parent owner agent message mode params
        SpawnSession parent agent message ->
            wrap RSessionMeta <$> Runner.spawnSession runner parent agent message
        PostMessage sid message mode params ->
            wrap RSessionMeta <$> Runner.postMessage runner sid message mode params
        Resume sid mode params ->
            wrap RSessionMeta <$> Runner.resume runner sid mode params
        CompleteCall token result autoResume params ->
            wrap RSessionMeta <$> Runner.completeCall runner token result autoResume params
        CancelRun sid ->
            wrap RSessionMeta <$> Runner.cancelRun runner sid
        CancelAttached sid ->
            wrap RSessionMeta <$> Runner.cancelAttachedCalls runner sid
        Pause sid ->
            wrap RSessionMeta <$> Runner.pauseSession runner sid
        SendMail sid priority body ->
            wrap RReceipt <$> Runner.sendMail runner sid owner priority body
        ListMail sid unreadOnly ->
            wrap RMail <$> Runner.listMail runner sid unreadOnly
        ForkSession sid atTurn newAgent ->
            wrap RSessionMeta <$> Runner.forkSession runner owner sid atTurn newAgent
        ListSessions query ->
            Right . RSessions <$> Runner.listSessions runner query
        GetSession sid ->
            maybe (Left (UnknownSession sid)) (\(sess, meta) -> Right (RSession sess meta)) <$> Runner.getSession runner sid
        ListAgents ->
            Right . RAgents <$> Runner.listAgents runner
        GetAgent slug ->
            maybe (Left (UnknownAgent slug)) (Right . RAgent) <$> Runner.getAgent runner slug
        DeleteSession sid mode ->
            wrap RDeletion <$> Runner.deleteSession runner sid mode
        AwaitRun sid limit ->
            wrap (uncurry RAwait) <$> Runner.awaitRun runner sid limit
        Stats ->
            Right . RStats <$> Runner.runnerStats runner

    wrap :: (a -> Reply) -> Either RunnerError a -> Either RunnerError Reply
    wrap f = fmap f

-------------------------------------------------------------------------------
-- Typed helpers
-------------------------------------------------------------------------------

{- | Unwrap the 'Reply' a helper expects, or fail with 'UnexpectedReply' --
never provoked by a correct 'RunnerClient' (see 'inProcessClient'\'s total
dispatch), only by a bug in one.
-}
expect :: (Reply -> Maybe a) -> Either RunnerError Reply -> Either RunnerError a
expect _ (Left err) = Left err
expect f (Right reply) = maybe (Left UnexpectedReply) Right (f reply)

asSessionMeta :: Reply -> Maybe SessionMeta
asSessionMeta (RSessionMeta meta) = Just meta
asSessionMeta _ = Nothing

-- | Create a root session (G2: no first message starts it idle).
createSession :: RunnerClient -> Text -> Maybe NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
createSession client agent message mode params =
    expect asSessionMeta <$> client.rcCommand (CreateSession Nothing agent message mode params)

-- | Create a session as a child (lineage only, G6) of another.
createSessionAsChild :: RunnerClient -> SessionId -> Text -> Maybe NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
createSessionAsChild client parent agent message mode params =
    expect asSessionMeta <$> client.rcCommand (CreateSession (Just parent) agent message mode params)

-- | @spawn-session@: a detached, durable child that answers by mail.
spawnSession :: RunnerClient -> SessionId -> Text -> NewMessage -> IO (Either RunnerError SessionMeta)
spawnSession client parent agent message =
    expect asSessionMeta <$> client.rcCommand (SpawnSession parent agent message)

-- | Post a message to a session (idle: a new turn; busy: mail, G2).
postMessage :: RunnerClient -> SessionId -> NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
postMessage client sid message mode params =
    expect asSessionMeta <$> client.rcCommand (PostMessage sid message mode params)

-- | Resume an idle or paused session.
resumeSession :: RunnerClient -> SessionId -> RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
resumeSession client sid mode params =
    expect asSessionMeta <$> client.rcCommand (Resume sid mode params)

-- | Give a deferred call its result.
completeCall :: RunnerClient -> ContinuationToken -> UserToolResponse -> Bool -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
completeCall client token result autoResume params =
    expect asSessionMeta <$> client.rcCommand (CompleteCall token result autoResume params)

-- | Stop a session's active run (hard cancel, tears down the run).
cancelRun :: RunnerClient -> SessionId -> IO (Either RunnerError SessionMeta)
cancelRun client sid = expect asSessionMeta <$> client.rcCommand (CancelRun sid)

-- | Cancel every tool call currently attached to a session, without stopping its run.
cancelAttachedCalls :: RunnerClient -> SessionId -> IO (Either RunnerError SessionMeta)
cancelAttachedCalls client sid = expect asSessionMeta <$> client.rcCommand (CancelAttached sid)

-- | Pause a session.
pauseSession :: RunnerClient -> SessionId -> IO (Either RunnerError SessionMeta)
pauseSession client sid = expect asSessionMeta <$> client.rcCommand (Pause sid)

-- | Post mail to a session (G6): 'System.Agents.Session.Base.UserMessage',
-- 'System.Agents.Session.Base.AgentMessage', or
-- 'System.Agents.Session.Base.Control' (e.g. 'System.Agents.Session.Base.StopRun' on quit).
sendMail :: RunnerClient -> SessionId -> Priority -> MailBody -> IO (Either RunnerError Receipt)
sendMail client sid priority body =
    expect asReceipt <$> client.rcCommand (SendMail sid priority body)
  where
    asReceipt (RReceipt r) = Just r
    asReceipt _ = Nothing

-- | A session's mail, oldest first ('True': unread only).
listMail :: RunnerClient -> SessionId -> Bool -> IO (Either RunnerError [Envelope])
listMail client sid unreadOnly =
    expect asMail <$> client.rcCommand (ListMail sid unreadOnly)
  where
    asMail (RMail es) = Just es
    asMail _ = Nothing

-- | Fork a session at a turn (G6); 'Nothing' new agent keeps the source's.
forkSession :: RunnerClient -> SessionId -> Maybe Int -> Maybe Text -> IO (Either RunnerError SessionMeta)
forkSession client sid atTurn newAgent =
    expect asSessionMeta <$> client.rcCommand (ForkSession sid atTurn newAgent)

-- | Matching sessions, most recently updated first.
listSessions :: RunnerClient -> SessionQuery -> IO (Either RunnerError [SessionMeta])
listSessions client query =
    expect asSessions <$> client.rcCommand (ListSessions query)
  where
    asSessions (RSessions ms) = Just ms
    asSessions _ = Nothing

-- | The latest stored version of a session.
getSession :: RunnerClient -> SessionId -> IO (Either RunnerError (Session, SessionMeta))
getSession client sid =
    expect asSession <$> client.rcCommand (GetSession sid)
  where
    asSession (RSession sess meta) = Just (sess, meta)
    asSession _ = Nothing

-- | Every root agent this host knows (G6, the Agents tab).
listAgents :: RunnerClient -> IO (Either RunnerError [AgentDescriptor])
listAgents client =
    expect asAgents <$> client.rcCommand ListAgents
  where
    asAgents (RAgents as) = Just as
    asAgents _ = Nothing

-- | One agent's descriptor, by slug.
getAgent :: RunnerClient -> Text -> IO (Either RunnerError AgentDescriptor)
getAgent client slug =
    expect asAgent <$> client.rcCommand (GetAgent slug)
  where
    asAgent (RAgent a) = Just a
    asAgent _ = Nothing

-- | Delete a session (and its descendants); 'DryRun' only plans it.
deleteSession :: RunnerClient -> SessionId -> DeleteMode -> IO (Either RunnerError DeletionPlan)
deleteSession client sid mode =
    expect asDeletion <$> client.rcCommand (DeleteSession sid mode)
  where
    asDeletion (RDeletion p) = Just p
    asDeletion _ = Nothing

-- | Wait until a session's active run stops, or the timeout expires.
awaitRun :: RunnerClient -> SessionId -> NominalDiffTime -> IO (Either RunnerError (SessionMeta, Bool))
awaitRun client sid limit =
    expect asAwait <$> client.rcCommand (AwaitRun sid limit)
  where
    asAwait (RAwait meta active) = Just (meta, active)
    asAwait _ = Nothing

-- | A runner's current load.
stats :: RunnerClient -> IO (Either RunnerError RunnerStats)
stats client =
    expect asStats <$> client.rcCommand Stats
  where
    asStats (RStats s) = Just s
    asStats _ = Nothing

-- | Subscribe to every session's events, server-wide, from now.
subscribeAll :: RunnerClient -> IO (Either ReplayUnavailable Subscription)
subscribeAll client = client.rcSubscribe AllSessions Nothing
