{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Run durable agent sessions inside a long-lived process.

A 'SessionRunner' owns, per session:

* a lock that serialises every change to the session;
* at most one run: a background thread stepping the session until it
  stops, is blocked on deferred calls, or (in 'StepOnce' mode) took a step;
* the session's agent, kept between runs so that background tool calls
  started by one run are picked up by the next.

External results for deferred calls (@completeCall@) are delivered as
'ContinuationResult' mail on the session's durable mailbox
(@todos/session-mailbox.md@, a Phase 3 follow-up) rather than a
runner-private queue; 'Session.Step.applyContinuationMail' is what a run
actually applies them with, at the same point it processes every other
kind of mail.

Every write goes through 'sbCompareAndStore', so a second process writing
the same session is detected rather than silently overwritten.
-}
module System.Agents.Host.Runner (
    -- * Runner
    SessionRunner,
    newSessionRunner,
    shutdownSessionRunner,
    withSessionRunner,
    RunnerStats (..),
    runnerStats,

    -- * Operations
    RunMode (..),
    NewMessage (..),
    RunnerError (..),
    RunnerConfig (..),
    defaultRunnerConfig,
    newSessionRunnerWith,
    createSession,
    createSessionAs,
    createSessionAsWithParent,
    spawnSession,
    sessionOwner,
    postMessage,
    cancelAttachedCalls,
    pauseSession,
    sendMail,
    sendControlMail,
    listMail,
    forkSession,
    resume,
    completeCall,
    cancelRun,
    serverMailRouter,
    serverSpawnSession,
    serverWatchSession,
    serverUnwatchSession,
    getSession,
    listSessions,
    listAgents,
    getAgent,
    awaitRun,
    recoverOnStartup,

    -- * Deletion
    DeleteMode (..),
    DeletionPlan (..),
    deleteSession,

    -- * Agent descriptors (G6)
    AgentDescriptor (..),
    ToolDescriptor (..),
    AgentParameter (..),

    -- * Events
    Event (..),
    EventBody (..),
    EventSeq (..),
    SubscribeScope (..),
    ReplayUnavailable (..),
    eventKind,
    subscribe,
    subscribeSTM,
    subscribeSession,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeAsyncException, SomeException, bracket, displayException, fromException, throwIO, try)
import Control.Monad (filterM, forM, forM_, forever, unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Foldable (for_, toList)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Prod.Tracer (contramap, runTracer)
import System.Timeout (timeout)

import System.Agents.AgentFactory (AgentDeps (..), AgentRole (..), buildAgent)
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.Base as Base
import System.Agents.AgentStore (StoredAgent (..))
import System.Agents.Host
import System.Agents.OS.Events (OSEmission (..))
import System.Agents.Protocol (
    AgentDescriptor (..),
    AgentParameter (..),
    DeleteMode (..),
    DeletionPlan (..),
    Event (..),
    EventBody (..),
    EventSeq (..),
    NewMessage (..),
    RunMode (..),
    RunnerError (..),
    RunnerStats (..),
    SubscribeScope (..),
    ToolDescriptor (..),
    eventKind,
    nextEventSeq,
 )
import System.Agents.Session.Async (ContinuationStore (..))
import System.Agents.Session.Async.Engine (shutdownAsyncEngine)
import System.Agents.Session.AgentConfig (matchGlob)
import System.Agents.Session.Base hiding (SessionProgress (..))
import System.Agents.Session.Step (
    applyContinuationMail,
    applyControlMail,
    buildContext,
    cancelAttachedCall,
    refreshHeadPartialTurn,
    runningToolCallIds,
    runStepM,
 )
import System.Agents.Session.Wake (WakeOutcome (..), findSessionForToken, wakeSessionWith)
import System.Agents.SessionStore (
    SessionMeta (..),
    SessionQuery (..),
    VersionConflict,
    allSessionsQuery,
    freshSessionMeta,
    sessionIdToConversationId,
 )
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ToolDescription (..), ToolName (..))
import System.Agents.Tools.Params.Types (
    ParamName,
    ParamScope (..),
    ParameterDecl (..),
    ParamValue (..),
    Params,
    ProcessValue (..),
 )

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

{- | 'RunMode', 'NewMessage', 'RunnerError', 'DeleteMode', 'DeletionPlan',
'Event', 'EventBody', 'EventSeq' and 'SubscribeScope' now live in
"System.Agents.Protocol", the one module owning the wire types and their
JSON. Re-exported here unchanged so nothing outside this module (and
"System.Agents.Protocol" itself) needs to know that split.
-}

-- | Tunables for a 'SessionRunner', beyond the 'Host' it runs.
newtype RunnerConfig = RunnerConfig
    { rcEventRingSize :: Int
    -- ^ How many past events 'subscribe'\/'subscribeSTM' can replay from,
    -- per server (not per session). Default 4096.
    }
    deriving (Show, Eq)

defaultRunnerConfig :: RunnerConfig
defaultRunnerConfig = RunnerConfig{rcEventRingSize = 4096}

-- | A subscription's @after@ is older than every event still in the ring:
-- the caller missed some, and should fall back to a fresh snapshot.
data ReplayUnavailable = ReplayUnavailable
    deriving (Show, Eq)

type RunnerAgent = Agent (LlmTurnContent, Session)

data SessionRunner = SessionRunner
    { srHost :: Host
    , srLive :: TVar (Map SessionId LiveSession)
    , srEvents :: TChan Event
    -- ^ Broadcast; subscribers filter by scope, so eviction loses nothing.
    , srReaper :: Async ()
    , srWatches :: TVar (Map Text WatchHandle)
    -- ^ Active @watch-session@ registrations (@todos/session-mailbox.md@ §7), keyed by watch id.
    , srSeq :: TVar EventSeq
    -- ^ The last 'EventSeq' stamped; monotonically increasing.
    , srRing :: TVar (Seq Event)
    -- ^ Bounded history of recent events (oldest first), for replay.
    , srRingSize :: Int
    }

-- | One active watch: who is watching, and the thread forwarding matches.
data WatchHandle = WatchHandle
    { whWatcher :: SessionId
    , whAsync :: Async ()
    }

-- | In-memory state of a session this process has touched.
data LiveSession = LiveSession
    { lsSessionId :: SessionId
    , lsLock :: MVar ()
    , lsRun :: TVar (Maybe (Async ()))
    , lsAgent :: TVar (Maybe RunnerAgent)
    , lsLatest :: TVar (Maybe (Session, SessionMeta))
    -- ^ The last version this process stored or loaded.
    , lsLastTouched :: TVar UTCTime
    , lsEvicted :: TVar Bool
    , lsParams :: TVar Params
    {- ^ Session-scope parameter values, secret and non-secret, kept only in
    this process's memory. Lost on restart or eviction; a client must
    resupply them, or the next run that needs one fails with
    'MissingRequiredParams' (@todos/tool-partial-application.md@, Phase 4).
    -}
    }

-- | A versioned write lost to a writer outside this runner.
newtype ConcurrentModification = ConcurrentModification VersionConflict
    deriving (Show)

instance Exception ConcurrentModification

-------------------------------------------------------------------------------
-- Lifecycle
-------------------------------------------------------------------------------

newSessionRunner :: Host -> IO SessionRunner
newSessionRunner = newSessionRunnerWith defaultRunnerConfig

-- | Like 'newSessionRunner', with the event ring size configurable.
newSessionRunnerWith :: RunnerConfig -> Host -> IO SessionRunner
newSessionRunnerWith config host = do
    live <- newTVarIO Map.empty
    events <- newBroadcastTChanIO
    watches <- newTVarIO Map.empty
    seqVar <- newTVarIO (EventSeq 0)
    ring <- newTVarIO Seq.empty
    let ttl = host.hostLiveSessionTtl
        ringSize = max 1 config.rcEventRingSize
    self <- newEmptyTMVarIO
    reaper <- async $ do
        runner <- atomically $ readTMVar self
        forever $ do
            threadDelay (reaperInterval ttl)
            evictIdle runner
    let runner = SessionRunner host live events reaper watches seqVar ring ringSize
    atomically $ putTMVar self runner
    pure runner

-- | How often idle sessions are looked for: half the idle time, within bounds.
reaperInterval :: NominalDiffTime -> Int
reaperInterval ttl = max 50_000 (min 60_000_000 (round (realToFrac ttl * 500_000 :: Double)))

-- | Cancel active runs (storing their sessions), and stop every engine.
shutdownSessionRunner :: SessionRunner -> IO ()
shutdownSessionRunner runner = do
    cancel runner.srReaper
    watches <- Map.elems <$> readTVarIO runner.srWatches
    forM_ watches $ \wh -> cancel wh.whAsync
    lives <- Map.elems <$> readTVarIO runner.srLive
    forM_ lives $ \live -> do
        active <- readTVarIO live.lsRun
        when (isJust active) $ void $ cancelRun runner live.lsSessionId
        mAgent <- readTVarIO live.lsAgent
        forM_ (mAgent >>= (.ctxAsyncEngine)) shutdownAsyncEngine

withSessionRunner :: Host -> (SessionRunner -> IO a) -> IO a
withSessionRunner host = bracket (newSessionRunner host) shutdownSessionRunner

runnerStats :: SessionRunner -> IO RunnerStats
runnerStats runner = do
    lives <- Map.elems <$> readTVarIO runner.srLive
    active <- filterM (fmap isJust . readTVarIO . (.lsRun)) lives
    pure $ RunnerStats (length lives) (length active)

-------------------------------------------------------------------------------
-- Live sessions
-------------------------------------------------------------------------------

getLive :: SessionRunner -> SessionId -> IO LiveSession
getLive runner sid = do
    now <- getCurrentTime
    fresh <- newLive now
    atomically $ do
        lives <- readTVar runner.srLive
        case Map.lookup sid lives of
            Just live -> pure live
            Nothing -> do
                writeTVar runner.srLive (Map.insert sid fresh lives)
                pure fresh
  where
    newLive now =
        LiveSession sid
            <$> newMVar ()
            <*> newTVarIO Nothing
            <*> newTVarIO Nothing
            <*> newTVarIO Nothing
            <*> newTVarIO now
            <*> newTVarIO False
            <*> newTVarIO Map.empty

lookupLive :: SessionRunner -> SessionId -> IO (Maybe LiveSession)
lookupLive runner sid = Map.lookup sid <$> readTVarIO runner.srLive

{- | Run an action on a session under its lock. Retries if the session was
evicted while waiting for the lock, so the action never works on a detached
'LiveSession'.
-}
withLive :: SessionRunner -> SessionId -> (LiveSession -> IO a) -> IO a
withLive runner sid action = do
    live <- getLive runner sid
    result <- withMVar live.lsLock $ \_ -> do
        evicted <- readTVarIO live.lsEvicted
        if evicted
            then pure Nothing
            else do
                now <- getCurrentTime
                atomically $ writeTVar live.lsLastTouched now
                Just <$> action live
    maybe (withLive runner sid action) pure result

-- | Forget a session's in-memory state (under its lock).
dropLive :: SessionRunner -> LiveSession -> IO ()
dropLive runner live = do
    mAgent <- readTVarIO live.lsAgent
    forM_ (mAgent >>= (.ctxAsyncEngine)) shutdownAsyncEngine
    atomically $ do
        writeTVar live.lsEvicted True
        modifyTVar' runner.srLive (Map.delete live.lsSessionId)

-- | Evict sessions idle for longer than the host's TTL.
evictIdle :: SessionRunner -> IO ()
evictIdle runner = do
    lives <- Map.elems <$> readTVarIO runner.srLive
    forM_ lives $ \live -> withMVar live.lsLock $ \_ -> do
        now <- getCurrentTime
        touched <- readTVarIO live.lsLastTouched
        active <- readTVarIO live.lsRun
        latest <- readTVarIO live.lsLatest
        let idle = diffUTCTime now touched > runner.srHost.hostLiveSessionTtl
            running = maybe False (hasRunningCalls . fst) latest
        evicted <- readTVarIO live.lsEvicted
        when (idle && not running && not (isJust active) && not evicted) $ dropLive runner live
  where
    hasRunningCalls :: Session -> Bool
    hasRunningCalls sess =
        or [tc.tcState == Running | PartialUserTurn partial _ <- sess.turns, tc <- partial.pTrackedToolCalls]

-------------------------------------------------------------------------------
-- Events and storage
-------------------------------------------------------------------------------

{- | Stamp and broadcast an event of one session: a fresh, strictly
increasing 'EventSeq', the owner looked up from that session's cached
'SessionMeta' (best effort; 'Nothing' if the session has no live, cached
meta -- e.g. a freshly deleted session, which 'emitOwned' covers instead),
appended to the ring and written to the broadcast channel in one
transaction (so a concurrent 'subscribeSTM' never sees a torn view: either
before this event, ring and channel both including it, or after).
-}
emit :: SessionRunner -> SessionId -> EventBody -> IO ()
emit runner sid body = do
    owner <- ownerOfLive runner sid
    emitOwned runner sid owner body

-- | Like 'emit', with the owner supplied by the caller rather than looked
-- up live (needed once a session's 'LiveSession' is already gone, e.g.
-- 'SessionDeleted').
emitOwned :: SessionRunner -> SessionId -> Maybe Text -> EventBody -> IO ()
emitOwned runner sid owner body = do
    atomically $ do
        n <- nextEventSeq <$> readTVar runner.srSeq
        writeTVar runner.srSeq n
        let ev = Event n (Just sid) owner body
        modifyTVar' runner.srRing (pushRing runner.srRingSize ev)
        writeTChan runner.srEvents ev
    case body of
        -- One per token: too many, and too revealing, for the logs.
        TextDelta{} -> pure ()
        _ -> runTracer runner.srHost.hostTracer $ HostRunnerTrace (eventKind body) sid

-- | A session's owner, from this process's cached copy of its 'SessionMeta'
-- (never hits the backend); 'Nothing' if this process has no live copy.
ownerOfLive :: SessionRunner -> SessionId -> IO (Maybe Text)
ownerOfLive runner sid = do
    mLive <- lookupLive runner sid
    case mLive of
        Nothing -> pure Nothing
        Just live -> do
            cached <- readTVarIO live.lsLatest
            pure (cached >>= (smOwner . snd))

-- | Append to a bounded ring, dropping the oldest entries past 'cap'.
pushRing :: Int -> Event -> Seq Event -> Seq Event
pushRing cap ev s =
    let s' = s Seq.|> ev
     in if Seq.length s' > cap then Seq.drop (Seq.length s' - cap) s' else s'

{- | Next events of one session. Each call to 'subscribe' gets its own
stream, starting now (or, with @after@, replayed from the ring first); the
returned action blocks until the next matching event. 'Left
ReplayUnavailable' when @after@ names an event older than everything the
ring still holds: the caller should fall back to a fresh snapshot and
subscribe again with @after = Nothing@.
-}
subscribe :: SessionRunner -> SubscribeScope -> Maybe EventSeq -> IO (Either ReplayUnavailable (IO Event))
subscribe runner scope after = do
    result <- subscribeSTM runner scope after
    pure $ case result of
        Left e -> Left e
        Right next ->
            -- One transaction per event: after a 'readTChan', 'retry' would
            -- roll the read back and block on the same event forever.
            Right (let loop = atomically next >>= maybe loop pure in loop)

{- | Like 'subscribe', to combine with others (timers, shutdown flags) as a
transaction. Each run consumes one event, and yields 'Nothing' for one that
does not match 'scope'.

The ring snapshot and the broadcast channel's duplicate are taken in one
STM transaction: any event committed by 'emit' either lands in both (and
this call's replay list sees it) or in neither (and the live stream, taken
over from exactly this point, sees it) -- never a gap, never a duplicate.
-}
subscribeSTM :: SessionRunner -> SubscribeScope -> Maybe EventSeq -> IO (Either ReplayUnavailable (STM (Maybe Event)))
subscribeSTM runner scope after = atomically $ do
    ring <- readTVar runner.srRing
    chan <- dupTChan runner.srEvents
    current <- readTVar runner.srSeq
    case after of
        Nothing -> pure $ Right (liveOnly chan)
        Just n
            | n > current ->
                -- A client carrying an id from a previous server process
                -- (or a bogus one): nothing this server ever stamped is
                -- past 'current', so replaying "from" it would silently
                -- skip straight to live events instead of reporting the
                -- gap. Answer unavailable, same as an id too old for the
                -- ring, so the caller falls back to a fresh snapshot.
                pure (Left ReplayUnavailable)
            | replayAvailable ring n -> do
                replayBuf <- newTVar (filter (matchesScope scope) (toList (Seq.filter ((> n) . evSeq) ring)))
                pure $ Right (replayThenLive replayBuf chan)
            | otherwise -> pure (Left ReplayUnavailable)
  where
    liveOnly chan = do
        ev <- readTChan chan
        pure $ if matchesScope scope ev then Just ev else Nothing
    replayThenLive replayBuf chan = do
        buf <- readTVar replayBuf
        case buf of
            (ev : rest) -> writeTVar replayBuf rest >> pure (Just ev)
            [] -> liveOnly chan

-- | Whether @after@ is recent enough that nothing between it and the
-- ring's oldest kept event was evicted (an empty ring never lost anything).
replayAvailable :: Seq Event -> EventSeq -> Bool
replayAvailable ring n = case Seq.viewl ring of
    Seq.EmptyL -> True
    ev Seq.:< _ -> n >= EventSeq (let EventSeq k = evSeq ev in k - 1)

matchesScope :: SubscribeScope -> Event -> Bool
matchesScope (OneSession sid) ev = ev.evSession == Just sid
matchesScope (Owner o) ev = ev.evOwner == o
matchesScope AllSessions _ = True

-- | Like 'subscribe' for one session, never replaying: always available.
subscribeSession :: SessionRunner -> SessionId -> IO (IO Event)
subscribeSession runner sid =
    subscribe runner (OneSession sid) Nothing >>= \case
        Right next -> pure next
        Left ReplayUnavailable -> error "subscribeSession: unreachable, after = Nothing is always available"

-- | Store a new version of a session (call under its lock).
store :: SessionRunner -> LiveSession -> SessionMeta -> Session -> SessionStatus -> Maybe Text -> IO (Either VersionConflict SessionMeta)
store runner live meta sess status detail = do
    nonSecretParams <- nonSecretSessionParams live
    result <-
        runner.srHost.hostBackend.sbCompareAndStore
            meta{smStatus = status, smStatusDetail = detail, smParams = nonSecretParams}
            sess
    for_ result $ \meta' -> do
        atomically $ writeTVar live.lsLatest (Just (sess, meta'))
        emit runner live.lsSessionId $ SessionUpdated meta' (headTurn sess)
    pure result

-- | The session's session-scope parameters that may be persisted (never secret ones).
nonSecretSessionParams :: LiveSession -> IO (Map ParamName Aeson.Value)
nonSecretSessionParams live = do
    ps <- readTVarIO live.lsParams
    pure $ Map.map (.pvValue) (Map.filter (not . (.pvSecret)) ps)

-- | Like 'store', throwing 'ConcurrentModification' on a conflict.
storeOrThrow :: SessionRunner -> LiveSession -> SessionMeta -> Session -> SessionStatus -> IO SessionMeta
storeOrThrow runner live meta sess status =
    store runner live meta sess status Nothing >>= either (throwIO . ConcurrentModification) pure

headTurn :: Session -> Maybe Turn
headTurn sess = case sess.turns of
    (turn : _) -> Just turn
    [] -> Nothing

-- | The latest stored version of a session (call under its lock).
loadLatest :: SessionRunner -> LiveSession -> IO (Maybe (Session, SessionMeta))
loadLatest runner live = do
    loaded <- runner.srHost.hostBackend.sbLoadMeta live.lsSessionId
    for_ loaded $ atomically . writeTVar live.lsLatest . Just
    pure loaded

-------------------------------------------------------------------------------
-- Agents
-------------------------------------------------------------------------------

-- | The session's agent: the kept one, or a new one for the session's agent slug.
sessionAgent :: SessionRunner -> LiveSession -> SessionMeta -> IO (Either RunnerError RunnerAgent)
sessionAgent runner live meta =
    readTVarIO live.lsAgent >>= \case
        Just agent -> pure (Right agent)
        Nothing -> case meta.smAgent of
            Nothing -> pure $ Left $ UnknownAgent ""
            Just slug ->
                lookupAgent runner.srHost slug >>= \case
                    Nothing -> pure $ Left $ UnknownAgent slug
                    Just node -> do
                        agent <- newAgent runner live node
                        atomically $ writeTVar live.lsAgent (Just agent)
                        pure $ Right agent

{- | A root agent for a session, always asynchronous: runs can pause and
resume. Its tool list re-reads 'LiveSession.lsParams' fresh on every turn
(via 'adLiveParams'), so an @whenUnbound: "expose"@ argument (§7) hides or
reappears as the session's own parameters are set, without ever rebuilding
the agent.
-}
newAgent :: SessionRunner -> LiveSession -> OSAgentNode -> IO RunnerAgent
newAgent runner live node = do
    let host = runner.srHost
        sid = live.lsSessionId
    let deps0
            | host.hostStreamTokens = host.hostDeps{adOnTextDelta = Just (emit runner sid . TextDelta)}
            | otherwise = host.hostDeps
        deps = deps0{adLiveParams = readTVarIO live.lsParams}
    agent <- buildAgent (contramap HostAgentTrace host.hostTracer) deps RootAgent (sessionIdToConversationId sid) node
    -- Phase 3 (@todos/session-mailbox.md@): every server-run session has a
    -- durable mailbox, hydrated from 'hostMail', so 'postMessage' can always
    -- accept mail (G2) and R1/R2 fold it into the session's turns.
    mailbox <- newDurableMailbox host.hostMail sid
    pure $
        withEmit (emit runner sid . toEventBody) $
            withWatchSession (serverWatchSession runner sid) (serverUnwatchSession runner) $
                withSpawnSession (serverSpawnSession runner sid) $
                    withMailRouter (serverMailRouter runner) $
                        withMailbox mailbox $
                            withExecutionMode Asynchronous agent

{- | Turn a Phase 2c 'OSEmission' (subcall lifecycle, tool-call activity)
into the matching 'EventBody', for 'newAgent''s 'ctxEmit' hook. See
'OSEmission' for why this conversion lives here rather than in
"System.Agents.OS.Events" or "System.Agents.Protocol" directly: it is the
one module that already depends on both.
-}
toEventBody :: OSEmission -> EventBody
toEventBody = \case
    EmitSubcallStarted parent child slug depth -> SubcallStarted parent child slug depth
    EmitSubcallCompleted child result -> SubcallCompleted child result
    EmitSubcallFailed child msg -> SubcallFailed child msg
    EmitToolCallActivity activity -> ToolCallProgressed activity

{- | The server's @spawn-session@ hook (§5): reuses 'spawnSession'
(durable, recorded with 'sid' as parent) and reports only the new
'SessionId' as text, or an error, to the calling LLM.
-}
serverSpawnSession :: SessionRunner -> SessionId -> Text -> Text -> IO (Either Text SessionId)
serverSpawnSession runner sid slug text = do
    result <- spawnSession runner sid slug (NewMessage text [] False)
    pure $ either (Left . runnerErrorText) (Right . (.smSessionId)) result

-- | Render a 'RunnerError' as text, for hooks that report to the LLM rather than the HTTP API.
runnerErrorText :: RunnerError -> Text
runnerErrorText = Text.pack . show

{- | The server's 'MailRouter' (@todos/session-mailbox.md@, Phase 4, §5,
D12). Every session on the server is durable, so unlike the TUI or @run@
there is nothing to register: any session id can be looked up on demand by
loading its 'SessionMeta' and opening its durable mailbox, per the spec's
own routing table ("durable mail; unknown-but-stored sessions are loaded
on demand").
-}
serverMailRouter :: SessionRunner -> MailRouter
serverMailRouter runner =
    MailRouter
        { mrRegister = \_sid _info _mb -> pure (pure ())
        , mrLookup = \sid -> do
            mLive <- lookupLiveMailbox sid
            case mLive of
                Just found -> pure (Just found)
                Nothing ->
                    sbLoadMeta runner.srHost.hostBackend sid >>= \case
                        Nothing -> pure Nothing
                        Just (_sess, meta) -> do
                            mb <- newDurableMailbox runner.srHost.hostMail sid
                            info <- mailboxInfoFor meta
                            pure $ Just (info, mb)
        , mrList = do
            metas <- sbQuery runner.srHost.hostBackend allSessionsQuery
            mapM (\meta -> (,) meta.smSessionId <$> mailboxInfoFor meta) metas
        }
  where
    {- | If this session already has a live agent in this process, its
    'ctxMailbox' is the one whose in-memory 'TVar' any active run actually
    reads. Opening a *second* 'newDurableMailbox' for the same session
    (what this function used to do unconditionally) hydrates its own,
    separate 'TVar' from the store, so mail sent through it would sit in
    the database until the session's next run re-hydrates -- invisible to
    the run reading mail right now. Fall back to a fresh durable mailbox
    only for a session with no live agent (idle, evicted, or on another
    process, where the store is the only shared state).
    -}
    lookupLiveMailbox :: SessionId -> IO (Maybe (MailboxInfo, Mailbox))
    lookupLiveMailbox sid = do
        mLive <- lookupLive runner sid
        case mLive of
            Nothing -> pure Nothing
            Just live -> do
                mAgent <- readTVarIO live.lsAgent
                mLatest <- readTVarIO live.lsLatest
                case (mAgent, mLatest) of
                    (Just agent, Just (_, meta)) -> case agent.ctxMailbox of
                        Just mb -> do
                            info <- mailboxInfoFor meta
                            pure $ Just (info, mb)
                        Nothing -> pure Nothing
                    _ -> pure Nothing

    mailboxInfoFor :: SessionMeta -> IO MailboxInfo
    mailboxInfoFor meta = do
        (scope, iscope) <- agentMailScopes meta
        pure
            MailboxInfo
                { miAgentSlug = meta.smAgent
                , miParent = meta.smParent
                , miStatus = sessionStatusText meta.smStatus
                , miMailScope = scope
                , miInterruptScope = iscope
                }

    {- | A session's own @mailScope@\/@interruptScope@ (§5 Permissions),
    read from its agent's JSON config, defaulting to the spec's own
    'MailScopeSubtree'\/'MailScopeChildren' when unset or the agent is
    unknown.
    -}
    agentMailScopes :: SessionMeta -> IO (MailScope, MailScope)
    agentMailScopes meta = do
        mNode <- maybe (pure Nothing) (lookupAgent runner.srHost) meta.smAgent
        let mCfg = osNodeConfig <$> mNode
        pure
            ( fromMaybe MailScopeSubtree (mCfg >>= Base.mailScope)
            , fromMaybe MailScopeChildren (mCfg >>= Base.interruptScope)
            )

-------------------------------------------------------------------------------
-- Watches (todos/session-mailbox.md, Phase 6, §7)
-------------------------------------------------------------------------------

-- | Hard cap on concurrently active watches per watching session.
maxWatchesPerSession :: Int
maxWatchesPerSession = 32

-- | Default TTL for a watch that did not specify one.
defaultWatchTtlSeconds :: Int
defaultWatchTtlSeconds = 600

{- | The server's @watch-session@ hook (§7): scoped like mail (the watcher
must be the target or one of its descendants, mirroring @send-message@'s
own @mailScope: subtree@ default), capped per watcher, and forwarding
matching events as 'WatchedEvent' mail until its TTL elapses or
'serverUnwatchSession' is called.
-}
serverWatchSession :: SessionRunner -> SessionId -> WatchRequest -> IO (Either Text Text)
serverWatchSession runner watcherSid req = do
    mOwnInfo <- fmap fst <$> (serverMailRouter runner).mrLookup watcherSid
    let ownScope = maybe MailScopeSubtree miMailScope mOwnInfo
    withinSubtree <- isWithinScope runner ownScope watcherSid req.wrTarget
    if not withinSubtree
        then pure $ Left "not permitted to watch this session"
        else do
            activeForWatcher <- length . filter ((== watcherSid) . whWatcher) . Map.elems <$> readTVarIO runner.srWatches
            if activeForWatcher >= maxWatchesPerSession
                then pure $ Left "too many active watches for this session"
                else do
                    watchId <- Text.pack . show <$> newContinuationToken
                    next <- subscribeSession runner req.wrTarget
                    deadline <- addUTCTime (fromIntegral (fromMaybe defaultWatchTtlSeconds req.wrTtlSeconds)) <$> getCurrentTime
                    handle <- async $ forwardLoop watchId next deadline
                    atomically $ modifyTVar' runner.srWatches (Map.insert watchId (WatchHandle watcherSid handle))
                    pure $ Right watchId
  where
    forwardLoop :: Text -> IO Event -> UTCTime -> IO ()
    forwardLoop watchId next deadline = do
        now <- getCurrentTime
        let remainingMicros = round (max 0 (diffUTCTime deadline now)) * 1_000_000
        if remainingMicros <= 0
            then dropWatch watchId
            else do
                result <- timeout remainingMicros next
                case result of
                    Nothing -> dropWatch watchId
                    Just event -> do
                        when (eventMatches req event) $ forwardEvent watcherSid req.wrTarget event
                        forwardLoop watchId next deadline

    dropWatch :: Text -> IO ()
    dropWatch watchId = atomically $ modifyTVar' runner.srWatches (Map.delete watchId)

    forwardEvent :: SessionId -> SessionId -> Event -> IO ()
    forwardEvent watcher target event = do
        mTarget <- (serverMailRouter runner).mrLookup watcher
        forM_ mTarget $ \(_, mb) ->
            void $
                mb.mbSend
                    Outgoing
                        { outId = Nothing
                        , outFrom = FromSystem "watch-session"
                        , outPriority = Normal
                        , outHops = 0
                        , outBody = WatchedEvent target (eventKind event.evBody) (watchedEventPayload event.evBody)
                        }

-- | Stop a previously registered watch (§7).
serverUnwatchSession :: SessionRunner -> Text -> IO Bool
serverUnwatchSession runner watchId = do
    mHandle <- atomically $ do
        table <- readTVar runner.srWatches
        writeTVar runner.srWatches (Map.delete watchId table)
        pure (Map.lookup watchId table)
    case mHandle of
        Nothing -> pure False
        Just wh -> cancel wh.whAsync >> pure True

{- | Whether an 'Event' matches a watch request's @events@\/@tool@ filter.
'Nothing' for @wrEvents@ matches every kind; @wrTool@ only applies to the
two @tool.*@ events (per §7, other events always match it).
-}
eventMatches :: WatchRequest -> Event -> Bool
eventMatches req event = kindMatches && toolMatches
  where
    kindMatches = maybe True (eventKind event.evBody `elem`) req.wrEvents
    toolMatches = case (req.wrTool, event.evBody) of
        (Nothing, _) -> True
        (Just pat, ToolCallStarted _ toolName) -> matchGlob pat toolName
        (Just pat, ToolCallCompleted _ toolName _) -> matchGlob pat toolName
        (Just _, _) -> True

-- | A small JSON view of an 'EventBody', for 'WatchedEvent' mail.
watchedEventPayload :: EventBody -> Aeson.Value
watchedEventPayload event = case event of
    RunStarted mode -> Aeson.object ["mode" .= Text.pack (show mode)]
    SessionUpdated meta _turn -> Aeson.object ["session_id" .= meta.smSessionId, "status" .= sessionStatusText meta.smStatus]
    CallsDeferred calls -> Aeson.object ["deferred_count" .= length calls]
    RunStopped status -> Aeson.object ["status" .= sessionStatusText status]
    SessionFailed msg -> Aeson.object ["message" .= msg]
    TextDelta _ -> Aeson.object []
    ToolCallStarted callId toolName -> Aeson.object ["tool_call_id" .= callId, "tool" .= toolName]
    ToolCallCompleted callId toolName succeeded ->
        Aeson.object ["tool_call_id" .= callId, "tool" .= toolName, "succeeded" .= succeeded]
    SubcallStarted parent child slug depth ->
        Aeson.object ["parent_session_id" .= parent, "child_session_id" .= child, "agent" .= slug, "depth" .= depth]
    SubcallCompleted child result -> Aeson.object ["child_session_id" .= child, "result" .= result]
    SubcallFailed child msg -> Aeson.object ["child_session_id" .= child, "message" .= msg]
    ToolCallProgressed activity -> Aeson.toJSON activity
    SessionCreated meta -> Aeson.object ["session_id" .= meta.smSessionId]
    SessionDeleted sid -> Aeson.object ["session_id" .= sid]

{- | Whether 'targetSid' is reachable from 'ownSid' under the given
'MailScope', walking each session's recorded parent up from the target for
'MailScopeSubtree' (bounded depth so a corrupt or cyclic parent chain
cannot hang). Mirrors
"System.Agents.Tools.SystemToolbox.Mail"'s @isWithinScope@, which is not
reusable here without threading a 'MailRouter' value through -- both read
the same 'MailboxInfo.miParent' shape via 'mrLookup'.
-}
isWithinScope :: SessionRunner -> MailScope -> SessionId -> SessionId -> IO Bool
isWithinScope _runner MailScopeAll _ownSid _targetSid = pure True
isWithinScope _runner _scope ownSid targetSid
    | ownSid == targetSid = pure True
isWithinScope _runner MailScopeOwn _ownSid _targetSid = pure False
isWithinScope runner MailScopeChildren ownSid targetSid = do
    mTarget <- (serverMailRouter runner).mrLookup targetSid
    pure $ maybe False ((== Just ownSid) . miParent . fst) mTarget
isWithinScope runner MailScopeSubtree ownSid targetSid = go targetSid (32 :: Int)
  where
    router = serverMailRouter runner
    go _ 0 = pure False
    go sid depthLeft = do
        mInfo <- router.mrLookup sid
        case mInfo of
            Nothing -> pure False
            Just (info, _) -> case info.miParent of
                Nothing -> pure False
                Just parentSid
                    | parentSid == ownSid -> pure True
                    | otherwise -> go parentSid (depthLeft - 1)

-------------------------------------------------------------------------------
-- Session-level parameters (todos/tool-partial-application.md, Phase 4)
-------------------------------------------------------------------------------

-- | Overlay parameter values on top of an agent's process-level 'ctxParams'.
withParams :: Params -> RunnerAgent -> RunnerAgent
withParams overlay agent = agent{ctxParams = Map.union overlay agent.ctxParams}

-- | The loaded node for a session's agent, by its recorded slug.
agentNodeFor :: SessionRunner -> SessionMeta -> IO (Either RunnerError OSAgentNode)
agentNodeFor runner meta = case meta.smAgent of
    Nothing -> pure $ Left $ UnknownAgent ""
    Just slug -> maybe (Left (UnknownAgent slug)) Right <$> lookupAgent runner.srHost slug

-- | The parameters an agent declares.
nodeParameterDecls :: OSAgentNode -> [ParameterDecl]
nodeParameterDecls node = fromMaybe [] (Base.parameters (osNodeConfig node))

{- | Validate caller-supplied parameter values against an agent's
declarations, splitting them by scope. A @null@ value removes a session-scope
value rather than setting one.
-}
resolveSuppliedParams ::
    Set.Set ParamName ->
    [ParameterDecl] ->
    Map ParamName Aeson.Value ->
    Either RunnerError (Map ParamName ParamValue, Map ParamName ParamValue, [ParamName])
resolveSuppliedParams forbidden decls supplied
    | not (null unknown) = Left $ UnknownParams unknown
    | not (null forbiddenNames) = Left $ ForbiddenParams forbiddenNames
    | not (null invalid) = Left $ InvalidParams invalid
    | otherwise = Right (sessionPs, messagePs, removals)
  where
    byName = Map.fromList [(d.paramName, d) | d <- decls]
    supplied' = Map.toList supplied
    unknown = [n | (n, _) <- supplied', not (Map.member n byName)]
    forbiddenNames = [n | (n, _) <- supplied', n `Set.member` forbidden]
    isString (Aeson.String _) = True
    isString _ = False
    invalid =
        [ n
        | (n, v) <- supplied'
        , v /= Aeson.Null
        , maybe False (.paramSecret) (Map.lookup n byName)
        , not (isString v)
        ]
    removals = [n | (n, Aeson.Null) <- supplied', maybe False ((== ScopeSession) . (.paramScope)) (Map.lookup n byName)]
    toValue :: Text -> Aeson.Value -> ParamValue
    toValue n v = ParamValue v (maybe False (.paramSecret) (Map.lookup n byName))
    withScope s =
        Map.fromList
            [ (n, toValue n v)
            | (n, v) <- supplied'
            , v /= Aeson.Null
            , maybe False ((== s) . (.paramScope)) (Map.lookup n byName)
            ]
    sessionPs = withScope ScopeSession
    messagePs = withScope ScopeMessage

{- | Validate the caller-supplied parameters of one request, update the
session's persisted session-scope values, and return the overlay to apply
for the run this request may start (session-scope values plus this
request's message-scope ones).
-}
prepareParams :: SessionRunner -> LiveSession -> OSAgentNode -> Map ParamName Aeson.Value -> IO (Either RunnerError Params)
prepareParams runner live node supplied = do
    let decls = nodeParameterDecls node
        pinned = Set.fromList [n | (n, pv) <- Map.toList runner.srHost.hostProcessParams, pv.pvPinned]
        forbidden = Set.fromList [d.paramName | d <- decls, d.paramScope == ScopeProcess] <> pinned
    case resolveSuppliedParams forbidden decls supplied of
        Left err -> pure (Left err)
        Right (sessionPs, messagePs, removals) -> do
            sessionSnapshot <- atomically $ do
                modifyTVar' live.lsParams (\m -> Map.union sessionPs (foldr Map.delete m removals))
                readTVar live.lsParams
            pure $ Right (Map.union messagePs sessionSnapshot)

-- | Required parameters still unbound once 'prepareParams' overlay is merged in.
missingRequiredParams :: OSAgentNode -> RunnerAgent -> Params -> [ParamName]
missingRequiredParams node agent overlay =
    [d.paramName | d <- decls, d.paramRequired, not (Map.member d.paramName resolved)]
  where
    decls = nodeParameterDecls node
    resolved = Map.union overlay agent.ctxParams :: Params

-------------------------------------------------------------------------------
-- Runs
-------------------------------------------------------------------------------

-- | Start a run from the given version (call under the session's lock).
startRun :: SessionRunner -> LiveSession -> RunMode -> Params -> Session -> SessionMeta -> IO (Either RunnerError SessionMeta)
startRun runner live mode overlay sess meta =
    sessionAgent runner live meta >>= \case
        Left err -> pure (Left err)
        Right agent0 -> do
            let agent = withParams overlay agent0
            store runner live meta sess StatusRunning Nothing >>= \case
                Left conflict -> pure (Left (Conflict conflict))
                Right meta' -> do
                    emit runner live.lsSessionId $ RunStarted mode
                    -- The run's first step waits for this lock, so lsRun is set
                    -- before the run can finish and clear it.
                    handle <- async $ runLoop runner live mode agent
                    atomically $ writeTVar live.lsRun (Just handle)
                    pure (Right meta')

{- | Step a session until it should stop.

Each iteration, under the session's lock, applies queued external results
and decides whether to stop; the step itself runs without the lock.
-}
runLoop :: SessionRunner -> LiveSession -> RunMode -> RunnerAgent -> IO ()
runLoop runner live mode agent0 = do
    outcome <- try (go agent0 0 False)
    case outcome of
        Right () -> pure ()
        Left (e :: SomeException)
            | isJust (fromException e :: Maybe SomeAsyncException) -> throwIO e
            | otherwise -> failRun runner live (Text.pack (displayException e))
  where
    sid = live.lsSessionId
    convId = sessionIdToConversationId sid

    go :: RunnerAgent -> Int -> Bool -> IO ()
    go agent steps finished = do
        next <- withMVar live.lsLock $ \_ -> do
            (sess0, meta) <- applyInbox runner agent live
            -- Phase 3/6 (@todos/session-mailbox.md@ §4): unread 'Control'
            -- mail this run reacts to without waiting for the next receive
            -- point. When every envelope currently unread is a 'Control'
            -- one, the cursor is committed past them here ("consumed...
            -- but render nothing", §2) so a 'Pause'/'StopRun' that stops
            -- the run before it ever steps doesn't leave that same
            -- envelope to immediately re-trigger on the next run. A batch
            -- that mixes in other mail is left alone: the next real R1
            -- (once this run, or a later one, actually steps) is what
            -- renders it, and advancing the cursor here would silently
            -- drop it unread.
            (sess, controls) <- applyControlMail agent sess0
            forM_ [ids | CancelCalls ids <- controls] $ mapM_ (cancelAttachedCall agent)
            when (CancelAllAttached `elem` controls) $ mapM_ (cancelAttachedCall agent) (runningToolCallIds sess)
            let stopRequested = StopRun `elem` controls
                pauseRequested = Pause `elem` controls
                stop =
                    finished
                        || stopRequested
                        || pauseRequested
                        || (mode == StepOnce && steps >= 1)
                        || isBlockedOnDeferredCalls sess
            if stop
                then do
                    when pauseRequested $ do
                        cancelsCalls <- agentBoolOption runner meta Base.pauseCancelsCalls
                        when cancelsCalls $ mapM_ (cancelAttachedCall agent) (runningToolCallIds sess)
                    let status
                            | pauseRequested = StatusPaused
                            | otherwise = sessionStatusOf sess
                    _ <- storeOrThrow runner live meta sess status
                    atomically $ writeTVar live.lsRun Nothing
                    when (status == StatusWaitingExternal) $
                        emit runner sid $
                            CallsDeferred (pendingDeferredCalls sess)
                    emit runner sid $ RunStopped status
                    pure Nothing
                else pure (Just sess)
        for_ next $ \sess -> do
            (agent', result) <- runStepM convId agent sess
            atomically $ writeTVar live.lsAgent (Just agent')
            let (sess', done) = case result of
                    Left (_, final) -> (final, True)
                    Right s -> (s, False)
            emitToolCallEvents runner sid sess sess'
            withMVar live.lsLock $ \_ -> do
                (_, meta) <- latestOrThrow live
                void $ storeOrThrow runner live meta sess' StatusRunning
            go agent' (steps + 1) done

-- | Whether an agent's JSON config enables a given boolean option, looked
-- up by the session's recorded agent slug.
agentBoolOption :: SessionRunner -> SessionMeta -> (Base.Agent -> Maybe Bool) -> IO Bool
agentBoolOption runner meta field = do
    mNode <- maybe (pure Nothing) (lookupAgent runner.srHost) meta.smAgent
    pure $ fromMaybe False (mNode >>= field . osNodeConfig)

{- | Whether the given 'WakeOnKind' is in an agent's configured @wakeOn@
list (§5 "Scheduling rule"), defaulting to 'defaultWakeOn' when unset.
-}
agentWakesOn :: SessionRunner -> SessionMeta -> WakeOnKind -> IO Bool
agentWakesOn runner meta kind = do
    mNode <- maybe (pure Nothing) (lookupAgent runner.srHost) meta.smAgent
    let kinds = fromMaybe defaultWakeOn (mNode >>= Base.wakeOn . osNodeConfig)
    pure (kind `elem` kinds)

{- | Every tracked call visible in the head turn, if it is a
'PartialUserTurn' -- the only turn shape that still carries per-call state
(a finalized 'UserTurn' has already folded each call into a plain
@userToolResponses@ pair, with no 'ToolCallState' left to diff against).
-}
headTrackedCalls :: Session -> Map.Map ToolCallId (Text, ToolCallState)
headTrackedCalls sess = case take 1 sess.turns of
    [PartialUserTurn partial _] ->
        Map.fromList [(tc.tcId, (llmToolCallName tc.tcCall, tc.tcState)) | tc <- partial.pTrackedToolCalls]
    _ -> Map.empty

{- | Emit 'ToolCallStarted' \/ 'ToolCallCompleted' for whatever changed
between the turn the step started with and the turn it produced (§7).

Only observes calls while the head turn stays a 'PartialUserTurn': a call
that starts and reaches a final state within the same step, ending in a
finalized 'UserTurn', is not caught here (its 'ToolCallState' is gone by
the time this compares 'before'\/'after') -- an accepted gap, since such a
call was never visible to anything waiting on 'tool.started' either. A call
still 'Running' before this step whose turn is fully finalized after it is
reported 'ToolCallCompleted' with @succeeded = True@: the exact
success\/failure only survives in @userToolResponses@, keyed by
'LlmToolCall' rather than 'ToolCallId', which is not worth walking for an
event that is informational rather than authoritative (the stored session
and 'get-tool-call-status' remain the source of truth).
-}
emitToolCallEvents :: SessionRunner -> SessionId -> Session -> Session -> IO ()
emitToolCallEvents runner sid before after = do
    forM_ (Map.toList afterCalls) $ \(callId, (toolName, state)) ->
        when (state == Running && not (wasRunning callId)) $
            emit runner sid (ToolCallStarted callId toolName)
    forM_ (Map.toList beforeCalls) $ \(callId, (toolName, state)) ->
        when (state == Running) $ case Map.lookup callId afterCalls of
            Just (_, Completed) -> emit runner sid (ToolCallCompleted callId toolName True)
            Just (_, Failed) -> emit runner sid (ToolCallCompleted callId toolName False)
            Just (_, Running) -> pure ()
            Just (_, Ready) -> pure ()
            Just (_, Deferred) -> pure ()
            Nothing -> emit runner sid (ToolCallCompleted callId toolName True)
  where
    beforeCalls = headTrackedCalls before
    afterCalls = headTrackedCalls after
    wasRunning callId = case Map.lookup callId beforeCalls of
        Just (_, Running) -> True
        _ -> False

{- | Apply unread 'ContinuationResult' mail to the latest version, before this
iteration's blocked-on-deferred-calls check (under the lock).

@completeCall@ posts a deferred call's external result as mail rather than
writing it to a runner-private queue (a Phase 3 follow-up to
@todos/session-mailbox.md@: "'autoResume' becomes 'post
`ContinuationResult`'", 'lsInbox' is gone). This still has to run here,
ahead of 'runStepM', rather than only relying on the ordinary R1 receive
point inside the step: a session blocked on deferred calls never reaches R1
at all if 'isBlockedOnDeferredCalls' below still sees it as blocked, so the
result would sit applied-but-invisible in the mailbox forever. Mirrors
'applyControlMail': the cursor only advances when the whole unread batch is
'ContinuationResult' mail, so mail mixed in with something else is left for
the step's own R1 to render\/consume\/advance past as usual.
-}
applyInbox :: SessionRunner -> RunnerAgent -> LiveSession -> IO (Session, SessionMeta)
applyInbox runner agent live = do
    (sess, meta) <- latestOrThrow live
    case agent.ctxMailbox of
        Nothing -> pure (sess, meta)
        Just mb -> do
            envelopes <- atomically (mbUnread mb sess.mailCursor)
            let results = [e | e <- envelopes, ContinuationResult{} <- [e.envBody]]
            if null results
                then pure (sess, meta)
                else do
                    woken <- applyContinuationMail agent results sess
                    let allResults = length results == length envelopes
                        woken' = if allResults then woken{mailCursor = maximum (map (.envSeq) results)} else woken
                    meta' <- storeOrThrow runner live meta woken' StatusRunning
                    pure (woken', meta')

latestOrThrow :: LiveSession -> IO (Session, SessionMeta)
latestOrThrow live =
    readTVarIO live.lsLatest >>= maybe (throwIO $ userError "runner: session state missing") pure

{- | The mailbox to post to for a given session: the live agent's own
'ctxMailbox' when this process is actively running or holding it (so a
post lands in the same in-memory 'TVar' an active run reads, not a second,
separately-hydrated durable mailbox -- the same live-vs-durable distinction
'serverMailRouter' has to get right), otherwise a fresh durable mailbox
opened directly from the store, or 'Nothing' for a session this host
doesn't know about at all.
-}
sessionMailbox :: SessionRunner -> SessionId -> IO (Maybe Mailbox)
sessionMailbox runner sid = do
    mLive <- lookupLive runner sid
    mAgentMailbox <- case mLive of
        Nothing -> pure Nothing
        Just live -> do
            mAgent <- readTVarIO live.lsAgent
            pure (mAgent >>= \a -> a.ctxMailbox)
    case mAgentMailbox of
        Just mb -> pure (Just mb)
        Nothing ->
            runner.srHost.hostBackend.sbLoadMeta sid >>= \case
                Nothing -> pure Nothing
                Just _ -> Just <$> newDurableMailbox runner.srHost.hostMail sid

-- | Record a failed run: the last stored version, marked failed.
failRun :: SessionRunner -> LiveSession -> Text -> IO ()
failRun runner live reason = withMVar live.lsLock $ \_ -> do
    loaded <- loadLatest runner live
    for_ loaded $ \(sess, meta) ->
        void $ store runner live meta sess StatusFailed (Just reason)
    atomically $ writeTVar live.lsRun Nothing
    emit runner live.lsSessionId $ SessionFailed reason
    emit runner live.lsSessionId $ RunStopped StatusFailed

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Create a session for an agent with a first message, and start a run
-- unless the mode is 'Nothing'. A convenience over 'createSessionAs' for
-- the common case; see it (and 'createSessionAsWithParent') for a session
-- with no first message (G2).
createSession :: SessionRunner -> Text -> NewMessage -> Maybe RunMode -> IO (Either RunnerError SessionMeta)
createSession runner slug message mode = createSessionAs runner Nothing slug (Just message) mode Map.empty

{- | Like 'createSession', for an owner, with caller-supplied parameter
values. 'Nothing' for the message (G2, @todos/os-as-standalone-server.md@
Design §1) creates an idle session with no turn at all: no LLM call, no
'RunMode' honoured (a run needs something to step), just a fresh
'SessionMeta' at 'StatusReady', ready for a later 'postMessage' or 'resume'.
-}
createSessionAs :: SessionRunner -> Maybe Text -> Text -> Maybe NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
createSessionAs runner owner slug message mode supplied =
    createSessionAsWithParent runner Nothing owner slug message mode supplied

{- | Create a session as a child (in lineage only, not call\/return -- see
'spawnSession') of another one (@todos/session-mailbox.md@, Phase 4, §5).
Shared by 'createSessionAs' (no parent) and 'spawnSession' (a parent, and
never a run mode -- a spawned session always starts running and answers by
mail, not by handing its final result back to the caller).
-}
createSessionAsWithParent :: SessionRunner -> Maybe SessionId -> Maybe Text -> Text -> Maybe NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
createSessionAsWithParent runner parent owner slug message mode supplied =
    lookupAgent runner.srHost slug >>= \case
        Nothing -> pure $ Left $ UnknownAgent slug
        Just node -> do
            sid <- newSessionId
            withLive runner sid $ \live ->
                prepareParams runner live node supplied >>= \case
                    Left err -> pure (Left err)
                    Right overlay -> do
                        now <- getCurrentTime
                        let meta0 = (freshSessionMeta sid now){smAgent = Just slug, smOwner = owner, smParent = parent}
                        sessionAgent runner live meta0 >>= \case
                            Left err -> pure (Left err)
                            Right agent -> case missingRequiredParams node agent overlay of
                                missing@(_ : _) -> pure $ Left $ MissingRequiredParams missing
                                [] -> case message of
                                    Nothing -> do
                                        -- G2: no first message, no turn, no run --
                                        -- regardless of 'mode'. 'sessionStatusOf'
                                        -- would already say 'StatusReady' for an
                                        -- empty turn list; 'store' is given it
                                        -- explicitly like every other branch here.
                                        tid <- newTurnId
                                        let sess = Session [] sid Nothing tid (Just 2) (Just Asynchronous) 0
                                        store runner live meta0 sess StatusReady Nothing >>= \case
                                            Left conflict -> pure (Left (Conflict conflict))
                                            Right meta -> do
                                                emit runner sid (SessionCreated meta)
                                                pure (Right meta)
                                    Just msg -> do
                                        sPrompt <- agent.sysPrompt
                                        sTools <- agent.sysTools
                                        sess <- newSessionFromPrompt sid sPrompt sTools (UserQuery msg.nmText msg.nmMedia)
                                        store runner live meta0 sess StatusReady Nothing >>= \case
                                            Left conflict -> pure (Left (Conflict conflict))
                                            Right meta -> do
                                                emit runner sid (SessionCreated meta)
                                                maybe (pure (Right meta)) (\m -> startRun runner live m overlay sess meta) mode

{- | @spawn-session@ (§5): start a new, durable, detached child session
running a caller's helper agent, recorded with 'parentSid' as parent in
lineage, and return as soon as it exists -- unlike 'createSessionAs' with a
run mode, this never waits for or returns the child's answer; the caller
gets it later by mail (a 'ToolCallFinished'-shaped reply is not produced,
since there is no call to complete -- the child answers with @send-message@
whenever it has something to say).
-}
spawnSession :: SessionRunner -> SessionId -> Text -> NewMessage -> IO (Either RunnerError SessionMeta)
spawnSession runner parentSid slug message =
    createSessionAsWithParent runner (Just parentSid) Nothing slug (Just message) (Just UntilBlocked) Map.empty

-- | Add a user message to an idle session, and start a run unless the mode is 'Nothing'.
{- | Add a user message to a session.

Per @todos/session-mailbox.md@ (Phase 3, closing G2), a message is always
accepted: an idle session gets it as a new turn (unchanged from before) and
starts a run unless the mode is 'Nothing'; a busy session gets it posted as
'UserMessage' mail on its durable mailbox instead of being refused — the
run already in progress folds it in at its next R1\/R2 receive point (see
"System.Agents.Session.Step").
-}
postMessage :: SessionRunner -> SessionId -> NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
postMessage runner sid message mode supplied =
    withLive runner sid $ \live -> do
        active <- readTVarIO live.lsRun
        if isJust active
            then acceptAsMail live
            else
                withIdle runner live $ \sess meta ->
                    -- Phase 6 (@todos/session-mailbox.md@ §4): a paused
                    -- session (no active run, so it fell to this branch)
                    -- has a persisted status -- not derivable from turns,
                    -- so 'sessionStatusOf' can't see it -- of
                    -- 'StatusPaused'. Its turns are exactly as they were
                    -- when it paused (e.g. a 'PartialUserTurn' with a
                    -- background call still going), so it is not safe to
                    -- append a plain new 'UserTurn' as the ordinary idle
                    -- path below does; treat the message as mail instead
                    -- (like the "busy" case above) and, since nothing is
                    -- running to fold it in, start a run so something does.
                    -- Only when 'resumeOnAnyMail' says so, and only when
                    -- 'WakeOnUser' mail (this is always a 'postMessage',
                    -- hence 'FromUser') is in the agent's 'wakeOn' list
                    -- (§5 "Scheduling rule": mail outside 'wakeOn' does not
                    -- wake a session by itself). An explicit 'resume' call
                    -- always works regardless ('resume' only checks for an
                    -- active run, not status or 'wakeOn').
                    if meta.smStatus == StatusPaused
                        then do
                            resumeOk <- agentBoolOption runner meta Base.resumeOnAnyMail
                            wakesOnUser <- agentWakesOn runner meta WakeOnUser
                            if not resumeOk || not wakesOnUser
                                then pure $ Left $ NotAcceptingMessages sid meta.smStatus
                                else
                                    acceptAsMail live >>= \case
                                        Left err -> pure (Left err)
                                        Right _ ->
                                            -- Start a run directly (not via 'resume', which
                                            -- would re-take 'live.lsLock' this callback
                                            -- already holds): the new run's own first
                                            -- receive point folds in the mail just posted.
                                            case mode of
                                                Nothing -> pure (Right meta)
                                                Just m ->
                                                    agentNodeFor runner meta >>= \case
                                                        Left err -> pure (Left err)
                                                        Right node ->
                                                            prepareParams runner live node supplied >>= \case
                                                                Left err -> pure (Left err)
                                                                Right overlay -> startRun runner live m overlay sess meta
                        else
                            let status = sessionStatusOf sess
                                {- G2 (@todos/os-as-standalone-server.md@ Phase 3b):
                                a session 'createSessionAs'\/'createSessionAsWithParent'
                                made with no first message ('Nothing') has no turns at
                                all, so 'sessionStatusOf' reports it 'StatusReady' (the
                                same status a pending, unanswered 'UserTurn' or a
                                not-yet-finished background call also reports) rather
                                than 'StatusIdle' -- it never had an answer to be idle
                                after. Accepting a message here as a fresh turn is only
                                safe in that specific case (an *empty* session): for
                                every other 'StatusReady' cause, there is already a turn
                                this new one would follow incoherently, so those still
                                fall through to 'NotAcceptingMessages' below.
                                -}
                                acceptable = status == StatusIdle || (status == StatusReady && null sess.turns)
                             in if not acceptable
                                    then pure $ Left $ NotAcceptingMessages sid status
                                    else
                                        agentNodeFor runner meta >>= \case
                                            Left err -> pure (Left err)
                                            Right node ->
                                                prepareParams runner live node supplied >>= \case
                                                    Left err -> pure (Left err)
                                                    Right overlay ->
                                                        sessionAgent runner live meta >>= \case
                                                            Left err -> pure (Left err)
                                                            Right agent -> case missingRequiredParams node agent overlay of
                                                                missing@(_ : _) -> pure $ Left $ MissingRequiredParams missing
                                                                [] -> do
                                                                    sPrompt <- agent.sysPrompt
                                                                    sTools <- agent.sysTools
                                                                    tid <- newTurnId
                                                                    let turn = UserTurn (UserTurnContent sPrompt sTools (Just (UserQuery message.nmText message.nmMedia)) [] []) Nothing
                                                                        sess' = sess{turns = turn : sess.turns, turnId = tid}
                                                                    store runner live meta sess' StatusReady Nothing >>= \case
                                                                        Left conflict -> pure (Left (Conflict conflict))
                                                                        Right meta' -> maybe (pure (Right meta')) (\m -> startRun runner live m overlay sess' meta') mode
  where
    acceptAsMail live =
        loadLatest runner live >>= \case
            Nothing -> pure $ Left $ UnknownSession sid
            Just (_, meta) ->
                sessionAgent runner live meta >>= \case
                    Left err -> pure (Left err)
                    Right agent -> case agent.ctxMailbox of
                        -- Every runner agent gets one in 'newAgent'; this
                        -- would only trip if that invariant broke.
                        Nothing -> pure $ Left $ MailboxRejected sid
                        Just mb -> do
                            sent <-
                                mb.mbSend
                                    Outgoing
                                        { outId = Nothing
                                        , outFrom = FromUser Nothing
                                        , outPriority = if message.nmInterrupt then Interrupt else Normal
                                        , outHops = 0
                                        , outBody = UserMessage (UserQuery message.nmText message.nmMedia)
                                        }
                            pure $ either (const (Left (MailboxRejected sid))) (const (Right meta)) sent

-- | Start a run on a session that has none.
resume :: SessionRunner -> SessionId -> RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
resume runner sid mode supplied =
    withLive runner sid $ \live ->
        withIdle runner live $ \sess meta ->
            agentNodeFor runner meta >>= \case
                Left err -> pure (Left err)
                Right node ->
                    prepareParams runner live node supplied >>= \case
                        Left err -> pure (Left err)
                        Right overlay ->
                            sessionAgent runner live meta >>= \case
                                Left err -> pure (Left err)
                                Right agent -> case missingRequiredParams node agent overlay of
                                    missing@(_ : _) -> pure $ Left $ MissingRequiredParams missing
                                    [] -> startRun runner live mode overlay sess meta

-- | Load a session without an active run (call under its lock).
withIdle ::
    SessionRunner ->
    LiveSession ->
    (Session -> SessionMeta -> IO (Either RunnerError a)) ->
    IO (Either RunnerError a)
withIdle runner live action = do
    active <- readTVarIO live.lsRun
    if isJust active
        then pure $ Left $ RunInProgress live.lsSessionId
        else
            loadLatest runner live >>= \case
                Nothing -> pure $ Left $ UnknownSession live.lsSessionId
                Just (sess, meta) -> action sess meta

{- | Give a deferred call its result.

Without an active run, the result is applied and stored at once, and with
auto-resume a run starts when the session can progress. During a run the
result is checked and queued; the run applies it before its next step.
-}
completeCall :: SessionRunner -> ContinuationToken -> UserToolResponse -> Bool -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
completeCall runner token result autoResume supplied = do
    let host = runner.srHost
    findSessionForToken (Just host.hostContinuations) host.hostBackend token >>= \case
        Nothing -> pure $ Left $ UnknownToken token
        Just sid -> withLive runner sid $ \live ->
            host.hostBackend.sbLoadMeta sid >>= \case
                Nothing -> pure $ Left $ UnknownSession sid
                Just (_, meta) ->
                    agentNodeFor runner meta >>= \case
                        Left err -> pure (Left err)
                        Right node ->
                            prepareParams runner live node supplied >>= \case
                                Left err -> pure (Left err)
                                Right overlay -> do
                                    active <- readTVarIO live.lsRun
                                    if isJust active then enqueue live else applyNow live overlay node
  where
    enqueue live = do
        (sess, meta) <- latestOrThrow live
        outcome <- wakeSessionWith Nothing Nothing sess [(token, result)]
        mMailbox <- sessionMailbox runner live.lsSessionId
        alreadyQueued <- case mMailbox of
            Nothing -> pure False
            Just mb -> do
                unread <- atomically (mbUnread mb sess.mailCursor)
                pure $ any isSameToken unread
        classify outcome alreadyQueued $ do
            forM_ mMailbox $ \mb ->
                void $
                    mb.mbSend
                        Outgoing
                            { outId = Nothing
                            , outFrom = FromSystem "completeCall"
                            , outPriority = Normal
                            , outHops = 0
                            , outBody = ContinuationResult token result
                            }
            pure (Right meta)
      where
        isSameToken :: Envelope -> Bool
        isSameToken e = case e.envBody of
            ContinuationResult t _ -> t == token
            _ -> False

    applyNow live overlay node =
        loadLatest runner live >>= \case
            Nothing -> pure $ Left $ UnknownSession live.lsSessionId
            Just (sess, meta) -> do
                outcome <- wakeSessionWith (Just runner.srHost.hostContinuations) Nothing sess [(token, result)]
                classify outcome False $ do
                    let woken = outcome.woSession
                        status = sessionStatusOf woken
                    store runner live meta woken status Nothing >>= \case
                        Left conflict -> pure (Left (Conflict conflict))
                        Right meta'
                            | autoResume && status == StatusReady ->
                                sessionAgent runner live meta' >>= \case
                                    Left err -> pure (Left err)
                                    Right agent -> case missingRequiredParams node agent overlay of
                                        missing@(_ : _) -> pure $ Left $ MissingRequiredParams missing
                                        [] -> startRun runner live UntilBlocked overlay woken meta'
                            | otherwise -> pure (Right meta')

    classify :: WakeOutcome -> Bool -> IO (Either RunnerError SessionMeta) -> IO (Either RunnerError SessionMeta)
    classify outcome alreadyQueued accept
        | alreadyQueued || token `elem` outcome.woAlreadyCompleted = pure $ Left $ TokenAlreadyCompleted token
        | token `elem` outcome.woApplied = accept
        | otherwise = pure $ Left $ UnknownToken token

{- | Stop a session's active run.

Its background calls are cancelled (and recorded as such), queued external
results are applied, and the session is stored with the status its turns
imply. The session keeps its agent, with a fresh engine on the next run.
-}
cancelRun :: SessionRunner -> SessionId -> IO (Either RunnerError SessionMeta)
cancelRun runner sid = do
    mRun <- maybe (pure Nothing) (readTVarIO . (.lsRun)) =<< lookupLive runner sid
    case mRun of
        Nothing -> noRun
        Just handle -> do
            -- Not under the lock: the run takes it to store its steps.
            cancel handle
            withLive runner sid $ \live -> do
                owned <- readTVarIO live.lsRun
                if owned /= Just handle then current live else tearDown live
  where
    -- The cancelled run had already stopped (and maybe another started).
    current live =
        loadLatest runner live >>= \case
            Nothing -> pure $ Left $ UnknownSession sid
            Just (_, meta) -> pure $ Right meta

    tearDown live = do
        mAgent <- readTVarIO live.lsAgent
        forM_ (mAgent >>= (.ctxAsyncEngine)) shutdownAsyncEngine
        atomically $ writeTVar live.lsAgent (fmap (\a -> a{ctxAsyncEngine = Nothing}) mAgent)
        loadLatest runner live >>= \case
            Nothing -> pure $ Left $ UnknownSession sid
            Just (sess0, meta0) -> do
                atomically $ writeTVar live.lsLatest (Just (sess0, meta0))
                (sess1, meta1) <- case mAgent of
                    Just agent -> applyInbox runner agent live
                    Nothing -> pure (sess0, meta0)
                sess2 <- case mAgent of
                    Just agent -> refreshHeadPartialTurn (buildContext agent sess1 (sessionIdToConversationId sid)) sess1
                    Nothing -> pure sess1
                atomically $ writeTVar live.lsRun Nothing
                let status = sessionStatusOf sess2
                result <- store runner live meta1 sess2 status Nothing
                emit runner sid $ RunStopped status
                pure $ either (Left . Conflict) Right result

    noRun =
        runner.srHost.hostBackend.sbLoadMeta sid >>= \case
            Nothing -> pure $ Left $ UnknownSession sid
            Just _ -> pure $ Left $ NoActiveRun sid

{- | Hard-cancel every tool call currently attached to a session: posts
'CancelAllAttached' 'Control' mail, which the runner loop reacts to on its
next iteration by killing each call still tracked as 'Running' through the
agent's async engine (see 'runningToolCallIds', 'cancelAttachedCall').

Unlike 'cancelRun' (which tears down the whole run and its async engine),
this only targets attached calls; the run itself keeps going and the LLM
is asked again once the calls are gone. Unlike an 'Interrupt'-priority
'UserMessage' (a soft interrupt, only detaches), a cancelled call's result
never arrives -- there is nothing left to deliver. Works whether or not
the session currently has an active run, since 'Control' mail is picked
up the next time one does.
-}
cancelAttachedCalls :: SessionRunner -> SessionId -> IO (Either RunnerError SessionMeta)
cancelAttachedCalls runner sid = sendControlMail runner sid CancelAllAttached

{- | Pause a session: posts 'Pause' 'Control' mail, which the runner loop
reacts to at its next iteration by stopping the run (persisting
'StatusPaused') without cancelling any attached calls, unless the agent's
config sets 'pauseCancelsCalls'. Reached the same way as
'cancelAttachedCalls': works whether or not a run is currently active,
since 'Control' mail is picked up the next time one starts. Resuming is
'resume', which (per its own doc) works from 'StatusPaused' regardless of
this mail ever being read.
-}
pauseSession :: SessionRunner -> SessionId -> IO (Either RunnerError SessionMeta)
pauseSession runner sid = sendControlMail runner sid Pause

{- | Post 'Control' mail to a session at 'Normal' priority, from no owner --
a thin wrapper over 'sendMail' (G6) kept for 'cancelAttachedCalls' and
'pauseSession', and for existing callers that only want the up to date
'SessionMeta' rather than a mail 'Receipt'.
-}
sendControlMail :: SessionRunner -> SessionId -> ControlMsg -> IO (Either RunnerError SessionMeta)
sendControlMail runner sid msg =
    sendMail runner sid Nothing Normal (Control msg) >>= \case
        Left err -> pure (Left err)
        Right _receipt -> maybe (Left (UnknownSession sid)) (Right . snd) <$> getSession runner sid

{- | Post mail to a session (G6, @todos/os-as-standalone-server.md@),
generalizing 'sendControlMail': any 'MailBody' -- 'UserMessage',
'AgentMessage', 'ToolCallFinished', 'ContinuationResult', 'WatchedEvent',
or 'Control' (of 'Pause', 'Resume', 'CancelCalls', 'CancelAllAttached',
'StopRun') -- from a given owner (or none), at a given 'Priority'. This is
how a client posts 'StopRun' on quit, or sends 'AgentMessage' mail on
behalf of another session, over the same mailbox 'postMessage' and
'serverMailRouter' already share.

Reuses 'sessionMailbox' (the live-vs-durable lookup 'postMessage' and
'serverMailRouter' use), so a stored-but-not-live session gets its durable
mailbox, not a fresh, disconnected one. Waking mirrors 'postMessage''s own
paused branch exactly: a session with an active run, or one that is
otherwise idle\/ready, is left alone (mail just queues, folded in at the
next R1\/R2, or whenever something else starts a run); a 'StatusPaused'
session is woken -- a run started with 'UntilBlocked' -- only when the
agent's @resumeOnAnyMail@ option is set and this mail's sender classifies
(via 'senderWakeKind') into one of its @wakeOn@ kinds, exactly the test
'postMessage' applies to a message arriving while paused.
-}
sendMail :: SessionRunner -> SessionId -> Maybe Text -> Priority -> MailBody -> IO (Either RunnerError Receipt)
sendMail runner sid owner priority body =
    withLive runner sid $ \live -> do
        mMailbox <- sessionMailbox runner sid
        case mMailbox of
            Nothing -> pure $ Left $ UnknownSession sid
            Just mb -> do
                sent <-
                    mb.mbSend
                        Outgoing
                            { outId = Nothing
                            , outFrom = FromUser owner
                            , outPriority = priority
                            , outHops = 0
                            , outBody = body
                            }
                case sent of
                    Left _sendErr -> pure $ Left $ MailboxRejected sid
                    Right receipt -> do
                        wakeIfPaused live
                        pure $ Right receipt
  where
    wakeIfPaused live = do
        active <- readTVarIO live.lsRun
        unless (isJust active) $
            loadLatest runner live >>= \case
                Nothing -> pure ()
                Just (sess, meta) ->
                    when (meta.smStatus == StatusPaused) $ do
                        resumeOk <- agentBoolOption runner meta Base.resumeOnAnyMail
                        wakes <- agentWakesOn runner meta (senderWakeKind (FromUser owner))
                        when (resumeOk && wakes) $
                            agentNodeFor runner meta >>= \case
                                Left _ -> pure ()
                                Right node ->
                                    prepareParams runner live node Map.empty >>= \case
                                        Left _ -> pure ()
                                        Right overlay -> void $ startRun runner live UntilBlocked overlay sess meta

{- | A session's mail, oldest first: every envelope ever accepted, or only
those unread past the session's stored 'mailCursor' (G6). Uses 'getSession'
for the cursor -- the last-stored one, which is what the session's next run
will fold in from -- not any in-flight cursor a run in progress has not
stored yet.
-}
listMail :: SessionRunner -> SessionId -> Bool -> IO (Either RunnerError [Envelope])
listMail runner sid unreadOnly = do
    mMailbox <- sessionMailbox runner sid
    case mMailbox of
        Nothing -> pure $ Left $ UnknownSession sid
        Just mb -> do
            cursor <-
                if unreadOnly
                    then maybe 0 ((.mailCursor) . fst) <$> getSession runner sid
                    else pure 0
            Right <$> atomically (mb.mbUnread cursor)

{- | Fork a session (G6): copy it, or a prefix of it up to and including
one turn, into a fresh session with its own id, recording 'forkedFromSessionId'
and starting no run.

'atTurn', when given, is a 0-based index into the source session's 'turns'
(newest first, matching the TUI's own @navSelectedTurnIndex@\/
@handleForkAtTurn@): the fork keeps that turn and every older one,
dropping anything newer, the same truncation the TUI performs today.
'Nothing' copies the whole session. See 'RunnerError.UnknownTurn' for why
this is an index rather than the @TurnId@ the design sketch shows -- 'Turn'
carries no id of its own to fork at.

The fork keeps the source's agent unless @newAgentSlug@ names another one
(validated to exist, 'UnknownAgent' otherwise -- this also covers
"continue with another agent": a fork at the head, i.e. @atTurn = Just 0@
or 'Nothing', with a new slug). It also keeps the source's parent link and
its non-secret session parameters (as the initial live overlay, so a
resumed fork does not immediately hit 'MissingRequiredParams' for values
the source already had bound), but never its status: a fork's status is
whatever 'sessionStatusOf' derives from the turns it kept, so it never
shares a later status change with the session it was forked from (a
paused or failed source forks into a session that is simply ready or idle
per its copied turns).
-}
forkSession :: SessionRunner -> Maybe Text -> SessionId -> Maybe Int -> Maybe Text -> IO (Either RunnerError SessionMeta)
forkSession runner owner sourceSid atTurn newAgentSlug =
    runner.srHost.hostBackend.sbLoadMeta sourceSid >>= \case
        Nothing -> pure $ Left $ UnknownSession sourceSid
        Just (sourceSess, sourceMeta) -> case atTurn of
            Just idx
                | idx < 0 || idx >= length sourceSess.turns -> pure $ Left $ UnknownTurn sourceSid idx
            _ -> resolveAgent sourceSess sourceMeta
  where
    resolveAgent :: Session -> SessionMeta -> IO (Either RunnerError SessionMeta)
    resolveAgent sourceSess sourceMeta = case newAgentSlug of
        Just slug ->
            lookupAgent runner.srHost slug >>= \case
                Nothing -> pure $ Left $ UnknownAgent slug
                Just _ -> doFork sourceSess sourceMeta slug
        Nothing -> case sourceMeta.smAgent of
            Nothing -> pure $ Left $ UnknownAgent ""
            Just slug -> doFork sourceSess sourceMeta slug

    doFork :: Session -> SessionMeta -> Text -> IO (Either RunnerError SessionMeta)
    doFork sourceSess sourceMeta slug = do
        newSid <- newSessionId
        newTid <- newTurnId
        now <- getCurrentTime
        let keptTurns = maybe sourceSess.turns (`drop` sourceSess.turns) atTurn
            forked =
                Session
                    { turns = keptTurns
                    , sessionId = newSid
                    , forkedFromSessionId = Just sourceSid
                    , turnId = newTid
                    , sessionVersion = Just 2
                    , sessionExecutionMode = sourceSess.sessionExecutionMode
                    , mailCursor = 0
                    }
            status = sessionStatusOf forked
            meta0 =
                (freshSessionMeta newSid now)
                    { smAgent = Just slug
                    , smOwner = owner
                    , smParent = sourceMeta.smParent
                    , smParams = sourceMeta.smParams
                    }
        withLive runner newSid $ \live -> do
            atomically $ writeTVar live.lsParams (Map.map (\v -> ParamValue v False) sourceMeta.smParams)
            store runner live meta0 forked status Nothing >>= \case
                Left conflict -> pure (Left (Conflict conflict))
                Right meta -> do
                    emit runner newSid (SessionCreated meta)
                    pure (Right meta)

{- | Who a session belongs to: the owner of its root session, since
sub-sessions record none. 'Nothing' when the session does not exist.
-}
sessionOwner :: SessionRunner -> SessionId -> IO (Maybe (Maybe Text))
sessionOwner runner = go []
  where
    go seen sid =
        runner.srHost.hostBackend.sbLoadMeta sid >>= \case
            Nothing -> pure Nothing
            Just (_, meta) -> case meta.smParent of
                Just parent | parent `notElem` seen -> go (sid : seen) parent
                _ -> pure (Just meta.smOwner)

-- | The latest stored version of a session.
getSession :: SessionRunner -> SessionId -> IO (Maybe (Session, SessionMeta))
getSession runner sid = runner.srHost.hostBackend.sbLoadMeta sid

{- | Metadata of matching sessions, most recently updated first (G6,
@todos/os-as-standalone-server.md@): 'sbQuery' on the runner's backend,
with each result's metadata replaced by this process's own live, cached
copy when the session is live here, so a caller sees a status this process
just set (e.g. 'StatusRunning') without waiting for the next store to
land. A session live in another process is unaffected: only this
process's 'srLive' is consulted.
-}
listSessions :: SessionRunner -> SessionQuery -> IO [SessionMeta]
listSessions runner query = do
    stored <- runner.srHost.hostBackend.sbQuery query
    mapM preferLive stored
  where
    preferLive :: SessionMeta -> IO SessionMeta
    preferLive meta = do
        mLive <- lookupLive runner meta.smSessionId
        case mLive of
            Nothing -> pure meta
            Just live -> maybe meta snd <$> readTVarIO live.lsLatest

{- | Every root agent this host knows, as an 'AgentDescriptor' (G6, the
"agent details" gap row: model, system prompt, tool activation, helpers --
a superset of what @GET \/v1\/agents@ used to answer with).
-}
listAgents :: SessionRunner -> IO [AgentDescriptor]
listAgents runner = do
    agents <- hostAllAgents runner.srHost
    mapM (uncurry (agentDescriptor runner)) (Map.toList agents)

-- | One root agent's descriptor, by slug; 'Nothing' if there is none.
getAgent :: SessionRunner -> Text -> IO (Maybe AgentDescriptor)
getAgent runner slug = do
    agents <- hostAllAgents runner.srHost
    mapM (agentDescriptor runner slug) (Map.lookup slug agents)

-- | Build one agent's 'AgentDescriptor' from its live 'OSAgentNode'.
agentDescriptor :: SessionRunner -> Text -> (AgentSource, OSAgentNode) -> IO AgentDescriptor
agentDescriptor runner slug (source, node) = do
    tools <- readTVarIO node.osNodeTools
    resolved <- readTVarIO node.osNodeParams
    let host = runner.srHost
        cfg = node.osNodeConfig
        pinnedNames = Set.fromList [n | (n, pv) <- Map.toList host.hostProcessParams, pv.pvPinned]
        decls = fromMaybe [] (Base.parameters cfg)
        toParam :: ParameterDecl -> AgentParameter
        toParam d =
            AgentParameter
                { apName = d.paramName
                , apDescription = d.paramDescription
                , apSecret = d.paramSecret
                , apScope = d.paramScope
                , apRequired = d.paramRequired
                , apBound = Map.member d.paramName resolved
                , apPinned = d.paramScope == ScopeProcess || d.paramName `Set.member` pinnedNames
                }
        toTool :: ToolRegistration -> ToolDescriptor
        toTool tr =
            ToolDescriptor
                { tdName = tr.declareTool.toolDescriptionName.getToolName
                , tdDescription = tr.declareTool.toolDescriptionText
                , tdActivation = tr.toolActivation
                }
        (src, updatedAt, updatedBy, config) = case source of
            FromFile -> ("file", Nothing, Nothing, Nothing)
            FromDatabase sa -> ("database", Just sa.saUpdatedAt, sa.saUpdatedBy, Just (Aeson.toJSON sa.saConfig))
    pure
        AgentDescriptor
            { adSlug = slug
            , adDescription = Base.announce cfg
            , adModel = Base.modelName cfg
            , adSystemPrompt = Base.systemPrompt cfg
            , adSource = src
            , adTools = map toTool tools
            , adParameters = map toParam decls
            , adHelpers = [Base.slug c.osNodeConfig | c <- node.osNodeChildren]
            , adUpdatedAt = updatedAt
            , adUpdatedBy = updatedBy
            , adConfig = config
            }

{- | Wait until the session's active run stops, or the timeout expires.

Returns the latest metadata and whether a run is still active. Never takes
the session's lock, and never affects the run.
-}
awaitRun :: SessionRunner -> SessionId -> NominalDiffTime -> IO (Either RunnerError (SessionMeta, Bool))
awaitRun runner sid limit = do
    mLive <- lookupLive runner sid
    mRun <- maybe (pure Nothing) (readTVarIO . (.lsRun)) mLive
    for_ mRun $ \handle -> timeout (micros limit) (waitCatch handle)
    active <- maybe (pure False) (fmap isJust . readTVarIO . (.lsRun)) mLive
    getSession runner sid >>= \case
        Nothing -> pure $ Left $ UnknownSession sid
        Just (_, meta) -> pure $ Right (meta, active)
  where
    micros t = max 0 (round (realToFrac t * 1_000_000 :: Double))

{- | Mark sessions left running by a previous process as they are.

Their interrupted step is lost; background calls they were running are
resolved as orphaned on the next run. Nothing is resumed.
-}
recoverOnStartup :: SessionRunner -> IO [SessionId]
recoverOnStartup runner = do
    let backend = runner.srHost.hostBackend
    stale <- backend.sbQuery allSessionsQuery{sqStatuses = Just [StatusRunning]}
    recovered <- forM stale $ \meta0 -> withLive runner meta0.smSessionId $ \live -> do
        active <- readTVarIO live.lsRun
        if isJust active
            then pure Nothing
            else
                loadLatest runner live >>= \case
                    Just (sess, meta) | meta.smStatus == StatusRunning -> do
                        result <- store runner live meta sess (sessionStatusOf sess) Nothing
                        pure $ either (const Nothing) (const (Just meta.smSessionId)) result
                    _ -> pure Nothing
    let sids = [sid | Just sid <- recovered]
    unless (null sids) $ runTracer runner.srHost.hostTracer $ HostRecoveredSessions sids
    pure sids

{- | Delete a session with all its sub-sessions and their continuations.

Refused while a run is active on any session of the tree or on an ancestor
(whose sub-agent calls write into the tree), in both modes. Deepest sessions
go first, so an interrupted delete never leaves a sub-session without its
parent; deleting again finishes the job.
-}
deleteSession :: SessionRunner -> SessionId -> DeleteMode -> IO (Either RunnerError DeletionPlan)
deleteSession runner sid mode =
    host.hostBackend.sbLoadMeta sid >>= \case
        Nothing -> pure $ Left $ UnknownSession sid
        Just (_, meta) -> do
            tree <- descendantsFirst [sid]
            ancestors <- ancestorsOf meta
            busy <- filterM hasActiveRun (tree <> ancestors)
            case busy of
                (owner : _) -> pure $ Left $ RunInProgress owner
                [] -> case mode of
                    DryRun -> do
                        counts <- mapM host.hostContinuations.csCountSession tree
                        pure $ Right $ DeletionPlan tree (sum counts) True
                    DeleteForReal -> deleteAll tree 0
  where
    host = runner.srHost

    deleteAll [] removed = pure $ Right $ DeletionPlan [] removed False
    deleteAll (s : rest) removed = do
        deleted <- withLive runner s $ \live -> do
            active <- readTVarIO live.lsRun
            if isJust active
                then pure $ Left $ RunInProgress s
                else do
                    mOwner <- ownerOfLive runner s
                    n <- host.hostContinuations.csDeleteSession s
                    host.hostBackend.sbDelete s
                    dropLive runner live
                    emitOwned runner s mOwner (SessionDeleted s)
                    pure $ Right n
        case deleted of
            Left err -> pure (Left err)
            Right n -> fmap (\plan -> plan{dpSessions = s : plan.dpSessions}) <$> deleteAll rest (removed + n)

    -- Post-order over children, guarding against cycles.
    descendantsFirst :: [SessionId] -> IO [SessionId]
    descendantsFirst = go []
      where
        go _ [] = pure []
        go seen (s : rest)
            | s `elem` seen = go seen rest
            | otherwise = do
                children <- map (.smSessionId) <$> host.hostBackend.sbQuery allSessionsQuery{sqParent = Just s}
                below <- go (s : seen) children
                others <- go (s : seen <> below) rest
                pure $ nub (below <> [s] <> others)

    ancestorsOf :: SessionMeta -> IO [SessionId]
    ancestorsOf = go []
      where
        go seen m = case m.smParent of
            Just p | p `notElem` seen -> do
                loaded <- host.hostBackend.sbLoadMeta p
                case loaded of
                    Just (_, pm) -> (p :) <$> go (p : seen) pm
                    Nothing -> pure [p]
            _ -> pure []

    hasActiveRun :: SessionId -> IO Bool
    hasActiveRun s = lookupLive runner s >>= maybe (pure False) (fmap isJust . readTVarIO . (.lsRun))
