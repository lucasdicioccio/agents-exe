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
  started by one run are picked up by the next;
* a queue of external results for deferred calls, applied by the run.

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
    createSession,
    createSessionAs,
    sessionOwner,
    postMessage,
    resume,
    completeCall,
    cancelRun,
    getSession,
    awaitRun,
    recoverOnStartup,

    -- * Deletion
    DeleteMode (..),
    DeletionPlan (..),
    deleteSession,

    -- * Events
    SessionEvent (..),
    sessionEventKind,
    sessionEventSession,
    subscribe,
    subscribeSTM,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeAsyncException, SomeException, bracket, displayException, fromException, throwIO, try)
import Control.Monad (filterM, forM, forM_, forever, unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Foldable (for_)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Prod.Tracer (contramap, runTracer)
import System.Timeout (timeout)

import System.Agents.AgentFactory (AgentDeps (..), AgentRole (..), buildAgent)
import System.Agents.AgentTree (OSAgentNode (osNodeConfig))
import qualified System.Agents.Base as Base
import System.Agents.Host
import System.Agents.Media.Types (MediaAttachment)
import System.Agents.Session.Async (ContinuationStore (..))
import System.Agents.Session.Async.Engine (shutdownAsyncEngine)
import System.Agents.Session.Base hiding (SessionProgress (..))
import System.Agents.Session.Step (buildContext, refreshHeadPartialTurn, runStepM)
import System.Agents.Session.Wake (WakeOutcome (..), findSessionForToken, wakeSessionWith)
import System.Agents.SessionStore (
    SessionMeta (..),
    SessionQuery (..),
    VersionConflict,
    allSessionsQuery,
    freshSessionMeta,
    sessionIdToConversationId,
 )
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

-- | How far a run goes.
data RunMode
    = -- | One step, e.g. one LLM call or one batch of tool calls.
      StepOnce
    | -- | Until the LLM answers, or only deferred calls remain.
      UntilBlocked
    deriving (Show, Eq)

-- | A user message.
data NewMessage = NewMessage
    { nmText :: Text
    , nmMedia :: [MediaAttachment]
    }

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
    deriving (Show, Eq)

-- | What happened to a session, in the order it happened.
data SessionEvent
    = RunStarted SessionId RunMode
    | -- | A new version was stored; carries the head turn.
      SessionUpdated SessionId SessionMeta (Maybe Turn)
    | -- | The run stopped on deferred calls, which an external worker must complete.
      CallsDeferred SessionId [DeferredCallView]
    | RunStopped SessionId SessionStatus
    | SessionFailed SessionId Text
    | -- | A piece of the LLM's answer, when the host streams tokens.
      TextDelta SessionId Text
    deriving (Show)

-- | The event's name in the HTTP events stream.
sessionEventKind :: SessionEvent -> Text
sessionEventKind = \case
    RunStarted{} -> "run.started"
    SessionUpdated{} -> "session.updated"
    CallsDeferred{} -> "calls.deferred"
    RunStopped{} -> "run.stopped"
    SessionFailed{} -> "session.failed"
    TextDelta{} -> "text.delta"

sessionEventSession :: SessionEvent -> SessionId
sessionEventSession = \case
    RunStarted sid _ -> sid
    SessionUpdated sid _ _ -> sid
    CallsDeferred sid _ -> sid
    RunStopped sid _ -> sid
    SessionFailed sid _ -> sid
    TextDelta sid _ -> sid

data DeleteMode = DryRun | DeleteForReal
    deriving (Show, Eq)

data DeletionPlan = DeletionPlan
    { dpSessions :: [SessionId]
    -- ^ The session and all its descendants, deepest first.
    , dpContinuations :: Int
    -- ^ Continuation rows removed, or that would be.
    , dpDryRun :: Bool
    }
    deriving (Show, Eq)

type RunnerAgent = Agent (LlmTurnContent, Session)

data SessionRunner = SessionRunner
    { srHost :: Host
    , srLive :: TVar (Map SessionId LiveSession)
    , srEvents :: TChan SessionEvent
    -- ^ Broadcast; subscribers filter by session, so eviction loses nothing.
    , srReaper :: Async ()
    }

-- | In-memory state of a session this process has touched.
data LiveSession = LiveSession
    { lsSessionId :: SessionId
    , lsLock :: MVar ()
    , lsRun :: TVar (Maybe (Async ()))
    , lsAgent :: TVar (Maybe RunnerAgent)
    , lsLatest :: TVar (Maybe (Session, SessionMeta))
    -- ^ The last version this process stored or loaded.
    , lsInbox :: TVar [(ContinuationToken, UserToolResponse)]
    -- ^ Results accepted while a run is active; the run applies them.
    , lsLastTouched :: TVar UTCTime
    , lsEvicted :: TVar Bool
    , lsParams :: TVar Params
    {- ^ Session-scope parameter values, secret and non-secret, kept only in
    this process's memory. Lost on restart or eviction; a client must
    resupply them, or the next run that needs one fails with
    'MissingRequiredParams' (@todos/tool-partial-application.md@, Phase 4).
    -}
    }

data RunnerStats = RunnerStats
    { rsLiveSessions :: Int
    , rsActiveRuns :: Int
    }
    deriving (Show, Eq)

-- | A versioned write lost to a writer outside this runner.
newtype ConcurrentModification = ConcurrentModification VersionConflict
    deriving (Show)

instance Exception ConcurrentModification

-------------------------------------------------------------------------------
-- Lifecycle
-------------------------------------------------------------------------------

newSessionRunner :: Host -> IO SessionRunner
newSessionRunner host = do
    live <- newTVarIO Map.empty
    events <- newBroadcastTChanIO
    let ttl = host.hostLiveSessionTtl
    self <- newEmptyTMVarIO
    reaper <- async $ do
        runner <- atomically $ readTMVar self
        forever $ do
            threadDelay (reaperInterval ttl)
            evictIdle runner
    let runner = SessionRunner host live events reaper
    atomically $ putTMVar self runner
    pure runner

-- | How often idle sessions are looked for: half the idle time, within bounds.
reaperInterval :: NominalDiffTime -> Int
reaperInterval ttl = max 50_000 (min 60_000_000 (round (realToFrac ttl * 500_000 :: Double)))

-- | Cancel active runs (storing their sessions), and stop every engine.
shutdownSessionRunner :: SessionRunner -> IO ()
shutdownSessionRunner runner = do
    cancel runner.srReaper
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
            <*> newTVarIO []
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

emit :: SessionRunner -> SessionEvent -> IO ()
emit runner event = do
    atomically $ writeTChan runner.srEvents event
    case event of
        -- One per token: too many, and too revealing, for the logs.
        TextDelta{} -> pure ()
        _ -> runTracer runner.srHost.hostTracer $ HostRunnerTrace (sessionEventKind event) (sessionEventSession event)

{- | Next events of one session. Each call to 'subscribe' gets its own
stream, starting now; the returned action blocks until the next event.
-}
subscribe :: SessionRunner -> SessionId -> IO (IO SessionEvent)
subscribe runner sid = do
    next <- subscribeSTM runner sid
    -- One transaction per event: after a 'readTChan', 'retry' would roll the
    -- read back and block on the same event of another session forever.
    let loop = atomically next >>= maybe loop pure
    pure loop

{- | Like 'subscribe', as a transaction to combine with others (timers,
shutdown flags). Each run consumes one event of any session, and yields
'Nothing' for another session's event.
-}
subscribeSTM :: SessionRunner -> SessionId -> IO (STM (Maybe SessionEvent))
subscribeSTM runner sid = do
    chan <- atomically $ dupTChan runner.srEvents
    pure $ do
        event <- readTChan chan
        pure $ if sessionEventSession event == sid then Just event else Nothing

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
        emit runner $ SessionUpdated live.lsSessionId meta' (headTurn sess)
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
            | host.hostStreamTokens = host.hostDeps{adOnTextDelta = Just (emit runner . TextDelta sid)}
            | otherwise = host.hostDeps
        deps = deps0{adLiveParams = readTVarIO live.lsParams}
    agent <- buildAgent (contramap HostAgentTrace host.hostTracer) deps RootAgent (sessionIdToConversationId sid) node
    pure $ withExecutionMode Asynchronous agent

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
                    emit runner $ RunStarted live.lsSessionId mode
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
            (sess, meta) <- applyInbox runner live
            let stop =
                    finished
                        || (mode == StepOnce && steps >= 1)
                        || isBlockedOnDeferredCalls sess
            if stop
                then do
                    let status = sessionStatusOf sess
                    _ <- storeOrThrow runner live meta sess status
                    atomically $ writeTVar live.lsRun Nothing
                    when (status == StatusWaitingExternal) $
                        emit runner $
                            CallsDeferred sid (pendingDeferredCalls sess)
                    emit runner $ RunStopped sid status
                    pure Nothing
                else pure (Just sess)
        for_ next $ \sess -> do
            (agent', result) <- runStepM convId agent sess
            atomically $ writeTVar live.lsAgent (Just agent')
            let (sess', done) = case result of
                    Left (_, final) -> (final, True)
                    Right s -> (s, False)
            withMVar live.lsLock $ \_ -> do
                (_, meta) <- latestOrThrow live
                void $ storeOrThrow runner live meta sess' StatusRunning
            go agent' (steps + 1) done

-- | Apply the queued external results to the latest version (under the lock).
applyInbox :: SessionRunner -> LiveSession -> IO (Session, SessionMeta)
applyInbox runner live = do
    (sess, meta) <- latestOrThrow live
    inbox <- atomically $ swapTVar live.lsInbox []
    if null inbox
        then pure (sess, meta)
        else do
            outcome <- wakeSessionWith (Just runner.srHost.hostContinuations) Nothing sess inbox
            meta' <- storeOrThrow runner live meta outcome.woSession StatusRunning
            pure (outcome.woSession, meta')

latestOrThrow :: LiveSession -> IO (Session, SessionMeta)
latestOrThrow live =
    readTVarIO live.lsLatest >>= maybe (throwIO $ userError "runner: session state missing") pure

-- | Record a failed run: the last stored version, marked failed.
failRun :: SessionRunner -> LiveSession -> Text -> IO ()
failRun runner live reason = withMVar live.lsLock $ \_ -> do
    loaded <- loadLatest runner live
    for_ loaded $ \(sess, meta) ->
        void $ store runner live meta sess StatusFailed (Just reason)
    atomically $ writeTVar live.lsRun Nothing
    emit runner $ SessionFailed live.lsSessionId reason
    emit runner $ RunStopped live.lsSessionId StatusFailed

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Create a session for an agent, and start a run unless the mode is 'Nothing'.
createSession :: SessionRunner -> Text -> NewMessage -> Maybe RunMode -> IO (Either RunnerError SessionMeta)
createSession runner slug message mode = createSessionAs runner Nothing slug message mode Map.empty

-- | Like 'createSession', for an owner, with caller-supplied parameter values.
createSessionAs :: SessionRunner -> Maybe Text -> Text -> NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
createSessionAs runner owner slug message mode supplied =
    lookupAgent runner.srHost slug >>= \case
        Nothing -> pure $ Left $ UnknownAgent slug
        Just node -> do
            sid <- newSessionId
            withLive runner sid $ \live ->
                prepareParams runner live node supplied >>= \case
                    Left err -> pure (Left err)
                    Right overlay -> do
                        now <- getCurrentTime
                        let meta0 = (freshSessionMeta sid now){smAgent = Just slug, smOwner = owner}
                        sessionAgent runner live meta0 >>= \case
                            Left err -> pure (Left err)
                            Right agent -> case missingRequiredParams node agent overlay of
                                missing@(_ : _) -> pure $ Left $ MissingRequiredParams missing
                                [] -> do
                                    sPrompt <- agent.sysPrompt
                                    sTools <- agent.sysTools
                                    sess <- newSessionFromPrompt sid sPrompt sTools (UserQuery message.nmText message.nmMedia)
                                    store runner live meta0 sess StatusReady Nothing >>= \case
                                        Left conflict -> pure (Left (Conflict conflict))
                                        Right meta -> maybe (pure (Right meta)) (\m -> startRun runner live m overlay sess meta) mode

-- | Add a user message to an idle session, and start a run unless the mode is 'Nothing'.
postMessage :: SessionRunner -> SessionId -> NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> IO (Either RunnerError SessionMeta)
postMessage runner sid message mode supplied =
    withLive runner sid $ \live ->
        withIdle runner live $ \sess meta -> do
            let status = sessionStatusOf sess
            if status /= StatusIdle
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
        queued <- map fst <$> readTVarIO live.lsInbox
        outcome <- wakeSessionWith Nothing Nothing sess [(token, result)]
        classify outcome (token `elem` queued) $ do
            atomically $ modifyTVar' live.lsInbox (<> [(token, result)])
            pure (Right meta)

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
                (sess1, meta1) <- applyInbox runner live
                sess2 <- case mAgent of
                    Just agent -> refreshHeadPartialTurn (buildContext agent sess1 (sessionIdToConversationId sid)) sess1
                    Nothing -> pure sess1
                atomically $ writeTVar live.lsRun Nothing
                let status = sessionStatusOf sess2
                result <- store runner live meta1 sess2 status Nothing
                emit runner $ RunStopped sid status
                pure $ either (Left . Conflict) Right result

    noRun =
        runner.srHost.hostBackend.sbLoadMeta sid >>= \case
            Nothing -> pure $ Left $ UnknownSession sid
            Just _ -> pure $ Left $ NoActiveRun sid

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
                    n <- host.hostContinuations.csDeleteSession s
                    host.hostBackend.sbDelete s
                    dropLive runner live
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
