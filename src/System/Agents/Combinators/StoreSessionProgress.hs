{-# LANGUAGE ScopedTypeVariables #-}

{- | Session storage and progress tracking combinators for agents.

This module provides combinators for wrapping agents with session persistence
and progress tracking capabilities. The primary functions are:

* 'agentPersistSession' - wraps an agent to persist sessions to an explicit 'SessionSink'
* 'agentStoreSession' - wraps an agent to persist sessions to a 'SessionStore'
* 'agentStoreSessionWithCallback' - same but also invokes a progress callback
* 'agentWithSessionProgress' - wraps an agent to emit progress events after each step

'agentPersistSession' stores to the sink it is given, whatever the agent's
fields say later. The older 'agentStoreSession' reads 'ctxSessionBackend' from
the agent it wraps, at wrapping time: a backend installed on the result
afterwards is not used by it. The optional explicit 'FilePath' always receives
an additional copy.

Example usage:

@
import System.Agents.Combinators.StoreSessionProgress (agentStoreSession, agentWithSessionProgress)

-- Wrap an agent with session storage
storedAgent <- agentStoreSession store Nothing convId baseAgent

-- Or wrap with custom progress handling
progressAgent <- agentWithSessionProgress myProgressCallback baseAgent

-- Use a durable backend (e.g., SQLite)
durableAgent <- pure $ withSessionBackend backend baseAgent
storedAgent' <- agentStoreSession store Nothing convId durableAgent

-- Storage + external progress callback
storedAndObserved <- agentStoreSessionWithCallback store Nothing convId myCallback durableAgent
@
-}
module System.Agents.Combinators.StoreSessionProgress (
    -- * Session Storage Combinators
    SessionSink (..),
    agentPersistSession,
    sinkStoreCallback,
    agentStoreSession,
    agentStoreSessionWithCallback,
    agentWithSessionProgress,

    -- * Callback Utilities
    sessionStoreCallback,
    backendStoreCallback,
    backendStoreCallbackWith,
    backendWithCallbackStoreCallback,
    filepathStoreCallback,
) where

import System.Agents.Base (ConversationId)
import System.Agents.Session.Base (
    Action,
    Agent (..),
    OnSessionProgress,
    Session (..),
    SessionBackend (..),
    SessionProgress (..),
 )
import System.Agents.SessionStore (SessionLabels (..), SessionStore, noLabels)
import qualified System.Agents.SessionStore as SessionStore

{- | Where an agent persists its sessions.

The sink is chosen when the agent is built, so storage never depends on
agent fields set afterwards.
-}
data SessionSink
    = -- | Store under the session's own 'SessionId'.
      SinkBackend SessionBackend
    | -- | Store as a file keyed by the agent's 'ConversationId'.
      SinkFiles SessionStore
    | -- | Do not store sessions.
      SinkNone

{- | Creates a callback that stores session progress into a 'SessionSink'.

Backends also record the labels; the file store has nowhere to put them.
-}
sinkStoreCallback :: SessionSink -> SessionLabels -> ConversationId -> OnSessionProgress
sinkStoreCallback (SinkBackend backend) labels _ = backendStoreCallbackWith backend labels
sinkStoreCallback (SinkFiles store) _ convId = sessionStoreCallback store convId
sinkStoreCallback SinkNone _ _ = const (pure ())

-- | Wrap an agent to store its session into the given sink before every step.
agentPersistSession :: forall r. SessionSink -> SessionLabels -> ConversationId -> Agent r -> Agent r
agentPersistSession SinkNone _ _ agent = agent
agentPersistSession sink labels convId agent =
    agentWithSessionProgress (sinkStoreCallback sink labels convId) agent

-- | Creates a callback that stores session progress using a SessionStore.
sessionStoreCallback :: SessionStore -> ConversationId -> OnSessionProgress
sessionStoreCallback store convId progress =
    case progress of
        SessionUpdated sess -> storeSessionWithStore sess
        SessionCompleted sess -> storeSessionWithStore sess
        SessionStarted sess -> storeSessionWithStore sess
        SessionFailed sess _ -> storeSessionWithStore sess
  where
    storeSessionWithStore sess =
        SessionStore.storeSession store convId sess

{- | Creates a callback that stores session progress using a 'SessionBackend'.

The session id from the progress event is used as the backend key, so the
backend is responsible for mapping 'SessionId's to its storage layout.
-}
backendStoreCallback :: SessionBackend -> OnSessionProgress
backendStoreCallback backend = backendStoreCallbackWith backend noLabels

-- | Like 'backendStoreCallback', also recording labels next to the session.
backendStoreCallbackWith :: SessionBackend -> SessionLabels -> OnSessionProgress
backendStoreCallbackWith backend labels progress =
    case progress of
        SessionUpdated sess -> storeSessionWithBackend sess
        SessionCompleted sess -> storeSessionWithBackend sess
        SessionStarted sess -> storeSessionWithBackend sess
        SessionFailed sess _ -> storeSessionWithBackend sess
  where
    storeSessionWithBackend sess =
        sbStoreLabelled backend labels sess.sessionId sess

{- | Creates a callback that stores session progress using a 'SessionBackend'
and then forwards the progress event to an additional callback.

Useful when the caller wants to both persist to a durable backend and
observe / react to every progress event (for example, to emit metrics,
update a UI, or log lifecycle transitions).
-}
backendWithCallbackStoreCallback :: SessionBackend -> OnSessionProgress -> OnSessionProgress
backendWithCallbackStoreCallback backend callback progress = do
    backendStoreCallback backend progress
    callback progress

{- | Creates a callback that stores session progress using an extra optional session-path.
This is useful in OneShot command where the command-line drives the filename.
-}
filepathStoreCallback :: Maybe FilePath -> OnSessionProgress
filepathStoreCallback Nothing _ = pure ()
filepathStoreCallback (Just path) progress =
    case progress of
        SessionUpdated sess -> go sess
        SessionCompleted sess -> go sess
        SessionStarted sess -> go sess
        SessionFailed sess _ -> go sess
  where
    go sess =
        SessionStore.storeSessionToFile sess path

{- | Wrap an agent to store sessions using a SessionStore.
The session is stored using the conversation ID from the session.

This combinator combines both 'sessionStoreCallback' and 'filepathStoreCallback'
to provide comprehensive session persistence:

1. If the agent has a 'ctxSessionBackend' when it is wrapped, sessions are
   stored via that backend. Prefer 'agentPersistSession' with an explicit sink.
2. Otherwise, sessions are stored to the provided 'SessionStore' (file-based).
3. If a file path is provided, sessions are also stored to that specific file.

The 'ConversationId' is used for the fallback 'SessionStore', while the optional
'FilePath' provides an additional storage location (useful for command-line
specified outputs). The durable backend uses the session's own 'SessionId'.
-}
agentStoreSession :: forall r. SessionStore -> Maybe FilePath -> ConversationId -> Agent r -> Agent r
agentStoreSession store mPath convId agent =
    agentWithSessionProgress handleProgress agent
  where
    handleProgress x = do
        sinkStoreCallback (agentSink store agent) noLabels convId x
        filepathStoreCallback mPath x

-- | The sink 'agentStoreSession' uses: the agent's backend, else the files.
agentSink :: SessionStore -> Agent r -> SessionSink
agentSink store agent = maybe (SinkFiles store) SinkBackend (ctxSessionBackend agent)

{- | Wrap an agent to store sessions and also emit progress events to a callback.

This is a variant of 'agentStoreSession' that invokes a user-supplied progress
callback in addition to durable storage. It is useful when the caller wants to
observe every session update while still benefiting from backend/file persistence.

Storage is performed first, then the user callback is invoked.
-}
agentStoreSessionWithCallback ::
    forall r.
    SessionStore ->
    Maybe FilePath ->
    ConversationId ->
    OnSessionProgress ->
    Agent r ->
    Agent r
agentStoreSessionWithCallback store mPath convId userCallback agent =
    agentWithSessionProgress handleProgress agent
  where
    handleProgress x = do
        sinkStoreCallback (agentSink store agent) noLabels convId x
        filepathStoreCallback mPath x
        userCallback x

-- | Wrap an agent to emit session progress events after each step.
agentWithSessionProgress :: forall r. OnSessionProgress -> Agent r -> Agent r
agentWithSessionProgress onProgress agent =
    agent{step = decorate agent.step}
  where
    decorate :: (Session -> IO (Action r)) -> (Session -> IO (Action r))
    decorate f = \sess -> do
        onProgress (SessionUpdated sess)
        f sess

