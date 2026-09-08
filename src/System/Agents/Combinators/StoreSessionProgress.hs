{-# LANGUAGE ScopedTypeVariables #-}

{- | Session storage and progress tracking combinators for agents.

This module provides combinators for wrapping agents with session persistence
and progress tracking capabilities. The primary functions are:

* 'agentStoreSession' - wraps an agent to persist sessions to a 'SessionStore'
* 'agentWithSessionProgress' - wraps an agent to emit progress events after each step

When the agent has a 'ctxSessionBackend' configured, 'agentStoreSession' uses that
backend as the primary store and falls back to the provided file-based
'SessionStore' when no backend is configured. The optional explicit 'FilePath'
always receives an additional copy.

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
@
-}
module System.Agents.Combinators.StoreSessionProgress (
    -- * Session Storage Combinators
    agentStoreSession,
    agentWithSessionProgress,

    -- * Callback Utilities
    sessionStoreCallback,
    backendStoreCallback,
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
import System.Agents.SessionStore (SessionStore)
import qualified System.Agents.SessionStore as SessionStore

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
backendStoreCallback backend progress =
    case progress of
        SessionUpdated sess -> storeSessionWithBackend sess
        SessionCompleted sess -> storeSessionWithBackend sess
        SessionStarted sess -> storeSessionWithBackend sess
        SessionFailed sess _ -> storeSessionWithBackend sess
  where
    storeSessionWithBackend sess =
        sbStore backend sess.sessionId sess

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

1. If the agent has a 'ctxSessionBackend', sessions are stored via that backend.
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
        case ctxSessionBackend agent of
            Just backend -> backendStoreCallback backend x
            Nothing -> sessionStoreCallback store convId x
        filepathStoreCallback mPath x

-- | Wrap an agent to emit session progress events after each step.
agentWithSessionProgress :: forall r. OnSessionProgress -> Agent r -> Agent r
agentWithSessionProgress onProgress agent =
    agent{step = decorate agent.step}
  where
    decorate :: (Session -> IO (Action r)) -> (Session -> IO (Action r))
    decorate f = \sess -> do
        onProgress (SessionUpdated sess)
        f sess

