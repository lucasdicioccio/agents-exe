{-# LANGUAGE ScopedTypeVariables #-}

{- | Session storage and progress tracking combinators for agents.

This module provides combinators for wrapping agents with session persistence
and progress tracking capabilities. The primary functions are:

* 'agentStoreSession' - wraps an agent to persist sessions to a 'SessionStore'
* 'agentWithSessionProgress' - wraps an agent to emit progress events after each step

Example usage:

@
import System.Agents.Combinators.StoreSessionProgress (agentStoreSession, agentWithSessionProgress)

-- Wrap an agent with session storage
storedAgent <- agentStoreSession store Nothing convId baseAgent

-- Or wrap with custom progress handling
progressAgent <- agentWithSessionProgress myProgressCallback baseAgent
@
-}
module System.Agents.Combinators.StoreSessionProgress (
    -- * Session Storage Combinators
    agentStoreSession,
    agentWithSessionProgress,

    -- * Callback Utilities
    sessionStoreCallback,
    filepathStoreCallback,
) where

import System.Agents.Base (ConversationId)
import System.Agents.Session.Base (
    Action,
    Agent (..),
    OnSessionProgress,
    Session,
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

1. Sessions are stored to the 'SessionStore' (typically a directory with auto-naming)
2. If a file path is provided, sessions are also stored to that specific file

The 'ConversationId' is used for the SessionStore, while the optional 'FilePath'
provides an additional storage location (useful for command-line specified outputs).
-}
agentStoreSession :: forall r. SessionStore -> Maybe FilePath -> ConversationId -> Agent r -> Agent r
agentStoreSession store mPath convId agent =
    agentWithSessionProgress handleProgress agent
  where
    handleProgress x = do
        sessionStoreCallback store convId x
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

