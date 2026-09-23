{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Session execution loop with support for both synchronous and asynchronous modes.

This module provides the main 'run' function for executing agents, as well as
async-aware variants that support pausing and resuming sessions.

Usage (synchronous):

@
result <- run convId agent session
@

Usage (asynchronous with pause/resume):

@
-- Start or resume a session
result <- runAsync convId agent session

case result of
    Left finalResult -> pure finalResult  -- Session completed
    Right pausedSession -> do
        -- Session paused - save and resume later
        saveSession pausedSession
        -- Later, on potentially different machine:
        resumedSession <- loadSession sessionId
        result2 <- runAsync convId agent resumedSession
@
-}
module System.Agents.Session.Loop (
    -- * Synchronous execution
    run,
    runUntilBlocked,
    isBlockedOnDeferredCalls,

    -- * Asynchronous execution with pause/resume
    runAsync,
    runWithProgress,
    runAsyncWithProgress,

    -- * Session state inspection
    isSessionComplete,
    getSessionStatus,

    -- * Re-exports for convenience
    module System.Agents.Session.Step,
) where

import Control.Exception (onException)
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import System.Agents.Base (ConversationId)
import System.Agents.Session.Async.Engine (shutdownAsyncEngine)
import System.Agents.Session.Base
import System.Agents.Session.Step

-------------------------------------------------------------------------------
-- Synchronous Execution
-------------------------------------------------------------------------------

{- | Keeps running an agent until it stops (synchronous mode).

This is the traditional execution mode where all tool calls complete
immediately and the session runs to completion without pausing.
-}
run :: forall r. ConversationId -> Agent r -> Session -> IO r
run convId agent sess =
    withEngineShutdown agent $ \latest ->
        go latest agent sess
  where
    go :: IORef (Agent r) -> Agent r -> Session -> IO r
    go latest agent0 sess0 = do
        (agent1, res) <- runStepM convId agent0 sess0
        writeIORef latest agent1
        case res of
            Left r -> pure r
            Right sess1 -> go latest agent1 sess1

{- | Run an action that tracks the evolving agent, then cancel whatever the
agent's async engine is still running.

Steps install the engine on the agent they return, so the last agent seen is
the one holding the engine. Background calls are only useful while the agent
runs, so they (and their subprocesses) are cancelled when it is done or fails.
-}
withEngineShutdown :: Agent r -> (IORef (Agent r) -> IO a) -> IO a
withEngineShutdown agent0 action = do
    latest <- newIORef agent0
    let shutdown = do
            agent <- readIORef latest
            mapM_ shutdownAsyncEngine agent.ctxAsyncEngine
    result <- action latest `onException` shutdown
    shutdown
    pure result

{- | Like 'run', but returns the session instead of looping when nothing can
progress in this process: the head partial turn only waits on deferred calls
(see 'isBlockedOnDeferredCalls'), which an external worker must complete, or
a 'StopRun' or 'Pause' 'Control' message has arrived on the agent's mailbox.

Background calls running in this process are waited for, since they cannot
outlive it -- unless the 'Control' mail that stopped this loop was itself a
'CancelCalls'/'CancelAllAttached', which kills them outright through the
agent's async engine before returning (mirroring
'System.Agents.Host.Runner''s run loop, the other driver of this same
'applyControlMail'/'cancelAttachedCall' pair, so a front-end with no
'SessionRunner' of its own -- e.g. the TUI -- still reacts to the same
'Control' mail the same way).
-}
runUntilBlocked :: forall r. ConversationId -> Agent r -> Session -> IO (Either r Session)
runUntilBlocked convId agent sess =
    withEngineShutdown agent $ \latest -> go latest agent sess
  where
    go :: IORef (Agent r) -> Agent r -> Session -> IO (Either r Session)
    go latest agent0 sess0
        | isBlockedOnDeferredCalls sess0 = pure (Right sess0)
        | otherwise = do
            (sess0', controls) <- applyControlMail agent0 sess0
            mapM_ (cancelAttachedCall agent0) [cid | CancelCalls ids <- controls, cid <- ids]
            when (CancelAllAttached `elem` controls) $
                mapM_ (cancelAttachedCall agent0) (runningToolCallIds sess0')
            if StopRun `elem` controls || Pause `elem` controls
                then pure (Right sess0')
                else do
                    (agent1, res) <- runStepM convId agent0 sess0'
                    writeIORef latest agent1
                    case res of
                        Left r -> pure (Left r)
                        Right sess1 -> go latest agent1 sess1

-------------------------------------------------------------------------------
-- Asynchronous Execution with Progress Callbacks
-------------------------------------------------------------------------------

{- | Run an agent with progress callbacks.

Calls the provided callback after each step with the current session state.
This allows external code to track session progress, save intermediate
states, or implement custom pause logic.
-}
runWithProgress ::
    forall r.
    ConversationId ->
    Agent r ->
    Session ->
    OnSessionProgress ->
    IO r
runWithProgress convId agent sess onProgress = do
    onProgress $ SessionStarted sess
    withEngineShutdown agent $ \latest -> go latest agent sess
  where
    go :: IORef (Agent r) -> Agent r -> Session -> IO r
    go latest agent0 sess0 = do
        (agent1, res) <- runStepM convId agent0 sess0
        writeIORef latest agent1
        case res of
            Left r -> do
                onProgress $ SessionCompleted sess0
                pure r
            Right sess1 -> do
                onProgress $ SessionUpdated sess1
                go latest agent1 sess1

{- | Run an agent in async mode with progress callbacks.

Similar to 'runWithProgress' but designed for async execution where
sessions may pause after partial tool execution. The callback receives
progress updates and can be used to persist session state for later
resumption.

In async mode, the session may yield after each tool call, allowing
external systems to process results before continuing.
-}
runAsyncWithProgress ::
    forall r.
    ConversationId ->
    Agent r ->
    Session ->
    OnSessionProgress ->
    IO (Either r Session)
runAsyncWithProgress convId agent sess onProgress = do
    onProgress $ SessionStarted sess
    -- Only cancel on failure: pausing with calls still running is the point of
    -- this loop, and they are picked up again on resume.
    latest <- newIORef agent
    let shutdown = readIORef latest >>= \a -> mapM_ shutdownAsyncEngine a.ctxAsyncEngine
    go latest agent sess `onException` shutdown
  where
    go :: IORef (Agent r) -> Agent r -> Session -> IO (Either r Session)
    go latest agent0 sess0 = do
        (agent1, res) <- runStepM convId agent0 sess0
        writeIORef latest agent1
        case res of
            Left r -> do
                onProgress $ SessionCompleted sess0
                pure $ Left r
            Right sess1 -> do
                onProgress $ SessionUpdated sess1
                -- In async mode, check if we should pause
                case agent1.ctxExecutionMode of
                    Asynchronous ->
                        -- Check if there's a partial turn (indicating pause)
                        case getPartialTurn sess1 of
                            Just _ -> pure $ Right sess1
                            Nothing -> go latest agent1 sess1
                    Synchronous -> go latest agent1 sess1

-------------------------------------------------------------------------------
-- Asynchronous Execution
-------------------------------------------------------------------------------

{- | Run an agent in async mode.

Executes the agent step by step. Returns either:
* 'Left r' - The session completed with result 'r'
* 'Right Session' - The session paused (has a partial turn)

This allows sessions to be saved and resumed later, potentially on
different machines.

The async engine created during the run is not returned. When resuming in
the same process, install one with 'withAsyncEngine' beforehand so calls
started before the pause can still be cancelled. Calls whose process is
gone are resolved as orphaned on resume.

Example:

@
-- Create async agent
let asyncAgent = (withExecutionMode Asynchronous baseAgent)
            { ctxToolCache = Just cache }

-- Run session
result <- runAsync convId asyncAgent session

case result of
    Left finalResult -> handleCompletion finalResult
    Right pausedSession -> do
        -- Persist the paused session
        saveSession pausedSession
        -- Resume later
        loadedSession <- loadSession sessionId
        result2 <- runAsync convId asyncAgent loadedSession
@
-}
runAsync ::
    forall r.
    ConversationId ->
    Agent r ->
    Session ->
    IO (Either r Session)
runAsync convId agent sess =
    runAsyncWithProgress convId agent sess ignoreSessionProgress

-------------------------------------------------------------------------------
-- Session State Inspection
-------------------------------------------------------------------------------

{- | Check if a session has completed (no partial turns pending).

Returns True if the session is complete and can be discarded,
False if there are pending tool calls that need to be executed.
-}
isSessionComplete :: Session -> Bool
isSessionComplete session =
    case getPartialTurn session of
        Nothing -> True
        Just _ -> False

{- | Get the current status of a session.

Returns a description of the session state for debugging/display.
-}
getSessionStatus :: Session -> String
getSessionStatus session =
    case getPartialTurn session of
        Nothing ->
            "Complete - " ++ show (length session.turns) ++ " turns"
        Just partial ->
            let count st = length [() | tc <- partial.pTrackedToolCalls, tc.tcState == st]
             in "Partial - "
                    ++ show (count Completed)
                    ++ " completed, "
                    ++ show (count Failed)
                    ++ " failed, "
                    ++ show (count Running)
                    ++ " running, "
                    ++ show (count Ready)
                    ++ " pending, "
                    ++ show (count Deferred)
                    ++ " deferred"
