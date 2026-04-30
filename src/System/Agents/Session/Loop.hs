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

import System.Agents.Base (ConversationId)
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
    go agent sess
  where
    go :: Agent r -> Session -> IO r
    go agent0 sess0 = do
        (agent1, res) <- runStepM convId agent0 sess0
        case res of
            Left r -> pure r
            Right sess1 -> go agent1 sess1

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
    go agent sess
  where
    go :: Agent r -> Session -> IO r
    go agent0 sess0 = do
        (agent1, res) <- runStepM convId agent0 sess0
        case res of
            Left r -> do
                onProgress $ SessionCompleted sess0
                pure r
            Right sess1 -> do
                onProgress $ SessionUpdated sess1
                go agent1 sess1

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
    go agent sess
  where
    go :: Agent r -> Session -> IO (Either r Session)
    go agent0 sess0 = do
        (agent1, res) <- runStepM convId agent0 sess0
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
                            Nothing -> go agent1 sess1
                    Synchronous -> go agent1 sess1

-------------------------------------------------------------------------------
-- Asynchronous Execution
-------------------------------------------------------------------------------

{- | Run an agent in async mode.

Executes the agent step by step. Returns either:
* 'Left r' - The session completed with result 'r'
* 'Right Session' - The session paused (has a partial turn)

This allows sessions to be saved and resumed later, potentially on
different machines.

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
            "Partial - "
                ++ show (length partial.pCompletedResponses)
                ++ " completed, "
                ++ show (length partial.pPendingCalls)
                ++ " pending"

