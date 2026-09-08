{-# LANGUAGE OverloadedRecordDot #-}

{- | Wake and resume support for durable async sessions.

This module provides the primitives needed to inject external tool results
into a paused session and to resume execution from a partial or completed
user turn.
-}
module System.Agents.Session.Wake (
    -- * Wake a session with external results
    wakeSession,
    wakeSessionWithCache,

    -- * Resume execution
    resumeSession,
) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map

import System.Agents.Base (ConversationId)
import System.Agents.Session.Base
import System.Agents.Session.Step (
    calculatePartialTurnByteUsage,
    calculateUserTurnByteUsage,
    getPartialTurn,
    runStepM,
 )
import System.Agents.Tools.Cache (CachedResult (..), ToolCache (..), computeCacheKey)
import Data.Time (getCurrentTime)

{- | Wake a paused session by injecting external tool results.

Finds the most recent 'PartialUserTurn', matches deferred
'TrackedToolCall's by their continuation token, and moves them to the
'Completed' state. If all calls in the turn are then complete, the partial
turn is converted into a full 'UserTurn'.

This variant does not update any tool cache. Use 'wakeSessionWithCache'
when a cache is configured.
-}
wakeSession :: Session -> [(ContinuationToken, UserToolResponse)] -> IO Session
wakeSession = wakeSessionWithCache Nothing

{- | Wake a paused session, optionally updating the tool cache.

For every deferred call that receives a result, the result is stored in
the provided cache (if any) under the call's content-derived cache key.
-}
wakeSessionWithCache ::
    Maybe ToolCache ->
    Session ->
    [(ContinuationToken, UserToolResponse)] ->
    IO Session
wakeSessionWithCache mCache session responses =
    case getPartialTurn session of
        Nothing -> pure session
        Just partial -> do
            updated <- mapM (wakeTracked responses) (pTrackedToolCalls partial)
            let newTurn = makeTurn partial updated
            let newTurns = newTurn : drop 1 session.turns
            pure $ session{turns = newTurns}
  where
    wakeTracked :: [(ContinuationToken, UserToolResponse)] -> TrackedToolCall -> IO TrackedToolCall
    wakeTracked _ tc | tc.tcState /= Deferred = pure tc
    wakeTracked rs tc =
        case tc.tcContinuation of
            Nothing -> pure tc
            Just token ->
                case Map.lookup token (Map.fromList rs) of
                    Nothing -> pure tc
                    Just result -> do
                        forM_ mCache $ \cache -> do
                            now <- getCurrentTime
                            cache.cacheStore (computeCacheKey tc.tcCall) $ CachedResult result now Nothing
                        pure $ tc{tcState = Completed, tcResult = Just result}

    makeTurn :: PartialUserTurnContent -> [TrackedToolCall] -> Turn
    makeTurn partial tracked
        | all (\tc -> tc.tcState == Completed) tracked =
            let content = PartialUserTurnContent (pUserPrompt partial) (pUserTools partial) (pUserQuery partial) tracked
                completed = partialCompletedResponses content
                byteUsage =
                    calculateUserTurnByteUsage
                        (pUserPrompt partial)
                        (pUserTools partial)
                        (pUserQuery partial)
                        (map snd completed)
             in UserTurn (UserTurnContent (pUserPrompt partial) (pUserTools partial) (pUserQuery partial) completed) (Just byteUsage)
        | otherwise =
            let content = PartialUserTurnContent (pUserPrompt partial) (pUserTools partial) (pUserQuery partial) tracked
                byteUsage = calculatePartialTurnByteUsage (pUserPrompt partial) (pUserTools partial) (pUserQuery partial) tracked
             in PartialUserTurn content (Just byteUsage)

{- | Resume execution of a session.

Runs the agent step-by-step until either:

* The session completes and a result @r@ is returned ('Left r').
* The scheduler yields because there are deferred calls pending
  ('Right Session').

If the latest turn is a 'PartialUserTurn', the scheduler is re-run on the
remaining ready/deferred calls. If the latest turn is a 'UserTurn',
execution continues to the LLM step.
-}
resumeSession :: ConversationId -> Agent r -> Session -> IO (Either r Session)
resumeSession convId agent sess = go agent sess
  where
    go :: Agent r -> Session -> IO (Either r Session)
    go a s = do
        (a', res) <- runStepM convId a s
        case res of
            Left r -> pure $ Left r
            Right s' ->
                case getPartialTurn s' of
                    Just _ -> pure $ Right s'
                    Nothing -> go a' s'

