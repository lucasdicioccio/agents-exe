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
    wakeSessionWith,
    WakeOutcome (..),

    -- * Find the session waiting for a token
    findSessionForToken,
    sessionHasToken,

    -- * Resume execution
    resumeSession,
) where

import Control.Monad (filterM, forM_)
import qualified Data.Map.Strict as Map

import System.Agents.Base (ConversationId)
import System.Agents.Session.Async (ContinuationStore (..))
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
    (.woSession) <$> wakeSessionWith Nothing mCache session responses

-- | What 'wakeSessionWith' did with each token it was given.
data WakeOutcome = WakeOutcome
    { woSession :: Session
    -- ^ The session with the applied results.
    , woApplied :: [ContinuationToken]
    -- ^ Tokens of deferred calls that received their result.
    , woAlreadyCompleted :: [ContinuationToken]
    -- ^ Tokens of calls that already had a result; theirs is unchanged.
    , woUnknown :: [ContinuationToken]
    -- ^ Tokens this session never issued (as far as it can tell).
    }
    deriving (Show)

{- | Wake a paused session, reporting what happened to each token.

A token is applied when it belongs to a deferred call of the head partial
turn. It is already completed when a call of the session carries it but is
no longer deferred, or when the continuation store knows it for this session
(once a turn is complete, the session itself no longer holds its tokens).

Applied tokens are marked completed in the continuation store, and their
results are stored in the tool cache, when those are given.
-}
wakeSessionWith ::
    Maybe ContinuationStore ->
    Maybe ToolCache ->
    Session ->
    [(ContinuationToken, UserToolResponse)] ->
    IO WakeOutcome
wakeSessionWith mStore mCache session responses = do
    woken <- case getPartialTurn session of
        Nothing -> pure session
        Just partial -> do
            updated <- mapM (wakeTracked responses) (pTrackedToolCalls partial)
            let newTurn = makeTurn partial updated
            pure $ session{turns = newTurn : drop 1 session.turns}
    let pendingTokens = [token | Just partial <- [getPartialTurn session], tc <- partial.pTrackedToolCalls, tc.tcState == Deferred, Just token <- [tc.tcContinuation]]
        applied = [token | (token, _) <- responses, token `elem` pendingTokens]
        others = [token | (token, _) <- responses, token `notElem` pendingTokens]
    known <- filterM isKnown others
    forM_ mStore $ \store ->
        forM_ responses $ \(token, result) ->
            if token `elem` applied then () <$ csComplete store token result else pure ()
    pure
        WakeOutcome
            { woSession = woken
            , woApplied = applied
            , woAlreadyCompleted = known
            , woUnknown = filter (`notElem` known) others
            }
  where
    -- A token not pending in the head turn, but issued by this session.
    isKnown :: ContinuationToken -> IO Bool
    isKnown token
        | sessionHasToken token session = pure True
        | otherwise = case mStore of
            Nothing -> pure False
            Just store -> (== Just session.sessionId) <$> csFindSession store token

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
        | all (isFinalToolCallState . tcState) tracked =
            let content = PartialUserTurnContent (pUserPrompt partial) (pUserTools partial) (pUserQuery partial) tracked (pUserMail partial)
                completed = partialToolMessages content
                byteUsage =
                    calculateUserTurnByteUsage
                        (pUserPrompt partial)
                        (pUserTools partial)
                        (pUserQuery partial)
                        (map snd completed)
             in UserTurn (UserTurnContent (pUserPrompt partial) (pUserTools partial) (pUserQuery partial) completed (pUserMail partial)) (Just byteUsage)
        | otherwise =
            let content = PartialUserTurnContent (pUserPrompt partial) (pUserTools partial) (pUserQuery partial) tracked (pUserMail partial)
                byteUsage = calculatePartialTurnByteUsage (pUserPrompt partial) (pUserTools partial) (pUserQuery partial) tracked
             in PartialUserTurn content (Just byteUsage)

-- | Whether a call in any partial turn of the session carries the token.
sessionHasToken :: ContinuationToken -> Session -> Bool
sessionHasToken token sess =
    or
        [ tc.tcContinuation == Just token
        | PartialUserTurn partial _ <- sess.turns
        , tc <- partial.pTrackedToolCalls
        ]

{- | The session that issued a continuation token.

Asks the continuation store first, which answers without loading any session.
Tokens it does not know (e.g. issued before a store was installed) are
searched in the backend's sessions, loading each in turn.
-}
findSessionForToken :: Maybe ContinuationStore -> SessionBackend -> ContinuationToken -> IO (Maybe SessionId)
findSessionForToken mStore backend token = do
    indexed <- maybe (pure Nothing) (\store -> csFindSession store token) mStore
    case indexed of
        Just sid -> pure (Just sid)
        Nothing -> sbList backend >>= scan . map fst
  where
    scan [] = pure Nothing
    scan (sid : rest) = do
        mSess <- sbLoad backend sid
        if maybe False (sessionHasToken token) mSess
            then pure (Just sid)
            else scan rest

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

