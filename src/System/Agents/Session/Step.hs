{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: propose some non-naive combinators
module System.Agents.Session.Step where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, TVar, atomically, orElse, registerDelay, readTVar, retry)
import Control.Monad (filterM, forM, forM_, unless, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time
import qualified System.Timeout

import Data.Void (Void)

import System.Agents.Base (ConversationId)
import qualified System.Agents.OS.Conversation as OSConv
import qualified System.Agents.OS.Conversation.ToolCalls as TCT
import System.Agents.OS.Core.Types (EntityId)
import System.Agents.OS.Core.World (World, getComponent, newWorld)
import System.Agents.Session.Async (ContinuationStore (..), mkToolContinuationSnapshot, storeContinuation)
import System.Agents.Session.Async.Engine (
    mkAsyncEngine,
    startAsyncBatch,
 )
import qualified System.Agents.Session.Async.Engine as Engine
import System.Agents.Session.Base
import System.Agents.Session.Compat (parseToolCallFromLlmToolCall)
import System.Agents.Media.Types (ContentPart (..), MediaAttachment (..))
import System.Agents.ToolSchema (ParamProperty)
import qualified System.Agents.Tools.Bindings as Bindings
import System.Agents.Tools.Cache (CachedResult (..), ToolCache (..))
import qualified System.Agents.Tools.Cache as Cache
import System.Agents.Tools.Context (ToolExecutionContext, ToolCall (..), mkToolExecutionContext)
import qualified System.Agents.Tools.Context as Ctx

{- | Runs a single step of agent for a given session (synchronous mode).
Agent may be modified, may decide to return a session, or decide to stop.
-}
runStepM :: forall r. ConversationId -> Agent r -> Session -> IO (Agent r, Either r Session)
runStepM convId agent sess =
    case agent.ctxExecutionMode of
        Asynchronous -> runStepMAsync convId agent sess
        Synchronous -> runStepMSync convId agent sess

{- | Synchronous step execution - all tools execute immediately.

This is the traditional execution mode where all tool calls in a turn
are executed immediately and the session continues without pausing.

When the agent has an OS 'World', every tool call is promoted to an ECS
entity and its lifecycle state is updated as it starts and completes.

If the session holds background calls from an earlier asynchronous step,
their results are delivered with the next user turn (see
'collectLateResults').
-}
runStepMSync :: forall r. ConversationId -> Agent r -> Session -> IO (Agent r, Either r Session)
runStepMSync convId agent sess =
    go agent sess
  where
    addTurn :: Session -> Turn -> IO Session
    addTurn sess0 turn = do
        tId <- newTurnId
        pure $ sess0{turns = turn : sess0.turns, turnId = tId}

    go :: Agent r -> Session -> IO (Agent r, Either r Session)
    go agent0 sess0 = do
        agent0' <- prepareAgentWorld agent0
        next <- agent0'.step sess0
        case next of
            Stop r -> pure (agent0', Left r)
            Evolve agent1 -> pure (agent1{ctxWorld = agent0'.ctxWorld}, Right sess0)
            AskUserPrompt missing -> do
                sPrompt <- agent0'.sysPrompt
                sTools <- agent0'.sysTools
                -- Construct ToolExecutionContext for each tool call
                let ctx = buildContext agent0' sess0 convId
                (uQuery0, blockForLate) <- askUserQuery ctx agent0' missing sess0
                (sessLate, late) <- collectLateResults ctx agent0'.ctxAsyncYieldStrategy blockForLate sess0
                let shouldBlockForMail = blockForLate && null late && null missing.missingToolCalls
                (sessMail, mailEnvelopes) <- receiveMailForTurn agent0' shouldBlockForMail sessLate
                let uQuery = mergeUserQueries (mergeUserQueries uQuery0 (lateResultsQuery late)) (mailQuery mailEnvelopes)
                -- Execute tool calls with optional OS entity tracking
                tracked <- traverse (mkReadyTrackedCall ctx agent0') missing.missingToolCalls
                trackedWithEntities <- ensureTrackedCallEntities ctx tracked
                resultsWithTracked <- traverse (executeTrackedCallWithEntity agent0' ctx) trackedWithEntities
                let toolResponses = map fst resultsWithTracked
                let uToolResponses = zip missing.missingToolCalls toolResponses
                -- Calculate byte usage for this user turn
                let byteUsage = calculateUserTurnByteUsage sPrompt sTools uQuery toolResponses
                sess1 <- addTurn sessMail (UserTurn (UserTurnContent{userPrompt = sPrompt, userTools = sTools, userQuery = uQuery, userToolResponses = uToolResponses, userMail = mailEnvelopes}) (Just byteUsage))
                pure (agent0', Right sess1)
            AskLlmCompletion completion -> do
                outcome <- askForCompletionOrInterrupt agent0' sess0 completion
                case outcome of
                    Left (llmRsp, llmTool) -> do
                        -- Calculate byte usage for this LLM turn, including token usage
                        let byteUsage = calculateLlmTurnByteUsage llmRsp llmTool
                        sess1 <- addTurn sess0 (LlmTurn (LlmTurnContent llmRsp llmTool) (Just byteUsage))
                        pure (agent0', Right sess1)
                    Right envelopes ->
                        -- R4: the completion was cancelled by an 'Interrupt'
                        -- envelope; amend the head turn and ask again.
                        go agent0' (amendHeadTurnWithMail envelopes sess0)

-------------------------------------------------------------------------------
-- Asynchronous Step Execution
-------------------------------------------------------------------------------

{- | Asynchronous step execution - supports partial tool execution.

In async mode:
1. Tool calls are tracked individually through their lifecycle state.
2. The configured 'ToolCallPolicy' classifies each call as sync, async,
   isolated, or deferred.
3. 'RunAsync' calls are handed to the concurrent async engine first, then
   'RunSync' and 'RunIsolated' calls run inline while the async calls
   progress in the background.
4. Depending on the 'AsyncYieldStrategy', the step may yield a
   'PartialUserTurn' before all async calls finish. When the LLM is asked
   for a completion from such a turn, still-running calls get a placeholder
   tool message (see 'partialToolMessages').
5. 'Defer' calls are moved to the 'Deferred' state and a continuation token
   is generated.
6. If all calls reach a final state, the turn is finalized; otherwise a
   'PartialUserTurn' is yielded.
7. Results of calls that finish after the LLM saw their placeholder are
   delivered with the next user turn (see 'collectLateResults').

Before each step, the head partial turn (if any) is refreshed from the OS
world so calls that finished in the background, or whose entity no longer
exists, are resolved.

When the agent has an OS 'World', every tracked call is promoted to an ECS
entity and its lifecycle state is kept in sync. A single async engine is
installed on the agent and returned so later steps share it.
-}
runStepMAsync :: forall r. ConversationId -> Agent r -> Session -> IO (Agent r, Either r Session)
runStepMAsync convId agent sess =
    go agent sess
  where
    addTurn :: Session -> Turn -> IO Session
    addTurn sess0 turn = do
        tId <- newTurnId
        pure $ sess0{turns = turn : sess0.turns, turnId = tId}

    go :: Agent r -> Session -> IO (Agent r, Either r Session)
    go agent0 sess0 = do
        agent0' <- prepareAsyncEngine =<< prepareAgentWorld agent0
        sessR <- refreshHeadPartialTurn (buildContext agent0' sess0 convId) sess0
        next <- agent0'.step sessR
        case next of
            Stop r -> pure (agent0', Left r)
            Evolve agent1 ->
                pure
                    ( agent1
                        { ctxWorld = agent0'.ctxWorld
                        , ctxAsyncEngine = agent1.ctxAsyncEngine <|> agent0'.ctxAsyncEngine
                        }
                    , Right sessR
                    )
            AskUserPrompt missing -> do
                -- Check if there's a partial turn to continue
                case getPartialTurn sessR of
                    Just partial -> do
                        -- Continue execution of partial turn
                        continuePartialTurn convId agent0' sessR partial
                    Nothing -> do
                        -- Start new async execution
                        sPrompt <- agent0'.sysPrompt
                        sTools <- agent0'.sysTools
                        (uQuery, blockForLate) <- askUserQuery (buildContext agent0' sessR convId) agent0' missing sessR
                        startNewAsyncTurn convId agent0' sessR sPrompt sTools uQuery blockForLate missing.missingToolCalls
            AskLlmCompletion completion -> do
                outcome <- askForCompletionOrInterrupt agent0' sessR completion
                case outcome of
                    Left (llmRsp, llmTool) -> do
                        let byteUsage = calculateLlmTurnByteUsage llmRsp llmTool
                        sess1 <- addTurn sessR (LlmTurn (LlmTurnContent llmRsp llmTool) (Just byteUsage))
                        pure (agent0', Right sess1)
                    Right envelopes ->
                        -- R4: the completion was cancelled by an 'Interrupt'
                        -- envelope; amend the head turn and ask again.
                        go agent0' (amendHeadTurnWithMail envelopes sessR)

{- | Get the most recent partial turn from a session, if any.

Looks at the most recent turn and returns it if it's a PartialUserTurn.
-}
getPartialTurn :: Session -> Maybe PartialUserTurnContent
getPartialTurn session =
    case session.turns of
        (PartialUserTurn content _ : _) -> Just content
        _ -> Nothing

{- | Start a new async user turn.

Delivers the results of background calls from earlier turns, wraps all
pending tool calls as 'Ready' tracked calls, classifies them according to
the agent's policy, executes them, and yields any deferred or still-running
calls.
-}
startNewAsyncTurn ::
    forall r.
    ConversationId ->
    Agent r ->
    Session ->
    SystemPrompt ->
    [SystemTool] ->
    Maybe UserQuery ->
    Bool ->
    [LlmToolCall] ->
    IO (Agent r, Either r Session)
startNewAsyncTurn convId agent sess sPrompt sTools uQuery blockForLate calls = do
    let ctx = buildContext agent sess convId
    (sessLate, late) <- collectLateResults ctx agent.ctxAsyncYieldStrategy blockForLate sess
    let shouldBlockForMail = blockForLate && null late && null calls
    (sessMail, mailEnvelopes) <- receiveMailForTurn agent shouldBlockForMail sessLate
    tracked <- traverse (mkReadyTrackedCall ctx agent) calls
    trackedWithEntities <- ensureTrackedCallEntities ctx tracked
    let uQuery' = mergeUserQueries (mergeUserQueries uQuery (lateResultsQuery late)) (mailQuery mailEnvelopes)
    executeTrackedCalls ctx agent sessMail False sPrompt sTools uQuery' mailEnvelopes trackedWithEntities

{- | Continue execution of a partial turn.

Takes the existing tracked calls, polls any calls that were previously
started by the async engine, re-runs the scheduler for remaining 'Ready'
calls, waits on in-flight calls according to the yield strategy, and
replaces the head partial turn with the result.
-}
continuePartialTurn ::
    forall r.
    ConversationId ->
    Agent r ->
    Session ->
    PartialUserTurnContent ->
    IO (Agent r, Either r Session)
continuePartialTurn convId agent sess partial = do
    let ctx = buildContext agent sess convId
    -- Poll running calls from a previous step so completed results are
    -- copied back into the tracked calls before re-scheduling.
    polled <- mapM (pollRunningCall ctx) partial.pTrackedToolCalls
    -- Mail already folded into this partial turn (see 'pUserMail') carries
    -- forward unchanged; a fresh receive happens once a new turn starts.
    executeTrackedCalls ctx agent sess True partial.pUserPrompt partial.pUserTools partial.pUserQuery partial.pUserMail polled

{- | Execute the scheduler over a list of tracked calls.

* 'Ready' calls classified as 'RunAsync' are started in the async engine
  first so they overlap with the inline calls.
* 'Ready' calls classified as 'RunSync' (or 'RunIsolated') are executed
  immediately, with an optional cache lookup first. Async calls that cannot
  be tracked (no OS world or no entity) also run inline.
* 'Ready' calls classified as 'Defer' are moved to the 'Deferred' state and
  a continuation snapshot is stored if a store is configured.
* Calls already in 'Running', 'Deferred', 'Completed', or 'Failed' are left
  untouched, but running calls are waited on together with the new async
  calls according to the 'AsyncYieldStrategy'.
* If every call ends up 'Completed' or 'Failed', a full 'UserTurn' is emitted.
* Otherwise a 'PartialUserTurn' is emitted, allowing the session to be
  resumed later.

When @replaceHead@ is set, the emitted turn replaces the head turn (the
partial turn being continued) instead of being pushed on top of it.
-}
executeTrackedCalls ::
    forall r.
    ToolExecutionContext ->
    Agent r ->
    Session ->
    Bool ->
    SystemPrompt ->
    [SystemTool] ->
    Maybe UserQuery ->
    [Envelope] ->
    [TrackedToolCall] ->
    IO (Agent r, Either r Session)
executeTrackedCalls ctx agent sess replaceHead sPrompt sTools uQuery mailEnvelopes tracked = do
    -- Ensure every call has an OS entity when a world is available.
    trackedWithEntities <- ensureTrackedCallEntities ctx tracked

    -- Classify each ready call and leave non-ready calls untouched.
    classified <- mapM (classifyTrackedCall ctx agent) trackedWithEntities

    let (readyItems, finalCalls) = partitionReady classified

    -- Every non-deferred ready call is handed to the engine (D4: the
    -- disposition controls how long the step stays attached, not where the
    -- call runs); only calls that couldn't be tracked with an OS entity
    -- fall back to inline execution.
    let (engineItems, deferItems) = partitionForExecution readyItems

    (engineStarted, engineFallback) <- startAsyncCalls ctx agent engineItems

    -- Execute the calls that could not be tracked with an OS entity.
    inlineProcessed <- mapM (executeInlineTrackedCall agent ctx) engineFallback

    -- Defer calls that asked to be paused.
    deferProcessed <- mapM (deferTrackedCall ctx agent) deferItems

    -- R3a: wait on everything in flight (including calls started in earlier
    -- steps), attached per each call's own disposition, detachable at once
    -- by an 'Interrupt' envelope.
    let inFlight = [tc | tc <- engineStarted ++ finalCalls, tcState tc == Running]
    attachOutcome <- waitAttachedCalls ctx agent.ctxMailbox sess.mailCursor agent.ctxAsyncYieldStrategy inFlight

    -- Reassemble in the original order using a map keyed by call id.
    let processedMap =
            Map.fromList
                [ (tcId tc, tc)
                | tc <- inlineProcessed ++ engineStarted ++ deferProcessed ++ finalCalls
                ]
    processedPolled <-
        mapM
            (pollRunningCall ctx . (\tc -> Map.findWithDefault tc (tcId tc) processedMap))
            trackedWithEntities
    let processedOrdered = map (applyAttachOutcome attachOutcome) processedPolled

    -- An interrupt detaches every attached call at once and is folded into
    -- this same turn (R1's mail-rendering, reused rather than duplicated),
    -- so the LLM sees both the placeholders and why they appeared ("^Z then
    -- talk"). A non-interrupt outcome (a deadline, or the yield strategy)
    -- leaves mail for the next ordinary receive point.
    (sess', uQuery', mailEnvelopes') <-
        if attachOutcome.awoInterrupted
            then do
                (sessMail, moreMail) <- receiveMailForTurn agent False sess
                pure (sessMail, mergeUserQueries uQuery (mailQuery moreMail), mailEnvelopes ++ moreMail)
            else pure (sess, uQuery, mailEnvelopes)

    -- Decide whether we can emit a full user turn or need a partial one.
    let content = PartialUserTurnContent sPrompt sTools uQuery' processedOrdered mailEnvelopes'
    if all (isFinalToolCallState . tcState) processedOrdered
        then do
            let responses = partialToolMessages content
            let byteUsage = calculateUserTurnByteUsage sPrompt sTools uQuery' (map snd responses)
            sess'' <- pushTurn sess' (UserTurn (UserTurnContent sPrompt sTools uQuery' responses mailEnvelopes') (Just byteUsage))
            pure (agent, Right sess'')
        else do
            let byteUsage = calculatePartialTurnByteUsage sPrompt sTools uQuery' processedOrdered
            sess'' <- pushTurn sess' (PartialUserTurn content (Just byteUsage))
            pure (agent, Right sess'')
  where
    pushTurn s t = do
        tId <- newTurnId
        let rest = if replaceHead then drop 1 s.turns else s.turns
        pure $ s{turns = t : rest, turnId = tId}

{- | Classify a single tracked call.

Non-'Ready' calls are returned as already-final. 'Ready' calls are
classified using the agent's policy and tagged with their base disposition
and decorators.
-}
classifyTrackedCall ::
    forall r.
    ToolExecutionContext ->
    Agent r ->
    TrackedToolCall ->
    IO (Either TrackedToolCall ClassifiedCall)
classifyTrackedCall _ctx _agent tc
    | tcState tc /= Ready = pure $ Left tc
classifyTrackedCall ctx agent tc = do
    let disp = agent.ctxToolCallPolicy ctx tc.tcCall
    let (decorators, base) = flattenDisposition disp
    pure $ Right $ ClassifiedCall tc base decorators disp

{- | A ready call together with its classified disposition.
-}
data ClassifiedCall = ClassifiedCall
    { ccCall :: TrackedToolCall
    , ccBase :: ToolCallDisposition
    , ccDecorators :: [Decorator]
    , ccFullDisposition :: ToolCallDisposition
    }

partitionReady :: [Either TrackedToolCall ClassifiedCall] -> ([ClassifiedCall], [TrackedToolCall])
partitionReady = foldr go ([], [])
  where
    go (Left tc) (ready, final) = (ready, tc : final)
    go (Right c) (ready, final) = (c : ready, final)

{- | Split classified, ready calls into those to hand to the engine and
those to defer.

D4 (@todos/session-mailbox.md@): in asynchronous mode every disposition
except 'Defer' now goes through the engine — the disposition controls how
long the step stays *attached* to a call (see 'waitAttachedCalls'), not
*where* it runs. Only a call with no OS entity ends up running inline
('startAsyncCalls' falls it back), which preserves the pre-Phase-2 fallback
for calls that could not be tracked.
-}
partitionForExecution :: [ClassifiedCall] -> ([ClassifiedCall], [ClassifiedCall])
partitionForExecution = foldr go ([], [])
  where
    go c (engine, defer)
        | isDeferDisposition (ccBase c) = (engine, c : defer)
        | otherwise = (c : engine, defer)

isDeferDisposition :: ToolCallDisposition -> Bool
isDeferDisposition (Defer _) = True
isDeferDisposition _ = False

{- | Execute a classified inline call and mark it completed.
-}
executeInlineTrackedCall ::
    forall r.
    Agent r ->
    ToolExecutionContext ->
    ClassifiedCall ->
    IO TrackedToolCall
executeInlineTrackedCall agent ctx c = do
    let tc = ccCall c
    (result, tc') <- executeTrackedCallWithEntity agent ctx tc
    pure $ tc'{tcState = Completed, tcResult = Just result, tcPolicy = AppliedPolicy (ccFullDisposition c) Nothing}

{- | Start a batch of engine calls in the background (D4: every non-deferred
disposition, not only 'RunAsync').

Returns the calls that were started (in the 'Running' state) and the calls
that must run inline instead. Calls fall back to inline execution when the
agent has no OS 'World' or async engine, or when the call has no OS entity
(e.g. its payload could not be parsed), so that the step remains total.

A call classified @'RunAsync' ('Just' n)@ gets its 'tcAttachDeadline' fixed
here, at start time, so it survives across however many steps a
'PartialUserTurn' is resumed over (see 'waitAttachedCalls').
-}
startAsyncCalls ::
    forall r.
    ToolExecutionContext ->
    Agent r ->
    [ClassifiedCall] ->
    IO ([TrackedToolCall], [ClassifiedCall])
startAsyncCalls _ctx _agent [] = pure ([], [])
startAsyncCalls ctx agent engineItems =
    case (Ctx.ctxWorld ctx, agent.ctxAsyncEngine) of
        (Just _world, Just engine) -> do
            let (trackable, untrackable) = partition (isJust . tcEntityId . ccCall) engineItems
            unless (null trackable) $ do
                _batch <- startAsyncBatch engine ctx (map ccCall trackable)
                pure ()
            now <- getCurrentTime
            let running =
                    [ (ccCall c)
                        { tcState = Running
                        , tcPolicy = AppliedPolicy (ccFullDisposition c) Nothing
                        , tcAttachDeadline = attachDeadline now (ccBase c)
                        }
                    | c <- trackable
                    ]
            pure (running, untrackable)
        _ -> pure ([], engineItems)
  where
    attachDeadline now base = case base of
        RunAsync (Just secs) -> Just (Data.Time.addUTCTime (fromIntegral secs) now)
        _ -> Nothing

{- | Outcome of the R3a wait (see 'waitAttachedCalls'): which calls, if any,
should now be marked detached, and why.
-}
data AttachWaitOutcome = AttachWaitOutcome
    { awoDeadlineElapsed :: [ToolCallId]
    -- ^ Calls whose 'tcAttachDeadline' passed during this wait.
    , awoInterrupted :: Bool
    -- ^ An 'Interrupt' envelope pre-empted the wait: every call still
    -- attached (any bucket) should be detached at once ("^Z then talk").
    }

{- | R3a (@todos/session-mailbox.md@ §2, §4): "attached calls vs.
interrupts, one transaction". Blocks until the first of:

* an unread 'Interrupt'-priority envelope: every attached call detaches at
  once, in the same transaction (peeked, not consumed — the caller folds it
  into the turn being built via 'receiveMailForTurn'/R1, reused rather than
  duplicated);
* a call's 'tcAttachDeadline' elapses (one 'orElse' arm per distinct
  deadline, via 'registerDelay'): only that call detaches, the rest keep
  waiting;
* the ordinary 'AsyncYieldStrategy' condition holds for calls with no
  explicit deadline (unchanged pre-Phase-2 behaviour for those calls).

Calls attached "forever" ('RunSync', 'RunIsolated', i.e. no
'tcAttachDeadline' and not @'RunAsync' 'Nothing'@) are only released by
finishing or by an interrupt — never by the yield strategy or a deadline,
per D4.

No OS world, or nothing 'Running', returns immediately with an empty,
non-interrupted outcome.
-}
waitAttachedCalls ::
    ToolExecutionContext ->
    Maybe Mailbox ->
    Cursor ->
    AsyncYieldStrategy ->
    [TrackedToolCall] ->
    IO AttachWaitOutcome
waitAttachedCalls ctx mMailbox cursor strategy calls =
    case Ctx.ctxWorld ctx of
        Nothing -> pure noOutcome
        Just world
            | null running -> pure noOutcome
            | otherwise -> do
                now <- getCurrentTime
                deadlineVars <- forM deadlineCalls $ \tc -> do
                    let deadline = fromMaybe now tc.tcAttachDeadline
                        micros = max 0 (round (realToFrac (Data.Time.diffUTCTime deadline now) * 1000000 :: Double))
                    tvar <- registerDelay micros
                    pure (tc.tcId, tvar)
                mTimeoutVar <- case strategy of
                    YieldOnTimeout ms -> Just <$> registerDelay (max 0 ms * 1000)
                    _ -> pure Nothing
                atomically $
                    (noOutcome{awoInterrupted = True} <$ interruptArm)
                        `orElse` (noOutcome <$ controlArm)
                        `orElse` deadlineArm deadlineVars
                        `orElse` (noOutcome <$ mainCondition world mTimeoutVar)
  where
    noOutcome = AttachWaitOutcome [] False
    running = filter ((== Running) . tcState) calls
    bucketOf :: TrackedToolCall -> AttachBucket
    bucketOf tc
        | isJust tc.tcAttachDeadline = BDeadline
        | otherwise = case snd (flattenDisposition tc.tcPolicy.apDisposition) of
            RunAsync Nothing -> BStrategy
            _ -> BForever
    foreverCalls = [tc | tc <- running, bucketOf tc == BForever]
    strategyCalls = [tc | tc <- running, bucketOf tc == BStrategy]
    deadlineCalls = [tc | tc <- running, bucketOf tc == BDeadline]
    foreverEids = [eid | tc <- foreverCalls, Just eid <- [tc.tcEntityId]]
    strategyEids = [eid | tc <- strategyCalls, Just eid <- [tc.tcEntityId]]

    interruptArm :: STM ()
    interruptArm = case mMailbox of
        Nothing -> retry
        Just mb -> void (awaitMail mb cursor isInterruptEnvelope)
    isInterruptEnvelope :: Envelope -> Bool
    isInterruptEnvelope e = e.envPriority == Interrupt

    {- 'Control' mail (e.g. 'CancelAllAttached', 'CancelCalls') is otherwise
    only checked between turns; without this arm, mail sent while genuinely
    attached and waiting here would sit unnoticed until this wait exits on
    its own -- i.e. until the call it was meant to cancel has already
    finished. Peeked, not consumed: the caller loop re-reads it and applies
    it via 'applyControlMail' once this wait returns.
    -}
    controlArm :: STM ()
    controlArm = case mMailbox of
        Nothing -> retry
        Just mb -> void (awaitMail mb cursor isControlEnvelope)
    isControlEnvelope :: Envelope -> Bool
    isControlEnvelope e = case e.envBody of
        Control _ -> True
        _ -> False

    deadlineArm :: [(ToolCallId, TVar Bool)] -> STM AttachWaitOutcome
    deadlineArm [] = retry
    deadlineArm vars = do
        fired <- filterM (readTVar . snd) vars
        if null fired then retry else pure $ AttachWaitOutcome (map fst fired) False

    mainCondition :: World -> Maybe (TVar Bool) -> STM ()
    mainCondition world mTimeoutVar = do
        awaitEntities world all foreverEids
        case strategy of
            YieldWhenAllDone -> awaitEntities world all strategyEids
            YieldOnAnyProgress -> unless (null strategyEids) $ awaitEntities world any strategyEids
            YieldOnTimeout _ ->
                unless (null strategyEids) $ case mTimeoutVar of
                    Just tvar -> awaitEntities world any strategyEids `orElse` (readTVar tvar >>= \fired -> unless fired retry)
                    Nothing -> awaitEntities world any strategyEids

data AttachBucket = BForever | BStrategy | BDeadline
    deriving (Eq)

{- | Apply an R3a outcome to a polled call: mark it detached (with a
human-readable reason for the placeholder, see 'partialToolMessages') when
it is still 'Running' and either an interrupt fired or its own deadline
was among those that elapsed. Calls governed by the yield strategy, or
still within their attach window, are left untouched — that is today's
ordinary "still running" placeholder, not a new detach.
-}
applyAttachOutcome :: AttachWaitOutcome -> TrackedToolCall -> TrackedToolCall
applyAttachOutcome outcome tc
    | tc.tcState /= Running = tc
    | outcome.awoInterrupted = tc{tcDetachedReason = Just "interrupted"}
    | tc.tcId `elem` outcome.awoDeadlineElapsed = tc{tcDetachedReason = Just (deadlineReason tc)}
    | otherwise = tc
  where
    deadlineReason :: TrackedToolCall -> Text.Text
    deadlineReason t = case snd (flattenDisposition t.tcPolicy.apDisposition) of
        RunAsync (Just secs) -> "still running after " <> Text.pack (show secs) <> "s"
        _ -> "still running"

{- | Retry until the predicate ('any' or 'all') holds over the entities'
finality.
-}
awaitEntities :: World -> ((Bool -> Bool) -> [Bool] -> Bool) -> [EntityId] -> STM ()
awaitEntities world quantifier eids = do
    finals <- mapM entityFinal eids
    unless (quantifier id finals) retry
  where
    entityFinal eid = do
        mState <- getComponent @OSConv.ToolCallState world eid
        pure $ maybe True OSConv.isToolCallCompleted mState

{- | Block on background calls from an earlier step according to the yield
strategy (used by 'collectLateResults' and 'askUserQuery' — R1's "late
results", not R3a's attach/detach wait over the calls the current turn just
started; see 'waitAttachedCalls' for that).

The wait watches the calls' OS entities, so it covers calls started in an
earlier step.

* 'YieldOnAnyProgress' blocks until at least one call is final.
* 'YieldWhenAllDone' blocks until every call is final.
* 'YieldOnTimeout' blocks until the timeout elapses or at least one call
  is final.

A call whose entity is missing counts as final (it is orphaned and will be
resolved by 'pollRunningCall'). Without an OS world there is nothing to
wait on and the function returns immediately.
-}
waitForRunningCalls :: ToolExecutionContext -> AsyncYieldStrategy -> [TrackedToolCall] -> IO ()
waitForRunningCalls _ _ [] = pure ()
waitForRunningCalls ctx strategy calls =
    case Ctx.ctxWorld ctx of
        Nothing -> pure ()
        Just world -> do
            let eids = [eid | tc <- calls, Just eid <- [tcEntityId tc]]
                -- Calls without an entity are orphaned, hence already final.
                hasOrphan = length eids /= length calls
            case strategy of
                YieldWhenAllDone -> atomically $ awaitEntities world all eids
                _ | hasOrphan -> pure ()
                YieldOnAnyProgress -> atomically $ awaitEntities world any eids
                YieldOnTimeout ms ->
                    void $ System.Timeout.timeout (max 0 ms * 1000) $ atomically $ awaitEntities world any eids

{- | Ensure the agent has an async engine when it runs asynchronously with
an OS world, creating one if necessary.

The engine is stored on the returned agent so every later step (and the
cancel hook in 'buildContext') shares it.
-}
prepareAsyncEngine :: Agent r -> IO (Agent r)
prepareAsyncEngine agent =
    case (agent.ctxExecutionMode, agent.ctxWorld, agent.ctxAsyncEngine) of
        (Asynchronous, Just world, Nothing) -> do
            let limit = maybe defaultMaxConcurrency (max 1) agent.ctxMaxConcurrency
            engine <- mkAsyncEngine world (executeCall agent) limit agent.ctxAsyncCallTimeout agent.ctxMailbox
            pure agent{ctxAsyncEngine = Just engine}
        _ -> pure agent

{- | Default maximum number of concurrent async tool calls.
-}
defaultMaxConcurrency :: Int
defaultMaxConcurrency = 4

{- | Defer a classified call by generating a continuation token and storing
a snapshot when a continuation store is configured.
-}
deferTrackedCall ::
    forall r.
    ToolExecutionContext ->
    Agent r ->
    ClassifiedCall ->
    IO TrackedToolCall
deferTrackedCall ctx agent c = do
    let tc = ccCall c
    let disp = ccFullDisposition c
    token <- newContinuationToken
    now <- getCurrentTime
    let snapshot = mkToolContinuationSnapshot token now tc disp ctx
    case agent.ctxContinuationStore of
        Just store -> storeContinuation store snapshot
        Nothing -> pure ()
    pure $ tc{tcState = Deferred, tcContinuation = Just token, tcPolicy = AppliedPolicy disp Nothing}

{- | Poll a running call's OS entity and update its state if it has finished.

On resume, calls that were previously started by the async engine may have
finished in the background. This helper copies their final result from the
OS entity back into the 'TrackedToolCall'.

A running call whose entity no longer exists (e.g. after a process
restart), or that cannot be looked up at all, is orphaned: it can never
finish, so it is marked 'Failed' with an explanatory response.
-}
pollRunningCall ::
    ToolExecutionContext ->
    TrackedToolCall ->
    IO TrackedToolCall
pollRunningCall ctx tc
    | tcState tc /= Running = pure tc
    | otherwise =
        case (Ctx.ctxWorld ctx, tcEntityId tc) of
            (Just world, Just eid) -> do
                (mState, mChildSessionId) <-
                    atomically $
                        (,)
                            <$> getComponent @OSConv.ToolCallState world eid
                            <*> ((>>= OSConv.tcChildSessionId) <$> getComponent @OSConv.ToolCallConfig world eid)
                let tc0 = maybe tc (\sid -> tc{tcChildSessionId = Just sid}) mChildSessionId
                case mState of
                    Nothing -> pure orphaned
                    Just st -> updateFromState tc0 st
            _ -> pure orphaned
  where
    orphaned =
        tc
            { tcState = Failed
            , tcResult =
                Just $
                    TextResponse
                        "tool call orphaned: it was running in a process that no longer tracks it (e.g. after a restart), so its result is lost"
            }

    updateFromState tc' st =
        case OSConv.tcStatus st of
            OSConv.TcCompleted val ->
                case Aeson.fromJSON val of
                    Aeson.Success response ->
                        pure $ tc'{tcState = Completed, tcResult = Just response}
                    Aeson.Error err ->
                        pure $ tc'{tcState = Failed, tcResult = Just $ TextResponse $ "failed to decode async result: " <> Text.pack err}
            OSConv.TcFailed err ->
                pure $ tc'{tcState = Failed, tcResult = Just $ TextResponse $ "async tool call failed: " <> err}
            OSConv.TcCancelled ->
                pure $ tc'{tcState = Failed, tcResult = Just $ TextResponse "async tool call was cancelled"}
            _ -> pure tc'

{- | Refresh the head partial turn from the OS world.

Running calls are polled without blocking. If every call is then final, the
partial turn is replaced by a full 'UserTurn'; otherwise the partial turn
is replaced with the refreshed calls. Sessions whose head is not a partial
turn with running calls are returned unchanged.
-}
refreshHeadPartialTurn :: ToolExecutionContext -> Session -> IO Session
refreshHeadPartialTurn ctx sess =
    case sess.turns of
        (PartialUserTurn partial _ : rest)
            | any ((== Running) . tcState) partial.pTrackedToolCalls -> do
                polled <- mapM (pollRunningCall ctx) partial.pTrackedToolCalls
                if polled == partial.pTrackedToolCalls
                    then pure sess
                    else do
                        let content = partial{pTrackedToolCalls = polled}
                            sPrompt = partial.pUserPrompt
                            sTools = partial.pUserTools
                            uQuery = partial.pUserQuery
                            turn
                                | all (isFinalToolCallState . tcState) polled =
                                    let responses = partialToolMessages content
                                     in UserTurn
                                            (UserTurnContent sPrompt sTools uQuery responses partial.pUserMail)
                                            (Just $ calculateUserTurnByteUsage sPrompt sTools uQuery (map snd responses))
                                | otherwise =
                                    PartialUserTurn content (Just $ calculatePartialTurnByteUsage sPrompt sTools uQuery polled)
                        pure sess{turns = turn : rest}
        _ -> pure sess


{- | Collect the results of background calls that finished since the LLM saw
their placeholder.

Finished calls are marked 'tcDeliveredLate' in the partial turn that holds
them, so they are delivered exactly once, and returned so the caller can
include them in the next user turn (see 'lateResultsQuery').

When @block@ is set (the step has no new tool calls to run) and background
calls exist, the function first waits for them: until all are done under
'YieldWhenAllDone', otherwise until at least one is done.
-}
collectLateResults :: ToolExecutionContext -> AsyncYieldStrategy -> Bool -> Session -> IO (Session, [TrackedToolCall])
collectLateResults ctx strategy block sess =
    case backgroundCalls sess of
        [] -> pure (sess, [])
        pending -> do
            if block then waitForRunningCalls ctx (lateWaitStrategy strategy) pending else pure ()
            polled <- mapM (pollRunningCall ctx) pending
            let delivered = [tc{tcDeliveredLate = True} | tc <- polled, isFinalToolCallState (tcState tc)]
                deliveredMap = Map.fromList [(tcId tc, tc) | tc <- delivered]
                updateCall tc = Map.findWithDefault tc (tcId tc) deliveredMap
                updateTurn (PartialUserTurn partial usage) =
                    PartialUserTurn partial{pTrackedToolCalls = map updateCall partial.pTrackedToolCalls} usage
                updateTurn t = t
            if null delivered
                then pure (sess, [])
                else pure (sess{turns = map updateTurn sess.turns}, delivered)

{- | How long to wait for background calls before delivering their results:
all of them under 'YieldWhenAllDone', otherwise the first one to finish.
-}
lateWaitStrategy :: AsyncYieldStrategy -> AsyncYieldStrategy
lateWaitStrategy = \case
    YieldWhenAllDone -> YieldWhenAllDone
    _ -> YieldOnAnyProgress

{- | Obtain the user query for an 'AskUserPrompt' action.

Returns the query and whether late results should be waited for (see
'collectLateResults'), which is only the case when the step has neither a
query to ask for nor tool calls to run.

When a query is needed while background calls are running, the agent's
'usrQuery' races the background calls (per 'lateWaitStrategy'). If the calls
finish first, 'usrQuery' is cancelled and no query is returned, so their
results reach the LLM without waiting for the user. 'usrQuery' must
therefore tolerate cancellation (e.g. read input with STM).
-}
askUserQuery :: ToolExecutionContext -> Agent r -> MissingUserPrompt -> Session -> IO (Maybe UserQuery, Bool)
askUserQuery ctx agent missing sess
    | not missing.missingQuery = pure (Nothing, null missing.missingToolCalls)
    | otherwise =
        case (Ctx.ctxWorld ctx, backgroundCalls sess) of
            (Just _, pending@(_ : _)) -> do
                winner <- race agent.usrQuery (waitForRunningCalls ctx (lateWaitStrategy agent.ctxAsyncYieldStrategy) pending)
                pure (either id (const Nothing) winner, False)
            _ -> do
                q <- agent.usrQuery
                pure (q, False)

{- | Render late results as a user message.

Each finished call is listed with its provider id, tool name, status, and
result. Media from the results is attached to the query.
-}
lateResultsQuery :: [TrackedToolCall] -> Maybe UserQuery
lateResultsQuery [] = Nothing
lateResultsQuery calls =
    Just $
        UserQuery
            (Text.intercalate "\n\n" ("Background tool calls finished since their placeholder responses:" : map describe calls))
            (concatMap media calls)
  where
    describe tc =
        let callId = maybe "(unknown id)" id (providerToolCallId tc.tcCall)
            toolName = maybe "unknown tool" (\(ToolCall n _) -> n) (parseToolCallFromLlmToolCall tc.tcCall)
            status = if tcState tc == Completed then "completed" else "failed"
         in "tool_call_id " <> callId <> " (" <> toolName <> ") " <> status <> ":\n" <> maybe "" renderResult (tcResult tc)

    renderResult (TextResponse txt) = txt
    renderResult (JsonResponse val) = Text.decodeUtf8 (LByteString.toStrict (Aeson.encode val))
    renderResult (MediaResponse m) = "[attached " <> m.mediaMimeType <> "]"
    renderResult (MixedResponse parts) = Text.intercalate "\n" (map renderPart parts)

    renderPart (TextPart txt) = txt
    renderPart (MediaPart m) = "[attached " <> m.mediaMimeType <> "]"

    media tc = case tcResult tc of
        Just (MediaResponse m) -> [m]
        Just (MixedResponse parts) -> [m | MediaPart m <- parts]
        _ -> []

-- | Combine two optional user queries, joining texts and media.
mergeUserQueries :: Maybe UserQuery -> Maybe UserQuery -> Maybe UserQuery
mergeUserQueries Nothing q = q
mergeUserQueries q Nothing = q
mergeUserQueries (Just (UserQuery t1 m1)) (Just (UserQuery t2 m2)) =
    Just $ UserQuery (t1 <> "\n\n" <> t2) (m1 <> m2)

{- | Receive points R1 and R2 (@todos/session-mailbox.md@ §2).

Always peeks unread mail without blocking (R1: "building a user turn,
before AskLlmCompletion"). When @shouldBlock@ is set — the step has nothing
else to do this turn (no tool calls to run, no late results, and the query
would otherwise be a plain idle wait) — and nothing was unread, blocks until
some mail arrives (R2: "idle").

Advances the returned session's 'mailCursor' to the highest 'envSeq' seen,
so a crash between this read and storing the session redelivers into a
session that does not contain the mail yet (D1: exactly-once effect via the
versioned session store).

A 'Nothing' 'ctxMailbox' (the default) leaves the session and its cursor
untouched, so every existing code path is unaffected.
-}
receiveMailForTurn :: Agent r -> Bool -> Session -> IO (Session, [Envelope])
receiveMailForTurn agent shouldBlock sess =
    case agent.ctxMailbox of
        Nothing -> pure (sess, [])
        Just mb -> do
            let cur = sess.mailCursor
            unread <- atomically (mbUnread mb cur)
            envelopes <-
                if null unread && shouldBlock
                    then atomically (awaitMail mb cur (const True))
                    else pure unread
            case envelopes of
                [] -> pure (sess, [])
                _ -> do
                    sess' <- applyContinuationMail agent envelopes sess
                    pure (sess'{mailCursor = maximum (map envSeq envelopes)}, envelopes)

{- | Apply unread 'ContinuationResult' mail to the session's deferred calls.

Replaces the old per-runner @lsInbox@\/@completeCall@ special case
(@todos/session-mailbox.md@, a Phase 3 follow-up: "'autoResume' becomes
'post `ContinuationResult`'"): instead of a side channel only 'Host.Runner'
understood, an external result now arrives as ordinary mail and is acted on
here, at the same receive point (R1) that already folds every other kind of
mail into the turn being built — so every front-end with a mailbox gets
this behaviour for free, not just the server.

A 'Deferred' call matching a token moves to 'Completed'; once every call in
the head partial turn is final, it is folded into a full 'UserTurn' exactly
as 'refreshHeadPartialTurn' already does for a background call finishing.
Structurally idempotent: a call whose state is no longer 'Deferred' is left
untouched, so mail delivered again before its cursor advances (mail is
never consumed by reading, only by the cursor committing, per §1) never
re-applies the same result twice.
-}
applyContinuationMail :: Agent r -> [Envelope] -> Session -> IO Session
applyContinuationMail agent envelopes sess =
    case results of
        [] -> pure sess
        _ -> case sess.turns of
            (PartialUserTurn partial _usage : rest) -> do
                updated <- mapM applyResult partial.pTrackedToolCalls
                if updated == partial.pTrackedToolCalls
                    then pure sess
                    else do
                        let content = partial{pTrackedToolCalls = updated}
                            sPrompt = partial.pUserPrompt
                            sTools = partial.pUserTools
                            uQuery = partial.pUserQuery
                            turn
                                | all (isFinalToolCallState . tcState) updated =
                                    let responses = partialToolMessages content
                                     in UserTurn
                                            (UserTurnContent sPrompt sTools uQuery responses partial.pUserMail)
                                            (Just $ calculateUserTurnByteUsage sPrompt sTools uQuery (map snd responses))
                                | otherwise =
                                    PartialUserTurn content (Just $ calculatePartialTurnByteUsage sPrompt sTools uQuery updated)
                        pure sess{turns = turn : rest}
            _ -> pure sess
  where
    results = [(token, result) | e <- envelopes, ContinuationResult token result <- [e.envBody]]

    applyResult :: TrackedToolCall -> IO TrackedToolCall
    applyResult tc
        | tc.tcState /= Deferred = pure tc
        | otherwise = case tc.tcContinuation of
            Nothing -> pure tc
            Just token -> case lookup token results of
                Nothing -> pure tc
                Just result -> do
                    forM_ agent.ctxToolCache $ \cache -> do
                        now <- Data.Time.getCurrentTime
                        cache.cacheStore (Cache.computeCacheKey tc.tcCall) $ CachedResult result now Nothing
                    forM_ agent.ctxContinuationStore $ \store -> void $ csComplete store token result
                    pure tc{tcState = Completed, tcResult = Just result}

{- | Render mail as a user message, one block per envelope with a header the
model can quote back (§2, "What the LLM sees"). 'Control' envelopes are
consumed (already reflected in the advanced cursor) but render nothing.
-}
mailQuery :: [Envelope] -> Maybe UserQuery
mailQuery envelopes =
    case filter (not . isControlEnvelope) envelopes of
        [] -> Nothing
        rendered -> Just $ UserQuery (Text.intercalate "\n\n" (map renderMailEnvelope rendered)) (concatMap mailMedia rendered)
  where
    isControlEnvelope :: Envelope -> Bool
    isControlEnvelope e = case e.envBody of
        Control _ -> True
        _ -> False

-- | Render one non-'Control' envelope as a block of text.
renderMailEnvelope :: Envelope -> Text.Text
renderMailEnvelope e =
    "[mail " <> messageIdText e.envId <> " " <> senderText e.envFrom <> replyHint <> "]\n" <> bodyText
  where
    replyHint = case e.envBody of
        AgentMessage _ _ True -> ", expects reply"
        _ -> ""

    senderText sender = case sender of
        FromUser owner -> "from user" <> maybe "" (\o -> " (" <> o <> ")") owner
        FromSession sid agentSlug ->
            "from session " <> Text.pack (show sid) <> maybe "" (\a -> " (agent \"" <> a <> "\")") agentSlug
        FromToolCall tcid -> "from tool call " <> Text.pack (show tcid)
        FromSystem source -> "from " <> source

    bodyText = case e.envBody of
        UserMessage q -> q.queryText
        AgentMessage txt _ _ -> txt
        ToolCallFinished tcid state result ->
            "tool call "
                <> Text.pack (show tcid)
                <> " "
                <> (if state == Completed then "completed" else "failed")
                <> ":\n"
                <> renderMailResult result
        ContinuationResult _token result -> renderMailResult result
        WatchedEvent _sid eventName payload ->
            eventName <> ":\n" <> Text.decodeUtf8 (LByteString.toStrict (Aeson.encode payload))
        Control _ -> ""

    renderMailResult (TextResponse txt) = txt
    renderMailResult (JsonResponse val) = Text.decodeUtf8 (LByteString.toStrict (Aeson.encode val))
    renderMailResult (MediaResponse m) = "[attached " <> m.mediaMimeType <> "]"
    renderMailResult (MixedResponse parts) = Text.intercalate "\n" (map renderMailPart parts)

    renderMailPart (TextPart txt) = txt
    renderMailPart (MediaPart m) = "[attached " <> m.mediaMimeType <> "]"

-- | Media attached to a mail envelope's body, if any.
mailMedia :: Envelope -> [MediaAttachment]
mailMedia e = case e.envBody of
    UserMessage q -> q.queryMedia
    ToolCallFinished _ _ (MediaResponse m) -> [m]
    ToolCallFinished _ _ (MixedResponse parts) -> [m | MediaPart m <- parts]
    ContinuationResult _ (MediaResponse m) -> [m]
    ContinuationResult _ (MixedResponse parts) -> [m | MediaPart m <- parts]
    _ -> []

{- | R4 (@todos/session-mailbox.md@ §2, D5): ask for a completion, or, when
'ctxInterruptCompletions' is set and the agent has a mailbox, let an
'Interrupt'-priority envelope arriving first cancel it instead.

A genuine 'race' (thread cancellation), per the spec's own note that this
is one of only two places that need one. On the mail side winning, nothing
about the completion is kept -- there is no partial 'LlmTurn' to record --
and the caller is expected to fold the returned envelopes into the head
turn ('amendHeadTurnWithMail') and ask again, rather than treat this as a
normal completion result.

With 'ctxInterruptCompletions' off (the default) or no mailbox, this is
exactly @'Left' \<$\> agent.complete completion@: no behaviour change.
-}
askForCompletionOrInterrupt ::
    Agent r ->
    Session ->
    LlmCompletion ->
    IO (Either (LlmResponse, [LlmToolCall]) [Envelope])
askForCompletionOrInterrupt agent sess completion =
    case (agent.ctxInterruptCompletions, agent.ctxMailbox) of
        (True, Just mb) -> do
            outcome <-
                race
                    (agent.complete completion)
                    (atomically (awaitMail mb sess.mailCursor isInterruptMail))
            pure outcome
        _ -> Left <$> agent.complete completion

-- | Whether an envelope is 'Interrupt' priority (R3a\/R4's trigger).
isInterruptMail :: Envelope -> Bool
isInterruptMail e = e.envPriority == Interrupt

{- | Fold newly-arrived mail into the session's head turn in place (R4, D5):
the turn that was about to be answered is amended, not stacked under a
second 'UserTurn' or left behind a truncated 'LlmTurn'. Advances
'mailCursor' past the folded-in envelopes.

A no-op if there is no mail, or if the head turn is not a 'UserTurn' \/
'PartialUserTurn' (an 'LlmTurn' can't be the head at the point 'AskLlmCompletion'
is asked, so this is only a defensive fallback).
-}
amendHeadTurnWithMail :: [Envelope] -> Session -> Session
amendHeadTurnWithMail [] sess = sess
amendHeadTurnWithMail envelopes sess = case sess.turns of
    (UserTurn content byteUsage : rest) ->
        sess
            { turns =
                UserTurn
                    content
                        { userQuery = mergeUserQueries content.userQuery (mailQuery envelopes)
                        , userMail = content.userMail ++ envelopes
                        }
                    byteUsage
                    : rest
            , mailCursor = newCursor
            }
    (PartialUserTurn content byteUsage : rest) ->
        sess
            { turns =
                PartialUserTurn
                    content
                        { pUserQuery = mergeUserQueries content.pUserQuery (mailQuery envelopes)
                        , pUserMail = content.pUserMail ++ envelopes
                        }
                    byteUsage
                    : rest
            , mailCursor = newCursor
            }
    _ -> sess
  where
    newCursor = max sess.mailCursor (maximum (map (.envSeq) envelopes))

{- | Process a single tracked call according to the agent's policy.

This function is retained for backward compatibility with callers that
process calls individually. It now only handles inline ('RunSync' and
'RunIsolated') and 'Defer' dispositions; 'RunAsync' calls should be
processed in a batch via 'executeTrackedCalls'.
-}
processTrackedCall ::
    forall r.
    ToolExecutionContext ->
    Agent r ->
    TrackedToolCall ->
    IO TrackedToolCall
processTrackedCall _ctx _agent tc
    | tcState tc /= Ready = pure tc
processTrackedCall ctx agent tc = do
    let disp = agent.ctxToolCallPolicy ctx tc.tcCall
    case snd (flattenDisposition disp) of
        RunSync -> executeAndComplete disp
        RunIsolated _spec -> executeAndComplete disp
        Defer _reason -> deferCall disp
        RunAsync _attachSeconds ->
            -- RunAsync is executed via the async engine in executeTrackedCalls.
            -- As a safe fallback for individual processing, degrade to sync.
            executeAndComplete disp
        Decorate _ _ -> executeAndComplete disp
  where
    executeAndComplete disp = do
        (result, tc') <- executeTrackedCallWithEntity agent ctx tc
        pure $ tc'{tcState = Completed, tcResult = Just result, tcPolicy = AppliedPolicy disp Nothing}

    deferCall disp = do
        token <- newContinuationToken
        now <- getCurrentTime
        let snapshot = mkToolContinuationSnapshot token now tc disp ctx
        case agent.ctxContinuationStore of
            Just store -> storeContinuation store snapshot
            Nothing -> pure ()
        pure $ tc{tcState = Deferred, tcContinuation = Just token, tcPolicy = AppliedPolicy disp Nothing}

getCurrentTime :: IO Data.Time.UTCTime
getCurrentTime = Data.Time.getCurrentTime

{- | Create a fresh 'TrackedToolCall' in the 'Ready' state.

The policy is recorded for observability but the call starts as 'Ready'
and is only classified when the scheduler processes it.
-}
mkReadyTrackedCall ::
    ToolExecutionContext ->
    Agent r ->
    LlmToolCall ->
    IO TrackedToolCall
mkReadyTrackedCall ctx agent call = do
    callId <- newToolCallId
    let policy = agent.ctxToolCallPolicy ctx call
    pure $
        TrackedToolCall
            { tcId = callId
            , tcCall = call
            , tcState = Ready
            , tcResult = Nothing
            , tcContinuation = Nothing
            , tcPolicy = AppliedPolicy policy Nothing
            , tcEntityId = Nothing
            , tcDeliveredLate = False
            , tcAttachDeadline = Nothing
            , tcDetachedReason = Nothing
            , tcChildSessionId = Nothing
            }

{- | Ensure every tracked call has a corresponding OS entity when a world is
available.

Calls that already have an entity id are left unchanged. For calls without
an entity, an OS 'ToolCallConfig' and 'ToolCallState' are created in the
world and the 'tcEntityId' field is populated. The provider's tool-call id
is recorded on the entity so capabilities can resolve it.
-}
ensureTrackedCallEntities ::
    ToolExecutionContext ->
    [TrackedToolCall] ->
    IO [TrackedToolCall]
ensureTrackedCallEntities ctx =
    traverse ensureEntity
  where
    ensureEntity :: TrackedToolCall -> IO TrackedToolCall
    ensureEntity tc
        | isJust (tcEntityId tc) = pure tc
        | otherwise =
            case Ctx.ctxWorld ctx of
                Nothing -> pure tc
                Just world ->
                    case parseToolCallFromLlmToolCall tc.tcCall of
                        Just (ToolCall toolName args) -> do
                            eid <-
                                TCT.createToolCallEntityWithProviderId
                                    world
                                    (Ctx.ctxSessionId ctx)
                                    (Ctx.ctxConversationId ctx)
                                    (Ctx.ctxTurnId ctx)
                                    Nothing
                                    toolName
                                    args
                                    tc.tcId
                                    (providerToolCallId tc.tcCall)
                            pure tc{tcEntityId = Just eid}
                        Nothing -> pure tc

{- | Execute a tracked call, updating its OS entity lifecycle state when a
world is available.

The entity is moved to 'TcExecuting' before execution and to
'TcCompleted' afterwards. The returned tuple contains the tool response
and the updated tracked call (with state set to 'Completed' and the
result attached).
-}
executeTrackedCallWithEntity ::
    forall r.
    Agent r ->
    ToolExecutionContext ->
    TrackedToolCall ->
    IO (UserToolResponse, TrackedToolCall)
executeTrackedCallWithEntity agent ctx tc = do
    -- Start execution in the OS world
    case (Ctx.ctxWorld ctx, tcEntityId tc) of
        (Just world, Just eid) -> TCT.startToolCall world eid
        _ -> pure ()
    -- Execute the call (with optional cache lookup)
    result <- executeTrackedCallWithCache agent ctx tc
    -- Mark completion in the OS world
    case (Ctx.ctxWorld ctx, tcEntityId tc) of
        (Just world, Just eid) -> TCT.completeToolCall world eid (Aeson.toJSON result)
        _ -> pure ()
    pure (result, tc{tcState = Completed, tcResult = Just result})

{- | Execute a tracked call with optional cache lookup.

If a cache is configured, checks it first. If found, returns the cached
result. Otherwise executes the tool and optionally stores the result.
-}
executeTrackedCallWithCache :: Agent r -> ToolExecutionContext -> TrackedToolCall -> IO UserToolResponse
executeTrackedCallWithCache agent ctx tc = do
    case agent.ctxToolCache of
        Just cache -> do
            let key = Cache.computeCacheKey tc.tcCall
            mCached <- cache.cacheLookup key
            case mCached of
                Just cached -> pure cached.crResult
                Nothing -> do
                    result <- executeCall agent ctx tc.tcCall
                    now <- getCurrentTime
                    cache.cacheStore key $ CachedResult result now Nothing
                    pure result
        Nothing -> executeCall agent ctx tc.tcCall

{- | Execute a single tool call using the agent's configured executor or toolCall.
When a 'DeploymentRunner' is configured but no explicit 'ToolExecutor' is set,
isolated calls are dispatched through the runner and non-isolated calls fall
back to the agent's 'toolCall'.

The decorators carried by the call's resolved 'ToolCallDisposition' (see
'flattenDisposition') wrap the selected execution path, so a 'wrappers' rule
in the agent's @toolCallPolicyConfig@ (timeout, retries, cache, truncate)
applies regardless of which branch below runs the call.
-}
executeCall :: Agent r -> ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
executeCall agent ctx call = do
    response <- applyDecorators wrapperEnv decorators baseExec ctx call
    case response of
        ToolComplete result -> pure result
        ToolYield{} -> pure $ TextResponse "tool call yielded unexpectedly under a synchronous executor"
  where
    wrapperEnv = WrapperEnv{weCache = agent.ctxToolCache, weInvokeTool = Just (mkToolInvoker agent.toolCall ctx)}
    (decorators, _base) = flattenDisposition (agent.ctxToolCallPolicy ctx call)
    baseExec ctx' call' = ToolComplete <$> baseExecSync ctx' call'
    baseExecSync ctx' call' =
        case agent.ctxToolExecutor of
            Just executor -> executor.execSync ctx' call'
            Nothing ->
                case agent.ctxDeploymentRunner of
                    Just runner ->
                        let executor = isolatedExecutor agent.ctxToolCallPolicy runner (inProcessExecutor agent.toolCall)
                         in executor.execSync ctx' call'
                    Nothing -> agent.toolCall ctx' call'

{- | Build a ToolExecutionContext based on the agent's configuration.

The context is populated according to 'ContextConfig' settings:
* 'includeFullSession' controls whether 'ctxFullSession' is populated
* 'includeAgentId' controls whether 'ctxAgentId' is included (as Nothing or Just)

This uses the agent's 'ctxCallStack' to maintain the call chain for nested
agent invocations, supporting arbitrarily deep nesting of sub-conversations.

The context also includes the agent's 'ctxWorld' and 'ctxEventQueue' if present,
which enables subcall conversations to be visible in the TUI, and a cancel
hook backed by the agent's async engine.
-}
buildContext :: Agent r -> Session -> ConversationId -> ToolExecutionContext
buildContext agent sess convId =
    let config = agent.contextConfig
        baseCtx =
            mkToolExecutionContext
                sess.sessionId
                convId
                sess.turnId
                (if config.includeAgentId then Nothing else Nothing) -- AgentId not available in Session, use Nothing
                (if config.includeFullSession then Just sess else Nothing)
                agent.toolPortal
                agent.ctxCallStack
                Nothing -- No max recursion depth by default
     in baseCtx
            { Ctx.ctxWorld = agent.ctxWorld
            , Ctx.ctxEventQueue = agent.ctxEventQueue
            , Ctx.ctxCancelToolCall = fmap Engine.cancelToolCall agent.ctxAsyncEngine
            , -- Phase 2 (@todos/session-mailbox.md@ §3): built once per turn
              -- from this session's current cursor, so every tool call in
              -- the turn shares the same view of "what mail is new".
              Ctx.ctxAwaitMail = fmap (\mb -> awaitMail mb sess.mailCursor (const True)) agent.ctxMailbox
            , -- Phase 4 (@todos/session-mailbox.md@ §5): copied straight
              -- through so send-message/spawn-session can address other
              -- sessions.
              Ctx.ctxMailRouter = agent.ctxMailRouter
            , Ctx.ctxSpawnSession = agent.ctxSpawnSession
            , -- Phase 6 (@todos/session-mailbox.md@ §7): copied the same way.
              Ctx.ctxWatchSession = agent.ctxWatchSession
            , Ctx.ctxUnwatchSession = agent.ctxUnwatchSession
            , -- Cheap view of the calls the capabilities may be asked about,
              -- including calls left behind by an earlier process.
              Ctx.ctxSessionToolCalls = sessionTrackedCalls sess
            , -- The session's derive_agent narrowings (§8.4), folded fresh
              -- from its own turns each time, the same as ctxSessionToolCalls.
              Ctx.ctxDerivedNarrowings = Bindings.deriveAgentTable sess
            , Ctx.ctxParams = agent.ctxParams
            , Ctx.ctxInheritedBindings = agent.ctxInheritedBindings
            }

-- | Tracked calls of every partial turn of a session, newest turn first.
sessionTrackedCalls :: Session -> [TrackedToolCall]
sessionTrackedCalls sess =
    [tc | PartialUserTurn partial _ <- sess.turns, tc <- partial.pTrackedToolCalls]

{- | Ensure the agent's OS world has tool-call component stores registered.

If the agent has a 'World', the tool-call component stores are registered
idempotently and the agent is returned with the registered world. This
should be called once per step so that any entities created during the
step are visible through the agent's context.

An asynchronous agent without a 'World' gets a private one: background
calls are tracked as entities, so without it async execution would
silently degrade to inline execution. The loops keep the returned agent,
so the world lives as long as the run.
-}
prepareAgentWorld :: Agent r -> IO (Agent r)
prepareAgentWorld agent =
    case (agent.ctxWorld, agent.ctxExecutionMode) of
        (Just world, _) -> register world
        (Nothing, Asynchronous) -> register =<< atomically newWorld
        (Nothing, Synchronous) -> pure agent
  where
    register world = do
        world' <- TCT.ensureToolCallComponentsIO world
        pure agent{ctxWorld = Just world'}

-------------------------------------------------------------------------------
-- Byte Usage Calculation
-------------------------------------------------------------------------------

{- | Calculate byte usage for a user turn.

This includes:
* Input bytes: system prompt + tools + user query
* Tool bytes: tool call responses
-}
calculateUserTurnByteUsage :: SystemPrompt -> [SystemTool] -> Maybe UserQuery -> [UserToolResponse] -> StepByteUsage
calculateUserTurnByteUsage sPrompt sTools uQuery toolResponses =
    let inputBytes =
            systemPromptBytes sPrompt
                + toolsBytes sTools
                + userQueryBytes uQuery
        toolBytes = sum (map userToolResponseBytes toolResponses)
        -- User turns have no output or reasoning from the LLM
        outputBytes = 0
        reasoningBytes = 0
     in calculateStepByteUsage inputBytes outputBytes reasoningBytes toolBytes Nothing

{- | Calculate byte usage for a partial turn.

Similar to user turn but accounts for partial completion.
-}
calculatePartialTurnByteUsage :: SystemPrompt -> [SystemTool] -> Maybe UserQuery -> [TrackedToolCall] -> StepByteUsage
calculatePartialTurnByteUsage sPrompt sTools uQuery tracked =
    let inputBytes =
            systemPromptBytes sPrompt
                + toolsBytes sTools
                + userQueryBytes uQuery
        toolBytes = sum [userToolResponseBytes result | tc <- tracked, Just result <- [tcResult tc]]
        outputBytes = 0
        reasoningBytes = 0
     in calculateStepByteUsage inputBytes outputBytes reasoningBytes toolBytes Nothing

{- | Calculate byte usage for an LLM turn.

This includes:
* Output bytes: LLM response text
* Reasoning bytes: thinking/reasoning content
* Tool bytes: tool call definitions (payload sent to LLM)
* Token usage: actual token counts from the LLM provider
-}
calculateLlmTurnByteUsage :: LlmResponse -> [LlmToolCall] -> StepByteUsage
calculateLlmTurnByteUsage llmRsp llmTools =
    let outputBytes = fromIntegral $ maybe 0 (LByteString.length . Aeson.encode . Aeson.String) llmRsp.responseText
        reasoningBytes = fromIntegral $ maybe 0 (LByteString.length . Aeson.encode . Aeson.String) llmRsp.responseThinking
        -- Tool call definitions are part of LLM output context
        toolBytes = sum (map toolCallBytes llmTools)
        -- LLM turns don't have input bytes (those are in the user turn)
        inputBytes = 0
        -- Extract token usage from the LLM response
        mTokenUsage = llmRsp.responseTokenUsage
     in calculateStepByteUsage inputBytes outputBytes reasoningBytes toolBytes mTokenUsage

-- | Calculate bytes for system prompt.
systemPromptBytes :: SystemPrompt -> Int
systemPromptBytes (SystemPrompt txt) = Text.length txt * 4 -- Approximate UTF-8 max bytes per char

-- | Calculate bytes for tools list.
toolsBytes :: [SystemTool] -> Int
toolsBytes tools = sum (map toolDefBytes tools)

-- | Calculate bytes for a single tool definition.
toolDefBytes :: SystemTool -> Int
toolDefBytes (SystemTool (V0 val)) = fromIntegral (LByteString.length (Aeson.encode val))
toolDefBytes (SystemTool (V1 def)) =
    let nameBytes = Text.length def.name * 4
        descBytes = Text.length def.description * 4
        -- Properties contribute to size
        propsBytes = sum (map propertyBytes def.properties)
     in nameBytes + descBytes + propsBytes + 100 -- Base overhead

-- | Estimate bytes for a property definition.
propertyBytes :: ParamProperty -> Int
propertyBytes _ = 50 -- Rough estimate per property

-- | Calculate bytes for user query.
userQueryBytes :: Maybe UserQuery -> Int
userQueryBytes Nothing = 0
userQueryBytes (Just (UserQuery txt _)) = Text.length txt * 4

-- | Calculate bytes for a tool call.
toolCallBytes :: LlmToolCall -> Int
toolCallBytes (LlmToolCall val) = fromIntegral (LByteString.length (Aeson.encode val))

-- | Calculate bytes for a user tool response.
userToolResponseBytes :: UserToolResponse -> Int
userToolResponseBytes (TextResponse txt) = Text.length txt * 4 -- UTF-8 max bytes per char
userToolResponseBytes (JsonResponse val) = fromIntegral $ LByteString.length $ Aeson.encode val
userToolResponseBytes (MediaResponse media) = Text.length media.mediaBase64Data -- Already base64 = ASCII
userToolResponseBytes (MixedResponse parts) = sum (map contentPartBytes parts)

-- | Calculate bytes for a content part.
contentPartBytes :: ContentPart -> Int
contentPartBytes (TextPart txt) = Text.length txt * 4
contentPartBytes (MediaPart media) = Text.length media.mediaBase64Data

-- | Check whether any tracked call is in the 'Ready' state.
hasReady :: [TrackedToolCall] -> Bool
hasReady tcs = any (\tc -> tcState tc == Ready) tcs

-- | Check whether any tracked call is in the 'Deferred' state.
hasDeferred :: [TrackedToolCall] -> Bool
hasDeferred tcs = any (\tc -> tcState tc == Deferred) tcs


-------------------------------------------------------------------------------
-- Naive Step Functions
-------------------------------------------------------------------------------

-- Naive action selection function that merely parrots the least surprising
-- thing: it never evolves or stops the agent, always ask for a user query or a prompt.
naiveStep :: Session -> IO (Action Void)
naiveStep sess0 = do
    go sess0
  where
    -- Looks at what the latest Session turn was to decide what to ask.
    go :: Session -> IO (Action Void)
    go sess =
        case sess.turns of
            [] -> pure $ AskUserPrompt (MissingUserPrompt True [])
            (turn : hist) ->
                case turn of
                    (LlmTurn llmTurn _mUsage) -> do
                        pure $ AskUserPrompt (MissingUserPrompt True llmTurn.llmToolCalls)
                    (UserTurn userTurn _mUsage) -> do
                        let sPrompt0 = userTurn.userPrompt
                        let sTools0 = userTurn.userTools
                        let uQuery0 = userTurn.userQuery
                        let tAnswers0 = userTurn.userToolResponses
                        pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist [] (Just sess.sessionId))
                    (PartialUserTurn partial _mUsage) ->
                        -- Partial turn: continue with remaining ready calls.
                        -- If only deferred calls remain, ask for a user prompt with
                        -- an empty missing-tool list so the async scheduler yields.
                        -- Calls still running get a placeholder tool message.
                        if hasReady partial.pTrackedToolCalls || hasDeferred partial.pTrackedToolCalls
                            then pure $ AskUserPrompt (MissingUserPrompt False (partialPendingCalls partial))
                            else do
                                let sPrompt0 = partial.pUserPrompt
                                let sTools0 = partial.pUserTools
                                let uQuery0 = partial.pUserQuery
                                let tAnswers0 = partialToolMessages partial
                                pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist [] (Just sess.sessionId))

-- | Step function that stops when the LLM returns no tool calls.
naiveTilNoToolCallStep :: Session -> IO (Action (LlmTurnContent, Session))
naiveTilNoToolCallStep sess = do
    case sess.turns of
        [] ->
            -- Initial state: need to ask the LLM for completion
            pure $ AskUserPrompt $ MissingUserPrompt True []
        (turn : hist) -> do
            case turn of
                UserTurn userTurn _mUsage -> do
                    -- Last turn was user turn, ask LLM for completion
                    let sPrompt0 = userTurn.userPrompt
                    let sTools0 = userTurn.userTools
                    let uQuery0 = userTurn.userQuery
                    let tAnswers0 = userTurn.userToolResponses
                    pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist [] (Just sess.sessionId))
                LlmTurn llmTurn _mUsage ->
                    -- Last turn was LLM turn
                    if null llmTurn.llmToolCalls
                        then
                            if hasBackgroundCalls sess
                                then
                                    -- No tool calls but background calls are still
                                    -- running: wait for their results.
                                    pure $ AskUserPrompt $ MissingUserPrompt False []
                                else
                                    -- No tool calls: stop
                                    pure $ Stop (llmTurn, sess)
                        else
                            -- Has tool calls: continue with user prompt for tool responses
                            pure $ AskUserPrompt $ MissingUserPrompt False llmTurn.llmToolCalls
                PartialUserTurn partial _mUsage ->
                    -- Last turn was partial - need to continue execution
                    if hasReady partial.pTrackedToolCalls || hasDeferred partial.pTrackedToolCalls
                        then
                            -- Still have ready or deferred calls: continue execution
                            pure $ AskUserPrompt $ MissingUserPrompt False (partialPendingCalls partial)
                        else
                            -- No ready or deferred calls: ask the LLM, with a
                            -- placeholder for calls still running in the background
                            let sPrompt0 = partial.pUserPrompt
                                sTools0 = partial.pUserTools
                                uQuery0 = partial.pUserQuery
                                tAnswers0 = partialToolMessages partial
                             in pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist [] (Just sess.sessionId))

