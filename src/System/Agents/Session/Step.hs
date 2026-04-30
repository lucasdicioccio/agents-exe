{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: propose some non-naive combinators
module System.Agents.Session.Step where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Data.Time

import Data.Void (Void)

import System.Agents.Base (ConversationId)
import System.Agents.Media.Types (ContentPart (..), MediaAttachment (..))
import System.Agents.Session.Base
import System.Agents.Session.Types (StepByteUsage, calculateStepByteUsage)
import System.Agents.ToolSchema (ParamProperty)
import System.Agents.Tools.Cache (CachedResult (..), ToolCache (..))
import qualified System.Agents.Tools.Cache as Cache
import System.Agents.Tools.Context (ToolExecutionContext, mkToolExecutionContext)
import qualified System.Agents.Tools.Context as Ctx

{- | Runs a single step of agent for a given session (synchronous mode).
Agent may be modified, may decide to return a session, or decide to stop.

This is the original synchronous implementation that executes all tool
calls at once using 'traverse'.
-}
runStepM :: forall r. ConversationId -> Agent r -> Session -> IO (Agent r, Either r Session)
runStepM convId agent sess =
    case agent.ctxExecutionMode of
        Asynchronous -> runStepMAsync convId agent sess
        Synchronous -> runStepMSync convId agent sess

{- | Synchronous step execution - all tools execute immediately.

This is the traditional execution mode where all tool calls in a turn
are executed immediately and the session continues without pausing.
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
        next <- agent0.step sess0
        case next of
            Stop r -> pure (agent0, Left r)
            Evolve agent1 -> pure (agent1, Right sess0)
            AskUserPrompt missing -> do
                sPrompt <- agent0.sysPrompt
                sTools <- agent0.sysTools
                uQuery <- if missing.missingQuery then agent0.usrQuery else pure Nothing
                -- Construct ToolExecutionContext for each tool call
                let ctx = buildContext agent0 sess0 convId
                toolResponses <- traverse (agent0.toolCall ctx) missing.missingToolCalls
                let uToolResponses = zip missing.missingToolCalls toolResponses
                -- Calculate byte usage for this user turn
                let byteUsage = calculateUserTurnByteUsage sPrompt sTools uQuery toolResponses
                sess1 <- addTurn sess0 (UserTurn (UserTurnContent{userPrompt = sPrompt, userTools = sTools, userQuery = uQuery, userToolResponses = uToolResponses}) (Just byteUsage))
                pure (agent0, Right sess1)
            AskLlmCompletion completion -> do
                (llmRsp, llmTool) <- agent0.complete completion
                -- Calculate byte usage for this LLM turn, including token usage
                let byteUsage = calculateLlmTurnByteUsage llmRsp llmTool
                sess1 <- addTurn sess0 (LlmTurn (LlmTurnContent llmRsp llmTool) (Just byteUsage))
                pure (agent0, Right sess1)

-------------------------------------------------------------------------------
-- Asynchronous Step Execution
-------------------------------------------------------------------------------

{- | Asynchronous step execution - supports partial tool execution.

In async mode:
1. Tool calls are executed one at a time
2. Each call checks the cache first
3. After each call, the session can be persisted
4. If all calls complete, the turn is finalized
5. If yielding is needed, a PartialUserTurn is created

This enables sessions to be paused and resumed, potentially on different machines.
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
        next <- agent0.step sess0
        case next of
            Stop r -> pure (agent0, Left r)
            Evolve agent1 -> pure (agent1, Right sess0)
            AskUserPrompt missing -> do
                sPrompt <- agent0.sysPrompt
                sTools <- agent0.sysTools
                uQuery <- if missing.missingQuery then agent0.usrQuery else pure Nothing
                -- Check if there's a partial turn to continue
                case getPartialTurn sess0 of
                    Just partial -> do
                        -- Continue execution of partial turn
                        continuePartialTurn convId agent0 sess0 partial
                    Nothing -> do
                        -- Start new async execution
                        startNewAsyncTurn convId agent0 sess0 sPrompt sTools uQuery missing.missingToolCalls
            AskLlmCompletion completion -> do
                (llmRsp, llmTool) <- agent0.complete completion
                let byteUsage = calculateLlmTurnByteUsage llmRsp llmTool
                sess1 <- addTurn sess0 (LlmTurn (LlmTurnContent llmRsp llmTool) (Just byteUsage))
                pure (agent0, Right sess1)

{- | Get the most recent partial turn from a session, if any.

Looks at the most recent turn and returns it if it's a PartialUserTurn.
-}
getPartialTurn :: Session -> Maybe PartialUserTurnContent
getPartialTurn session =
    case session.turns of
        (PartialUserTurn content _ : _) -> Just content
        _ -> Nothing

{- | Start a new async user turn.

Executes the first tool call (checking cache first), then either:
1. If more calls remain: creates a PartialUserTurn and pauses
2. If all calls complete: creates a complete UserTurn
-}
startNewAsyncTurn ::
    forall r.
    ConversationId ->
    Agent r ->
    Session ->
    SystemPrompt ->
    [SystemTool] ->
    Maybe UserQuery ->
    [LlmToolCall] ->
    IO (Agent r, Either r Session)
startNewAsyncTurn convId agent sess sPrompt sTools uQuery calls = do
    let ctx = buildContext agent sess convId
    -- Execute first call
    executeNextCall ctx agent sess sPrompt sTools uQuery [] calls

{- | Continue execution of a partial turn.

Takes the completed responses so far and the pending calls, executes
the next pending call, and updates the session accordingly.
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
    executeNextCall ctx agent sess partial.pUserPrompt partial.pUserTools partial.pUserQuery partial.pCompletedResponses partial.pPendingCalls

{- | Execute the next tool call in sequence.

Checks cache first, then executes if needed. Updates the session with
either a new partial turn or a complete user turn.
-}
executeNextCall ::
    forall r.
    ToolExecutionContext ->
    Agent r ->
    Session ->
    SystemPrompt ->
    [SystemTool] ->
    Maybe UserQuery ->
    [(LlmToolCall, UserToolResponse)] ->
    [LlmToolCall] ->
    IO (Agent r, Either r Session)
executeNextCall ctx agent sess sPrompt sTools uQuery completed pending =
    case pending of
        [] -> do
            -- All calls complete - finalize the turn
            let byteUsage = calculateUserTurnByteUsage sPrompt sTools uQuery (map snd completed)
            sess' <- addTurn sess (UserTurn (UserTurnContent sPrompt sTools uQuery completed) (Just byteUsage))
            pure (agent, Right sess')
        (nextCall : remaining) -> do
            -- Execute next call with cache check
            result <- executeToolCallWithCache agent ctx nextCall
            let newCompleted = completed ++ [(nextCall, result)]
            if null remaining
                then do
                    -- This was the last call - complete the turn
                    let byteUsage = calculateUserTurnByteUsage sPrompt sTools uQuery (map snd newCompleted)
                    sess' <- addTurn sess (UserTurn (UserTurnContent sPrompt sTools uQuery newCompleted) (Just byteUsage))
                    pure (agent, Right sess')
                else do
                    -- More calls remain - create partial turn
                    let partial = PartialUserTurnContent sPrompt sTools uQuery newCompleted remaining []
                    let byteUsage = calculatePartialTurnByteUsage sPrompt sTools uQuery newCompleted
                    sess' <- addTurn sess (PartialUserTurn partial (Just byteUsage))
                    pure (agent, Right sess')
  where
    addTurn s t = do
        tId <- newTurnId
        pure $ s{turns = t : s.turns, turnId = tId}

{- | Execute a tool call with optional cache lookup.

If a cache is configured, checks it first. If found, returns the cached
result. Otherwise executes the tool and optionally stores the result.
-}
executeToolCallWithCache :: Agent r -> ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
executeToolCallWithCache agent ctx call = do
    case agent.ctxToolCache of
        Just cache -> do
            let key = computeSimpleCacheKey call
            mCached <- cache.cacheLookup key
            case mCached of
                Just cached -> pure $ cached.crResult
                Nothing -> do
                    result <- agent.toolCall ctx call
                    -- Store in cache
                    now <- getCurrentTime
                    cache.cacheStore key $ CachedResult result now Nothing
                    pure result
        Nothing -> agent.toolCall ctx call

-- Simple cache key computation (tool name + argument hash)
computeSimpleCacheKey :: LlmToolCall -> Cache.CacheKey
computeSimpleCacheKey (LlmToolCall val) =
    let jsonStr = Text.pack $ show val
        hash = Text.take 64 jsonStr
     in Cache.CacheKey "tool" hash

-- Import needed for getCurrentTime
getCurrentTime :: IO Data.Time.UTCTime
getCurrentTime = Data.Time.getCurrentTime

{- | Build a ToolExecutionContext based on the agent's configuration.

The context is populated according to 'ContextConfig' settings:
* 'includeFullSession' controls whether 'ctxFullSession' is populated
* 'includeAgentId' controls whether 'ctxAgentId' is included (as Nothing or Just)

This uses the agent's 'ctxCallStack' to maintain the call chain for nested
agent invocations, supporting arbitrarily deep nesting of sub-conversations.

The context also includes the agent's 'ctxWorld' and 'ctxEventQueue' if present,
which enables subcall conversations to be visible in the TUI.
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
            }

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
calculatePartialTurnByteUsage :: SystemPrompt -> [SystemTool] -> Maybe UserQuery -> [(LlmToolCall, UserToolResponse)] -> StepByteUsage
calculatePartialTurnByteUsage sPrompt sTools uQuery completed =
    let inputBytes =
            systemPromptBytes sPrompt
                + toolsBytes sTools
                + userQueryBytes uQuery
        toolBytes = sum (map (userToolResponseBytes . snd) completed)
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
                    (PartialUserTurn partial _mUsage) -> do
                        -- Partial turn: continue with remaining pending calls
                        let sPrompt0 = partial.pUserPrompt
                        let sTools0 = partial.pUserTools
                        let uQuery0 = partial.pUserQuery
                        let tAnswers0 = partial.pCompletedResponses
                        -- Create completion with completed responses so far
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
                            -- No tool calls: stop
                            pure $ Stop (llmTurn, sess)
                        else
                            -- Has tool calls: continue with user prompt for tool responses
                            pure $ AskUserPrompt $ MissingUserPrompt False llmTurn.llmToolCalls
                PartialUserTurn partial _mUsage ->
                    -- Last turn was partial - need to continue execution
                    if null partial.pPendingCalls
                        then
                            -- No pending calls: treat like user turn completion
                            let sPrompt0 = partial.pUserPrompt
                                sTools0 = partial.pUserTools
                                uQuery0 = partial.pUserQuery
                                tAnswers0 = partial.pCompletedResponses
                             in pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist [] (Just sess.sessionId))
                        else
                            -- Still have pending calls: continue execution
                            pure $ AskUserPrompt $ MissingUserPrompt False partial.pPendingCalls
