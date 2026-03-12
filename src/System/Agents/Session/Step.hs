{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: propose some non-naive combinators
module System.Agents.Session.Step where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)

import Data.Void (Void)

import System.Agents.Base (ConversationId)
import System.Agents.Session.Base
import System.Agents.Session.Types (StepByteUsage, calculateStepByteUsage)
import System.Agents.ToolSchema (ParamProperty)
import System.Agents.Tools.Base (CallResult, callResultByteSize)
import System.Agents.Tools.Context (CallStackEntry (..), ToolExecutionContext, mkToolExecutionContext)

{- | Runs a single step of agent for a given session.
Agent may be modified, may decide to return a session, or decide to stop.
-}
runStepM :: forall r. ConversationId -> Agent r -> Session -> IO (Agent r, Either r Session)
runStepM convId agent sess =
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
                let ctx = buildContext agent0.contextConfig sess0 convId
                toolResponses <- traverse (agent0.toolCall ctx) missing.missingToolCalls
                let uToolResponses = zip missing.missingToolCalls toolResponses
                -- Calculate byte usage for this user turn
                let byteUsage = calculateUserTurnByteUsage sPrompt sTools uQuery toolResponses
                sess1 <- addTurn sess0 (UserTurn (UserTurnContent{userPrompt = sPrompt, userTools = sTools, userQuery = uQuery, userToolResponses = uToolResponses}) (Just byteUsage))
                pure (agent0, Right sess1)
            AskLlmCompletion completion -> do
                (llmRsp, llmTool) <- agent0.complete completion
                -- Calculate byte usage for this LLM turn
                let byteUsage = calculateLlmTurnByteUsage llmRsp llmTool
                sess1 <- addTurn sess0 (LlmTurn (LlmTurnContent llmRsp llmTool) (Just byteUsage))
                pure (agent0, Right sess1)

{- | Build a ToolExecutionContext based on the agent's configuration.

The context is populated according to 'ContextConfig' settings:
* 'includeFullSession' controls whether 'ctxFullSession' is populated
* 'includeAgentId' controls whether 'ctxAgentId' is included (as Nothing or Just)

This creates a root-level context with a single "root" entry in the call stack
at depth 0, and no recursion depth limit.
-}
buildContext :: ContextConfig -> Session -> ConversationId -> ToolExecutionContext
buildContext config sess convId =
    mkToolExecutionContext
        sess.sessionId
        convId
        sess.turnId
        (if config.includeAgentId then Nothing else Nothing) -- AgentId not available in Session, use Nothing
        (if config.includeFullSession then Just sess else Nothing)
        [CallStackEntry "root" convId 0] -- Root call stack entry
        Nothing -- No max recursion depth by default

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
     in calculateStepByteUsage inputBytes outputBytes reasoningBytes toolBytes

{- | Calculate byte usage for an LLM turn.

This includes:
* Output bytes: LLM response text
* Reasoning bytes: thinking/reasoning content
* Tool bytes: tool call definitions (payload sent to LLM)
-}
calculateLlmTurnByteUsage :: LlmResponse -> [LlmToolCall] -> StepByteUsage
calculateLlmTurnByteUsage llmRsp llmTools =
    let outputBytes = fromIntegral $ maybe 0 (LByteString.length . Aeson.encode . Aeson.String) llmRsp.responseText
        reasoningBytes = fromIntegral $ maybe 0 (LByteString.length . Aeson.encode . Aeson.String) llmRsp.responseThinking
        -- Tool call definitions are part of LLM output context
        toolBytes = sum (map toolCallBytes llmTools)
        -- LLM turns don't have input bytes (those are in the user turn)
        inputBytes = 0
     in calculateStepByteUsage inputBytes outputBytes reasoningBytes toolBytes

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
userQueryBytes (Just (UserQuery txt)) = Text.length txt * 4

-- | Calculate bytes for a tool call.
toolCallBytes :: LlmToolCall -> Int
toolCallBytes (LlmToolCall val) = fromIntegral (LByteString.length (Aeson.encode val))

-- | Calculate bytes for a user tool response.
userToolResponseBytes :: UserToolResponse -> Int
userToolResponseBytes (UserToolResponse val) = fromIntegral (LByteString.length (Aeson.encode val))

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
                        pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist)

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
                    pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist)
                LlmTurn llmTurn _mUsage ->
                    -- Last turn was LLM turn
                    if null llmTurn.llmToolCalls
                        then
                            -- No tool calls: stop
                            pure $ Stop (llmTurn, sess)
                        else
                            -- Has tool calls: continue with user prompt for tool responses
                            pure $ AskUserPrompt $ MissingUserPrompt False llmTurn.llmToolCalls
