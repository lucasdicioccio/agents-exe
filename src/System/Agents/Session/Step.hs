{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: propose some non-naive combinators
module System.Agents.Session.Step where

import Data.Void (Void)


import System.Agents.Session.Base

-- | Runs a single step of agent for a given session.
-- Agent may be modified, may decide to return a session, or decide to stop.
runStepM :: forall r. Agent r -> Session -> IO (Agent r, Either r Session)
runStepM agent sess =
    go agent sess
  where
    addTurn :: Session -> Turn -> IO Session
    addTurn sess0 turn = do
      tId <- newTurnId
      pure $ sess0 { turns = turn : sess0.turns, turnId = tId }

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
          results <- traverse agent0.toolCall missing.missingToolCalls
          let uToolResponses = zip missing.missingToolCalls results 
          sess1 <- addTurn sess0 (UserTurn $ UserTurnContent { userPrompt = sPrompt, userTools = sTools, userQuery = uQuery, userToolResponses = uToolResponses})
          pure (agent0, Right sess1)
        AskLlmCompletion completion -> do
          (llmRsp,llmTool) <- agent0.complete completion
          sess1 <- addTurn sess0 (LlmTurn $  LlmTurnContent llmRsp llmTool)
          pure (agent0, Right sess1)

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
        (turn:hist) ->
          case turn of
            (LlmTurn llmTurn) -> do
              pure $ AskUserPrompt (MissingUserPrompt True llmTurn.llmToolCalls)
            (UserTurn userTurn) -> do
              let sPrompt0 = userTurn.userPrompt
              let  sTools0 = userTurn.userTools
              let  uQuery0 = userTurn.userQuery
              let  tAnswers0 = userTurn.userToolResponses
              pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist)

-- | Step function that stops when the LLM returns no tool calls.
naiveTilNoToolCallStep :: Session -> IO (Action (LlmTurnContent, Session))
naiveTilNoToolCallStep sess = do
    case sess.turns of
        [] ->
            -- Initial state: need to ask the LLM for completion
            pure $ AskUserPrompt $ MissingUserPrompt True []
        (turn:hist) -> do
            case turn of
                UserTurn userTurn -> do
                    -- Last turn was user turn, ask LLM for completion
                    let sPrompt0 = userTurn.userPrompt
                    let sTools0 = userTurn.userTools
                    let uQuery0 = userTurn.userQuery
                    let tAnswers0 = userTurn.userToolResponses
                    pure $ AskLlmCompletion (LlmCompletion sPrompt0 sTools0 uQuery0 tAnswers0 hist)
                LlmTurn llmTurn ->
                    -- Last turn was LLM turn
                    if null llmTurn.llmToolCalls
                        then
                            -- No tool calls: stop
                            pure $ Stop (llmTurn, sess)
                        else
                            -- Has tool calls: continue with user prompt for tool responses
                            pure $ AskUserPrompt $ MissingUserPrompt False llmTurn.llmToolCalls

