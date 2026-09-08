{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Self-contained durable workflow demonstrator.

This executable exercises the durable-workflow machinery without needing a
real LLM.  It simulates an agent turn that issues three tool calls:

1. @fetch_local@  — executed synchronously in-process.
2. @fetch_remote_a@ — deferred (e.g. waiting for an external system).
3. @fetch_remote_b@ — deferred.

The program:

* starts a session whose latest turn is the LLM request with those three
  calls;
* runs one asynchronous step, which completes the local call and yields
  two continuation tokens;
* prints the yielded state;
* "wakes" the session with the two deferred results;
* resumes execution, which drives the mock LLM to produce a final answer;
* prints the final response.

Build and run with:

> cabal run durable-workflow-demo

The source is intentionally small and heavily commented so it can be used
as a starting point for custom durable agents.
-}
module Main where

import Control.Monad (forM, forM_)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.UUID as UUID

import System.Agents.Base (ConversationId (..), newConversationId)
import System.Agents.Session.Base
import System.Agents.Session.Step (getPartialTurn, naiveTilNoToolCallStep, runStepM)
import System.Agents.Session.Wake (resumeSession, wakeSession)
import System.Agents.Tools.Context (CallStackEntry (..), ToolExecutionContext, ToolPortal, ToolResult (..))

-------------------------------------------------------------------------------
-- Mock data
-------------------------------------------------------------------------------

-- | Three fake tool calls issued by the simulated LLM.
localCall, remoteCallA, remoteCallB :: LlmToolCall
localCall  = mkCall "fetch_local"     "lookup user profile"
remoteCallA = mkCall "fetch_remote_a" "query service A"
remoteCallB = mkCall "fetch_remote_b" "query service B"

allCalls :: [LlmToolCall]
allCalls = [localCall, remoteCallA, remoteCallB]

-- | Build a minimal 'LlmToolCall' value from a tool name and argument text.
mkCall :: Text -> Text -> LlmToolCall
mkCall toolName arg =
    LlmToolCall $
        Aeson.object
            [ "function" .= Aeson.object ["name" .= toolName]
            , "arguments" .= Aeson.object ["query" .= arg]
            ]

-- | Extract the function name from a tool call.
callName :: LlmToolCall -> Text
callName (LlmToolCall val) =
    case val of
        Aeson.Object obj ->
            case KeyMap.lookup "function" obj of
                Just (Aeson.Object func) ->
                    case KeyMap.lookup "name" func of
                        Just (Aeson.String n) -> n
                        _ -> "unknown"
                _ -> "unknown"
        _ -> "unknown"

-- | Format a continuation token for display.
formatToken :: ContinuationToken -> Text
formatToken (ContinuationToken uuid) = UUID.toText uuid

-- | Minimal tool portal for agents that do not need inter-toolbox calls.
dummyPortal :: ToolPortal
dummyPortal _ _ =
    pure $
        ToolResult
            { resultData = Aeson.object [("error", Aeson.String "portal not used in demo")]
            , resultDuration = 0
            , resultTraceId = "demo"
            }

-------------------------------------------------------------------------------
-- Mock agent
-------------------------------------------------------------------------------

-- | A deterministic "LLM" that returns the three tool calls on the first
-- completion request and a final text response on the second request.
mockComplete :: LlmCompletion -> IO (LlmResponse, [LlmToolCall])
mockComplete completion
    | null completion.completeToolResponses =
        -- First request: the LLM emits three parallel tool calls.
        pure (emptyResponse, allCalls)
    | otherwise =
        -- Second request: all tool results are in, produce a final answer.
        let summary =
                Text.unlines $
                    "All three calls are complete. Results:"
                        : [ "- " <> callName c <> ": " <> renderResponse r
                          | (c, r) <- completion.completeToolResponses
                          ]
         in pure (LlmResponse (Just summary) Nothing Aeson.Null Nothing, [])
  where
    emptyResponse = LlmResponse Nothing Nothing Aeson.Null Nothing
    renderResponse (TextResponse txt) = txt
    renderResponse other = Text.pack (show other)

-- | A mock tool executor.  It runs in-process and pretends to perform work.
mockToolCall :: ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
mockToolCall _ctx call = do
    let toolName = callName call
    Text.putStrLn $ "  [executing in-process] " <> toolName
    pure $ TextResponse $ "result from " <> toolName

-- | Build the durable agent used for the demo.
--
-- The policy is the interesting part: it decides, per call, whether to run
-- synchronously or to defer.  Decorators such as timeouts or retries can be
-- wrapped around the base dispositions using 'Decorate'.
mkDemoAgent :: ConversationId -> IO (Agent (LlmTurnContent, Session))
mkDemoAgent convId = do
    let policy _ctx call
            | callName call == "fetch_local" = RunSync
            | otherwise = Defer (Reason "waiting for external service")
    pure $
        Agent
            { step = naiveTilNoToolCallStep
            , sysPrompt = pure $ SystemPrompt "You are a durable workflow demonstrator."
            , sysTools = pure []
            , usrQuery = pure Nothing
            , toolCall = mockToolCall
            , toolPortal = dummyPortal
            , complete = mockComplete
            , contextConfig = defaultContextConfig
            , ctxWorld = Nothing
            , ctxEventQueue = Nothing
            , ctxCallStack = [CallStackEntry "durable-demo" convId 0]
            , ctxParentConversation = Nothing
            , ctxExecutionMode = Asynchronous
            , ctxToolCache = Nothing
            , ctxToolCallPolicy = policy
            , ctxToolExecutor = Nothing
            , ctxContinuationStore = Nothing
            , ctxDeploymentRunner = Nothing
            , ctxSessionBackend = Nothing
            }

-------------------------------------------------------------------------------
-- Session setup
-------------------------------------------------------------------------------

-- | Create a fresh session whose latest turn is the LLM turn with the three
-- mock tool calls.
mkDemoSession :: IO Session
mkDemoSession = do
    sid <- newSessionId
    tid <- newTurnId
    pure $
        Session
            { turns =
                [ LlmTurn
                    ( LlmTurnContent
                        { llmResponse = LlmResponse Nothing Nothing Aeson.Null Nothing
                        , llmToolCalls = allCalls
                        }
                    )
                    Nothing
                ]
            , sessionId = sid
            , forkedFromSessionId = Nothing
            , turnId = tid
            , sessionVersion = Just 2
            , sessionExecutionMode = Just Asynchronous
            }

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

printYieldedState :: Session -> IO ()
printYieldedState session =
    case getPartialTurn session of
        Nothing -> Text.putStrLn "No partial turn."
        Just partial -> do
            let completed = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Completed]
            let deferred = [tc | tc <- partial.pTrackedToolCalls, tc.tcState == Deferred]
            Text.putStrLn "Yielded partial turn:"
            Text.putStrLn $ "  completed: " <> Text.pack (show $ length completed)
            forM_ completed $ \tc ->
                Text.putStrLn $ "    - " <> callName tc.tcCall <> ": " <> resultText tc.tcResult
            Text.putStrLn $ "  deferred:  " <> Text.pack (show $ length deferred)
            forM_ deferred $ \tc ->
                forM_ tc.tcContinuation $ \token ->
                    Text.putStrLn $ "    - " <> callName tc.tcCall <> " token: " <> formatToken token
  where
    resultText (Just (TextResponse txt)) = txt
    resultText _ = "<non-text result>"

printFinalAnswer :: LlmTurnContent -> IO ()
printFinalAnswer llmTurn = do
    Text.putStrLn "\n=== Final LLM response ==="
    Text.putStrLn $ fromMaybe "<no text>" llmTurn.llmResponse.responseText

-------------------------------------------------------------------------------
-- Main flow
-------------------------------------------------------------------------------

main :: IO ()
main = do
    Text.putStrLn "Durable workflow demonstrator"
    Text.putStrLn "=============================\n"

    convId <- newConversationId
    agent <- mkDemoAgent convId
    session0 <- mkDemoSession

    Text.putStrLn "Step 1: run the async scheduler on the LLM turn."
    (_agent, step1) <- runStepM convId agent session0
    session1 <- case step1 of
        Left _ -> error "Expected the session to yield, not complete."
        Right s -> pure s

    printYieldedState session1

    -- Collect deferred calls and their continuation tokens.
    partial <- case getPartialTurn session1 of
        Nothing -> error "Expected a partial turn after the first async step."
        Just p -> pure p

    let deferred =
            [ (fromJust tc.tcContinuation, tc.tcCall)
            | tc <- partial.pTrackedToolCalls
            , tc.tcState == Deferred
            ]

    Text.putStrLn "\nStep 2: complete the deferred calls from the outside world."
    externalResults <- forM deferred $ \(token, call) -> do
        let answer = TextResponse $ "external result for " <> callName call
        Text.putStrLn $ "  providing result for token " <> formatToken token
        pure (token, answer)

    session2 <- wakeSession session1 externalResults
    case getPartialTurn session2 of
        Nothing -> Text.putStrLn "Turn is now complete.\n"
        Just _ -> error "Expected the partial turn to become a full user turn."

    Text.putStrLn "Step 3: resume the session until the LLM produces a final answer."
    final <- resumeSession convId agent session2
    case final of
        Left (llmTurn, _session) -> printFinalAnswer llmTurn
        Right _ -> error "Expected the session to complete after resume."

