{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the traces of the async engine: which 'AsyncTrace' each
lifecycle step of a background tool call gives, that an agent built by
'buildAgent' and one run by the session runner (the server, and the TUI's
embedded runner) report them to the front-end's tracer, and how the server
log and the CLI's JSON log write them.
-}
module AsyncEngineTraceTests (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (throwIO)
import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (nil)
import Prod.Tracer (Tracer (..), runTracer, silent)
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, openFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockDeps, newConvId, testNode)
import AgentsServer.Log (hostTraceLogger, newHandleLogger)
import RunnerTests (backgroundAll, expectRight, firstThen, gatedTool, message, slowCall, testHost, waitUntil)
import qualified System.Agents.AgentFactory as AgentFactory
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import qualified System.Agents.Base as Base
import qualified System.Agents.CLI as CLI
import qualified System.Agents.CLI.TUI as TUICmd
import System.Agents.Host (Host (..), HostTrace (..))
import System.Agents.Host.Runner (RunMode (..), awaitRun, createSession, resume, withSessionRunner)
import System.Agents.OS.Conversation.ToolCalls (createToolCallEntity, registerToolCallComponents)
import System.Agents.OS.Core.World (World, newWorld)
import System.Agents.Session.Async.Engine
import System.Agents.Session.Base (Agent (..))
import System.Agents.Session.Types (
    AppliedPolicy (..),
    LlmToolCall (..),
    SessionId (..),
    ToolCallDisposition (..),
    ToolCallId,
    ToolCallState (..),
    TrackedToolCall (..),
    TurnId (..),
    UserToolResponse (..),
    newToolCallId,
 )
import System.Agents.SessionStore (SessionMeta (..))
import System.Agents.Tools.Context (ToolExecutionContext (..), mkMinimalContext)

tests :: TestTree
tests =
    testGroup
        "Async engine traces"
        [ testGroup
            "engine"
            [ testCase "a call that returns is queued, started, then completed" completedCallTrace
            , testCase "a progress payload is traced" progressTrace
            , testCase "a call that throws is traced as failed, with the error" failedCallTrace
            , testCase "a call that outlives the timeout is traced as timed out" timedOutCallTrace
            , testCase "cancelToolCall traces the call as cancelled, once" cancelledCallTrace
            , testCase "shutdown traces running calls as cancelled, not finished ones" shutdownTrace
            ]
        , testGroup
            "wiring"
            [ testCase "buildAgent hands the engine the front-end's tracer" factoryTracerIsWired
            , testCase "a runner session reports its background calls to the host tracer" runnerTracesBackgroundCalls
            ]
        , testGroup
            "log lines"
            [ testCase "the fields name the session, the conversation and the call" traceFields
            , testCase "the server logs one JSON line per step, with session and call ids" serverLogLines
            , testCase "the CLI's JSON log (TUI, run, mcp-server) keeps the same steps" cliJsonLog
            ]
        ]

-------------------------------------------------------------------------------
-- Engine
-------------------------------------------------------------------------------

completedCallTrace :: Assertion
completedCallTrace = do
    (traces, tracer) <- recordingTracer
    world <- mkWorld
    engine <- mkAsyncEngine tracer world (\_ _ -> pure (TextResponse "ok")) 2 Nothing Nothing
    tc <- mkTrackedCall world "quick"
    batch <- startAsyncBatch engine baseContext [tc]
    waitAllCompleted batch
    got <- readIORef traces
    map eventName got @?= ["queued", "started", "completed"]
    -- Every step names the session, the conversation, the call and the tool.
    map traceInfo got
        @?= replicate
            3
            AsyncCallInfo
                { aciSessionId = testSessionId
                , aciConversationId = testConversationId
                , aciToolCallId = tc.tcId
                , aciProviderCallId = Just "call_quick"
                , aciToolName = "quick"
                }

progressTrace :: Assertion
progressTrace = do
    (traces, tracer) <- recordingTracer
    world <- mkWorld
    let payload = object ["percent" .= (50 :: Int)]
    let executor ctx _ = do
            mapM_ ($ payload) (ctxProgressCallback ctx)
            pure (TextResponse "ok")
    engine <- mkAsyncEngine tracer world executor 2 Nothing Nothing
    tc <- mkTrackedCall world "reports"
    batch <- startAsyncBatch engine baseContext [tc]
    waitAllCompleted batch
    got <- readIORef traces
    map traceEvent got !! 2 @?= AsyncCallProgressed payload
    map eventName got @?= ["queued", "started", "progressed", "completed"]

failedCallTrace :: Assertion
failedCallTrace = do
    (traces, tracer) <- recordingTracer
    world <- mkWorld
    engine <- mkAsyncEngine tracer world (\_ _ -> throwIO (userError "tool exploded")) 2 Nothing Nothing
    tc <- mkTrackedCall world "breaks"
    batch <- startAsyncBatch engine baseContext [tc]
    waitAllCompleted batch
    got <- readIORef traces
    map eventName got @?= ["queued", "started", "failed"]
    case traceEvent (last got) of
        -- The text of the exception, which GHC may follow with a backtrace.
        AsyncCallFailed _ err -> assertBool (show err) ("user error (tool exploded)" `Text.isPrefixOf` err)
        other -> assertFailure ("expected a failure, got " <> show other)

timedOutCallTrace :: Assertion
timedOutCallTrace = do
    (traces, tracer) <- recordingTracer
    world <- mkWorld
    engine <- mkAsyncEngine tracer world (sleepExecutor 10000000) 2 (Just 1) Nothing
    tc <- mkTrackedCall world "hangs"
    batch <- startAsyncBatch engine baseContext [tc]
    waitAllCompleted batch
    got <- readIORef traces
    map eventName got @?= ["queued", "started", "timed_out"]
    traceEvent (last got) @?= AsyncCallTimedOut 1

cancelledCallTrace :: Assertion
cancelledCallTrace = do
    (traces, tracer) <- recordingTracer
    world <- mkWorld
    engine <- mkAsyncEngine tracer world (sleepExecutor 10000000) 2 Nothing Nothing
    tc <- mkTrackedCall world "slow"
    _ <- startAsyncBatch engine baseContext [tc]
    waitUntil (elem "started" . map eventName <$> readIORef traces)
    cancelToolCall engine tc.tcId >>= (@?= True)
    -- A second attempt finds nothing to cancel and traces nothing.
    cancelToolCall engine tc.tcId >>= (@?= False)
    got <- readIORef traces
    map eventName got @?= ["queued", "started", "cancelled"]

shutdownTrace :: Assertion
shutdownTrace = do
    (traces, tracer) <- recordingTracer
    world <- mkWorld
    let executor _ call
            | callToolName call == "slow" = threadDelay 10000000 >> pure (TextResponse "late")
            | otherwise = pure (TextResponse "ok")
    engine <- mkAsyncEngine tracer world executor 2 Nothing Nothing
    slow <- mkTrackedCall world "slow"
    quick <- mkTrackedCall world "quick"
    -- The quick call ends but is not collected: it stays in the batch.
    _ <- startAsyncBatch engine baseContext [slow, quick]
    waitUntil (elem (quick.tcId, "completed") . map idAndName <$> readIORef traces)
    waitUntil (elem (slow.tcId, "started") . map idAndName <$> readIORef traces)
    shutdownAsyncEngine engine
    got <- readIORef traces
    [name | (callId, name) <- map idAndName got, callId == slow.tcId] @?= ["queued", "started", "cancelled"]
    [name | (callId, name) <- map idAndName got, callId == quick.tcId] @?= ["queued", "started", "completed"]
  where
    idAndName t = ((traceInfo t).aciToolCallId, eventName t)

-------------------------------------------------------------------------------
-- Wiring
-------------------------------------------------------------------------------

{- | The tracer given to 'buildAgent' is the one the agent hands to the
engine it creates: what the agent traces arrives wrapped in
'AgentFactory.AsyncToolCallTrace'.
-}
factoryTracerIsWired :: Assertion
factoryTracerIsWired = do
    (traces, tracer) <- recordingTracer
    node <- testNode "{}"
    convId <- newConvId
    agent <- AgentFactory.buildAgent tracer mockDeps AgentFactory.RootAgent convId node
    callId <- newToolCallId
    let trace = AsyncCallTrace (sampleInfo callId) AsyncCallQueued
    runTracer agent.ctxAsyncTracer trace
    got <- readIORef traces
    [t | AgentFactory.AsyncToolCallTrace t <- got] @?= [trace]

{- | A session run by the session runner ('System.Agents.Host.Runner', which
the server and the TUI's embedded runner both use) reports a background
call's steps to 'hostTracer', with the session's id.
-}
runnerTracesBackgroundCalls :: Assertion
runnerTracesBackgroundCalls = do
    (traces, tracer) <- recordingTracer
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    host0 <- testHost [node] (\_ c -> firstThen [slowCall] c)
    let host = host0{hostTracer = tracer}
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner (Base.slug node.osNodeConfig) (message "go") (Just StepOnce)
        let sid = meta.smSessionId
        _ <- expectRight =<< awaitRun runner sid 5
        -- Second step: starts the background call, then yields while it runs.
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
        _ <- expectRight =<< awaitRun runner sid 5
        waitUntil (elem "started" . map eventName . asyncTraces <$> readIORef traces)
        putMVar gate ()
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
        _ <- expectRight =<< awaitRun runner sid 5
        waitUntil (elem "completed" . map eventName . asyncTraces <$> readIORef traces)
        got <- asyncTraces <$> readIORef traces
        map eventName got @?= ["queued", "started", "completed"]
        map ((.aciSessionId) . traceInfo) got @?= replicate 3 sid
        map ((.aciToolName) . traceInfo) got @?= replicate 3 "io_slow"
        map ((.aciProviderCallId) . traceInfo) got @?= replicate 3 (Just "call_slow")
  where
    asyncTraces :: [HostTrace] -> [AsyncTrace]
    asyncTraces hostTraces = [t | HostAgentTrace (AgentFactory.AsyncToolCallTrace t) <- hostTraces]

-------------------------------------------------------------------------------
-- Log lines
-------------------------------------------------------------------------------

traceFields :: Assertion
traceFields = do
    callId <- newToolCallId
    let info = sampleInfo callId
    let ids =
            [ "session_id" .= testSessionId
            , "conversation_id" .= testConversationId
            , "tool_call_id" .= callId
            , "tool" .= ("run_tests" :: Text)
            , "provider_call_id" .= ("call_abc" :: Text)
            ]
    let line event = (asyncTraceKind t, object (asyncTraceFields t))
          where
            t = AsyncCallTrace info event
    line AsyncCallQueued @?= ("tool_call.queued", object ids)
    line (AsyncCallStarted 12) @?= ("tool_call.started", object (ids <> ["queued_ms" .= (12 :: Int)]))
    line (AsyncCallCompleted 340) @?= ("tool_call.completed", object (ids <> ["elapsed_ms" .= (340 :: Int)]))
    line (AsyncCallFailed 7 "boom")
        @?= ("tool_call.failed", object (ids <> ["elapsed_ms" .= (7 :: Int), "error" .= ("boom" :: Text)]))
    line (AsyncCallTimedOut 30) @?= ("tool_call.timed_out", object (ids <> ["timeout_seconds" .= (30 :: Int)]))
    line AsyncCallCancelled @?= ("tool_call.cancelled", object ids)
    -- A progress payload is tool output: only its size is logged.
    line (AsyncCallProgressed (toJSON ("secret output" :: Text)))
        @?= ("tool_call.progressed", object (ids <> ["payload_bytes" .= (15 :: Int)]))
    -- Without a provider id, the field is left out.
    let anonymous = AsyncCallTrace info{aciProviderCallId = Nothing} AsyncCallQueued
    lookup "provider_call_id" (asyncTraceFields anonymous) @?= Nothing

serverLogLines :: Assertion
serverLogLines = withSystemTempDirectory "async-trace-log" $ \dir -> do
    callId <- newToolCallId
    let path = dir </> "server.log"
    let trace = AgentFactory.AsyncToolCallTrace . AsyncCallTrace (sampleInfo callId)
    h <- openFile path WriteMode
    logger <- newHandleLogger h
    let tracer = hostTraceLogger logger
    runTracer tracer (HostAgentTrace (trace (AsyncCallStarted 3)))
    runTracer tracer (HostSubAgentTrace (OneShotTool.OneShotTrace (trace (AsyncCallTimedOut 30))))
    hClose h
    logged <- mapMaybe Aeson.decode . LBS8.lines <$> LBS8.readFile path
    map (field "kind") logged @?= [Just "tool_call.started", Just "tool_call.timed_out"]
    map (field "session_id") logged @?= replicate 2 (Just (toJSON testSessionId))
    map (field "tool_call_id") logged @?= replicate 2 (Just (toJSON callId))
    map (field "tool") logged @?= replicate 2 (Just "run_tests")
    map (field "queued_ms") logged @?= [Just (toJSON (3 :: Int)), Nothing]
    map (field "sub_agent") logged @?= [Nothing, Just (toJSON True)]

cliJsonLog :: Assertion
cliJsonLog = do
    callId <- newToolCallId
    let trace = AgentFactory.AsyncToolCallTrace (AsyncCallTrace (sampleInfo callId) AsyncCallCancelled)
    let subAgentTrace = OneShotTool.OneShotTrace trace
    -- The TUI's embedded runner, a top-level agent and a sub-agent.
    let tui = CLI.toJsonTrace (CLI.TUICmdTrace (TUICmd.HostTrace (HostAgentTrace trace)))
    let tuiSub = CLI.toJsonTrace (CLI.TUICmdTrace (TUICmd.HostTrace (HostSubAgentTrace subAgentTrace)))
    fmap (field "kind") tui @?= Just (Just "tool_call.cancelled")
    fmap (field "session_id") tui @?= Just (Just (toJSON testSessionId))
    fmap (field "tool_call_id") tui @?= Just (Just (toJSON callId))
    fmap (field "sub_agent") tui @?= Just Nothing
    fmap (field "sub_agent") tuiSub @?= Just (Just (toJSON True))
    -- Traces that had no JSON form before still have none.
    CLI.toJsonTrace (CLI.TUICmdTrace (TUICmd.HostTrace (HostRecoveredSessions []))) @?= Nothing

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

-- | A tracer keeping what it is given, oldest first.
recordingTracer :: IO (IORef [a], Tracer IO a)
recordingTracer = do
    ref <- newIORef []
    pure (ref, Tracer $ \t -> atomicModifyIORef' ref (\ts -> (ts ++ [t], ())))

traceInfo :: AsyncTrace -> AsyncCallInfo
traceInfo (AsyncCallTrace info _) = info

traceEvent :: AsyncTrace -> AsyncCallEvent
traceEvent (AsyncCallTrace _ event) = event

-- | The step, without what it carries.
eventName :: AsyncTrace -> Text
eventName t = case traceEvent t of
    AsyncCallQueued -> "queued"
    AsyncCallStarted _ -> "started"
    AsyncCallProgressed _ -> "progressed"
    AsyncCallCompleted _ -> "completed"
    AsyncCallFailed _ _ -> "failed"
    AsyncCallTimedOut _ -> "timed_out"
    AsyncCallCancelled -> "cancelled"

field :: Text -> Value -> Maybe Value
field name (Aeson.Object o) = KeyMap.lookup (Key.fromText name) o
field _ _ = Nothing

testSessionId :: SessionId
testSessionId = SessionId nil

testConversationId :: Base.ConversationId
testConversationId = Base.ConversationId nil

sampleInfo :: ToolCallId -> AsyncCallInfo
sampleInfo callId =
    AsyncCallInfo
        { aciSessionId = testSessionId
        , aciConversationId = testConversationId
        , aciToolCallId = callId
        , aciProviderCallId = Just "call_abc"
        , aciToolName = "run_tests"
        }

mkWorld :: IO World
mkWorld = atomically $ newWorld >>= registerToolCallComponents

baseContext :: ToolExecutionContext
baseContext = mkMinimalContext testSessionId testConversationId (TurnId nil) (\_ _ -> fail "no portal in this test")

-- | A running call of the named tool, with its OS entity.
mkTrackedCall :: World -> Text -> IO TrackedToolCall
mkTrackedCall world toolName = do
    callId <- newToolCallId
    eid <- createToolCallEntity world testSessionId testConversationId (TurnId nil) Nothing toolName (object []) callId
    pure
        TrackedToolCall
            { tcId = callId
            , tcCall =
                LlmToolCall $
                    object
                        [ "id" .= ("call_" <> toolName)
                        , "type" .= ("function" :: Text)
                        , "function" .= object ["name" .= toolName, "arguments" .= ("{}" :: Text)]
                        ]
            , tcState = Running
            , tcResult = Nothing
            , tcContinuation = Nothing
            , tcPolicy = AppliedPolicy (RunAsync Nothing) Nothing
            , tcEntityId = Just eid
            , tcDeliveredLate = False
            , tcAttachDeadline = Nothing
            , tcDetachedReason = Nothing
            , tcChildSessionId = Nothing
            }

callToolName :: LlmToolCall -> Text
callToolName (LlmToolCall (Aeson.Object o))
    | Just (Aeson.Object f) <- KeyMap.lookup "function" o
    , Just (Aeson.String n) <- KeyMap.lookup "name" f =
        n
callToolName _ = "unknown"

sleepExecutor :: Int -> ToolExecutionContext -> LlmToolCall -> IO UserToolResponse
sleepExecutor micros _ _ = threadDelay micros >> pure (TextResponse "slept")

-- | Block until every call of the batch ended.
waitAllCompleted :: AsyncBatch -> IO ()
waitAllCompleted batch = do
    update <- waitForProgress batch
    if null update.abuStillRunning then pure () else waitAllCompleted batch
