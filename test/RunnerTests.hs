{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the session runner and the host.
module RunnerTests (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (open)
import Prod.Tracer (silent)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, testNode)
import System.Agents.AgentFactory
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (AgentId (..))
import qualified System.Agents.Base as Base
import System.Agents.Host
import System.Agents.Host.Runner
import System.Agents.Session.Async (ContinuationStore (..), mkSqliteContinuationStore)
import System.Agents.Session.Base hiding (SessionProgress (..))
import System.Agents.SessionStore
import System.Agents.ToolRegistration (ToolRegistration, registerIOScriptInLLM)
import qualified System.Agents.Tools.IO as IOTools

tests :: TestTree
tests =
    testGroup
        "Session runner"
        [ testCase "a deferred call completed with auto-resume leads to a final answer" deferredFlowTest
        , testCase "concurrent completions of one turn both land" concurrentCompletionsTest
        , testCase "messages are refused during a run and accepted after" postMessageTest
        , testCase "a background call started in one run is picked up by the next" engineKeptTest
        , testCase "cancelling stops the run and its background calls" cancelTest
        , testCase "sessions left running are recovered, their calls orphaned" recoveryTest
        , testCase "awaitRun waits for the run or the timeout" awaitRunTest
        , testCase "deleting cascades to sub-sessions and continuations; sub-sessions have their root's owner" deleteTest
        , testCase "idle sessions are evicted and come back on demand" evictionTest
        , testCase "withHost loads agents and serves sessions from a database file" withHostTest
        , testCase "a subscriber skips other sessions' events" subscribeFilterTest
        ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

deferredFlowTest :: Assertion
deferredFlowTest = do
    node <- testNode deferAll
    host <- testHost [node] (\_ c -> firstThen [remoteCall "call_1"] c)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "fetch it") (Just UntilBlocked)
        let sid = meta.smSessionId
        (blocked, active) <- expectRight =<< awaitRun runner sid 5
        (blocked.smStatus, active) @?= (StatusWaitingExternal, False)
        token <- singleToken runner sid
        next <- subscribe runner sid
        _ <- expectRight =<< completeCall runner token (TextResponse "42") True
        kinds <- eventsUntilStopped next
        assertBool ("events: " <> show kinds) (take 1 kinds == ["session.updated"] && "run.started" `elem` kinds && last kinds == "run.stopped")
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle
        sess <- currentSession runner sid
        assertBool "tool result delivered" ("42" `elem` responseTexts sess)
        again <- completeCall runner token (TextResponse "43") True
        again @?= Left (TokenAlreadyCompleted token)

concurrentCompletionsTest :: Assertion
concurrentCompletionsTest = do
    node <- testNode deferAll
    host <- testHost [node] (\_ c -> firstThen [remoteCall "call_1", remoteCall "call_2"] c)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "fetch both") (Just UntilBlocked)
        let sid = meta.smSessionId
        _ <- expectRight =<< awaitRun runner sid 5
        sess0 <- currentSession runner sid
        [t1, t2] <- pure [t | v <- pendingDeferredCalls sess0, Just t <- [v.dcvToken]]
        (r1, r2) <-
            concurrently
                (completeCall runner t1 (TextResponse "first") True)
                (completeCall runner t2 (TextResponse "second") True)
        _ <- expectRight r1
        _ <- expectRight r2
        waitUntil $ (== StatusIdle) . (.smStatus) . fst <$> (expectRight =<< awaitRun runner sid 5)
        sess <- currentSession runner sid
        let texts = responseTexts sess
        assertBool ("both results: " <> show texts) ("first" `elem` texts && "second" `elem` texts)

postMessageTest :: Assertion
postMessageTest = do
    gate <- newEmptyMVar
    node <- testNode "{}"
    host <- testHost [node] (\_ c -> readMVar gate >> mockCompletion c)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        let sid = meta.smSessionId
        refused <- postMessage runner sid (message "too early") (Just UntilBlocked)
        refused @?= Left (RunInProgress sid)
        putMVar gate ()
        (idle, _) <- expectRight =<< awaitRun runner sid 5
        idle.smStatus @?= StatusIdle
        _ <- expectRight =<< postMessage runner sid (message "and again") (Just UntilBlocked)
        (idle2, _) <- expectRight =<< awaitRun runner sid 5
        idle2.smStatus @?= StatusIdle
        sess <- currentSession runner sid
        length [() | LlmTurn{} <- sess.turns] @?= 2
        stillIdle <- postMessage runner sid (message "fine") Nothing
        fmap (.smStatus) stillIdle @?= Right StatusReady

engineKeptTest :: Assertion
engineKeptTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    host <- testHost [node] (\_ c -> firstThen [slowCall] c)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "go") (Just StepOnce)
        let sid = meta.smSessionId
        _ <- expectRight =<< awaitRun runner sid 5
        -- Second step: starts the background call, then yields while it runs.
        _ <- expectRight =<< resume runner sid StepOnce
        _ <- expectRight =<< awaitRun runner sid 5
        running <- currentSession runner sid
        assertBool "a call runs in the background" (hasRunningCall running)
        putMVar gate ()
        threadDelay 100_000
        _ <- expectRight =<< resume runner sid UntilBlocked
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle
        sess <- currentSession runner sid
        assertBool ("result of the background call: " <> show (responseTexts sess)) (any ("slow result" `Text.isInfixOf`) (responseTexts sess))

cancelTest :: Assertion
cancelTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    -- Late results reach the LLM as a message, not as tool responses, so the
    -- LLM must call the tool only once.
    complete <- onceThen [slowCall]
    host <- testHost [node] (const complete)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "go") (Just UntilBlocked)
        let sid = meta.smSessionId
        -- The LLM answers with a placeholder for the call, then the run waits
        -- for the call's late result.
        waitUntil $ hasBackgroundCall <$> currentSession runner sid
        cancelled <- expectRight =<< cancelRun runner sid
        cancelled.smStatus @?= StatusReady
        (_, active) <- expectRight =<< awaitRun runner sid 1
        active @?= False
        again <- cancelRun runner sid
        again @?= Left (NoActiveRun sid)
        -- The next run delivers the cancellation to the LLM.
        _ <- expectRight =<< resume runner sid UntilBlocked
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle
        sess <- currentSession runner sid
        assertBool ("call reported cancelled: " <> show (sessionTexts sess)) (any ("cancelled" `Text.isInfixOf`) (sessionTexts sess))

recoveryTest :: Assertion
recoveryTest = do
    node <- testNode "{}"
    host <- testHost [node] (\_ c -> mockCompletion c)
    sid <- newSessionId
    sess0 <- newSessionFromPrompt sid (SystemPrompt "sys") [] (UserQuery "hello" [])
    callId <- newToolCallId
    let running =
            TrackedToolCall callId slowCall Running Nothing Nothing (AppliedPolicy RunAsync Nothing) Nothing False
        llm = LlmTurn (LlmTurnContent (LlmResponse Nothing Nothing Aeson.Null Nothing) [slowCall]) Nothing
        partial = PartialUserTurn (PartialUserTurnContent (SystemPrompt "sys") [] Nothing [running]) Nothing
        sess = sess0{turns = partial : llm : sess0.turns}
    now <- getCurrentTime
    _ <- expectRight =<< host.hostBackend.sbCompareAndStore (freshSessionMeta sid now){smAgent = Just "test-agent", smStatus = StatusRunning} sess
    withSessionRunner host $ \runner -> do
        recovered <- recoverOnStartup runner
        recovered @?= [sid]
        Just (_, meta) <- getSession runner sid
        meta.smStatus @?= StatusReady
        _ <- expectRight =<< resume runner sid StepOnce
        _ <- expectRight =<< awaitRun runner sid 5
        resumed <- currentSession runner sid
        assertBool ("orphaned: " <> show (responseTexts resumed)) (any ("orphaned" `Text.isInfixOf`) (responseTexts resumed))

awaitRunTest :: Assertion
awaitRunTest = do
    gate <- newEmptyMVar
    node <- testNode "{}"
    host <- testHost [node] (\_ c -> readMVar gate >> mockCompletion c)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        let sid = meta.smSessionId
        (waiting, active) <- expectRight =<< awaitRun runner sid 0.1
        (waiting.smStatus, active) @?= (StatusRunning, True)
        putMVar gate ()
        (done, stillActive) <- expectRight =<< awaitRun runner sid 5
        (done.smStatus, stillActive) @?= (StatusIdle, False)
        missing <- newSessionId
        unknown <- awaitRun runner missing 0.1
        fmap fst unknown @?= Left (UnknownSession missing)

deleteTest :: Assertion
deleteTest = do
    gate <- newEmptyMVar
    child <- testNode "{\"slug\": \"child\"}"
    parent <-
        testNode
            "{\"slug\": \"parent\", \"executionMode\": \"asynchronous\", \"toolCallPolicyConfig\": {\"default\": {\"tag\": \"defer\", \"reason\": \"external\"}, \"rules\": [{\"tool\": \"io_prompt_agent_child\", \"disposition\": {\"tag\": \"runSync\"}}]}}"
    host <-
        testHost [parent] $ \node c ->
            if Base.slug node.osNodeConfig == "child"
                then readMVar gate >> mockCompletion c
                else firstThen [childCall, remoteCall "call_2"] c
    agentId <- AgentId <$> nextRandom
    atomically $ writeTVar parent.osNodeTools [OneShotTool.turnAgentRuntimeIntoIOTool silent host.hostSubAgentDeps child "parent" agentId]
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSessionAs runner (Just "alice") "parent" (message "delegate") (Just UntilBlocked)
        let sid = meta.smSessionId
            children = map (.smSessionId) <$> host.hostBackend.sbQuery allSessionsQuery{sqParent = Just sid}
        waitUntil $ not . null <$> children
        [childSid] <- children
        owners <- mapM (sessionOwner runner) [sid, childSid]
        owners @?= [Just (Just "alice"), Just (Just "alice")]
        busyParent <- deleteSession runner sid DryRun
        busyParent @?= Left (RunInProgress sid)
        busyChild <- deleteSession runner childSid DeleteForReal
        busyChild @?= Left (RunInProgress sid)
        putMVar gate ()
        (blocked, _) <- expectRight =<< awaitRun runner sid 5
        blocked.smStatus @?= StatusWaitingExternal
        plan <- expectRight =<< deleteSession runner sid DryRun
        plan @?= DeletionPlan [childSid, sid] 1 True
        stillThere <- mapM (getSession runner) [childSid, sid]
        length [() | Just _ <- stillThere] @?= 2
        rows <- host.hostContinuations.csCountSession sid
        rows @?= 1
        done <- expectRight =<< deleteSession runner sid DeleteForReal
        done @?= DeletionPlan [childSid, sid] 1 False
        gone <- mapM (getSession runner) [childSid, sid]
        length [() | Just _ <- gone] @?= 0
        host.hostContinuations.csCountSession sid >>= (@?= 0)
  where
    childCall = openAICall "call_1" "io_prompt_agent_child" "{\"what\": \"help\"}"

evictionTest :: Assertion
evictionTest = do
    node <- testNode "{}"
    host0 <- testHost [node] (\_ c -> mockCompletion c)
    let host = host0{hostLiveSessionTtl = 0.1}
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "hello") Nothing
        let sid = meta.smSessionId
        (.rsLiveSessions) <$> runnerStats runner >>= (@?= 1)
        waitUntil $ (== 0) . (.rsLiveSessions) <$> runnerStats runner
        _ <- expectRight =<< resume runner sid UntilBlocked
        (idle, _) <- expectRight =<< awaitRun runner sid 5
        idle.smStatus @?= StatusIdle

withHostTest :: Assertion
withHostTest =
    withSystemTempDirectory "agents-host" $ \dir -> do
        let agentFile = dir </> "smoke.json"
            keysFile = dir </> "keys.json"
            dbFile = dir </> "agents.db"
        writeFile agentFile $
            "{\"tag\": \"OpenAIAgentDescription\", \"contents\": {\"slug\": \"smoke\", \"apiKeyId\": \"none\", \"flavor\": \"OpenAIv1\", "
                <> "\"modelUrl\": \"http://127.0.0.1:1\", \"modelName\": \"mock\", \"announce\": \"smoke test\", "
                <> "\"systemPrompt\": [\"You are a test\"], \"builtinToolboxes\": [], \"mcpServers\": []}}"
        writeFile keysFile "{}"
        let cfg = (defaultHostConfig [agentFile] keysFile dbFile){hcCompletion = Just (const mockCompletion)}
        withHost cfg silent $ \host -> do
            Map.keys host.hostAgents @?= ["smoke"]
            withSessionRunner host $ \runner -> do
                meta <- expectRight =<< createSession runner "smoke" (message "hello") (Just UntilBlocked)
                (idle, _) <- expectRight =<< awaitRun runner meta.smSessionId 5
                idle.smStatus @?= StatusIdle
                unknown <- createSession runner "nobody" (message "hello") Nothing
                fmap (.smSessionId) unknown @?= Left (UnknownAgent "nobody")
        doesFileExist dbFile >>= assertBool "database file created"

subscribeFilterTest :: Assertion
subscribeFilterTest = do
    node <- testNode "{}"
    host <- testHost [node] (\_ c -> mockCompletion c)
    withSessionRunner host $ \runner -> do
        a <- expectRight =<< createSession runner "test-agent" (message "first") Nothing
        b <- expectRight =<< createSession runner "test-agent" (message "second") Nothing
        next <- subscribe runner b.smSessionId
        -- Session a's events come first in the stream.
        _ <- expectRight =<< resume runner a.smSessionId UntilBlocked
        _ <- expectRight =<< awaitRun runner a.smSessionId 5
        _ <- expectRight =<< resume runner b.smSessionId UntilBlocked
        kinds <- eventsUntilStopped next
        assertBool ("events of b: " <> show kinds) ("run.started" `elem` kinds)

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

-- | A host over in-memory SQLite, with the given agents and LLM.
testHost :: [OSAgentNode] -> (OSAgentNode -> Completion) -> IO Host
testHost nodes complete = do
    conn <- open ":memory:"
    backend <- mkSqliteSessionStore conn
    store <- mkSqliteContinuationStore conn
    let deps = (defaultAgentDeps []){adContinuationStore = Just store, adCompletion = Just complete}
    pure
        Host
            { hostAgents = Map.fromList [(Base.slug n.osNodeConfig, n) | n <- nodes]
            , hostDeps = deps
            , hostSubAgentDeps = deps{adSessionSink = SinkBackend backend}
            , hostBackend = backend
            , hostContinuations = store
            , hostTracer = silent
            , hostLiveSessionTtl = 15 * 60
            }

-- | Agent config: asynchronous, every call deferred to an external worker.
deferAll :: String
deferAll = "{\"executionMode\": \"asynchronous\", \"toolCallPolicyConfig\": {\"default\": {\"tag\": \"defer\", \"reason\": \"external\"}, \"rules\": []}}"

-- | Agent config: every call runs in the background; steps yield after 20ms.
backgroundAll :: String
backgroundAll =
    "{\"executionMode\": \"asynchronous\", \"asyncYieldStrategy\": {\"tag\": \"yieldOnTimeout\", \"milliseconds\": 20}, "
        <> "\"toolCallPolicyConfig\": {\"default\": {\"tag\": \"runAsync\"}, \"rules\": []}}"

-- | The first completion ever calls the tools; every other one answers.
onceThen :: [LlmToolCall] -> IO Completion
onceThen calls = do
    called <- newIORef False
    pure $ \completion -> do
        first <- atomicModifyIORef' called (\c -> (True, not c))
        if first
            then pure (LlmResponse Nothing Nothing Aeson.Null Nothing, calls)
            else mockCompletion completion

-- | The first completion of a turn calls the tools; the next one answers.
firstThen :: [LlmToolCall] -> Completion
firstThen calls completion
    | null completion.completeToolResponses = pure (LlmResponse Nothing Nothing Aeson.Null Nothing, calls)
    | otherwise = mockCompletion completion

openAICall :: Text -> Text -> Text -> LlmToolCall
openAICall callId name args =
    LlmToolCall $
        Aeson.object
            [ "id" Aeson..= callId
            , "type" Aeson..= ("function" :: Text)
            , "function" Aeson..= Aeson.object ["name" Aeson..= name, "arguments" Aeson..= args]
            ]

remoteCall :: Text -> LlmToolCall
remoteCall callId = openAICall callId "fetch_remote" "{}"

slowCall :: LlmToolCall
slowCall = openAICall "call_slow" "io_slow" "{}"

-- | A tool that answers once the gate is open.
gatedTool :: MVar () -> ToolRegistration
gatedTool gate =
    registerIOScriptInLLM
        (IOTools.IOScript (IOTools.IOScriptDescription "slow" "waits for the test") run)
        []
  where
    run _ctx (_ :: Aeson.Value) = do
        readMVar gate
        pure (CByteString.pack "slow result")

message :: Text -> NewMessage
message txt = NewMessage txt []

expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> assertFailure ("unexpected error: " <> show e)) pure

currentSession :: SessionRunner -> SessionId -> IO Session
currentSession runner sid =
    getSession runner sid >>= maybe (assertFailure "session missing") (pure . fst)

singleToken :: SessionRunner -> SessionId -> IO ContinuationToken
singleToken runner sid = do
    sess <- currentSession runner sid
    case [t | v <- pendingDeferredCalls sess, Just t <- [v.dcvToken]] of
        [t] -> pure t
        other -> assertFailure ("expected one pending call, got " <> show (length other))

hasRunningCall :: Session -> Bool
hasRunningCall sess =
    or [tc.tcState == Running | PartialUserTurn p _ <- take 1 sess.turns, tc <- p.pTrackedToolCalls]

-- | Whether a call runs in the background in any turn of the session.
hasBackgroundCall :: Session -> Bool
hasBackgroundCall sess =
    or [tc.tcState == Running | PartialUserTurn p _ <- sess.turns, tc <- p.pTrackedToolCalls]

-- | Tool results and user messages: everything the LLM was told.
sessionTexts :: Session -> [Text]
sessionTexts sess =
    responseTexts sess <> [q.queryText | UserTurn content _ <- sess.turns, Just q <- [content.userQuery]]

-- | The text of every tool result the session delivered to the LLM.
responseTexts :: Session -> [Text]
responseTexts sess =
    [render r | UserTurn content _ <- sess.turns, (_, r) <- content.userToolResponses]
  where
    render (TextResponse t) = t
    render other = Text.pack (show other)

-- | Event kinds until the run stops (or 5 seconds pass).
eventsUntilStopped :: IO SessionEvent -> IO [Text]
eventsUntilStopped next = go []
  where
    go acc = do
        mEvent <- timeout 5_000_000 next
        case mEvent of
            Nothing -> assertFailure ("no run.stopped after " <> show (reverse acc))
            Just event ->
                let acc' = sessionEventKind event : acc
                 in case event of
                        RunStopped{} -> pure (reverse acc')
                        _ -> go acc'

-- | Poll a condition every 20ms for up to 5 seconds.
waitUntil :: IO Bool -> Assertion
waitUntil check = go (250 :: Int)
  where
    go 0 = assertFailure "condition not reached in time"
    go n = do
        ok <- check
        if ok then pure () else threadDelay 20_000 >> go (n - 1)

