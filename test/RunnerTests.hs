{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the session runner and the host.
module RunnerTests (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple (open)
import Prod.Tracer (Tracer (..), silent)
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
import System.Agents.OneShot (oneShotSpawnSession)
import System.Agents.Session.Async (ContinuationStore (..), mkSqliteContinuationStore)
import System.Agents.Session.Base hiding (SessionProgress (..))
import System.Agents.Session.Mailbox (Mailbox (..), MailboxInfo (..), MailRouter (..), newInMemoryMailbox, newMailRouter)
import System.Agents.Session.MailStore (mkSqliteMailStore)
import System.Agents.SessionStore
import System.Agents.ToolRegistration (ToolRegistration, registerIOScriptInLLM)
import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.Context (ToolExecutionContext (..), ToolPortal, ToolResult (..), mkMinimalContext)
import qualified System.Agents.Tools.Context as Ctx
import qualified System.Agents.Tools.SystemToolbox.Mail as Mail
import System.Agents.Tools.SystemToolbox.Types (
    QueryError (..),
    SendMessageParams (..),
    SendMessageResult (..),
    SpawnSessionParams (..),
    SpawnSessionResult (..),
 )

tests :: TestTree
tests =
    testGroup
        "Session runner"
        [ testCase "a deferred call completed with auto-resume leads to a final answer" deferredFlowTest
        , testCase "concurrent completions of one turn both land" concurrentCompletionsTest
        , testCase "messages are refused during a run and accepted after" postMessageTest
        , testCase "an interrupt message is posted as Interrupt-priority mail" postMessageInterruptTest
        , testCase "a background call started in one run is picked up by the next" engineKeptTest
        , testCase "cancelling stops the run and its background calls" cancelTest
        , testCase "sessions left running are recovered, their calls orphaned" recoveryTest
        , testCase "awaitRun waits for the run or the timeout" awaitRunTest
        , testCase "deleting cascades to sub-sessions and continuations; sub-sessions have their root's owner" deleteTest
        , testCase "idle sessions are evicted and come back on demand" evictionTest
        , testCase "withHost loads agents and serves sessions from a database file" withHostTest
        , testCase "a subscriber skips other sessions' events" subscribeFilterTest
        , testCase "stored agents survive a restart; a file agent hides a stored one" storedAgentsTest
        , testCase "the server's MailRouter resolves a stored session on demand" serverMailRouterTest
        , testCase "send-message delivers agent-to-agent mail the recipient can read" sendMessageTest
        , testCase "send-message refuses a recipient outside the sender's subtree" sendMessageScopeDenialTest
        , testCase "a send-message reply carries hops one past the mail it answers" sendMessageHopsTest
        , testCase "spawn-session starts a durable child session that answers by mail" spawnSessionTest
        , testCase "Control Pause stops a run and leaves calls running by default" controlPauseTest
        , testCase "pauseCancelsCalls makes Control Pause cancel attached calls" controlPauseCancelsCallsTest
        , testCase "a paused session refuses ordinary messages by default" controlPauseRefusesMessagesByDefaultTest
        , testCase "resumeOnAnyMail lets postMessage wake a paused session" controlResumeOnAnyMailTest
        , testCase "wakeOn excluding \"user\" still refuses an ordinary message" wakeOnExcludesUserTest
        , testCase "Control (CancelCalls ids) cancels one attached call" controlCancelCallsTest
        , testCase "a background call reports tool.started and tool.completed events" toolCallEventsTest
        , testCase "watch-session forwards a matching tool.completed event as mail" watchSessionTest
        , testCase "watch-session does not forward a non-matching event" watchSessionFilterTest
        , testCase "unwatch-session stops forwarding" unwatchSessionTest
        , testCase "watch-session refuses a target outside the watcher's subtree" watchSessionScopeDenialTest
        , testCase "list-sessions merges live MailRouter entries with the persisted catalog" listSessionsMergeTest
        , testCase "run's spawn-session resolves a helper slug into a registered child session" oneShotSpawnSessionTest
        , testCase "run's spawn-session refuses an unknown helper slug" oneShotSpawnSessionUnknownSlugTest
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
        _ <- expectRight =<< completeCall runner token (TextResponse "42") True Map.empty
        kinds <- eventsUntilStopped next
        assertBool ("events: " <> show kinds) (take 1 kinds == ["session.updated"] && "run.started" `elem` kinds && last kinds == "run.stopped")
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle
        sess <- currentSession runner sid
        assertBool "tool result delivered" ("42" `elem` responseTexts sess)
        again <- completeCall runner token (TextResponse "43") True Map.empty
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
                (completeCall runner t1 (TextResponse "first") True Map.empty)
                (completeCall runner t2 (TextResponse "second") True Map.empty)
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
        -- Phase 3 (@todos/session-mailbox.md@, G2): a busy session now
        -- accepts mail instead of refusing it; it is folded in at the run's
        -- next receive point rather than added as a turn right away.
        accepted <- postMessage runner sid (message "too early") (Just UntilBlocked) Map.empty
        either (\e -> assertFailure ("expected mail to be accepted, got " <> show e)) (const (pure ())) accepted
        putMVar gate ()
        (idle, _) <- expectRight =<< awaitRun runner sid 5
        idle.smStatus @?= StatusIdle
        _ <- expectRight =<< postMessage runner sid (message "and again") (Just UntilBlocked) Map.empty
        (idle2, _) <- expectRight =<< awaitRun runner sid 5
        idle2.smStatus @?= StatusIdle
        sess <- currentSession runner sid
        length [() | LlmTurn{} <- sess.turns] @?= 2
        stillIdle <- postMessage runner sid (message "fine") Nothing Map.empty
        fmap (.smStatus) stillIdle @?= Right StatusReady

{- | A message posted with @interrupt: true@ against a busy session is
delivered as 'Interrupt'-priority mail (R3a\/R4), not 'Normal' -- what
distinguishes it, per §4, is that it pre-empts attached calls\/an in-flight
completion instead of waiting behind them.
-}
postMessageInterruptTest :: Assertion
postMessageInterruptTest = do
    gate <- newEmptyMVar
    node <- testNode "{}"
    host <- testHost [node] (\_ c -> readMVar gate >> mockCompletion c)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        let sid = meta.smSessionId
        accepted <- postMessage runner sid (NewMessage "urgent" [] True) (Just UntilBlocked) Map.empty
        either (\e -> assertFailure ("expected mail to be accepted, got " <> show e)) (const (pure ())) accepted
        let router = serverMailRouter runner
        mTarget <- router.mrLookup sid
        case mTarget of
            Nothing -> assertFailure "session has no mailbox"
            Just (_, mb) -> do
                envelopes <- atomically (mbUnread mb 0)
                let priorities = [e.envPriority | e <- envelopes, UserMessage q <- [e.envBody], q.queryText == "urgent"]
                priorities @?= [Interrupt]
        putMVar gate ()
        _ <- expectRight =<< awaitRun runner sid 5
        pure ()

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
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
        _ <- expectRight =<< awaitRun runner sid 5
        running <- currentSession runner sid
        assertBool "a call runs in the background" (hasRunningCall running)
        putMVar gate ()
        threadDelay 100_000
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
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
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
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
            TrackedToolCall callId slowCall Running Nothing Nothing (AppliedPolicy (RunAsync Nothing) Nothing) Nothing False Nothing Nothing Nothing
        llm = LlmTurn (LlmTurnContent (LlmResponse Nothing Nothing Aeson.Null Nothing) [slowCall]) Nothing
        partial = PartialUserTurn (PartialUserTurnContent (SystemPrompt "sys") [] Nothing [running] []) Nothing
        sess = sess0{turns = partial : llm : sess0.turns}
    now <- getCurrentTime
    _ <- expectRight =<< host.hostBackend.sbCompareAndStore (freshSessionMeta sid now){smAgent = Just "test-agent", smStatus = StatusRunning} sess
    withSessionRunner host $ \runner -> do
        recovered <- recoverOnStartup runner
        recovered @?= [sid]
        Just (_, meta) <- getSession runner sid
        meta.smStatus @?= StatusReady
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
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
    atomically $ writeTVar parent.osNodeTools [OneShotTool.turnAgentRuntimeIntoIOTool silent host.hostSubAgentDeps child "parent" agentId Nothing True]
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSessionAs runner (Just "alice") "parent" (message "delegate") (Just UntilBlocked) Map.empty
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
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
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
        _ <- expectRight =<< resume runner a.smSessionId UntilBlocked Map.empty
        _ <- expectRight =<< awaitRun runner a.smSessionId 5
        _ <- expectRight =<< resume runner b.smSessionId UntilBlocked Map.empty
        kinds <- eventsUntilStopped next
        assertBool ("events of b: " <> show kinds) ("run.started" `elem` kinds)

storedAgentsTest :: Assertion
storedAgentsTest =
    withSystemTempDirectory "agents-stored" $ \dir -> do
        let keysFile = dir </> "keys.json"
            dbFile = dir </> "agents.db"
            agentFile slug = dir </> (slug <> ".json")
            config slug extra =
                "{\"slug\": \""
                    <> slug
                    <> "\", \"apiKeyId\": \"none\", \"flavor\": \"OpenAIv1\", "
                    <> "\"modelUrl\": \"http://127.0.0.1:1\", \"modelName\": \"mock\", \"announce\": \"test\", "
                    <> "\"systemPrompt\": [\"You are a test\"]"
                    <> extra
                    <> "}"
            writeAgentFile slug = writeFile (agentFile slug) ("{\"tag\": \"OpenAIAgentDescription\", \"contents\": " <> config slug "" <> "}")
            decodeAgent txt = either (assertFailure . ("bad config: " <>)) pure (Aeson.eitherDecode (LBS8.pack txt))
            cfg files = (defaultHostConfig (map agentFile files) keysFile dbFile){hcCompletion = Just (const mockCompletion)}
        writeFile keysFile "{}"
        writeAgentFile "smoke"
        helper <- decodeAgent (config "helper" "")
        withHost (cfg ["smoke"]) silent $ \host -> do
            first <- putStoredAgent host (Just "alice") helper
            fmap snd first @?= Right True
            second <- putStoredAgent host (Just "alice") helper
            fmap snd second @?= Right False
            withFiles <- decodeAgent (config "files" ", \"toolDirectory\": \"tools\"")
            fmap snd <$> putStoredAgent host Nothing withFiles >>= (@?= Left (AgentUsesFiles ["toolDirectory"]))
            smoke <- decodeAgent (config "smoke" "")
            fmap snd <$> putStoredAgent host Nothing smoke >>= (@?= Left (AgentDefinedByFile "smoke"))
            withSessionRunner host $ \runner -> do
                meta <- expectRight =<< createSession runner "helper" (message "hello") (Just UntilBlocked)
                (idle, _) <- expectRight =<< awaitRun runner meta.smSessionId 5
                idle.smStatus @?= StatusIdle
        -- After a restart the stored agent is back.
        withHost (cfg ["smoke"]) silent $ \host ->
            Map.keys <$> hostAllAgents host >>= (@?= ["helper", "smoke"])
        -- A file agent with the same slug hides it, and the skip is traced.
        writeAgentFile "helper"
        traces <- newIORef []
        let tracer = Tracer $ \t -> atomicModifyIORef' traces (\ts -> (t : ts, ()))
        withHost (cfg ["smoke", "helper"]) tracer $ \host -> do
            agents <- hostAllAgents host
            [slug | (slug, (FromFile, _)) <- Map.toList agents] @?= ["helper", "smoke"]
            deleteStoredAgent host "helper" >>= (@?= Left (AgentDefinedByFile "helper"))
        skipped <- readIORef traces
        [slug | HostStoredAgentSkipped slug _ <- skipped] @?= ["helper"]
        -- Deleted agents stay deleted.
        withHost (cfg ["smoke"]) silent $ \host -> do
            deleteStoredAgent host "helper" >>= (@?= Right ())
            deleteStoredAgent host "helper" >>= (@?= Left (NoStoredAgent "helper"))
        withHost (cfg ["smoke"]) silent $ \host ->
            Map.keys <$> hostAllAgents host >>= (@?= ["smoke"])

-- | 'serverMailRouter' (@todos/session-mailbox.md@, Phase 4) resolves any
-- stored session on demand, without needing prior registration.
serverMailRouterTest :: Assertion
serverMailRouterTest = do
    node <- testNode "{}"
    host <- testHost [node] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        _ <- expectRight =<< awaitRun runner meta.smSessionId 5
        let router = serverMailRouter runner
        found <- router.mrLookup meta.smSessionId
        case found of
            Nothing -> assertFailure "expected the just-created session to be found"
            Just (info, _mb) -> info.miAgentSlug @?= Just "test-agent"
        listed <- router.mrList
        meta.smSessionId `elem` map fst listed @?= True
        unknownSid <- SessionId <$> nextRandom
        unknownFound <- router.mrLookup unknownSid
        isJust unknownFound @?= False

-- | @send-message@ posts an 'AgentMessage' envelope the recipient's own
-- durable mailbox (hence its own R1/R2) will see.
sendMessageTest :: Assertion
sendMessageTest = do
    senderNode <- testNode "{}"
    recipientNode <- testNode "{}"
    host <- testHost [senderNode, recipientNode] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        senderMeta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        _ <- expectRight =<< awaitRun runner senderMeta.smSessionId 5
        -- §5's "Permissions": mailScope defaults to subtree, so the
        -- recipient must be the sender itself or a descendant. Spawn it as
        -- a child, exercising 'spawnSession' at the same time.
        recipientMeta <- expectRight =<< spawnSession runner senderMeta.smSessionId "test-agent" (message "hi")
        _ <- expectRight =<< awaitRun runner recipientMeta.smSessionId 5

        let router = serverMailRouter runner
            senderSid = senderMeta.smSessionId
            recipientSid = recipientMeta.smSessionId
            recipientText = case recipientSid of SessionId uuid -> UUID.toText uuid
        turnId <- newTurnId
        let ctx =
                (mkMinimalContext senderSid (sessionIdToConversationId senderSid) turnId dummyPortal)
                    { Ctx.ctxMailRouter = Just router
                    }
            params =
                SendMessageParams
                    { smpTo = recipientText
                    , smpText = "how's it going?"
                    , smpInReplyTo = Nothing
                    , smpExpectsReply = True
                    , smpInterrupt = False
                    }
        result <- Mail.sendMessageToSession ctx params
        sendResult <- expectRight result
        smrRecipientStatus sendResult @?= "idle"

        mb <- newDurableMailbox host.hostMail recipientSid
        unread <- atomically (mbUnread mb 0)
        case [e | e <- unread, matchesAgentMessage e] of
            [e] -> do
                e.envFrom @?= FromSession senderSid (Just "test-agent")
                case e.envBody of
                    AgentMessage txt _ expectsReply -> do
                        txt @?= "how's it going?"
                        expectsReply @?= True
                    other -> assertFailure ("expected AgentMessage, got " <> show other)
            other -> assertFailure ("expected exactly one AgentMessage envelope, got " <> show (length other))

-- | An unrelated session (no parent\/child relationship) is outside the
-- sender's default @mailScope: subtree@, so @send-message@ refuses it.
sendMessageScopeDenialTest :: Assertion
sendMessageScopeDenialTest = do
    senderNode <- testNode "{}"
    recipientNode <- testNode "{}"
    host <- testHost [senderNode, recipientNode] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        senderMeta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        _ <- expectRight =<< awaitRun runner senderMeta.smSessionId 5
        recipientMeta <- expectRight =<< createSession runner "test-agent" (message "hi") (Just UntilBlocked)
        _ <- expectRight =<< awaitRun runner recipientMeta.smSessionId 5

        let router = serverMailRouter runner
            senderSid = senderMeta.smSessionId
            recipientText = case recipientMeta.smSessionId of SessionId uuid -> UUID.toText uuid
        turnId <- newTurnId
        let ctx =
                (mkMinimalContext senderSid (sessionIdToConversationId senderSid) turnId dummyPortal)
                    { Ctx.ctxMailRouter = Just router
                    }
            params =
                SendMessageParams
                    { smpTo = recipientText
                    , smpText = "hi there"
                    , smpInReplyTo = Nothing
                    , smpExpectsReply = False
                    , smpInterrupt = False
                    }
        result <- Mail.sendMessageToSession ctx params
        result @?= Left (SystemInfoError "not permitted to send to this session")

{- | A message that answers one already received (@in_reply_to@ set)
carries 'envHops' one past it; fresh mail (no @in_reply_to@) carries 0.

Relays root -> child -> grandchild, rather than a reply back up the tree,
because the default @mailScope: subtree@ only allows a sender to reach its
own descendants (see 'sendMessageScopeDenialTest'): a child cannot mail its
parent by default, so a literal "reply" can't be exercised here without
also changing scope. The hop-increment logic is identical either way --
what matters is that a message's own 'outHops' is computed from the mail
named by 'smpInReplyTo' in the *sender's own* received mail.
-}
sendMessageHopsTest :: Assertion
sendMessageHopsTest = do
    rootNode <- testNode "{}"
    childNode <- testNode "{}"
    grandchildNode <- testNode "{}"
    host <- testHost [rootNode, childNode, grandchildNode] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        rootMeta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        let rootSid = rootMeta.smSessionId
        _ <- expectRight =<< awaitRun runner rootSid 5
        childMeta <- expectRight =<< spawnSession runner rootSid "test-agent" (message "hi")
        let childSid = childMeta.smSessionId
        _ <- expectRight =<< awaitRun runner childSid 5
        grandchildMeta <- expectRight =<< spawnSession runner childSid "test-agent" (message "hi")
        let grandchildSid = grandchildMeta.smSessionId
        _ <- expectRight =<< awaitRun runner grandchildSid 5

        let router = serverMailRouter runner
        turnId <- newTurnId
        let rootCtx =
                (mkMinimalContext rootSid (sessionIdToConversationId rootSid) turnId dummyPortal)
                    { Ctx.ctxMailRouter = Just router
                    }
        -- Fresh mail, root -> child: hops 0.
        firstResult <-
            expectRight
                =<< Mail.sendMessageToSession
                    rootCtx
                    SendMessageParams
                        { smpTo = sessionIdText childSid
                        , smpText = "ping"
                        , smpInReplyTo = Nothing
                        , smpExpectsReply = True
                        , smpInterrupt = False
                        }
        childMb <- newDurableMailbox host.hostMail childSid
        childUnread <- atomically (mbUnread childMb 0)
        firstEnvelope <- case [e | e <- childUnread, matchesAgentMessage e] of
            [e] -> pure e
            other -> assertFailure ("expected exactly one AgentMessage envelope, got " <> show (length other)) >> error "unreachable"
        firstEnvelope.envHops @?= 0

        -- Relayed, child -> grandchild, in reply to the mail child just
        -- received from root: hops 1.
        let childCtx =
                (mkMinimalContext childSid (sessionIdToConversationId childSid) turnId dummyPortal)
                    { Ctx.ctxMailRouter = Just router
                    }
        _ <-
            expectRight
                =<< Mail.sendMessageToSession
                    childCtx
                    SendMessageParams
                        { smpTo = sessionIdText grandchildSid
                        , smpText = "fyi"
                        , smpInReplyTo = Just (smrMessageId firstResult)
                        , smpExpectsReply = False
                        , smpInterrupt = False
                        }
        grandchildMb <- newDurableMailbox host.hostMail grandchildSid
        grandchildUnread <- atomically (mbUnread grandchildMb 0)
        case [e | e <- grandchildUnread, matchesAgentMessage e] of
            [e] -> e.envHops @?= 1
            other -> assertFailure ("expected exactly one AgentMessage envelope, got " <> show (length other))
  where
    sessionIdText :: SessionId -> Text
    sessionIdText (SessionId uuid) = UUID.toText uuid

-- | @spawn-session@ starts a durable child session recorded with the
-- caller as parent, which can then be reached by @send-message@.
spawnSessionTest :: Assertion
spawnSessionTest = do
    callerNode <- testNode "{}"
    helperNode <- testNode "{}"
    host <- testHost [callerNode, helperNode] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        callerMeta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        let callerSid = callerMeta.smSessionId
        _ <- expectRight =<< awaitRun runner callerSid 5

        let router = serverMailRouter runner
        turnId <- newTurnId
        let ctx =
                (mkMinimalContext callerSid (sessionIdToConversationId callerSid) turnId dummyPortal)
                    { Ctx.ctxSpawnSession = Just (serverSpawnSession runner callerSid)
                    }
        result <- expectRight =<< Mail.spawnSession ctx SpawnSessionParams{sspAgent = "test-agent", sspMessage = "get started"}
        childSid <- case UUID.fromText (ssrSessionId result) of
            Just uuid -> pure (SessionId uuid)
            Nothing -> assertFailure "spawn-session did not return a valid session id" >> error "unreachable"

        (childMeta, _) <- expectRight =<< awaitRun runner childSid 5
        childMeta.smParent @?= Just callerSid

        childInfo <- router.mrLookup childSid
        case childInfo of
            Just (info, _) -> info.miParent @?= Just callerSid
            Nothing -> assertFailure "spawned session is not reachable via the MailRouter"

{- | 'Mail.mergeLiveSessions' (@todos/session-mailbox.md@, Phase 4, §5)
appends a live-only session to a @list-sessions@-shaped result and
overwrites the status of one already present in it, leaving everything
else (an id present in neither list) untouched.
-}
listSessionsMergeTest :: Assertion
listSessionsMergeTest = do
    router <- newMailRouter
    persistedSid <- SessionId <$> nextRandom
    liveOnlySid <- SessionId <$> nextRandom
    mb <- newInMemoryMailbox
    _ <-
        router.mrRegister
            persistedSid
            (MailboxInfo (Just "persisted-agent") Nothing "running" MailScopeSubtree MailScopeChildren)
            mb
    _ <-
        router.mrRegister
            liveOnlySid
            (MailboxInfo (Just "live-agent") Nothing "running" MailScopeSubtree MailScopeChildren)
            mb
    let idText sid = case sid of SessionId uuid -> UUID.toText uuid
        catalogResult =
            Aeson.object
                [ "sessions"
                    Aeson..= [ Aeson.object
                                [ "sessionId" Aeson..= idText persistedSid
                                , "conversationId" Aeson..= idText persistedSid
                                , "status" Aeson..= ("idle" :: Text)
                                ]
                             ]
                , "totalAccessible" Aeson..= (1 :: Int)
                ]
    merged <- Mail.mergeLiveSessions (Just router) catalogResult
    case merged of
        Aeson.Object obj -> do
            case Aeson.fromJSON <$> KeyMap.lookup "sessions" obj of
                Just (Aeson.Success (sessions :: [Aeson.Object])) -> do
                    let byId sid = [s | s <- sessions, KeyMap.lookup "sessionId" s == Just (Aeson.String (idText sid))]
                    case byId persistedSid of
                        [s] -> KeyMap.lookup "status" s @?= Just (Aeson.String "running")
                        other -> assertFailure ("expected exactly one persisted entry, got " <> show other)
                    length (byId liveOnlySid) @?= 1
                    length sessions @?= 2
                other -> assertFailure ("expected a decodable sessions array, got " <> show other)
            KeyMap.lookup "totalAccessible" obj @?= Just (Aeson.Number 2)
        other -> assertFailure ("expected a merged object, got " <> show other)

{- | 'System.Agents.OneShot.oneShotSpawnSession' (@todos/session-mailbox.md@,
Phase 4, §5) resolves @<slug>@ against the caller's own declared helpers and
returns immediately with a registered child session id, without waiting for
(or requiring) the child to actually answer -- so this only exercises the
resolution/registration/bookkeeping, not a real LLM round trip (there is no
fake-completion seam in @run@'s current design to drive one in a test).
-}
oneShotSpawnSessionTest :: Assertion
oneShotSpawnSessionTest = withSystemTempDirectory "oneshot-spawn-session" $ \dir -> do
    let store = mkSimpleSessionStore dir
    router <- newMailRouter
    spawnedVar <- newTVarIO []
    ownSid <- SessionId <$> nextRandom
    helperNode <- testNode "{}"
    callerNode <- testNode "{}"
    let callerWithHelper = callerNode{osNodeChildren = [helperNode]}
    result <- oneShotSpawnSession store silent [] router spawnedVar ownSid callerWithHelper "test-agent" "get started"
    childSid <- case result of
        Right sid -> pure sid
        Left err -> assertFailure ("expected spawn-session to succeed, got " <> Text.unpack err) >> error "unreachable"
    info <- router.mrLookup childSid
    case info of
        Just (mailboxInfo, _) -> mailboxInfo.miParent @?= Just ownSid
        Nothing -> assertFailure "spawned session is not reachable via the MailRouter"
    spawned <- readTVarIO spawnedVar
    length spawned @?= 1

-- | An unknown helper slug is refused rather than crashing or hanging.
oneShotSpawnSessionUnknownSlugTest :: Assertion
oneShotSpawnSessionUnknownSlugTest = withSystemTempDirectory "oneshot-spawn-session-unknown" $ \dir -> do
    let store = mkSimpleSessionStore dir
    router <- newMailRouter
    spawnedVar <- newTVarIO []
    ownSid <- SessionId <$> nextRandom
    callerNode <- testNode "{}"
    result <- oneShotSpawnSession store silent [] router spawnedVar ownSid callerNode "no-such-helper" "hi"
    case result of
        Left _ -> pure ()
        Right sid -> assertFailure ("expected an unknown slug to be refused, got session " <> show sid)

{- | Get a session to the point where one call runs in the background,
using the deterministic 'StepOnce'-driven flow 'engineKeptTest' also uses:
first step issues the call, second step yields while it runs. Driving the
run one step at a time (rather than 'UntilBlocked' plus polling) makes
exactly when a run is (not) active, and thus when it next checks its
mailbox, independent of the async engine's own yield/attach timing.
-}
setUpBackgroundCall :: SessionRunner -> OSAgentNode -> IO SessionId
setUpBackgroundCall runner node = do
    meta <- expectRight =<< createSession runner (Base.slug node.osNodeConfig) (message "go") (Just StepOnce)
    let sid = meta.smSessionId
    _ <- expectRight =<< awaitRun runner sid 5
    _ <- expectRight =<< resume runner sid StepOnce Map.empty
    _ <- expectRight =<< awaitRun runner sid 5
    running <- currentSession runner sid
    assertBool "a call runs in the background" (hasRunningCall running)
    pure sid

-- | 'Control' 'Pause' mail stops a run at its next iteration, sets the
-- session's persisted status to 'StatusPaused', and (by default) leaves an
-- attached background call running.
controlPauseTest :: Assertion
controlPauseTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    host <- testHost [node] (\_ c -> firstThen [slowCall] c)
    withSessionRunner host $ \runner -> do
        sid <- setUpBackgroundCall runner node

        sendControl runner sid Pause
        -- A new run's very first iteration checks the mailbox before
        -- taking any step, so this stops immediately: no step, no LLM call.
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
        (paused, active) <- expectRight =<< awaitRun runner sid 5
        (paused.smStatus, active) @?= (StatusPaused, False)
        stillRunning <- hasBackgroundCall <$> currentSession runner sid
        assertBool "the background call keeps running through the pause by default" stillRunning

        -- An explicit 'resume' works regardless of the persisted status.
        putMVar gate ()
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle

-- | 'pauseCancelsCalls' makes 'Control' 'Pause' also cancel attached calls.
controlPauseCancelsCallsTest :: Assertion
controlPauseCancelsCallsTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAllPauseCancelsCalls
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    complete <- onceThen [slowCall]
    host <- testHost [node] (const complete)
    withSessionRunner host $ \runner -> do
        sid <- setUpBackgroundCall runner node

        sendControl runner sid Pause
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
        (paused, _) <- expectRight =<< awaitRun runner sid 5
        paused.smStatus @?= StatusPaused

        -- Cancellation of the background call's thread is asynchronous, so
        -- (per 'cancelTest's established pattern) it isn't asserted right
        -- here against the just-persisted turn; the next run re-polls the
        -- OS entity and delivers the cancellation to the LLM.
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle
        sess <- currentSession runner sid
        assertBool ("call reported cancelled: " <> show (sessionTexts sess)) (any ("cancelled" `Text.isInfixOf`) (sessionTexts sess))

-- | Without 'resumeOnAnyMail' (the default), 'postMessage' refuses an
-- ordinary message sent to a paused session.
controlPauseRefusesMessagesByDefaultTest :: Assertion
controlPauseRefusesMessagesByDefaultTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    host <- testHost [node] (\_ c -> firstThen [slowCall] c)
    withSessionRunner host $ \runner -> do
        sid <- setUpBackgroundCall runner node
        sendControl runner sid Pause
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
        (paused, _) <- expectRight =<< awaitRun runner sid 5
        paused.smStatus @?= StatusPaused

        refused <- postMessage runner sid (message "are you there") Nothing Map.empty
        case refused of
            Left (NotAcceptingMessages _ StatusPaused) -> pure ()
            other -> assertFailure ("expected a paused refusal, got " <> show other)
        putMVar gate ()

-- | With 'resumeOnAnyMail', 'postMessage' accepts a message into a paused
-- session and starts a run again.
controlResumeOnAnyMailTest :: Assertion
controlResumeOnAnyMailTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAllResumeOnAnyMail
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    host <- testHost [node] (\_ c -> firstThen [slowCall] c)
    withSessionRunner host $ \runner -> do
        sid <- setUpBackgroundCall runner node
        sendControl runner sid Pause
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
        (paused, _) <- expectRight =<< awaitRun runner sid 5
        paused.smStatus @?= StatusPaused

        accepted <- expectRight =<< postMessage runner sid (message "are you there") (Just UntilBlocked) Map.empty
        accepted.smStatus @?= StatusRunning
        putMVar gate ()
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle

{- | Even with 'resumeOnAnyMail' on, a @wakeOn@ that excludes @\"user\"@
means an ordinary message does not wake a paused session by itself (§5
"Scheduling rule": mail outside 'wakeOn' is queued, not a wake trigger).
-}
wakeOnExcludesUserTest :: Assertion
wakeOnExcludesUserTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAllResumeOnAnyMailNoUserWake
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    host <- testHost [node] (\_ c -> firstThen [slowCall] c)
    withSessionRunner host $ \runner -> do
        sid <- setUpBackgroundCall runner node
        sendControl runner sid Pause
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
        (paused, _) <- expectRight =<< awaitRun runner sid 5
        paused.smStatus @?= StatusPaused

        refused <- postMessage runner sid (message "are you there") (Just UntilBlocked) Map.empty
        refused @?= Left (NotAcceptingMessages sid StatusPaused)

        -- An explicit resume still works regardless of wakeOn.
        putMVar gate ()
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle

-- | 'Control' ('CancelCalls' ids) cancels one specific attached call, which
-- the run then reports as failed, without stopping the run itself.
controlCancelCallsTest :: Assertion
controlCancelCallsTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    complete <- onceThen [slowCall]
    host <- testHost [node] (const complete)
    withSessionRunner host $ \runner -> do
        sid <- setUpBackgroundCall runner node
        sess <- currentSession runner sid
        let runningIds = [tc.tcId | PartialUserTurn p _ <- sess.turns, tc <- p.pTrackedToolCalls, tc.tcState == Running]
        case runningIds of
            [callId] -> sendControl runner sid (CancelCalls [callId])
            other -> assertFailure ("expected exactly one running call, got " <> show (length other))

        -- The next run picks the cancellation up and delivers it to the LLM.
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle
        sessF <- currentSession runner sid
        assertBool ("call reported cancelled: " <> show (sessionTexts sessF)) (any ("cancelled" `Text.isInfixOf`) (sessionTexts sessF))

-- | A background call reports 'ToolCallStarted' while it runs and
-- 'ToolCallCompleted' once it finishes (@todos/session-mailbox.md@ §7).
toolCallEventsTest :: Assertion
toolCallEventsTest = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    host <- testHost [node] (\_ c -> firstThen [slowCall] c)
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner (Base.slug node.osNodeConfig) (message "go") (Just StepOnce)
        let sid = meta.smSessionId
        _ <- expectRight =<< awaitRun runner sid 5
        startEvents <- subscribe runner sid
        -- Second step: starts the background call, then yields while it runs.
        _ <- expectRight =<< resume runner sid StepOnce Map.empty
        _ <- expectRight =<< awaitRun runner sid 5
        startKinds <- eventsUntilStopped startEvents
        assertBool ("tool.started among " <> show startKinds) ("tool.started" `elem` startKinds)

        completeEvents <- subscribe runner sid
        putMVar gate ()
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle
        completeKinds <- eventsUntilStopped completeEvents
        assertBool ("tool.completed among " <> show completeKinds) ("tool.completed" `elem` completeKinds)

{- | A watch always permits watching one's own session ('isWithinSubtree's
own-id short-circuit), so a session watching itself is enough to exercise
the whole watch/forward/deliver mechanism deterministically, on the exact
same 'StepOnce'-driven background-call setup 'toolCallEventsTest' and
'setUpBackgroundCall' already use, rather than layering 'spawnSession's own
un-controllable 'UntilBlocked' auto-stepping (which raced unpredictably
under a parallel test run) on top of it.

Runs @before gate@ (watch setup, with the background call still running)
then, once the gate is open and the run has finished, @after run@ (mailbox
assertions), giving both access to the 'Host' (for 'hostMail') and the
common session id.
-}
watchSelfSetup :: (SessionRunner -> Host -> SessionId -> IO ()) -> (SessionRunner -> Host -> SessionId -> IO ()) -> Assertion
watchSelfSetup beforeGate afterRun = do
    gate <- newEmptyMVar
    node <- testNode backgroundAll
    atomically $ writeTVar node.osNodeTools [gatedTool gate]
    host <- testHost [node] (\_ c -> firstThen [slowCall] c)
    withSessionRunner host $ \runner -> do
        sid <- setUpBackgroundCall runner node
        beforeGate runner host sid
        putMVar gate ()
        _ <- expectRight =<< resume runner sid UntilBlocked Map.empty
        (final, _) <- expectRight =<< awaitRun runner sid 5
        final.smStatus @?= StatusIdle
        afterRun runner host sid

-- | A watch forwards a matching event as 'WatchedEvent' mail to the watcher's own mailbox.
watchSessionTest :: Assertion
watchSessionTest =
    watchSelfSetup
        ( \runner _host sid ->
            void $
                expectRight
                    =<< serverWatchSession
                        runner
                        sid
                        WatchRequest{wrTarget = sid, wrEvents = Just ["tool.completed"], wrTool = Nothing, wrTtlSeconds = Nothing}
        )
        ( \_runner host sid -> do
            mb <- newDurableMailbox host.hostMail sid
            waitUntil (any isWatchedEvent <$> atomically (mbUnread mb 0))
            unread <- atomically (mbUnread mb 0)
            case [e | e <- unread, isWatchedEvent e] of
                (e : _) -> case e.envBody of
                    WatchedEvent watchedSid kind _ -> do
                        watchedSid @?= sid
                        kind @?= "tool.completed"
                    _ -> assertFailure "expected WatchedEvent"
                [] -> assertFailure "no WatchedEvent mail arrived"
        )

isWatchedEvent :: Envelope -> Bool
isWatchedEvent e = case e.envBody of
    WatchedEvent{} -> True
    _ -> False

-- | A watch's @events@ filter excludes non-matching kinds.
watchSessionFilterTest :: Assertion
watchSessionFilterTest =
    watchSelfSetup
        ( \runner _host sid ->
            void $
                expectRight
                    =<< serverWatchSession
                        runner
                        sid
                        WatchRequest{wrTarget = sid, wrEvents = Just ["run.started"], wrTool = Nothing, wrTtlSeconds = Nothing}
        )
        ( \_runner host sid -> do
            threadDelay 200_000
            mb <- newDurableMailbox host.hostMail sid
            unread <- atomically (mbUnread mb 0)
            let toolCompletedForwarded = or [True | e <- unread, WatchedEvent _ "tool.completed" _ <- [e.envBody]]
            assertBool "tool.completed was not requested and must not be forwarded" (not toolCompletedForwarded)
        )

-- | 'unwatch-session' immediately stops a watch from forwarding further events.
unwatchSessionTest :: Assertion
unwatchSessionTest =
    watchSelfSetup
        ( \runner _host sid -> do
            watchId <-
                expectRight
                    =<< serverWatchSession
                        runner
                        sid
                        WatchRequest{wrTarget = sid, wrEvents = Just ["tool.completed"], wrTool = Nothing, wrTtlSeconds = Nothing}
            stopped <- serverUnwatchSession runner watchId
            stopped @?= True
            stoppedAgain <- serverUnwatchSession runner watchId
            stoppedAgain @?= False
        )
        ( \_runner host sid -> do
            threadDelay 200_000
            mb <- newDurableMailbox host.hostMail sid
            unread <- atomically (mbUnread mb 0)
            let forwarded = or [True | e <- unread, WatchedEvent{} <- [e.envBody]]
            assertBool "an unwatched watch must not forward" (not forwarded)
        )

-- | @watch-session@ enforces the same subtree scope @send-message@ does.
watchSessionScopeDenialTest :: Assertion
watchSessionScopeDenialTest = do
    aNode <- testNode "{}"
    bNode <- testNode "{}"
    host <- testHost [aNode, bNode] (const mockCompletion)
    withSessionRunner host $ \runner -> do
        aMeta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        bMeta <- expectRight =<< createSession runner "test-agent" (message "hello") (Just UntilBlocked)
        _ <- expectRight =<< awaitRun runner aMeta.smSessionId 5
        _ <- expectRight =<< awaitRun runner bMeta.smSessionId 5
        result <-
            serverWatchSession
                runner
                aMeta.smSessionId
                WatchRequest{wrTarget = bMeta.smSessionId, wrEvents = Nothing, wrTool = Nothing, wrTtlSeconds = Nothing}
        case result of
            Left _ -> pure ()
            Right _ -> assertFailure "expected watching an unrelated session to be refused"

-- | A dummy tool portal for tests that need a valid 'ToolExecutionContext'.
dummyPortal :: ToolPortal
dummyPortal _ _ =
    pure $
        ToolResult
            { resultData = Aeson.object []
            , resultDuration = 0
            , resultTraceId = "dummy"
            }

-- | Whether an envelope is an 'AgentMessage'.
matchesAgentMessage :: Envelope -> Bool
matchesAgentMessage e = case e.envBody of
    AgentMessage{} -> True
    _ -> False

-------------------------------------------------------------------------------
-- Fixtures
-------------------------------------------------------------------------------

-- | A host over in-memory SQLite, with the given agents and LLM.
testHost :: [OSAgentNode] -> (OSAgentNode -> Completion) -> IO Host
testHost nodes complete = do
    conn <- open ":memory:"
    backend <- mkSqliteSessionStore conn
    store <- mkSqliteContinuationStore conn
    mail <- mkSqliteMailStore conn
    let deps = (defaultAgentDeps []){adContinuationStore = Just store, adCompletion = Just complete}
    stored <- noStoredAgents
    pure
        Host
            { hostAgents = Map.fromList [(Base.slug n.osNodeConfig, n) | n <- nodes]
            , hostStoredAgents = stored
            , hostDeps = deps
            , hostSubAgentDeps = deps{adSessionSink = SinkBackend backend}
            , hostBackend = backend
            , hostContinuations = store
            , hostMail = mail
            , hostTracer = silent
            , hostStreamTokens = False
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

-- | Like 'backgroundAll', with 'pauseCancelsCalls' turned on (Phase 6).
backgroundAllPauseCancelsCalls :: String
backgroundAllPauseCancelsCalls =
    "{\"executionMode\": \"asynchronous\", \"asyncYieldStrategy\": {\"tag\": \"yieldOnTimeout\", \"milliseconds\": 20}, "
        <> "\"toolCallPolicyConfig\": {\"default\": {\"tag\": \"runAsync\"}, \"rules\": []}, \"pauseCancelsCalls\": true}"

-- | Like 'backgroundAll', with 'resumeOnAnyMail' turned on (Phase 6).
backgroundAllResumeOnAnyMail :: String
backgroundAllResumeOnAnyMail =
    "{\"executionMode\": \"asynchronous\", \"asyncYieldStrategy\": {\"tag\": \"yieldOnTimeout\", \"milliseconds\": 20}, "
        <> "\"toolCallPolicyConfig\": {\"default\": {\"tag\": \"runAsync\"}, \"rules\": []}, \"resumeOnAnyMail\": true}"

-- | Like 'backgroundAllResumeOnAnyMail', but with a @wakeOn@ that excludes
-- @\"user\"@ (Phase 4, §5 "Scheduling rule").
backgroundAllResumeOnAnyMailNoUserWake :: String
backgroundAllResumeOnAnyMailNoUserWake =
    "{\"executionMode\": \"asynchronous\", \"asyncYieldStrategy\": {\"tag\": \"yieldOnTimeout\", \"milliseconds\": 20}, "
        <> "\"toolCallPolicyConfig\": {\"default\": {\"tag\": \"runAsync\"}, \"rules\": []}, \"resumeOnAnyMail\": true, \"wakeOn\": [\"tool\"]}"

{- | Post a 'Control' envelope directly to a session's mailbox, bypassing
any tool: the way something like a future admin endpoint would reach it.

Sent at 'Interrupt' priority: per §4, "the user and 'Control' may always
interrupt", and it is what lets R3a's interrupt arm detach an attached call
right away rather than waiting for the ordinary yield-strategy condition
(which, for calls with no explicit 'attachSeconds', can otherwise take
arbitrarily long to next hand control back to the outer run loop).
-}
sendControl :: SessionRunner -> SessionId -> ControlMsg -> IO ()
sendControl runner sid msg = do
    let router = serverMailRouter runner
    mTarget <- router.mrLookup sid
    case mTarget of
        Nothing -> assertFailure "session not reachable via the MailRouter"
        Just (_, mb) -> do
            sent <-
                mb.mbSend
                    Outgoing
                        { outId = Nothing
                        , outFrom = FromSystem "test"
                        , outPriority = Interrupt
                        , outHops = 0
                        , outBody = Control msg
                        }
            either (\e -> assertFailure ("control mail refused: " <> show e)) (const (pure ())) sent

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
message txt = NewMessage txt [] False

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
waitUntil = waitUntilFor 250

-- | Poll a condition every 20ms for up to @n@ attempts.
waitUntilFor :: Int -> IO Bool -> Assertion
waitUntilFor attempts check = go attempts
  where
    go 0 = assertFailure "condition not reached in time"
    go n = do
        ok <- check
        if ok then pure () else threadDelay 20_000 >> go (n - 1)
