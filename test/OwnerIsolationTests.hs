{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Per-owner API keys ('hostOwnerApiKeys') and host-wide isolation of bash
tool calls ('adToolIsolation'), through the session runner.
-}
module OwnerIsolationTests (tests) where

import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Prod.Tracer (silent)
import System.Directory (doesFileExist, emptyPermissions, executable, readable, setPermissions)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import AgentFactoryTests (mockCompletion, testNode)
import RunnerTests (expectRight, firstThen, message, responseTexts, testHost)
import System.Agents.AgentFactory
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.Base (AgentId (..), ConversationId (..))
import qualified System.Agents.Base as Base
import System.Agents.Host
import System.Agents.Host.Runner
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Session.Base hiding (SessionProgress (..))
import System.Agents.SessionStore hiding (listSessions)
import System.Agents.ToolRegistration (ToolRegistration, registerBashToolInLLM, registerIOScriptInLLM)
import System.Agents.Tools.Bash (ScriptDescription (..), ScriptInfo (..))
import qualified System.Agents.Tools.Context as Ctx
import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.Isolated (ToolIsolation, toolIsolationFromSpec)

tests :: TestTree
tests =
    testGroup
        "Per-owner API keys and tool isolation"
        [ testCase "each owner's sessions and sub-sessions call the LLM with that owner's keys" ownerKeysTest
        , testCase "an owner's keys file replaces the shared one, it is not merged with it" ownerKeysNotMergedTest
        , testCase "a bad owner keys file, or an owner listed twice, fails the start" ownerKeysStartupTest
        , testCase "without isolation a bash tool runs in-process" bashInProcessTest
        , testCase "with isolation a bash call goes to the worker, whatever the agent's policy" bashIsolatedTest
        , testCase "a failing worker fails the call, with no in-process fallback" bashIsolationFailsClosedTest
        , testCase "with isolation other tool kinds still run in-process" otherToolsInProcessTest
        , testCase "with isolation the tool portal refuses a bash tool" portalRefusedTest
        , testCase "a function runner is refused as an isolation target" functionRunnerRefusedTest
        ]

-------------------------------------------------------------------------------
-- Per-owner API keys
-------------------------------------------------------------------------------

ownerKeysTest :: Assertion
ownerKeysTest = do
    seen <- newIORef []
    testWithApplication (pure (fakeLlm seen)) $ \port -> do
        (host, _) <- keysHost port [("alice", [("none", "alice-key")]), ("bob", [("none", "bob-key")])]
        withSessionRunner host $ \runner -> do
            let run owner = do
                    meta <- expectRight =<< createSessionAs runner owner "parent" (Just (message "delegate")) (Just UntilBlocked) Map.empty
                    (final, _) <- expectRight =<< awaitRun runner meta.smSessionId 10
                    final.smStatus @?= StatusIdle
                    children <- listSessions runner allSessionsQuery{sqParent = Just meta.smSessionId}
                    map (.smAgent) children @?= [Just "child"]
                    calls <- readIORef seen
                    atomicWrite seen []
                    pure calls
                expected key = [("parent-model", key), ("child-model", key), ("parent-model", key)]
            alice <- run (Just "alice")
            alice @?= expected "Bearer alice-key"
            bob <- run (Just "bob")
            bob @?= expected "Bearer bob-key"
            -- Not listed, and no owner at all: the host's shared keys.
            carol <- run (Just "carol")
            carol @?= expected "Bearer shared-key"
            nobody <- run Nothing
            nobody @?= expected "Bearer shared-key"
  where
    atomicWrite ref v = modifyIORef' ref (const v)

ownerKeysNotMergedTest :: Assertion
ownerKeysNotMergedTest = do
    seen <- newIORef []
    testWithApplication (pure (fakeLlm seen)) $ \port -> do
        -- Alice has keys of her own, but not the one the agents name.
        (host, _) <- keysHost port [("alice", [("another", "alice-key")])]
        withSessionRunner host $ \runner -> do
            meta <- expectRight =<< createSessionAs runner (Just "alice") "parent" (Just (message "delegate")) (Just UntilBlocked) Map.empty
            _ <- expectRight =<< awaitRun runner meta.smSessionId 10
            calls <- readIORef seen
            assertBool ("the shared key is never sent for alice, got " <> show calls) (all ((== "") . snd) calls)
            assertBool "the LLM was called" (not (null calls))

ownerKeysStartupTest :: Assertion
ownerKeysStartupTest = withSystemTempDirectory "owner-keys" $ \dir -> do
    let good = dir </> "good.json"
        bad = dir </> "bad.json"
        cfg owners = (defaultHostConfig [] "/dev/null" (dir </> "agents.db")){hcOwnerApiKeysFiles = owners}
        start owners = try (withHost (cfg owners) silent (\host -> pure (Map.keys host.hostOwnerApiKeys)))
    writeFile good "{\"keys\": [{\"id\": \"main\", \"value\": \"k\"}]}"
    writeFile bad "{\"keys\": 3}"
    start [("alice", good)] >>= \case
        Right owners -> owners @?= ["alice"]
        Left (e :: HostError) -> assertFailure ("expected a start, got " <> show e)
    let refused label owners =
            start owners >>= \case
                Left (OwnerApiKeysFailed _ _ _) -> pure ()
                other -> assertFailure (label <> ": expected OwnerApiKeysFailed, got " <> show other)
    refused "unparseable" [("alice", bad)]
    refused "missing" [("alice", dir </> "missing.json")]
    refused "twice" [("alice", good), ("alice", good)]
    refused "empty owner" [("", good)]

{- | A host with a parent agent that delegates to a child (a real sub-session),
both calling the fake LLM on 'port' with key id @none@. The shared keys hold
@none = shared-key@.
-}
keysHost :: Int -> [(Text, [(Text, ByteString.ByteString)])] -> IO (Host, OSAgentNode)
keysHost port owners = do
    let url = "http://127.0.0.1:" <> show port <> "/v1"
        node slug model = testNode ("{\"slug\": \"" <> slug <> "\", \"modelUrl\": \"" <> url <> "\", \"modelName\": \"" <> model <> "\", \"toolCallPolicyConfig\": {\"default\": {\"tag\": \"runSync\"}, \"rules\": []}}")
    child <- node "child" "child-model"
    parent0 <- node "parent" "parent-model"
    let parent = parent0{osNodeChildren = [child]}
    host0 <- testHost [parent] (const mockCompletion)
    let loaded keys = [(i, OpenAI.ApiKey v) | (i, v) <- keys]
        deps = host0.hostDeps{adCompletion = Nothing, adApiKeys = loaded [("none", "shared-key")]}
        host =
            host0
                { hostDeps = deps
                , -- As 'withHostStores' does with per-owner keys: no key at all.
                  hostSubAgentDeps = host0.hostSubAgentDeps{adCompletion = Nothing, adApiKeys = []}
                , hostOwnerApiKeys = Map.fromList [(owner, loaded keys) | (owner, keys) <- owners]
                }
    agentId <- AgentId <$> nextRandom
    atomically $ writeTVar parent.osNodeTools [OneShotTool.turnAgentRuntimeIntoIOTool silent host.hostSubAgentDeps child "parent" agentId Nothing True]
    pure (host, parent)

{- | An OpenAI-compatible endpoint recording each request's model and
@Authorization@ header. The parent model first delegates to the child, then
answers; the child answers at once.
-}
fakeLlm :: IORef [(Text, Text)] -> Wai.Application
fakeLlm seen req respond = do
    body <- Wai.strictRequestBody req
    let request = fromMaybe Aeson.Null (Aeson.decode body)
        model = case field "model" request of
            Aeson.String m -> m
            _ -> ""
        auth = maybe "" Text.decodeUtf8 (lookup "Authorization" (Wai.requestHeaders req))
        toolAnswered = case field "messages" request of
            Aeson.Array msgs -> any ((== Aeson.String "tool") . field "role") (Vector.toList msgs)
            _ -> False
        answer
            | model == "parent-model" && not toolAnswered =
                Aeson.object
                    [ "role" Aeson..= ("assistant" :: Text)
                    , "content" Aeson..= Aeson.Null
                    , "tool_calls"
                        Aeson..= [ Aeson.object
                                    [ "id" Aeson..= ("call_1" :: Text)
                                    , "type" Aeson..= ("function" :: Text)
                                    , "function" Aeson..= Aeson.object ["name" Aeson..= ("io_prompt_agent_child" :: Text), "arguments" Aeson..= ("{\"what\": \"help\"}" :: Text)]
                                    ]
                                 ]
                    ]
            | otherwise = Aeson.object ["role" Aeson..= ("assistant" :: Text), "content" Aeson..= ("done" :: Text)]
    modifyIORef' seen (<> [(model, auth)])
    respond $
        Wai.responseLBS status200 [("Content-Type", "application/json")] $
            Aeson.encode $
                Aeson.object ["choices" Aeson..= [Aeson.object ["index" Aeson..= (0 :: Int), "message" Aeson..= answer, "finish_reason" Aeson..= ("stop" :: Text)]]]
  where
    field :: Text -> Aeson.Value -> Aeson.Value
    field k v = case Aeson.fromJSON v :: Aeson.Result (Map.Map Text Aeson.Value) of
        Aeson.Success m -> fromMaybe Aeson.Null (Map.lookup k m)
        Aeson.Error _ -> Aeson.Null

-------------------------------------------------------------------------------
-- Tool isolation
-------------------------------------------------------------------------------

bashInProcessTest :: Assertion
bashInProcessTest = withBashFixture $ \fx -> do
    texts <- runBashCall fx Nothing explicitRunSync
    ranInProcess <- doesFileExist fx.fxInProcessMarker
    assertBool "the script ran on the host" ranInProcess
    assertBool ("the script's output is the result, got " <> show texts) (any ("in-process" `Text.isInfixOf`) texts)

bashIsolatedTest :: Assertion
bashIsolatedTest = withBashFixture $ \fx -> do
    isolation <- either (assertFailure . Text.unpack) pure (toolIsolationFromSpec (LocalProcess fx.fxWorker))
    -- The agent's own policy says "run in-process, synchronously"; the host wins.
    texts <- runBashCall fx (Just isolation) explicitRunSync
    texts @?= ["from worker"]
    ranInProcess <- doesFileExist fx.fxInProcessMarker
    assertBool "the script did not run on the host" (not ranInProcess)
    envelope <- Aeson.eitherDecodeFileStrict fx.fxEnvelope
    case envelope of
        Left err -> assertFailure ("the worker got no envelope: " <> err)
        Right (v :: Aeson.Value) -> do
            let text = Text.decodeUtf8 (ByteString.toStrict (Aeson.encode v))
            assertBool ("the envelope names the tool, got " <> show text) ("bash_touch" `Text.isInfixOf` text)
            assertBool ("the envelope says runIsolated, got " <> show text) ("runIsolated" `Text.isInfixOf` text)

bashIsolationFailsClosedTest :: Assertion
bashIsolationFailsClosedTest = withBashFixture $ \fx -> do
    isolation <- either (assertFailure . Text.unpack) pure (toolIsolationFromSpec (LocalProcess (fx.fxDir </> "no-such-worker")))
    texts <- runBashCall fx (Just isolation) explicitRunSync
    assertBool ("the call fails with an isolation error, got " <> show texts) (all ("isolation error" `Text.isPrefixOf`) texts && not (null texts))
    ranInProcess <- doesFileExist fx.fxInProcessMarker
    assertBool "the script did not run on the host" (not ranInProcess)

otherToolsInProcessTest :: Assertion
otherToolsInProcessTest = withBashFixture $ \fx -> do
    isolation <- either (assertFailure . Text.unpack) pure (toolIsolationFromSpec (LocalProcess fx.fxWorker))
    node <- testNode explicitRunSync
    atomically $ writeTVar node.osNodeTools [ioTool]
    texts <- runCall node (Just isolation) "io_plain"
    texts @?= ["plain result"]
    workerRan <- doesFileExist fx.fxEnvelope
    assertBool "the worker was not called" (not workerRan)
  where
    ioTool :: ToolRegistration
    ioTool =
        registerIOScriptInLLM
            (IOTools.IOScript (IOTools.IOScriptDescription "plain" "an in-process tool") (\_ctx (_ :: Aeson.Value) -> pure "plain result"))
            []

portalRefusedTest :: Assertion
portalRefusedTest = withBashFixture $ \fx -> do
    isolation <- either (assertFailure . Text.unpack) pure (toolIsolationFromSpec (LocalProcess fx.fxWorker))
    node <- testNode "{}"
    atomically $ writeTVar node.osNodeTools [fx.fxTool]
    convId <- ConversationId <$> nextRandom
    let deps = (defaultAgentDeps []){adCompletion = Just (const mockCompletion), adToolIsolation = Just isolation}
    agent <- buildAgent silent deps RootAgent convId node
    result <- agent.toolPortal Nothing (Ctx.ToolCall "bash_touch" (Aeson.object []))
    result.resultTraceId @?= "isolation-refused"
    ranInProcess <- doesFileExist fx.fxInProcessMarker
    assertBool "the script did not run on the host" (not ranInProcess)

functionRunnerRefusedTest :: Assertion
functionRunnerRefusedTest =
    case toolIsolationFromSpec (FunctionRunner "anything") of
        Left _ -> pure ()
        Right _ -> assertFailure "a function runner cannot run tool calls"

data BashFixture = BashFixture
    { fxDir :: FilePath
    , fxTool :: ToolRegistration
    -- ^ @bash_touch@: leaves 'fxInProcessMarker' when it runs on the host.
    , fxInProcessMarker :: FilePath
    , fxWorker :: FilePath
    -- ^ An isolation worker: saves its envelope to 'fxEnvelope', answers "from worker".
    , fxEnvelope :: FilePath
    }

withBashFixture :: (BashFixture -> IO a) -> IO a
withBashFixture k = withSystemTempDirectory "tool-isolation" $ \dir -> do
    let script = dir </> "touch.sh"
        marker = dir </> "ran-in-process"
        worker = dir </> "worker.sh"
        envelope = dir </> "envelope.json"
    writeFile script $ unlines ["#!/usr/bin/env bash", "touch '" <> marker <> "'", "echo in-process"]
    writeFile worker $
        unlines
            [ "#!/usr/bin/env bash"
            , "set -e"
            , "cat > '" <> envelope <> "'"
            , "TOKEN=$(sed -n 's/.*\"token\":\"\\([^\"]*\\)\".*/\\1/p' '" <> envelope <> "' | head -1)"
            , "echo \"{\\\"token\\\":\\\"$TOKEN\\\",\\\"status\\\":\\\"success\\\",\\\"result\\\":{\\\"type\\\":\\\"text\\\",\\\"content\\\":\\\"from worker\\\"}}\""
            ]
    mapM_ (\f -> setPermissions f emptyPermissions{readable = True, executable = True}) [script, worker]
    let tool = registerBashToolInLLM Nothing (ScriptDescription script (ScriptInfo [] "touch" "leaves a marker" Nothing Nothing))
    k (BashFixture dir tool marker worker envelope)

-- | Agent config whose own policy runs every call in-process, synchronously.
explicitRunSync :: String
explicitRunSync = "{\"toolCallPolicyConfig\": {\"default\": {\"tag\": \"runSync\"}, \"rules\": []}}"

-- | Have an agent with the fixture's bash tool call it once; the tool results.
runBashCall :: BashFixture -> Maybe ToolIsolation -> String -> IO [Text]
runBashCall fx isolation config = do
    node <- testNode config
    atomically $ writeTVar node.osNodeTools [fx.fxTool]
    runCall node isolation "bash_touch"

-- | Run a session in which the LLM calls the named tool once; the tool results.
runCall :: OSAgentNode -> Maybe ToolIsolation -> Text -> IO [Text]
runCall node isolation toolName = do
    host0 <- testHost [node] (const (firstThen [toolCallNamed toolName]))
    let host = host0{hostDeps = host0.hostDeps{adToolIsolation = isolation}}
    withSessionRunner host $ \runner -> do
        meta <- expectRight =<< createSession runner (Base.slug node.osNodeConfig) (message "go") (Just UntilBlocked)
        (final, _) <- expectRight =<< awaitRun runner meta.smSessionId 10
        final.smStatus @?= StatusIdle
        maybe (assertFailure "session missing") (pure . responseTexts . fst) =<< getSession runner meta.smSessionId

toolCallNamed :: Text -> LlmToolCall
toolCallNamed name =
    LlmToolCall $
        Aeson.object
            [ "id" Aeson..= ("call_1" :: Text)
            , "type" Aeson..= ("function" :: Text)
            , "function" Aeson..= Aeson.object ["name" Aeson..= name, "arguments" Aeson..= ("{}" :: Text)]
            ]
