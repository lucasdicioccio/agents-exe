{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | Integration tests for @agents-server@: the application on a random
port, a mock LLM, and real HTTP requests.
-}
module Main (main) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types (Method, statusCode, urlEncode)
import Network.Wai.Handler.Warp (testWithApplication)
import Prod.Tracer (silent)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import AgentsServer.Api
import System.Agents.AgentFactory (Completion)
import System.Agents.Host
import System.Agents.Host.Runner (withSessionRunner)
import System.Agents.Session.Base (LlmCompletion (..), LlmResponse (..), LlmToolCall (..))

main :: IO ()
main =
    defaultMain $
        testGroup
            "agents-server"
            [ testCase "events stream: snapshot, then each run until it stops" eventsFlowTest
            , testCase "wait=true: blocked session, then final answer, no events needed" waitFlowTest
            , testCase "event streams send keepalives" keepAliveTest
            , testCase "listing pages by update time, deleting with a dry run" listDeleteTest
            , testCase "agents and health" agentsHealthTest
            , testCase "errors have a status and a code" errorsTest
            , testCase "shutdown ends event streams and releases waiting requests" shutdownTest
            ]

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

eventsFlowTest :: Assertion
eventsFlowTest = withServer deferAll (firstThen [remoteCall "call_1"]) $ \srv -> do
    (created, view) <- call srv "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
    created @?= 201
    field "status" view @?= "ready"
    let sid = textField "session_id" view
    withEvents srv sid $ \next -> do
        (kind0, snapshot) <- next
        (kind0, field "status" snapshot) @?= ("snapshot", "ready")
        (resumed, _) <- call srv "POST" ("/v1/sessions/" <> sid <> "/resume") Nothing
        assertBool ("resume answers 200 or 202, got " <> show resumed) (resumed `elem` [200, 202])
        firstRun <- untilStopped next
        let kinds = map fst firstRun
        assertBool ("first run: " <> show kinds) ("run.started" `elem` kinds && "session.updated" `elem` kinds)
        drop (length kinds - 2) kinds @?= ["calls.deferred", "run.stopped"]
        field "status" (snd (last firstRun)) @?= "waiting_external"
        token <- case [d | ("calls.deferred", d) <- firstRun] of
            [d] | [c] <- arrayField "calls" d -> pure (textField "continuation_token" c)
            other -> assertFailure ("expected one deferred call, got " <> show other)
        (completed, _) <- call srv "POST" ("/v1/continuations/" <> token) (Just (Aeson.object ["result" .= ("42" :: Text)]))
        assertBool ("continuation answers 200 or 202, got " <> show completed) (completed `elem` [200, 202])
        secondRun <- untilStopped next
        assertBool ("second run: " <> show (map fst secondRun)) ("run.started" `elem` map fst secondRun)
        field "status" (snd (last secondRun)) @?= "idle"

waitFlowTest :: Assertion
waitFlowTest = withServer deferAll (firstThen [remoteCall "call_1"]) $ \srv -> do
    (created, blocked) <- call srv "POST" "/v1/sessions?wait=true" (Just (createBody []))
    created @?= 201
    field "status" blocked @?= "waiting_external"
    let sid = textField "session_id" blocked
    token <- case arrayField "pending" blocked of
        [c] -> pure (textField "continuation_token" c)
        other -> assertFailure ("expected one pending call, got " <> show other)
    (refused, err) <- call srv "POST" ("/v1/sessions/" <> sid <> "/messages") (Just (Aeson.object ["prompt" .= ("more" :: Text)]))
    (refused, field "error" err) @?= (409, "not_accepting_messages")
    (done, final) <- call srv "POST" ("/v1/continuations/" <> token <> "?wait=true") (Just (Aeson.object ["result" .= ("42" :: Text)]))
    done @?= 200
    field "status" final @?= "idle"
    arrayField "pending" final @?= []
    let transcript = LByteString.toStrict (Aeson.encode (field "session" final))
    assertBool "the tool result is in the session" ("42" `ByteString.isInfixOf` transcript)
    assertBool "the final answer is in the session" ("done" `ByteString.isInfixOf` transcript)
    (again, err2) <- call srv "POST" ("/v1/continuations/" <> token) (Just (Aeson.object ["result" .= ("43" :: Text)]))
    (again, field "error" err2) @?= (409, "token_already_completed")
    -- The mock LLM calls the tool again for the new message.
    (followUp, next) <- call srv "POST" ("/v1/sessions/" <> sid <> "/messages?wait=true") (Just (Aeson.object ["prompt" .= ("again" :: Text)]))
    (followUp, field "status" next) @?= (200, "waiting_external")
    (pendingStatus, pending) <- call srv "GET" ("/v1/sessions/" <> sid <> "/pending") Nothing
    (pendingStatus, length (arrayField "calls" pending)) @?= (200, 1)

keepAliveTest :: Assertion
keepAliveTest = withServer "{}" mockCompletion $ \srv -> do
    (_, view) <- call srv "POST" "/v1/sessions?wait=true" (Just (createBody []))
    withRawEvents srv (textField "session_id" view) $ \nextFrame -> do
        first <- nextFrame
        assertBool ("snapshot first: " <> show first) ("event: snapshot" `ByteString.isPrefixOf` first)
        second <- nextFrame
        second @?= ": keepalive"

listDeleteTest :: Assertion
listDeleteTest = withServer "{}" mockCompletion $ \srv -> do
    sids <- mapM (const (textField "session_id" . snd <$> call srv "POST" "/v1/sessions" (Just (createBody [("run", "none")])))) [1 :: Int, 2, 3]
    (_, page1) <- call srv "GET" "/v1/sessions?limit=2" Nothing
    length (arrayField "sessions" page1) @?= 2
    before <- case field "next_before" page1 of
        Aeson.String t -> pure t
        other -> assertFailure ("expected next_before, got " <> show other)
    (_, page2) <- call srv "GET" ("/v1/sessions?limit=2&before=" <> encode before) Nothing
    length (arrayField "sessions" page2) @?= 1
    field "next_before" page2 @?= Aeson.Null
    let listed = map (textField "session_id") (arrayField "sessions" page1 <> arrayField "sessions" page2)
    listed @?= reverse sids
    (_, ready) <- call srv "GET" "/v1/sessions?status=ready,idle&agent=server-test" Nothing
    length (arrayField "sessions" ready) @?= 3
    (_, running) <- call srv "GET" "/v1/sessions?status=running" Nothing
    arrayField "sessions" running @?= []
    (bogus, _) <- call srv "GET" "/v1/sessions?status=bogus" Nothing
    bogus @?= 400
    target <- case sids of
        (s : _) -> pure s
        [] -> assertFailure "no sessions created"
    (dryStatus, dry) <- call srv "DELETE" ("/v1/sessions/" <> target <> "?dry_run=true") Nothing
    (dryStatus, field "dry_run" dry, field "sessions" dry) @?= (200, Aeson.Bool True, Aeson.toJSON [target])
    (stillThere, _) <- call srv "GET" ("/v1/sessions/" <> target) Nothing
    stillThere @?= 200
    (deleted, plan) <- call srv "DELETE" ("/v1/sessions/" <> target) Nothing
    (deleted, field "dry_run" plan) @?= (200, Aeson.Bool False)
    (gone, err) <- call srv "GET" ("/v1/sessions/" <> target) Nothing
    (gone, field "error" err) @?= (404, "unknown_session")

agentsHealthTest :: Assertion
agentsHealthTest = withServer "{}" mockCompletion $ \srv -> do
    (status, agents) <- call srv "GET" "/v1/agents" Nothing
    status @?= 200
    case agents of
        Aeson.Array xs | [a] <- Vector.toList xs -> do
            field "slug" a @?= "server-test"
            field "description" a @?= "a test agent"
        other -> assertFailure ("expected one agent, got " <> show other)
    (healthStatus, health) <- call srv "GET" "/healthz" Nothing
    (healthStatus, field "ok" health) @?= (200, Aeson.Bool True)

errorsTest :: Assertion
errorsTest = withServer "{}" mockCompletion $ \srv -> do
    let expect path method body (status, code) = do
            (s, v) <- call srv method path body
            (path, s, field "error" v) @?= (path, status, code)
    expect "/v1/sessions" "POST" (Just (Aeson.object ["agent" .= ("nobody" :: Text), "prompt" .= ("hi" :: Text)])) (404, "unknown_agent")
    expect "/v1/sessions" "POST" (Just (Aeson.object ["agent" .= ("server-test" :: Text)])) (400, "bad_request")
    expect "/v1/sessions" "POST" (Just (createBody [("run", "forever")])) (400, "bad_request")
    expect "/v1/sessions?wait=maybe" "POST" (Just (createBody [])) (400, "bad_request")
    expect "/v1/sessions/not-a-uuid" "GET" Nothing (404, "unknown_session")
    expect "/v1/sessions/00000000-0000-0000-0000-000000000000" "GET" Nothing (404, "unknown_session")
    expect "/v1/continuations/00000000-0000-0000-0000-000000000000" "POST" (Just (Aeson.object ["result" .= ("x" :: Text)])) (404, "unknown_token")
    expect "/v1/nothing" "GET" Nothing (404, "not_found")
    expect "/v1/sessions" "PUT" Nothing (405, "method_not_allowed")
    (_, view) <- call srv "POST" "/v1/sessions?wait=true" (Just (createBody []))
    let sid = textField "session_id" view
    expect ("/v1/sessions/" <> sid <> "/cancel") "POST" Nothing (409, "no_active_run")

shutdownTest :: Assertion
shutdownTest = do
    gate <- newEmptyMVar
    withServer "{}" (\c -> readMVar gate >> mockCompletion c) $ \srv -> do
        (_, view) <- call srv "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
        let sid = textField "session_id" view
        withRawEvents srv sid $ \nextFrame -> do
            _snapshot <- nextFrame
            waiting <- async $ call srv "POST" ("/v1/sessions/" <> sid <> "/resume?wait=true&timeout=30") Nothing
            requestShutdown srv.srvEnv
            answered <- timeout 5_000_000 (wait waiting)
            case answered of
                Nothing -> assertFailure "the waiting request was not released"
                Just (status, current) -> (status, field "status" current) @?= (202, "running")
            ended <- timeout 5_000_000 nextFrame
            ended @?= Just ""
        putMVar gate ()

-------------------------------------------------------------------------------
-- Server fixture
-------------------------------------------------------------------------------

data Srv = Srv
    { srvPort :: Int
    , srvEnv :: ServerEnv
    , srvManager :: Http.Manager
    }

{- | Run the application on a free port over a fresh database, with one
agent (slug @server-test@, extra config fields merged in) and a mock LLM.
-}
withServer :: String -> Completion -> (Srv -> IO a) -> IO a
withServer extraConfig complete k =
    withSystemTempDirectory "agents-server" $ \dir -> do
        let agentFile = dir </> "agent.json"
            keysFile = dir </> "keys.json"
        extra <- either fail pure (Aeson.eitherDecodeStrict (Char8.pack extraConfig))
        LByteString.writeFile agentFile $
            Aeson.encode $
                Aeson.object ["tag" .= ("OpenAIAgentDescription" :: Text), "contents" .= merge baseConfig extra]
        writeFile keysFile "{}"
        let cfg = (defaultHostConfig [agentFile] keysFile (dir </> "agents.db")){hcCompletion = Just (const complete)}
        manager <- Http.newManager Http.defaultManagerSettings{Http.managerResponseTimeout = Http.responseTimeoutMicro 30_000_000}
        withHost cfg silent $ \host ->
            withSessionRunner host $ \runner -> do
                env0 <- newServerEnv host runner
                let env = env0{envKeepAlive = 300_000}
                testWithApplication (pure (application env)) $ \p -> k (Srv p env manager)
  where
    baseConfig =
        Aeson.object
            [ "slug" .= ("server-test" :: Text)
            , "apiKeyId" .= ("none" :: Text)
            , "flavor" .= ("OpenAIv1" :: Text)
            , "modelUrl" .= ("http://127.0.0.1:1" :: Text)
            , "modelName" .= ("mock" :: Text)
            , "announce" .= ("a test agent" :: Text)
            , "systemPrompt" .= ["You are a test" :: Text]
            , "builtinToolboxes" .= ([] :: [Text])
            , "mcpServers" .= ([] :: [Text])
            ]
    merge (Aeson.Object a) (Aeson.Object b) = Aeson.Object (KeyMap.union b a)
    merge a _ = a

-- | Agent config: asynchronous, every call deferred to an external worker.
deferAll :: String
deferAll = "{\"executionMode\": \"asynchronous\", \"toolCallPolicyConfig\": {\"default\": {\"tag\": \"defer\", \"reason\": \"external\"}, \"rules\": []}}"

createBody :: [(Text, Text)] -> Aeson.Value
createBody extra =
    Aeson.object $
        ["agent" .= ("server-test" :: Text), "prompt" .= ("fetch it" :: Text)]
            <> [Key.fromText k .= v | (k, v) <- extra]

-------------------------------------------------------------------------------
-- Mock LLM
-------------------------------------------------------------------------------

mockCompletion :: Completion
mockCompletion _ = pure (LlmResponse (Just "done") Nothing Aeson.Null Nothing, [])

-- | The first completion of a turn calls the tools; the next one answers.
firstThen :: [LlmToolCall] -> Completion
firstThen calls completion
    | null completion.completeToolResponses = pure (LlmResponse Nothing Nothing Aeson.Null Nothing, calls)
    | otherwise = mockCompletion completion

remoteCall :: Text -> LlmToolCall
remoteCall callId =
    LlmToolCall $
        Aeson.object
            [ "id" .= callId
            , "type" .= ("function" :: Text)
            , "function" .= Aeson.object ["name" .= ("fetch_remote" :: Text), "arguments" .= ("{}" :: Text)]
            ]

-------------------------------------------------------------------------------
-- HTTP client
-------------------------------------------------------------------------------

request :: Srv -> Method -> Text -> Maybe Aeson.Value -> IO Http.Request
request srv method path body = do
    req <- Http.parseRequest ("http://127.0.0.1:" <> show srv.srvPort <> Text.unpack path)
    pure
        req
            { Http.method = method
            , Http.requestBody = maybe mempty (Http.RequestBodyLBS . Aeson.encode) body
            , Http.requestHeaders = [("Content-Type", "application/json") | Just _ <- [body]]
            }

-- | Status and JSON body (@null@ when empty).
call :: Srv -> Method -> Text -> Maybe Aeson.Value -> IO (Int, Aeson.Value)
call srv method path body = do
    req <- request srv method path body
    rsp <- Http.httpLbs req srv.srvManager
    let raw = Http.responseBody rsp
    value <-
        if LByteString.null raw
            then pure Aeson.Null
            else either (\e -> assertFailure ("not JSON (" <> e <> "): " <> show raw)) pure (Aeson.eitherDecode raw)
    pure (statusCode (Http.responseStatus rsp), value)

{- | Open a session's event stream; the action reads the next raw frame
(without its trailing blank line), or @""@ once the stream has ended.
-}
withRawEvents :: Srv -> Text -> (IO ByteString.ByteString -> IO a) -> IO a
withRawEvents srv sid k = do
    req <- request srv "GET" ("/v1/sessions/" <> sid <> "/events") Nothing
    Http.withResponse req srv.srvManager $ \rsp -> do
        buffer <- newIORef ByteString.empty
        let nextFrame = do
                buf <- readIORef buffer
                case ByteString.breakSubstring "\n\n" buf of
                    (frame, rest)
                        | not (ByteString.null rest) -> do
                            writeIORef buffer (ByteString.drop 2 rest)
                            pure frame
                    _ -> do
                        chunk <- Http.brRead (Http.responseBody rsp)
                        if ByteString.null chunk
                            then writeIORef buffer ByteString.empty >> pure buf
                            else writeIORef buffer (buf <> chunk) >> nextFrame
        k nextFrame

-- | Like 'withRawEvents', parsed, skipping keepalives; 5 seconds per event.
withEvents :: Srv -> Text -> (IO (Text, Aeson.Value) -> IO a) -> IO a
withEvents srv sid k = withRawEvents srv sid $ \nextFrame ->
    let next =
            timeout 5_000_000 nextFrame >>= \case
                Nothing -> assertFailure "no event within 5 seconds"
                Just frame
                    | ":" `ByteString.isPrefixOf` frame -> next
                    | otherwise -> parseFrame frame
     in k next
  where
    parseFrame frame = do
        let lines' = Char8.lines frame
            value prefix = [ByteString.drop (ByteString.length prefix) l | l <- lines', prefix `ByteString.isPrefixOf` l]
        case (value "event: ", value "data: ") of
            ([kind], [payload]) ->
                either (\e -> assertFailure ("bad event data: " <> e)) (pure . (Text.decodeUtf8 kind,)) (Aeson.eitherDecodeStrict payload)
            _ -> assertFailure ("bad frame: " <> show frame)

-- | Events up to and including the next @run.stopped@.
untilStopped :: IO (Text, Aeson.Value) -> IO [(Text, Aeson.Value)]
untilStopped next = go []
  where
    go acc = do
        event <- next
        let acc' = event : acc
        if fst event == "run.stopped" then pure (reverse acc') else go acc'

-------------------------------------------------------------------------------
-- JSON helpers
-------------------------------------------------------------------------------

field :: Text -> Aeson.Value -> Aeson.Value
field name = \case
    Aeson.Object o | Just v <- KeyMap.lookup (Key.fromText name) o -> v
    _ -> Aeson.Null

textField :: Text -> Aeson.Value -> Text
textField name v = case field name v of
    Aeson.String t -> t
    _ -> ""

arrayField :: Text -> Aeson.Value -> [Aeson.Value]
arrayField name v = case field name v of
    Aeson.Array xs -> Vector.toList xs
    _ -> []

encode :: Text -> Text
encode = Text.decodeUtf8 . urlEncode True . Text.encodeUtf8
