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

import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types (Header, Method, status200, statusCode, urlEncode)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Prod.Tracer (silent)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

import AgentsServer.Api
import AgentsServer.Auth (authTokensFromList, authenticate, bearerToken, loadAuthTokens, tokenDigest)
import AgentsServer.Log (silentLogger)
import AgentsServer.Server (ServerOptions (..), runServer)
import Control.Exception (IOException, try)
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
            , testCase "with tokens, callers need one and only see their own sessions" authTest
            , testCase "sealed sessions and session tokens" sessionTokenTest
            , testCase "tokens files hold hashed or plain tokens" tokensFileTest
            , testCase "MCP over HTTP: initialize, list tools, call an agent" mcpTest
            , testCase "MCP over HTTP: a call stopping on deferred calls reports the tokens" mcpDeferredTest
            , testCase "MCP over HTTP: Agents-Param- headers and _meta set session params" mcpParamsTest
            , testCase "PUT params, 409 params_required on lapsed params, fork with params" sessionParamsTest
            , testCase "without authentication, non-local browser origins are refused" originTest
            , testCase "CORS: preflight, matching origins, refused origins, SSE" corsTest
            , testCase "CORS: --cors-origin '*' is refused at startup with --auth-tokens" corsWildcardStartupTest
            , testCase "with --stream-tokens, answers arrive as text.delta events" streamingTest
            , testCase "admins store agents, which serve sessions and MCP until deleted" storedAgentsTest
            , testCase "without admin owners, storing agents is disabled" agentEditsDisabledTest
            , testCase "the openapi document covers every route and resolves" openApiTest
            , testCase "the chat page is served only when it is enabled" uiPageTest
            , testCase "EventSource may carry its token as a query parameter" accessTokenTest
            , testCase "an attached file is stored on the turn that carried it" mediaRoundTripTest
            , testCase "--socket: /healthz answers over a Unix domain socket" socketHealthzTest
            , testCase "reconnecting with Last-Event-ID replays exactly the missed events" reconnectLastEventIdTest
            , testCase "GET /v1/events sees session.created and session.deleted" crossSessionEventsTest
            , testCase "GET /v1/events is owner-scoped when auth is on" ownerScopedEventsTest
            , testCase "POST /v1/sessions with no prompt creates an idle session with no turn" createNoPromptTest
            , testCase "POST and GET /v1/sessions/:id/mail send and list mail" mailRouteTest
            , testCase "POST /v1/sessions/:id/fork forks whole, at a turn, with a new agent, and refuses bad input" forkRouteTest
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

{- | A client that disconnects mid-run and reconnects with @Last-Event-ID@
(G5) gets exactly the events it missed, replayed from the runner's ring,
and nothing it already saw.
-}
reconnectLastEventIdTest :: Assertion
reconnectLastEventIdTest = withServer deferAll (firstThen [remoteCall "call_1"]) $ \srv -> do
    (created, view) <- call srv "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
    created @?= 201
    let sid = textField "session_id" view
    firstBatch <- withIdEvents srv ("/v1/sessions/" <> sid <> "/events") $ \next -> do
        (kind0, _, _) <- next
        kind0 @?= "snapshot"
        (resumed, _) <- call srv "POST" ("/v1/sessions/" <> sid <> "/resume") Nothing
        assertBool ("resume answers 200 or 202, got " <> show resumed) (resumed `elem` [200, 202])
        untilStoppedWithId next
    (_, cutId, _) <- case firstBatch of
        (e : _) -> pure e
        [] -> assertFailure "expected at least one event"
    let srv2 = srv{srvHeaders = [("Last-Event-ID", Text.encodeUtf8 cutId)]}
    replayed <- withIdEvents srv2 ("/v1/sessions/" <> sid <> "/events") untilStoppedWithId
    -- No "snapshot": the reconnect replays from the ring instead.
    map (\(k, _, _) -> k) replayed @?= drop 1 (map (\(k, _, _) -> k) firstBatch)
    replayed @?= drop 1 firstBatch

-- | 'GET \/v1\/events' (no auth) sees a session being created and deleted,
-- across sessions -- the server-wide feed (G5).
crossSessionEventsTest :: Assertion
crossSessionEventsTest = withServer "{}" mockCompletion $ \srv ->
    withEventsAt srv "/v1/events" $ \next -> do
        (created, view) <- call srv "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
        created @?= 201
        let sid = textField "session_id" view
        untilKind next "session.created"
        (deleted, _) <- call srv "DELETE" ("/v1/sessions/" <> sid) Nothing
        deleted @?= 200
        untilKind next "session.deleted"
  where
    untilKind next kind = do
        (k, _) <- next
        if k == kind then pure () else untilKind next kind

-- | With authentication on, @scope=all@ needs an admin owner, the default
-- scope is the caller's own, and one owner never sees another's events.
ownerScopedEventsTest :: Assertion
ownerScopedEventsTest = do
    let tokens = authTokensFromList [("alice-token", "alice"), ("bob-token", "bob")]
    withServerAuth (Just tokens) "{}" mockCompletion $ \anonymous -> do
        let alice = anonymous{srvToken = Just "alice-token"}
            bob = anonymous{srvToken = Just "bob-token"}
        (refused, err) <- call alice "GET" "/v1/events?scope=all" Nothing
        (refused, field "error" err) @?= (403, "forbidden")
        withEventsAt alice "/v1/events" $ \nextAlice -> do
            (createdA, _) <- call alice "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
            createdA @?= 201
            untilKind nextAlice "session.created"
            (createdB, _) <- call bob "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
            createdB @?= 201
            stray <- timeout 300_000 nextAlice
            stray @?= Nothing
  where
    untilKind next kind = do
        (k, _) <- next
        if k == kind then pure () else untilKind next kind

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

-- | G2: no `prompt` (and no `media`) creates an idle session with no turn
-- and no run, unlike `prompt` present which behaves as today.
createNoPromptTest :: Assertion
createNoPromptTest = withServer "{}" mockCompletion $ \srv -> do
    (status, body) <- call srv "POST" "/v1/sessions" (Just (Aeson.object ["agent" .= ("server-test" :: Text)]))
    status @?= 201
    field "status" body @?= Aeson.String "ready"
    arrayField "session" body @?= []
    let sid = textField "session_id" body
    (getStatus, got) <- call srv "GET" ("/v1/sessions/" <> sid) Nothing
    getStatus @?= 200
    field "status" got @?= Aeson.String "ready"
    arrayField "session" got @?= []

-- | POST posts a MailBody and answers a Receipt; GET lists it back, and
-- filters to only what is unread (which, with nothing ever run against
-- this session, is everything).
mailRouteTest :: Assertion
mailRouteTest = withServer "{}" mockCompletion $ \srv -> do
    (created, view) <- call srv "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
    created @?= 201
    let sid = textField "session_id" view
        mailBody =
            Aeson.object
                [ "body" .= Aeson.object ["tag" .= ("agentMessage" :: Text), "text" .= ("hi there" :: Text), "expectsReply" .= False]
                , "priority" .= ("normal" :: Text)
                ]
    (posted, receipt) <- call srv "POST" ("/v1/sessions/" <> sid <> "/mail") (Just mailBody)
    posted @?= 202
    case field "id" receipt of
        Aeson.String _ -> pure ()
        other -> assertFailure ("expected a receipt id, got " <> show other)
    field "duplicate" receipt @?= Aeson.Bool False

    (listed, page) <- call srv "GET" ("/v1/sessions/" <> sid <> "/mail") Nothing
    listed @?= 200
    length (arrayField "mail" page) @?= 1

    (listedUnread, unreadPage) <- call srv "GET" ("/v1/sessions/" <> sid <> "/mail?unread=true") Nothing
    listedUnread @?= 200
    length (arrayField "mail" unreadPage) @?= 1

    (missing, err) <- call srv "GET" "/v1/sessions/00000000-0000-0000-0000-000000000000/mail" Nothing
    (missing, field "error" err) @?= (404, "unknown_session")

{- | Fork whole (default), fork at a turn (a prefix), an unknown turn index
(404 unknown_turn), and an unknown agent slug (404 unknown_agent).
-}
forkRouteTest :: Assertion
forkRouteTest = withServer "{}" mockCompletion $ \srv -> do
    (created, view) <- call srv "POST" "/v1/sessions?wait=true" (Just (createBody []))
    created @?= 201
    let sid = textField "session_id" view
        sourceTurns = arrayField "turns" (field "session" view)
    assertBool "at least two turns" (length sourceTurns >= 2)

    (forkedStatus, forked) <- call srv "POST" ("/v1/sessions/" <> sid <> "/fork") (Just (Aeson.object []))
    forkedStatus @?= 201
    field "forkedFromSessionId" (field "session" forked) @?= Aeson.String sid
    arrayField "turns" (field "session" forked) @?= sourceTurns

    (atTurnStatus, atTurnForked) <- call srv "POST" ("/v1/sessions/" <> sid <> "/fork") (Just (Aeson.object ["at_turn" .= (1 :: Int)]))
    atTurnStatus @?= 201
    arrayField "turns" (field "session" atTurnForked) @?= drop 1 sourceTurns

    (badTurn, badTurnErr) <- call srv "POST" ("/v1/sessions/" <> sid <> "/fork") (Just (Aeson.object ["at_turn" .= (999 :: Int)]))
    (badTurn, field "error" badTurnErr) @?= (404, "unknown_turn")

    (badAgent, badAgentErr) <- call srv "POST" ("/v1/sessions/" <> sid <> "/fork") (Just (Aeson.object ["agent" .= ("no-such-agent" :: Text)]))
    (badAgent, field "error" badAgentErr) @?= (404, "unknown_agent")

agentsHealthTest :: Assertion
agentsHealthTest = withServer "{}" mockCompletion $ \srv -> do
    (status, agents) <- call srv "GET" "/v1/agents" Nothing
    status @?= 200
    case agents of
        Aeson.Array xs | [a] <- Vector.toList xs -> do
            field "slug" a @?= "server-test"
            field "description" a @?= "a test agent"
            -- G6 (@todos/os-as-standalone-server.md@ Phase 3a): the agent
            -- descriptor carries model, system prompt and tools (now
            -- objects, not bare names), not just slug/description.
            field "model" a @?= "mock"
            field "system_prompt" a @?= Aeson.Array (Vector.fromList ["You are a test"])
            arrayField "tools" a @?= []
        other -> assertFailure ("expected one agent, got " <> show other)
    (healthStatus, health) <- call srv "GET" "/healthz" Nothing
    (healthStatus, field "ok" health) @?= (200, Aeson.Bool True)

errorsTest :: Assertion
errorsTest = withServer "{}" mockCompletion $ \srv -> do
    let expect path method body (status, code) = do
            (s, v) <- call srv method path body
            (path, s, field "error" v) @?= (path, status, code)
    expect "/v1/sessions" "POST" (Just (Aeson.object ["agent" .= ("nobody" :: Text), "prompt" .= ("hi" :: Text)])) (404, "unknown_agent")
    -- No prompt is valid (G2: an idle session with no turn); no agent is not.
    expect "/v1/sessions" "POST" (Just (Aeson.object [])) (400, "bad_request")
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

authTest :: Assertion
authTest = do
    let tokens = authTokensFromList [("alice-token", "alice"), ("bob-token", "bob")]
    withServerAuth (Just tokens) deferAll (firstThen [remoteCall "call_1"]) $ \anonymous -> do
        let alice = anonymous{srvToken = Just "alice-token"}
            bob = anonymous{srvToken = Just "bob-token"}
            stranger = anonymous{srvToken = Just "guessed"}
        (health, _) <- call anonymous "GET" "/healthz" Nothing
        health @?= 200
        (noToken, err) <- call anonymous "GET" "/v1/agents" Nothing
        (noToken, field "error" err) @?= (401, "unauthorized")
        (badToken, _) <- call stranger "GET" "/v1/agents" Nothing
        badToken @?= 401
        (created, view) <- call alice "POST" "/v1/sessions?wait=true" (Just (createBody []))
        (created, field "owner" view) @?= (201, "alice")
        let sid = textField "session_id" view
        token <- case arrayField "pending" view of
            [c] -> pure (textField "continuation_token" c)
            other -> assertFailure ("expected one pending call, got " <> show other)
        let hidden who path method body code = do
                (status, v) <- call who method path body
                (path, status, field "error" v) @?= (path, 404, code)
        hidden bob ("/v1/sessions/" <> sid) "GET" Nothing "unknown_session"
        hidden bob ("/v1/sessions/" <> sid <> "/events") "GET" Nothing "unknown_session"
        hidden bob ("/v1/sessions/" <> sid) "DELETE" Nothing "unknown_session"
        hidden bob ("/v1/sessions?parent=" <> sid) "GET" Nothing "unknown_session"
        hidden bob ("/v1/continuations/" <> token) "POST" (Just (Aeson.object ["result" .= ("x" :: Text)])) "unknown_token"
        (_, bobList) <- call bob "GET" "/v1/sessions" Nothing
        arrayField "sessions" bobList @?= []
        (_, aliceList) <- call alice "GET" "/v1/sessions" Nothing
        map (textField "session_id") (arrayField "sessions" aliceList) @?= [sid]
        (done, final) <- call alice "POST" ("/v1/continuations/" <> token <> "?wait=true") (Just (Aeson.object ["result" .= ("42" :: Text)]))
        (done, field "status" final) @?= (200, "idle")

sessionTokenTest :: Assertion
sessionTokenTest = do
    let tokens = authTokensFromList [("alice-token", "alice")]
        obj = Aeson.object
    withServerAuth (Just tokens) withTenantParam mockCompletion $ \anonymous -> do
        let alice = anonymous{srvToken = Just "alice-token"}
            params = ["params" .= obj ["tenant" .= ("acme" :: Text)]]
        -- alice mints a sealed session with a token, and another session
        (created, view) <- call alice "POST" "/v1/sessions" (Just (obj (["agent" .= ("server-test" :: Text), "seal" .= True, "session_token" .= True] <> params)))
        created @?= 201
        field "sealed" view @?= Aeson.Bool True
        let sid = textField "session_id" view
            minted = textField "session_token" view
            path = "/v1/sessions/" <> sid
            holder = anonymous{srvToken = Just (Text.encodeUtf8 minted)}
        (_, other) <- call alice "POST" "/v1/sessions" (Just (obj (["agent" .= ("server-test" :: Text)] <> params)))
        let otherPath = "/v1/sessions/" <> textField "session_id" other
        assertBool "the token has a recognisable shape" ("st_" `Text.isPrefixOf` minted)
        -- the token is shown once: reads never carry it, and a session without one has none
        (_, again) <- call alice "GET" path Nothing
        assertBool "no token in a read" (not ("session_token" `Text.isInfixOf` Text.pack (show again)))
        assertBool "no token on a session created without one" (not ("session_token" `Text.isInfixOf` Text.pack (show other)))
        -- the holder reads, messages and cancels its own session
        (get, gv) <- call holder "GET" path Nothing
        (get, field "sealed" gv) @?= (200, Aeson.Bool True)
        (msg, _) <- call holder "POST" (path <> "/messages?wait=true") (Just (obj ["prompt" .= ("hi" :: Text)]))
        msg @?= 200
        -- ... but never parameters, on a sealed session or any other
        (withParams, pErr) <- call holder "POST" (path <> "/messages") (Just (obj ["prompt" .= ("hi" :: Text), "params" .= obj ["tenant" .= ("evil" :: Text)]]))
        (withParams, field "error" pErr) @?= (403, "forbidden_params")
        (put, _) <- call holder "PUT" (path <> "/params") (Just (obj ["params" .= obj ["tenant" .= ("evil" :: Text)]]))
        put @?= 403
        -- ... nor anything beyond it
        forM_ [("POST", path <> "/fork"), ("POST", path <> "/resume"), ("DELETE", path), ("GET", path <> "/mail"), ("GET", path <> "/pending"), ("DELETE", path <> "/token")] $ \(method, p) -> do
            (status, _) <- call holder method p (Just (obj []))
            (method, p, status) @?= (method, p, 403)
        -- ... and the token is no credential outside its own session's paths
        forM_ [("GET", "/v1/sessions"), ("GET", "/v1/agents"), ("GET", "/v1/events")] $ \(method, p) -> do
            (status, _) <- call holder method p Nothing
            (method, p, status) @?= (method, p, 401)
        -- ... on any other session: not even a 404 to probe with
        (elsewhere, _) <- call holder "GET" otherPath Nothing
        elsewhere @?= 401
        -- a wrong token, or the token on a session that has none
        (wrong, _) <- call anonymous{srvToken = Just "st_guess"} "GET" path Nothing
        wrong @?= 401
        -- the owner keeps control, and can still set parameters
        (ownerPut, _) <- call alice "PUT" (path <> "/params") (Just (obj ["params" .= obj ["tenant" .= ("acme-2" :: Text)]]))
        ownerPut @?= 200
        -- revoking the token ends the holder's access
        (revoked, _) <- call alice "DELETE" (path <> "/token") Nothing
        revoked @?= 200
        (afterRevoke, _) <- call holder "GET" path Nothing
        afterRevoke @?= 401
        (ownerStill, _) <- call alice "GET" path Nothing
        ownerStill @?= 200
    -- without authentication a session token means nothing, so it is refused
    withServer withTenantParam mockCompletion $ \open -> do
        (bad, err) <- call open "POST" "/v1/sessions" (Just (obj ["agent" .= ("server-test" :: Text), "session_token" .= True, "params" .= obj ["tenant" .= ("acme" :: Text)]]))
        (bad, field "error" err) @?= (400, "bad_request")
  where
    withTenantParam = "{\"parameters\": [{\"name\": \"tenant\", \"scope\": \"session\", \"required\": true}]}"

tokensFileTest :: Assertion
tokensFileTest = withSystemTempDirectory "agents-server-tokens" $ \dir -> do
    let path = dir </> "tokens.json"
        aliceDigest = tokenDigest "alice-token"
    writeFile path $
        "{\"tokens\": [{\"owner\": \"alice\", \"sha256\": \""
            <> Text.unpack (Text.toUpper aliceDigest)
            <> "\"}, "
            <> "{\"owner\": \"bob\", \"token\": \"bob-token\"}]}"
    tokens <- loadAuthTokens path
    map (authenticate tokens) ["alice-token", "bob-token", "carol-token"] @?= [Just "alice", Just "bob", Nothing]
    bearerToken "Bearer abc" @?= Just "abc"
    bearerToken "bearer abc" @?= Just "abc"
    bearerToken "Basic abc" @?= Nothing
    writeFile path "{\"tokens\": [{\"owner\": \"alice\", \"sha256\": \"not-hex\"}]}"
    bad <- try (loadAuthTokens path)
    case bad of
        Left (_ :: IOException) -> pure ()
        Right _ -> assertFailure "a malformed digest was accepted"

mcpTest :: Assertion
mcpTest = withServer "{}" mockCompletion $ \srv -> do
    let rpc i method params = Aeson.object ["jsonrpc" .= ("2.0" :: Text), "id" .= (i :: Int), "method" .= (method :: Text), "params" .= params]
    (initStatus, initialized) <- call srv "POST" "/mcp" (Just (rpc 1 "initialize" (Aeson.object ["protocolVersion" .= ("2025-03-26" :: Text), "capabilities" .= Aeson.object [], "clientInfo" .= Aeson.object ["name" .= ("test" :: Text), "version" .= ("1" :: Text)]])))
    initStatus @?= 200
    field "protocolVersion" (field "result" initialized) @?= "2025-03-26"
    (notified, empty) <- call srv "POST" "/mcp" (Just (Aeson.object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)]))
    (notified, empty) @?= (202, Aeson.Null)
    (_, listed) <- call srv "POST" "/mcp" (Just (rpc 2 "tools/list" (Aeson.object [])))
    case arrayField "tools" (field "result" listed) of
        [tool] -> do
            field "name" tool @?= "ask_server-test"
            field "required" (field "inputSchema" tool) @?= Aeson.toJSON ["prompt" :: Text]
        other -> assertFailure ("expected one tool, got " <> show other)
    (_, called) <- call srv "POST" "/mcp" (Just (rpc 3 "tools/call" (Aeson.object ["name" .= ("ask_server-test" :: Text), "arguments" .= Aeson.object ["prompt" .= ("hello" :: Text)]])))
    let result = field "result" called
    field "isError" result @?= Aeson.Bool False
    map (field "text") (arrayField "content" result) @?= ["done"]
    let sid = textField "session_id" (field "_meta" result)
    (getStatus, view) <- call srv "GET" ("/v1/sessions/" <> sid) Nothing
    (getStatus, field "status" view) @?= (200, "idle")
    (_, unknownTool) <- call srv "POST" "/mcp" (Just (rpc 4 "tools/call" (Aeson.object ["name" .= ("ask_nobody" :: Text), "arguments" .= Aeson.object ["prompt" .= ("hi" :: Text)]])))
    field "code" (field "error" unknownTool) @?= Aeson.Number (-32602)
    (_, unknownMethod) <- call srv "POST" "/mcp" (Just (rpc 5 "sampling/createMessage" (Aeson.object [])))
    field "code" (field "error" unknownMethod) @?= Aeson.Number (-32601)
    (_, batch) <- call srv "POST" "/mcp" (Just (Aeson.toJSON [rpc 6 "ping" (Aeson.object []), Aeson.object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/cancelled" :: Text)]]))
    case batch of
        Aeson.Array xs -> map (field "id") (Vector.toList xs) @?= [Aeson.Number 6]
        other -> assertFailure ("expected a batch answer, got " <> show other)
    (getMcp, _) <- call srv "GET" "/mcp" Nothing
    getMcp @?= 405

mcpDeferredTest :: Assertion
mcpDeferredTest = withServer deferAll (firstThen [remoteCall "call_1"]) $ \srv -> do
    let params = Aeson.object ["name" .= ("ask_server-test" :: Text), "arguments" .= Aeson.object ["prompt" .= ("fetch it" :: Text)]]
    (_, called) <- call srv "POST" "/mcp" (Just (Aeson.object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= ("tools/call" :: Text), "params" .= params]))
    let result = field "result" called
    field "isError" result @?= Aeson.Bool False
    token <- case arrayField "content" result of
        [_, calls] -> case Aeson.eitherDecodeStrict (Text.encodeUtf8 (textField "text" calls)) of
            Right (Aeson.Array xs) | [c] <- Vector.toList xs -> pure (textField "continuation_token" c)
            other -> assertFailure ("expected the pending calls, got " <> show other)
        other -> assertFailure ("expected two contents, got " <> show other)
    (done, final) <- call srv "POST" ("/v1/continuations/" <> token <> "?wait=true") (Just (Aeson.object ["result" .= ("42" :: Text)]))
    (done, field "status" final) @?= (200, "idle")

mcpParamsTest :: Assertion
mcpParamsTest =
    withServer withTenantParam mockCompletion $ \srv -> do
        let call' :: Text -> Text -> Maybe Aeson.Value -> Aeson.Value
            call' name prompt meta = Aeson.object $ ["name" .= name, "arguments" .= Aeson.object ["prompt" .= prompt]] <> maybe [] (\m -> ["_meta" .= m]) meta
            rpc i method params = Aeson.object ["jsonrpc" .= ("2.0" :: Text), "id" .= (i :: Int), "method" .= (method :: Text), "params" .= params]
        -- a missing required parameter is refused before the run starts
        (_, missing) <- call srv "POST" "/mcp" (Just (rpc 1 "tools/call" (call' "ask_server-test" "hi" Nothing)))
        field "isError" (field "result" missing) @?= Aeson.Bool True
        -- an Agents-Param- header supplies it
        let withHeader = srv{srvHeaders = [("Agents-Param-tenant", "acme-corp")]}
        (_, viaHeader) <- call withHeader "POST" "/mcp" (Just (rpc 2 "tools/call" (call' "ask_server-test" "hi" Nothing)))
        field "isError" (field "result" viaHeader) @?= Aeson.Bool False
        let sid1 = textField "session_id" (field "_meta" (field "result" viaHeader))
        (_, view1) <- call srv "GET" ("/v1/sessions/" <> sid1) Nothing
        field "tenant" (field "params" view1) @?= Aeson.String "acme-corp"
        -- _meta."agents-exe/params" on the call overrides the header
        let meta = Aeson.object ["agents-exe/params" .= Aeson.object ["tenant" .= ("from-meta" :: Text)]]
        (_, viaMeta) <- call withHeader "POST" "/mcp" (Just (rpc 3 "tools/call" (call' "ask_server-test" "hi" (Just meta))))
        field "isError" (field "result" viaMeta) @?= Aeson.Bool False
        let sid2 = textField "session_id" (field "_meta" (field "result" viaMeta))
        (_, view2) <- call srv "GET" ("/v1/sessions/" <> sid2) Nothing
        field "tenant" (field "params" view2) @?= Aeson.String "from-meta"
  where
    withTenantParam = "{\"parameters\": [{\"name\": \"tenant\", \"scope\": \"session\", \"required\": true}]}"

sessionParamsTest :: Assertion
sessionParamsTest =
    withServer withTenantParam mockCompletion $ \srv -> do
        let obj = Aeson.object
        (created, sess) <- call srv "POST" "/v1/sessions" (Just (obj ["agent" .= ("server-test" :: Text), "params" .= obj ["tenant" .= ("acme" :: Text)]]))
        created @?= 201
        let sid = textField "session_id" sess
            path = "/v1/sessions/" <> sid
        -- PUT rotates a value without starting a run
        (put, rotated) <- call srv "PUT" (path <> "/params") (Just (obj ["params" .= obj ["tenant" .= ("acme-2" :: Text)]]))
        put @?= 200
        field "tenant" (field "params" rotated) @?= Aeson.String "acme-2"
        -- validation table applies
        (unknown, unknownErr) <- call srv "PUT" (path <> "/params") (Just (obj ["params" .= obj ["nope" .= ("x" :: Text)]]))
        (unknown, field "error" unknownErr) @?= (422, "unknown_params")
        (noBody, _) <- call srv "PUT" (path <> "/params") (Just (obj []))
        noBody @?= 400
        -- clearing the value lapses it: a message now gets 409 params_required
        (cleared, _) <- call srv "PUT" (path <> "/params") (Just (obj ["params" .= obj ["tenant" .= Aeson.Null]]))
        cleared @?= 200
        (lapsed, lapsedErr) <- call srv "POST" (path <> "/messages") (Just (obj ["prompt" .= ("hi" :: Text)]))
        (lapsed, field "error" lapsedErr) @?= (409, "params_required")
        -- creation without the parameter stays 422
        (bare, bareErr) <- call srv "POST" "/v1/sessions" (Just (obj ["agent" .= ("server-test" :: Text)]))
        (bare, field "error" bareErr) @?= (422, "params_required")
        -- a fork starts from the source's values overlaid with the request's
        _ <- call srv "PUT" (path <> "/params") (Just (obj ["params" .= obj ["tenant" .= ("acme-3" :: Text)]]))
        (forked, fork1) <- call srv "POST" (path <> "/fork") (Just (obj []))
        forked @?= 201
        field "tenant" (field "params" fork1) @?= Aeson.String "acme-3"
        (forked2, fork2) <- call srv "POST" (path <> "/fork") (Just (obj ["params" .= obj ["tenant" .= ("fresh" :: Text)]]))
        forked2 @?= 201
        field "tenant" (field "params" fork2) @?= Aeson.String "fresh"
        (badFork, badForkErr) <- call srv "POST" (path <> "/fork") (Just (obj ["params" .= obj ["nope" .= ("x" :: Text)]]))
        (badFork, field "error" badForkErr) @?= (422, "unknown_params")
  where
    withTenantParam = "{\"parameters\": [{\"name\": \"tenant\", \"scope\": \"session\", \"required\": true}]}"

originTest :: Assertion
originTest = do
    withServer "{}" mockCompletion $ \srv -> do
        (remote, err) <- call srv{srvHeaders = [("Origin", "http://evil.example")]} "GET" "/v1/agents" Nothing
        (remote, field "error" err) @?= (403, "forbidden_origin")
        (local, _) <- call srv{srvHeaders = [("Origin", "http://localhost:3000")]} "GET" "/v1/agents" Nothing
        local @?= 200
        (ipv6, _) <- call srv{srvHeaders = [("Origin", "http://[::1]:3000")]} "GET" "/v1/agents" Nothing
        ipv6 @?= 200
    let tokens = authTokensFromList [("alice-token", "alice")]
    withServerAuth (Just tokens) "{}" mockCompletion $ \srv -> do
        (withToken, _) <- call srv{srvHeaders = [("Origin", "http://evil.example")], srvToken = Just "alice-token"} "GET" "/v1/agents" Nothing
        withToken @?= 200

{- | @--cors-origin@: a listed origin gets preflight and response headers and
passes 'checkOrigin' even without authentication; a non-listed, non-loopback
origin is still refused; the event stream carries the headers too.
-}
corsTest :: Assertion
corsTest = do
    let allowed = "http://allowed.example" :: ByteString.ByteString
    withServerConfig Nothing "{}" id (\env -> env{envCorsOrigins = ["http://allowed.example"]}) $ \srv -> do
        -- Preflight from the allowed origin: 204, the CORS headers, no auth needed.
        preflightReq <- request srv{srvHeaders = [("Origin", allowed)]} "OPTIONS" "/v1/sessions" Nothing
        preflightRsp <- Http.httpLbs preflightReq srv.srvManager
        statusCode (Http.responseStatus preflightRsp) @?= 204
        let preflightHeaders = Http.responseHeaders preflightRsp
        lookup "Access-Control-Allow-Origin" preflightHeaders @?= Just allowed
        lookup "Access-Control-Allow-Methods" preflightHeaders @?= Just "GET, POST, PUT, DELETE, OPTIONS"
        lookup "Access-Control-Allow-Headers" preflightHeaders @?= Just "Authorization, Content-Type, Last-Event-ID"
        lookup "Access-Control-Max-Age" preflightHeaders @?= Just "600"
        lookup "Vary" preflightHeaders @?= Just "Origin"
        -- A real POST from the allowed origin carries the header and succeeds
        -- (an allowed origin passes checkOrigin even without --auth-tokens).
        createReq <- request srv{srvHeaders = [("Origin", allowed)]} "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
        createRsp <- Http.httpLbs createReq srv.srvManager
        statusCode (Http.responseStatus createRsp) @?= 201
        lookup "Access-Control-Allow-Origin" (Http.responseHeaders createRsp) @?= Just allowed
        lookup "Access-Control-Expose-Headers" (Http.responseHeaders createRsp) @?= Just "Location"
        -- A non-listed, non-loopback origin is still refused without authentication.
        (refused, err) <- call srv{srvHeaders = [("Origin", "http://evil.example")]} "GET" "/v1/agents" Nothing
        (refused, field "error" err) @?= (403, "forbidden_origin")
        -- The event stream, opened cross-origin, carries the same header.
        view <- Aeson.decode (Http.responseBody createRsp) `orFail` "the create response was not JSON"
        let sid = textField "session_id" view
        eventsReq <- request srv{srvHeaders = [("Origin", allowed)]} "GET" ("/v1/sessions/" <> sid <> "/events") Nothing
        Http.withResponse eventsReq srv.srvManager $ \rsp -> do
            statusCode (Http.responseStatus rsp) @?= 200
            lookup "Access-Control-Allow-Origin" (Http.responseHeaders rsp) @?= Just allowed
            chunk <- Http.brRead (Http.responseBody rsp)
            assertBool "the stream sent a snapshot" ("event: snapshot" `ByteString.isInfixOf` chunk)
  where
    orFail (Just v) _ = pure v
    orFail Nothing msg = assertFailure msg

-- | The startup check runs before any file is touched, so bogus paths are fine.
corsWildcardStartupTest :: Assertion
corsWildcardStartupTest = withSystemTempDirectory "agents-server-cors" $ \dir -> do
    let tokensFile = dir </> "tokens.json"
    writeFile tokensFile "{\"tokens\": [{\"owner\": \"alice\", \"token\": \"alice-token\"}]}"
    let opts =
            ServerOptions
                { soAgentFiles = ["/nonexistent/agent.json"]
                , soApiKeysFile = "/nonexistent/keys.json"
                , soDatabase = dir </> "agents.db"
                , soBind = "127.0.0.1"
                , soPort = 0
                , soLiveSessionTtl = 900
                , soShutdownGrace = 1
                , soAuthTokens = Just tokensFile
                , soStreamTokens = False
                , soAdminOwners = []
                , soNoUI = True
                , soCorsOrigins = ["*"]
                , soSocket = Nothing
                , soLegacySessionDirs = []
                , soProcessParams = mempty
                }
    result <- try (runServer opts silentLogger)
    case result of
        Left (_ :: IOException) -> pure ()
        Right () -> assertFailure "expected --cors-origin '*' with --auth-tokens to be refused at startup"

streamingTest :: Assertion
streamingTest = do
    requests <- newIORef []
    testWithApplication (pure (fakeStreamingLlm requests)) $ \llmPort -> do
        let extra = "{\"modelUrl\": \"http://127.0.0.1:" <> show llmPort <> "/v1\"}"
        withServerConfig Nothing extra (\c -> c{hcCompletion = Nothing, hcStreamTokens = True}) id $ \srv -> do
            (_, view) <- call srv "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
            let sid = textField "session_id" view
            withEvents srv sid $ \next -> do
                _snapshot <- next
                _ <- call srv "POST" ("/v1/sessions/" <> sid <> "/resume") Nothing
                events <- untilStopped next
                [field "text" d | ("text.delta", d) <- events] @?= ["Hel", "lo world"]
                field "status" (snd (last events)) @?= "idle"
            (_, final) <- call srv "GET" ("/v1/sessions/" <> sid) Nothing
            let transcript = LByteString.toStrict (Aeson.encode (field "session" final))
            assertBool "the whole answer is stored" ("Hello world" `ByteString.isInfixOf` transcript)
    sent <- readIORef requests
    map (field "stream") sent @?= [Aeson.Bool True]
    map (field "include_usage" . field "stream_options") sent @?= [Aeson.Bool True]

{- | An OpenAI-compatible endpoint streaming "Hello world" in two pieces,
with a pause after each chunk. Records the request bodies.
-}
fakeStreamingLlm :: IORef [Aeson.Value] -> Wai.Application
fakeStreamingLlm requests req respond = do
    body <- Wai.strictRequestBody req
    mapM_ (\v -> modifyIORef' requests (<> [v])) (Aeson.decode body :: Maybe Aeson.Value)
    respond $ Wai.responseStream status200 [("Content-Type", "text/event-stream")] $ \write flush ->
        mapM_ (\frame -> write (Builder.lazyByteString frame) >> flush >> threadDelay 20_000) frames
  where
    frames = map (\v -> "data: " <> Aeson.encode v <> "\n\n") chunks <> ["data: [DONE]\n\n"]
    choice fields = Aeson.object ["choices" .= [Aeson.object (("index" .= (0 :: Int)) : fields)]]
    chunks =
        [ choice ["delta" .= Aeson.object ["role" .= ("assistant" :: Text), "content" .= ("" :: Text)]]
        , choice ["delta" .= Aeson.object ["content" .= ("Hel" :: Text)]]
        , choice ["delta" .= Aeson.object ["content" .= ("lo world" :: Text)]]
        , choice ["delta" .= Aeson.object [], "finish_reason" .= ("stop" :: Text)]
        , Aeson.object ["choices" .= ([] :: [Aeson.Value]), "usage" .= Aeson.object ["prompt_tokens" .= (3 :: Int), "completion_tokens" .= (2 :: Int), "total_tokens" .= (5 :: Int)]]
        ]

storedAgentsTest :: Assertion
storedAgentsTest = do
    let tokens = authTokensFromList [("alice-token", "alice"), ("bob-token", "bob")]
    withServerConfig (Just tokens) "{}" (\c -> c{hcCompletion = Just (const mockCompletion)}) (\e -> e{envAdmins = ["alice"]}) $ \anonymous -> do
        let alice = anonymous{srvToken = Just "alice-token"}
            bob = anonymous{srvToken = Just "bob-token"}
            helper extra = Just (storedConfig extra)
        (notAdmin, err) <- call bob "PUT" "/v1/agents/helper" (helper [])
        (notAdmin, field "error" err) @?= (403, "forbidden")
        (created, view) <- call alice "PUT" "/v1/agents/helper" (helper [])
        (created, field "source" view, field "updated_by" view) @?= (201, "database", "alice")
        (replaced, _) <- call alice "PUT" "/v1/agents/helper" (helper ["announce" .= ("a better helper" :: Text)])
        replaced @?= 200
        (_, listed) <- call bob "GET" "/v1/agents" Nothing
        case listed of
            Aeson.Array xs -> map (\a -> (field "slug" a, field "source" a)) (Vector.toList xs) @?= [("helper", "database"), ("server-test", "file")]
            other -> assertFailure ("expected a list, got " <> show other)
        (_, one) <- call bob "GET" "/v1/agents/helper" Nothing
        field "description" one @?= "a better helper"
        let refused extra path code = do
                (status, e) <- call alice "PUT" path (helper extra)
                (path, status, field "error" e) @?= (path, fst code, snd code)
        refused ["toolDirectory" .= ("tools" :: Text)] "/v1/agents/files" (400, "agent_uses_files")
        refused [] "/v1/agents/server-test" (409, "agent_defined_by_file")
        refused ["slug" .= ("other" :: Text)] "/v1/agents/helper" (400, "bad_request")
        (sessionStatus, session) <- call bob "POST" "/v1/sessions?wait=true" (Just (Aeson.object ["agent" .= ("helper" :: Text), "prompt" .= ("hi" :: Text)]))
        (sessionStatus, field "status" session, field "agent" session) @?= (201, "idle", "helper")
        (_, tools) <- call bob "POST" "/mcp" (Just (Aeson.object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= ("tools/list" :: Text)]))
        map (field "name") (arrayField "tools" (field "result" tools)) @?= ["ask_helper", "ask_server-test"]
        (deleted, _) <- call alice "DELETE" "/v1/agents/helper" Nothing
        deleted @?= 200
        (again, e1) <- call alice "DELETE" "/v1/agents/helper" Nothing
        (again, field "error" e1) @?= (404, "unknown_agent")
        (gone, e2) <- call bob "POST" "/v1/sessions" (Just (Aeson.object ["agent" .= ("helper" :: Text), "prompt" .= ("hi" :: Text)]))
        (gone, field "error" e2) @?= (404, "unknown_agent")

agentEditsDisabledTest :: Assertion
agentEditsDisabledTest = withServer "{}" mockCompletion $ \srv -> do
    (status, err) <- call srv "PUT" "/v1/agents/helper" (Just (storedConfig []))
    (status, field "error" err) @?= (403, "agent_edits_disabled")

-- | An agent configuration that needs no files, with extra fields.
storedConfig :: [Aeson.Pair] -> Aeson.Value
storedConfig extra =
    case (base, Aeson.object extra) of
        (Aeson.Object a, Aeson.Object b) -> Aeson.Object (KeyMap.union b a)
        (a, _) -> a
  where
    base =
        Aeson.object
            [ "apiKeyId" .= ("none" :: Text)
            , "flavor" .= ("OpenAIv1" :: Text)
            , "modelUrl" .= ("http://127.0.0.1:1" :: Text)
            , "modelName" .= ("mock" :: Text)
            , "announce" .= ("a stored agent" :: Text)
            , "systemPrompt" .= ["You help" :: Text]
            , "builtinToolboxes" .= ([] :: [Text])
            , "mcpServers" .= ([] :: [Text])
            ]

-------------------------------------------------------------------------------
-- Self-description
-------------------------------------------------------------------------------

{- | The document is the contract a client that has never heard of
agents-exe reads, so it must describe every route, need no token, and leave
no dangling reference.
-}
openApiTest :: Assertion
openApiTest = do
    let tokens = authTokensFromList [("alice-token", "alice")]
    withServerAuth (Just tokens) "{}" mockCompletion $ \anonymous -> do
        (status, doc) <- call anonymous "GET" "/openapi.json" Nothing
        status @?= 200
        -- Every path the router answers, and nothing else.
        let documented = case field "paths" doc of
                Aeson.Object o -> [Key.toText k | k <- KeyMap.keys o]
                _ -> []
        sortText documented
            @?= [ "/healthz"
                , "/mcp"
                , "/v1/agents"
                , "/v1/agents/{slug}"
                , "/v1/continuations/{token}"
                , "/v1/events"
                , "/v1/sessions"
                , "/v1/sessions/{id}"
                , "/v1/sessions/{id}/cancel"
                , "/v1/sessions/{id}/cancel-attached"
                , "/v1/sessions/{id}/events"
                , "/v1/sessions/{id}/fork"
                , "/v1/sessions/{id}/mail"
                , "/v1/sessions/{id}/messages"
                , "/v1/sessions/{id}/pause"
                , "/v1/sessions/{id}/pending"
                , "/v1/sessions/{id}/resume"
                ]
        let known = case field "schemas" (field "components" doc) of
                Aeson.Object o -> [Key.toText k | k <- KeyMap.keys o]
                _ -> []
        assertBool "schemas are published" (not (null known))
        assertBool "every $ref resolves" (all (`elem` known) (refNames doc))
        -- The orientation text names the flow a newcomer must follow.
        let blurb = textField "description" (field "info" doc)
        mapM_
            (\needle -> assertBool (Text.unpack ("the description mentions " <> needle)) (needle `Text.isInfixOf` blurb))
            ["/v1/agents", "/v1/sessions", "/v1/continuations/{token}", "deferred"]

-- | Every schema name a @$ref@ points at, anywhere in the document.
refNames :: Aeson.Value -> [Text]
refNames = \case
    Aeson.Object o ->
        case KeyMap.lookup "$ref" o of
            Just (Aeson.String r) -> [Text.takeWhileEnd (/= '/') r]
            _ -> concatMap refNames (KeyMap.elems o)
    Aeson.Array xs -> concatMap refNames (Vector.toList xs)
    _ -> []

sortText :: [Text] -> [Text]
sortText = List.sort

uiPageTest :: Assertion
uiPageTest = do
    -- Off by default in the fixture, as on a non-loopback bind.
    withServer "{}" mockCompletion $ \srv -> do
        (off, _) <- call srv "GET" "/" Nothing
        off @?= 404
    withServerConfig Nothing "{}" id (\env -> env{envUI = True}) $ \srv -> do
        (status, contentType, body) <- fetchWhole srv "GET" "/"
        status @?= 200
        contentType @?= Just "text/html; charset=utf-8"
        mapM_
            (\needle -> assertBool (show needle <> " is on the page") (needle `ByteString.isInfixOf` body))
            [ "<!DOCTYPE html>"
            , "/v1/sessions"
            , "/v1/continuations/"
            , "EventSource"
            , -- the session list follows the server-wide feed
              "/v1/events"
            , "session.created"
            , "openapi.json"
            , -- attachments: the picker, and the key the API wants
              "type='file'"
            , "mediaPayload"
            , -- the parameter form: rendered from the descriptor, sent as params
              "showParams"
            , "paramValues"
            ]

{- | @EventSource@ cannot set headers, so the event stream also takes the
token as a query parameter. No other endpoint does.
-}
accessTokenTest :: Assertion
accessTokenTest = do
    let tokens = authTokensFromList [("alice-token", "alice")]
    withServerAuth (Just tokens) "{}" mockCompletion $ \anonymous -> do
        let alice = anonymous{srvToken = Just "alice-token"}
        (_, view) <- call alice "POST" "/v1/sessions" (Just (createBody [("run", "none")]))
        let sid = textField "session_id" view
            query = "?access_token=alice-token"
        -- The stream opens and sends its snapshot.
        (streamStatus, body) <- fetchFirstChunk anonymous "GET" ("/v1/sessions/" <> sid <> "/events" <> query)
        streamStatus @?= 200
        assertBool "the stream sent a snapshot" ("event: snapshot" `ByteString.isInfixOf` body)
        -- The same parameter is refused everywhere else.
        (rejected, err) <- call anonymous "GET" ("/v1/sessions/" <> sid <> query) Nothing
        (rejected, field "error" err) @?= (401, "unauthorized")
        (listRejected, _) <- call anonymous "GET" ("/v1/sessions" <> query) Nothing
        listRejected @?= 401

-- | Status, content type, and the whole body.
fetchWhole :: Srv -> Method -> Text -> IO (Int, Maybe ByteString.ByteString, ByteString.ByteString)
fetchWhole srv method path = do
    req <- request srv method path Nothing
    rsp <- Http.httpLbs req srv.srvManager
    pure
        ( statusCode (Http.responseStatus rsp)
        , lookup "Content-Type" (Http.responseHeaders rsp)
        , LByteString.toStrict (Http.responseBody rsp)
        )

{- | Status and the first chunk only: an event stream never ends on its own,
so reading it whole would block.
-}
fetchFirstChunk :: Srv -> Method -> Text -> IO (Int, ByteString.ByteString)
fetchFirstChunk srv method path = do
    req <- request srv method path Nothing
    Http.withResponse req srv.srvManager $ \rsp -> do
        chunk <- Http.brRead (Http.responseBody rsp)
        pure (statusCode (Http.responseStatus rsp), chunk)

{- | A message with media is stored as @{text, media}@ rather than a bare
string, and the attachment comes back under different keys from the ones it
was sent with. The chat page reads both, so both are pinned here.
-}
mediaRoundTripTest :: Assertion
mediaRoundTripTest = withServer "{}" mockCompletion $ \srv -> do
    let body =
            Aeson.object
                [ "agent" .= ("server-test" :: Text)
                , "prompt" .= ("what is in this file?" :: Text)
                , "media"
                    .= [ Aeson.object
                            [ "mime" .= ("text/plain" :: Text)
                            , "base64" .= ("aGVsbG8=" :: Text)
                            , "filename" .= ("note.txt" :: Text)
                            ]
                       ]
                ]
    (status, view) <- call srv "POST" "/v1/sessions?wait=true" (Just body)
    status @?= 201
    let userTurns =
            [ field "userQuery" (field "contents" t)
            | t <- arrayField "turns" (field "session" view)
            , textField "tag" t `elem` ["UserTurn", "PartialUserTurn"]
            ]
    case userTurns of
        (query : _) -> do
            -- A bare string once it has media would lose the attachment.
            field "text" query @?= "what is in this file?"
            case arrayField "media" query of
                [attachment] ->
                    ( field "mimeType" attachment
                    , field "base64Data" attachment
                    , field "filename" attachment
                    )
                        @?= ("text/plain", "aGVsbG8=", "note.txt")
                other -> assertFailure ("expected one attachment, got " <> show (length other))
        [] -> assertFailure "no user turn was stored"

-------------------------------------------------------------------------------
-- Server fixture
-------------------------------------------------------------------------------

data Srv = Srv
    { srvPort :: Int
    , srvEnv :: ServerEnv
    , srvManager :: Http.Manager
    , srvToken :: Maybe ByteString.ByteString
    -- ^ Sent as a bearer token.
    , srvHeaders :: [Header]
    }

{- | Run the application on a free port over a fresh database, with one
agent (slug @server-test@, extra config fields merged in) and a mock LLM.
-}
withServer :: String -> Completion -> (Srv -> IO a) -> IO a
withServer = withServerAuth Nothing

withServerAuth :: Maybe AuthTokens -> String -> Completion -> (Srv -> IO a) -> IO a
withServerAuth auth extraConfig complete = withServerConfig auth extraConfig (\c -> c{hcCompletion = Just (const complete)}) id

-- | The most general fixture: the caller adjusts the host configuration and the server environment.
withServerConfig :: Maybe AuthTokens -> String -> (HostConfig -> HostConfig) -> (ServerEnv -> ServerEnv) -> (Srv -> IO a) -> IO a
withServerConfig auth extraConfig adjust adjustEnv k =
    withSystemTempDirectory "agents-server" $ \dir -> do
        let agentFile = dir </> "agent.json"
            keysFile = dir </> "keys.json"
        extra <- either fail pure (Aeson.eitherDecodeStrict (Char8.pack extraConfig))
        LByteString.writeFile agentFile $
            Aeson.encode $
                Aeson.object ["tag" .= ("OpenAIAgentDescription" :: Text), "contents" .= merge baseConfig extra]
        writeFile keysFile "{}"
        let cfg = adjust (defaultHostConfig [agentFile] keysFile (dir </> "agents.db"))
        manager <- Http.newManager Http.defaultManagerSettings{Http.managerResponseTimeout = Http.responseTimeoutMicro 30_000_000}
        withHost cfg silent $ \host ->
            withSessionRunner host $ \runner -> do
                env0 <- newServerEnv host runner auth
                let env = adjustEnv env0{envKeepAlive = 300_000}
                testWithApplication (pure (application env)) $ \p -> k (Srv p env manager Nothing [])
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
            , Http.requestHeaders =
                [("Content-Type", "application/json") | Just _ <- [body]]
                    <> [("Authorization", "Bearer " <> t) | Just t <- [srv.srvToken]]
                    <> srv.srvHeaders
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
withRawEvents srv sid = withRawEventsAt srv ("/v1/sessions/" <> sid <> "/events")

-- | Like 'withRawEvents', at any path (e.g. @\/v1\/events?scope=all@).
withRawEventsAt :: Srv -> Text -> (IO ByteString.ByteString -> IO a) -> IO a
withRawEventsAt srv path k = do
    req <- request srv "GET" path Nothing
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
withEvents srv sid k = withRawEvents srv sid (k . parsedNext)

-- | Like 'withEvents', at any path.
withEventsAt :: Srv -> Text -> (IO (Text, Aeson.Value) -> IO a) -> IO a
withEventsAt srv path k = withRawEventsAt srv path (k . parsedNext)

-- | Like 'withEvents', also giving each event's @id:@ (its 'EventSeq').
withIdEvents :: Srv -> Text -> (IO (Text, Text, Aeson.Value) -> IO a) -> IO a
withIdEvents srv path k = withRawEventsAt srv path (k . parsedNextWithId)

parsedNext :: IO ByteString.ByteString -> IO (Text, Aeson.Value)
parsedNext nextFrame = do
    (kind, _, value) <- parsedNextWithId nextFrame
    pure (kind, value)

parsedNextWithId :: IO ByteString.ByteString -> IO (Text, Text, Aeson.Value)
parsedNextWithId nextFrame = next
  where
    next =
        timeout 5_000_000 nextFrame >>= \case
            Nothing -> assertFailure "no event within 5 seconds"
            Just frame
                | ":" `ByteString.isPrefixOf` frame -> next
                | otherwise -> parseFrame frame
    parseFrame frame = do
        let lines' = Char8.lines frame
            value prefix = [ByteString.drop (ByteString.length prefix) l | l <- lines', prefix `ByteString.isPrefixOf` l]
            eid = case value "id: " of
                (i : _) -> Text.decodeUtf8 i
                [] -> "" -- e.g. the "snapshot" frame, which carries no id.
        case (value "event: ", value "data: ") of
            ([kind], [payload]) ->
                either
                    (\e -> assertFailure ("bad event data: " <> e))
                    (pure . (Text.decodeUtf8 kind,eid,))
                    (Aeson.eitherDecodeStrict payload)
            _ -> assertFailure ("bad frame: " <> show frame)

-- | Events up to and including the next @run.stopped@.
untilStopped :: IO (Text, Aeson.Value) -> IO [(Text, Aeson.Value)]
untilStopped next = go []
  where
    go acc = do
        event <- next
        let acc' = event : acc
        if fst event == "run.stopped" then pure (reverse acc') else go acc'

-- | Like 'untilStopped', keeping each event's @id:@.
untilStoppedWithId :: IO (Text, Text, Aeson.Value) -> IO [(Text, Text, Aeson.Value)]
untilStoppedWithId next = go []
  where
    go acc = do
        event@(kind, _, _) <- next
        let acc' = event : acc
        if kind == "run.stopped" then pure (reverse acc') else go acc'

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

-------------------------------------------------------------------------------
-- --socket
-------------------------------------------------------------------------------

{- | @runServer@ with @--socket@ set, on a real (if throwaway) TCP port
alongside it. Confirms the socket file appears, is reachable with a plain
HTTP/1.1 request, and is gone once the server stops.
-}
socketHealthzTest :: Assertion
socketHealthzTest = withSystemTempDirectory "agents-server-socket" $ \dir -> do
    let agentFile = dir </> "agent.json"
        keysFile = dir </> "keys.json"
        sockPath = dir </> "agents-server.sock"
    LByteString.writeFile agentFile $
        Aeson.encode $
            Aeson.object
                [ "tag" .= ("OpenAIAgentDescription" :: Text)
                , "contents"
                    .= Aeson.object
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
                ]
    writeFile keysFile "{}"
    port <- getFreePort
    let opts =
            ServerOptions
                { soAgentFiles = [agentFile]
                , soApiKeysFile = keysFile
                , soDatabase = dir </> "agents.db"
                , soBind = "127.0.0.1"
                , soPort = port
                , soLiveSessionTtl = 900
                , soShutdownGrace = 1
                , soAuthTokens = Nothing
                , soStreamTokens = False
                , soAdminOwners = []
                , soNoUI = True
                , soCorsOrigins = []
                , soSocket = Just sockPath
                , soLegacySessionDirs = []
                , soProcessParams = mempty
                }
    serverAsync <- async (runServer opts silentLogger)
    waitForFile sockPath
    response <- httpOverUnixSocket sockPath "GET /healthz HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
    Async.cancel serverAsync
    assertBool ("expected 200 OK, got: " <> Char8.unpack response) ("200 OK" `ByteString.isInfixOf` response)
    assertBool ("expected the healthz body, got: " <> Char8.unpack response) ("\"ok\":true" `ByteString.isInfixOf` response)
    waitForFileGone sockPath

-- | A free TCP port on loopback, picked by asking the kernel for one (a
-- small, accepted race: nothing stops another process taking it before
-- 'runServer' binds).
getFreePort :: IO Int
getFreePort = do
    sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    NS.bind sock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
    addr <- NS.getSocketName sock
    NS.close sock
    case addr of
        NS.SockAddrInet p _ -> pure (fromIntegral p)
        other -> assertFailure ("expected an IPv4 address, got: " <> show other) >> pure 0

waitForFile :: FilePath -> IO ()
waitForFile path = do
    got <- timeout 5_000_000 poll
    case got of
        Just () -> pure ()
        Nothing -> assertFailure ("expected " <> path <> " to appear in time")
  where
    poll = do
        exists <- doesFileExist path
        if exists then pure () else threadDelay 20_000 >> poll

waitForFileGone :: FilePath -> IO ()
waitForFileGone path = do
    got <- timeout 5_000_000 poll
    case got of
        Just () -> pure ()
        Nothing -> assertFailure ("expected " <> path <> " to be removed on shutdown")
  where
    poll = do
        exists <- doesFileExist path
        if exists then threadDelay 20_000 >> poll else pure ()

-- | A minimal HTTP/1.1 request over a Unix domain socket: sends the raw
-- request bytes, then reads until the peer closes the connection (the
-- request above sends @Connection: close@).
httpOverUnixSocket :: FilePath -> ByteString.ByteString -> IO ByteString.ByteString
httpOverUnixSocket path rawRequest = do
    sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
    NS.connect sock (NS.SockAddrUnix path)
    NSB.sendAll sock rawRequest
    chunks <- readAll sock
    NS.close sock
    pure (ByteString.concat chunks)
  where
    readAll sock = do
        chunk <- NSB.recv sock 4096
        if ByteString.null chunk
            then pure []
            else (chunk :) <$> readAll sock
