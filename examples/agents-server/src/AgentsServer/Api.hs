{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The HTTP API of @agents-server@: JSON over a 'SessionRunner', and a
server-sent events stream per session.

Every endpoint that can start a run takes @wait@ and @timeout@ query
parameters; see @docs/agents-server.md@ for the full reference.
-}
module AgentsServer.Api (
    ServerEnv (..),
    newServerEnv,
    AuthTokens,
    requestShutdown,
    application,
) where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (forM, unless, when)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (Builder, byteString, lazyByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (asum)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.UUID as UUID
import Network.HTTP.Types
import Network.Wai
import Text.Read (readMaybe)

import AgentsServer.Auth (AuthTokens, authenticate, bearerToken)
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.Base as Base
import System.Agents.Host (Host (..))
import System.Agents.Host.Runner
import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.Session.Base (ContinuationToken (..), Session, SessionId (..), SessionStatus (..), UserToolResponse (..), parseSessionStatus, pendingDeferredCalls, sessionStatusText)
import System.Agents.Session.Wake (findSessionForToken)
import System.Agents.SessionStore (SessionBackend (..), SessionMeta (..), SessionQuery (..), allSessionsQuery)
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ToolDescription (..), ToolName (..))

data ServerEnv = ServerEnv
    { envHost :: Host
    , envRunner :: SessionRunner
    , envShutdown :: TVar Bool
    -- ^ Set on shutdown: event streams end, waiting requests answer at once.
    , envKeepAlive :: Int
    -- ^ Microseconds of silence before an event stream sends a keepalive.
    , envAuth :: Maybe AuthTokens
    {- ^ With tokens, every endpoint but @/healthz@ needs a bearer token, and
    callers only see their own sessions. Without, everything is open.
    -}
    }

newServerEnv :: Host -> SessionRunner -> Maybe AuthTokens -> IO ServerEnv
newServerEnv host runner auth = do
    shutdown <- newTVarIO False
    pure $ ServerEnv host runner shutdown 15_000_000 auth

-- | Who is calling: an owner when authentication is on, 'Nothing' when off.
newtype Caller = Caller (Maybe Text)

-- | End event streams and release waiting requests; runs carry on.
requestShutdown :: ServerEnv -> IO ()
requestShutdown env = atomically $ writeTVar env.envShutdown True

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

-- | An error answer: status, code, and message.
data ApiError = ApiError Status Text Text
    deriving (Show)

instance Exception ApiError

errorResponse :: ApiError -> Response
errorResponse (ApiError status code msg) =
    responseLBS status headers (Aeson.encode (Aeson.object ["error" .= code, "message" .= msg]))
  where
    headers =
        (hContentType, jsonType)
            : [("WWW-Authenticate", "Bearer") | status == status401]

fromRunnerError :: RunnerError -> ApiError
fromRunnerError = \case
    UnknownAgent slug -> ApiError status404 "unknown_agent" ("no agent named " <> slug)
    UnknownSession sid -> ApiError status404 "unknown_session" ("no session " <> showId sid)
    UnknownToken _ -> ApiError status404 "unknown_token" "no pending call has this continuation token"
    TokenAlreadyCompleted _ -> ApiError status409 "token_already_completed" "this call already has a result"
    RunInProgress sid -> ApiError status409 "run_in_progress" ("a run is active on session " <> showId sid)
    NoActiveRun sid -> ApiError status409 "no_active_run" ("no run is active on session " <> showId sid)
    NotAcceptingMessages _ status ->
        ApiError status409 "not_accepting_messages" ("the session is " <> sessionStatusText status <> ", not idle")
    Conflict _ -> ApiError status409 "conflict" "the session was modified by another writer; retry"

orThrow :: IO (Either RunnerError a) -> IO a
orThrow action = action >>= either (throwIO . fromRunnerError) pure

badRequest :: Text -> IO a
badRequest = throwIO . ApiError status400 "bad_request"

-------------------------------------------------------------------------------
-- Routing
-------------------------------------------------------------------------------

application :: ServerEnv -> Application
application env req respond = do
    result <- try (route env req)
    case result of
        Right rsp -> respond rsp
        Left (e :: SomeException)
            | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
            | Just apiError <- fromException e -> respond (errorResponse apiError)
            | otherwise -> respond $ errorResponse $ ApiError status500 "internal_error" (Text.pack (displayException e))

route :: ServerEnv -> Request -> IO Response
route env req = case (requestMethod req, path) of
    ("GET", ["healthz"]) -> healthz env
    _ -> do
        caller <- authenticateRequest env req
        routeAuthenticated env req caller path
  where
    path = filter (not . Text.null) (pathInfo req)

routeAuthenticated :: ServerEnv -> Request -> Caller -> [Text] -> IO Response
routeAuthenticated env req caller path = case (requestMethod req, path) of
    ("GET", ["v1", "agents"]) -> listAgents env
    ("POST", ["v1", "sessions"]) -> createH env req caller
    ("GET", ["v1", "sessions"]) -> listSessions env req caller
    ("GET", ["v1", "sessions", sid]) -> withSession sid (getH env)
    ("DELETE", ["v1", "sessions", sid]) -> withSession sid (deleteH env req)
    ("POST", ["v1", "sessions", sid, "messages"]) -> withSession sid (messagesH env req)
    ("POST", ["v1", "sessions", sid, "resume"]) -> withSession sid (resumeH env req)
    ("POST", ["v1", "sessions", sid, "cancel"]) -> withSession sid (cancelH env)
    ("GET", ["v1", "sessions", sid, "pending"]) -> withSession sid (pendingH env)
    ("GET", ["v1", "sessions", sid, "events"]) -> withSession sid (eventsH env)
    ("POST", ["v1", "continuations", token]) -> continuationH env req caller token
    _
        | knownPath path -> throwIO $ ApiError status405 "method_not_allowed" "method not allowed on this path"
        | otherwise -> throwIO $ ApiError status404 "not_found" "no such endpoint"
  where
    withSession :: Text -> (SessionId -> IO Response) -> IO Response
    withSession txt k = case UUID.fromText txt of
        Just uuid -> authorize env caller (SessionId uuid) >> k (SessionId uuid)
        Nothing -> throwIO $ ApiError status404 "unknown_session" ("not a session id: " <> txt)

    knownPath = \case
        ["healthz"] -> True
        ["v1", "agents"] -> True
        ["v1", "sessions"] -> True
        ["v1", "sessions", _] -> True
        ["v1", "sessions", _, action] -> action `elem` ["messages", "resume", "cancel", "pending", "events"]
        ["v1", "continuations", _] -> True
        _ -> False

-------------------------------------------------------------------------------
-- Authentication
-------------------------------------------------------------------------------

authenticateRequest :: ServerEnv -> Request -> IO Caller
authenticateRequest env req = case env.envAuth of
    Nothing -> pure (Caller Nothing)
    Just tokens ->
        case lookup hAuthorization (requestHeaders req) >>= bearerToken >>= authenticate tokens of
            Just owner -> pure (Caller (Just owner))
            Nothing -> throwIO $ ApiError status401 "unauthorized" "a valid bearer token is required"

{- | Refuse access to another owner's session. It answers as if the session
did not exist, so callers cannot probe for other owners' sessions.
-}
authorize :: ServerEnv -> Caller -> SessionId -> IO ()
authorize _ (Caller Nothing) _ = pure ()
authorize env (Caller (Just owner)) sid =
    sessionOwner env.envRunner sid >>= \case
        Just (Just o) | o == owner -> pure ()
        _ -> throwIO $ fromRunnerError (UnknownSession sid)

-------------------------------------------------------------------------------
-- Handlers
-------------------------------------------------------------------------------

healthz :: ServerEnv -> IO Response
healthz env = do
    stats <- runnerStats env.envRunner
    pure $ json status200 $ Aeson.object ["ok" .= True, "live_sessions" .= stats.rsLiveSessions, "active_runs" .= stats.rsActiveRuns]

listAgents :: ServerEnv -> IO Response
listAgents env = do
    agents <- forM (Map.toList env.envHost.hostAgents) $ \(slug, node) -> do
        tools <- readTVarIO node.osNodeTools
        pure $
            Aeson.object
                [ "slug" .= slug
                , "description" .= Base.announce node.osNodeConfig
                , "tools" .= [t.declareTool.toolDescriptionName.getToolName | t <- tools]
                ]
    pure $ json status200 agents

createH :: ServerEnv -> Request -> Caller -> IO Response
createH env req (Caller owner) = do
    w <- waitParams req
    body <- jsonBody req
    (agent, msg, mode) <- parseBody body $ \o -> do
        agent <- o .: "agent"
        msg <- messageFields o
        mode <- runField o
        pure (agent, msg, mode)
    meta <- orThrow $ createSessionAs env.envRunner owner agent msg mode
    let sid = meta.smSessionId
    view <- afterRun env w sid
    pure $
        responseLBS
            status201
            [(hContentType, jsonType), ("Location", "/v1/sessions/" <> Text.encodeUtf8 (showId sid))]
            (Aeson.encode view)

listSessions :: ServerEnv -> Request -> Caller -> IO Response
listSessions env req caller@(Caller owner) = do
    let params = queryParams req
    statuses <- traverse (mapM parseStatus . Text.splitOn ",") (param "status" params)
    parent <- traverse (maybe (badRequest "parent must be a session id") (pure . SessionId) . UUID.fromText) (param "parent" params)
    limit <- maybe (pure 50) (parseNumber "limit") (param "limit" params)
    unless (limit >= 1 && limit <= 500) $ badRequest "limit must be between 1 and 500"
    before <- traverse parseTime (param "before" params)
    -- Sub-sessions record no owner: list them through their parent.
    mapM_ (authorize env caller) parent
    let query =
            allSessionsQuery
                { sqAgent = param "agent" params
                , sqStatuses = statuses
                , sqParent = parent
                , sqOwner = maybe owner (const Nothing) parent
                , sqUpdatedBefore = before
                , sqLimit = Just limit
                }
    metas <- env.envHost.hostBackend.sbQuery query
    let nextBefore
            | length metas == limit, (m : _) <- reverse metas = Just m.smUpdatedAt
            | otherwise = Nothing
    pure $ json status200 $ Aeson.object ["sessions" .= metas, "next_before" .= nextBefore]
  where
    parseStatus s = maybe (badRequest ("unknown status: " <> s)) pure (parseSessionStatus s)
    parseTime :: Text -> IO UTCTime
    parseTime t = maybe (badRequest "before must be an ISO 8601 time") pure (iso8601ParseM (Text.unpack t))

getH :: ServerEnv -> SessionId -> IO Response
getH env sid = json status200 <$> loadView env sid

deleteH :: ServerEnv -> Request -> SessionId -> IO Response
deleteH env req sid = do
    dryRun <- boolParam "dry_run" False req
    plan <- orThrow $ deleteSession env.envRunner sid (if dryRun then DryRun else DeleteForReal)
    pure $
        json status200 $
            Aeson.object ["sessions" .= plan.dpSessions, "continuations" .= plan.dpContinuations, "dry_run" .= plan.dpDryRun]

messagesH :: ServerEnv -> Request -> SessionId -> IO Response
messagesH env req sid = do
    w <- waitParams req
    body <- jsonBody req
    (msg, mode) <- parseBody body $ \o -> (,) <$> messageFields o <*> runField o
    _ <- orThrow $ postMessage env.envRunner sid msg mode
    runResponse <$> afterRun env w sid

resumeH :: ServerEnv -> Request -> SessionId -> IO Response
resumeH env req sid = do
    w <- waitParams req
    body <- jsonBodyOrEmpty req
    mode <- parseBody body $ \o ->
        o .:? "mode" >>= \case
            Nothing -> pure UntilBlocked
            Just m -> maybe (fail "mode must be \"step\" or \"until_blocked\"") pure (runModeFromText m)
    _ <- orThrow $ resume env.envRunner sid mode
    runResponse <$> afterRun env w sid

cancelH :: ServerEnv -> SessionId -> IO Response
cancelH env sid = json status200 <$> orThrow (cancelRun env.envRunner sid)

pendingH :: ServerEnv -> SessionId -> IO Response
pendingH env sid = do
    (sess, _) <- loadSession env sid
    pure $ json status200 $ Aeson.object ["calls" .= pendingDeferredCalls sess]

continuationH :: ServerEnv -> Request -> Caller -> Text -> IO Response
continuationH env req caller tokenText = do
    token <- case UUID.fromText tokenText of
        Just uuid -> pure (ContinuationToken uuid)
        Nothing -> throwIO $ ApiError status404 "unknown_token" "not a continuation token"
    case caller of
        Caller Nothing -> pure ()
        Caller (Just _) -> do
            let host = env.envHost
            findSessionForToken (Just host.hostContinuations) host.hostBackend token >>= \case
                Nothing -> throwIO $ fromRunnerError (UnknownToken token)
                Just sid -> do
                    owned <- try (authorize env caller sid)
                    either (\(_ :: ApiError) -> throwIO (fromRunnerError (UnknownToken token))) pure owned
    w <- waitParams req
    body <- jsonBody req
    (result, autoResume) <- parseBody body $ \o -> do
        result <-
            o .: "result" >>= \case
                Aeson.String txt -> pure (TextResponse txt)
                other -> Aeson.parseJSON other
        autoResume <- fromMaybe True <$> o .:? "resume"
        pure (result, autoResume)
    meta <- orThrow $ completeCall env.envRunner token result autoResume
    runResponse <$> afterRun env w meta.smSessionId

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

data Wake = ShuttingDown | Delivered (Maybe SessionEvent) | KeepAlive

eventsH :: ServerEnv -> SessionId -> IO Response
eventsH env sid = do
    -- Subscribe before the snapshot, so no event falls between the two.
    next <- subscribeSTM env.envRunner sid
    (_, meta) <- loadSession env sid
    pure $ responseStream status200 headers $ \write flush -> do
        let send frame = write frame >> flush
            loop = do
                timer <- registerDelay env.envKeepAlive
                let wake =
                        asum
                            [ ShuttingDown <$ (readTVar env.envShutdown >>= check)
                            , Delivered <$> next
                            , KeepAlive <$ (readTVar timer >>= check)
                            ]
                    -- Skip other sessions' events without resetting the timer.
                    await =
                        atomically wake >>= \case
                            Delivered Nothing -> await
                            other -> pure other
                await >>= \case
                    ShuttingDown -> pure ()
                    KeepAlive -> send ": keepalive\n\n" >> loop
                    Delivered (Just event) -> send (eventFrame event) >> loop
                    Delivered Nothing -> loop
        send (sseFrame "snapshot" (Aeson.toJSON meta))
        loop
  where
    headers =
        [ (hContentType, "text/event-stream")
        , (hCacheControl, "no-cache")
        , ("X-Accel-Buffering", "no")
        ]

eventFrame :: SessionEvent -> Builder
eventFrame event = sseFrame (sessionEventKind event) $ case event of
    RunStarted sid mode -> Aeson.object ["session_id" .= sid, "mode" .= runModeText mode]
    SessionUpdated _ meta turn -> withFields (Aeson.toJSON meta) ["head_turn" .= turn]
    CallsDeferred sid calls -> Aeson.object ["session_id" .= sid, "calls" .= calls]
    RunStopped sid status -> Aeson.object ["session_id" .= sid, "status" .= status]
    SessionFailed sid msg -> Aeson.object ["session_id" .= sid, "message" .= msg]

-- | One event; JSON encoding has no raw newlines, so @data@ is one line.
sseFrame :: Text -> Aeson.Value -> Builder
sseFrame kind value =
    "event: " <> byteString (Text.encodeUtf8 kind) <> "\ndata: " <> lazyByteString (Aeson.encode value) <> "\n\n"

-------------------------------------------------------------------------------
-- Waiting and views
-------------------------------------------------------------------------------

data WaitParams = WaitParams
    { wpWait :: Bool
    , wpTimeout :: NominalDiffTime
    }

waitParams :: Request -> IO WaitParams
waitParams req = do
    wait <- boolParam "wait" False req
    seconds <- maybe (pure 120) (parseNumber "timeout") (param "timeout" (queryParams req))
    when (seconds < (0 :: Double)) $ badRequest "timeout must not be negative"
    pure $ WaitParams wait (realToFrac (min 600 seconds))

-- | Wait for the run if asked (or until shutdown), then load the session.
afterRun :: ServerEnv -> WaitParams -> SessionId -> IO Aeson.Value
afterRun env w sid = do
    when w.wpWait $
        race_
            (atomically (readTVar env.envShutdown >>= check))
            (awaitRun env.envRunner sid w.wpTimeout)
    loadView env sid

-- | @202@ while a run is going, @200@ once it stopped (or none started).
runResponse :: Aeson.Value -> Response
runResponse view = json status view
  where
    status = case view of
        Aeson.Object o | KeyMap.lookup "status" o == Just (Aeson.toJSON StatusRunning) -> status202
        _ -> status200

loadSession :: ServerEnv -> SessionId -> IO (Session, SessionMeta)
loadSession env sid =
    getSession env.envRunner sid >>= maybe (throwIO (fromRunnerError (UnknownSession sid))) pure

-- | The session's metadata, its turns, and its pending deferred calls.
loadView :: ServerEnv -> SessionId -> IO Aeson.Value
loadView env sid = do
    (sess, meta) <- loadSession env sid
    pure $ withFields (Aeson.toJSON meta) ["session" .= sess, "pending" .= pendingDeferredCalls sess]

withFields :: Aeson.Value -> [Aeson.Pair] -> Aeson.Value
withFields (Aeson.Object o) pairs = Aeson.Object (o <> KeyMap.fromList pairs)
withFields other _ = other

-------------------------------------------------------------------------------
-- Bodies and parameters
-------------------------------------------------------------------------------

-- | Largest accepted body: prompts may carry base64 media.
maxBodyBytes :: Int
maxBodyBytes = 32 * 1024 * 1024

readBody :: Request -> IO LByteString.ByteString
readBody req = go 0 []
  where
    go size acc = do
        chunk <- getRequestBodyChunk req
        if ByteString.null chunk
            then pure (LByteString.fromChunks (reverse acc))
            else do
                let size' = size + ByteString.length chunk
                when (size' > maxBodyBytes) $
                    throwIO $
                        ApiError status413 "payload_too_large" "the request body is larger than 32 MiB"
                go size' (chunk : acc)

jsonBody :: Request -> IO Aeson.Object
jsonBody req = do
    raw <- readBody req
    case Aeson.eitherDecode raw of
        Right (Aeson.Object o) -> pure o
        Right _ -> badRequest "the body must be a JSON object"
        Left err -> badRequest ("invalid JSON body: " <> Text.pack err)

-- | Like 'jsonBody', with an empty body read as @{}@.
jsonBodyOrEmpty :: Request -> IO Aeson.Object
jsonBodyOrEmpty req = do
    raw <- readBody req
    if LByteString.null raw
        then pure mempty
        else case Aeson.eitherDecode raw of
            Right (Aeson.Object o) -> pure o
            Right _ -> badRequest "the body must be a JSON object"
            Left err -> badRequest ("invalid JSON body: " <> Text.pack err)

parseBody :: Aeson.Object -> (Aeson.Object -> Aeson.Parser a) -> IO a
parseBody o parser = either (badRequest . Text.pack) pure (Aeson.parseEither parser o)

-- | @prompt@ and optional @media: [{mime, base64, filename?}]@.
messageFields :: Aeson.Object -> Aeson.Parser NewMessage
messageFields o = do
    prompt <- o .: "prompt"
    media <- fromMaybe [] <$> o .:? "media"
    NewMessage prompt <$> mapM mediaItem media
  where
    mediaItem = Aeson.withObject "media" $ \m ->
        MediaAttachment <$> m .: "mime" <*> m .: "base64" <*> m .:? "filename"

-- | @run@: @none@, @step@, or @until_blocked@ (the default).
runField :: Aeson.Object -> Aeson.Parser (Maybe RunMode)
runField o =
    o .:? "run" >>= \case
        Nothing -> pure (Just UntilBlocked)
        Just ("none" :: Text) -> pure Nothing
        Just other -> maybe (fail "run must be \"none\", \"step\", or \"until_blocked\"") (pure . Just) (runModeFromText other)

runModeFromText :: Text -> Maybe RunMode
runModeFromText = \case
    "step" -> Just StepOnce
    "until_blocked" -> Just UntilBlocked
    _ -> Nothing

runModeText :: RunMode -> Text
runModeText = \case
    StepOnce -> "step"
    UntilBlocked -> "until_blocked"

param :: Text -> QueryText -> Maybe Text
param name params = case lookup name params of
    Just (Just v) | not (Text.null v) -> Just v
    _ -> Nothing

-- | @?name@ and @?name=true@ mean true; absent means the default.
boolParam :: Text -> Bool -> Request -> IO Bool
boolParam name def req = case lookup name (queryParams req) of
    Nothing -> pure def
    Just Nothing -> pure True
    Just (Just v)
        | v `elem` ["true", "1", ""] -> pure True
        | v `elem` ["false", "0"] -> pure False
        | otherwise -> badRequest (name <> " must be true or false")

parseNumber :: (Read a) => Text -> Text -> IO a
parseNumber name v = maybe (badRequest (name <> " must be a number")) pure (readMaybe (Text.unpack v))

queryParams :: Request -> QueryText
queryParams = queryToQueryText . queryString

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

json :: (Aeson.ToJSON a) => Status -> a -> Response
json status value = responseLBS status [(hContentType, jsonType)] (Aeson.encode value)

jsonType :: ByteString.ByteString
jsonType = "application/json"

showId :: SessionId -> Text
showId (SessionId uuid) = UUID.toText uuid
