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
import Control.Monad (unless, when)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (Builder, byteString, lazyByteString)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.CaseInsensitive as CI
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
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
import AgentsServer.Mcp (McpContext (..), handleMcp)
import AgentsServer.OpenApi (apiDocument)
import AgentsServer.UI (uiPage)
import System.Agents.AgentStore (StoredAgent (..))
import System.Agents.AgentTree (OSAgentNode (..))
import qualified System.Agents.Base as Base
import System.Agents.Host (AgentEditError (..), AgentSource (..), Host (..), deleteStoredAgent, hostAllAgents, putStoredAgent)
import System.Agents.Host.Runner
import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.Session.Base (ContinuationToken (..), Session, SessionId (..), SessionStatus (..), UserToolResponse (..), parseSessionStatus, pendingDeferredCalls, sessionStatusText)
import System.Agents.Session.Wake (findSessionForToken)
import System.Agents.SessionStore (SessionBackend (..), SessionMeta (..), SessionQuery (..), allSessionsQuery)
import System.Agents.ToolRegistration (ToolRegistration (..))
import System.Agents.ToolSchema (ToolDescription (..), ToolName (..))
import System.Agents.Tools.Params.Types (ParamName, ParamScope (..), ParameterDecl (..), ProcessValue (..))

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
    , envAdmins :: [Text]
    {- ^ Owners allowed to store and delete agents. Empty: nobody can, which
    is the default, as an agent definition can start MCP servers (commands
    run on this machine).
    -}
    , envUI :: Bool
    -- ^ Serve the chat page at @/@; see "AgentsServer.UI".
    , envDocument :: Aeson.Value
    {- ^ The OpenAPI document, built once: it is the same for every
    request, and deriving it again per request would be wasted work.
    -}
    , envCorsOrigins :: [Text]
    {- ^ Origins allowed to call this server cross-origin (@--cors-origin@,
    repeatable). @"*"@ matches any origin and is only ever set when
    'envAuth' is 'Nothing' (checked at startup in "AgentsServer.Server").
    An origin here passes 'checkOrigin' even without authentication, and
    every response to a matching request carries @Access-Control-*@
    headers; see 'corsHeadersFor'.
    -}
    }

newServerEnv :: Host -> SessionRunner -> Maybe AuthTokens -> IO ServerEnv
newServerEnv host runner auth = do
    shutdown <- newTVarIO False
    pure $ ServerEnv host runner shutdown 15_000_000 auth [] False (Aeson.toJSON (apiDocument Nothing)) []

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
    UnknownParams names -> ApiError status422 "unknown_params" ("unknown parameter(s): " <> Text.intercalate ", " names)
    ForbiddenParams names ->
        ApiError status403 "forbidden_params" ("process-scope or pinned parameter(s) cannot be set here: " <> Text.intercalate ", " names)
    InvalidParams names -> ApiError status422 "invalid_params" ("secret parameter(s) must be given as strings: " <> Text.intercalate ", " names)
    MissingRequiredParams names -> ApiError status422 "params_required" ("required parameter(s) not bound: " <> Text.intercalate ", " names)
    MailboxRejected sid -> ApiError status429 "mailbox_full" ("session " <> showId sid <> " has too much unread mail; try again later")

orThrow :: IO (Either RunnerError a) -> IO a
orThrow action = action >>= either (throwIO . fromRunnerError) pure

badRequest :: Text -> IO a
badRequest = throwIO . ApiError status400 "bad_request"

-------------------------------------------------------------------------------
-- Routing
-------------------------------------------------------------------------------

application :: ServerEnv -> Application
application env req respond = do
    let corsHdrs = corsHeadersFor env req
        respond' rsp = respond (addHeaders corsHdrs rsp)
    result <- try (route env req)
    case result of
        Right rsp -> respond' rsp
        Left (e :: SomeException)
            | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
            | Just apiError <- fromException e -> respond' (errorResponse apiError)
            | otherwise -> respond' $ errorResponse $ ApiError status500 "internal_error" (Text.pack (displayException e))

{- | The three endpoints a client can reach before it has a token describe
the server itself; everything else needs one, when tokens are in use.
@OPTIONS@ (a CORS preflight) is answered on any path, before both auth and
'checkOrigin': see 'preflightResponse'.
-}
route :: ServerEnv -> Request -> IO Response
route env req = case (requestMethod req, path) of
    ("OPTIONS", _) -> preflightResponse env req
    ("GET", ["healthz"]) -> healthz env
    ("GET", ["openapi.json"]) -> pure $ json status200 env.envDocument
    ("GET", []) | env.envUI -> pure uiResponse
    _ -> do
        checkOrigin env req
        caller <- authenticateRequest env req path
        routeAuthenticated env req caller path
  where
    path = filter (not . Text.null) (pathInfo req)

uiResponse :: Response
uiResponse =
    responseLBS
        status200
        [(hContentType, "text/html; charset=utf-8")]
        (LByteString.fromStrict (Text.encodeUtf8 uiPage))

routeAuthenticated :: ServerEnv -> Request -> Caller -> [Text] -> IO Response
routeAuthenticated env req caller path = case (requestMethod req, path) of
    ("GET", ["v1", "agents"]) -> listAgents env
    ("GET", ["v1", "agents", slug]) -> getAgentH env slug
    ("PUT", ["v1", "agents", slug]) -> requireAdmin env caller >>= \by -> putAgentH env req by slug
    ("DELETE", ["v1", "agents", slug]) -> requireAdmin env caller >> deleteAgentH env slug
    ("POST", ["v1", "sessions"]) -> createH env req caller
    ("GET", ["v1", "sessions"]) -> listSessions env req caller
    ("GET", ["v1", "sessions", sid]) -> withSession sid (getH env)
    ("DELETE", ["v1", "sessions", sid]) -> withSession sid (deleteH env req)
    ("POST", ["v1", "sessions", sid, "messages"]) -> withSession sid (messagesH env req)
    ("POST", ["v1", "sessions", sid, "resume"]) -> withSession sid (resumeH env req)
    ("POST", ["v1", "sessions", sid, "cancel"]) -> withSession sid (cancelH env)
    ("POST", ["v1", "sessions", sid, "cancel-attached"]) -> withSession sid (cancelAttachedH env)
    ("POST", ["v1", "sessions", sid, "pause"]) -> withSession sid (pauseH env)
    ("GET", ["v1", "sessions", sid, "pending"]) -> withSession sid (pendingH env)
    ("GET", ["v1", "sessions", sid, "events"]) -> withSession sid (eventsH env)
    ("POST", ["v1", "continuations", token]) -> continuationH env req caller token
    ("POST", ["mcp"]) -> mcpH env req caller
    _
        | knownPath path -> throwIO $ ApiError status405 "method_not_allowed" "method not allowed on this path"
        | otherwise -> throwIO $ ApiError status404 "not_found" "no such endpoint"
  where
    withSession :: Text -> (SessionId -> IO Response) -> IO Response
    withSession txt k = case UUID.fromText txt of
        Just uuid -> authorize env caller (SessionId uuid) >> k (SessionId uuid)
        Nothing -> throwIO $ ApiError status404 "unknown_session" ("not a session id: " <> txt)

    knownPath = \case
        [] -> env.envUI
        ["healthz"] -> True
        ["openapi.json"] -> True
        ["v1", "agents"] -> True
        ["v1", "agents", _] -> True
        ["v1", "sessions"] -> True
        ["v1", "sessions", _] -> True
        ["v1", "sessions", _, action] -> action `elem` ["messages", "resume", "cancel", "cancel-attached", "pause", "pending", "events"]
        ["v1", "continuations", _] -> True
        ["mcp"] -> True
        _ -> False

-------------------------------------------------------------------------------
-- Authentication
-------------------------------------------------------------------------------

{- | Without authentication, refuse requests that a browser sends from a
page that is not on this machine. This blocks DNS-rebinding attacks, where a
remote page reaches the server through a name that resolves to 127.0.0.1. An
origin named by @--cors-origin@ is allowed too, even without authentication:
the operator opted it in explicitly. Requests without an @Origin@ header
(curl, servers, MCP clients) pass.
-}
checkOrigin :: ServerEnv -> Request -> IO ()
checkOrigin env req = case (env.envAuth, lookup "Origin" (requestHeaders req)) of
    (Nothing, Just origin)
        | isLoopbackOrigin origin -> pure ()
        | Just _ <- matchOrigin env.envCorsOrigins origin -> pure ()
        | otherwise ->
            throwIO $ ApiError status403 "forbidden_origin" "cross-origin requests need authentication (--auth-tokens), or list the origin with --cors-origin"
    _ -> pure ()

-- | @http(s)://localhost@, @127.0.0.1@, or @[::1]@, with any port.
isLoopbackOrigin :: ByteString.ByteString -> Bool
isLoopbackOrigin origin = case Text.breakOn "://" (Text.decodeUtf8Lenient origin) of
    (scheme, rest)
        | scheme `elem` ["http", "https"]
        , Just hostPort <- Text.stripPrefix "://" rest ->
            hostOf hostPort `elem` ["localhost", "127.0.0.1", "[::1]"]
    _ -> False
  where
    hostOf hp
        | "[" `Text.isPrefixOf` hp = Text.takeWhile (/= ']') hp <> "]"
        | otherwise = Text.takeWhile (/= ':') hp

-------------------------------------------------------------------------------
-- CORS
-------------------------------------------------------------------------------

{- | Whether a request's @Origin@ is allowed by @--cors-origin@, and what to
echo back in @Access-Control-Allow-Origin@ if so: the request's own origin,
verbatim, never a literal @"*"@ (so the header is meaningful even when a
browser sends credentials). Matching is exact on scheme and port; the host
is compared case-insensitively. A configured @"*"@ matches any origin.
-}
matchOrigin :: [Text] -> ByteString.ByteString -> Maybe Text
matchOrigin allowed originBytes
    | "*" `elem` allowed = Just originText
    | any ((== normalizeOrigin originText) . normalizeOrigin) allowed = Just originText
    | otherwise = Nothing
  where
    originText = Text.decodeUtf8Lenient originBytes

-- | Lower-cases the scheme and host of an origin; the port is left as-is.
normalizeOrigin :: Text -> Text
normalizeOrigin o = case Text.breakOn "://" o of
    (scheme, rest)
        | Just hostPort <- Text.stripPrefix "://" rest ->
            Text.toLower scheme <> "://" <> Text.toLower hostPort
    _ -> Text.toLower o

{- | @Access-Control-*@ headers to add to every response to a request whose
@Origin@ matches @--cors-origin@: the origin echoed back, @Vary: Origin@ (a
cache must not serve this response to a different origin), and
@Access-Control-Expose-Headers: Location@, which a fetch client needs to
read the @Location@ header @POST \/v1\/sessions@ answers with. Applied in
'application' to every response, success or error, so it also covers the
SSE stream and CORS-refused answers alike.
-}
corsHeadersFor :: ServerEnv -> Request -> [Header]
corsHeadersFor env req = case lookup "Origin" (requestHeaders req) of
    Nothing -> []
    Just origin -> case matchOrigin env.envCorsOrigins origin of
        Nothing -> []
        Just matched ->
            [ ("Access-Control-Allow-Origin", Text.encodeUtf8 matched)
            , ("Vary", "Origin")
            , ("Access-Control-Expose-Headers", "Location")
            ]

addHeaders :: [Header] -> Response -> Response
addHeaders hdrs = mapResponseHeaders (hdrs <>)

{- | A CORS preflight: answered on any path, before authentication and
before 'checkOrigin' rejects a request outright. It still runs the same
origin check 'checkOrigin' would (so a non-loopback, non-listed origin
without authentication gets @403 forbidden_origin@ here too, rather than a
misleading 204 that the real request would then refuse), then answers with
no body and:

* @204@ (no content);
* the 'corsHeadersFor' headers, when the origin matches @--cors-origin@;
* @Access-Control-Allow-Methods@, @Access-Control-Allow-Headers@ (the ones
  every route reads: @Authorization@, @Content-Type@, and @Last-Event-ID@
  for a reconnecting event stream), and @Access-Control-Max-Age@, always.
-}
preflightResponse :: ServerEnv -> Request -> IO Response
preflightResponse env req = do
    checkOrigin env req
    pure $ responseLBS status204 (corsHeadersFor env req <> preflightHeaders) ""

preflightHeaders :: [Header]
preflightHeaders =
    [ ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
    , ("Access-Control-Allow-Headers", "Authorization, Content-Type, Last-Event-ID")
    , ("Access-Control-Max-Age", "600")
    ]

{- | The bearer token of a request. The event stream also accepts it as an
@access_token@ query parameter, because @EventSource@ cannot set headers;
no other endpoint does, and the request log records no query strings.
-}
authenticateRequest :: ServerEnv -> Request -> [Text] -> IO Caller
authenticateRequest env req path = case env.envAuth of
    Nothing -> pure (Caller Nothing)
    Just tokens ->
        case asum [headerToken, queryToken] >>= authenticate tokens of
            Just owner -> pure (Caller (Just owner))
            Nothing -> throwIO $ ApiError status401 "unauthorized" "a valid bearer token is required"
  where
    headerToken = lookup hAuthorization (requestHeaders req) >>= bearerToken
    queryToken = case path of
        ["v1", "sessions", _, "events"] -> Text.encodeUtf8 <$> param "access_token" (queryParams req)
        _ -> Nothing

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
    agents <- hostAllAgents env.envHost
    json status200 <$> mapM (uncurry (agentView env.envHost)) (Map.toList agents)

{- | An agent: its description, tools, where it comes from, and its
declared parameters (@todos/tool-partial-application.md@, Phase 4). A
parameter's value is never included, only whether the process already
supplies one (@bound@) and whether it can be overridden (@pinned@: false
when the operator locked it down with @--pin@, or when it is process-scope).
-}
agentView :: Host -> Text -> (AgentSource, OSAgentNode) -> IO Aeson.Value
agentView host slug (source, node) = do
    tools <- readTVarIO node.osNodeTools
    resolved <- readTVarIO node.osNodeParams
    let pinnedNames = Set.fromList [n | (n, pv) <- Map.toList host.hostProcessParams, pv.pvPinned]
        decls = fromMaybe [] (Base.parameters node.osNodeConfig) :: [ParameterDecl]
        paramView :: ParameterDecl -> Aeson.Value
        paramView d =
            Aeson.object $
                [ "name" .= d.paramName
                , "secret" .= d.paramSecret
                , "scope" .= d.paramScope
                , "required" .= d.paramRequired
                , "bound" .= Map.member d.paramName resolved
                , "pinned" .= (d.paramScope == ScopeProcess || d.paramName `Set.member` pinnedNames)
                ]
                    <> maybe [] (\desc -> ["description" .= desc]) d.paramDescription
    pure $
        Aeson.object $
            [ "slug" .= slug
            , "description" .= Base.announce node.osNodeConfig
            , "tools" .= [t.declareTool.toolDescriptionName.getToolName | t <- tools]
            , "parameters" .= map paramView decls
            ]
                <> case source of
                    FromFile -> ["source" .= ("file" :: Text)]
                    FromDatabase sa ->
                        [ "source" .= ("database" :: Text)
                        , "updated_at" .= sa.saUpdatedAt
                        , "updated_by" .= sa.saUpdatedBy
                        , "config" .= sa.saConfig
                        ]

getAgentH :: ServerEnv -> Text -> IO Response
getAgentH env slug =
    Map.lookup slug <$> hostAllAgents env.envHost >>= \case
        Nothing -> throwIO $ fromRunnerError (UnknownAgent slug)
        Just entry -> json status200 <$> agentView env.envHost slug entry

{- | Store an agent. The body is the @contents@ of an agent file; its slug
is the one in the path.
-}
putAgentH :: ServerEnv -> Request -> Maybe Text -> Text -> IO Response
putAgentH env req by slug = do
    body <- jsonBody req
    case KeyMap.lookup "slug" body of
        Just (Aeson.String other) | other /= slug -> badRequest ("the body's slug is " <> other <> ", the path's " <> slug)
        _ -> pure ()
    agent <- parseBody (KeyMap.insert "slug" (Aeson.String slug) body) (Aeson.parseJSON . Aeson.Object)
    putStoredAgent env.envHost by agent >>= \case
        Left err -> throwIO $ fromEditError err
        Right (_, created) ->
            getAgentH env slug >>= \rsp ->
                pure $ if created then mapResponseStatus (const status201) rsp else rsp

deleteAgentH :: ServerEnv -> Text -> IO Response
deleteAgentH env slug =
    deleteStoredAgent env.envHost slug >>= \case
        Left err -> throwIO $ fromEditError err
        Right () -> pure $ json status200 $ Aeson.object ["deleted" .= slug]

fromEditError :: AgentEditError -> ApiError
fromEditError = \case
    EditsUnsupported -> ApiError status403 "agent_edits_disabled" "this server stores no agents"
    AgentDefinedByFile slug -> ApiError status409 "agent_defined_by_file" (slug <> " comes from an agent file and cannot be changed over the API")
    AgentUsesFiles fields -> ApiError status400 "agent_uses_files" ("stored agents cannot use file-based fields: " <> Text.intercalate ", " fields)
    AgentFailedToLoad err -> ApiError status400 "agent_failed_to_load" err
    NoStoredAgent slug -> ApiError status404 "unknown_agent" ("no stored agent named " <> slug)

-- | The caller, if allowed to edit agents.
requireAdmin :: ServerEnv -> Caller -> IO (Maybe Text)
requireAdmin env (Caller owner)
    | null env.envAdmins = throwIO $ ApiError status403 "agent_edits_disabled" "storing agents is disabled (see --admin-owners)"
    | Just o <- owner, o `elem` env.envAdmins = pure owner
    | otherwise = throwIO $ ApiError status403 "forbidden" "only admin owners can store or delete agents"

createH :: ServerEnv -> Request -> Caller -> IO Response
createH env req (Caller owner) = do
    w <- waitParams req
    body <- jsonBody req
    (agent, msg, mode, params) <- parseBody body $ \o -> do
        agent <- o .: "agent"
        msg <- messageFields o
        mode <- runField o
        params <- paramsField o
        pure (agent, msg, mode, params)
    meta <- orThrow $ createSessionAs env.envRunner owner agent msg mode params
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
    (msg, mode, params) <- parseBody body $ \o -> (,,) <$> messageFields o <*> runField o <*> paramsField o
    _ <- orThrow $ postMessage env.envRunner sid msg mode params
    runResponse <$> afterRun env w sid

resumeH :: ServerEnv -> Request -> SessionId -> IO Response
resumeH env req sid = do
    w <- waitParams req
    body <- jsonBodyOrEmpty req
    (mode, params) <- parseBody body $ \o -> do
        mode <-
            o .:? "mode" >>= \case
                Nothing -> pure UntilBlocked
                Just m -> maybe (fail "mode must be \"step\" or \"until_blocked\"") pure (runModeFromText m)
        params <- paramsField o
        pure (mode, params)
    _ <- orThrow $ resume env.envRunner sid mode params
    runResponse <$> afterRun env w sid

cancelH :: ServerEnv -> SessionId -> IO Response
cancelH env sid = json status200 <$> orThrow (cancelRun env.envRunner sid)

{- | Hard-cancel every tool call currently attached to a session, without
stopping the run itself (unlike 'cancelH', a full teardown). Posts
'Control' mail directly; a cancelled call's result never arrives.
-}
cancelAttachedH :: ServerEnv -> SessionId -> IO Response
cancelAttachedH env sid = json status200 <$> orThrow (cancelAttachedCalls env.envRunner sid)

{- | Pause a session's run: posts 'Pause' 'Control' mail, which the runner
loop reacts to at its next iteration by stopping the run and persisting
'StatusPaused' (attached calls keep running unless the agent's config sets
'pauseCancelsCalls' -- see 'cancelAttachedH' to cancel them explicitly).
Resume with 'resumeH', which works from 'StatusPaused' regardless of
whether this mail was ever read.
-}
pauseH :: ServerEnv -> SessionId -> IO Response
pauseH env sid = json status200 <$> orThrow (pauseSession env.envRunner sid)

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
    (result, autoResume, params) <- parseBody body $ \o -> do
        result <-
            o .: "result" >>= \case
                Aeson.String txt -> pure (TextResponse txt)
                other -> Aeson.parseJSON other
        autoResume <- fromMaybe True <$> o .:? "resume"
        params <- paramsField o
        pure (result, autoResume, params)
    meta <- orThrow $ completeCall env.envRunner token result autoResume params
    runResponse <$> afterRun env w meta.smSessionId

mcpH :: ServerEnv -> Request -> Caller -> IO Response
mcpH env req (Caller owner) = do
    raw <- readBody req
    agents <- Map.map snd <$> hostAllAgents env.envHost
    let ctx =
            McpContext
                { mcRunner = env.envRunner
                , mcAgents = agents
                , mcOwner = owner
                , mcWait = \sid -> waitForRun env sid 120
                , mcHeaderParams = headerParams req
                }
    (status, answer) <- handleMcp ctx raw
    pure $ case answer of
        Just value -> json status value
        Nothing -> responseLBS status [] ""

{- | Parameter values from this request's @Agents-Param-<name>@ headers
(@todos/tool-partial-application.md@, Phase 5). Every value is a string;
a @tools/call@'s @_meta@ can carry any JSON value and overrides these.
-}
headerParams :: Request -> Map Text Aeson.Value
headerParams req =
    Map.fromList
        [ (pName, Aeson.String (Text.decodeUtf8 value))
        | (name, value) <- requestHeaders req
        , let nameText = Text.decodeUtf8 (CI.original name)
        , Text.toLower (Text.take (Text.length prefix) nameText) == prefix
        , let pName = Text.drop (Text.length prefix) nameText
        , not (Text.null pName)
        ]
  where
    prefix = "agents-param-"

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
    TextDelta sid text -> Aeson.object ["session_id" .= sid, "text" .= text]
    ToolCallStarted sid callId toolName -> Aeson.object ["session_id" .= sid, "tool_call_id" .= callId, "tool" .= toolName]
    ToolCallCompleted sid callId toolName succeeded ->
        Aeson.object ["session_id" .= sid, "tool_call_id" .= callId, "tool" .= toolName, "succeeded" .= succeeded]

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
    when w.wpWait $ waitForRun env sid w.wpTimeout
    loadView env sid

-- | Wait for a session's run to stop, the timeout, or shutdown.
waitForRun :: ServerEnv -> SessionId -> NominalDiffTime -> IO ()
waitForRun env sid limit =
    race_
        (atomically (readTVar env.envShutdown >>= check))
        (awaitRun env.envRunner sid limit)

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

{- | @prompt@, optional @media: [{mime, base64, filename?}]@, and optional
@interrupt: false@ (@todos/session-mailbox.md@ R3a\/R4: only has an effect
against a busy session, where it detaches attached tool calls -- and, with
@interruptCompletions@ on, cancels an in-flight completion -- rather than
waiting behind them).
-}
messageFields :: Aeson.Object -> Aeson.Parser NewMessage
messageFields o = do
    prompt <- o .: "prompt"
    media <- fromMaybe [] <$> o .:? "media"
    interrupt <- fromMaybe False <$> o .:? "interrupt"
    NewMessage prompt <$> mapM mediaItem media <*> pure interrupt
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

{- | @params@: an object of parameter name to value. A @null@ value clears a
session-scope value rather than setting one
(@todos/tool-partial-application.md@, Phase 4).
-}
paramsField :: Aeson.Object -> Aeson.Parser (Map ParamName Aeson.Value)
paramsField o =
    o .:? "params" >>= \case
        Nothing -> pure Map.empty
        Just (Aeson.Object obj) -> pure $ Map.fromList [(Key.toText k, v) | (k, v) <- KeyMap.toList obj]
        Just _ -> fail "params must be an object"

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
