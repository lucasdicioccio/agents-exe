{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | 'httpClient': a 'RunnerClient' over the HTTP API of a running
@agents-exe serve@\/@agents-server@ (@todos/os-as-standalone-server.md@
Design §3, Phase 4), so the TUI can attach to a server exactly as it runs
over an in-process runner (D6).

Commands map one to one onto the documented routes
(@documentation/agents-server.md@); replies decode into the same 'Reply'
'inProcessClient' builds; an @{error, message}@ answer decodes into the
'RunnerError' its code names. The event feed is the SSE stream
(@GET \/v1\/events@, or @GET \/v1\/sessions\/:id\/events@ for one
session), reconnected transparently with @Last-Event-ID@.

Two kinds of endpoint are accepted ('parseEndpoint'):

* @http:\/\/host:port@ or @https:\/\/host:port@, optionally with a path
  prefix (@http:\/\/host\/agents@ when a proxy mounts the API there);
* a Unix domain socket (@agents-exe serve --socket PATH@), written
  @unix:\/\/\/abs\/path\/to.sock@, @unix:relative\/path.sock@, or just a
  filesystem path (anything without a @scheme:\/\/@ that contains a
  @\/@ or ends in @.sock@).

Like "System.Agents.Host.Client", this module depends on no server
library (@wai@\/@warp@): only @http-client@ and @network@.
-}
module System.Agents.Host.Client.Http (
    -- * Configuration
    HttpClientConfig (..),
    Endpoint (..),
    parseEndpoint,
    renderEndpoint,
    defaultHttpClientConfig,

    -- * The client
    httpClient,
    HttpClientError (..),

    -- * SSE framing (exported for tests)
    SseFrame (..),
    SseParser,
    emptySseParser,
    feedSse,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (asyncWithUnmask, cancel)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, bracketOnError, displayException, finally, fromException, mask_, throwIO)
import Control.Monad (unless)
import Data.Aeson ((.:), (.:?), (.=))
import Data.Bifunctor (first)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (for_)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.UUID as UUID
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.Internal as HttpInternal
import qualified Network.HTTP.Client.TLS as HttpTls
import Network.HTTP.Types (Method, encodePathSegments, statusCode)
import qualified Network.Socket as NS
import System.Timeout (timeout)
import Text.Read (readMaybe)
import UnliftIO.Exception (tryAny)

import System.Agents.Host.Client (RunnerClient (..), Subscription (..))
import System.Agents.Host.Runner (ReplayUnavailable (..))
import System.Agents.Protocol
import System.Agents.Session.Base (ContinuationToken (..), SessionId (..), sessionStatusText)
import System.Agents.SessionStore (SessionMeta (..), SessionQuery (..))
import System.Agents.Tools.Params.Types (ParamName)

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | Where the server listens.
data Endpoint
    = -- | @http(s):\/\/host:port[\/prefix]@, kept verbatim (no trailing slash).
      TcpEndpoint String
    | -- | A Unix domain socket path (@serve --socket@).
      UnixEndpoint FilePath
    deriving (Show, Eq)

{- | Read an @--attach@ argument: see the module header for the accepted
forms.
-}
parseEndpoint :: String -> Either String Endpoint
parseEndpoint raw
    | Just rest <- stripPrefix' "unix://" raw = unixPath rest
    | Just rest <- stripPrefix' "unix:" raw = unixPath rest
    | any (`startsWith` raw) ["http://", "https://"] = Right (TcpEndpoint (dropTrailingSlash raw))
    | "://" `Text.isInfixOf` Text.pack raw = Left ("unsupported URL scheme in " <> raw <> " (use http://, https://, unix:// or a socket path)")
    | '/' `elem` raw || ".sock" `isSuffixOf` raw = unixPath raw
    | otherwise = Left ("not a URL or a socket path: " <> raw <> " (use http://HOST:PORT, unix:///PATH or a socket path)")
  where
    startsWith p s = take (length p) s == p
    stripPrefix' p s = if startsWith p s then Just (drop (length p) s) else Nothing
    unixPath "" = Left "empty Unix socket path"
    unixPath p = Right (UnixEndpoint p)
    dropTrailingSlash s = case reverse s of
        ('/' : r) -> dropTrailingSlash (reverse r)
        _ -> s

-- | An 'Endpoint' as a URL, for messages.
renderEndpoint :: Endpoint -> String
renderEndpoint = \case
    TcpEndpoint url -> url
    UnixEndpoint path -> "unix://" <> path

data HttpClientConfig = HttpClientConfig
    { hccEndpoint :: Endpoint
    , hccToken :: Maybe Text
    {- ^ A bearer token (@--auth-tokens@ on the server): sent as
    @Authorization: Bearer@ on commands, and as @?access_token=@ on the
    event streams.
    -}
    , hccOwnerHint :: Maybe Text
    {- ^ Who this client says it is. Informational only: a server derives
    the owner from the token, never from anything a client claims.
    -}
    , hccCommandTimeout :: NominalDiffTime
    -- ^ How long a command may take to answer (an 'AwaitRun' adds its own limit).
    , hccReconnectDelay :: NominalDiffTime
    -- ^ First delay before reconnecting a dropped event stream; doubles up to 'hccReconnectMaxDelay'.
    , hccReconnectMaxDelay :: NominalDiffTime
    , hccStallTimeout :: NominalDiffTime
    {- ^ An event stream that sends nothing -- not even the server's
    keepalive comment, every 15 seconds by default -- for this long is
    considered dead and reconnected.
    -}
    }
    deriving (Show)

defaultHttpClientConfig :: Endpoint -> HttpClientConfig
defaultHttpClientConfig endpoint =
    HttpClientConfig
        { hccEndpoint = endpoint
        , hccToken = Nothing
        , hccOwnerHint = Nothing
        , hccCommandTimeout = 60
        , hccReconnectDelay = 0.1
        , hccReconnectMaxDelay = 5
        , hccStallTimeout = 45
        }

{- | Failing to open the /first/ connection of an event stream (the server
is unreachable, refuses the token, ...). 'rcSubscribe' can only report
'ReplayUnavailable' in its result, so this is thrown instead; once a
stream is up, every later failure is retried silently.
-}
newtype HttpClientError = HttpClientError Text
    deriving (Show)

instance Exception HttpClientError

-------------------------------------------------------------------------------
-- Transport
-------------------------------------------------------------------------------

data Transport = Transport
    { tManager :: Http.Manager
    , tBase :: Http.Request
    -- ^ Scheme, host, port, and the path prefix (without a trailing slash).
    , tConfig :: HttpClientConfig
    }

newTransport :: HttpClientConfig -> IO Transport
newTransport cfg = case cfg.hccEndpoint of
    TcpEndpoint url -> do
        manager <- HttpTls.newTlsManagerWith HttpTls.tlsManagerSettings{Http.managerResponseTimeout = Http.responseTimeoutNone}
        base <- Http.parseRequest url
        pure $ Transport manager base cfg
    UnixEndpoint path -> do
        manager <-
            Http.newManager
                Http.defaultManagerSettings
                    { Http.managerRawConnection = pure (\_ _ _ -> unixConnection path)
                    , Http.managerResponseTimeout = Http.responseTimeoutNone
                    }
        base <- Http.parseRequest "http://localhost"
        pure $ Transport manager base cfg

unixConnection :: FilePath -> IO HttpInternal.Connection
unixConnection path =
    bracketOnError (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol) NS.close $ \sock -> do
        NS.connect sock (NS.SockAddrUnix path)
        HttpInternal.socketConnection sock 8192

-- | A request to @segments@ (each one percent-encoded) under the base URL.
mkRequest :: Transport -> Method -> [Text] -> [(ByteString.ByteString, ByteString.ByteString)] -> Maybe Aeson.Value -> Http.Request
mkRequest t method segments query body =
    Http.setQueryString [(k, Just v) | (k, v) <- query] $
        t.tBase
            { Http.method = method
            , Http.path = basePath <> LByteString.toStrict (Builder.toLazyByteString (encodePathSegments segments))
            , Http.requestHeaders =
                [("Content-Type", "application/json") | isJust body]
                    <> [("Accept", "application/json")]
                    <> [("Authorization", "Bearer " <> Text.encodeUtf8 tok) | Just tok <- [t.tConfig.hccToken]]
            , Http.requestBody = maybe mempty (Http.RequestBodyLBS . Aeson.encode) body
            , Http.responseTimeout = micros t.tConfig.hccCommandTimeout
            }
  where
    basePath = Char8.dropWhileEnd (== '/') (Http.path t.tBase)

micros :: NominalDiffTime -> Http.ResponseTimeout
micros d = Http.responseTimeoutMicro (max 1 (round (realToFrac d * 1_000_000 :: Double)))

delayMicros :: NominalDiffTime -> Int
delayMicros d = max 0 (round (realToFrac d * 1_000_000 :: Double))

-- | Status and body, or a 'TransportError' when there was no answer at all.
perform :: Transport -> Http.Request -> IO (Either RunnerError (Int, LByteString.ByteString))
perform t req =
    tryAny (Http.httpLbs req t.tManager) >>= \case
        Left e -> pure $ Left $ TransportError ("cannot reach " <> Text.pack (renderEndpoint t.tConfig.hccEndpoint) <> ": " <> describeException e)
        Right rsp -> pure $ Right (statusCode (Http.responseStatus rsp), Http.responseBody rsp)

{- | One command round trip: a 2xx answer decodes with @parser@ (given the
status, for 'AwaitRun'); anything else decodes as @{error, message}@.
-}
roundTrip :: Transport -> Http.Request -> (Int -> Aeson.Value -> Aeson.Parser a) -> IO (Either RunnerError a)
roundTrip t req parser =
    perform t req >>= \case
        Left err -> pure (Left err)
        Right (status, raw)
            | status >= 200 && status < 300 -> pure $ decodeWith (parser status) raw
            | otherwise -> pure $ Left $ decodeError status raw
  where
    decodeWith p raw = case Aeson.eitherDecode (if LByteString.null raw then "null" else raw) of
        Left err -> Left $ TransportError ("undecodable answer: " <> Text.pack err)
        Right value -> case Aeson.parseEither p value of
            Left err -> Left $ TransportError ("unexpected answer: " <> Text.pack err)
            Right reply -> Right reply

{- | What went wrong, without http-client's dump of the whole request
(long, and not what a user needs to read).
-}
describeException :: SomeException -> Text
describeException e = case fromException e of
    Just (Http.HttpExceptionRequest _ content) -> Text.pack (show content)
    Just (Http.InvalidUrlException url reason) -> Text.pack ("invalid URL " <> url <> ": " <> reason)
    Nothing -> Text.pack (displayException e)

-- | An error answer: the 'RunnerError' its code names, or a 'TransportError'.
decodeError :: Int -> LByteString.ByteString -> RunnerError
decodeError status raw = case Aeson.decode raw of
    Just (Aeson.Object o)
        | Just (Aeson.String code) <- KeyMap.lookup "error" o ->
            let msg = case KeyMap.lookup "message" o of
                    Just (Aeson.String m) -> m
                    _ -> code
             in fromMaybe
                    (TransportError ("HTTP " <> Text.pack (show status) <> " " <> code <> ": " <> msg))
                    (runnerErrorFromKnownCode code msg)
    _ -> TransportError ("HTTP " <> Text.pack (show status) <> ": " <> Text.decodeUtf8Lenient (LByteString.toStrict (LByteString.take 500 raw)))

-------------------------------------------------------------------------------
-- The client
-------------------------------------------------------------------------------

{- | A 'RunnerClient' talking to a server over HTTP (or HTTP over a Unix
socket). Creating it opens no connection; the first command does.
-}
httpClient :: HttpClientConfig -> IO RunnerClient
httpClient cfg = do
    t <- newTransport cfg
    pure
        RunnerClient
            { rcCommand = \cmd -> first (refineError cmd) <$> dispatch t cmd
            , rcSubscribe = subscribeHttp t
            }

{- | Every 'Command' onto its route. Total over 'Command' on purpose (no
catch-all), like 'System.Agents.Host.Client.inProcessClient''s.
-}
dispatch :: Transport -> Command -> IO (Either RunnerError Reply)
dispatch t = \case
    CreateSession parent agent message mode params ->
        post ["v1", "sessions"] [] (createBody parent agent message mode params) viewMeta
    SpawnSession parent agent message ->
        -- 'System.Agents.Host.Runner.spawnSession' is a create with a parent,
        -- a first message, and a run to completion; the server attributes
        -- it to the caller, whose sessions a child's owner resolves to anyway.
        post ["v1", "sessions"] [] (createBody (Just parent) agent (Just message) (Just UntilBlocked) Map.empty) viewMeta
    PostMessage sid message mode params ->
        post (session sid ["messages"]) [] (merge (Aeson.toJSON message) ["run" .= runText mode, "params" .= params]) viewMeta
    Resume sid mode params ->
        post (session sid ["resume"]) [] (Aeson.object ["mode" .= runModeText mode, "params" .= params]) viewMeta
    CompleteCall token result autoResume params ->
        post ["v1", "continuations", tokenText token] [] (Aeson.object ["result" .= result, "resume" .= autoResume, "params" .= params]) viewMeta
    CancelRun sid -> postEmpty (session sid ["cancel"]) plainMeta
    CancelAttached sid -> postEmpty (session sid ["cancel-attached"]) plainMeta
    Pause sid -> postEmpty (session sid ["pause"]) plainMeta
    SendMail sid priority body ->
        post (session sid ["mail"]) [] (Aeson.object ["body" .= body, "priority" .= priority]) (\_ v -> RReceipt <$> Aeson.parseJSON v)
    ListMail sid unreadOnly ->
        get (session sid ["mail"]) [("unread", boolText unreadOnly)] (\_ -> Aeson.withObject "mail" $ \o -> RMail <$> o .: "mail")
    ForkSession sid atTurn newAgent ->
        post (session sid ["fork"]) [] (Aeson.object ["at_turn" .= atTurn, "agent" .= newAgent]) viewMeta
    ListSessions query -> fmap RSessions <$> listAll t query
    GetSession sid ->
        get (session sid []) [] (\_ -> Aeson.withObject "session" $ \o -> RSession <$> o .: "session" <*> Aeson.parseJSON (Aeson.Object o))
    ListAgents -> get ["v1", "agents"] [] (\_ v -> RAgents <$> Aeson.parseJSON v)
    GetAgent slug -> get ["v1", "agents", slug] [] (\_ v -> RAgent <$> Aeson.parseJSON v)
    DeleteSession sid mode ->
        send "DELETE" (session sid []) [("dry_run", boolText (mode == DryRun))] Nothing (\_ v -> RDeletion <$> Aeson.parseJSON v)
    AwaitRun sid limit ->
        let req = (mkRequest t "GET" (session sid []) [("wait", "true"), ("timeout", seconds limit)] Nothing){Http.responseTimeout = micros (limit + t.tConfig.hccCommandTimeout)}
         in roundTrip t req (\status v -> RAwait <$> Aeson.parseJSON v <*> pure (status == 202))
    Stats -> get ["healthz"] [] (\_ v -> RStats <$> Aeson.parseJSON v)
  where
    send method segs query body = roundTrip t (mkRequest t method segs query body)
    post segs query body = send "POST" segs query (Just body)
    postEmpty segs = send "POST" segs [] Nothing
    get segs query = send "GET" segs query Nothing

    session (SessionId uuid) rest = ["v1", "sessions", UUID.toText uuid] <> rest
    tokenText (ContinuationToken uuid) = UUID.toText uuid

    -- A session view (@GET \/v1\/sessions\/:id@'s shape): the metadata's
    -- own fields, plus @session@ and @pending@.
    viewMeta _ v = RSessionMeta <$> Aeson.parseJSON v
    plainMeta _ v = RSessionMeta <$> Aeson.parseJSON v

    boolText b = if b then "true" else "false"
    seconds :: NominalDiffTime -> ByteString.ByteString
    seconds d = Char8.pack (show (realToFrac d :: Double))

-- | @run@: @none@ for no run, as the server defaults an absent one to @until_blocked@.
runText :: Maybe RunMode -> Text
runText = maybe "none" runModeText

createBody :: Maybe SessionId -> Text -> Maybe NewMessage -> Maybe RunMode -> Map ParamName Aeson.Value -> Aeson.Value
createBody parent agent message mode params =
    merge
        (maybe (Aeson.object []) Aeson.toJSON message)
        (["agent" .= agent, "run" .= runText mode, "params" .= params] <> ["parent" .= p | Just p <- [parent]])

merge :: Aeson.Value -> [Aeson.Pair] -> Aeson.Value
merge (Aeson.Object o) pairs = Aeson.Object (KeyMap.fromList pairs <> o)
merge _ pairs = Aeson.object pairs

{- | 'ListSessions' over @GET \/v1\/sessions@, which pages by 500 at most
(50 by default): follow @next_before@ until the query's own limit (or,
with none, every match) is reached. The server ignores 'sqOwner' (it uses
the caller's).
-}
listAll :: Transport -> SessionQuery -> IO (Either RunnerError [SessionMeta])
listAll t query = go query.sqUpdatedBefore []
  where
    wanted = query.sqLimit
    go before acc = do
        let pageSize = maybe 500 (\n -> max 1 (min 500 (n - length acc))) wanted
            params =
                [("limit", Char8.pack (show pageSize))]
                    <> [("agent", Text.encodeUtf8 a) | Just a <- [query.sqAgent]]
                    <> [("status", Text.encodeUtf8 (Text.intercalate "," (map sessionStatusText ss))) | Just ss <- [query.sqStatuses]]
                    <> [("parent", Text.encodeUtf8 (UUID.toText p)) | Just (SessionId p) <- [query.sqParent]]
                    <> [("before", Char8.pack (iso8601Show b)) | Just b <- [before]]
            req = mkRequest t "GET" ["v1", "sessions"] params Nothing
        page <- roundTrip t req $ \_ -> Aeson.withObject "sessions" $ \o ->
            (,) <$> o .: "sessions" <*> o .:? "next_before"
        case page of
            Left err -> pure (Left err)
            Right (metas, next) -> do
                let acc' = acc <> metas
                    full = maybe False (length acc' >=) wanted
                case next of
                    Just nb | not full, not (null metas) -> go (Just nb) acc'
                    _ -> pure $ Right (maybe id take wanted acc')

{- | Put back what the lossy @{error, message}@ decoding lost, from the
command itself: which session, token, agent or turn it was about. A
'RunnerError' from 'httpClient' then compares equal to the one
'System.Agents.Host.Client.inProcessClient' would give for the same
failure, in the cases the command names the thing (not in
'NotAcceptingMessages'\'s status, a 'Conflict''s versions, or a
parameter error's names, which only the message still carries).
-}
refineError :: Command -> RunnerError -> RunnerError
refineError cmd = \case
    UnknownSession _ | Just sid <- cmdSession -> UnknownSession sid
    RunInProgress _ | Just sid <- cmdSession -> RunInProgress sid
    NoActiveRun _ | Just sid <- cmdSession -> NoActiveRun sid
    NotAcceptingMessages _ st | Just sid <- cmdSession -> NotAcceptingMessages sid st
    MailboxRejected _ | Just sid <- cmdSession -> MailboxRejected sid
    UnknownTurn _ _ | ForkSession sid (Just idx) _ <- cmd -> UnknownTurn sid idx
    UnknownAgent _ | Just slug <- cmdAgent -> UnknownAgent slug
    UnknownToken _ | CompleteCall token _ _ _ <- cmd -> UnknownToken token
    TokenAlreadyCompleted _ | CompleteCall token _ _ _ <- cmd -> TokenAlreadyCompleted token
    other -> other
  where
    cmdSession = case cmd of
        CreateSession parent _ _ _ _ -> parent
        SpawnSession parent _ _ -> Just parent
        PostMessage sid _ _ _ -> Just sid
        Resume sid _ _ -> Just sid
        CompleteCall{} -> Nothing
        CancelRun sid -> Just sid
        CancelAttached sid -> Just sid
        Pause sid -> Just sid
        SendMail sid _ _ -> Just sid
        ListMail sid _ -> Just sid
        ForkSession sid _ _ -> Just sid
        ListSessions query -> query.sqParent
        GetSession sid -> Just sid
        ListAgents -> Nothing
        GetAgent _ -> Nothing
        DeleteSession sid _ -> Just sid
        AwaitRun sid _ -> Just sid
        Stats -> Nothing
    cmdAgent = case cmd of
        CreateSession _ agent _ _ _ -> Just agent
        SpawnSession _ agent _ -> Just agent
        GetAgent slug -> Just slug
        ForkSession _ _ agent -> agent
        _ -> Nothing

-------------------------------------------------------------------------------
-- Event streams
-------------------------------------------------------------------------------

{- | Open an event stream. 'OneSession' follows @GET \/v1\/sessions\/:id\/events@;
'Owner' and 'AllSessions' follow @GET \/v1\/events?scope=owner|all@
('AllSessions' falls back to @scope=owner@ when the server refuses it,
i.e. authentication is on and the token is not an admin's: "every session
this caller may see" is then exactly its own).

The initial connection is made before this returns: its @Agents-Replay@
header says whether @after@ could be replayed, and 'ReplayUnavailable'
is answered exactly when the in-process runner would. After that, a
reader thread parses frames and feeds 'subNext'. When the connection
drops (or stalls past 'hccStallTimeout'), it reconnects with
@Last-Event-ID@ set to the last event it delivered, so the server replays
what was missed from its ring; events already delivered are never
delivered twice. If that replay is no longer possible (the ring moved on,
or the server restarted), the stream just resumes live: a client that
cares refetches what it shows (the TUI does on every @session.updated@).
'subClose' stops the reader and closes the connection.
-}
subscribeHttp :: Transport -> SubscribeScope -> Maybe EventSeq -> IO (Either ReplayUnavailable Subscription)
subscribeHttp t requested after = do
    (scope, rsp, replay) <- openInitial
    if isJust after && replay == Just "unavailable"
        then Http.responseClose rsp >> pure (Left ReplayUnavailable)
        else do
            queue <- newTQueueIO
            lastSeq <- newTVarIO after
            let deliver = deliverFrame queue lastSeq
            reader <- mask_ $ asyncWithUnmask $ \unmask -> do
                _ <- unmask (tryAny (pump t rsp deliver)) `finally` Http.responseClose rsp
                unmask (reconnectLoop scope lastSeq deliver t.tConfig.hccReconnectDelay)
            pure $
                Right
                    Subscription
                        { subNext = atomically (readTQueue queue)
                        , subClose = cancel reader
                        }
  where
    openInitial =
        tryAny (openStream t requested after) >>= \case
            Left e -> throwIO $ HttpClientError ("cannot open the event stream at " <> Text.pack (renderEndpoint t.tConfig.hccEndpoint) <> ": " <> describeException e)
            Right (Right (rsp, replay)) -> pure (requested, rsp, replay)
            Right (Left (403, _))
                | requested == AllSessions ->
                    openStream t (Owner Nothing) after >>= \case
                        Right (rsp, replay) -> pure (Owner Nothing, rsp, replay)
                        Left (status, body) -> refused status body
            Right (Left (status, body)) -> refused status body
    refused status body =
        throwIO $ HttpClientError ("the event stream was refused: " <> runnerErrorMessage (decodeError status body))

    reconnectLoop scope lastSeq deliver delay = do
        threadDelay (delayMicros delay)
        from <- readTVarIO lastSeq
        result <-
            tryAny $
                bracket
                    (openStream t scope from)
                    (either (const (pure ())) (Http.responseClose . fst))
                    (either (const (pure False)) (\(rsp, _) -> True <$ tryAny (pump t rsp deliver)))
        let connected = either (const False) id result
            next = if connected then t.tConfig.hccReconnectDelay else min t.tConfig.hccReconnectMaxDelay (delay * 2)
        reconnectLoop scope lastSeq deliver next

{- | The stream's frames, one at a time, until the connection ends (or
stalls: no byte, not even a keepalive, within 'hccStallTimeout').
-}
pump :: Transport -> Http.Response Http.BodyReader -> (SseFrame -> IO ()) -> IO ()
pump t rsp deliver = loop emptySseParser
  where
    loop parser =
        timeout (delayMicros t.tConfig.hccStallTimeout) (Http.brRead (Http.responseBody rsp)) >>= \case
            Nothing -> pure ()
            Just chunk
                | ByteString.null chunk -> pure ()
                | otherwise -> do
                    let (parser', frames) = feedSse parser chunk
                    mapM_ deliver frames
                    loop parser'

{- | One frame: a @snapshot@ (a fresh connection's, or one the server sends
when it cannot replay) is not an 'Event' and is skipped; an event already
delivered (by sequence number) is skipped; an event this client cannot
decode (a newer server's kind) only advances the reconnect cursor.
-}
deliverFrame :: TQueue Event -> TVar (Maybe EventSeq) -> SseFrame -> IO ()
deliverFrame queue lastSeq frame
    | frame.sfEvent == Just "snapshot" = pure ()
    | otherwise = case Aeson.eitherDecodeStrict frame.sfData of
        Right ev -> atomically $ do
            seen <- readTVar lastSeq
            unless (maybe False (ev.evSeq <=) seen) $ do
                writeTVar lastSeq (Just ev.evSeq)
                writeTQueue queue ev
        Left _ -> for_ (frame.sfId >>= readMaybe . Text.unpack) $ \n ->
            atomically $ modifyTVar' lastSeq (Just . maybe (EventSeq n) (max (EventSeq n)))

{- | Open one event stream from @from@; a non-200 answer is closed here and
reported with its body.
-}
openStream :: Transport -> SubscribeScope -> Maybe EventSeq -> IO (Either (Int, LByteString.ByteString) (Http.Response Http.BodyReader, Maybe ByteString.ByteString))
openStream t scope from = do
    rsp <- Http.responseOpen req t.tManager
    let status = statusCode (Http.responseStatus rsp)
    if status == 200
        then pure $ Right (rsp, lookup "Agents-Replay" (Http.responseHeaders rsp))
        else do
            body <- LByteString.fromChunks <$> Http.brConsume (Http.responseBody rsp)
            Http.responseClose rsp
            pure $ Left (status, body)
  where
    (segments, scopeQuery) = case scope of
        OneSession (SessionId uuid) -> (["v1", "sessions", UUID.toText uuid, "events"], [])
        Owner _ -> (["v1", "events"], [("scope", "owner")])
        AllSessions -> (["v1", "events"], [("scope", "all")])
    query = scopeQuery <> [("access_token", Text.encodeUtf8 tok) | Just tok <- [t.tConfig.hccToken]]
    base = mkRequest t "GET" segments query Nothing
    req =
        base
            { Http.requestHeaders =
                [("Accept", "text/event-stream")]
                    <> [("Last-Event-ID", Char8.pack (show n)) | Just (EventSeq n) <- [from]]
            , Http.responseTimeout = Http.responseTimeoutNone
            }

-------------------------------------------------------------------------------
-- SSE framing
-------------------------------------------------------------------------------

-- | One server-sent event: its @id:@, @event:@, and @data:@ (lines joined by @\\n@).
data SseFrame = SseFrame
    { sfId :: Maybe Text
    , sfEvent :: Maybe Text
    , sfData :: ByteString.ByteString
    }
    deriving (Show, Eq)

-- | Incremental SSE parser state: a partial line, and the frame being read.
data SseParser = SseParser
    { spPartial :: ByteString.ByteString
    , spId :: Maybe Text
    , spEvent :: Maybe Text
    , spData :: [ByteString.ByteString]
    -- ^ Reversed.
    , spAny :: Bool
    -- ^ Whether any field was seen since the last dispatch.
    }

emptySseParser :: SseParser
emptySseParser = SseParser ByteString.empty Nothing Nothing [] False

{- | Feed bytes, get back every frame they complete. Lines end in @\\n@ or
@\\r\\n@; a blank line ends a frame; a line starting with @:@ (the server's
keepalive) is a comment; unknown fields are ignored.
-}
feedSse :: SseParser -> ByteString.ByteString -> (SseParser, [SseFrame])
feedSse parser chunk = go parser{spPartial = ByteString.empty} (spPartial parser <> chunk) []
  where
    go p buf acc = case Char8.elemIndex '\n' buf of
        Nothing -> (p{spPartial = buf}, reverse acc)
        Just i -> do
            let line = stripCR (ByteString.take i buf)
                rest = ByteString.drop (i + 1) buf
            case line of
                _
                    | ByteString.null line ->
                        if p.spAny
                            then go (reset p) rest (SseFrame p.spId p.spEvent (Char8.intercalate "\n" (reverse p.spData)) : acc)
                            else go p rest acc
                    | Char8.head line == ':' -> go p rest acc
                    | otherwise -> go (field p line) rest acc
    stripCR l = if not (ByteString.null l) && Char8.last l == '\r' then ByteString.init l else l
    reset p = p{spId = Nothing, spEvent = Nothing, spData = [], spAny = False}
    field p line =
        let (name, rest0) = Char8.break (== ':') line
            value = case ByteString.drop 1 rest0 of
                v | not (ByteString.null v) && Char8.head v == ' ' -> ByteString.drop 1 v
                v -> v
         in case name of
                "id" -> p{spId = Just (Text.decodeUtf8Lenient value), spAny = True}
                "event" -> p{spEvent = Just (Text.decodeUtf8Lenient value), spAny = True}
                "data" -> p{spData = value : p.spData, spAny = True}
                _ -> p
