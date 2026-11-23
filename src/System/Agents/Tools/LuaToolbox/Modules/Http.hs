{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | HTTP client module for LuaToolbox with host filtering.

This module provides HTTP request capabilities restricted by an 'HttpConfig'.
Key security features:

* Proper URI parsing for host extraction
* Two modes ('HttpMode'): an allowlist (the default; empty allowedHosts means
  NO network access) or a denylist (every host except deniedHosts)
* deniedHosts apply in both modes and win over allowedHosts; an IP-literal
  entry also refuses any host that resolves to that address
* Redirects are followed by hand and every hop is checked again
* Standard HTTP methods: GET, POST, and generic request
-}
module System.Agents.Tools.LuaToolbox.Modules.Http (
    HttpConfig (..),
    HttpMode,
    LuaHttpMode (..),
    HttpTrace (..),
    RequestOptions (..),
    registerHttpModule,
    extractHost,
    validateHost,
    checkUrl,
    maxRedirects,
    parseOptions,
) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- import Foreign.C.Types (CInt (..))
import qualified HsLua as Lua
import Network.HTTP.Client (
    Manager,
    Request (..),
    RequestBody (RequestBodyBS, RequestBodyLBS),
    Response (..),
    getUri,
    httpLbs,
    parseRequest,
 )
import Network.HTTP.Client.Internal (getRedirectedRequest)
import qualified Network.HTTP.Client.TLS as HTTPS
import Network.HTTP.Types (statusCode)
import qualified Network.Socket as NS
import Network.URI (URI (..), parseURI, uriAuthority, uriRegName)
import Prod.Tracer (Tracer (..), runTracer)
import System.Agents.Base (LuaHttpMode (..))

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

{- | Trace events for HTTP operations.

These events allow tracking of all HTTP requests for debugging,
auditing, and monitoring purposes.
-}
data HttpTrace
    = -- | HTTP GET request
      HttpGetTrace !Text !Int !(Maybe Text) !(Maybe Int)
    | -- | HTTP POST request
      HttpPostTrace !Text !Int !(Maybe Text) !(Maybe Int)
    | -- | HTTP generic request
      HttpRequestTrace !Text !Text !Int !(Maybe Text) !(Maybe Int)
    deriving (Show, Eq)

-- | How hosts are filtered (the toolbox's @HttpMode@).
type HttpMode = LuaHttpMode

{- | HTTP module configuration.

Security defaults:
* In allowlist mode, empty allowedHosts means NO network access
* deniedHosts win over allowedHosts, in both modes

Example:
@
HttpConfig
    { httpMode = LuaHttpAllowlist
    , httpAllowedHosts = ["localhost", "127.0.0.1", "api.example.com"]
    , httpDeniedHosts = []
    }
@
-}
data HttpConfig = HttpConfig
    { httpMode :: HttpMode
    -- ^ 'LuaHttpAllowlist': only allowedHosts; 'LuaHttpDenylist': all but deniedHosts.
    , httpAllowedHosts :: [Text]
    -- ^ Whitelist of allowed hosts (allowlist mode). If empty, NO hosts are allowed.
    , httpDeniedHosts :: [Text]
    -- ^ Hosts never reached, in either mode.
    }
    deriving (Show, Eq)

-- | How many redirects a request follows (each hop is re-checked).
maxRedirects :: Int
maxRedirects = 10

-- | Register the http module.
registerHttpModule :: Tracer IO HttpTrace -> Lua.State -> HttpConfig -> IO ()
registerHttpModule tracer lstate config = do
    manager <- HTTPS.newTlsManager
    Lua.runWith lstate $ do
        Lua.newtable

        Lua.pushName "get"
        Lua.pushHaskellFunction (luaGet tracer config manager)
        Lua.settable (Lua.nthTop 3)

        Lua.pushName "post"
        Lua.pushHaskellFunction (luaPost tracer config manager)
        Lua.settable (Lua.nthTop 3)

        Lua.pushName "request"
        Lua.pushHaskellFunction (luaRequest tracer config manager)
        Lua.settable (Lua.nthTop 3)

        Lua.setglobal (Lua.Name "http")

{- | Extract host from URL using proper URI parsing.

This function properly parses the URL and extracts the host component,
handling various URL formats correctly.

Returns Left with error message if URL is invalid or has no host.
-}
extractHost :: Text -> Either Text Text
extractHost url =
    case parseURI (Text.unpack url) of
        Nothing -> Left "Invalid URL"
        Just uri -> case uriAuthority uri of
            Nothing -> Left "No host in URL"
            Just auth ->
                let hostVal = Text.pack $ uriRegName auth
                 in if Text.null hostVal
                        then Left "No host in URL"
                        else Right hostVal

{- | Validate a host name against the config, by name only.

In allowlist mode an empty whitelist means no hosts are allowed (secure
default). Denied hosts win in both modes. Hosts are compared ignoring case
and a trailing dot. Returns Left with an error message if the host is refused.

This does not resolve the host: 'checkUrl' also refuses hosts that resolve to
an IP-literal entry of deniedHosts.
-}
validateHost :: HttpConfig -> Text -> Either Text ()
validateHost config hostVal
    | normHost hostVal `elem` map normHost (httpDeniedHosts config) =
        Left $ "Host is in denied list: " <> hostVal
    | otherwise = case httpMode config of
        LuaHttpDenylist -> Right ()
        LuaHttpAllowlist
            | null (httpAllowedHosts config) -> Left "No hosts allowed (empty whitelist)"
            | normHost hostVal `elem` map normHost (httpAllowedHosts config) -> Right ()
            | otherwise -> Left $ "Host not in allowed list: " <> hostVal

-- | Lowercase, and drop IPv6 brackets and one trailing dot.
normHost :: Text -> Text
normHost h0 =
    let h1 = Text.toLower (Text.strip h0)
        h2 = fromMaybe h1 (Text.stripPrefix "[" h1 >>= Text.stripSuffix "]")
     in fromMaybe h2 (Text.stripSuffix "." h2)

{- | Check a URL before a request (or a redirect hop) is made.

Returns @Left (status, message)@ when the URL is refused: 400 for a URL with
no usable host, 403 for a host the config refuses.
-}
checkUrl :: HttpConfig -> Text -> IO (Either (Int, Text) ())
checkUrl config url =
    case extractHost url of
        Left err -> pure (Left (400, err))
        Right hostVal -> case validateHost config hostVal of
            Left err -> pure (Left (403, err))
            Right () -> checkResolved config hostVal

{- | Refuse a host that resolves to an IP-literal entry of deniedHosts.

Only resolves when deniedHosts has IP literals. Fails closed: a host that
cannot be resolved is refused, since it could not be checked.
-}
checkResolved :: HttpConfig -> Text -> IO (Either (Int, Text) ())
checkResolved config hostVal = do
    deniedAddrs <- concat <$> mapM numericAddrs (httpDeniedHosts config)
    if null deniedAddrs
        then pure (Right ())
        else do
            r <- try (NS.getAddrInfo (Just NS.defaultHints) (Just (Text.unpack (normHost hostVal))) Nothing)
            case r of
                Left (_ :: SomeException) ->
                    pure (Left (403, "Could not resolve host to check it against the denied list: " <> hostVal))
                Right infos ->
                    let addrs = mapMaybe (addrKey . NS.addrAddress) infos
                     in if any (`elem` deniedAddrs) addrs
                            then pure (Left (403, "Host resolves to a denied address: " <> hostVal))
                            else pure (Right ())

-- | The addresses of a deniedHosts entry if it is an IP literal, else [].
numericAddrs :: Text -> IO [AddrKey]
numericAddrs entry = do
    let hints = NS.defaultHints{NS.addrFlags = [NS.AI_NUMERICHOST]}
    r <- try (NS.getAddrInfo (Just hints) (Just (Text.unpack (normHost entry))) Nothing)
    pure $ case r of
        Left (_ :: SomeException) -> []
        Right infos -> mapMaybe (addrKey . NS.addrAddress) infos

-- | An address without port, with IPv4-mapped IPv6 folded into IPv4.
data AddrKey = AddrV4 (Int, Int, Int, Int) | AddrV6 NS.HostAddress6
    deriving (Eq, Show)

addrKey :: NS.SockAddr -> Maybe AddrKey
addrKey (NS.SockAddrInet _ h) =
    let (a, b, c, d) = NS.hostAddressToTuple h
     in Just (AddrV4 (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d))
addrKey (NS.SockAddrInet6 _ _ h _) =
    case NS.hostAddress6ToTuple h of
        (0, 0, 0, 0, 0, 0xffff, hi, lo) ->
            Just (AddrV4 (fromIntegral (hi `shiftR` 8), fromIntegral (hi .&. 0xff), fromIntegral (lo `shiftR` 8), fromIntegral (lo .&. 0xff)))
        _ -> Just (AddrV6 h)
addrKey _ = Nothing

-- | HTTP GET request with host validation.
luaGet :: Tracer IO HttpTrace -> HttpConfig -> Manager -> Lua.LuaE Lua.Exception Lua.NumResults
luaGet tracer config manager = do
    top <- Lua.gettop
    let nargs = getStackInt top
    if nargs < 1
        then do
            Lua.pop (getStackInt top)
            Lua.pushnil
            Lua.pushstring "Usage: http.get(url, [options])"
            liftIO $ runTracer tracer (HttpGetTrace "" 400 (Just "Usage: http.get(url, [options])") Nothing)
            pure 2
        else do
            urlBs <- Lua.tostring' (Lua.nthTop 1)
            mOptions <-
                if nargs >= 2
                    then do
                        ltype <- Lua.ltype (Lua.nthTop 2)
                        if ltype == Lua.TypeTable
                            then parseOptions (Lua.nthTop 2)
                            else pure (Just defaultOptions)
                    else pure (Just defaultOptions)
            Lua.pop (getStackInt top)

            let url = Text.pack $ Text.unpack $ Text.decodeUtf8 urlBs

            -- Validate host
            checked <- liftIO $ checkUrl config url
            case checked of
                Left (code, err) -> do
                    liftIO $ runTracer tracer (HttpGetTrace url code (Just err) Nothing)
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right () -> do
                    let opts = fromMaybe defaultOptions mOptions
                    performRequest tracer config manager "GET" url opts (RequestBodyLBS LBS.empty)

{- | HTTP POST request with host validation.

Arguments on the stack (from bottom to top):
  nargs == 2: [url, body]
  nargs >= 3: [url, body, options, ...]

When accessing from the top using nthTop:
  - url is always at nthTop nargs (bottom of arguments)
  - body is always at nthTop (nargs - 1)
  - options (if present) is at nthTop 1
-}
luaPost :: Tracer IO HttpTrace -> HttpConfig -> Manager -> Lua.LuaE Lua.Exception Lua.NumResults
luaPost tracer config manager = do
    top <- Lua.gettop
    let nargs = getStackInt top
    if nargs < 2
        then do
            Lua.pop (getStackInt top)
            Lua.pushnil
            Lua.pushstring "Usage: http.post(url, body, [options])"
            liftIO $ runTracer tracer (HttpPostTrace "" 400 (Just "Usage: http.post(url, body, [options])") Nothing)
            pure 2
        else do
            -- url is at the bottom of the arguments, which is nthTop nargs
            urlBs <- Lua.tostring' (Lua.nthTop (fromIntegral nargs))
            -- body is second from bottom, which is nthTop (nargs - 1)
            bodyBs <- Lua.tostring' (Lua.nthTop (fromIntegral (nargs - 1)))
            mOptions <-
                if nargs >= 3
                    then do
                        -- options is at the top, which is nthTop 1
                        ltype <- Lua.ltype (Lua.nthTop 1)
                        if ltype == Lua.TypeTable
                            then parseOptions (Lua.nthTop 1)
                            else pure (Just defaultOptions)
                    else pure (Just defaultOptions)
            Lua.pop (getStackInt top)

            let url = Text.pack $ Text.unpack $ Text.decodeUtf8 urlBs

            -- Validate host
            checked <- liftIO $ checkUrl config url
            case checked of
                Left (code, err) -> do
                    liftIO $ runTracer tracer (HttpPostTrace url code (Just err) Nothing)
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right () -> do
                    let opts = fromMaybe defaultOptions mOptions
                    performRequest tracer config manager "POST" url opts (RequestBodyBS bodyBs)

-- | Generic HTTP request with host validation.
luaRequest :: Tracer IO HttpTrace -> HttpConfig -> Manager -> Lua.LuaE Lua.Exception Lua.NumResults
luaRequest tracer config manager = do
    top <- Lua.gettop
    let nargs = getStackInt top
    if nargs < 1
        then do
            Lua.pop (getStackInt top)
            Lua.pushnil
            Lua.pushstring "Usage: http.request(options)"
            liftIO $ runTracer tracer (HttpRequestTrace "" "" 400 (Just "Usage: http.request(options)") Nothing)
            pure 2
        else do
            let optsIdx = Lua.nthTop 1

            Lua.pushstring "method"
            _ <- Lua.gettable optsIdx
            mMethodBs <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop 1

            Lua.pushstring "url"
            _ <- Lua.gettable optsIdx
            urlBs <- Lua.tostring' (Lua.nthTop 1)
            Lua.pop 1

            Lua.pushstring "body"
            _ <- Lua.gettable optsIdx
            ltype <- Lua.ltype (Lua.nthTop 1)
            mBodyBs <-
                if ltype == Lua.TypeString
                    then do
                        bs <- Lua.tostring' (Lua.nthTop 1)
                        pure (Just bs)
                    else pure Nothing
            Lua.pop 1

            mOptions <- parseOptions optsIdx

            Lua.pop (getStackInt top)

            let url = Text.pack $ Text.unpack $ Text.decodeUtf8 urlBs
            let reqMethod = if BS.null mMethodBs then "GET" else Text.decodeUtf8 mMethodBs

            -- Validate host
            checked <- liftIO $ checkUrl config url
            case checked of
                Left (code, err) -> do
                    liftIO $ runTracer tracer (HttpRequestTrace reqMethod url code (Just err) Nothing)
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right () -> do
                    let opts = fromMaybe defaultOptions mOptions
                    let reqBody = case mBodyBs of
                            Just bs -> RequestBodyBS bs
                            Nothing -> RequestBodyLBS LBS.empty
                    performRequest tracer config manager (Text.unpack reqMethod) url opts reqBody

-- | Request options.
data RequestOptions = RequestOptions
    { optHeaders :: [(ByteString, ByteString)]
    }
    deriving (Show, Eq)

defaultOptions :: RequestOptions
defaultOptions = RequestOptions []

{- | Parse options table from Lua.

This function extracts HTTP request options from a Lua table.
Currently supports parsing the "headers" key which should contain
a table of header name-value pairs.

Returns 'Nothing' if the value at the given index is not a table.
Returns 'Just RequestOptions' with parsed headers otherwise.

Note: This function does NOT pop the input table from the stack.
The caller is responsible for stack cleanup.
-}
parseOptions :: Lua.StackIndex -> Lua.LuaE Lua.Exception (Maybe RequestOptions)
parseOptions idx = do
    ltype <- Lua.ltype idx
    if ltype /= Lua.TypeTable
        then pure Nothing
        else do
            -- Convert to absolute index immediately, before any stack modifications
            absIdx <- Lua.absindex idx
            -- Try to get the "headers" field directly
            Lua.pushstring "headers"
            _ <- Lua.gettable absIdx
            -- Check if the result is a table
            headersType <- Lua.ltype (Lua.nthTop 1)
            headers <-
                if headersType == Lua.TypeTable
                    then do
                        -- Parse the headers table
                        hdrs <- parseHeadersTable (Lua.nthTop 1)
                        Lua.pop 1 -- pop the headers table
                        pure hdrs
                    else do
                        Lua.pop 1 -- pop the non-table value (likely nil)
                        pure []
            pure $ Just $ RequestOptions headers

{- | Parse a headers table into a list of key-value pairs.
The headers table is at the given index (relative to current top).
This function does NOT pop the headers table.
-}
parseHeadersTable :: Lua.StackIndex -> Lua.LuaE Lua.Exception [(ByteString, ByteString)]
parseHeadersTable _idx = do
    -- Use Lua.next to iterate over the table.
    -- Push nil as the first key.
    Lua.pushnil
    go []
  where
    -- Note: We use nthTop 2 to reference the table because:
    -- - After pushing nil, the table is at nthTop 2
    -- - The nil key is at nthTop 1
    tableIdx = Lua.nthTop 2

    go :: [(ByteString, ByteString)] -> Lua.LuaE Lua.Exception [(ByteString, ByteString)]
    go acc = do
        hasNext <- Lua.next tableIdx
        if not hasNext
            then do
                -- Lua.next popped the nil when returning False
                pure acc
            else do
                -- After successful Lua.next:
                -- - The table is at nthTop 3 (below key and value)
                -- - The key is at nthTop 2
                -- - The value is at nthTop 1 (top of stack)
                keyBs <- Lua.tostring' (Lua.nthTop 2)
                valBs <- Lua.tostring' (Lua.nthTop 1)
                Lua.pop 1 -- pop value, keep key for next iteration
                go ((keyBs, valBs) : acc)

{- | Perform HTTP request.

Redirects are not left to http-client: each 3xx with a Location is turned
into the next request ('getRedirectedRequest', so methods and headers change
as http-client would change them), and that hop's URL goes through
'checkUrl' before it is sent. A refused hop ends the call with an error.
-}
performRequest :: Tracer IO HttpTrace -> HttpConfig -> Manager -> String -> Text -> RequestOptions -> RequestBody -> Lua.LuaE Lua.Exception Lua.NumResults
performRequest tracer config manager reqMethod url opts reqBody = do
    result <- liftIO $ try $ do
        initReq <- parseRequest (Text.unpack url)
        let req =
                initReq
                    { method = Text.encodeUtf8 $ Text.pack reqMethod
                    , requestBody = reqBody
                    , requestHeaders = map (\(k, v) -> (mk k, v)) (optHeaders opts)
                    , redirectCount = 0
                    }
        followRedirects config manager maxRedirects req

    case result of
        Left (e :: SomeException) -> do
            let errMsg = Text.pack $ show e
            liftIO $ traceOutcome 0 (Just errMsg) Nothing
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 errMsg)
            pure 2
        Right (Left (code, errMsg)) -> do
            liftIO $ traceOutcome code (Just errMsg) Nothing
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 errMsg)
            pure 2
        Right (Right response) -> do
            let status = statusCode $ responseStatus response
            let bodySize = Just $ fromIntegral $ LBS.length $ responseBody response
            liftIO $ traceOutcome status Nothing bodySize
            pushResponse response
            pure 1
  where
    traceOutcome code mErr mSize = case reqMethod of
        "GET" -> runTracer tracer (HttpGetTrace url code mErr mSize)
        "POST" -> runTracer tracer (HttpPostTrace url code mErr mSize)
        _ -> runTracer tracer (HttpRequestTrace (Text.pack reqMethod) url code mErr mSize)

{- | Send a request with redirects disabled, and follow 3xx responses by hand,
checking each hop's URL first. @Left (status, message)@ when a hop is refused
or there are too many redirects.
-}
followRedirects :: HttpConfig -> Manager -> Int -> Request -> IO (Either (Int, Text) (Response LBS.ByteString))
followRedirects config manager hopsLeft req = do
    response <- httpLbs req manager
    let code = statusCode (responseStatus response)
    case getRedirectedRequest req req (responseHeaders response) (responseCookieJar response) code of
        Nothing -> pure (Right response)
        Just next
            | hopsLeft <= 0 -> pure (Left (400, "Too many redirects (more than " <> Text.pack (show maxRedirects) <> ")"))
            | otherwise -> do
                let nextUrl = Text.pack (show (getUri next))
                checked <- checkUrl config nextUrl
                case checked of
                    Left (c, err) -> pure (Left (c, "Redirect to " <> nextUrl <> " refused: " <> err))
                    Right () -> followRedirects config manager (hopsLeft - 1) next{redirectCount = 0}

-- | Push response as Lua table.
pushResponse :: Response LBS.ByteString -> Lua.LuaE Lua.Exception ()
pushResponse response = do
    Lua.newtable

    Lua.pushstring "status"
    Lua.pushinteger (fromIntegral $ statusCode $ responseStatus response)
    Lua.settable (Lua.nthTop 3)

    Lua.pushstring "headers"
    Lua.newtable
    mapM_
        ( \(name, val) -> do
            Lua.pushstring (Text.encodeUtf8 $ Text.toLower $ Text.pack $ show name)
            Lua.pushstring val
            Lua.settable (Lua.nthTop 3)
        )
        (responseHeaders response)
    Lua.settable (Lua.nthTop 3)

    Lua.pushstring "body"
    Lua.pushstring (LBS.toStrict $ responseBody response)
    Lua.settable (Lua.nthTop 3)
