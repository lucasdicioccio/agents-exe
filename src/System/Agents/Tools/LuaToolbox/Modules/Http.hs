{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | HTTP client module for LuaToolbox with enhanced host whitelisting.

This module provides HTTP request capabilities that are restricted to
specific allowed hosts. Key security features:

* Proper URI parsing for host extraction
* Host whitelist validation
* Secure default: empty allowedHosts means NO network access
* Standard HTTP methods: GET, POST, and generic request

Host validation extracts the host from the URL and validates it against
the whitelist before making any requests.
-}
module System.Agents.Tools.LuaToolbox.Modules.Http (
    HttpConfig (..),
    HttpTrace (..),
    RequestOptions (..),
    registerHttpModule,
    extractHost,
    validateHost,
    parseOptions,
) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Maybe (fromMaybe)
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
    httpLbs,
    parseRequest,
 )
import qualified Network.HTTP.Client.TLS as HTTPS
import Network.HTTP.Types (statusCode)
import Network.URI (URI (..), parseURI, uriAuthority, uriRegName)
import Prod.Tracer (Tracer (..), runTracer)

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

{- | HTTP module configuration.

Security defaults:
* Empty allowedHosts means NO network access
* All hosts must be explicitly whitelisted

Example:
@
HttpConfig
    { httpAllowedHosts = ["localhost", "127.0.0.1", "api.example.com"]
    }
@
-}
data HttpConfig = HttpConfig
    { httpAllowedHosts :: [Text]
    -- ^ Whitelist of allowed hosts. If empty, NO hosts are allowed (secure default).
    }
    deriving (Show, Eq)

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

{- | Validate host against whitelist.

An empty whitelist means no hosts are allowed (secure default).
Returns Left with error message if host is not allowed.
-}
validateHost :: HttpConfig -> Text -> Either Text ()
validateHost config hostVal
    | null (httpAllowedHosts config) = Left "No hosts allowed (empty whitelist)"
    | hostVal `elem` httpAllowedHosts config = Right ()
    | otherwise = Left $ "Host not in allowed list: " <> hostVal

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
            case extractHost url of
                Left err -> do
                    liftIO $ runTracer tracer (HttpGetTrace url 400 (Just err) Nothing)
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right hostVal ->
                    case validateHost config hostVal of
                        Left err -> do
                            liftIO $ runTracer tracer (HttpGetTrace url 403 (Just err) Nothing)
                            Lua.pushnil
                            Lua.pushstring (Text.encodeUtf8 err)
                            pure 2
                        Right () -> do
                            let opts = fromMaybe defaultOptions mOptions
                            performRequest tracer manager "GET" url opts (RequestBodyLBS LBS.empty)

-- | HTTP POST request with host validation.
-- 
-- Arguments on the stack (from bottom to top):
--   nargs == 2: [url, body]
--   nargs >= 3: [url, body, options, ...]
-- 
-- When accessing from the top using nthTop:
--   - url is always at nthTop nargs (bottom of arguments)
--   - body is always at nthTop (nargs - 1)
--   - options (if present) is at nthTop 1
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
            case extractHost url of
                Left err -> do
                    liftIO $ runTracer tracer (HttpPostTrace url 400 (Just err) Nothing)
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right hostVal ->
                    case validateHost config hostVal of
                        Left err -> do
                            liftIO $ runTracer tracer (HttpPostTrace url 403 (Just err) Nothing)
                            Lua.pushnil
                            Lua.pushstring (Text.encodeUtf8 err)
                            pure 2
                        Right () -> do
                            let opts = fromMaybe defaultOptions mOptions
                            performRequest tracer manager "POST" url opts (RequestBodyBS bodyBs)

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
            case extractHost url of
                Left err -> do
                    liftIO $ runTracer tracer (HttpRequestTrace reqMethod url 400 (Just err) Nothing)
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right hostVal ->
                    case validateHost config hostVal of
                        Left err -> do
                            liftIO $ runTracer tracer (HttpRequestTrace reqMethod url 403 (Just err) Nothing)
                            Lua.pushnil
                            Lua.pushstring (Text.encodeUtf8 err)
                            pure 2
                        Right () -> do
                            let opts = fromMaybe defaultOptions mOptions
                            let reqBody = case mBodyBs of
                                    Just bs -> RequestBodyBS bs
                                    Nothing -> RequestBodyLBS LBS.empty
                            performRequest tracer manager (Text.unpack reqMethod) url opts reqBody

-- | Request options.
data RequestOptions = RequestOptions
    { optHeaders :: [(ByteString, ByteString)]
    }
    deriving (Show, Eq)

defaultOptions :: RequestOptions
defaultOptions = RequestOptions []

-- | Parse options table from Lua.
--
-- This function extracts HTTP request options from a Lua table.
-- Currently supports parsing the "headers" key which should contain
-- a table of header name-value pairs.
--
-- Returns 'Nothing' if the value at the given index is not a table.
-- Returns 'Just RequestOptions' with parsed headers otherwise.
parseOptions :: Lua.StackIndex -> Lua.LuaE Lua.Exception (Maybe RequestOptions)
parseOptions idx = do
    ltype <- Lua.ltype idx
    if ltype /= Lua.TypeTable
        then pure Nothing
        else do
            -- Convert to absolute index before we start modifying the stack
            absIdx <- Lua.absindex idx
            -- Iterate through the table to find the "headers" key
            headers <- findAndParseHeaders absIdx
            pure $ Just $ RequestOptions headers
  where
    -- Find and parse the headers subtable from the options table.
    -- The tableIdx is an ABSOLUTE index, so it remains valid as we push/pop.
    findAndParseHeaders :: Lua.StackIndex -> Lua.LuaE Lua.Exception [(ByteString, ByteString)]
    findAndParseHeaders tableIdx = do
        -- Push nil to start iteration
        Lua.pushnil
        go []
      where
        go :: [(ByteString, ByteString)] -> Lua.LuaE Lua.Exception [(ByteString, ByteString)]
        go acc = do
            -- tableIdx is absolute, so it's stable regardless of stack changes
            hasNext <- Lua.next tableIdx
            if not hasNext
                then do
                    -- Pop the nil key that was pushed before iteration started
                    Lua.pop 1
                    pure acc
                else do
                    -- Stack: ..., key, value
                    keyBs <- Lua.tostring' (Lua.nthTop 2)
                    if keyBs == "headers"
                        then do
                            -- Check if value is a table
                            valType <- Lua.ltype (Lua.nthTop 1)
                            if valType == Lua.TypeTable
                                then do
                                    -- Parse the headers table
                                    -- nthTop 1 is the headers table value
                                    hdrs <- parseHeadersTable (Lua.nthTop 1)
                                    -- Pop the value (keep key for next iteration)
                                    Lua.pop 1
                                    go (hdrs ++ acc)
                                else do
                                    -- Not a table, skip
                                    Lua.pop 1
                                    go acc
                        else do
                            -- Not the headers key, skip
                            Lua.pop 1
                            go acc

    -- Parse a headers table (key-value pairs of header names and values).
    -- The headersIdx is a relative index (nthTop), valid at the time of call.
    parseHeadersTable :: Lua.StackIndex -> Lua.LuaE Lua.Exception [(ByteString, ByteString)]
    parseHeadersTable headersIdx = do
        -- Convert to absolute index before pushing nil
        absHdrIdx <- Lua.absindex headersIdx
        Lua.pushnil
        go absHdrIdx []
      where
        go :: Lua.StackIndex -> [(ByteString, ByteString)] -> Lua.LuaE Lua.Exception [(ByteString, ByteString)]
        go absHdrIdx acc = do
            hasNext <- Lua.next absHdrIdx
            if not hasNext
                then do
                    -- Pop the nil key that was pushed before iteration started
                    Lua.pop 1
                    pure acc
                else do
                    -- Stack: ..., key, value
                    keyBs <- Lua.tostring' (Lua.nthTop 2)
                    valBs <- Lua.tostring' (Lua.nthTop 1)
                    Lua.pop 1
                    go absHdrIdx ((keyBs, valBs) : acc)

-- | Perform HTTP request.
performRequest :: Tracer IO HttpTrace -> Manager -> String -> Text -> RequestOptions -> RequestBody -> Lua.LuaE Lua.Exception Lua.NumResults
performRequest tracer manager reqMethod url opts reqBody = do
    result <- liftIO $ try $ do
        initReq <- parseRequest (Text.unpack url)
        let req =
                initReq
                    { method = Text.encodeUtf8 $ Text.pack reqMethod
                    , requestBody = reqBody
                    , requestHeaders = map (\(k, v) -> (mk k, v)) (optHeaders opts)
                    }
        httpLbs req manager

    case result of
        Left (e :: SomeException) -> do
            let errMsg = Text.pack $ show e
            liftIO $ case reqMethod of
                "GET" -> runTracer tracer (HttpGetTrace url 0 (Just errMsg) Nothing)
                "POST" -> runTracer tracer (HttpPostTrace url 0 (Just errMsg) Nothing)
                _ -> runTracer tracer (HttpRequestTrace (Text.pack reqMethod) url 0 (Just errMsg) Nothing)
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 errMsg)
            pure 2
        Right response -> do
            let status = statusCode $ responseStatus response
            let bodySize = Just $ fromIntegral $ LBS.length $ responseBody response
            liftIO $ case reqMethod of
                "GET" -> runTracer tracer (HttpGetTrace url status Nothing bodySize)
                "POST" -> runTracer tracer (HttpPostTrace url status Nothing bodySize)
                _ -> runTracer tracer (HttpRequestTrace (Text.pack reqMethod) url status Nothing bodySize)
            pushResponse response
            pure 1

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

