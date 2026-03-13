{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | HTTP client module for LuaToolbox.
-}
module System.Agents.Tools.LuaToolbox.Modules.Http (
    HttpConfig (..),
    registerHttpModule,
) where

import qualified Data.ByteString as BS
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.CaseInsensitive (mk)
import qualified HsLua as Lua
import Network.HTTP.Client (
    Manager,
    Request (..),
    RequestBody (RequestBodyLBS, RequestBodyBS),
    Response (..),
    httpLbs,
    parseRequest,
 )
import qualified Network.HTTP.Client.TLS as HTTPS
import Network.HTTP.Types (Status, statusCode)
import Foreign.C.Types (CInt(..))

stackIdxToInt :: Lua.StackIndex -> Int
stackIdxToInt (Lua.StackIndex n) = fromIntegral n

getStackInt :: Lua.StackIndex -> Int
getStackInt = stackIdxToInt

-- | HTTP module configuration.
data HttpConfig = HttpConfig
    { httpAllowedHosts :: [Text]
    }
    deriving (Show, Eq)

-- | Register the http module.
registerHttpModule :: Lua.State -> HttpConfig -> IO ()
registerHttpModule lstate config = do
    manager <- HTTPS.newTlsManager
    Lua.runWith lstate $ do
        Lua.newtable

        Lua.pushName "get"
        Lua.pushHaskellFunction (luaGet config manager)
        Lua.settable (Lua.nthTop 3)

        Lua.pushName "post"
        Lua.pushHaskellFunction (luaPost config manager)
        Lua.settable (Lua.nthTop 3)

        Lua.pushName "request"
        Lua.pushHaskellFunction (luaRequest config manager)
        Lua.settable (Lua.nthTop 3)

        Lua.setglobal (Lua.Name "http")

-- | HTTP GET request.
luaGet :: HttpConfig -> Manager -> Lua.LuaE Lua.Exception Lua.NumResults
luaGet config manager = do
    top <- Lua.gettop
    let nargs = getStackInt top
    if nargs < 1
        then do
            Lua.pop (getStackInt top)
            Lua.pushnil
            Lua.pushstring "Usage: http.get(url, [options])"
            pure 2
        else do
            urlBs <- Lua.tostring' (Lua.nthTop 1)
            mOptions <- if nargs >= 2
                then do
                    ltype <- Lua.ltype (Lua.nthTop 2)
                    if ltype == Lua.TypeTable
                        then parseOptions (Lua.nthTop 2)
                        else pure (Just defaultOptions)
                else pure (Just defaultOptions)
            Lua.pop (getStackInt top)
            
            let url = Text.unpack $ Text.decodeUtf8 urlBs
            case validateUrl config url of
                Left err -> do
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right validUrl -> do
                    let opts = fromMaybe defaultOptions mOptions
                    performRequest manager "GET" validUrl opts (RequestBodyLBS LBS.empty)

-- | HTTP POST request.
luaPost :: HttpConfig -> Manager -> Lua.LuaE Lua.Exception Lua.NumResults
luaPost config manager = do
    top <- Lua.gettop
    let nargs = getStackInt top
    if nargs < 2
        then do
            Lua.pop (getStackInt top)
            Lua.pushnil
            Lua.pushstring "Usage: http.post(url, body, [options])"
            pure 2
        else do
            urlBs <- Lua.tostring' (Lua.nthTop 2)
            bodyBs <- Lua.tostring' (Lua.nthTop 1)
            mOptions <- if nargs >= 3
                then do
                    ltype <- Lua.ltype (Lua.nthTop 3)
                    if ltype == Lua.TypeTable
                        then parseOptions (Lua.nthTop 3)
                        else pure (Just defaultOptions)
                else pure (Just defaultOptions)
            Lua.pop (getStackInt top)
            
            let url = Text.unpack $ Text.decodeUtf8 urlBs
            case validateUrl config url of
                Left err -> do
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right validUrl -> do
                    let opts = fromMaybe defaultOptions mOptions
                    performRequest manager "POST" validUrl opts (RequestBodyBS bodyBs)

-- | Generic HTTP request.
luaRequest :: HttpConfig -> Manager -> Lua.LuaE Lua.Exception Lua.NumResults
luaRequest config manager = do
    top <- Lua.gettop
    let nargs = getStackInt top
    if nargs < 1
        then do
            Lua.pop (getStackInt top)
            Lua.pushnil
            Lua.pushstring "Usage: http.request(options)"
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
            mBodyBs <- if ltype == Lua.TypeString
                then do
                    bs <- Lua.tostring' (Lua.nthTop 1)
                    pure (Just bs)
                else pure Nothing
            Lua.pop 1
            
            mOptions <- parseOptions optsIdx
            
            Lua.pop (getStackInt top)
            
            let url = Text.unpack $ Text.decodeUtf8 urlBs
            case validateUrl config url of
                Left err -> do
                    Lua.pushnil
                    Lua.pushstring (Text.encodeUtf8 err)
                    pure 2
                Right validUrl -> do
                    let opts = fromMaybe defaultOptions mOptions
                    let reqMethod = if BS.null mMethodBs then "GET" else Text.decodeUtf8 mMethodBs
                    let reqBody = case mBodyBs of
                            Just bs -> RequestBodyBS bs
                            Nothing -> RequestBodyLBS LBS.empty
                    performRequest manager (Text.unpack reqMethod) validUrl opts reqBody

-- | Request options.
data RequestOptions = RequestOptions
    { optHeaders :: [(ByteString, ByteString)]
    }

defaultOptions :: RequestOptions
defaultOptions = RequestOptions []

-- | Parse options table.
parseOptions :: Lua.StackIndex -> Lua.LuaE Lua.Exception (Maybe RequestOptions)
parseOptions idx = do
    ltype <- Lua.ltype idx
    if ltype /= Lua.TypeTable
        then pure Nothing
        else do
            Lua.pushnil
            headers <- parseHeaders idx []
            pure $ Just $ RequestOptions headers
  where
    parseHeaders tableIdx acc = do
        let offset = fromIntegral (stackIdxToInt tableIdx + 1) :: CInt
        hasNext <- Lua.next (Lua.nthTop offset)
        if not hasNext
            then pure acc
            else do
                keyBs <- Lua.tostring' (Lua.nthTop 2)
                Lua.pop 1
                if keyBs == "headers"
                    then do
                        ltype <- Lua.ltype (Lua.nthTop 1)
                        if ltype == Lua.TypeTable
                            then do
                                hdrs <- parseHeadersTable (Lua.nthTop 1)
                                parseHeaders tableIdx (hdrs ++ acc)
                            else parseHeaders tableIdx acc
                    else parseHeaders tableIdx acc

    parseHeadersTable idx' = do
        Lua.pushnil
        go []
      where
        go acc = do
            let offset = fromIntegral (stackIdxToInt idx' + 1) :: CInt
            hasNext <- Lua.next (Lua.nthTop offset)
            if not hasNext
                then pure acc
                else do
                    keyBs <- Lua.tostring' (Lua.nthTop 2)
                    valBs <- Lua.tostring' (Lua.nthTop 1)
                    Lua.pop 1
                    go ((keyBs, valBs) : acc)

-- | Validate URL.
validateUrl :: HttpConfig -> String -> Either Text String
validateUrl config url
    | null (httpAllowedHosts config) = Left "No hosts allowed"
    | otherwise = 
        case extractHost url of
            Nothing -> Left "Could not extract host from URL"
            Just host -> 
                if host `elem` httpAllowedHosts config
                    then Right url
                    else Left $ "Host not allowed: " <> host

-- | Extract host from URL.
extractHost :: String -> Maybe Text
extractHost url = 
    let url' = if "://" `Text.isInfixOf` Text.pack url
               then drop 1 $ dropWhile (/= '/') $ dropWhile (/= ':') $ dropWhile (/= '/') url
               else url
        hostPort = takeWhile (/= '/') url'
        host = takeWhile (/= ':') hostPort
    in if null host then Nothing else Just (Text.pack host)

-- | Perform HTTP request.
performRequest :: Manager -> String -> String -> RequestOptions -> RequestBody -> Lua.LuaE Lua.Exception Lua.NumResults
performRequest manager reqMethod url opts reqBody = do
    result <- liftIO $ try $ do
        initReq <- parseRequest url
        let req = initReq
                { method = Text.encodeUtf8 $ Text.pack reqMethod
                , requestBody = reqBody
                , requestHeaders = map (\(k, v) -> (mk k, v)) (optHeaders opts)
                }
        httpLbs req manager
    
    case result of
        Left (e :: SomeException) -> do
            Lua.pushnil
            Lua.pushstring (Text.encodeUtf8 $ Text.pack $ show e)
            pure 2
        Right response -> do
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
    mapM_ (\(name, val) -> do
        Lua.pushstring (Text.encodeUtf8 $ Text.toLower $ Text.pack $ show name)
        Lua.pushstring val
        Lua.settable (Lua.nthTop 3)
        ) (responseHeaders response)
    Lua.settable (Lua.nthTop 3)
    
    Lua.pushstring "body"
    Lua.pushstring (LBS.toStrict $ responseBody response)
    Lua.settable (Lua.nthTop 3)

