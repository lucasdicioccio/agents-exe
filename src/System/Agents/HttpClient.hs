{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.HttpClient where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.CaseInsensitive (CI)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Base (Symbol)
import Network.HTTP.Client as HttpClient
import Network.HTTP.Client.TLS as TlsHttpClient
import Network.HTTP.Types as HttpTypes
import Prod.Tracer (Tracer, runTracer)

-------------------------------------------------------------------------------
data Trace
    = RunRequest
        (HttpClient.Request)
        (HttpClient.Response LByteString.ByteString)
    deriving (Show)

-------------------------------------------------------------------------------
data ScoopError
    = SomeError String
    deriving (Show)

data Runtime = Runtime
    { get :: Tracer IO Trace -> Text -> IO (Either ScoopError (HttpClient.Response LByteString.ByteString))
    , post :: Tracer IO Trace -> Text -> Maybe Aeson.Value -> IO (Either ScoopError (HttpClient.Response LByteString.ByteString))
    }

data Token
    = NoToken
    | BasicToken Text
    | BearerToken Text

newRuntime :: Token -> IO Runtime
newRuntime token =
    httpsClientApi token <$> manager
  where
    manager :: IO HttpClient.Manager
    manager = do
        let setts =
                TlsHttpClient.tlsManagerSettings
                    { HttpClient.managerResponseTimeout =
                        HttpClient.responseTimeoutMicro 120_000_000
                    }
        TlsHttpClient.newTlsManagerWith setts

httpsClientApi :: Token -> HttpClient.Manager -> Runtime
httpsClientApi token manager =
    Runtime getF postF
  where
    getF tracer u = do
        let modHeaders = addHeaders defaultHeaders . addBasicTokenAuth token
        req <- modHeaders <$> HttpClient.parseRequest (Text.unpack u)
        rsp <- HttpClient.httpLbs req manager
        runTracer tracer $ RunRequest req rsp
        pure $ Right rsp
    postF tracer u mbodyVal = do
        let modHeaders = (\r -> r{HttpClient.method = "POST"}) . addHeaders defaultHeaders . addBasicTokenAuth token
        let modbody = maybe id (\val -> \r -> r{HttpClient.requestBody = HttpClient.RequestBodyLBS (Aeson.encode val)}) mbodyVal
        req <- modbody . modHeaders <$> HttpClient.parseRequest (Text.unpack u)
        rsp <- HttpClient.httpLbs req manager
        runTracer tracer $ RunRequest req rsp
        pure $ Right rsp
    addBasicTokenAuth NoToken = id
    addBasicTokenAuth (BasicToken tokval) =
        addHeaders
            [ ("Authorization", Text.encodeUtf8 tokval)
            ]
    addBasicTokenAuth (BearerToken tokval) =
        addHeaders
            [ ("Authorization", "Bearer " <> Text.encodeUtf8 tokval)
            ]
    defaultHeaders =
        [ ("User-Agent", "haskell-http-client+agents-exe")
        , ("Content-Type", "application/json")
        ]
    addHeaders extra req =
        let currentHeaders = HttpClient.requestHeaders req
         in req{HttpClient.requestHeaders = currentHeaders <> extra}

type ScoopIO a = Runtime -> IO (Either ScoopError a)

runScoopIO :: Runtime -> ScoopIO a -> IO (Either ScoopError a)
runScoopIO api run = run api

-------------------------------------------------------------------------------
type Url (a :: Symbol) = Text

type RawResponse = HttpClient.Response LByteString.ByteString
data J a = J {raw :: RawResponse}
    deriving (Show)

decodeBody :: (Aeson.FromJSON a) => J a -> Maybe a
decodeBody j = Aeson.decode $ HttpClient.responseBody j.raw

readHeaderWith :: CI ByteString.ByteString -> (ByteString.ByteString -> Maybe o) -> J a -> Maybe o
readHeaderWith hdr f j = do
    val <- lookup hdr (HttpClient.responseHeaders j.raw)
    f val

responseHeaders :: HttpClient.Response body -> HttpTypes.ResponseHeaders
responseHeaders = HttpClient.responseHeaders
