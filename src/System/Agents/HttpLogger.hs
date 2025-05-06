{-# LANGUAGE DataKinds #-}

module System.Agents.HttpLogger where

import Data.Aeson (ToJSON (..))
import qualified Data.ByteString.Lazy as LByteString
import qualified Network.HTTP.Client as HttpClient
import Prod.Tracer (Tracer (..))

import qualified System.Agents.HttpClient as AgentsHttpClient

data Runtime
    = Runtime
    { clientRuntime :: AgentsHttpClient.Runtime
    , endpoint :: AgentsHttpClient.Url "log-sink"
    }

-------------------------------------------------------------------------------
httpLog :: (ToJSON val) => Runtime -> Tracer IO AgentsHttpClient.Trace -> val -> IO (Either AgentsHttpClient.ScoopError (HttpClient.Response LByteString.ByteString))
httpLog rt clientTracer obj =
    rt.clientRuntime.post clientTracer rt.endpoint (Just $ toJSON obj)

httpTracer :: (ToJSON val) => Runtime -> Tracer IO AgentsHttpClient.Trace -> Tracer IO val
httpTracer rt clientTracer =
    Tracer $ \x -> httpLog rt clientTracer x >> pure ()
