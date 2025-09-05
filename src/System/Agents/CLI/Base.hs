{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.CLI.Base where

import Control.Concurrent.QSem
import Control.Exception (bracket_)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Char8 as C8
import Prod.Tracer (Tracer (..), contramap)
import qualified Data.Aeson as Aeson

makeShowLogFileTracer :: forall t. (Show t) => FilePath -> IO (Tracer IO t)
makeShowLogFileTracer path = do
    contramap (\item -> C8.pack (show item <> "\n")) <$> makeLogFileTracer path

makeLogFileTracer :: FilePath -> IO (Tracer IO C8.ByteString)
makeLogFileTracer path = do
    traceLogFile <$> newQSem 1
  where
    traceLogFile :: QSem -> Tracer IO C8.ByteString
    traceLogFile q =
        Tracer $ \item -> bracket_ (waitQSem q) (signalQSem q) $ do
            ByteString.appendFile path item

-- | A tracer that appends JSON-encoded objects as JSON lines to a file, thread-safe.
makeFileJsonTracer :: forall a. (Aeson.ToJSON a) => FilePath -> IO (Tracer IO a)
makeFileJsonTracer path = do
    qsem <- newQSem 1
    let tracer = Tracer $ \item -> bracket_ (waitQSem qsem) (signalQSem qsem) $ do
            let bs = Aeson.encode item
            -- Append the JSON line plus a newline
            ByteString.appendFile path (LByteString.toStrict bs)
            ByteString.appendFile path (C8.pack "\n")
    pure tracer

