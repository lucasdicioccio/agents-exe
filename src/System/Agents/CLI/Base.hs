{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.CLI.Base where

import Control.Concurrent.QSem
import Control.Exception (bracket_)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8
import Prod.Tracer (Tracer (..), contramap)

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
