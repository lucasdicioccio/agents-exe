module System.Agents.MCP.Client.Runtime where

import Conduit (ConduitT, Flush, Void, (.|))
import qualified Conduit as C
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.Conduit.TMChan
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer, runTracer)
import System.Exit (ExitCode)
import System.Process (CreateProcess)
import UnliftIO (Async, MonadUnliftIO, async, atomically, liftIO, wait, withAsync)

data RunTrace
    = RunCommandStart !CreateProcess
    | RunCommandStopped !CreateProcess !ExitCode
    deriving (Show)

data Runtime = Runtime
    { processAsync :: Async (ExitCode)
    , reqChan :: TBMChan (Flush ByteString)
    , rspChan :: TBMChan ByteString
    }

initRuntime :: Tracer IO RunTrace -> CreateProcess -> IO (Runtime)
initRuntime tracer proc = do
    outChan <- newTBMChanIO 1024 :: IO (TBMChan ByteString)
    inChan <- newTBMChanIO 1024 :: IO (TBMChan (Flush ByteString))
    let process :: IO ExitCode
        process = do
            runTracer tracer (RunCommandStart proc)
            (code, _, _) <-
                sourceProcessWithStreams
                    proc
                    (sourceTBMChan inChan .| discardFlush)
                    (sinkTBMChan outChan)
                    C.sinkNull -- todo: extract errorstream
            runTracer tracer (RunCommandStopped proc code)
            pure code
    a <- async $ process
    pure $ Runtime a inChan outChan

discardFlush :: (Monad m) => ConduitT (Flush a) a m ()
discardFlush = do
    C.awaitForever $ \c -> do
        case c of
            C.Flush -> pure ()
            (C.Chunk buf) -> C.yield buf

-- A modified runJSONRPCT that allows to flush and add a carriage return after
-- every full json payload (the original code only allows bytestrings).
runJSONRPCT' ::
    (MonadLoggerIO m, MonadUnliftIO m) =>
    -- | JSON-RPC version
    Rpc.Ver ->
    -- | Ignore incoming requests/notifs
    Bool ->
    -- | Sink to send messages
    ConduitT (Flush ByteString) Void m () ->
    -- | Source to receive messages from
    ConduitT () ByteString m () ->
    -- | JSON-RPC action
    Rpc.JSONRPCT m a ->
    -- | Output of action
    m a
runJSONRPCT' ver ignore snk src f = do
    qs <- liftIO . atomically $ Rpc.initSession ver ignore
    let inSnk = sinkTBMChan (Rpc.inCh qs)
        outSrc = sourceTBMChan (Rpc.outCh qs)
    withAsync ((C.runConduit $ src .| Rpc.decodeConduit ver .| inSnk) >> liftIO (atomically $ closeTBMChan $ Rpc.inCh qs)) $
        const $
            withAsync (C.runConduit $ outSrc .| encodeConduit .| snk) $ \o ->
                withAsync (runReaderT Rpc.processIncoming qs) $ const $ do
                    a <- runReaderT f qs
                    liftIO $ do
                        atomically . closeTBMChan $ Rpc.outCh qs
                        _ <- wait o
                        return a

encodeConduit :: (Aeson.ToJSON j, Monad m) => ConduitT j (Flush ByteString) m ()
encodeConduit = do
    C.awaitForever $ \m -> do
        C.yield $ C.Chunk . L8.toStrict $ Aeson.encode m
        C.yield $ C.Chunk "\n"
        C.yield C.Flush
