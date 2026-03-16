{-# LANGUAGE TemplateHaskell #-}
module System.Agents.MCP.Client.Runtime where

import Conduit (ConduitT, Flush, Void, (.|))
import qualified Conduit as C
import Control.Monad.Logger (MonadLoggerIO, logDebugS)
import Control.Monad.Reader (runReaderT, reader)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.Conduit.TMChan
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer, runTracer)
import System.Exit (ExitCode)
import System.Process (CreateProcess)
import UnliftIO (Async, MonadUnliftIO, async, atomically, liftIO, wait, withAsync, readTVar, writeTVar, newEmptyTMVar, takeTMVar)

import Control.Monad (unless, forM, liftM)
import qualified Data.HashMap.Strict        as M

data BufferStream
    = In
    | Out
    | Err
    deriving (Show)

data RunTrace
    = RunCommandStart !CreateProcess
    | RunBufferMoved !BufferStream !ByteString
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
                    (sourceTBMChan inChan .| discardFlush .| dupToTraces tracer In)
                    (dupToTraces tracer Out .| sinkTBMChan outChan)
                    (dupToTraces tracer Err .| C.sinkNull)
            runTracer tracer (RunCommandStopped proc code)
            pure code
    a <- async $ process
    pure $ Runtime a inChan outChan

dupToTraces :: Tracer IO RunTrace -> BufferStream -> ConduitT ByteString ByteString IO ()
dupToTraces tracer s = do
    C.awaitForever $ \buf -> do
        liftIO $ runTracer tracer (RunBufferMoved s buf)
        C.yield buf

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

-- | Returns Nothing if did not receive response, could not parse it, or
-- request is a notification. Just Left contains the error object returned
-- by server if any. Just Right means response was received just right.
sendRequest' :: (MonadLoggerIO m , Aeson.ToJSON q, Rpc.ToRequest q, Rpc.FromResponse r)
            => q -> Rpc.JSONRPCT m (Maybe (Either Rpc.ErrorObj r))
sendRequest' q = do
    f `liftM` sendBatchRequest' [q]
  where
    f :: [Maybe (Either Rpc.ErrorObj r)] -> Maybe (Either Rpc.ErrorObj r)
    f [] = Nothing
    f (x:_) = x

-- | Send multiple requests in a batch. If only a single request, do not
-- put it in a batch.
sendBatchRequest' :: (MonadLoggerIO m, Aeson.ToJSON q, Rpc.ToRequest q, Rpc.FromResponse r)
                 => [q] -> Rpc.JSONRPCT m [Maybe (Either Rpc.ErrorObj r)]
sendBatchRequest' qs = do
    v <- reader Rpc.rpcVer
    l <- reader Rpc.lastId
    s <- reader Rpc.sentReqs
    o <- reader Rpc.outCh
    k <- reader Rpc.dead
    aps <- liftIO . atomically $ do
        d <- readTVar k
        aps <- forM qs $ \q ->
            if Rpc.requestIsNotif q
                then return (Rpc.buildRequest v q undefined, Nothing)
                else do
                    p <- newEmptyTMVar
                    i <- succ <$> readTVar l
                    m <- readTVar s
                    unless d $ writeTVar s $ M.insert i p m
                    unless d $ writeTVar l i
                    if d
                        then return (Rpc.buildRequest v q i, Nothing)
                        else return (Rpc.buildRequest v q i, Just p)
        case map fst aps of
            []  -> return ()
            [a] -> unless d $ writeTBMChan o $ Rpc.MsgRequest a
            as  -> unless d $ writeTBMChan o $ Rpc.MsgBatch $ map Rpc.MsgRequest as
        return aps
    if null aps
        then $logDebugS "json-rpc" "no responses pending"
        else $logDebugS "json-rpc" "listening for responses if pending"
    (ret,_) <- fmap unzip . liftIO . atomically $ forM aps $ \(a, pM) ->
        case pM of
            Nothing -> do
                return (Nothing, "branch-1" :: String)
            Just  p -> do
                rM <- takeTMVar p
                case rM of
                    Nothing -> return (Nothing, "branch-2")
                    Just r@Rpc.Response{} ->
                        case Rpc.fromResponse (Rpc.getReqMethod a) r of
                            Nothing -> return (Nothing, "branch-3")
                            Just  x -> return (Just $ Right x, "branch-4")
                    Just e -> return (Just $ Left $ Rpc.getError e, "branch-5")
    pure ret

