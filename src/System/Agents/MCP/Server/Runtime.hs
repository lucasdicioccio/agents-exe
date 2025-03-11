{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Agents.MCP.Server.Runtime where

import Conduit (ConduitT, Flush (..), Void, (.|))
import qualified Conduit as C
import qualified Control.Concurrent.Async as Async
import Control.Monad.Logger (LoggingT (..), MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit.TMChan
import Data.Foldable (for_)
import qualified Data.List as List
import qualified Network.JSONRPC as Rpc
import UnliftIO (Async, IORef, MonadIO, MonadUnliftIO, async, atomicModifyIORef, atomically, cancel, liftIO, newIORef, readIORef, wait, withAsync)

import qualified System.Agents.CLI.Prompt as Prompt
import System.Agents.MCP.Base as Mcp

data Runtime = Runtime
    { agentInfo :: Prompt.AgentInfo
    , actions :: IORef [(Rpc.Request, Async ())]
    }

initRuntime :: Prompt.AgentInfo -> IO Runtime
initRuntime ai =
    Runtime ai <$> newIORef []

-- returns another async waiting and removing the first one from the list of pending requests
addAsync :: Runtime -> Rpc.Request -> Async () -> IO (Async ())
addAsync rt req a = do
    atomicModifyIORef rt.actions insert
    async $ do
        _ <- Async.waitCatch a
        atomicModifyIORef rt.actions remove
  where
    insert :: [(Rpc.Request, Async ())] -> ([(Rpc.Request, Async ())], ())
    insert xs = (((req, a) : xs), ())

    remove :: [(Rpc.Request, Async ())] -> ([(Rpc.Request, Async ())], ())
    remove xs = ([x | x@(r, _) <- xs, r.getReqId /= r.getReqId], ())

cancellAsync :: Runtime -> Mcp.RequestId -> IO ()
cancellAsync rt reqId = do
    -- todo: modify so that the list is an STVar, the we cancel first and atomically wait and remove
    found <- atomicModifyIORef rt.actions remove
    for_ found $ \a -> cancel a
  where
    remove :: [(Rpc.Request, Async ())] -> ([(Rpc.Request, Async ())], (Maybe (Async ())))
    remove xs =
        ( [x | x@(r, _) <- xs, not $ matchingIDs r.getReqId reqId]
        , fmap snd (List.find (\(r, _) -> matchingIDs r.getReqId reqId) xs)
        )

    matchingIDs :: Rpc.Id -> Mcp.RequestId -> Bool
    matchingIDs (Rpc.IdInt n) (Mcp.IntRequestId m) = n == m
    matchingIDs (Rpc.IdTxt n) (Mcp.TextRequestId m) = n == m
    matchingIDs _ _ = False

lookupRequestAsync :: Runtime -> Rpc.Id -> IO (Maybe (Rpc.Request, Async ()))
lookupRequestAsync rt i = do
    List.find (\(r, _) -> r.getReqId == i) <$> readIORef rt.actions

newtype McpStack a = McpStack {runMcpStack :: ReaderT Runtime (LoggingT IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadUnliftIO)

instance MonadReader Runtime McpStack where
    ask = McpStack ask
    local f (McpStack a) = McpStack (local f a)

askRuntime :: Rpc.JSONRPCT McpStack Runtime
askRuntime = lift ask

askAgentInfo :: Rpc.JSONRPCT McpStack Prompt.AgentInfo
askAgentInfo = agentInfo <$> lift ask

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

encodeConduit :: (Aeson.ToJSON j, MonadLogger m) => ConduitT j (Flush ByteString) m ()
encodeConduit = do
    C.awaitForever $ \m -> do
        C.yield $ Chunk . L8.toStrict $ Aeson.encode m
        C.yield $ Chunk "\n"
        C.yield Flush
