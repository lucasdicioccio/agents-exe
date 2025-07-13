{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Agents.MCP.Client where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT (..), MonadLogger, MonadLoggerIO, logDebugN, runStderrLoggingT)
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import Data.Conduit.TMChan
import qualified Data.Text as Text
import qualified Network.JSONRPC as Rpc
import UnliftIO (Async, MonadIO, MonadUnliftIO, async, atomically, cancel, liftIO, wait, withAsync)

import System.Agents.MCP.Base as Mcp
import System.Agents.MCP.Client.Runtime

data Trace
    = ProcessLevelTrace Loc LogSource LogLevel LogStr
    deriving (Show)

newtype McpStack a
    = McpStack {runMcpStack :: ReaderT Runtime (LoggingT IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadUnliftIO)

instance MonadReader Runtime McpStack where
    ask = McpStack ask
    local f (McpStack a) = McpStack (local f a)

askRuntime :: Rpc.JSONRPCT McpStack Runtime
askRuntime = lift ask

debugString :: String -> Rpc.JSONRPCT McpStack ()
debugString = logDebugN . Text.pack

debugShow :: (Show a) => a -> Rpc.JSONRPCT McpStack ()
debugShow = debugString . show

toto :: Runtime -> IO ()
toto rt = do
    runStderrLoggingT $
        runReaderT (runMcpStack scheduleMcp) rt
  where
    scheduleMcp = do
        runJSONRPCT'
            Rpc.V2
            False
            (sinkTBMChan rt.reqChan)
            (sourceTBMChan rt.rspChan)
            handlerLoop

data ClientMsg
    = InitializeMsg Mcp.InitializeRequest
    deriving (Show)

newtype InitializeResultRsp = InitializeResultRsp {getInitializeResult :: Mcp.InitializeResult}
    deriving (Show, Aeson.FromJSON)

instance Rpc.ToRequest ClientMsg where
    requestMethod _ = "initialize"
    requestIsNotif _ = False

instance Rpc.FromResponse InitializeResultRsp where
    parseResult "initialize" =
        Just Aeson.parseJSON
    parseResult _ = Nothing

instance Aeson.ToJSON ClientMsg where
    toJSON (InitializeMsg msg) = Aeson.toJSON msg

handlerLoop :: Rpc.JSONRPCT McpStack ()
handlerLoop = do
    srv <- initialize
    case srv of
        (Just (Right srv)) -> loop srv
  where
    initialize :: Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj InitializeResultRsp))
    initialize =
        Rpc.sendRequest $
            InitializeMsg $
                Mcp.InitializeRequest
                    "2024-11-05"
                    ( Mcp.ClientCapabilities
                        Nothing
                        Nothing
                        []
                    )
                    (Mcp.Implementation "agents-exe-mcp-client" "0.0.1")

    loop :: InitializeResultRsp -> Rpc.JSONRPCT McpStack ()
    loop srv = do
        -- todo: need to cycle between requests and responses
        debugString "hello"
        liftIO $ threadDelay 1000000
        loop srv
