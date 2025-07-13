{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Agents.MCP.Client where

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT (..), MonadLogger, MonadLoggerIO, logDebugN, runStderrLoggingT)
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import Data.Conduit.TMChan
import Data.Text (Text)
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
    | NotifyInitializedMsg Mcp.InitializedNotification
    | ListToolsRequestMsg Mcp.ListToolsRequest
    | CallToolRequestMsg Mcp.CallToolRequest
    deriving (Show)

newtype InitializeResultRsp = InitializeResultRsp {getInitializeResult :: Mcp.InitializeResult}
    deriving (Show, Aeson.FromJSON)

newtype ListToolsResultRsp = ListToolsResultRsp {getListToolsResult :: Mcp.ListToolsResult}
    deriving (Show, Aeson.FromJSON)

newtype CallToolResultRsp = CallToolResultRsp {getCallToolResult :: Mcp.CallToolResult}
    deriving (Show, Aeson.FromJSON)

instance Rpc.ToRequest ClientMsg where
    requestMethod (InitializeMsg _) = "initialize"
    requestMethod (NotifyInitializedMsg _) = "notifications/initialized"
    requestMethod (ListToolsRequestMsg _) = "tools/list"
    requestMethod (CallToolRequestMsg _) = "tools/call"
    requestIsNotif (InitializeMsg _) = False
    requestIsNotif (NotifyInitializedMsg _) = True
    requestIsNotif (ListToolsRequestMsg _) = False
    requestIsNotif (CallToolRequestMsg _) = False

instance Rpc.FromResponse InitializeResultRsp where
    parseResult "initialize" =
        Just Aeson.parseJSON
    parseResult _ = Nothing

instance Rpc.FromResponse ListToolsResultRsp where
    parseResult "tools/list" =
        Just Aeson.parseJSON
    parseResult _ = Nothing

instance Rpc.FromResponse CallToolResultRsp where
    parseResult "tools/call" =
        Just Aeson.parseJSON
    parseResult _ = Nothing

instance Aeson.ToJSON ClientMsg where
    toJSON (InitializeMsg msg) = Aeson.toJSON msg
    toJSON (NotifyInitializedMsg _) = Aeson.toJSON (Aeson.object [])
    toJSON (ListToolsRequestMsg msg) = Aeson.toJSON msg
    toJSON (CallToolRequestMsg msg) = Aeson.toJSON msg

-------------------------------------------------------------------------------
clientProtocolVersion :: Text
clientProtocolVersion = "2024-11-05"

clientImplem :: Mcp.Implementation
clientImplem = Implementation "agents-exe-mcp-client" "0.0.1"

clientCapabilities :: Mcp.ClientCapabilities
clientCapabilities =
    Mcp.ClientCapabilities
        (Just mempty)
        (Just $ Aeson.object [])
        []

-------------------------------------------------------------------------------

handlerLoop :: Rpc.JSONRPCT McpStack ()
handlerLoop = do
    srv <- initialize
    case srv of
        (Just (Right srv)) -> do
            notifyInitialized
            loop srv
        _ -> do
            pure ()
  where
    initialize :: Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj InitializeResultRsp))
    initialize =
        Rpc.sendRequest $
            InitializeMsg $
                Mcp.InitializeRequest
                    clientProtocolVersion
                    clientCapabilities
                    clientImplem

    notifyInitialized :: Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj ()))
    notifyInitialized =
        Rpc.sendRequest $
            NotifyInitializedMsg $
                Mcp.InitializedNotification

    listTools :: Maybe Mcp.Cursor -> Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj ListToolsResultRsp))
    listTools cursor =
        Rpc.sendRequest $
            ListToolsRequestMsg $
                Mcp.ListToolsRequest
                    cursor

    callTool :: Mcp.Name -> Maybe Aeson.Object -> Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj CallToolResultRsp))
    callTool name arg =
        Rpc.sendRequest $
            CallToolRequestMsg $
                Mcp.CallToolRequest
                    name
                    arg

    loop :: InitializeResultRsp -> Rpc.JSONRPCT McpStack ()
    loop srv = do
        -- todo: need to cycle between requests and responses
        debugString "hello"
        listTools Nothing >>= liftIO . print
        callTool "ask_boss-openai_000" (Just ("prompt" Aeson..= ("hi" :: Text))) >>= liftIO . print
        liftIO $ threadDelay 10000000
        loop srv
