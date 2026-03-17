{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Agents.MCP.Client where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT (..), MonadLogger, MonadLoggerIO, logDebugN)
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import Data.Conduit.TMChan
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer, runTracer)
import UnliftIO (MonadIO, MonadUnliftIO, async, liftIO, withAsync)

import System.Agents.MCP.Base as Mcp
import System.Agents.MCP.Client.Runtime

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

data ClientTrace
    = JsonRpcLog Loc LogSource LogLevel LogStr
    deriving (Show)

runClient ::
    Tracer IO ClientTrace ->
    Runtime ->
    (ClientInfos -> Rpc.JSONRPCT McpStack ()) ->
    IO ()
runClient ctracer rt act = do
    runLoggingT
        (runReaderT (runMcpStack scheduleMcp) rt)
        logInTracer
  where
    logInTracer loc src lvl str =
        runTracer ctracer (JsonRpcLog loc src lvl str)
    scheduleMcp = do
        runJSONRPCT'
            Rpc.V2
            False
            (sinkTBMChan rt.reqChan)
            (sourceTBMChan rt.rspChan)
            (handleClient act)

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

data ServerMsg
    = NotifyToolListChanged Mcp.ToolListChangedNotification
    deriving (Show)

instance Rpc.FromRequest ServerMsg where
    parseParams "notifications/tools/list_changed" =
        Just (fmap NotifyToolListChanged <$> Aeson.parseJSON)
    parseParams _ =
        Nothing

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

data ClientInfos
    = ClientInfos
    { initializeResult :: InitializeResultRsp
    }

handleClient ::
    (ClientInfos -> Rpc.JSONRPCT McpStack ()) -> Rpc.JSONRPCT McpStack ()
handleClient act = do
    liftIO $ threadDelay 1000000
    initSrv <- initialize
    case initSrv of
        (Just (Right srv)) -> do
            x <- act (ClientInfos srv)
            _ <- notifyInitialized
            pure x
        _ -> do
            pure ()
  where
    -- primitives
    initialize :: Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj InitializeResultRsp))
    initialize =
        sendRequest' $
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

-------------------------------------------------------------------------------
listTools ::
    Maybe Mcp.Cursor ->
    Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj ListToolsResultRsp))
listTools ecursor =
    Rpc.sendRequest $
        ListToolsRequestMsg $
            Mcp.ListToolsRequest
                ecursor

callTool ::
    Mcp.Name ->
    Maybe Aeson.Object ->
    Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj CallToolResultRsp))
callTool tname arg =
    Rpc.sendRequest $
        CallToolRequestMsg $
            Mcp.CallToolRequest
                tname
                arg

enumerateTools ::
    Rpc.JSONRPCT McpStack ([Maybe (Either Rpc.ErrorObj ListToolsResultRsp)])
enumerateTools = do
    item <- listTools Nothing
    go [item] (previewCursor item)
  where
    previewCursor :: Maybe (Either Rpc.ErrorObj ListToolsResultRsp) -> Maybe Mcp.Cursor
    previewCursor (Just (Right rsp)) = rsp.getListToolsResult.nextCursor
    previewCursor _ = Nothing

    go xs Nothing = pure xs
    go xs ecursor@(Just _) = do
        item <- listTools ecursor
        go (item : xs) (previewCursor item)

-------------------------------------------------------------------------------
data ToolCall
    = ToolCall Mcp.Name (Maybe Aeson.Object)

type ToolCallResponse = Maybe (Either Rpc.ErrorObj CallToolResultRsp)

data FullToolCall
    = FullToolCall ToolCall (ToolCallResponse -> IO ())

data LoopTrace
    = StartToolCall Mcp.Name (Maybe Aeson.Object)
    | EndToolCall Mcp.Name (Maybe Aeson.Object) (Maybe (Either Rpc.ErrorObj CallToolResultRsp))
    | ToolsRefreshed [Maybe (Either Rpc.ErrorObj ListToolsResultRsp)]
    | ExitingToolCallLoop
    deriving (Show)

data LoopProps = LoopProps
    { tracer :: Tracer IO LoopTrace
    , waitToolCall :: IO (Maybe FullToolCall)
    }

defaultLoop :: LoopProps -> ClientInfos -> Rpc.JSONRPCT McpStack ()
defaultLoop props clientInfos = do
    withAsync loopToolCalls $ \_ -> do
        if hasToolsChangedNotif
            then do
                doRefreshTools
                loopEnumerateTools_Notif
            else do
                doRefreshTools
                loopEnumerateTools_Poll
  where
    waitToolChangeNotification :: Rpc.JSONRPCT McpStack Bool
    waitToolChangeNotification = do
        mreq <- Rpc.receiveRequest
        case mreq of
            Nothing -> do
                debugString "no request received"
                pure False
            Just req -> do
                msg <- handleReq req
                case msg of
                    Just (NotifyToolListChanged _) -> pure True
                    _ -> pure False
      where
        handleReq :: Rpc.Request -> Rpc.JSONRPCT McpStack (Maybe ServerMsg)
        handleReq req = do
            debugShow req
            let emsg = Rpc.fromRequest req :: Either Rpc.ErrorObj ServerMsg
            case emsg of
                (Left err) -> debugShow err >> pure Nothing
                (Right msg) -> pure $ Just msg

    hasToolsChangedNotif :: Bool
    hasToolsChangedNotif =
        Mcp.ToolsListChanged `elem` clientInfos.initializeResult.getInitializeResult.capabilities.flags

    doRefreshTools :: Rpc.JSONRPCT McpStack ()
    doRefreshTools = do
        enumerateTools >>= liftIO . runTracer props.tracer . ToolsRefreshed

    loopEnumerateTools_Notif :: Rpc.JSONRPCT McpStack ()
    loopEnumerateTools_Notif = do
        changed <- waitToolChangeNotification
        when changed $ do
            doRefreshTools
        loopEnumerateTools_Notif

    loopEnumerateTools_Poll :: Rpc.JSONRPCT McpStack ()
    loopEnumerateTools_Poll = do
        doRefreshTools
        liftIO (threadDelay 30000000)
        loopEnumerateTools_Poll

    loopToolCalls :: Rpc.JSONRPCT McpStack ()
    loopToolCalls = do
        tc <- liftIO props.waitToolCall
        case tc of
            Nothing -> do
                liftIO $ runTracer props.tracer ExitingToolCallLoop
            Just (FullToolCall (ToolCall tname obj) resp) -> do
                liftIO $ runTracer props.tracer (StartToolCall tname obj)
                _ <- async $ do
                    r <- callTool tname obj
                    liftIO $ do
                        runTracer props.tracer (EndToolCall tname obj r)
                        resp r
                loopToolCalls

