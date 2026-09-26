{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Agents.MCP.Client where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, when)
import Control.Monad.Logger (Loc, LogLevel, LogSource, LogStr, LoggingT (..), MonadLogger, MonadLoggerIO, logDebugN)
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Conduit.TMChan
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer, runTracer)
import UnliftIO (MonadIO, MonadUnliftIO, TVar, async, atomically, liftIO, modifyTVar', newTVarIO, readTVar, readTVarIO, withAsync)

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
    | -- | A tool call, with the progress token (if any) advertised in @_meta@.
      CallToolRequestMsg Mcp.CallToolRequest (Maybe Mcp.ProgressToken)
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
    requestMethod (CallToolRequestMsg _ _) = "tools/call"
    requestIsNotif (InitializeMsg _) = False
    requestIsNotif (NotifyInitializedMsg _) = True
    requestIsNotif (ListToolsRequestMsg _) = False
    requestIsNotif (CallToolRequestMsg _ _) = False

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
    toJSON (CallToolRequestMsg msg mtoken) =
        case (Aeson.toJSON msg, mtoken) of
            (Aeson.Object o, Just token) ->
                Aeson.Object $
                    KeyMap.insert "_meta" (Aeson.object ["progressToken" Aeson..= token]) o
            (v, _) -> v

data ServerMsg
    = NotifyToolListChanged Mcp.ToolListChangedNotification
    | NotifyProgress Mcp.ProgressNotification
    deriving (Show)

instance Rpc.FromRequest ServerMsg where
    parseParams "notifications/tools/list_changed" =
        Just (fmap NotifyToolListChanged <$> Aeson.parseJSON)
    parseParams "notifications/progress" =
        Just (fmap NotifyProgress <$> Aeson.parseJSON)
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
    Maybe Mcp.ProgressToken ->
    Rpc.JSONRPCT McpStack (Maybe (Either Rpc.ErrorObj CallToolResultRsp))
callTool tname arg mtoken =
    Rpc.sendRequest $
        CallToolRequestMsg
            ( Mcp.CallToolRequest
                tname
                arg
            )
            mtoken

-- | Receives a call's progress notifications, as JSON (see 'progressPayload').
type ProgressCallback = Aeson.Value -> IO ()

{- | The JSON reported to a 'ProgressCallback' for a @notifications/progress@:
@progress@, plus @total@ and @message@ when the server sent them.
-}
progressPayload :: Mcp.ProgressNotification -> Aeson.Value
progressPayload n =
    Aeson.object $
        ["progress" Aeson..= n.progress]
            <> maybe [] (\t -> ["total" Aeson..= t]) n.total
            <> maybe [] (\m -> ["message" Aeson..= m]) n.message

-- | Progress callbacks of the calls in flight, by the token advertised to the server.
type ProgressRegistry = TVar (Map.Map Mcp.ProgressToken ProgressCallback)

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
data McpToolCall
    = McpToolCall Mcp.Name (Maybe Aeson.Object)

type ToolCallResponse = Maybe (Either Rpc.ErrorObj CallToolResultRsp)

data FullToolCall
    = FullToolCall McpToolCall (Maybe ProgressCallback) (ToolCallResponse -> IO ())

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
    registry <- liftIO (newTVarIO Map.empty)
    counter <- liftIO (newTVarIO (0 :: Int))
    withAsync (loopToolCalls registry counter) $ \_ -> do
        doRefreshTools
        if hasToolsChangedNotif
            then loopServerMessages registry
            else withAsync (loopServerMessages registry) $ \_ -> loopEnumerateTools_Poll
  where
    -- Reads what the server sends us: tool-list changes (when the server
    -- announces them) and progress notifications. Ends when the connection
    -- closes.
    loopServerMessages :: ProgressRegistry -> Rpc.JSONRPCT McpStack ()
    loopServerMessages registry = do
        mreq <- Rpc.receiveRequest
        case mreq of
            Nothing -> debugString "no request received"
            Just req -> do
                debugShow req
                case Rpc.fromRequest req :: Either Rpc.ErrorObj ServerMsg of
                    Left err -> debugShow err
                    Right (NotifyToolListChanged _) -> when hasToolsChangedNotif doRefreshTools
                    Right (NotifyProgress n) -> do
                        callbacks <- liftIO (readTVarIO registry)
                        liftIO $ mapM_ ($ progressPayload n) (Map.lookup n.progressToken callbacks)
                loopServerMessages registry

    hasToolsChangedNotif :: Bool
    hasToolsChangedNotif =
        Mcp.ToolsListChanged `elem` clientInfos.initializeResult.getInitializeResult.capabilities.flags

    doRefreshTools :: Rpc.JSONRPCT McpStack ()
    doRefreshTools = do
        enumerateTools >>= liftIO . runTracer props.tracer . ToolsRefreshed

    loopEnumerateTools_Poll :: Rpc.JSONRPCT McpStack ()
    loopEnumerateTools_Poll = do
        liftIO (threadDelay 30000000)
        doRefreshTools
        loopEnumerateTools_Poll

    loopToolCalls :: ProgressRegistry -> TVar Int -> Rpc.JSONRPCT McpStack ()
    loopToolCalls registry counter = do
        tc <- liftIO props.waitToolCall
        case tc of
            Nothing -> do
                liftIO $ runTracer props.tracer ExitingToolCallLoop
            Just (FullToolCall (McpToolCall tname obj) mprogress resp) -> do
                liftIO $ runTracer props.tracer (StartToolCall tname obj)
                -- Only calls that want progress advertise a token.
                mtoken <- liftIO $ forM mprogress $ \cb -> atomically $ do
                    modifyTVar' counter (+ 1)
                    n <- readTVar counter
                    let token = Mcp.TextProgressToken ("agents-exe-" <> Text.pack (show n))
                    modifyTVar' registry (Map.insert token cb)
                    pure token
                _ <- async $ do
                    r <- callTool tname obj mtoken
                    liftIO $ do
                        forM_ mtoken $ \token -> atomically (modifyTVar' registry (Map.delete token))
                        runTracer props.tracer (EndToolCall tname obj r)
                        resp r
                loopToolCalls registry counter
