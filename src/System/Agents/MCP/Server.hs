{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module System.Agents.MCP.Server where

import Conduit (stdinC)
import Control.Monad (void)
import Control.Monad.Logger (LoggingT (..), defaultOutput, logDebugN)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import Data.Conduit.Combinators (sinkHandleFlush)
import Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Formatting ((%))
import qualified Formatting as Format
import qualified Network.JSONRPC as Rpc
import UnliftIO (async, liftIO, stderr, stdout)

import qualified System.Agents.AgentTree as AgentTree
import System.Agents.Base (Agent (..), newConversationId)
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.MCP.Base as Mcp
import qualified System.Agents.Runtime as Runtime

import System.Agents.MCP.Server.Runtime

mainAgentServer :: AgentTree.Props -> IO ()
mainAgentServer props = do
    multiAgentsServer [props]

multiAgentsServer :: [AgentTree.Props] -> IO ()
multiAgentsServer xs = multiAgentsServer' 0 xs []

multiAgentsServer' :: Int -> [AgentTree.Props] -> MappedTools -> IO ()
multiAgentsServer' _ [] [] = do
    print ("no agent definitions" :: Text)
multiAgentsServer' _ [] mtools = do
    rt <- initRuntime mtools
    runLoggingT (runReaderT (runMcpStack mainMcp) rt) logTrace
  where
    logTrace =
        defaultOutput stderr
multiAgentsServer' idx (props : xs) mtools = do
    AgentTree.withAgentTreeRuntime props go
  where
    go (AgentTree.Initialized ai) = do
        let oai = ai.agentBase
        let toolname = Format.format ("ask_" % Format.text % "_" % Format.left 3 '0') (LText.fromStrict oai.slug) idx
        let tool = ExpertAgentAsPrompt (LText.toStrict toolname) ai
        multiAgentsServer' (succ idx) xs (tool : mtools)
    go _ = do
        print ("failed to initialize" :: Text)

mainMcp :: McpStack ()
mainMcp = do
    runJSONRPCT'
        Rpc.V2
        False
        (sinkHandleFlush stdout)
        stdinC
        handlerLoop

debugString :: String -> Rpc.JSONRPCT McpStack ()
debugString = logDebugN . Text.pack

debugShow :: (Show a) => a -> Rpc.JSONRPCT McpStack ()
debugShow = debugString . show

-------------------------------------------------------------------------------
serverProtocolVersion :: Text
serverProtocolVersion = "2024-11-05"

serverImplem :: Mcp.Implementation
serverImplem = Implementation "agents-exe-mcp-server" "0.0.1"

serverCapabilities :: Mcp.ServerCapabilities
serverCapabilities =
    Mcp.ServerCapabilities
        (Just mempty)
        (Just $ Aeson.object [])
        [ToolsListChanged, PromptsListChanged, ResourcesListChanged]

-------------------------------------------------------------------------------
data ClientMsg
    = InitializeMsg Mcp.InitializeRequest
    | NotifyInitializedMsg Mcp.InitializedNotification
    | ListResourcesRequestMsg Mcp.ListResourcesRequest
    | ListToolsRequestMsg Mcp.ListToolsRequest
    | ListPromptsRequestMsg Mcp.ListPromptsRequest
    | CallToolRequestMsg Mcp.CallToolRequest
    | CancelledNotificationMsg Mcp.CancelledNotification
    deriving (Show)

instance Rpc.FromRequest ClientMsg where
    parseParams "initialize" =
        Just (fmap InitializeMsg <$> Aeson.parseJSON)
    parseParams "notifications/initialized" =
        Just (\_ -> pure (NotifyInitializedMsg Mcp.InitializedNotification))
    parseParams "notifications/cancelled" =
        Just (fmap CancelledNotificationMsg <$> Aeson.parseJSON)
    parseParams "resources/list" =
        Just (fmap ListResourcesRequestMsg <$> Aeson.parseJSON)
    parseParams "tools/list" =
        Just (fmap ListToolsRequestMsg <$> Aeson.parseJSON)
    parseParams "tools/call" =
        Just (fmap CallToolRequestMsg <$> Aeson.parseJSON)
    parseParams "prompts/list" =
        Just (fmap ListPromptsRequestMsg <$> Aeson.parseJSON)

-------------------------------------------------------------------------------
respond :: (Aeson.ToJSON val) => Rpc.Request -> val -> Rpc.Response
respond req obj =
    Rpc.Response (req.getReqVer) (Aeson.toJSON obj) (req.getReqId)

handlerLoop :: Rpc.JSONRPCT McpStack ()
handlerLoop = do
    loop
  where
    loop :: Rpc.JSONRPCT McpStack ()
    loop = do
        mreq <- Rpc.receiveRequest
        maybe (debugString "no request received") handleReq mreq
        loop

    handleReq :: Rpc.Request -> Rpc.JSONRPCT McpStack ()
    handleReq req = do
        debugShow req
        let emsg = Rpc.fromRequest req :: Either Rpc.ErrorObj ClientMsg
        case emsg of
            (Left err) -> debugShow err
            (Right msg) -> handleParsedReq req msg

    handleParsedReq :: Rpc.Request -> ClientMsg -> Rpc.JSONRPCT McpStack ()
    handleParsedReq req msg = do
        rt <- askRuntime
        a <- async $ handleMsg req msg
        void $ liftIO $ addAsync rt req a

handleMsg :: Rpc.Request -> ClientMsg -> Rpc.JSONRPCT McpStack ()
handleMsg _ (CancelledNotificationMsg c) = do
    rt <- askRuntime
    liftIO $ cancellAsync rt c.requestId
handleMsg req (InitializeMsg _) = do
    let rsp =
            respond
                req
                (Mcp.InitializeResult serverProtocolVersion serverCapabilities serverImplem (Just ""))
    Rpc.sendResponse rsp
handleMsg _ (NotifyInitializedMsg _) =
    pure ()
handleMsg req (ListResourcesRequestMsg _) = do
    let rsp =
            respond
                req
                (Mcp.ListResourcesResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg req (ListToolsRequestMsg _) = do
    toolset <- askMappedTools
    let rsp =
            respond
                req
                (Mcp.ListToolsResult (makeMappedTools toolset) Nothing)
    Rpc.sendResponse rsp
handleMsg req (ListPromptsRequestMsg _) = do
    let rsp =
            respond
                req
                (Mcp.ListPromptsResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg req (CallToolRequestMsg callTool) = do
    let conversationFunctions =
            Runtime.ConversationFunctions
                (pure Nothing)
                (\_hist -> pure ())
                (\err -> pure $ Left err)
                (\hist -> pure $ maybe (Left "no answer") Right $ OpenAI.lastAnswerMaybe hist)
    mappedTool <- askMappedTools
    res <- case lookupMappedTools mappedTool callTool.name of
        Just (ExpertAgentAsPrompt _ ai) -> do
            case extractPrompt callTool of
                Nothing -> pure $ Left "no prompt given"
                (Just query) -> do
                    liftIO $ do
                        cId <- newConversationId
                        Runtime.handleConversation ai.agentRuntime conversationFunctions cId query
        Nothing -> do
            pure $ Left $ Text.unpack $ "no matching tool for " <> callTool.name
    let rsp =
            respond
                req
                (Mcp.CallToolResult [toolCallContent res] Nothing)
    Rpc.sendResponse rsp

lookupMappedTools :: MappedTools -> Mcp.Name -> Maybe MappedTool
lookupMappedTools xs mcpName =
    List.find f xs
  where
    f :: MappedTool -> Bool
    f (ExpertAgentAsPrompt n _) = n == mcpName

-------------------------------------------------------------------------------

makeMappedTools :: MappedTools -> [Mcp.Tool]
makeMappedTools = Maybe.catMaybes . fmap adapt
  where
    adapt :: MappedTool -> Maybe Mcp.Tool
    adapt (ExpertAgentAsPrompt n ai) = callExpertTool n ai

callExpertTool :: Mcp.Name -> AgentTree.AgentTree -> Maybe Mcp.Tool
callExpertTool mcpName ai =
    let oai = ai.agentBase
     in Just $
            Mcp.Tool
                mcpName
                (Just oai.announce)
                ( Mcp.InputSchema
                    (Just ["prompt"])
                    ( Just $
                        Mcp.pairz
                            [ "prompt"
                                .= Mcp.object
                                    [ "type" .= ("string" :: Text)
                                    , "title" .= ("the prompt asked when calling the expert" :: Text)
                                    ]
                            ]
                    )
                )

extractPrompt :: Mcp.CallToolRequest -> Maybe Text
extractPrompt (Mcp.CallToolRequest _ Nothing) = Nothing
extractPrompt (Mcp.CallToolRequest _ (Just arg)) =
    Aeson.lookup "prompt" arg >>= f
  where
    f (Aeson.String txt) = Just txt
    f _ = Nothing

toolCallContent :: Either String OpenAI.Response -> Mcp.Content
toolCallContent (Left err) =
    Mcp.TextContent $ Mcp.TextContentImpl (Text.unwords ["got an error:", Text.pack err]) []
toolCallContent (Right rsp) =
    case rsp.rspContent of
        Nothing ->
            Mcp.TextContent $ Mcp.TextContentImpl ("got no anwser but it finished") []
        Just txt ->
            Mcp.TextContent $ Mcp.TextContentImpl (txt) []
