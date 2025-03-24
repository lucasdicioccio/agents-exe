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
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.JSONRPC as Rpc
import UnliftIO (async, liftIO, stderr, stdout)

import qualified System.Agents.CLI.Prompt as Prompt
import qualified System.Agents.FileLoader as FileLoader
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.MCP.Base as Mcp
import qualified System.Agents.OpenAI as Agent

import System.Agents.MCP.Server.Runtime

mainAgentServer :: Prompt.Props -> IO ()
mainAgentServer props = do
    Prompt.withAgentRuntime props go
  where
    go (Prompt.Initialized ai) = do
        rt <- initRuntime ai
        runLoggingT (runReaderT (runMcpStack mainMcp) rt) logTrace
    go _ =
        print ("failed to initialize" :: Text)
    logTrace =
        defaultOutput stderr

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
serverCapabilities = Mcp.ServerCapabilities (Just mempty) (Just $ Aeson.object []) [ToolsListChanged, PromptsListChanged, ResourcesListChanged]

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
    agent <- askAgentInfo
    let rsp =
            respond
                req
                (Mcp.ListToolsResult (Maybe.catMaybes [callExpertTool agent]) Nothing)
    Rpc.sendResponse rsp
handleMsg req (ListPromptsRequestMsg _) = do
    let rsp =
            respond
                req
                (Mcp.ListPromptsResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg req (CallToolRequestMsg callTool) = do
    let agentFunctions =
            Agent.AgentFunctions
                (pure Nothing)
                (\err -> pure $ Left err)
                (\hist -> pure $ maybe (Left "no answer") Right $ OpenAI.lastAnswerMaybe hist)
    ai <- askAgentInfo
    res <- case extractPrompt callTool of
        Nothing -> pure $ Left "no prompt given"
        (Just query) -> do
            liftIO $ Agent.openAIAgent ai.agentRuntime agentFunctions query
    let rsp =
            respond
                req
                (Mcp.CallToolResult [toolCallContent res] Nothing)
    Rpc.sendResponse rsp

-------------------------------------------------------------------------------

extractPrompt :: Mcp.CallToolRequest -> Maybe Text
extractPrompt (Mcp.CallToolRequest _ Nothing) = Nothing
extractPrompt (Mcp.CallToolRequest _ (Just arg)) =
    Aeson.lookup "prompt" arg >>= f
  where
    f (Aeson.String txt) = Just txt
    f _ = Nothing

callExpertTool :: Prompt.AgentInfo -> Maybe Mcp.Tool
callExpertTool ai =
    case ai.agentDescription of
        (FileLoader.Unspecified _) -> Nothing
        (FileLoader.OpenAIAgentDescription oai) ->
            Just $
                Mcp.Tool
                    ("ask_" <> oai.slug)
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

toolCallContent :: Either String OpenAI.Response -> Mcp.Content
toolCallContent (Left err) =
    Mcp.TextContent $ Mcp.TextContentImpl (Text.unwords ["got an error:", Text.pack err]) []
toolCallContent (Right rsp) =
    case rsp.rspContent of
        Nothing ->
            Mcp.TextContent $ Mcp.TextContentImpl ("got no anwser but it finished") []
        Just txt ->
            Mcp.TextContent $ Mcp.TextContentImpl (txt) []
