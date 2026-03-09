{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Direct MCP server implementation that exposes tools without agent indirection.
--
-- This module provides the ability to run an MCP server directly from tool sources
-- (bash tools, MCP toolboxes, OpenAPI specs, PostgREST specs) without requiring
-- an agent as an indirection layer.
--
-- Key differences from the agent-based MCP server in 'System.Agents.MCP.Server':
--
-- * No LLM calls - tools are executed directly without agent routing
-- * Lower overhead - no session management or agent runtime
-- * Simpler deployment - no LLM API keys needed for tool-only servers
-- * Direct tool execution - bypasses agent-level features like retries
--
-- Example usage:
--
-- @
-- import System.Agents.Tools.McpServer
--
-- main :: IO ()
-- main = do
--     let opts = McpToolboxOptions
--             { mcpToolSources =
--                 [ ToolSourceAgent "agent.json"
--                 , ToolSourceDirectory "./tools"
--                 ]
--             , mcpServerName = "my-tool-server"
--             , mcpServerVersion = "1.0.0"
--             , mcpTransport = StdioTransport
--             }
--     runMcpToolboxServer opts
-- @
module System.Agents.Tools.McpServer (
    -- * Configuration types
    McpToolboxOptions (..),
    ToolSource (..),
    McpTransport (..),
    NameMappingStrategy (..),

    -- * Main entry point
    runMcpToolboxServer,

    -- * Tool registry
    DirectToolRegistry (..),
    createDirectRegistry,

    -- * Tool conversion
    toolRegistrationsToMcpTools,
    toolRegistrationToMcpTool,
    stripLLMNamePrefix,
) where

import Conduit (stdinC)
import Control.Concurrent.Async (Async, waitCatch)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad (void, forM_)
import Control.Monad.Logger (LoggingT (..), MonadLogger, MonadLoggerIO, defaultOutput, runLoggingT)
import Control.Monad.Reader (MonadReader (..), ReaderT, ask, local, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.Conduit.Combinators (sinkHandleFlush)
import Data.IORef (IORef)
import Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import Formatting ((%))
import qualified Formatting as Format
import qualified Network.JSONRPC as Rpc
import Prod.Tracer (Tracer (..), contramap, silent)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr, stdout)
import qualified System.Process
import UnliftIO (MonadIO, MonadUnliftIO, async, cancel, liftIO, newIORef, wait, atomicModifyIORef)

import qualified System.Agents.Base as Base
import qualified System.Agents.MCP.Base as Mcp
import System.Agents.MCP.Server.Runtime (runJSONRPCT', encodeConduit)
import System.Agents.ToolRegistration (ToolRegistration (..))
import qualified System.Agents.ToolRegistration as ToolReg
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))
import qualified System.Agents.Tools.Bash as Bash
import qualified System.Agents.Tools.BashToolbox as BashToolbox
import qualified System.Agents.Tools.McpToolbox as McpTools
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import qualified System.Agents.Tools.PostgRESToolbox as PostgRESToolbox
import System.Agents.Tools.Base (CallResult (..), Tool (..))
import System.Agents.Tools.Context (ToolExecutionContext, mkToolExecutionContext, CallStackEntry (..))
import System.Agents.Tools.Trace (ToolTrace (..))
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.Session.Base as SessionBase
import System.Agents.Base (newConversationId, newStepId)

-------------------------------------------------------------------------------
-- Configuration Types
-------------------------------------------------------------------------------

-- | Options for running an MCP server directly from tool sources.
data McpToolboxOptions = McpToolboxOptions
    { mcpToolSources :: [ToolSource]
    -- ^ Sources to load tools from
    , mcpServerName :: Text
    -- ^ Server name for MCP protocol
    , mcpServerVersion :: Text
    -- ^ Server version for MCP protocol
    , mcpTransport :: McpTransport
    -- ^ Transport mechanism (stdio or HTTP)
    , mcpNameMapping :: NameMappingStrategy
    -- ^ How to handle LLM name prefixes when exposing as MCP tools
    } deriving (Show)

-- | Source of tools to load into the MCP server.
data ToolSource
    = ToolSourceAgent FilePath
    -- ^ Load tools from an agent's configuration and tool directories
    | ToolSourceDirectory FilePath
    -- ^ Load bash tools directly from a directory
    | ToolSourceMcpToolbox Text FilePath
    -- ^ Load tools from an MCP server config (name, config file)
    | ToolSourceOpenAPI FilePath
    -- ^ Load tools from an OpenAPI specification config
    | ToolSourcePostgREST FilePath
    -- ^ Load tools from a PostgREST specification config
    deriving (Show)

-- | Transport mechanism for the MCP server.
data McpTransport
    = StdioTransport
    -- ^ Use stdio for MCP protocol (standard for MCP servers)
    | HttpTransport Int
    -- ^ Use HTTP on the specified port (not yet implemented)
    deriving (Show)

-- | Strategy for mapping LLM-prefixed tool names to MCP tool names.
--
-- LLM-facing tool names have prefixes like @bash_@, @mcp_@, @openapi_@.
-- This controls how these are exposed as MCP tools.
data NameMappingStrategy
    = KeepPrefixedNames
    -- ^ Keep prefixes: @bash_hello@, @mcp_filesystem_read@
    | StripPrefixes
    -- ^ Strip prefixes: @hello@, @filesystem_read@
    | UseOriginalNames
    -- ^ Use original tool slugs where available
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Tool Registry
-------------------------------------------------------------------------------

-- | A tool registry that doesn't require an Agent runtime.
--
-- This is a simplified version of the agent's tool registration system
-- that allows direct tool execution without LLM session management.
data DirectToolRegistry = DirectToolRegistry
    { registryTools :: TVar [ToolRegistration]
    -- ^ Registered tools (mutable for refresh)
    , registryRefresh :: IO ()
    -- ^ Function to refresh tools from sources
    }

-- | Create a tool registry from a list of tool sources.
--
-- This loads tools from all sources and creates a registry that can
-- be used to serve MCP requests directly.
createDirectRegistry ::
    [ToolSource] ->
    NameMappingStrategy ->
    IO (Either Text DirectToolRegistry)
createDirectRegistry sources nameMapping = do
    -- Load tools from all sources
    results <- mapM loadToolsFromSource sources
    
    case sequence results of
        Left err -> pure $ Left err
        Right toolLists -> do
            let allTools = concat toolLists
            toolsVar <- newTVarIO allTools
            
            -- Create refresh function that reloads from all sources
            let refresh = do
                    newResults <- mapM loadToolsFromSource sources
                    case sequence newResults of
                        Left _ -> pure ()  -- Log error but keep old tools
                        Right newToolLists -> do
                            atomically $ writeTVar toolsVar (concat newToolLists)
            
            pure $ Right $ DirectToolRegistry
                { registryTools = toolsVar
                , registryRefresh = refresh
                }

-- | Load tools from a single source.
loadToolsFromSource :: ToolSource -> IO (Either Text [ToolRegistration])
loadToolsFromSource source = case source of
    ToolSourceAgent agentFile -> loadToolsFromAgent agentFile
    ToolSourceDirectory dir -> loadToolsFromDirectory dir
    ToolSourceMcpToolbox name configFile -> loadToolsFromMcpConfig name configFile
    ToolSourceOpenAPI configFile -> loadToolsFromOpenAPIConfig configFile
    ToolSourcePostgREST configFile -> loadToolsFromPostgRESTConfig configFile

-- | Load tools from an agent configuration file.
loadToolsFromAgent :: FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromAgent agentFile = do
    eAgent <- loadAgentFromFile agentFile
    case eAgent of
        Left err -> pure $ Left err
        Right agent -> do
            let agentDir = takeDirectory agentFile
            
            -- Load bash tools
            bashTools <- loadBashToolsFromDir (agentDir </> Base.toolDirectory agent)
            
            -- Load MCP tools
            mcpTools <- loadMcpToolsFromAgent agent
            
            -- Load OpenAPI tools
            openapiTools <- loadOpenAPIToolsFromAgent agent
            
            -- Load PostgREST tools
            postgrestTools <- loadPostgRESToolsFromAgent agent
            
            pure $ Right $ concat [bashTools, mcpTools, openapiTools, postgrestTools]

-- | Load bash tools from a directory.
loadToolsFromDirectory :: FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromDirectory dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure $ Left $ "Directory does not exist: " <> Text.pack dir
        else do
            tools <- loadBashToolsFromDir dir
            pure $ Right tools

-- | Load tools from an MCP server configuration.
loadToolsFromMcpConfig :: Text -> FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromMcpConfig name configFile = do
    exists <- doesFileExist configFile
    if not exists
        then pure $ Left $ "MCP config file does not exist: " <> Text.pack configFile
        else do
            eConfig <- Aeson.eitherDecodeFileStrict' configFile
            case eConfig of
                Left err -> pure $ Left $ "Failed to parse MCP config: " <> Text.pack err
                Right (Base.McpSimpleBinary cfg) -> do
                    -- Use pattern matching to avoid the 'executable' keyword issue
                    let exePath = Base.executable cfg
                        exeArgs = Base.args cfg
                    let proc = System.Process.proc exePath (map Text.unpack exeArgs)
                    let tracer = silent :: Tracer IO McpTools.Trace
                    
                    -- Initialize MCP toolbox
                    toolbox <- McpTools.initializeMcpToolbox tracer name proc
                    
                    -- Get tools from toolbox
                    tools <- atomically $ readTVar (McpTools.toolsList toolbox)
                    
                    -- Register each tool
                    let registrations = map (registerMcpTool toolbox) tools
                    pure $ Right $ Maybe.catMaybes $ map (either (const Nothing) Just) registrations

-- | Load tools from an OpenAPI configuration file.
loadToolsFromOpenAPIConfig :: FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromOpenAPIConfig configFile = do
    exists <- doesFileExist configFile
    if not exists
        then pure $ Left $ "OpenAPI config file does not exist: " <> Text.pack configFile
        else do
            eConfig <- Aeson.eitherDecodeFileStrict' configFile
            case eConfig of
                Left err -> pure $ Left $ "Failed to parse OpenAPI config: " <> Text.pack err
                Right desc -> do
                    let cfg = OpenAPIToolbox.Config
                            { OpenAPIToolbox.configUrl = Base.openApiSpecUrl desc
                            , OpenAPIToolbox.configBaseUrl = Base.openApiBaseUrl desc
                            , OpenAPIToolbox.configHeaders = Maybe.fromMaybe Map.empty $ Base.openApiHeaders desc
                            , OpenAPIToolbox.configToken = Base.openApiToken desc
                            }
                    let tracer = silent :: Tracer IO OpenAPIToolbox.Trace
                    
                    result <- OpenAPIToolbox.initializeToolbox tracer cfg
                    case result of
                        Left err -> pure $ Left $ "Failed to initialize OpenAPI toolbox: " <> Text.pack (show err)
                        Right toolbox -> do
                            -- Register all tools
                            eRegs <- ToolReg.registerOpenAPITools toolbox
                            case eRegs of
                                Left err -> pure $ Left $ "Failed to register OpenAPI tools: " <> Text.pack err
                                Right regs -> pure $ Right regs

-- | Load tools from a PostgREST configuration file.
loadToolsFromPostgRESTConfig :: FilePath -> IO (Either Text [ToolRegistration])
loadToolsFromPostgRESTConfig configFile = do
    exists <- doesFileExist configFile
    if not exists
        then pure $ Left $ "PostgREST config file does not exist: " <> Text.pack configFile
        else do
            eConfig <- Aeson.eitherDecodeFileStrict' configFile
            case eConfig of
                Left err -> pure $ Left $ "Failed to parse PostgREST config: " <> Text.pack err
                Right desc -> do
                    let cfg = PostgRESToolbox.Config
                            { PostgRESToolbox.configUrl = Base.postgrestSpecUrl desc
                            , PostgRESToolbox.configBaseUrl = Base.postgrestBaseUrl desc
                            , PostgRESToolbox.configHeaders = Maybe.fromMaybe Map.empty $ Base.postgrestHeaders desc
                            , PostgRESToolbox.configToken = Base.postgrestToken desc
                            }
                    let tracer = silent :: Tracer IO PostgRESToolbox.Trace
                    
                    result <- PostgRESToolbox.initializeToolbox tracer cfg
                    case result of
                        Left err -> pure $ Left $ "Failed to initialize PostgREST toolbox: " <> Text.pack (show err)
                        Right toolbox -> do
                            -- Register all tools
                            eRegs <- ToolReg.registerPostgRESTools toolbox
                            case eRegs of
                                Left err -> pure $ Left $ "Failed to register PostgREST tools: " <> Text.pack err
                                Right regs -> pure $ Right regs

-------------------------------------------------------------------------------
-- Helper functions for loading tools
-------------------------------------------------------------------------------

loadAgentFromFile :: FilePath -> IO (Either Text Base.Agent)
loadAgentFromFile path = do
    result <- Aeson.eitherDecodeFileStrict' path
    case result of
        Left err -> pure $ Left $ "Failed to parse agent file: " <> Text.pack err
        Right (Base.AgentDescription agent) -> pure $ Right agent

loadBashToolsFromDir :: FilePath -> IO [ToolRegistration]
loadBashToolsFromDir dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else do
            let tracer = silent :: Tracer IO Bash.LoadTrace
            (scripts, _errors) <- Bash.loadDirectory tracer dir
            pure $ map ToolReg.registerBashToolInLLM scripts.scriptDescriptions

loadMcpToolsFromAgent :: Base.Agent -> IO [ToolRegistration]
loadMcpToolsFromAgent agent = do
    case Base.mcpServers agent of
        Nothing -> pure []
        Just [] -> pure []
        Just servers -> do
            fmap concat $ mapM (loadMcpServerTools agent) servers

loadMcpServerTools :: Base.Agent -> Base.McpServerDescription -> IO [ToolRegistration]
loadMcpServerTools _agent (Base.McpSimpleBinary cfg) = do
    -- Use pattern matching to avoid the 'executable' keyword issue
    let exePath = Base.executable cfg
        exeArgs = Base.args cfg
    let proc = System.Process.proc exePath (map Text.unpack exeArgs)
    let tracer = silent :: Tracer IO McpTools.Trace
    
    -- Initialize MCP toolbox
    toolbox <- McpTools.initializeMcpToolbox tracer (Base.name cfg) proc
    
    -- Get tools from toolbox
    tools <- atomically $ readTVar (McpTools.toolsList toolbox)
    
    -- Register each tool
    let registrations = map (registerMcpTool toolbox) tools
    pure $ Maybe.catMaybes $ map (either (const Nothing) Just) registrations

registerMcpTool :: McpTools.Toolbox -> McpTools.ToolDescription -> Either String ToolRegistration
registerMcpTool toolbox tool = ToolReg.registerMcpToolInLLM toolbox tool

loadOpenAPIToolsFromAgent :: Base.Agent -> IO [ToolRegistration]
loadOpenAPIToolsFromAgent agent =
    case Base.openApiToolboxes agent of
        Nothing -> pure []
        Just [] -> pure []
        Just toolboxes -> fmap concat $ mapM loadOpenAPIToolbox toolboxes

loadOpenAPIToolbox :: Base.OpenAPIToolboxDescription -> IO [ToolRegistration]
loadOpenAPIToolbox (Base.OpenAPIServer desc) = do
    let cfg = OpenAPIToolbox.Config
            { OpenAPIToolbox.configUrl = Base.openApiSpecUrl desc
            , OpenAPIToolbox.configBaseUrl = Base.openApiBaseUrl desc
            , OpenAPIToolbox.configHeaders = Maybe.fromMaybe Map.empty $ Base.openApiHeaders desc
            , OpenAPIToolbox.configToken = Base.openApiToken desc
            }
    let tracer = silent :: Tracer IO OpenAPIToolbox.Trace
    result <- OpenAPIToolbox.initializeToolbox tracer cfg
    case result of
        Left _ -> pure []
        Right toolbox -> do
            eRegs <- ToolReg.registerOpenAPITools toolbox
            case eRegs of
                Left _ -> pure []
                Right regs -> pure regs
loadOpenAPIToolbox (Base.OpenAPIServerOnDiskDescription (Base.OpenAPIServerOnDisk path)) = do
    result <- loadToolsFromOpenAPIConfig path
    case result of
        Left _ -> pure []
        Right regs -> pure regs

loadPostgRESToolsFromAgent :: Base.Agent -> IO [ToolRegistration]
loadPostgRESToolsFromAgent agent =
    case Base.postgrestToolboxes agent of
        Nothing -> pure []
        Just [] -> pure []
        Just toolboxes -> fmap concat $ mapM loadPostgRESToolbox toolboxes

loadPostgRESToolbox :: Base.PostgRESTToolboxDescription -> IO [ToolRegistration]
loadPostgRESToolbox (Base.PostgRESTServer desc) = do
    let cfg = PostgRESToolbox.Config
            { PostgRESToolbox.configUrl = Base.postgrestSpecUrl desc
            , PostgRESToolbox.configBaseUrl = Base.postgrestBaseUrl desc
            , PostgRESToolbox.configHeaders = Maybe.fromMaybe Map.empty $ Base.postgrestHeaders desc
            , PostgRESToolbox.configToken = Base.postgrestToken desc
            }
    let tracer = silent :: Tracer IO PostgRESToolbox.Trace
    result <- PostgRESToolbox.initializeToolbox tracer cfg
    case result of
        Left _ -> pure []
        Right toolbox -> do
            eRegs <- ToolReg.registerPostgRESTools toolbox
            case eRegs of
                Left _ -> pure []
                Right regs -> pure regs
loadPostgRESToolbox (Base.PostgRESTServerOnDiskDescription (Base.PostgRESTServerOnDisk path)) = do
    result <- loadToolsFromPostgRESTConfig path
    case result of
        Left _ -> pure []
        Right regs -> pure regs

-------------------------------------------------------------------------------
-- MCP Server Implementation
-------------------------------------------------------------------------------

-- | Runtime state for the direct MCP toolbox server.
data DirectMcpRuntime = DirectMcpRuntime
    { directToolRegistry :: DirectToolRegistry
    , directServerName :: Text
    , directServerVersion :: Text
    , directActions :: IORef [(Rpc.Request, Async ())]
    }

-- | Initialize the direct MCP runtime.
initDirectMcpRuntime :: DirectToolRegistry -> Text -> Text -> IO DirectMcpRuntime
initDirectMcpRuntime registry name version = do
    actionsRef <- newIORef []
    pure $ DirectMcpRuntime
        { directToolRegistry = registry
        , directServerName = name
        , directServerVersion = version
        , directActions = actionsRef
        }

-- | Run the MCP server with direct tool execution.
runMcpToolboxServer :: McpToolboxOptions -> IO ()
runMcpToolboxServer opts = do
    -- Create the tool registry
    eRegistry <- createDirectRegistry (mcpToolSources opts) (mcpNameMapping opts)
    
    case eRegistry of
        Left err -> do
            Text.hPutStrLn stderr $ "Failed to create tool registry: " <> err
            exitFailure
        Right registry -> do
            -- Initialize runtime
            rt <- initDirectMcpRuntime registry (mcpServerName opts) (mcpServerVersion opts)
            
            -- Run the server based on transport
            case mcpTransport opts of
                StdioTransport -> runStdioServer rt
                HttpTransport port -> do
                    Text.hPutStrLn stderr $ "HTTP transport not yet implemented on port " <> Text.pack (show port)
                    exitFailure

-- | Run the server over stdio.
runStdioServer :: DirectMcpRuntime -> IO ()
runStdioServer rt = do
    let logTrace = defaultOutput stderr
    runLoggingT (runReaderT (runDirectMcpStack mainMcp) rt) logTrace

-- | The main MCP protocol loop.
mainMcp :: DirectMcpStack ()
mainMcp = do
    runJSONRPCT'
        Rpc.V2
        False
        (sinkHandleFlush stdout)
        stdinC
        handlerLoop

-- | Custom MCP stack for direct tool execution.
newtype DirectMcpStack a = DirectMcpStack {runDirectMcpStack :: ReaderT DirectMcpRuntime (LoggingT IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, MonadUnliftIO)

instance MonadReader DirectMcpRuntime DirectMcpStack where
    ask = DirectMcpStack ask
    local f (DirectMcpStack a) = DirectMcpStack (local f a)

askDirectRuntime :: Rpc.JSONRPCT DirectMcpStack DirectMcpRuntime
askDirectRuntime = lift ask

-------------------------------------------------------------------------------
-- MCP Protocol Handlers
-------------------------------------------------------------------------------

serverProtocolVersion :: Text
serverProtocolVersion = "2024-11-05"

handlerLoop :: Rpc.JSONRPCT DirectMcpStack ()
handlerLoop = do
    loop
  where
    loop :: Rpc.JSONRPCT DirectMcpStack ()
    loop = do
        mreq <- Rpc.receiveRequest
        maybe (pure ()) handleReq mreq
        loop

    handleReq :: Rpc.Request -> Rpc.JSONRPCT DirectMcpStack ()
    handleReq req = do
        let emsg = Rpc.fromRequest req :: Either Rpc.ErrorObj ClientMsg
        case emsg of
            (Left err) -> do
                -- Send error response
                let rsp = Rpc.Response (req.getReqVer) (Aeson.toJSON err) (req.getReqId)
                Rpc.sendResponse rsp
            (Right msg) -> handleParsedReq req msg

    handleParsedReq :: Rpc.Request -> ClientMsg -> Rpc.JSONRPCT DirectMcpStack ()
    handleParsedReq req msg = do
        rt <- askDirectRuntime
        a <- async $ handleMsg req msg
        void $ liftIO $ addAsyncDirect rt req a

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
    parseParams _ =
        Nothing

handleMsg :: Rpc.Request -> ClientMsg -> Rpc.JSONRPCT DirectMcpStack ()
handleMsg _ (CancelledNotificationMsg c) = do
    rt <- askDirectRuntime
    liftIO $ cancellAsyncDirect rt c.requestId
handleMsg req (InitializeMsg _) = do
    rt <- askDirectRuntime
    let implem = Mcp.Implementation rt.directServerName rt.directServerVersion
    let rsp = respond req (Mcp.InitializeResult serverProtocolVersion serverCapabilities implem (Just ""))
    Rpc.sendResponse rsp
handleMsg _ (NotifyInitializedMsg _) =
    pure ()
handleMsg req (ListResourcesRequestMsg _) = do
    let rsp = respond req (Mcp.ListResourcesResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg req (ListToolsRequestMsg _) = do
    rt <- askDirectRuntime
    tools <- liftIO $ atomically $ readTVar (registryTools (directToolRegistry rt))
    let mcpTools = toolRegistrationsToMcpTools tools
    let rsp = respond req (Mcp.ListToolsResult mcpTools Nothing)
    Rpc.sendResponse rsp
handleMsg req (ListPromptsRequestMsg _) = do
    let rsp = respond req (Mcp.ListPromptsResult [] Nothing)
    Rpc.sendResponse rsp
handleMsg req (CallToolRequestMsg callTool) = do
    rt <- askDirectRuntime
    tools <- liftIO $ atomically $ readTVar (registryTools (directToolRegistry rt))
    result <- liftIO $ callToolDirect tools callTool.name callTool.arguments
    let rsp = respond req result
    Rpc.sendResponse rsp

-------------------------------------------------------------------------------
-- Async management for direct MCP runtime
-------------------------------------------------------------------------------

addAsyncDirect :: DirectMcpRuntime -> Rpc.Request -> Async () -> IO (Async ())
addAsyncDirect rt req a = do
    atomicModifyIORef rt.directActions insert
    async $ do
        _ <- waitCatch a
        atomicModifyIORef rt.directActions remove
  where
    insert :: [(Rpc.Request, Async ())] -> ([(Rpc.Request, Async ())], ())
    insert xs = (((req, a) : xs), ())

    remove :: [(Rpc.Request, Async ())] -> ([(Rpc.Request, Async ())], ())
    remove xs = ([x | x@(r, _) <- xs, r.getReqId /= r.getReqId], ())

cancellAsyncDirect :: DirectMcpRuntime -> Mcp.RequestId -> IO ()
cancellAsyncDirect rt reqId = do
    found <- atomicModifyIORef rt.directActions (remove reqId)
    forM_ found $ \a -> cancel a
  where
    remove :: Mcp.RequestId -> [(Rpc.Request, Async ())] -> ([(Rpc.Request, Async ())], Maybe (Async ()))
    remove rid xs =
        ( [x | x@(r, _) <- xs, not $ matchingIDs r.getReqId rid]
        , fmap snd (List.find (\(r, _) -> matchingIDs r.getReqId rid) xs)
        )

    matchingIDs :: Rpc.Id -> Mcp.RequestId -> Bool
    matchingIDs (Rpc.IdInt n) (Mcp.IntRequestId m) = n == m
    matchingIDs (Rpc.IdTxt n) (Mcp.TextRequestId m) = n == m
    matchingIDs _ _ = False

-------------------------------------------------------------------------------
-- Tool Conversion and Execution
-------------------------------------------------------------------------------

-- | Convert tool registrations to MCP tool definitions.
toolRegistrationsToMcpTools :: [ToolRegistration] -> [Mcp.Tool]
toolRegistrationsToMcpTools = Maybe.catMaybes . map toolRegistrationToMcpTool

-- | Convert a single tool registration to an MCP tool definition.
toolRegistrationToMcpTool :: ToolRegistration -> Maybe Mcp.Tool
toolRegistrationToMcpTool reg =
    let llmTool = reg.declareTool
        toolName = llmTool.toolName.getToolName
    in Just Mcp.Tool
        { Mcp.name = toolName
        , Mcp.description = Just llmTool.toolDescription
        , Mcp.inputSchema = paramPropertiesToInputSchema llmTool.toolParamProperties
        }

-- | Convert parameter properties to MCP input schema.
paramPropertiesToInputSchema :: [ParamProperty] -> Mcp.InputSchema
paramPropertiesToInputSchema props =
    Mcp.InputSchema
        { Mcp.required = Just $ map propertyKey props
        , Mcp.properties = Just $ KeyMap.fromList $ map paramPropertyToJsonPair props
        }
  where
    paramPropertyToJsonPair :: ParamProperty -> (Aeson.Key, Aeson.Value)
    paramPropertyToJsonPair p = (AesonKey.fromText p.propertyKey, paramPropertyToJson p)
    
    paramPropertyToJson :: ParamProperty -> Aeson.Value
    paramPropertyToJson p =
        Aeson.object $
            [ "type" Aeson..= paramTypeToString p.propertyType
            , "description" Aeson..= p.propertyDescription
            ]
            ++ case p.propertyType of
                EnumParamType xs -> ["enum" Aeson..= xs]
                ObjectParamType subProps -> 
                    ["properties" Aeson..= KeyMap.fromList (map paramPropertyToJsonPair subProps)]
                _ -> []
    
    paramTypeToString :: ParamType -> Text
    paramTypeToString NullParamType = "null"
    paramTypeToString StringParamType = "string"
    paramTypeToString BoolParamType = "boolean"
    paramTypeToString NumberParamType = "number"
    paramTypeToString (EnumParamType _) = "string"
    paramTypeToString (OpaqueParamType t) = t
    paramTypeToString (MultipleParamType t) = t
    paramTypeToString (ObjectParamType _) = "object"

-- | Execute a tool call directly (without agent context).
callToolDirect :: [ToolRegistration] -> Text -> Maybe Aeson.Object -> IO Mcp.CallToolResult
callToolDirect registrations toolName mArgs = do
    -- Find the tool
    case findToolByName registrations toolName of
        Nothing -> do
            pure Mcp.CallToolResult
                { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl ("Tool not found: " <> toolName) (Just [])]
                , Mcp.isError = Just True
                }
        Just reg -> do
            -- Create a minimal execution context
            ctx <- createMinimalToolContext
            
            -- Execute the tool
            let argsValue = case mArgs of
                    Just obj -> Aeson.Object obj
                    Nothing -> Aeson.Object mempty
            
            -- Use pattern matching to access toolRun
            let Tool _ runFunc = reg.innerTool
            result <- runFunc silent ctx argsValue
            
            -- Convert result to MCP format
            pure $ callResultToMcpResult result

-- | Find a tool registration by name.
findToolByName :: [ToolRegistration] -> Text -> Maybe ToolRegistration
findToolByName regs name =
    List.find (\r -> r.declareTool.toolName.getToolName == name) regs

-- | Create a minimal tool execution context for direct execution.
createMinimalToolContext :: IO ToolExecutionContext
createMinimalToolContext = do
    sessId <- SessionBase.newSessionId
    convId <- newConversationId
    tId <- SessionBase.newTurnId
    pure $ mkToolExecutionContext
        sessId
        convId
        tId
        Nothing  -- No agent ID in direct mode
        Nothing  -- No full session
        [CallStackEntry "direct-mcp" convId 0]
        Nothing  -- No max recursion

-- | Convert a CallResult to an MCP CallToolResult.
callResultToMcpResult :: CallResult () -> Mcp.CallToolResult
callResultToMcpResult result = case result of
    BlobToolSuccess _ bs ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (TextEncoding.decodeUtf8 bs) (Just [])]
            , Mcp.isError = Just False
            }
    ToolNotFound _ ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl "Tool not found" (Just [])]
            , Mcp.isError = Just True
            }
    BashToolError _ err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack $ show err) (Just [])]
            , Mcp.isError = Just True
            }
    IOToolError _ err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack $ show err) (Just [])]
            , Mcp.isError = Just True
            }
    McpToolResult _ mcpResult ->
        mcpResult  -- Direct MCP result is already in the right format
    McpToolError _ err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack err) (Just [])]
            , Mcp.isError = Just True
            }
    OpenAPIToolResult _ toolResult ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack $ show toolResult) (Just [])]
            , Mcp.isError = Just False
            }
    OpenAPIToolError _ err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl ("OpenAPI error: " <> Text.pack err) (Just [])]
            , Mcp.isError = Just True
            }
    PostgRESToolResult _ toolResult ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl (Text.pack $ show toolResult) (Just [])]
            , Mcp.isError = Just False
            }
    PostgRESToolError _ err ->
        Mcp.CallToolResult
            { Mcp.content = [Mcp.TextContent $ Mcp.TextContentImpl ("PostgREST error: " <> Text.pack err) (Just [])]
            , Mcp.isError = Just True
            }

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

respond :: (Aeson.ToJSON val) => Rpc.Request -> val -> Rpc.Response
respond req obj =
    Rpc.Response (req.getReqVer) (Aeson.toJSON obj) (req.getReqId)

serverCapabilities :: Mcp.ServerCapabilities
serverCapabilities =
    Mcp.ServerCapabilities
        (Just mempty)
        (Just $ Aeson.object [])
        [Mcp.ToolsListChanged]

-- | Strip LLM name prefixes from tool names.
--
-- This removes prefixes like @bash_@, @mcp_@, @openapi_@, @postgrest_@
-- to expose cleaner names in MCP.
stripLLMNamePrefix :: Text -> Text
stripLLMNamePrefix name =
    let prefixes = ["bash_", "mcp_", "openapi_", "postgrest_", "io_"]
        stripPrefix p t = if Text.isPrefixOf p t then Text.drop (Text.length p) t else t
    in foldl (flip stripPrefix) name prefixes

