{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines an LLM tool registration.
--
-- This module provides functionality for registering tools with the LLM system.
-- It supports registration of various tool types:
--
-- * Bash tools - External scripts executed in separate processes
-- * IO tools - Haskell functions executed in-process
-- * MCP tools - Tools exposed via Model Context Protocol
-- * OpenAPI tools - Tools generated from OpenAPI specifications
--
-- Each tool type has specific registration functions that handle the
-- conversion to LLM-compatible format.
module System.Agents.ToolRegistration (
    -- * Core types
    ToolRegistration (..),
    
    -- * Registration functions
    registerBashToolInLLM,
    registerIOScriptInLLM,
    registerMcpToolInLLM,
    registerOpenAPIToolInLLM,
    registerOpenAPITools,
    registerOpenAPITool,
    
    -- * Naming policies
    io2LLMName,
    bash2LLMName,
    mcp2LLMName,
    openapi2LLMName,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.ByteString (ByteString)
import Data.Foldable.WithIndex (ifoldl')
import qualified Data.Maybe as Maybe
import Data.Text (Text)

import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.MCP.Client as McpClient
import System.Agents.Tools.Base (
    CallResult (..),
    Tool (..),
    ToolDef (..),
    mapCallResult,
    mapToolResult,
  )
import System.Agents.Tools.Context (ToolExecutionContext)
import System.Agents.Tools.IO (IOScript (..), IOScriptDescription (..), RunError (..))
import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.Bash (ScriptDescription (..), ScriptArg (..), RunScriptError)
import qualified System.Agents.Tools.Bash as BashTools
import System.Agents.Tools.McpToolbox (Toolbox, ToolDescription, callTool)
import qualified System.Agents.Tools.McpToolbox as McpTools
import System.Agents.Tools.OpenAPI.Converter (OpenAPITool (..), toOpenAITool)
import qualified System.Agents.Tools.OpenAPI.Converter as OpenAPI
import System.Agents.Tools.OpenAPIToolbox (
    Toolbox (..),
    createToolHandler,
    openapi2LLMName,
  )
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import System.Agents.ToolSchema
import System.Agents.Tools.Trace (ToolTrace (..))
import Prod.Tracer (Tracer, contramap)

-------------------------------------------------------------------------------

-- | We register tools that will take a ToolExecutionContext for execution.
--
-- The 'innerTool' field uses 'Tool ()' since the tool execution context
-- is passed at runtime via 'toolRun', not stored in the tool itself.
data ToolRegistration
    = ToolRegistration
    { innerTool :: Tool ()
    , declareTool :: OpenAI.Tool
    , findTool :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
    }
instance Show ToolRegistration where
    show (ToolRegistration d _ _) = Prelude.unwords ["ToolRegistration(", show d.toolDef, ")"]

-------------------------------------------------------------------------------

-- naming policy for IO tools
io2LLMName :: forall a b. IOScript a b -> OpenAI.ToolName
io2LLMName io = OpenAI.ToolName (mconcat ["io_", io.description.ioSlug])

-- naming policy for Bash tools
bash2LLMName :: ScriptDescription -> OpenAI.ToolName
bash2LLMName bash = OpenAI.ToolName (mconcat ["bash_", bash.scriptInfo.scriptSlug])

-- naming policy for MCP tools
mcp2LLMName :: McpTools.Toolbox -> McpTools.ToolDescription -> OpenAI.ToolName
mcp2LLMName box mcp =
    OpenAI.ToolName (mconcat ["mcp_", box.name, "_", mcp.getToolDescription.name])

-------------------------------------------------------------------------------

registerBashToolInLLM ::
    ScriptDescription ->
    ToolRegistration
registerBashToolInLLM script =
    let
        matchName :: ScriptDescription -> OpenAI.ToolCall -> Bool
        matchName bash call = bash2LLMName bash == call.toolCallFunction.toolCallFunctionName

        mapToolDescriptionBash2LLM :: ScriptDescription -> OpenAI.Tool
        mapToolDescriptionBash2LLM bash =
            OpenAI.Tool
                { OpenAI.toolName = bash2LLMName bash
                , OpenAI.toolDescription = bash.scriptInfo.scriptDescription
                , OpenAI.toolParamProperties = fmap mapArg bash.scriptInfo.scriptArgs
                }

        mapArg :: ScriptArg -> ParamProperty
        mapArg arg =
            ParamProperty
                { propertyKey = arg.argName
                , propertyType = OpaqueParamType arg.argBackingTypeString
                , propertyDescription = arg.argDescription
                }

        tool :: Tool ()
        tool = bashTool script

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing
     in
        ToolRegistration tool (mapToolDescriptionBash2LLM script) find

-------------------------------------------------------------------------------

{- | registers an IO Script, since we have not yet decided on a way to capture the
shape of the tool for IOScript (ideally some generics or something like Data.Aeson.Encoding) we take the whole Tool definition
-}
registerIOScriptInLLM ::
    (Aeson.FromJSON a) =>
    IOScript a ByteString ->
    [ParamProperty] ->
    ToolRegistration
registerIOScriptInLLM script llmProps =
    let
        matchName :: IOScript a b -> OpenAI.ToolCall -> Bool
        matchName io call = io2LLMName io == call.toolCallFunction.toolCallFunctionName

        tool :: Tool ()
        tool = ioTool script

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing

        llmTool :: OpenAI.Tool
        llmTool =
            OpenAI.Tool
                { OpenAI.toolName = io2LLMName script
                , OpenAI.toolDescription = script.description.ioDescription
                , OpenAI.toolParamProperties = llmProps
                }
     in
        ToolRegistration tool llmTool find

-------------------------------------------------------------------------------

-- | Register an MCP tool with the LLM system.
--
-- Returns 'Left' if the tool's schema cannot be adapted to the LLM format.
registerMcpToolInLLM ::
    McpTools.Toolbox ->
    McpTools.ToolDescription ->
    Either String ToolRegistration
registerMcpToolInLLM box mcp =
    let
        matchName :: McpTools.ToolDescription -> OpenAI.ToolCall -> Bool
        matchName td call = mcp2LLMName box td == call.toolCallFunction.toolCallFunctionName

        llmBasedSchema :: Either String [ParamProperty]
        llmBasedSchema = adaptSchema mcp.getToolDescription.inputSchema

        llmName :: OpenAI.ToolName
        llmName = mcp2LLMName box mcp

        llmDescription :: Text
        llmDescription = Maybe.fromMaybe "" mcp.getToolDescription.description

        mapToolDescriptionMcp2LLM :: [ParamProperty] -> OpenAI.Tool
        mapToolDescriptionMcp2LLM schema =
            OpenAI.Tool
                { OpenAI.toolName = llmName
                , OpenAI.toolDescription = llmDescription
                , OpenAI.toolParamProperties = schema
                }

        tool :: Tool ()
        tool = mcpTool box mcp

        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call = if matchName mcp call then Just (mapToolResult (const call) tool) else Nothing
     in
        case llmBasedSchema of
            Right schema ->
                Right $ ToolRegistration tool (mapToolDescriptionMcp2LLM schema) find
            Left err ->
                Left err

-------------------------------------------------------------------------------
-- OpenAPI Tool Registration
-------------------------------------------------------------------------------

-- | Register a single OpenAPI tool with the LLM system.
--
-- This function creates a 'ToolRegistration' from an OpenAPI tool and its
-- parent toolbox. The tool must have a valid operation ID.
--
-- Returns 'Left' if the tool cannot be registered (e.g., no operation ID).
--
-- Example:
--
-- @
-- case registerOpenAPITool toolbox apiTool of
--     Left err -> putStrLn $ "Failed to register: " ++ err
--     Right registration -> useWithAgent registration
-- @
registerOpenAPITool ::
    OpenAPIToolbox.Toolbox ->
    OpenAPI.OpenAPITool ->
    Either String ToolRegistration
registerOpenAPITool toolbox tool =
    let opId = Maybe.fromMaybe (OpenAPI.toolName tool) (OpenAPIToolbox.getOperationId (OpenAPI.toolOperation tool))
        llmName = openapi2LLMName (OpenAPIToolbox.toolboxName toolbox) opId

        -- Convert to OpenAI Tool format
        openaiTool = toOpenAITool tool

        -- Create the tool handler
        toolDef = IOTool $ IOScriptDescription
            { ioSlug = OpenAI.getToolName llmName
            , ioDescription = OpenAPI.toolDescription tool
            }

        -- Create the run function
        runFunc :: Tracer IO ToolTrace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
        runFunc tr ctx argz = createToolHandler toolbox tool tr ctx argz

        -- Create the Tool
        tool' = Tool
            { toolDef = toolDef
            , toolRun = runFunc
            }

        -- Find function
        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call =
            if call.toolCallFunction.toolCallFunctionName == llmName
                then Just $ mapToolResult (const call) tool'
                else Nothing

        -- Declare function
        declare :: OpenAI.Tool
        declare = openaiTool{OpenAI.toolName = llmName}
     in Right $ ToolRegistration
            { innerTool = mapToolResult (const ()) tool'
            , declareTool = declare
            , findTool = find
            }

-- | Register all tools from an OpenAPI toolbox.
--
-- This function iterates through all tools in the toolbox and attempts to
-- register each one. If any registration fails, the entire operation fails
-- fast with the first error encountered.
--
-- For partial success handling, use 'registerOpenAPITool' on individual tools.
--
-- Example:
--
-- @
-- result <- OpenAPI.initializeToolbox tracer config
-- case result of
--     Left err -> print err
--     Right toolbox -> do
--         regResult <- registerOpenAPITools toolbox
--         case regResult of
--             Left err -> putStrLn $ "Registration failed: " ++ err
--             Right registrations -> do
--                 -- Use registrations with agent runtime
--                 mapM_ (addToAgent agent) registrations
-- @
registerOpenAPITools ::
    OpenAPIToolbox.Toolbox ->
    IO (Either String [ToolRegistration])
registerOpenAPITools toolbox =
    -- Fail fast - return first error encountered
    let registerAll :: [OpenAPI.OpenAPITool] -> Either String [ToolRegistration] -> Either String [ToolRegistration]
        registerAll [] acc = acc
        registerAll (t:ts) (Right regs) =
            case registerOpenAPITool toolbox t of
                Left err -> Left err
                Right reg -> registerAll ts (Right (reg:regs))
        registerAll _ err = err
        
        tools = OpenAPIToolbox.toolboxTools toolbox
     in pure $ case registerAll tools (Right []) of
            Left err -> Left err
            Right regs -> Right (reverse regs)

-- | Register an OpenAPI tool in the LLM system (alias for 'registerOpenAPITool').
--
-- This is the original function name from the OpenAPIToolbox module,
-- provided for backward compatibility.
registerOpenAPIToolInLLM ::
    OpenAPIToolbox.Toolbox ->
    OpenAPI.OpenAPITool ->
    Either String ToolRegistration
registerOpenAPIToolInLLM = registerOpenAPITool

-------------------------------------------------------------------------------
-- Internal tool builders (copied from Tools to avoid import cycle)
-------------------------------------------------------------------------------

-- | Builder for a tool based on a Bash script-description.
bashTool ::
    ScriptDescription ->
    Tool ()
bashTool script =
    Tool
        { toolDef = BashTool script
        , toolRun = run
        }
  where
    call = ()
    run tracer ctx v = do
        ret <- BashTools.runValue (contramap BashToolsTrace tracer) script (Just ctx) v
        case ret of
            Left err -> pure $ BashToolError call err
            Right rsp -> pure $ BlobToolSuccess call rsp

-- | Builder for a tool based on an MCP toolbox tool.
mcpTool ::
    McpTools.Toolbox ->
    McpTools.ToolDescription ->
    Tool ()
mcpTool toolbox desc =
    Tool
        { toolDef = MCPTool desc
        , toolRun = run
        }
  where
    call = ()
    run _tracer _ctx (Aeson.Object v) = do
        ret <- McpTools.callTool toolbox desc (Just v)
        case ret of
            (Just (Right rsp)) -> pure $ extractContentsFromToolCall rsp
            err -> pure $ McpToolError call (mconcat ["calling error: ", show err])
    run _tracer _ctx _ = do
        pure $ McpToolError call ("can only call McpTools with Aeson.Object")
    extractContentsFromToolCall :: McpClient.CallToolResultRsp -> CallResult ()
    extractContentsFromToolCall rsp =
        McpToolResult call rsp.getCallToolResult

-- | Builder for a tool based on an IO-tool script-description.
ioTool ::
    (Aeson.FromJSON llmArg) =>
    IOScript llmArg ByteString ->
    Tool ()
ioTool script =
    Tool
        { toolDef = IOTool script.description
        , toolRun = run
        }
  where
    call = ()
    run tracer ctx v = do
        let adaptTrace = IOToolsTrace . IOTools.adaptTraceInput (const v)
        ret <- IOTools.runValue (contramap adaptTrace tracer) script ctx v
        case ret of
            Left err -> pure $ IOToolError call err
            Right rsp -> pure $ BlobToolSuccess call rsp

-------------------------------------------------------------------------------
-- Schema adaptation helpers
-------------------------------------------------------------------------------

adaptSchema :: Mcp.InputSchema -> Either String [ParamProperty]
adaptSchema schema =
    case schema.properties of
        Nothing -> Right []
        Just obj -> ifoldl' f (Right []) obj
  where
    f :: Aeson.Key -> Either String [ParamProperty] -> Aeson.Value -> Either String [ParamProperty]
    f _ err@(Left _) _ = err
    f k (Right xs) v =
        case adaptProperty k v of
            Left err -> Left err
            Right x -> Right (x : xs)

adaptProperty :: Aeson.Key -> Aeson.Value -> Either String ParamProperty
adaptProperty k val =
    case propMappingResult of
        Aeson.Success prop ->
            Right $
                ParamProperty
                    (AesonKey.toText k)
                    (OpaqueParamType prop._type)
                    prop._description
        Aeson.Error err -> Left err
  where
    propMappingResult :: Aeson.Result PropertyHelper
    propMappingResult = Aeson.fromJSON val

data PropertyHelper
    = PropertyHelper {_type :: Text, _description :: Text}

instance Aeson.FromJSON PropertyHelper where
    parseJSON = Aeson.withObject "PropertyHelper" $ \o ->
        PropertyHelper <$> o Aeson..: "type" <*> o Aeson..: "description"

