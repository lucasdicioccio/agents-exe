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
-- * PostgREST tools - Tools generated from PostgREST database APIs
--
-- For OpenAPI tools, special handling is done for name normalization:
-- OpenAPI operation IDs may contain invalid characters (dots, slashes, etc.)
-- which are normalized to LLM-safe names. The 'NameMapping' system maintains
-- bidirectional mapping between normalized and original names.
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
    registerPostgRESToolInLLM,
    registerPostgRESTools,
    registerPostgRESTool,
    
    -- * Naming policies
    io2LLMName,
    bash2LLMName,
    mcp2LLMName,
    openapi2LLMName,
    postgrest2LLMName,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.ByteString (ByteString)
import Data.Foldable.WithIndex (ifoldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.MCP.Client as McpClient
import System.Agents.Tools.Base (
    CallResult (..),
    Tool (..),
    ToolDef (..),
    mapToolResult,
 )
import System.Agents.Tools.Context (ToolExecutionContext)
import System.Agents.Tools.IO (IOScript (..), IOScriptDescription (..))
import qualified System.Agents.Tools.IO as IOTools
import System.Agents.Tools.Bash (ScriptDescription (..), ScriptArg (..))
import qualified System.Agents.Tools.Bash as BashTools
import System.Agents.Tools.McpToolbox (callTool)
import qualified System.Agents.Tools.McpToolbox as McpTools
import System.Agents.Tools.OpenAPI.Converter (
    OpenAPITool (..),
    NameMapping (..),
    toOpenAITool,
    normalizeForLLM,
 )
import qualified System.Agents.Tools.OpenAPI.Converter as OpenAPI
import System.Agents.Tools.OpenAPIToolbox (
    createToolHandler,
    getToolByNormalizedName,
    openapi2LLMName,
 )
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import System.Agents.Tools.PostgREST.Converter (
    PostgRESTool (..),
    buildToolParameters,
    ToolParameters (..),
    FilterSchema (..),
    ColumnFilterSchema (..),
    SubsetSchema (..),
    RankingSchema (..),
    extractRequestBody,
 )
import qualified System.Agents.Tools.PostgRESToolbox as PostgRESToolbox
import System.Agents.Tools.OpenAPI.Types (Schema (..), OpenAPISpec (..))
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

-- naming policy for PostgREST tools
postgrest2LLMName :: PostgRESToolbox.Toolbox -> PostgRESTool -> OpenAI.ToolName
postgrest2LLMName box tool =
    let normalizedToolbox = normalizeForLLM box.toolboxName
        -- Extract table name from path (e.g., "/users" -> "users")
        tableName = Text.dropWhile (== '/') tool.prtPath
        normalizedTable = normalizeForLLM tableName
        methodPart = Text.toLower tool.prtMethod
     in OpenAI.ToolName (mconcat ["postgrest_", normalizedToolbox, "_", methodPart, "_", normalizedTable])

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
                , propertyRequired = True
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
        llmDescription = fromMaybe "" mcp.getToolDescription.description

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
-- parent toolbox. The tool name is normalized for LLM compatibility.
--
-- The name normalization handles OpenAPI operation IDs that may contain
-- invalid characters (dots, slashes, etc.) by:
-- 1. Replacing invalid characters with underscores
-- 2. Ensuring the name starts with a letter
-- 3. Using the toolbox's name mapping for bidirectional lookup
--
-- Returns 'Left' if the tool cannot be registered.
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
    OpenAPITool ->
    Either String ToolRegistration
registerOpenAPITool toolbox tool =
    let 
        -- Get the original operation ID
        originalOpId = fromMaybe (toolName tool) 
                       (OpenAPIToolbox.getOperationId (toolOperation tool))
        
        -- Get the normalized name from the mapping
        mNameMapping = findNameMapping toolbox originalOpId
        
        -- Generate LLM name using the normalized operation ID
        llmName = case mNameMapping of
            Just nm -> openapi2LLMName (OpenAPIToolbox.toolboxName toolbox) (nmNormalized nm)
            Nothing -> openapi2LLMName (OpenAPIToolbox.toolboxName toolbox) originalOpId
     in case mNameMapping of
        Nothing -> Left $ "Tool not found in name mapping: " ++ Text.unpack originalOpId
        Just _nameMapping ->
            let -- Convert to OpenAI Tool format with normalized name
                openaiTool = (toOpenAITool tool)
                    { OpenAI.toolName = llmName }

                -- Create the tool handler that uses the mapping
                runFunc :: Tracer IO ToolTrace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
                runFunc tr ctx argz = createToolHandler toolbox tool tr ctx argz

                -- Create the Tool
                toolDef0 = IOTool $ IOScriptDescription
                    { ioSlug = OpenAI.getToolName llmName
                    , ioDescription = toolDescription tool
                    }

                tool' = Tool
                    { toolDef = toolDef0
                    , toolRun = runFunc
                    }

                -- Find function - matches on the normalized LLM name
                find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
                find call =
                    if call.toolCallFunction.toolCallFunctionName == llmName
                        then Just $ mapToolResult (const call) tool'
                        else Nothing
             in Right $ ToolRegistration
                    { innerTool = mapToolResult (const ()) tool'
                    , declareTool = openaiTool
                    , findTool = find
                    }

-- | Find the name mapping for a given original operation ID.
findNameMapping :: OpenAPIToolbox.Toolbox -> Text -> Maybe NameMapping
findNameMapping toolbox originalOpId =
    -- Look through the name mapping to find one with matching original name
    case filter (\nm -> nmOriginal nm == originalOpId) 
                (Map.elems $ OpenAPIToolbox.toolboxNameMapping toolbox) of
        (nm:_) -> Just nm
        [] -> Nothing

-- | Register all tools from an OpenAPI toolbox.
--
-- This function iterates through all tools in the toolbox and attempts to
-- register each one with normalized names for LLM compatibility.
--
-- If any registration fails, the entire operation fails fast with the first
-- error encountered.
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
    let registerAll :: [OpenAPITool] -> Either String [ToolRegistration] -> Either String [ToolRegistration]
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
    OpenAPITool ->
    Either String ToolRegistration
registerOpenAPIToolInLLM = registerOpenAPITool

-------------------------------------------------------------------------------
-- PostgREST Tool Registration
-------------------------------------------------------------------------------

-- | Register a single PostgREST tool with the LLM system.
--
-- This function creates a 'ToolRegistration' from a PostgREST tool and its
-- parent toolbox. The tool name follows the format:
-- postgrest_{toolbox}_{method}_{table}
--
-- The tool parameters are structured into groups:
-- * filters: Column-based row filters (for GET and filtering PATCH/DELETE)
-- * subset: Pagination (limit/offset) and column selection
-- * ranking: Ordering clause
-- * body: Request body for POST/PUT/PATCH operations
--
-- All parameter groups and their sub-properties are marked as optional,
-- allowing the LLM to provide only the parameters it needs.
--
-- Returns 'Left' if the tool cannot be registered.
--
-- Example:
--
-- @
-- case registerPostgRESTool toolbox prTool of
--     Left err -> putStrLn $ "Failed to register: " ++ err
--     Right registration -> useWithAgent registration
-- @
registerPostgRESTool ::
    PostgRESToolbox.Toolbox ->
    PostgRESTool ->
    Either String ToolRegistration
registerPostgRESTool toolbox tool =
    let llmName = postgrest2LLMName toolbox tool
        -- Get spec from toolbox
        spec = PostgRESToolbox.toolboxSpec toolbox
        -- Build base parameters from tool structure
        baseParams = buildToolParameters tool spec
        -- Extract request body schema for write operations
        requestBodySchema = extractRequestBodyFromTool tool spec
        -- Create final parameters with request body
        params = baseParams { tpRequestBody = requestBodySchema }
        
        -- Build parameter properties from structured parameters
        -- All parameters are marked as optional (propertyRequired = False)
        paramProps = buildPostgRESTParamProperties params
        
        -- Create the OpenAI Tool declaration
        openaiTool = OpenAI.Tool
            { OpenAI.toolName = llmName
            , OpenAI.toolDescription = prtDescription tool
            , OpenAI.toolParamProperties = paramProps
            }

        -- Create the tool handler
        runFunc :: Tracer IO ToolTrace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
        runFunc tr ctx argz = PostgRESToolbox.createToolHandler toolbox tool tr ctx argz

        -- Create the Tool definition
        toolDef0 = IOTool $ IOScriptDescription
            { ioSlug = OpenAI.getToolName llmName
            , ioDescription = prtDescription tool
            }

        tool' = Tool
            { toolDef = toolDef0
            , toolRun = runFunc
            }

        -- Find function - matches on the LLM name
        find :: OpenAI.ToolCall -> Maybe (Tool OpenAI.ToolCall)
        find call =
            if call.toolCallFunction.toolCallFunctionName == llmName
                then Just $ mapToolResult (const call) tool'
                else Nothing
     in Right $ ToolRegistration
            { innerTool = mapToolResult (const ()) tool'
            , declareTool = openaiTool
            , findTool = find
            }

-- | Extract request body schema for a PostgREST tool.
--
-- For write operations (POST, PUT, PATCH), looks up the request body
-- schema from the OpenAPI spec.
extractRequestBodyFromTool :: PostgRESTool -> OpenAPISpec -> Maybe Schema
extractRequestBodyFromTool tool spec =
    -- Only extract body for write operations
    if prtMethod tool `elem` ["POST", "PUT", "PATCH"]
        then
            -- Look up the operation in the spec and extract its body
            case Map.lookup (prtPath tool) (specPaths spec) of
                Just methods ->
                    case Map.lookup (prtMethod tool) methods of
                        Just op -> extractRequestBody op spec
                        Nothing -> Nothing
                Nothing -> Nothing
        else Nothing

-- | Build parameter properties for PostgREST tool from structured parameters.
--
-- All parameter groups (filters, subset, ranking, body) and their sub-properties
-- are marked as optional (propertyRequired = False), allowing the LLM to
-- provide only the parameters it needs.
--
-- For POST/PUT/PATCH operations, includes a 'body' parameter with the
-- appropriate schema for inserting or updating rows.
buildPostgRESTParamProperties :: ToolParameters -> [ParamProperty]
buildPostgRESTParamProperties params =
    let filterProp = case tpFilters params of
            Just fs -> Just $ ParamProperty
                { propertyKey = "filters"
                , propertyType = ObjectParamType (buildFilterSubProperties fs)
                , propertyDescription = fsDescription fs
                , propertyRequired = False  -- Optional parameter group
                }
            Nothing -> Nothing
        
        subsetProp = case tpSubset params of
            Just ss -> Just $ ParamProperty
                { propertyKey = "subset"
                , propertyType = ObjectParamType (buildSubsetSubProperties ss)
                , propertyDescription = "Pagination and column selection"
                , propertyRequired = False  -- Optional parameter group
                }
            Nothing -> Nothing
        
        rankingProp = case tpRanking params of
            Just rs -> Just $ ParamProperty
                { propertyKey = "ranking"
                , propertyType = ObjectParamType (buildRankingSubProperties rs)
                , propertyDescription = "Result ordering"
                , propertyRequired = False  -- Optional parameter group
                }
            Nothing -> Nothing
        
        bodyProp = case tpRequestBody params of
            Just schema -> Just $ buildBodyProperty schema
            Nothing -> Nothing
     in catMaybes [filterProp, subsetProp, rankingProp, bodyProp]
  where
    buildFilterSubProperties :: FilterSchema -> [ParamProperty]
    buildFilterSubProperties fs =
        map (\(col, schema) -> ParamProperty
            { propertyKey = col
            , propertyType = OpaqueParamType "string"
            , propertyDescription = cfsDescription schema
            , propertyRequired = False  -- Optional filter property
            }) (Map.toList $ fsProperties fs)
    
    buildSubsetSubProperties :: SubsetSchema -> [ParamProperty]
    buildSubsetSubProperties ss =
        catMaybes
            [ fmap (\desc -> ParamProperty "offset" (OpaqueParamType "string") desc False) (ssOffset ss)
            , fmap (\desc -> ParamProperty "limit" (OpaqueParamType "string") desc False) (ssLimit ss)
            , fmap (\desc -> ParamProperty "columns" (OpaqueParamType "string") desc False) (ssColumns ss)
            ]
    
    buildRankingSubProperties :: RankingSchema -> [ParamProperty]
    buildRankingSubProperties rs =
        catMaybes
            [ fmap (\desc -> ParamProperty "order" (OpaqueParamType "string") desc False) (rsOrder rs)
            ]

-- | Build the body property for write operations.
--
-- Converts the OpenAPI Schema to a ParamProperty that describes
-- the request body structure to the LLM.
buildBodyProperty :: Schema -> ParamProperty
buildBodyProperty schema =
    ParamProperty
        { propertyKey = "body"
        , propertyType = buildBodyParamType schema
        , propertyDescription = buildBodyDescription schema
        , propertyRequired = False  -- Optional to allow database defaults
        }

-- | Build the ParamType for a request body schema.
--
-- Handles:
-- * Object schemas - converted to ObjectParamType with properties
-- * Array schemas - for bulk inserts, uses ObjectParamType for items
-- * Primitive types - mapped directly
-- * References - treated as opaque object
buildBodyParamType :: Schema -> ParamType
buildBodyParamType schema =
    case schemaType schema of
        Just "object" ->
            case schemaProperties schema of
                Just props -> 
                    -- Build properties from schema
                    let paramProps = map schemaPropertyToParamProperty (Map.toList props)
                     in ObjectParamType paramProps
                Nothing -> 
                    -- Object without defined properties - opaque
                    OpaqueParamType "object"
        Just "array" ->
            case schemaItems schema of
                Just itemSchema ->
                    -- For arrays, we describe the item type
                    let itemType = buildBodyParamType itemSchema
                     in case itemType of
                            ObjectParamType props -> ObjectParamType props
                            _ -> OpaqueParamType "array"
                Nothing ->
                    OpaqueParamType "array"
        Just "string" -> StringParamType
        Just "integer" -> NumberParamType
        Just "number" -> NumberParamType
        Just "boolean" -> BoolParamType
        Just t -> OpaqueParamType t
        Nothing ->
            -- No type specified - check for properties or use opaque
            case schemaProperties schema of
                Just props -> 
                    let paramProps = map schemaPropertyToParamProperty (Map.toList props)
                     in ObjectParamType paramProps
                Nothing -> OpaqueParamType "object"

-- | Convert a schema property to a ParamProperty.
schemaPropertyToParamProperty :: (Text, Schema) -> ParamProperty
schemaPropertyToParamProperty (name, propSchema) =
    ParamProperty
        { propertyKey = name
        , propertyType = schemaToParamType propSchema
        , propertyDescription = fromMaybe ("Column " <> name) (schemaDescription propSchema)
        , propertyRequired = False  -- All body properties optional for flexibility
        }

-- | Convert a Schema to its corresponding ParamType.
schemaToParamType :: Schema -> ParamType
schemaToParamType schema =
    case schemaType schema of
        Just "string" -> 
            case schemaEnum schema of
                Just enumVals -> 
                    -- Convert enum values to text
                    let enumTexts = mapMaybe enumValueToText enumVals
                     in if null enumTexts then StringParamType else EnumParamType enumTexts
                Nothing -> StringParamType
        Just "integer" -> NumberParamType
        Just "number" -> NumberParamType
        Just "boolean" -> BoolParamType
        Just "object" ->
            case schemaProperties schema of
                Just props -> 
                    let paramProps = map schemaPropertyToParamProperty (Map.toList props)
                     in ObjectParamType paramProps
                Nothing -> OpaqueParamType "object"
        Just "array" ->
            case schemaItems schema of
                Just _ -> OpaqueParamType "array"
                Nothing -> OpaqueParamType "array"
        Just t -> OpaqueParamType t
        Nothing -> 
            case schemaProperties schema of
                Just props -> 
                    let paramProps = map schemaPropertyToParamProperty (Map.toList props)
                     in ObjectParamType paramProps
                Nothing -> OpaqueParamType "object"

-- | Convert an enum value to Text.
enumValueToText :: Aeson.Value -> Maybe Text
enumValueToText (Aeson.String s) = Just s
enumValueToText _ = Nothing

-- | Build a description for the body parameter.
buildBodyDescription :: Schema -> Text
buildBodyDescription schema =
    let baseDesc = fromMaybe "Request body" (schemaDescription schema)
        typeHint = case schemaType schema of
            Just "object" -> " (object with column values)"
            Just "array" -> " (array for bulk insert)"
            Just t -> " (type: " <> t <> ")"
            Nothing -> ""
        requiredHint = case schemaRequired schema of
            Just reqs | not (null reqs) -> ". Required fields: " <> Text.intercalate ", " reqs
            _ -> ""
     in baseDesc <> typeHint <> requiredHint

-- | Register all tools from a PostgREST toolbox.
--
-- This function iterates through all tools in the toolbox and attempts to
-- register each one.
--
-- If any registration fails, the entire operation fails fast with the first
-- error encountered.
--
-- For partial success handling, use 'registerPostgRESTool' on individual tools.
--
-- Example:
--
-- @
-- result <- PostgREST.initializeToolbox tracer config
-- case result of
--     Left err -> print err
--     Right toolbox -> do
--         regResult <- registerPostgRESTools toolbox
--         case regResult of
--             Left err -> putStrLn $ "Registration failed: " ++ err
--             Right registrations -> do
--                 -- Use registrations with agent runtime
--                 mapM_ (addToAgent agent) registrations
-- @
registerPostgRESTools ::
    PostgRESToolbox.Toolbox ->
    IO (Either String [ToolRegistration])
registerPostgRESTools toolbox =
    -- Fail fast - return first error encountered
    let registerAll :: [PostgRESTool] -> Either String [ToolRegistration] -> Either String [ToolRegistration]
        registerAll [] acc = acc
        registerAll (t:ts) (Right regs) =
            case registerPostgRESTool toolbox t of
                Left err -> Left err
                Right reg -> registerAll ts (Right (reg:regs))
        registerAll _ err = err
        
        tools = PostgRESToolbox.toolboxTools toolbox
     in pure $ case registerAll tools (Right []) of
            Left err -> Left err
            Right regs -> Right (reverse regs)

-- | Register a PostgREST tool in the LLM system (alias for 'registerPostgRESTool').
--
-- Provided for consistency with other registration functions.
registerPostgRESToolInLLM ::
    PostgRESToolbox.Toolbox ->
    PostgRESTool ->
    Either String ToolRegistration
registerPostgRESToolInLLM = registerPostgRESTool

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
        ret <- callTool toolbox desc (Just v)
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
                    True  -- MCP properties are required by default
        Aeson.Error err -> Left err
  where
    propMappingResult :: Aeson.Result PropertyHelper
    propMappingResult = Aeson.fromJSON val

data PropertyHelper
    = PropertyHelper {_type :: Text, _description :: Text}

instance Aeson.FromJSON PropertyHelper where
    parseJSON = Aeson.withObject "PropertyHelper" $ \o ->
        PropertyHelper <$> o Aeson..: "type" <*> o Aeson..: "description"

