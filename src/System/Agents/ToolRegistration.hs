{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Defines an LLM tool registration.

This module provides functionality for registering tools with the LLM system.
It supports registration of various tool types:

* Bash tools - External scripts executed in separate processes
* IO tools - Haskell functions executed in-process
* MCP tools - Tools exposed via Model Context Protocol
* OpenAPI tools - Tools generated from OpenAPI specifications
* PostgREST tools - Tools generated from PostgREST database APIs
* SQLite tools - Tools for executing SQL queries against SQLite databases
* System tools - Tools for gathering system information and file attachment
* Developer tools - Tools for writing/validating agents and tools
* Lua tools - Sandboxed Lua scripts for tool orchestration

For OpenAPI tools, special handling is done for name normalization:
OpenAPI operation IDs may contain invalid characters (dots, slashes, etc.)
which are normalized to LLM-safe names. The 'NameMapping' system maintains bidirectional mapping between normalized and original names.
-}
module System.Agents.ToolRegistration (
    -- * Core types
    Tool,
    Trace (..),
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
    registerSqliteTool,
    registerSqliteTools,
    registerSystemTool,
    registerSystemTools,
    registerDeveloperTool,
    registerDeveloperTools,
    registerLuaTool,
    registerLuaTools,

    -- * Naming policies
    io2LLMName,
    bash2LLMName,
    mcp2LLMName,
    openapi2LLMName,
    postgrest2LLMName,
    sqlite2LLMName,
    system2LLMName,
    developer2LLMName,
    lua2LLMName,

    -- * Tool builders
    luaTool,

    -- * Schema helpers
    mapArg,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Foldable.WithIndex (ifoldl')
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Prod.Tracer (Tracer, contramap)
import qualified Prod.Tracer as Prod
import System.Agents.Base (
    DeveloperToolCapability (..),
    DeveloperToolboxDescription (..),
    LuaToolboxDescription (..),
    SqliteToolboxDescription (..),
    SystemToolCapability (..),
    SystemToolboxDescription (..),
 )
import qualified System.Agents.LLMs.OpenAI as OpenAI
import qualified System.Agents.MCP.Base as Mcp
import qualified System.Agents.MCP.Client as McpClient
import System.Agents.ToolSchema
import System.Agents.Tools.Activation (Activation (..))
import System.Agents.Tools.Base (
    CallResult (..),
    ToolDef (..),
    mapToolResult,
 )
import qualified System.Agents.Tools.Base as ToolBase
import System.Agents.Tools.Bash (ScriptArg (..), ScriptDescription (..))
import qualified System.Agents.Tools.Bash as BashTools
import System.Agents.Tools.Context (ToolCall, ToolExecutionContext)
import qualified System.Agents.Tools.Context as Context
import qualified System.Agents.Tools.DeveloperToolbox as DeveloperTools
import System.Agents.Tools.IO (IOScript (..), IOScriptDescription (..))
import qualified System.Agents.Tools.IO as IOTools
import qualified System.Agents.Tools.LuaToolbox as LuaTools
import System.Agents.Tools.McpToolbox (callTool, mcpActivation)
import qualified System.Agents.Tools.McpToolbox as McpTools
import System.Agents.Tools.OpenAPI.Converter (
    normalizeForLLM,
 )
import qualified System.Agents.Tools.OpenAPI.Converter as OpenAPI
import System.Agents.Tools.OpenAPI.Types (Schema (..))
import System.Agents.Tools.OpenAPIToolbox (
    createToolHandler,
 )
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPIToolbox
import System.Agents.Tools.PostgREST.Converter (
    ColumnFilterSchema (..),
    FilterSchema (..),
    PostgRESTool (..),
    RankingSchema (..),
    SubsetSchema (..),
    ToolParameters (..),
    buildToolParameters,
    methodToText,
 )
import qualified System.Agents.Tools.PostgRESToolbox as PostgRESToolbox
import qualified System.Agents.Tools.SqliteToolbox as SqliteTools
import qualified System.Agents.Tools.SystemToolbox as SystemTools

type Tool call = ToolBase.Tool Trace call

data Trace
    = BashToolsRunTrace !BashTools.RunTrace
    | BashToolsLoadTrace !BashTools.LoadTrace
    | IOToolsTrace (IOTools.Trace Aeson.Value ByteString)
    | SqliteToolsTrace !SqliteTools.Trace
    | SystemToolsTrace !SystemTools.Trace
    | DeveloperToolsTrace !DeveloperTools.Trace
    | LuaToolsTrace !LuaTools.Trace
    | OpenAPIToolboxTrace !OpenAPIToolbox.Trace
    | PostgRESToolboxTrace !PostgRESToolbox.Trace
    deriving (Show)

-------------------------------------------------------------------------------

{- | We register tools that will take a ToolExecutionContext for execution.

The 'innerTool' field uses 'Tool ()' since the tool execution context
is passed at runtime via 'ToolBase.toolRun', not stored in the tool itself.
-}
data ToolRegistration
    = ToolRegistration
    { innerTool :: Tool ()
    , declareTool :: ToolDescription
    , findTool :: ToolCall -> Maybe (Tool ToolCall)
    , toolActivation :: Maybe Activation
    }

instance Show ToolRegistration where
    show tr =
        Prelude.unwords
            [ "ToolRegistration("
            , show tr.declareTool.toolDescriptionName
            , ", activation ="
            , show tr.toolActivation
            , ")"
            ]

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

{- | Convert an OpenAPI operation ID to an LLM tool name.

Names are prefixed with @openapi_@ and include the normalized toolbox name
and normalized operation ID to avoid conflicts and ensure LLM compatibility.

The operation ID is normalized to replace invalid characters:
- Dots (.) become underscores (_)
- Slashes (/) become underscores (_)
- Other invalid characters become underscores
- Names starting with digits are prefixed with 't'

Example:

>>> openapi2LLMName "myApi" "getPet"
ToolName {getToolName = "openapi_myApi_getPet"}

>>> openapi2LLMName "myApi" "pet.findByStatus"
ToolName {getToolName = "openapi_myApi_pet_findByStatus"}

>>> openapi2LLMName "myApi" "2.0/getPet"
ToolName {getToolName = "openapi_myApi_t2_0_getPet"}
-}
openapi2LLMName :: Text -> Text -> OpenAI.ToolName
openapi2LLMName tboxName operationId =
    let normalizedToolbox = normalizeForLLM tboxName
        normalizedOpId = normalizeForLLM operationId
     in OpenAI.ToolName ("openapi_" <> normalizedToolbox <> "_" <> normalizedOpId)

-- naming policy for PostgREST tools
postgrest2LLMName :: PostgRESToolbox.Toolbox -> PostgRESTool -> OpenAI.ToolName
postgrest2LLMName box tool =
    let normalizedToolbox = normalizeForLLM box.toolboxName
        -- Extract table name from path (e.g., "/users" -> "users")
        tableName = Text.dropWhile (== '/') tool.prtPath
        normalizedTable = normalizeForLLM tableName
        methodPart = Text.toLower $ methodToText tool.prtMethod
     in OpenAI.ToolName (mconcat ["postgrest_", normalizedToolbox, "_", methodPart, "_", normalizedTable])

{- | Naming policy for SQLite tools.

Generates an LLM-safe tool name in the format: @sqlite_{toolboxName}_{toolName}@
-}
sqlite2LLMName :: SqliteTools.Toolbox -> Text -> OpenAI.ToolName
sqlite2LLMName box tName =
    OpenAI.ToolName (mconcat ["sqlite_", box.toolboxName, "_", tName])

{- | Naming policy for System tools.

Generates an LLM-safe tool name in the format: @system_{toolboxName}_{toolName}@
-}
system2LLMName :: SystemTools.Toolbox -> Text -> OpenAI.ToolName
system2LLMName box tName =
    OpenAI.ToolName (mconcat ["system_", box.toolboxName, "_", tName])

{- | Naming policy for Developer tools.

Generates an LLM-safe tool name in the format: @developer_{toolboxName}_{toolName}@
-}
developer2LLMName :: DeveloperTools.Toolbox -> Text -> OpenAI.ToolName
developer2LLMName box tName =
    OpenAI.ToolName (mconcat ["developer_", box.toolboxName, "_", tName])

{- | Naming policy for Lua tools.

Generates an LLM-safe tool name in the format: @lua_{toolboxName}_{toolName}@
-}
lua2LLMName :: LuaTools.Toolbox -> Text -> OpenAI.ToolName
lua2LLMName box toolName =
    OpenAI.ToolName (mconcat ["lua_", box.toolboxName, "_", toolName])

-------------------------------------------------------------------------------

{- | Convert a ScriptArg to a ParamProperty for schema validation.

This function maps bash script arguments to the LLM schema format,
allowing validation of tool call payloads against the defined schema.
-}
mapArg :: ScriptArg -> ParamProperty
mapArg arg =
    ParamProperty
        { propertyKey = arg.argName
        , propertyType = OpaqueParamType arg.argBackingTypeString
        , propertyDescription = arg.argDescription
        , propertyRequired = True
        }

{- | Register a bash tool with the LLM system.
The activation is passed from the BashToolboxDescription and applied to all
scripts from that source.
-}
registerBashToolInLLM ::
    Maybe Activation ->
    ScriptDescription ->
    ToolRegistration
registerBashToolInLLM mbActivation script =
    let
        matchName :: ScriptDescription -> ToolCall -> Bool
        matchName bash call = getToolName (bash2LLMName bash) == call.callToolName

        mapToolDescriptionBash2LLM :: ScriptDescription -> ToolDescription
        mapToolDescriptionBash2LLM bash =
            ToolDescription
                { toolDescriptionName = bash2LLMName bash
                , toolDescriptionText = bash.scriptInfo.scriptDescription
                , toolDescriptionParamProperties = fmap mapArg bash.scriptInfo.scriptArgs
                }

        tool :: Tool ()
        tool = bashTool script

        find :: ToolCall -> Maybe (Tool ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing
     in
        ToolRegistration
            { innerTool = tool
            , declareTool = mapToolDescriptionBash2LLM script
            , findTool = find
            , toolActivation = mbActivation
            }

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
        matchName :: IOScript a b -> ToolCall -> Bool
        matchName io call = getToolName (io2LLMName io) == call.callToolName

        tool :: Tool ()
        tool = ioTool script

        find :: ToolCall -> Maybe (Tool ToolCall)
        find call = if matchName script call then Just (mapToolResult (const call) tool) else Nothing

        llmTool :: ToolDescription
        llmTool =
            ToolDescription
                { toolDescriptionName = io2LLMName script
                , toolDescriptionText = script.description.ioDescription
                , toolDescriptionParamProperties = llmProps
                }
     in
        ToolRegistration
            { innerTool = tool
            , declareTool = llmTool
            , findTool = find
            , toolActivation = Nothing
            }

-------------------------------------------------------------------------------

{- | Register an MCP tool with the LLM system.

Returns 'Left' if the tool's schema cannot be adapted to the LLM format.

The activation is extracted from the toolbox's 'mcpActivation' field,
allowing per-server progressive disclosure control.
-}
registerMcpToolInLLM ::
    McpTools.Toolbox ->
    McpTools.ToolDescription ->
    Either String ToolRegistration
registerMcpToolInLLM box mcp =
    let
        matchName :: McpTools.ToolDescription -> ToolCall -> Bool
        matchName td call = getToolName (mcp2LLMName box td) == call.callToolName

        llmBasedSchema :: Either String [ParamProperty]
        llmBasedSchema = adaptSchema mcp.getToolDescription.inputSchema

        llmName :: OpenAI.ToolName
        llmName = mcp2LLMName box mcp

        llmDescription :: Text
        llmDescription = Maybe.fromMaybe "" mcp.getToolDescription.description

        mapToolDescriptionMcp2LLM :: [ParamProperty] -> ToolDescription
        mapToolDescriptionMcp2LLM schema =
            ToolDescription
                { toolDescriptionName = llmName
                , toolDescriptionText = llmDescription
                , toolDescriptionParamProperties = schema
                }

        tool :: Tool ()
        tool = mcpTool box mcp

        find :: ToolCall -> Maybe (Tool ToolCall)
        find call = if matchName mcp call then Just (mapToolResult (const call) tool) else Nothing

        -- Extract activation from the toolbox configuration
        mbActivation = mcpActivation box
     in
        case llmBasedSchema of
            Right schema ->
                Right $
                    ToolRegistration
                        { innerTool = tool
                        , declareTool = mapToolDescriptionMcp2LLM schema
                        , findTool = find
                        , toolActivation = mbActivation
                        }
            Left err ->
                Left err

-------------------------------------------------------------------------------
-- OpenAPI Tool Registration
-------------------------------------------------------------------------------

{- | Register a single OpenAPI tool with the LLM system.

This function creates a 'ToolRegistration' from an OpenAPI tool and its
parent toolbox. The tool name is normalized for LLM compatibility.

The name normalization handles OpenAPI operation IDs that may contain
invalid characters (dots, slashes, etc.) by:
1. Replacing invalid characters with underscores
2. Ensuring the name starts with a letter
3. Using the toolbox's name mapping for bidirectional lookup

The activation is extracted from the toolbox's 'openApiActivation' field,
allowing per-toolbox progressive disclosure control.

Returns 'Left' if the tool cannot be registered.

Example:

@
case registerOpenAPITool toolbox apiTool of
    Left err -> putStrLn $ "Failed to register: " ++ err
    Right registration -> useWithAgent registration
@
-}
registerOpenAPITool ::
    OpenAPIToolbox.Toolbox ->
    OpenAPI.InternalTool ->
    Either String ToolRegistration
registerOpenAPITool toolbox tool =
    let
        -- Get the original operation ID
        originalOpId =
            Maybe.fromMaybe
                (OpenAPI.toolName tool)
                (OpenAPIToolbox.getOperationId (OpenAPI.toolOperation tool))

        -- Generate LLM name using the normalized operation ID
        llmName = openapi2LLMName (OpenAPIToolbox.toolboxName toolbox) originalOpId
        -- Convert to OpenAI Tool format with normalized name
        toolDescription =
            ToolDescription
                { toolDescriptionName = llmName
                , toolDescriptionText = OpenAPI.toolDescription tool
                , toolDescriptionParamProperties = OpenAPI.toolParamProperties tool
                }

        -- Create the tool handler that uses the mapping
        runFunc :: Tracer IO Trace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
        runFunc tracer ctx argz =
            createToolHandler toolbox tool (contramap OpenAPIToolboxTrace tracer) ctx argz

        -- Create the Tool
        toolDef0 =
            IOTool $
                IOScriptDescription
                    { ioSlug = OpenAI.getToolName llmName
                    , ioDescription = OpenAPI.toolDescription tool
                    }

        tool' =
            ToolBase.Tool
                { ToolBase.toolDef = toolDef0
                , ToolBase.toolRun = runFunc
                }

        -- Find function - matches on the normalized LLM name
        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool'
                else Nothing
     in
        Right $
            ToolRegistration
                { innerTool = mapToolResult (const ()) tool'
                , declareTool = toolDescription
                , findTool = find
                , toolActivation = OpenAPIToolbox.openApiActivation toolbox
                }

{- | Register all tools from an OpenAPI toolbox.

This function iterates through all tools in the toolbox and attempts to
register each one with normalized names for LLM compatibility.

If any registration fails, the entire operation fails fast with the first
error encountered.

For partial success handling, use 'registerOpenAPITool' on individual tools.

Example:

@
result <- OpenAPI.initializeToolbox tracer config
case result of
    Left err -> print err
    Right toolbox -> do
        regResult <- registerOpenAPITools toolbox
        case regResult of
            Left err -> putStrLn $ "Registration failed: " ++ err
            Right registrations -> do
                -- Use registrations with agent runtime
                mapM_ (addToAgent agent) registrations
@
-}
registerOpenAPITools ::
    OpenAPIToolbox.Toolbox ->
    IO (Either String [ToolRegistration])
registerOpenAPITools toolbox =
    -- Fail fast - return first error encountered
    let registerAll :: [OpenAPI.InternalTool] -> Either String [ToolRegistration] -> Either String [ToolRegistration]
        registerAll [] acc = acc
        registerAll (t : ts) (Right regs) =
            case registerOpenAPITool toolbox t of
                Left err -> Left err
                Right reg -> registerAll ts (Right (reg : regs))
        registerAll _ err = err

        tools = OpenAPIToolbox.toolboxTools toolbox
     in pure $ case registerAll tools (Right []) of
            Left err -> Left err
            Right regs -> Right (reverse regs)

{- | Register an OpenAPI tool in the LLM system (alias for 'registerOpenAPITool').

This is the original function name from the OpenAPIToolbox module,
provided for backward compatibility.
-}
registerOpenAPIToolInLLM ::
    OpenAPIToolbox.Toolbox ->
    OpenAPI.InternalTool ->
    Either String ToolRegistration
registerOpenAPIToolInLLM = registerOpenAPITool

-------------------------------------------------------------------------------
-- PostgREST Tool Registration
-------------------------------------------------------------------------------

{- | Register a single PostgREST tool with the LLM system.

This function creates a 'ToolRegistration' from a PostgREST tool and its
parent toolbox. The tool name follows the format:
postgrest_{toolbox}_{method}_{table}

The tool parameters are structured into groups:
* filters: Column-based row filters (for GET, DELETE, PATCH, PUT)
* subset: Pagination (limit/offset) and column selection (for GET)
* ranking: Ordering clause (for GET)
* body: JSON request body (for POST, PUT, PATCH)

All parameter groups and their sub-properties are marked as optional,
allowing the LLM to provide only the parameters it needs.

The activation is extracted from the toolbox's 'postgrestActivation' field,
allowing per-toolbox progressive disclosure control.

Returns 'Left' if the tool cannot be registered.

Example:

@
case registerPostgRESTool toolbox prTool of
    Left err -> putStrLn $ "Failed to register: " ++ err
    Right registration -> useWithAgent registration
@
-}
registerPostgRESTool ::
    PostgRESToolbox.Toolbox ->
    PostgRESTool ->
    Either String ToolRegistration
registerPostgRESTool toolbox tool =
    let llmName = postgrest2LLMName toolbox tool
        params = buildToolParameters tool

        -- Build parameter properties from structured parameters
        -- All parameters are marked as optional (propertyRequired = False)
        paramProps = buildPostgRESTParamProperties params

        -- Create the OpenAI Tool declaration
        toolDescription =
            ToolDescription
                { toolDescriptionName = llmName
                , toolDescriptionText = prtDescription tool
                , toolDescriptionParamProperties = paramProps
                }

        -- Create the tool handler
        runFunc :: Tracer IO Trace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
        runFunc tracer ctx argz =
            PostgRESToolbox.createToolHandler toolbox tool (Prod.contramap PostgRESToolboxTrace tracer) ctx argz

        -- Create the Tool definition
        toolDef0 =
            IOTool $
                IOScriptDescription
                    { ioSlug = OpenAI.getToolName llmName
                    , ioDescription = prtDescription tool
                    }

        tool' =
            ToolBase.Tool
                { ToolBase.toolDef = toolDef0
                , ToolBase.toolRun = runFunc
                }

        -- Find function - matches on the LLM name
        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool'
                else Nothing
     in Right $
            ToolRegistration
                { innerTool = mapToolResult (const ()) tool'
                , declareTool = toolDescription
                , findTool = find
                , toolActivation = PostgRESToolbox.postgrestActivation toolbox
                }

{- | Build parameter properties for PostgREST tool from structured parameters.

Parameter groups:
* filters: Column-based row filters (for GET, DELETE, PATCH, PUT)
* subset: Pagination (limit/offset) and column selection (for GET)
* ranking: Ordering clause (for GET)
* body: JSON request body (for POST, PUT, PATCH)

All parameter groups and their sub-properties are marked as optional
(propertyRequired = False), allowing the LLM to provide only the
parameters it needs.
-}
buildPostgRESTParamProperties :: ToolParameters -> [ParamProperty]
buildPostgRESTParamProperties params =
    let filterProp = case tpFilters params of
            Just fs ->
                Just $
                    ParamProperty
                        { propertyKey = "filters"
                        , propertyType = ObjectParamType (buildFilterSubProperties fs)
                        , propertyDescription = fsDescription fs
                        , propertyRequired = False -- Optional parameter group
                        }
            Nothing -> Nothing

        subsetProp = case tpSubset params of
            Just ss ->
                Just $
                    ParamProperty
                        { propertyKey = "subset"
                        , propertyType = ObjectParamType (buildSubsetSubProperties ss)
                        , propertyDescription = "Pagination and column selection"
                        , propertyRequired = False -- Optional parameter group
                        }
            Nothing -> Nothing

        rankingProp = case tpRanking params of
            Just rs ->
                Just $
                    ParamProperty
                        { propertyKey = "ranking"
                        , propertyType = ObjectParamType (buildRankingSubProperties rs)
                        , propertyDescription = "Result ordering"
                        , propertyRequired = False -- Optional parameter group
                        }
            Nothing -> Nothing

        -- Request body for write operations (POST, PUT, PATCH)
        bodyProp = case tpRequestBody params of
            Just schema ->
                Just $
                    ParamProperty
                        { propertyKey = "body"
                        , propertyType = buildBodyParamType schema
                        , propertyDescription = buildBodyDescription schema
                        , propertyRequired = False -- Optional - can insert with defaults
                        }
            Nothing -> Nothing
     in Maybe.catMaybes [filterProp, subsetProp, rankingProp, bodyProp]
  where
    buildFilterSubProperties :: FilterSchema -> [ParamProperty]
    buildFilterSubProperties fs =
        map
            ( \(col, schema) ->
                ParamProperty
                    { propertyKey = col
                    , propertyType = OpaqueParamType "string"
                    , propertyDescription = cfsDescription schema
                    , propertyRequired = False -- Optional filter property
                    }
            )
            (Map.toList $ fsProperties fs)

    buildSubsetSubProperties :: SubsetSchema -> [ParamProperty]
    buildSubsetSubProperties ss =
        Maybe.catMaybes
            [ fmap (\desc -> ParamProperty "offset" (OpaqueParamType "string") desc False) (ssOffset ss)
            , fmap (\desc -> ParamProperty "limit" (OpaqueParamType "string") desc False) (ssLimit ss)
            , fmap (\desc -> ParamProperty "columns" (OpaqueParamType "string") desc False) (ssColumns ss)
            ]

    buildRankingSubProperties :: RankingSchema -> [ParamProperty]
    buildRankingSubProperties rs =
        Maybe.catMaybes
            [ fmap (\desc -> ParamProperty "order" (OpaqueParamType "string") desc False) (rsOrder rs)
            ]

    -- Build the parameter type for the request body
    buildBodyParamType :: Schema -> ParamType
    buildBodyParamType schema =
        case schema.schemaType of
            Just "array" ->
                -- For bulk insert (array of objects)
                case schema.schemaItems of
                    Just itemSchema -> ObjectParamType (buildSchemaProperties itemSchema)
                    Nothing -> OpaqueParamType "object"
            Just "object" ->
                -- For single row insert
                ObjectParamType (buildSchemaProperties schema)
            _ -> OpaqueParamType "object"

    -- Build description for the request body parameter
    buildBodyDescription :: Schema -> Text
    buildBodyDescription schema =
        let baseDesc = Maybe.fromMaybe "JSON request body for insert/update operations" schema.schemaDescription
            typeHint = case schema.schemaType of
                Just "array" -> " (provide an array of objects for bulk insert, or a single object)"
                Just "object" -> " (provide a JSON object with column values)"
                _ -> ""
         in baseDesc <> typeHint

    -- Build properties from schema for object validation
    buildSchemaProperties :: Schema -> [ParamProperty]
    buildSchemaProperties schema =
        case schema.schemaProperties of
            Just props -> map (\(k, v) -> schemaToParamProperty k v) (Map.toList props)
            Nothing -> []

    -- Convert a schema property to ParamProperty
    schemaToParamProperty :: Text -> Schema -> ParamProperty
    schemaToParamProperty name schema =
        ParamProperty
            { propertyKey = name
            , propertyType = schemaTypeToParamType schema
            , propertyDescription = Maybe.fromMaybe (name <> " column value") schema.schemaDescription
            , propertyRequired = maybe False (name `elem`) schema.schemaRequired
            }

    -- Convert schema type to ParamType
    schemaTypeToParamType :: Schema -> ParamType
    schemaTypeToParamType schema =
        case schema.schemaType of
            Just "string" -> StringParamType
            Just "integer" -> NumberParamType
            Just "number" -> NumberParamType
            Just "boolean" -> BoolParamType
            Just "array" -> OpaqueParamType "array"
            Just "object" -> ObjectParamType (buildSchemaProperties schema)
            _ -> OpaqueParamType "string"

{- | Register all tools from a PostgREST toolbox.

This function iterates through all tools in the toolbox and attempts to
register each one.

If any registration fails, the entire operation fails fast with the first
error encountered.

For partial success handling, use 'registerPostgRESTool' on individual tools.

Example:

@
result <- PostgREST.initializeToolbox tracer config
case result of
    Left err -> print err
    Right toolbox -> do
        regResult <- registerPostgRESTools toolbox
        case regResult of
            Left err -> putStrLn $ "Registration failed: " ++ err
            Right registrations -> do
                -- Use registrations with agent runtime
                mapM_ (addToAgent agent) registrations
@
-}
registerPostgRESTools ::
    PostgRESToolbox.Toolbox ->
    IO (Either String [ToolRegistration])
registerPostgRESTools toolbox =
    -- Fail fast - return first error encountered
    let registerAll :: [PostgRESTool] -> Either String [ToolRegistration] -> Either String [ToolRegistration]
        registerAll [] acc = acc
        registerAll (t : ts) (Right regs) =
            case registerPostgRESTool toolbox t of
                Left err -> Left err
                Right reg -> registerAll ts (Right (reg : regs))
        registerAll _ err = err

        tools = PostgRESToolbox.toolboxTools toolbox
     in pure $ case registerAll tools (Right []) of
            Left err -> Left err
            Right regs -> Right (reverse regs)

{- | Register a PostgREST tool in the LLM system (alias for 'registerPostgRESTool').

Provided for consistency with other registration functions.
-}
registerPostgRESToolInLLM ::
    PostgRESToolbox.Toolbox ->
    PostgRESTool ->
    Either String ToolRegistration
registerPostgRESToolInLLM = registerPostgRESTool

-------------------------------------------------------------------------------
-- SQLite Tool Registration
-------------------------------------------------------------------------------

{- | Register a single SQLite tool with the LLM system.

SQLite tools expose a single 'query' function that accepts SQL.
The function name includes the toolbox name for uniqueness.

The activation is extracted from the toolbox configuration's
'sqliteToolboxActivation' field.
-}
registerSqliteTool ::
    SqliteTools.Toolbox ->
    Either String ToolRegistration
registerSqliteTool box =
    let
        -- Tool name is "query" since SQLite toolboxes expose a single query tool
        tName = "query"
        llmName = sqlite2LLMName box tName

        -- Single parameter: the SQL query
        paramProps =
            [ ParamProperty
                { propertyKey = "sql"
                , propertyType = StringParamType
                , propertyDescription = "SQL query to execute (SELECT for read-only, any valid SQL for read-write or snapshot)"
                , propertyRequired = True
                }
            ]

        toolDescription =
            ToolDescription
                { toolDescriptionName = llmName
                , toolDescriptionText = box.toolboxDescription
                , toolDescriptionParamProperties = paramProps
                }

        -- Create the tool
        tool' = sqliteTool box

        -- Find function - matches on the LLM name
        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool'
                else Nothing

        -- Extract activation from toolbox config
        mbActivation = (SqliteTools.toolboxConfig box).sqliteToolboxActivation
     in
        Right $
            ToolRegistration
                { innerTool = mapToolResult (const ()) tool'
                , declareTool = toolDescription
                , findTool = find
                , toolActivation = mbActivation
                }

{- | Register all tools from a SQLite toolbox.

Currently SQLite toolboxes expose a single query tool.
-}
registerSqliteTools ::
    SqliteTools.Toolbox ->
    IO (Either String [ToolRegistration])
registerSqliteTools box =
    pure $ case registerSqliteTool box of
        Left err -> Left err
        Right reg -> Right [reg]

-------------------------------------------------------------------------------
-- System Tool Registration
-------------------------------------------------------------------------------

{- | Register a SystemToolbox tool with the LLM system.

System tools expose functions based on configured capabilities.
The tool accepts a 'capability' parameter to select which information to retrieve,
and optional parameters:
- 'filepath' for the 'attach-file' capability
- 'session_id' for the 'read-session' capability
- 'query' for the 'search-sessions' capability
- 'take_n', 'drop_n', 'offset', 'limit' for session slicing (read-session)
- 'include_thinking', 'include_tool_responses' for content filtering (read-session)

The activation is extracted from the toolbox configuration's
'systemToolboxActivation' field.
-}
registerSystemTool ::
    SystemTools.Toolbox ->
    Either String ToolRegistration
registerSystemTool box =
    let
        tName = "system_info"
        llmName = system2LLMName box tName

        -- Build parameter properties based on enabled capabilities
        paramProps = buildSystemToolParams box

        toolDescription =
            ToolDescription
                { toolDescriptionName = llmName
                , toolDescriptionText = "Provides system information and file attachment: " <> box.toolboxDescription
                , toolDescriptionParamProperties = paramProps
                }

        -- Create the tool
        tool' = systemTool box

        -- Find function - matches on the LLM name
        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool'
                else Nothing

        -- Extract activation from toolbox config
        mbActivation = (SystemTools.toolboxConfig box).systemToolboxActivation
     in
        Right $
            ToolRegistration
                { innerTool = mapToolResult (const ()) tool'
                , declareTool = toolDescription
                , findTool = find
                , toolActivation = mbActivation
                }

-- | Build parameter properties for system tools based on enabled capabilities.
buildSystemToolParams :: SystemTools.Toolbox -> [ParamProperty]
buildSystemToolParams box =
    let
        baseParams =
            [ ParamProperty
                { propertyKey = "capability"
                , propertyType = StringParamType
                , propertyDescription = "Capability to query: " <> Text.intercalate ", " (map capabilityToText box.toolboxCapabilities)
                , propertyRequired = True
                }
            ]

        -- filepath parameter for attach-file capability
        filepathParam =
            ParamProperty
                { propertyKey = "filepath"
                , propertyType = StringParamType
                , propertyDescription = "For attach-file: Absolute path to the file to attach"
                , propertyRequired = False
                }

        -- session_id parameter for read-session capability
        sessionIdParam =
            ParamProperty
                { propertyKey = "session_id"
                , propertyType = StringParamType
                , propertyDescription = "For read-session: Session ID (UUID) to read"
                , propertyRequired = False
                }

        -- query parameter for search-sessions capability
        queryParam =
            ParamProperty
                { propertyKey = "query"
                , propertyType = StringParamType
                , propertyDescription = "For search-sessions: Search query string"
                , propertyRequired = False
                }

        -- Slicing parameters for read-session capability
        takeNParam =
            ParamProperty
                { propertyKey = "take_n"
                , propertyType = NumberParamType
                , propertyDescription = "For read-session: Take last N turns (alternative to offset/limit)"
                , propertyRequired = False
                }

        dropNParam =
            ParamProperty
                { propertyKey = "drop_n"
                , propertyType = NumberParamType
                , propertyDescription = "For read-session: Drop first N turns (alternative to offset)"
                , propertyRequired = False
                }

        offsetParam =
            ParamProperty
                { propertyKey = "offset"
                , propertyType = NumberParamType
                , propertyDescription = "For read-session: Starting turn index (0-based, alternative to drop_n)"
                , propertyRequired = False
                }

        limitParam =
            ParamProperty
                { propertyKey = "limit"
                , propertyType = NumberParamType
                , propertyDescription = "For read-session: Max turns to return (alternative to take_n)"
                , propertyRequired = False
                }

        -- Content control parameters for read-session capability
        includeThinkingParam =
            ParamProperty
                { propertyKey = "include_thinking"
                , propertyType = BoolParamType
                , propertyDescription = "For read-session: Include LLM thinking/reasoning content (default: false)"
                , propertyRequired = False
                }

        includeToolResponsesParam =
            ParamProperty
                { propertyKey = "include_tool_responses"
                , propertyType = BoolParamType
                , propertyDescription = "For read-session: Include tool call responses (default: false)"
                , propertyRequired = False
                }

        hasCapability cap = cap `elem` box.toolboxCapabilities

        -- Add optional parameters only if their respective capabilities are enabled
        optionalParams =
            (if hasCapability SystemToolAttachFile then [filepathParam] else [])
                ++ ( if hasCapability SystemToolReadSession
                        then
                            [ sessionIdParam
                            , takeNParam
                            , dropNParam
                            , offsetParam
                            , limitParam
                            , includeThinkingParam
                            , includeToolResponsesParam
                            ]
                        else []
                   )
                ++ (if hasCapability SystemToolSearchSessions then [queryParam] else [])
     in
        baseParams ++ optionalParams

-- Helper to convert capability to text
capabilityToText :: SystemToolCapability -> Text
capabilityToText SystemToolDate = "date"
capabilityToText SystemToolOperatingSystem = "operating-system"
capabilityToText SystemToolEnvVars = "env-vars"
capabilityToText SystemToolRunningUser = "running-user"
capabilityToText SystemToolHostname = "hostname"
capabilityToText SystemToolWorkingDirectory = "working-directory"
capabilityToText SystemToolProcessInfo = "process-info"
capabilityToText SystemToolUptime = "uptime"
capabilityToText SystemToolAttachFile = "attach-file"
capabilityToText SystemToolListSessions = "list-sessions"
capabilityToText SystemToolSearchSessions = "search-sessions"
capabilityToText SystemToolReadSession = "read-session"
capabilityToText SystemToolGetSessionStats = "get-session-stats"

{- | Register all tools from a System toolbox.

Currently System toolboxes expose a single system_info tool.
-}
registerSystemTools ::
    SystemTools.Toolbox ->
    IO (Either String [ToolRegistration])
registerSystemTools box =
    pure $ case registerSystemTool box of
        Left err -> Left err
        Right reg -> Right [reg]

-------------------------------------------------------------------------------
-- Developer Tool Registration
-------------------------------------------------------------------------------

{- | Register a DeveloperToolbox tool with the LLM system.

Developer tools expose functions based on configured capabilities:
* validate-tool: Validate a tool script
* scaffold-agent: Generate agent scaffolding
* scaffold-tool: Generate tool scaffolding
* show-spec: Display specification documentation
* read-file-range: Read specific line ranges from a file
* write-file-range: Replace line ranges in a file with new content
* patch-file: Apply a unified diff patch to a file

The tool accepts a 'capability' parameter to select which operation to perform.

The activation is extracted from the toolbox configuration's
'developerToolboxActivation' field.
-}
registerDeveloperTool ::
    DeveloperTools.Toolbox ->
    Either String ToolRegistration
registerDeveloperTool box =
    let
        tName = "developer_tools"
        llmName = developer2LLMName box tName

        -- Build parameter properties based on enabled capabilities
        paramProps = buildDeveloperToolParams box

        toolDescription =
            ToolDescription
                { toolDescriptionName = llmName
                , toolDescriptionText = "Developer tools for writing and validating agents and tools: " <> box.toolboxDescription
                , toolDescriptionParamProperties = paramProps
                }

        -- Create the tool
        tool' = developerTool box

        -- Find function - matches on the LLM name
        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool'
                else Nothing

        -- Extract activation from toolbox config
        mbActivation = (DeveloperTools.toolboxConfig box).developerToolboxActivation
     in
        Right $
            ToolRegistration
                { innerTool = mapToolResult (const ()) tool'
                , declareTool = toolDescription
                , findTool = find
                , toolActivation = mbActivation
                }

-- | Build parameter properties for developer tools based on enabled capabilities.
buildDeveloperToolParams :: DeveloperTools.Toolbox -> [ParamProperty]
buildDeveloperToolParams box =
    let baseParams =
            [ ParamProperty
                { propertyKey = "capability"
                , propertyType = StringParamType
                , propertyDescription = "Capability to execute: " <> Text.intercalate ", " (map devCapabilityToText box.toolboxCapabilities)
                , propertyRequired = True
                }
            , ParamProperty
                { propertyKey = "tool_path"
                , propertyType = StringParamType
                , propertyDescription = "For validate-tool: Path to tool script"
                , propertyRequired = False
                }
            , ParamProperty
                { propertyKey = "template"
                , propertyType = StringParamType
                , propertyDescription = "For scaffold-agent: Template (openai, mistral, ollama)"
                , propertyRequired = False
                }
            , ParamProperty
                { propertyKey = "language"
                , propertyType = StringParamType
                , propertyDescription = "For scaffold-tool: Language (bash, python, haskell)"
                , propertyRequired = False
                }
            , ParamProperty
                { propertyKey = "slug"
                , propertyType = StringParamType
                , propertyDescription = "For scaffold operations: Slug/name for the generated file"
                , propertyRequired = False
                }
            , ParamProperty
                { propertyKey = "file_path"
                , propertyType = StringParamType
                , propertyDescription = "For scaffold operations: Output file path"
                , propertyRequired = False
                }
            , ParamProperty
                { propertyKey = "force"
                , propertyType = BoolParamType
                , propertyDescription = "For scaffold operations: Overwrite existing files"
                , propertyRequired = False
                }
            , ParamProperty
                { propertyKey = "spec_name"
                , propertyType = StringParamType
                , propertyDescription = "For show-spec: Spec name (bash-tools)"
                , propertyRequired = False
                }
            ]

        -- Add read-file-range params if enabled
        readFileRangeParams =
            [ ParamProperty
                { propertyKey = "path"
                , propertyType = StringParamType
                , propertyDescription = "For read-file-range: Path to the file to read"
                , propertyRequired = False
                }
            , ParamProperty
                { propertyKey = "ranges"
                , propertyType = StringParamType
                , propertyDescription = "For read-file-range: Line ranges (e.g., '1-10', '5', 'head', 'tail'). Omit to read entire file."
                , propertyRequired = False
                }
            ]

        -- Add write-file-range params if enabled
        writeFileRangeParams =
            [ ParamProperty
                { propertyKey = "content"
                , propertyType = StringParamType
                , propertyDescription = "For write-file-range: Replacement content. Use '---' to separate multiple ranges."
                , propertyRequired = False
                }
            ]

        -- Add patch-file params if enabled
        patchFileParams =
            [ ParamProperty
                { propertyKey = "patch"
                , propertyType = StringParamType
                , propertyDescription = "For patch-file: Unified diff patch content to apply"
                , propertyRequired = False
                }
            ]

        hasCapability cap = cap `elem` box.toolboxCapabilities

        fileRangeParams =
            if hasCapability DevToolReadFileRange || hasCapability DevToolWriteFileRange
                then readFileRangeParams ++ writeFileRangeParams
                else []

        patchParams =
            if hasCapability DevToolPatchFile
                then patchFileParams
                else []
     in baseParams ++ fileRangeParams ++ patchParams

-- Helper to convert developer capability to text
devCapabilityToText :: DeveloperToolCapability -> Text
devCapabilityToText DevToolValidateTool = "validate-tool"
devCapabilityToText DevToolScaffoldAgent = "scaffold-agent"
devCapabilityToText DevToolScaffoldTool = "scaffold-tool"
devCapabilityToText DevToolShowSpec = "show-spec"
devCapabilityToText DevToolValidateAgent = "validate-agent"
devCapabilityToText DevToolCreateAgent = "create-agent"
devCapabilityToText DevToolCreateTool = "create-tool"
devCapabilityToText DevToolReadFileRange = "read-file-range"
devCapabilityToText DevToolWriteFileRange = "write-file-range"
devCapabilityToText DevToolPatchFile = "patch-file"

{- | Register all tools from a Developer toolbox.

Currently Developer toolboxes expose a single developer_tools tool.
-}
registerDeveloperTools ::
    DeveloperTools.Toolbox ->
    IO (Either String [ToolRegistration])
registerDeveloperTools box =
    pure $ case registerDeveloperTool box of
        Left err -> Left err
        Right reg -> Right [reg]

-------------------------------------------------------------------------------
-- Lua Tool Registration
-------------------------------------------------------------------------------

{- | Register a Lua toolbox with the LLM system.

Lua toolboxes expose:
* execute: Execute arbitrary Lua code

The 'execute' tool is always available.

The activation is extracted from the toolbox configuration's
'luaToolboxActivation' field.

Returns 'Left' if the tool cannot be registered.
-}
registerLuaTool ::
    LuaTools.Toolbox ->
    Either String ToolRegistration
registerLuaTool box =
    let
        toolName = "execute"
        llmName = lua2LLMName box toolName

        paramProps =
            [ ParamProperty
                { propertyKey = "script"
                , propertyType = StringParamType
                , propertyDescription = "Lua script source code to execute"
                , propertyRequired = True
                }
            , ParamProperty
                { propertyKey = "timeout"
                , propertyType = NumberParamType
                , propertyDescription = "Optional timeout override in seconds (default: toolbox configured timeout)"
                , propertyRequired = False
                }
            ]

        -- Extract the description from the toolbox config
        luaDescription = luaToolboxDescription (LuaTools.toolboxConfig box)

        toolDescription =
            ToolDescription
                { toolDescriptionName = llmName
                , toolDescriptionText = luaDescription
                , toolDescriptionParamProperties = paramProps
                }

        -- Create the tool
        tool' = luaTool box

        -- Find function - matches on the LLM name
        find :: ToolCall -> Maybe (Tool ToolCall)
        find call =
            if call.callToolName == getToolName llmName
                then Just $ mapToolResult (const call) tool'
                else Nothing

        -- Extract activation from toolbox config
        mbActivation = (LuaTools.toolboxConfig box).luaToolboxActivation
     in
        Right $
            ToolRegistration
                { innerTool = mapToolResult (const ()) tool'
                , declareTool = toolDescription
                , findTool = find
                , toolActivation = mbActivation
                }

{- | Register all tools from a Lua toolbox.

Lua toolboxes expose a single execute tool.
-}
registerLuaTools ::
    LuaTools.Toolbox ->
    IO (Either String [ToolRegistration])
registerLuaTools box =
    pure $ case registerLuaTool box of
        Left err -> Left err
        Right reg -> Right [reg]

{- | Builder for a Lua toolbox tool.

This creates a tool that executes Lua scripts in a sandboxed environment.
The tool accepts a 'script' parameter containing the Lua source code.

The tool execution:
1. Extracts the script from the arguments
2. Gets the tool portal from the execution context
3. Executes the script with portal access, passing the parent context for
   OS integration field propagation (World, EventQueue) to enable TUI
   visibility for nested subcalls
4. Returns the result as JSON

Errors are properly caught and returned as LuaToolError.
-}
luaTool :: LuaTools.Toolbox -> Tool ()
luaTool box =
    ToolBase.Tool
        { ToolBase.toolDef = LuaTool box.toolboxName
        , ToolBase.toolRun = run
        }
  where
    call = ()
    run tracer ctx (Aeson.Object v) = do
        -- Extract script from arguments
        case KeyMap.lookup (AesonKey.fromText "script") v of
            Just (Aeson.String script) -> do
                -- Execute script with portal, passing the parent context and tracer
                -- The parent context enables OS integration fields to propagate
                -- to nested subcalls for TUI visibility
                result <-
                    LuaTools.executeScriptWithPortal
                        (contramap LuaToolsTrace tracer)
                        box
                        script
                        ctx
                        (Context.ctxToolPortal ctx)

                case result of
                    Left err -> pure $ LuaToolError call (Text.pack $ show err)
                    Right execResult -> pure $ LuaToolResult call (Aeson.toJSON (LuaTools.resultValues execResult))
            _ ->
                pure $ LuaToolError call "Missing 'script' parameter or invalid type"
    run _tracer _ctx _ =
        pure $ LuaToolError call "Arguments must be a JSON object"

-------------------------------------------------------------------------------
-- Internal tool builders (copied from Tools to avoid import cycle)
-------------------------------------------------------------------------------

-- | Builder for a tool based on a Bash script-description.
bashTool ::
    ScriptDescription ->
    Tool ()
bashTool script =
    ToolBase.Tool
        { ToolBase.toolDef = BashTool script
        , ToolBase.toolRun = run
        }
  where
    call = ()
    run tracer ctx v = do
        ret <- BashTools.runValue (contramap BashToolsRunTrace tracer) script (Just ctx) v
        case ret of
            Left err -> pure $ BashToolError call err
            Right rsp -> pure $ BlobToolSuccess call rsp Nothing

-- | Builder for a tool based on an MCP toolbox tool.
mcpTool ::
    McpTools.Toolbox ->
    McpTools.ToolDescription ->
    Tool ()
mcpTool toolbox desc =
    ToolBase.Tool
        { ToolBase.toolDef = MCPTool desc
        , ToolBase.toolRun = run
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
    (Aeson.FromJSON a) =>
    IOScript a ByteString ->
    Tool ()
ioTool script =
    ToolBase.Tool
        { ToolBase.toolDef = IOTool script.description
        , ToolBase.toolRun = run
        }
  where
    call = ()
    run tracer ctx v = do
        let adaptTrace = IOToolsTrace . IOTools.adaptTraceInput (const v)
        ret <- IOTools.runValue (contramap adaptTrace tracer) script ctx v
        case ret of
            Left err -> pure $ IOToolError call err
            Right rsp -> pure $ BlobToolSuccess call rsp Nothing

-- | Builder for a tool based on a SQLite toolbox.
sqliteTool ::
    SqliteTools.Toolbox ->
    Tool ()
sqliteTool box =
    ToolBase.Tool
        { ToolBase.toolDef = SqliteTool toolDesc
        , ToolBase.toolRun = run
        }
  where
    call = ()

    -- Build tool description from toolbox
    toolDesc =
        SqliteTools.ToolDescription
            { SqliteTools.toolDescriptionName = "query"
            , SqliteTools.toolDescriptionDescription = box.toolboxDescription
            , SqliteTools.toolDescriptionToolboxName = box.toolboxName
            , SqliteTools.toolDescriptionDatabasePath = box.toolboxPath
            }

    run :: Tracer IO Trace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
    run tracer ctx (Aeson.Object v) = do
        case KeyMap.lookup (AesonKey.fromText "sql") v of
            Just (Aeson.String query) -> do
                -- Get conversation ID from context for snapshot mode
                let mConvId = Just (Context.ctxConversationId ctx)
                result <- SqliteTools.executeQueryWithContext (Prod.contramap SqliteToolsTrace tracer) box mConvId query
                case result of
                    Left err -> pure $ SqliteToolError call err
                    Right rsp -> pure $ SqliteToolResult call rsp
            _ -> pure $ SqliteToolError call (SqliteTools.SqlError "Missing 'sql' parameter or invalid type")
    run _tracer _ctx _ = do
        pure $ SqliteToolError call (SqliteTools.SqlError "Arguments must be a JSON object")

-- | Builder for a SystemToolbox-based tool.
systemTool :: SystemTools.Toolbox -> Tool ()
systemTool box =
    ToolBase.Tool
        { ToolBase.toolDef = SystemTool toolDesc
        , ToolBase.toolRun = run
        }
  where
    call = ()
    toolDesc =
        SystemTools.ToolDescription
            { SystemTools.toolDescriptionName = "system_info"
            , SystemTools.toolDescriptionDescription = box.toolboxDescription
            , SystemTools.toolDescriptionToolboxName = box.toolboxName
            }
    run :: Tracer IO Trace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
    run tracer _ctx (Aeson.Object v) = do
        case KeyMap.lookup (AesonKey.fromText "capability") v of
            Just (Aeson.String cap) -> do
                -- Check if this is the attach-file capability
                if cap == "attach-file"
                    then handleAttachFile tracer v
                    else do
                        -- Extract optional parameters for session introspection capabilities
                        let mSessionId = case KeyMap.lookup (AesonKey.fromText "session_id") v of
                                Just (Aeson.String sid) -> Just sid
                                _ -> Nothing
                        let mQuery = case KeyMap.lookup (AesonKey.fromText "query") v of
                                Just (Aeson.String q) -> Just q
                                _ -> Nothing

                        -- Extract read-session parameters
                        let mReadParams =
                                if cap == "read-session"
                                    then Just $ extractReadSessionParams v
                                    else Nothing

                        result <- SystemTools.executeQueryWithParams (Prod.contramap SystemToolsTrace tracer) box cap mSessionId mQuery mReadParams
                        case result of
                            Left err -> pure $ SystemToolError call err
                            Right rsp -> pure $ SystemToolResult call rsp
            _ -> pure $ SystemToolError call (SystemTools.SystemInfoError "Missing 'capability' parameter or invalid type")
    run _tracer _ctx _ = do
        pure $ SystemToolError call (SystemTools.SystemInfoError "Arguments must be a JSON object")

    -- Handle the attach-file capability
    handleAttachFile :: Tracer IO Trace -> Aeson.Object -> IO (CallResult ())
    handleAttachFile tracer params = do
        case KeyMap.lookup (AesonKey.fromText "filepath") params of
            Just (Aeson.String filePath) -> do
                result <- SystemTools.executeAttachFile (Prod.contramap SystemToolsTrace tracer) box (Text.unpack filePath)
                case result of
                    Left err -> pure $ SystemToolError call err
                    Right attachResult -> do
                        -- Convert AttachFileResult to MediaAttachment and return as BlobToolSuccess
                        let mediaType = SystemTools.detectMediaType (Text.unpack filePath)
                        case mediaType of
                            Nothing -> pure $ SystemToolError call (SystemTools.SystemInfoError "Could not detect media type")
                            Just mt -> do
                                -- Decode base64 data back to raw bytes for BlobToolSuccess
                                let base64Data = SystemTools.attachBase64Data attachResult
                                case B64.decode (Text.encodeUtf8 base64Data) of
                                    Left err ->
                                        pure $ SystemToolError call (SystemTools.SystemInfoError $ "Failed to decode base64: " <> Text.pack err)
                                    Right bytes ->
                                        pure $ BlobToolSuccess call bytes (Just mt)
            _ -> pure $ SystemToolError call (SystemTools.SystemInfoError "Missing 'filepath' parameter for attach-file capability")

    -- Extract read-session parameters from the tool call arguments
    extractReadSessionParams :: Aeson.Object -> SystemTools.ReadSessionParams
    extractReadSessionParams v =
        SystemTools.ReadSessionParams
            { SystemTools.rspTakeN = parseIntParam "take_n" v
            , SystemTools.rspDropN = parseIntParam "drop_n" v
            , SystemTools.rspOffset = parseIntParam "offset" v
            , SystemTools.rspLimit = parseIntParam "limit" v
            , SystemTools.rspIncludeThinking = parseBoolParam "include_thinking" v
            , SystemTools.rspIncludeToolResponses = parseBoolParam "include_tool_responses" v
            }

    -- Parse an optional integer parameter from the JSON object
    parseIntParam :: Text -> Aeson.Object -> Maybe Int
    parseIntParam key obj =
        case KeyMap.lookup (AesonKey.fromText key) obj of
            Just (Aeson.Number n) -> Just (round n)
            _ -> Nothing

    -- Parse an optional boolean parameter from the JSON object
    parseBoolParam :: Text -> Aeson.Object -> Bool
    parseBoolParam key obj =
        case KeyMap.lookup (AesonKey.fromText key) obj of
            Just (Aeson.Bool b) -> b
            _ -> False

-- | Builder for a DeveloperToolbox-based tool.
developerTool :: DeveloperTools.Toolbox -> Tool ()
developerTool box =
    ToolBase.Tool
        { ToolBase.toolDef = DeveloperTool toolDesc
        , ToolBase.toolRun = run
        }
  where
    call = ()
    toolDesc =
        DeveloperTools.ToolDescription
            { DeveloperTools.toolDescriptionName = "developer_tools"
            , DeveloperTools.toolDescriptionDescription = box.toolboxDescription
            , DeveloperTools.toolDescriptionToolboxName = box.toolboxName
            }
    run :: Tracer IO Trace -> ToolExecutionContext -> Aeson.Value -> IO (CallResult ())
    run tracer _ctx (Aeson.Object v) = do
        case KeyMap.lookup (AesonKey.fromText "capability") v of
            Just (Aeson.String cap) -> executeDeveloperCapability tracer box cap v
            _ -> pure $ DeveloperToolError call (DeveloperTools.ValidationError "Missing 'capability' parameter or invalid type")
    run _tracer _ctx _ = do
        pure $ DeveloperToolError call (DeveloperTools.ValidationError "Arguments must be a JSON object")

-- | Execute a developer tool capability
executeDeveloperCapability :: Tracer IO Trace -> DeveloperTools.Toolbox -> Text -> Aeson.Object -> IO (CallResult ())
executeDeveloperCapability tracer box cap params = case cap of
    "validate-tool" -> do
        case KeyMap.lookup (AesonKey.fromText "tool_path") params of
            Just (Aeson.String tPath) -> do
                result <- DeveloperTools.executeValidateTool (Prod.contramap BashToolsLoadTrace tracer) box (Text.unpack tPath)
                case result of
                    Left err -> pure $ DeveloperToolError () err
                    Right valResult -> pure $ DeveloperToolResult () valResult
            _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'tool_path' parameter")
    "scaffold-agent" -> do
        let mTemplate = case KeyMap.lookup (AesonKey.fromText "template") params of
                Just (Aeson.String t) -> t
                _ -> "openai"
        let mSlug = case KeyMap.lookup (AesonKey.fromText "slug") params of
                Just (Aeson.String s) -> s
                _ -> "new-agent"
        let mFilePath = case KeyMap.lookup (AesonKey.fromText "file_path") params of
                Just (Aeson.String fp) -> Text.unpack fp
                _ -> "new-agent.json"
        let mForce = case KeyMap.lookup (AesonKey.fromText "force") params of
                Just (Aeson.Bool f) -> f
                _ -> False
        result <- DeveloperTools.executeScaffoldAgent box mTemplate mSlug mFilePath mForce
        case result of
            Left err -> pure $ DeveloperToolError () err
            Right scaffoldResult -> pure $ DeveloperToolScaffoldResult () scaffoldResult
    "scaffold-tool" -> do
        let mLang = case KeyMap.lookup (AesonKey.fromText "language") params of
                Just (Aeson.String l) -> l
                _ -> "bash"
        let mSlug = case KeyMap.lookup (AesonKey.fromText "slug") params of
                Just (Aeson.String s) -> s
                _ -> "new-tool"
        let mFilePath = case KeyMap.lookup (AesonKey.fromText "file_path") params of
                Just (Aeson.String fp) -> Text.unpack fp
                _ -> "new-tool.sh"
        let mForce = case KeyMap.lookup (AesonKey.fromText "force") params of
                Just (Aeson.Bool f) -> f
                _ -> False
        result <- DeveloperTools.executeScaffoldTool box mLang mSlug mFilePath mForce
        case result of
            Left err -> pure $ DeveloperToolError () err
            Right scaffoldResult -> pure $ DeveloperToolScaffoldResult () scaffoldResult
    "show-spec" -> do
        case KeyMap.lookup (AesonKey.fromText "spec_name") params of
            Just (Aeson.String specName) -> do
                result <- DeveloperTools.executeShowSpec box specName
                case result of
                    Left err -> pure $ DeveloperToolError () err
                    Right content -> pure $ DeveloperToolSpecResult () content
            _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'spec_name' parameter")
    "read-file-range" -> do
        case KeyMap.lookup (AesonKey.fromText "path") params of
            Just (Aeson.String filePath) -> do
                let ranges = case KeyMap.lookup (AesonKey.fromText "ranges") params of
                        Just (Aeson.String r) -> r
                        _ -> ""
                result <- DeveloperTools.executeReadFileRange (Prod.contramap DeveloperToolsTrace tracer) box (Text.unpack filePath) ranges
                case result of
                    Left err -> pure $ DeveloperToolError () err
                    Right readResult -> pure $ DeveloperToolReadFileRangeResult () readResult
            _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'path' parameter")
    "write-file-range" -> do
        case KeyMap.lookup (AesonKey.fromText "path") params of
            Just (Aeson.String filePath) -> do
                case KeyMap.lookup (AesonKey.fromText "ranges") params of
                    Just (Aeson.String ranges) -> do
                        let content = case KeyMap.lookup (AesonKey.fromText "content") params of
                                Just (Aeson.String c) -> c
                                _ -> ""
                        -- Split content on '---' separator to get list of content blocks
                        let contentBlocks = Text.splitOn "---" content
                        result <- DeveloperTools.executeWriteFileRange (Prod.contramap DeveloperToolsTrace tracer) box (Text.unpack filePath) ranges contentBlocks
                        case result of
                            Left err -> pure $ DeveloperToolError () err
                            Right writeResult -> pure $ DeveloperToolWriteFileRangeResult () writeResult
                    _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'ranges' parameter")
            _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'path' parameter")
    "patch-file" -> do
        case KeyMap.lookup (AesonKey.fromText "path") params of
            Just (Aeson.String filePath) -> do
                case KeyMap.lookup (AesonKey.fromText "patch") params of
                    Just (Aeson.String patchContent) -> do
                        result <- DeveloperTools.executePatchFile (Prod.contramap DeveloperToolsTrace tracer) box (Text.unpack filePath) patchContent
                        case result of
                            Left err -> pure $ DeveloperToolError () err
                            Right patchResult -> pure $ DeveloperToolPatchResult () patchResult
                    _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'patch' parameter")
            _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError "Missing 'path' parameter")
    _ -> pure $ DeveloperToolError () (DeveloperTools.ValidationError $ "Unknown capability: " <> cap)

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
                    True -- MCP properties are required by default
        Aeson.Error err -> Left err
  where
    propMappingResult :: Aeson.Result PropertyHelper
    propMappingResult = Aeson.fromJSON val

data PropertyHelper
    = PropertyHelper {_type :: Text, _description :: Text}

instance Aeson.FromJSON PropertyHelper where
    parseJSON = Aeson.withObject "PropertyHelper" $ \o ->
        PropertyHelper <$> o Aeson..: "type" <*> o Aeson..: "description"

