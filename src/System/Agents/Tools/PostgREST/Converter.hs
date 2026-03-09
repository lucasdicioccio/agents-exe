{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | PostgREST to Tool conversion logic.
--
-- This module converts PostgREST OpenAPI specifications into executable
-- tool representations. It handles:
--
-- * Parsing PostgREST-specific OpenAPI specs
-- * Converting table endpoints to tools (GET, POST, PUT, PATCH operations)
-- * Extracting row filters from rowFilter. parameter references
-- * Building structured parameter schemas (filters/subset/ranking/body)
-- * Name normalization for LLM compatibility
--
-- Key differences from generic OpenAPI:
-- * Tools are named postgrest_{toolbox}_{table} instead of openapi_{operationId}
-- * GET parameters are structured into filters/subset/ranking groups
-- * POST/PUT/PATCH operations include request body schema for data insertion/updates
-- * Row filtering uses column-based parameters from the spec
-- * The root / path and OpenAPI description endpoints are skipped
--
-- Example:
-- >>> import qualified Data.Map.Strict as Map
-- >>> let tool = convertTable "/users" True True [RowFilter "name" "text" "Filter on name"]
-- >>> prtName tool
-- "postgrest_get_users"
module System.Agents.Tools.PostgREST.Converter (
    -- * Core types
    PostgRESTool (..),
    ToolParameters (..),
    FilterSchema (..),
    ColumnFilterSchema (..),
    SubsetSchema (..),
    RankingSchema (..),
    
    -- * Main conversion functions
    convertPostgRESToTools,
    convertTable,
    
    -- * Row filter extraction
    extractRowFilters,
    parseRowFilterRef,
    
    -- * Parameter building
    buildToolParameters,
    buildFilterSchema,
    buildSubsetSchema,
    buildRankingSchema,
    
    -- * Request body handling
    extractRequestBody,
    resolveSchemaRef,
    
    -- * Name generation
    deriveToolName,
    normalizeTableName,
    
    -- * Path filtering
    isTablePath,
    shouldSkipPath,
    
    -- * Re-export for convenience
    module System.Agents.Tools.PostgREST.Types,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isLetter)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Tools.OpenAPI.Types (
    Method,
    OpenAPISpec (..),
    Operation (..),
    ParamLocation (..),
    Parameter (..),
    Path,
    Schema (..),
    Components (..),
    RequestBody (..),
 )
import System.Agents.Tools.OpenAPI.Converter (normalizeForLLM)
import System.Agents.Tools.PostgREST.Types

-- -------------------------------------------------------------------------
-- Main Conversion Functions
-- -------------------------------------------------------------------------

-- | Converts a PostgREST OpenAPI spec into a list of tools.
--
-- This function:
-- 1. Filters out non-table paths (root /, OpenAPI endpoints)
-- 2. Extracts GET, POST, PUT, and PATCH operations for each table
-- 3. Detects row filters from parameter references
-- 4. Builds structured tool parameters including request body for write operations
-- 5. Generates LLM-compatible tool names
--
-- Supports GET for querying and POST/PUT/PATCH for write operations.
convertPostgRESToTools ::
    -- | Toolbox name (used for tool naming)
    Text ->
    OpenAPISpec ->
    [PostgRESTool]
convertPostgRESToTools toolboxName spec = do
    (path, methods) <- Map.toList (specPaths spec)
    -- Skip non-table paths
    guard (isTablePath path)
    guard (not (shouldSkipPath path))
    -- Process supported operations: GET, POST, PUT, PATCH
    (method, operation) <- Map.toList methods
    guard (method `elem` supportedMethods)
    -- Extract row filters from the operation
    let rowFilters = extractRowFilters operation spec
    pure $ convertTable toolboxName path method operation rowFilters spec
  where
    guard :: Bool -> [()]
    guard True = [()]
    guard False = []
    
    supportedMethods :: [Text]
    supportedMethods = ["GET", "POST", "PUT", "PATCH"]

-- | Converts a single PostgREST table endpoint to a tool.
--
-- This is the core conversion function that:
-- * Derives a tool name from the table path and method
-- * Builds the tool description from summary and description
-- * Creates structured parameters (filters/subset/ranking/body)
-- * Records detected row filters
--
-- The tool name format is: postgrest_{toolbox}_{method}_{table}
convertTable ::
    -- | Toolbox name
    Text ->
    -- | Table path (e.g., "/users")
    Path ->
    -- | HTTP method (e.g., "GET", "POST")
    Method ->
    -- | OpenAPI operation
    Operation ->
    -- | Detected row filters
    [RowFilter] ->
    -- | OpenAPI spec for resolving schema references
    OpenAPISpec ->
    PostgRESTool
convertTable toolboxName path method op rowFilters spec =
    PostgRESTool
        { prtPath = path
        , prtMethod = method
        , prtName = deriveToolName toolboxName path method
        , prtDescription = buildToolDescription op path method
        , prtRowFilters = rowFilters
        , prtHasPagination = hasPaginationParams op
        , prtHasOrdering = hasOrderingParam op
        }

-- -------------------------------------------------------------------------
-- Path Filtering
-- -------------------------------------------------------------------------

-- | Check if a path represents a table endpoint.
--
-- Table paths in PostgREST:
-- * Start with / (all paths do)
-- * Don't contain {parameters} in the main table path
-- * Are not the root path /
--
-- Examples:
-- >>> isTablePath "/users"
-- True
-- >>> isTablePath "/"
-- False
-- >>> isTablePath "/users/{id}"
-- True  -- We still consider this a table path (single row access)
-- >>> isTablePath "/rpc/some_function"
-- True  -- RPC endpoints are also valid
isTablePath :: Path -> Bool
isTablePath path =
    path /= "/" && not (isOpenApiEndpoint path)

-- | Check if a path should be skipped.
--
-- Skip paths that are:
-- * The root OpenAPI spec endpoint
-- * Swagger UI endpoints
-- * Internal PostgREST endpoints
--
-- Examples:
-- >>> shouldSkipPath "/"
-- False  -- Root is handled separately
-- >>> shouldSkipPath "/api/swagger.json"
-- True
shouldSkipPath :: Path -> Bool
shouldSkipPath path =
    path `elem`
        [ "/api/swagger.json"
        , "/swagger.json"
        , "/api/swagger-ui"
        , "/swagger-ui"
        , "/api/swagger-ui.html"
        ]

-- | Check if path is the OpenAPI description endpoint.
--
-- PostgREST serves the OpenAPI spec at the root / endpoint.
-- This should not be exposed as a tool.
isOpenApiEndpoint :: Path -> Bool
isOpenApiEndpoint path =
    -- The root path serves the OpenAPI spec in PostgREST
    path == "/"

-- -------------------------------------------------------------------------
-- Row Filter Extraction
-- -------------------------------------------------------------------------

-- | Extract row filters from an operation's parameters.
--
-- PostgREST exposes column filters via parameter references like:
-- * #/parameters/rowFilter.users.id
-- * #/parameters/rowFilter.public.orders.name
--
-- This function parses these references and extracts column information.
--
-- Examples:
-- >>> let param = Parameter "id" ParamInQuery (Just "Filter on id") False (Just (Schema (Just "integer") Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
-- >>> extractRowFilters (Operation Nothing Nothing Nothing [param] Nothing) (OpenAPISpec Map.empty Nothing)
-- []
extractRowFilters :: Operation -> OpenAPISpec -> [RowFilter]
extractRowFilters op spec =
    -- Look for parameters with rowFilter references
    let filterRefs = extractFilterRefs op
     in mapMaybe (resolveFilterRef spec) filterRefs

-- | Extract filter reference strings from operation parameters.
--
-- Scans parameter schemas for $ref entries pointing to rowFilter definitions.
extractFilterRefs :: Operation -> [Text]
extractFilterRefs op =
    catMaybes $ map extractRefFromParam (opParameters op)
  where
    extractRefFromParam :: Parameter -> Maybe Text
    extractRefFromParam param =
        case paramSchema param of
            Just schema -> schemaRef schema
            Nothing -> Nothing

    catMaybes = mapMaybe id

-- | Parse a rowFilter reference string.
--
-- PostgREST rowFilter references look like:
-- * #/parameters/rowFilter.users.id
-- * #/parameters/rowFilter.public.orders.name
--
-- Returns (table, column) tuple if parsing succeeds.
--
-- Examples:
-- >>> parseRowFilterRef "#/parameters/rowFilter.users.id"
-- Just ("users", "id")
-- >>> parseRowFilterRef "#/parameters/rowFilter.public.orders.name"
-- Just ("public.orders", "name")
-- >>> parseRowFilterRef "#/parameters/something.else"
-- Nothing
parseRowFilterRef :: Text -> Maybe (Text, Text)
parseRowFilterRef ref =
    -- Format: #/parameters/rowFilter.{table}.{column}
    let parts = Text.splitOn "." ref
     in case parts of
            ["#/parameters/rowFilter", table, column] ->
                Just (table, column)
            ["#/parameters/rowFilter", schema, table, column] ->
                -- Schema-qualified table: public.users
                Just (schema <> "." <> table, column)
            _ -> Nothing

-- | Resolve a filter reference to a RowFilter.
--
-- Looks up the parameter definition in the spec to get type information.
resolveFilterRef :: OpenAPISpec -> Text -> Maybe RowFilter
resolveFilterRef _spec ref =
    -- For now, parse the reference and create a basic RowFilter
    -- In a full implementation, we'd look up the parameter definition
    -- in specComponents to get the actual type
    case parseRowFilterRef ref of
        Just (table, column) ->
            Just $
                RowFilter
                    { rfColumn = column
                    , rfType = "unknown" -- Would lookup from spec
                    , rfDescription = "Filter on " <> column <> " column"
                    }
        Nothing -> Nothing

-- -------------------------------------------------------------------------
-- Tool Description Building
-- -------------------------------------------------------------------------

-- | Build a tool description from operation and path.
--
-- Combines operation summary/description with table path and method information.
buildToolDescription :: Operation -> Path -> Method -> Text
buildToolDescription op path method =
    let baseDesc = case (opSummary op, opDescription op) of
            (Just summary, Just desc) -> summary <> ":\n" <> desc
            (Just summary, Nothing) -> summary
            (Nothing, Just desc) -> desc
            (Nothing, Nothing) -> defaultDescription
        tableName = Text.dropWhile (== '/') path
        defaultDescription = case method of
            "GET" -> "Query " <> tableName <> " table"
            "POST" -> "Insert new row into " <> tableName <> " table"
            "PUT" -> "Update row in " <> tableName <> " table (full replacement)"
            "PATCH" -> "Update row(s) in " <> tableName <> " table (partial update)"
            _ -> "Operate on " <> tableName <> " table"
     in baseDesc <> "\nTable: " <> tableName <> "\nMethod: " <> method

-- -------------------------------------------------------------------------
-- Parameter Detection
-- -------------------------------------------------------------------------

-- | Check if operation has pagination parameters.
--
-- Looks for limit and offset parameters.
hasPaginationParams :: Operation -> Bool
hasPaginationParams op =
    let paramNames = map paramName (opParameters op)
     in "limit" `elem` paramNames && "offset" `elem` paramNames

-- | Check if operation has ordering parameter.
--
-- Looks for order parameter.
hasOrderingParam :: Operation -> Bool
hasOrderingParam op =
    let paramNames = map paramName (opParameters op)
     in "order" `elem` paramNames

-- -------------------------------------------------------------------------
-- Tool Name Derivation
-- -------------------------------------------------------------------------

-- | Derives a tool name from a PostgREST table path.
--
-- Format: postgrest_{toolbox}_{method}_{table}
--
-- Examples:
-- >>> deriveToolName "mydb" "/users" "GET"
-- "postgrest_mydb_get_users"
-- >>> deriveToolName "mydb" "/public.orders" "POST"
-- "postgrest_mydb_post_public_orders"
deriveToolName :: Text -> Path -> Method -> Text
deriveToolName toolboxName path method =
    let normalizedToolbox = normalizeForLLM toolboxName
        tablePart = normalizeTableName path
        methodPart = Text.toLower method
     in "postgrest_" <> normalizedToolbox <> "_" <> methodPart <> "_" <> tablePart

-- | Normalize a table path for use in a tool name.
--
-- Converts paths like:
-- * /users -> users
-- * /public.users -> public_users
-- * /rpc/my_function -> rpc_my_function
--
-- Examples:
-- >>> normalizeTableName "/users"
-- "users"
-- >>> normalizeTableName "/public.users"
-- "public_users"
-- >>> normalizeTableName "/rpc/my_function"
-- "rpc_my_function"
normalizeTableName :: Path -> Text
normalizeTableName path =
    let -- Remove leading slash
        withoutLeading = Text.dropWhile (== '/') path
        -- Replace dots and slashes with underscores
        normalized =
            Text.replace "/" "_"
                . Text.replace "." "_"
                $ withoutLeading
        -- Clean up multiple consecutive underscores
        collapsed = collapseUnderscores normalized
     in ensureValidStart collapsed
  where
    collapseUnderscores :: Text -> Text
    collapseUnderscores = Text.intercalate "_" . filter (not . Text.null) . Text.splitOn "_"

    ensureValidStart :: Text -> Text
    ensureValidStart t
        | Text.null t = "table"
        | isLetter (Text.head t) = t
        | otherwise = "t_" <> t

-- -------------------------------------------------------------------------
-- Parameter Schema Building
-- -------------------------------------------------------------------------

-- | Build structured tool parameters from operation and spec.
--
-- Creates parameter groups based on the HTTP method:
-- * All methods: filters, subset, ranking (for GET-like filtering)
-- * POST/PUT/PATCH: body parameter for the request payload
--
-- The request body schema is extracted from the operation and resolved
-- using the spec's components/schemas section.
buildToolParameters :: PostgRESTool -> OpenAPISpec -> ToolParameters
buildToolParameters tool spec =
    ToolParameters
        { tpFilters = buildFilterSchema tool
        , tpSubset = buildSubsetSchema tool
        , tpRanking = buildRankingSchema tool
        , tpRequestBody = Nothing  -- Will be populated by caller using extractRequestBody
        }

-- | Extract request body schema for write operations (POST/PUT/PATCH).
--
-- Looks for a request body in the operation and resolves any $ref
-- references to get the actual schema definition.
--
-- For PostgREST Swagger 2.0 specs, request bodies are often defined
-- via body.* parameters that reference definitions.
extractRequestBody :: Operation -> OpenAPISpec -> Maybe Schema
extractRequestBody op spec =
    -- First try to get from opRequestBody (OpenAPI 3.x style)
    case opRequestBody op of
        Just reqBody -> getJsonSchema reqBody spec
        Nothing -> extractBodyFromParameters op spec

-- | Get JSON schema from request body content.
getJsonSchema :: RequestBody -> OpenAPISpec -> Maybe Schema
getJsonSchema reqBody spec =
    -- Look for application/json content type
    case Map.lookup "application/json" (reqBodyContent reqBody) of
        Just schema -> Just (resolveSchemaRef spec schema)
        Nothing -> 
            -- Try first available content type as fallback
            case Map.toList (reqBodyContent reqBody) of
                ((_, schema):_) -> Just (resolveSchemaRef spec schema)
                [] -> Nothing

-- | Extract request body schema from body.* parameters (Swagger 2.0 style).
--
-- PostgREST Swagger specs define request bodies as parameters like:
-- {"$ref": "#/parameters/body.items"}
extractBodyFromParameters :: Operation -> OpenAPISpec -> Maybe Schema
extractBodyFromParameters op spec =
    -- Look for body parameter references
    let bodyParamRefs = extractBodyParamRefs op
     in case bodyParamRefs of
            (ref:_) -> resolveBodyParameterRef spec ref
            [] -> Nothing

-- | Extract body parameter reference strings from operation.
extractBodyParamRefs :: Operation -> [Text]
extractBodyParamRefs op =
    mapMaybe extractBodyRef (opParameters op)
  where
    extractBodyRef :: Parameter -> Maybe Text
    extractBodyRef param =
        case paramSchema param of
            Just schema -> 
                case schemaRef schema of
                    Just ref | "#/parameters/body." `Text.isPrefixOf` ref -> Just ref
                    _ -> Nothing
            Nothing -> Nothing

-- | Resolve a body parameter reference to its schema.
resolveBodyParameterRef :: OpenAPISpec -> Text -> Maybe Schema
resolveBodyParameterRef spec ref =
    -- For body.* parameters, the schema reference points to definitions
    -- e.g., #/parameters/body.items -> we need to find the schema reference
    -- within that parameter definition
    --
    -- In the spec, the parameter would be defined in the parameters section
    -- with a schema that references a definition
    --
    -- For now, we extract the table name from the ref and look it up
    -- in the definitions
    let tableName = Text.drop (Text.length "#/parameters/body.") ref
        -- Look in components/schemas for a matching definition
     in lookupDefinition spec tableName

-- | Look up a table definition in the spec's components/schemas.
lookupDefinition :: OpenAPISpec -> Text -> Maybe Schema
lookupDefinition spec name =
    case specComponents spec of
        Just components ->
            case componentsSchemas components of
                Just schemas -> Map.lookup name schemas
                Nothing -> Nothing
        Nothing -> Nothing

-- | Resolve schema references to actual schemas.
--
-- If the schema has a $ref, look it up in the spec's components/schemas.
-- Otherwise, return the schema as-is.
resolveSchemaRef :: OpenAPISpec -> Schema -> Schema
resolveSchemaRef spec schema =
    case schemaRef schema of
        Just ref -> 
            -- Extract definition name from ref like "#/definitions/items"
            let defName = Text.dropWhile (== '#') $ Text.dropWhile (/= '/') $ Text.dropWhile (== '/') ref
                cleanName = Text.dropWhile (== '/') defName
                -- Handle both "definitions/" and "components/schemas/" prefixes
                name = if "definitions/" `Text.isPrefixOf` cleanName
                          then Text.drop (Text.length "definitions/") cleanName
                          else if "components/schemas/" `Text.isPrefixOf` cleanName
                               then Text.drop (Text.length "components/schemas/") cleanName
                               else cleanName
             in fromMaybe schema (lookupDefinition spec name)
        Nothing -> schema

-- | Build filter schema from detected row filters.
--
-- Each row filter becomes a property in the filters object.
buildFilterSchema :: PostgRESTool -> Maybe FilterSchema
buildFilterSchema tool =
    if null (prtRowFilters tool)
        then Nothing
        else
            Just $
                FilterSchema
                    { fsDescription = "Row filters on columns. Use PostgREST operators like eq., gt., lt., like., etc."
                    , fsProperties = Map.fromList $ map filterToProperty (prtRowFilters tool)
                    }
  where
    filterToProperty :: RowFilter -> (Text, ColumnFilterSchema)
    filterToProperty rf =
        ( rfColumn rf
        , ColumnFilterSchema
            { cfsType = "string"
            , cfsDescription = rfDescription rf <> " (type: " <> rfType rf <> ")"
            }
        )

-- | Build subset schema for pagination and column selection.
--
-- Includes:
-- * offset: Number of rows to skip
-- * limit: Maximum rows to return
-- * columns: Comma-separated column names (select parameter)
buildSubsetSchema :: PostgRESTool -> Maybe SubsetSchema
buildSubsetSchema tool =
    if prtHasPagination tool
        then
            Just $
                SubsetSchema
                    { ssOffset = Just "Number of rows to skip"
                    , ssLimit = Just "Maximum number of rows to return"
                    , ssColumns = Just "Comma-separated column names to return (e.g., 'id,name,email')"
                    }
        else Nothing

-- | Build ranking schema for ordering.
--
-- Includes the order parameter for sorting results.
buildRankingSchema :: PostgRESTool -> Maybe RankingSchema
buildRankingSchema tool =
    if prtHasOrdering tool
        then
            Just $
                RankingSchema
                    { rsOrder = Just "Ordering clause (e.g., 'created_at.desc,name.asc')"
                    }
        else Nothing

