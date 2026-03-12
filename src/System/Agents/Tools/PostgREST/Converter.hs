{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | PostgREST to Tool conversion logic.

This module converts PostgREST OpenAPI specifications into executable
tool representations. It handles:

* Parsing PostgREST-specific OpenAPI specs
* Converting table endpoints to tools (GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS)
* Extracting row filters from rowFilter. parameter references
* Building structured parameter schemas (filters/subset/ranking)
* Name normalization for LLM compatibility

Key differences from generic OpenAPI:
* Tools are named postgrest_{toolbox}_{method}_{table} instead of openapi_{operationId}
* GET parameters are structured into filters/subset/ranking groups
* Row filtering uses column-based parameters from the spec
* The root / path and OpenAPI description endpoints are skipped

Example:
>>> import qualified Data.Map.Strict as Map
>>> let tool = convertTable "/users" GET True True [] Nothing
>>> prtName tool
"postgrest_get_users"
-}
module System.Agents.Tools.PostgREST.Converter (
    -- * Core types
    PostgRESTool (..),
    ToolParameters (..),
    FilterSchema (..),
    ColumnFilterSchema (..),
    SubsetSchema (..),
    RankingSchema (..),
    HttpMethod (..),
    methodToText,

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

    -- * Name generation
    deriveToolName,
    normalizeTableName,

    -- * Path filtering
    isTablePath,
    shouldSkipPath,

    -- * Method helpers
    methodToOperation,
    isMethodSupported,

    -- * Body parameter resolution (for PostgREST pattern)
    extractRequestBodySchema,
    resolveBodyParameterRef,
    isBodyParamRef,

    -- * Re-export for convenience
    module System.Agents.Tools.PostgREST.Types,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isLetter)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Tools.OpenAPI.Converter (normalizeForLLM)
import System.Agents.Tools.OpenAPI.Types (
    Method,
    OpenAPISpec (..),
    Operation (..),
    ParamLocation (..),
    Parameter (..),
    Path,
    RequestBody (..),
    Schema (..),
 )
import System.Agents.Tools.PostgREST.Types

-- -------------------------------------------------------------------------
-- Main Conversion Functions
-- -------------------------------------------------------------------------

{- | Converts a PostgREST OpenAPI spec into a list of tools.

This function:
1. Filters out non-table paths (root /, OpenAPI endpoints)
2. Extracts operations for each allowed HTTP method
3. Detects row filters from parameter references
4. Builds structured tool parameters
5. Generates LLM-compatible tool names

The 'allowedMethods' parameter controls which HTTP verbs are exposed
as tools. By default, only read-only methods are included for safety.

Example:
>>> let spec = OpenAPISpec (Map.fromList [("/users", Map.fromList [("GET", someOp)])]) Nothing Nothing
>>> let tools = convertPostgRESToTools "mydb" [GET] spec
>>> length tools
1
-}
convertPostgRESToTools ::
    -- | Toolbox name (used for tool naming)
    Text ->
    -- | Allowed HTTP methods to expose
    [HttpMethod] ->
    OpenAPISpec ->
    [PostgRESTool]
convertPostgRESToTools toolboxName allowedMethods spec = do
    (path, methods) <- Map.toList (specPaths spec)
    -- Skip non-table paths
    guard (isTablePath path)
    guard (not (shouldSkipPath path))
    -- Process all allowed methods
    method <- allowedMethods
    -- Convert OpenAPI Method to HttpMethod for comparison
    case methodToOperation method methods of
        Just operation -> do
            -- Extract row filters from the operation
            let rowFilters = extractRowFilters operation spec
            -- Extract request body schema for write operations (with PostgREST resolution)
            let requestBodySchema = extractRequestBodySchemaWithSpec spec operation
            pure $ convertTable toolboxName path method operation rowFilters requestBodySchema
        Nothing -> []
  where
    guard :: Bool -> [()]
    guard True = [()]
    guard False = []

-- | Find an operation matching the given HTTP method.
methodToOperation :: HttpMethod -> Map Method Operation -> Maybe Operation
methodToOperation method methods = Map.lookup (methodToText method) methods

-- | Check if a method should be supported based on the operation.
isMethodSupported :: HttpMethod -> Operation -> Bool
isMethodSupported _ _ = True

{- | Extract request body schema from an operation.
This is the standard extraction that doesn't use the spec.
-}
extractRequestBodySchema :: Operation -> Maybe Schema
extractRequestBodySchema op = do
    reqBody <- opRequestBody op
    -- Get the JSON schema from the request body content
    Map.lookup "application/json" (reqBodyContent reqBody)

{- | Extract request body schema with PostgREST-specific resolution.

PostgREST uses a two-level reference pattern:
1. Operation references: {"$ref": "#/parameters/body.tablename"}
2. Parameter def has: {"in": "body", "schema": {"$ref": "#/definitions/..."}}

This function first tries standard extraction, then falls back to
resolving the PostgREST body parameter pattern.
-}
extractRequestBodySchemaWithSpec :: OpenAPISpec -> Operation -> Maybe Schema
extractRequestBodySchemaWithSpec spec op =
    -- First try standard extraction
    case extractRequestBodySchema op of
        Just schema -> Just schema
        Nothing -> resolvePostgRESTBodyRef spec op

{- | Resolve PostgREST body parameter reference.

Looks for parameters with $ref to #/parameters/body.* and resolves
them against the spec's parameters section.
-}
resolvePostgRESTBodyRef :: OpenAPISpec -> Operation -> Maybe Schema
resolvePostgRESTBodyRef spec op = do
    -- Find a parameter that is a body parameter reference
    bodyParamRef <- find isBodyParamRef (opParameters op)
    -- Get the reference string (e.g., "#/parameters/body.trainers")
    ref <- getParamRef bodyParamRef
    -- Resolve the reference against the spec's parameters
    resolveBodyParameterRef spec ref

{- | Check if a parameter is a body parameter reference.
Pattern: $ref = "#/parameters/body.something"
-}
isBodyParamRef :: Parameter -> Bool
isBodyParamRef param =
    case paramSchema param >>= schemaRef of
        Just ref -> "#/parameters/body." `Text.isPrefixOf` ref
        Nothing -> False

-- | Get the $ref value from a parameter.
getParamRef :: Parameter -> Maybe Text
getParamRef param = paramSchema param >>= schemaRef

{- | Resolve a body parameter reference to get the actual schema.

Given a reference like "#/parameters/body.trainers", this function:
1. Looks up the parameter definition in specParameters
2. Verifies it's a body parameter
3. Returns the schema from that parameter definition
-}
resolveBodyParameterRef :: OpenAPISpec -> Text -> Maybe Schema
resolveBodyParameterRef spec ref = do
    -- Extract the parameter name from the reference
    -- Format: #/parameters/body.tablename
    paramName <- Text.stripPrefix "#/parameters/" ref
    -- Get the parameters section
    params <- specParameters spec
    -- Look up the parameter definition
    paramDef <- Map.lookup paramName params
    -- Verify it's a body parameter
    guard (paramIn paramDef == ParamInBody)
    -- Return the schema from the parameter definition
    paramSchema paramDef
  where
    guard :: Bool -> Maybe ()
    guard True = Just ()
    guard False = Nothing

{- | Converts a single PostgREST table endpoint to a tool.

This is the core conversion function that:
* Derives a tool name from the table path and method
* Builds the tool description from summary and description
* Creates structured parameters (filters/subset/ranking)
* Records detected row filters
* Includes request body schema for write operations

The tool name format is: postgrest_{toolbox}_{method}_{table}
-}
convertTable ::
    -- | Toolbox name
    Text ->
    -- | Table path (e.g., "/users")
    Path ->
    -- | HTTP method
    HttpMethod ->
    -- | OpenAPI operation
    Operation ->
    -- | Detected row filters
    [RowFilter] ->
    -- | Request body schema (for POST/PUT/PATCH)
    Maybe Schema ->
    PostgRESTool
convertTable toolboxName path method op rowFilters requestBodySchema =
    PostgRESTool
        { prtPath = path
        , prtMethod = method
        , prtName = deriveToolName toolboxName path method
        , prtDescription = buildToolDescription op path method
        , prtRowFilters = rowFilters
        , prtHasPagination = hasPaginationParams op
        , prtHasOrdering = hasOrderingParam op
        , prtRequestBodySchema = requestBodySchema
        , prtIsReadOnly = isReadOnlyMethod method
        }

-- -------------------------------------------------------------------------
-- Path Filtering
-- -------------------------------------------------------------------------

{- | Check if a path represents a table endpoint.

Table paths in PostgREST:
* Start with / (all paths do)
* Don't contain {parameters} in the main table path
* Are not the root path /

Examples:
>>> isTablePath "/users"
True
>>> isTablePath "/"
False
>>> isTablePath "/users/{id}"
True  -- We still consider this a table path (single row access)
>>> isTablePath "/rpc/some_function"
True  -- RPC endpoints are also valid
-}
isTablePath :: Path -> Bool
isTablePath path =
    path /= "/" && not (isOpenApiEndpoint path)

{- | Check if a path should be skipped.

Skip paths that are:
* The root OpenAPI spec endpoint
* Swagger UI endpoints
* Internal PostgREST endpoints

Examples:
>>> shouldSkipPath "/"
False  -- Root is handled separately
>>> shouldSkipPath "/api/swagger.json"
True
-}
shouldSkipPath :: Path -> Bool
shouldSkipPath path =
    path
        `elem` [ "/api/swagger.json"
               , "/swagger.json"
               , "/api/swagger-ui"
               , "/swagger-ui"
               , "/api/swagger-ui.html"
               ]

{- | Check if path is the OpenAPI description endpoint.

PostgREST serves the OpenAPI spec at the root / endpoint.
This should not be exposed as a tool.
-}
isOpenApiEndpoint :: Path -> Bool
isOpenApiEndpoint path =
    -- The root path serves the OpenAPI spec in PostgREST
    path == "/"

-- -------------------------------------------------------------------------
-- Row Filter Extraction
-- -------------------------------------------------------------------------

{- | Extract row filters from an operation's parameters.

PostgREST exposes column filters via parameter references like:
* #/parameters/rowFilter.users.id
* #/parameters/rowFilter.public.orders.name

This function parses these references and extracts column information.

Examples:
>>> let param = Parameter "id" ParamInQuery (Just "Filter on id") False (Just (Schema (Just "integer") Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
>>> extractRowFilters (Operation Nothing Nothing Nothing [param] Nothing) (OpenAPISpec Map.empty Nothing Nothing)
[]
-}
extractRowFilters :: Operation -> OpenAPISpec -> [RowFilter]
extractRowFilters op spec =
    -- Look for parameters with rowFilter references
    let filterRefs = extractFilterRefs op
     in mapMaybe (resolveFilterRef spec) filterRefs

{- | Extract filter reference strings from operation parameters.

Scans parameter schemas for $ref entries pointing to rowFilter definitions.
-}
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

{- | Parse a rowFilter reference string.

PostgREST rowFilter references look like:
* #/parameters/rowFilter.users.id
* #/parameters/rowFilter.public.orders.name

Returns (table, column) tuple if parsing succeeds.

Examples:
>>> parseRowFilterRef "#/parameters/rowFilter.users.id"
Just ("users", "id")
>>> parseRowFilterRef "#/parameters/rowFilter.public.orders.name"
Just ("public.orders", "name")
>>> parseRowFilterRef "#/parameters/something.else"
Nothing
-}
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

{- | Resolve a filter reference to a RowFilter.

Looks up the parameter definition in the spec to get type information.
-}
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

{- | Build a tool description from operation and path.

Combines operation summary/description with table path and method information.
-}
buildToolDescription :: Operation -> Path -> HttpMethod -> Text
buildToolDescription op path method =
    let baseDesc = case (opSummary op, opDescription op) of
            (Just summary, Just desc) -> summary <> ":\n" <> desc
            (Just summary, Nothing) -> summary
            (Nothing, Just desc) -> desc
            (Nothing, Nothing) -> defaultAction method <> " " <> tableName
        tableName = Text.dropWhile (== '/') path
        methodDesc = case method of
            GET -> "Query rows from"
            HEAD -> "Check existence/count in"
            POST -> "Create new rows in"
            PUT -> "Upsert (update or insert) rows in"
            PATCH -> "Partially update rows in"
            DELETE -> "Delete rows from"
            OPTIONS -> "Get metadata about"
     in baseDesc <> "\nTable: " <> tableName <> "\nMethod: " <> methodToText method
  where
    defaultAction :: HttpMethod -> Text
    defaultAction GET = "Query"
    defaultAction HEAD = "Check"
    defaultAction POST = "Create"
    defaultAction PUT = "Upsert"
    defaultAction PATCH = "Update"
    defaultAction DELETE = "Delete"
    defaultAction OPTIONS = "Get metadata for"

-- -------------------------------------------------------------------------
-- Parameter Detection
-- -------------------------------------------------------------------------

{- | Check if operation has pagination parameters.

Looks for limit and offset parameters.
-}
hasPaginationParams :: Operation -> Bool
hasPaginationParams op =
    let paramNames = map paramName (opParameters op)
     in "limit" `elem` paramNames && "offset" `elem` paramNames

{- | Check if operation has ordering parameter.

Looks for order parameter.
-}
hasOrderingParam :: Operation -> Bool
hasOrderingParam op =
    let paramNames = map paramName (opParameters op)
     in "order" `elem` paramNames

-- -------------------------------------------------------------------------
-- Tool Name Derivation
-- -------------------------------------------------------------------------

{- | Derives a tool name from a PostgREST table path.

Format: postgrest_{toolbox}_{method}_{table}

Examples:
>>> deriveToolName "mydb" "/users" GET
"postgrest_mydb_get_users"
>>> deriveToolName "mydb" "/public.orders" POST
"postgrest_mydb_post_public_orders"
-}
deriveToolName :: Text -> Path -> HttpMethod -> Text
deriveToolName toolboxName path method =
    let normalizedToolbox = normalizeForLLM toolboxName
        tablePart = normalizeTableName path
        methodPart = Text.toLower $ methodToText method
     in "postgrest_" <> normalizedToolbox <> "_" <> methodPart <> "_" <> tablePart

{- | Normalize a table path for use in a tool name.

Converts paths like:
* /users -> users
* /public.users -> public_users
* /rpc/my_function -> rpc_my_function

Examples:
>>> normalizeTableName "/users"
"users"
>>> normalizeTableName "/public.users"
"public_users"
>>> normalizeTableName "/rpc/my_function"
"rpc_my_function"
-}
normalizeTableName :: Path -> Text
normalizeTableName path =
    let
        -- Remove leading slash
        withoutLeading = Text.dropWhile (== '/') path
        -- Replace dots and slashes with underscores
        normalized =
            Text.replace "/" "_"
                . Text.replace "." "_"
                $ withoutLeading
        -- Clean up multiple consecutive underscores
        collapsed = collapseUnderscores normalized
     in
        ensureValidStart collapsed
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

{- | Build structured tool parameters from operation.

Creates parameter groups:
* filters: Column-based row filters (for GET, DELETE, PATCH, PUT)
* subset: Pagination (limit/offset) and column selection (for GET)
* ranking: Ordering clause (for GET)
* requestBody: JSON body for write operations (POST, PUT, PATCH)
-}
buildToolParameters :: PostgRESTool -> ToolParameters
buildToolParameters tool =
    ToolParameters
        { tpFilters = buildFilterSchema tool
        , tpSubset = if needsSubset then buildSubsetSchema tool else Nothing
        , tpRanking = if needsRanking then buildRankingSchema tool else Nothing
        , tpRequestBody = if needsRequestBody then prtRequestBodySchema tool else Nothing
        }
  where
    method = prtMethod tool
    -- Subset and ranking only for GET (and HEAD for counting)
    needsSubset = method == GET && prtHasPagination tool
    needsRanking = method == GET && prtHasOrdering tool
    -- Request body for POST, PUT, PATCH
    needsRequestBody = method `elem` [POST, PUT, PATCH]

{- | Build filter schema from detected row filters.

Each row filter becomes a property in the filters object.
-}
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

{- | Build subset schema for pagination and column selection.

Includes:
* offset: Number of rows to skip
* limit: Maximum rows to return
* columns: Comma-separated column names (select parameter)
-}
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

{- | Build ranking schema for ordering.

Includes the order parameter for sorting results.
-}
buildRankingSchema :: PostgRESTool -> Maybe RankingSchema
buildRankingSchema tool =
    if prtHasOrdering tool
        then
            Just $
                RankingSchema
                    { rsOrder = Just "Ordering clause (e.g., 'created_at.desc,name.asc')"
                    }
        else Nothing
