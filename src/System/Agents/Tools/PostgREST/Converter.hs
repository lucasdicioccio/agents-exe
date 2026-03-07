{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | PostgREST to Tool conversion logic.
--
-- This module converts PostgREST OpenAPI specifications into executable
-- tool representations. It handles:
--
-- * Parsing PostgREST-specific OpenAPI specs
-- * Converting table endpoints to tools (GET operations)
-- * Extracting row filters from rowFilter. parameter references
-- * Building structured parameter schemas (filters/subset/ranking)
-- * Name normalization for LLM compatibility
--
-- Key differences from generic OpenAPI:
-- * Tools are named postgrest_{toolbox}_{table} instead of openapi_{operationId}
-- * GET parameters are structured into filters/subset/ranking groups
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
import Data.Maybe (catMaybes, mapMaybe)
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
-- 2. Extracts GET operations for each table
-- 3. Detects row filters from parameter references
-- 4. Builds structured tool parameters
-- 5. Generates LLM-compatible tool names
--
-- Only GET operations are supported in the initial implementation.
-- POST/PUT/PATCH/DELETE can be added later.
--
-- Example:
-- >>> let spec = OpenAPISpec (Map.fromList [("/users", Map.fromList [("GET", someOp)])]) Nothing
-- >>> let tools = convertPostgRESToTools "mydb" spec
-- >>> length tools
-- 1
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
    -- Only process GET operations for now
    (method, operation) <- Map.toList methods
    guard (method == "GET")
    -- Extract row filters from the operation
    let rowFilters = extractRowFilters operation spec
    pure $ convertTable toolboxName path method operation rowFilters
  where
    guard :: Bool -> [()]
    guard True = [()]
    guard False = []

-- | Converts a single PostgREST table endpoint to a tool.
--
-- This is the core conversion function that:
-- * Derives a tool name from the table path
-- * Builds the tool description from summary and description
-- * Creates structured parameters (filters/subset/ranking)
-- * Records detected row filters
--
-- The tool name format is: postgrest_{toolbox}_{table}
convertTable ::
    -- | Toolbox name
    Text ->
    -- | Table path (e.g., "/users")
    Path ->
    -- | HTTP method (e.g., "GET")
    Method ->
    -- | OpenAPI operation
    Operation ->
    -- | Detected row filters
    [RowFilter] ->
    PostgRESTool
convertTable toolboxName path method op rowFilters =
    PostgRESTool
        { prtPath = path
        , prtMethod = method
        , prtName = deriveToolName toolboxName path method
        , prtDescription = buildToolDescription op path
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
-- Combines operation summary/description with table path information.
buildToolDescription :: Operation -> Path -> Text
buildToolDescription op path =
    let baseDesc = case (opSummary op, opDescription op) of
            (Just summary, Just desc) -> summary <> ":\n" <> desc
            (Just summary, Nothing) -> summary
            (Nothing, Just desc) -> desc
            (Nothing, Nothing) -> "Query " <> tableName <> " table"
        tableName = Text.dropWhile (== '/') path
     in baseDesc <> "\nTable: " <> tableName

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
-- Format: postgrest_{toolbox}_{table}
--
-- Examples:
-- >>> deriveToolName "mydb" "/users" "GET"
-- "postgrest_mydb_get_users"
-- >>> deriveToolName "mydb" "/public.orders" "GET"
-- "postgrest_mydb_get_public_orders"
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

-- | Build structured tool parameters from operation.
--
-- Creates three parameter groups:
-- * filters: Column-based row filters
-- * subset: Pagination (limit/offset) and column selection
-- * ranking: Ordering clause
buildToolParameters :: PostgRESTool -> ToolParameters
buildToolParameters tool =
    ToolParameters
        { tpFilters = buildFilterSchema tool
        , tpSubset = buildSubsetSchema tool
        , tpRanking = buildRankingSchema tool
        }

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

