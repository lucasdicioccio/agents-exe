{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | PostgREST-specific types for database API mapping.

PostgREST (https://postgrest.org) is a standalone web server that turns
PostgreSQL databases directly into RESTful APIs. This module provides
types for:

* Configuration for connecting to PostgREST instances
* Row filter definitions for column-based filtering
* Tool representations for database tables
* Structured parameter schemas for filters, subsetting, and ordering

Unlike generic OpenAPI, PostgREST:
* Maps endpoints directly to database tables (e.g., /users, /orders)
* Supports special query parameters: limit, offset, select, order
* Exposes column-based filtering via rowFilter. parameter references
* Requires special handling for the OpenAPI description endpoint itself
-}
module System.Agents.Tools.PostgREST.Types (
    -- * Configuration
    Config (..),
    defaultAllowedMethods,

    -- * HTTP Methods
    HttpMethod (..),
    methodToText,
    textToMethod,
    isReadOnlyMethod,

    -- * Row filtering
    RowFilter (..),

    -- * Trace events
    Trace (..),

    -- * Tool representation
    PostgRESTool (..),

    -- * Parameter schemas
    ToolParameters (..),
    FilterSchema (..),
    ColumnFilterSchema (..),
    SubsetSchema (..),
    RankingSchema (..),

    -- * Tool result
    ToolResult (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Tools.OpenAPI.Types (Schema)

-- -------------------------------------------------------------------------
-- HTTP Methods
-- -------------------------------------------------------------------------

{- | HTTP methods supported by PostgREST.

PostgREST supports the full range of HTTP verbs for RESTful operations:
* GET - Query rows
* HEAD - Check existence/count without body
* POST - Create new rows
* PUT - Upsert (update or insert)
* PATCH - Partial update
* DELETE - Remove rows
* OPTIONS - Get metadata about endpoint
-}
data HttpMethod
    = GET
    | HEAD
    | POST
    | PUT
    | PATCH
    | DELETE
    | OPTIONS
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert HttpMethod to Text.
methodToText :: HttpMethod -> Text
methodToText GET = "GET"
methodToText HEAD = "HEAD"
methodToText POST = "POST"
methodToText PUT = "PUT"
methodToText PATCH = "PATCH"
methodToText DELETE = "DELETE"
methodToText OPTIONS = "OPTIONS"

-- | Parse HttpMethod from Text (case-insensitive).
textToMethod :: Text -> Maybe HttpMethod
textToMethod t = case Text.toUpper t of
    "GET" -> Just GET
    "HEAD" -> Just HEAD
    "POST" -> Just POST
    "PUT" -> Just PUT
    "PATCH" -> Just PATCH
    "DELETE" -> Just DELETE
    "OPTIONS" -> Just OPTIONS
    _ -> Nothing

instance ToJSON HttpMethod where
    toJSON = Aeson.String . methodToText

instance FromJSON HttpMethod where
    parseJSON = Aeson.withText "HttpMethod" $ \t ->
        case textToMethod t of
            Just m -> pure m
            Nothing -> fail $ "Unknown HTTP method: " ++ Text.unpack t

{- | Check if a method is read-only (safe, no side effects).

Read-only methods: GET, HEAD, OPTIONS
Write methods: POST, PUT, PATCH, DELETE
-}
isReadOnlyMethod :: HttpMethod -> Bool
isReadOnlyMethod GET = True
isReadOnlyMethod HEAD = True
isReadOnlyMethod OPTIONS = True
isReadOnlyMethod _ = False

{- | Default allowed methods for PostgREST toolbox.

By default, only read-only methods are exposed for safety.
-}
defaultAllowedMethods :: [HttpMethod]
defaultAllowedMethods = [GET, HEAD, OPTIONS]

-- -------------------------------------------------------------------------
-- Configuration
-- -------------------------------------------------------------------------

{- | PostgREST-specific configuration for toolbox initialization.

This configuration connects to a PostgREST instance and provides
authentication credentials if needed.

Note: The 'configFilter' field for endpoint filtering is stored in
'System.Agents.Tools.PostgRESToolbox.Config' to avoid circular
dependencies (EndpointPredicate depends on PostgREST types).
-}
data Config = Config
    { configUrl :: Text
    -- ^ URL to PostgREST OpenAPI spec (e.g., "http://localhost:3000/" or "file:///path/to/spec.json")
    , configBaseUrl :: Text
    -- ^ Base URL for API calls (e.g., "http://localhost:3000")
    , configHeaders :: Map Text Text
    -- ^ Static headers to include in all requests
    , configToken :: Maybe Text
    -- ^ Optional Bearer token for JWT authentication
    , configAllowedMethods :: [HttpMethod]
    -- ^ HTTP methods to expose as tools (default: read-only methods)
    }
    deriving (Show, Eq)

-- -------------------------------------------------------------------------
-- Row Filtering
-- -------------------------------------------------------------------------

{- | Represents a row filter for a specific column.

PostgREST exposes column filters via parameters like "rowFilter.users.id"
in the OpenAPI spec. These allow filtering rows based on column values
using various operators (eq, neq, gt, gte, lt, lte, like, ilike, etc.).
-}
data RowFilter = RowFilter
    { rfColumn :: Text
    -- ^ Column name (e.g., "id", "name", "created_at")
    , rfType :: Text
    -- ^ PostgreSQL type (e.g., "integer", "text", "timestamp")
    , rfDescription :: Text
    -- ^ Description of the filter from the spec
    }
    deriving (Show, Eq)

-- -------------------------------------------------------------------------
-- Trace Events
-- -------------------------------------------------------------------------

{- | Trace events for monitoring PostgREST toolbox operations.

These events allow tracking of:
* Spec fetching progress
* Tool conversion status
* Table-specific operations
* Filter detection
* Query execution
-}
data Trace
    = -- | URL being fetched for the OpenAPI spec
      FetchingSpecTrace !Text
    | -- | Loading spec from file path
      FetchingSpecFromFileTrace !Text
    | -- | HTTP status of spec fetch (e.g., 200)
      SpecFetchedTrace !Int
    | -- | Successfully loaded spec from file
      SpecLoadedFromFileTrace !FilePath
    | -- | Table name that was converted to a tool (e.g., "users")
      TableConvertedTrace !Text
    | -- | Table name and count of detected column filters (e.g., "users", 5)
      ColumnFiltersDetectedTrace !Text !Int
    | -- | Number of tools before and after filtering (e.g., 50, 20)
      ToolsFilteredTrace !Int !Int
    | -- | method, path, query string being executed
      ExecutionTrace !Text !Text !Text
    | -- | Error message during execution
      ExecutionErrorTrace !Text
    deriving (Show)

-- -------------------------------------------------------------------------
-- Tool Representation
-- -------------------------------------------------------------------------

{- | Internal representation of a tool derived from a PostgREST table.

Each table/method combination in the PostgREST API becomes a tool with:
* Support for the specific HTTP method (GET, POST, PUT, etc.)
* Structured parameters for filters, subsetting, and ordering
* Detected row filters from the spec
* Optional request body schema for POST/PUT/PATCH
-}
data PostgRESTool = PostgRESTool
    { prtPath :: Text
    -- ^ Table path (e.g., "/users", "/public.orders")
    , prtMethod :: HttpMethod
    -- ^ HTTP method (GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS)
    , prtName :: Text
    -- ^ Tool name (e.g., "postgrest_get_users")
    , prtDescription :: Text
    -- ^ Tool description from spec or generated
    , prtRowFilters :: [RowFilter]
    -- ^ Detected column filters from spec
    , prtHasPagination :: Bool
    -- ^ Whether pagination parameters (limit/offset) are available
    , prtHasOrdering :: Bool
    -- ^ Whether ordering parameter is available
    , prtRequestBodySchema :: Maybe Schema
    -- ^ Schema for request body (for POST/PUT/PATCH methods)
    , prtIsReadOnly :: Bool
    -- ^ Whether this is a read-only operation (GET, HEAD, OPTIONS)
    }
    deriving (Show, Eq)

-- -------------------------------------------------------------------------
-- Parameter Schemas
-- -------------------------------------------------------------------------

{- | Parameters structure for LLM tool definitions.

PostgREST tools use structured parameter groups rather than flat
parameters to avoid collisions with column names and improve
discoverability for LLMs.
-}
data ToolParameters = ToolParameters
    { tpFilters :: Maybe FilterSchema
    -- ^ Column filters (mapped to query params like name=eq.John)
    , tpSubset :: Maybe SubsetSchema
    -- ^ Pagination (limit, offset) and column selection
    , tpRanking :: Maybe RankingSchema
    -- ^ Ordering clause (order parameter)
    , tpRequestBody :: Maybe Schema
    -- ^ Schema for request body (for POST/PUT/PATCH)
    }
    deriving (Show, Eq)

{- | Schema for filter parameters.

Contains a property for each filterable column. The LLM provides
filter values which are converted to PostgREST filter syntax.
-}
data FilterSchema = FilterSchema
    { fsDescription :: Text
    -- ^ Description of the filters object
    , fsProperties :: Map Text ColumnFilterSchema
    -- ^ Map of column name to filter schema
    }
    deriving (Show, Eq)

{- | Schema for a single column filter.

The value is typically a string that can include PostgREST operators
like "eq.", "gt.", "lt.", etc. For simplicity, operators can be
embedded in the value (e.g., "gt.18" for age greater than 18).
-}
data ColumnFilterSchema = ColumnFilterSchema
    { cfsType :: Text
    -- ^ JSON Schema type (usually "string")
    , cfsDescription :: Text
    -- ^ Description including the PostgreSQL column type
    }
    deriving (Show, Eq)

{- | Schema for subset/pagination parameters.

Controls which rows are returned and which columns are included.
-}
data SubsetSchema = SubsetSchema
    { ssOffset :: Maybe Text
    -- ^ Description for offset parameter (rows to skip)
    , ssLimit :: Maybe Text
    -- ^ Description for limit parameter (max rows to return)
    , ssColumns :: Maybe Text
    -- ^ Description for select parameter (comma-separated column names)
    }
    deriving (Show, Eq)

{- | Schema for ranking/ordering parameters.

Controls the order of returned rows.
-}
data RankingSchema = RankingSchema
    { rsOrder :: Maybe Text
    -- ^ Description for order parameter (e.g., "created_at.desc,name.asc")
    }
    deriving (Show, Eq)

-- -------------------------------------------------------------------------
-- Tool Result
-- -------------------------------------------------------------------------

{- | Result of executing a PostgREST tool.

Captures the structured result from a PostgREST query including
the HTTP response details and returned data.
-}
data ToolResult = ToolResult
    { resultPath :: Text
    -- ^ Path that was called (e.g., "/users")
    , resultMethod :: Text
    -- ^ HTTP method used (e.g., "GET")
    , resultStatus :: Int
    -- ^ HTTP status code (e.g., 200)
    , resultPayload :: Value
    -- ^ Response payload as JSON (array of rows or single row)
    }
    deriving (Show)

instance ToJSON ToolResult where
    toJSON tr =
        Aeson.object
            [ "path" Aeson..= resultPath tr
            , "method" Aeson..= resultMethod tr
            , "status" Aeson..= resultStatus tr
            , "payload" Aeson..= resultPayload tr
            ]
