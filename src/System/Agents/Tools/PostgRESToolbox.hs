{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | PostgREST Toolbox runtime for executing database API tools.

This module provides a runtime that:
* Fetches PostgREST OpenAPI specifications from URLs
* Converts table endpoints to executable tools (GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS)
* Handles HTTP execution with PostgREST-specific query parameter building
* Integrates with the LLM tool registration system

The toolbox follows patterns similar to OpenAPIToolbox, McpToolbox,
and BashToolbox for consistent integration with the agents system.

Key features:
* Structured query parameters (filters/subset/ranking) for read operations
* Request body support for write operations (POST/PUT/PATCH)
* Configurable HTTP method exposure (read-only by default for safety)
* Column-based row filtering
* Automatic pagination and ordering support
* JWT Bearer token authentication

Example usage:

@
import System.Agents
import qualified System.Agents.Tools.PostgRESToolbox as PostgREST

main :: IO ()
main = do
    let config = PostgREST.Config
            { configUrl = "http://localhost:3000/"
            , configBaseUrl = "http://localhost:3000"
            , configHeaders = Map.empty
            , configToken = Just "eyJhbG..."
            , configAllowedMethods = [GET, POST, PATCH]  -- Enable read and write
            , configFilter = Just (PathPrefix "/public")  -- Only public schema tables
            }
    result <- PostgREST.initializeToolbox tracer config
    case result of
        Right toolbox -> do
            -- Register tools with LLM
            regResult <- registerPostgRESTools toolbox
            case regResult of
                Right registrations -> useWithAgent registrations
                Left err -> handleError err
        Left err -> handleError err
@
-}
module System.Agents.Tools.PostgRESToolbox (
    -- * Core types
    Trace,
    Toolbox (..),
    Config (..),
    HttpMethod (..),
    defaultAllowedMethods,
    InitializationError (..),

    -- * Re-exports from Types
    ToolResult,
    PostgRESTool (..),
    RowFilter (..),
    isReadOnlyMethod,

    -- * Re-export EndpointPredicate for filter configuration
    EndpointPredicate,

    -- * Initialization
    initializeToolbox,

    -- * Tool execution
    createToolHandler,
    handleToolCall,

    -- * Query string building
    buildQueryString,
    buildFilterParams,
    buildSubsetParams,
    buildRankingParams,

    -- * Response handling
    handleResponse,

    -- * Tool lookup
    getToolByName,

    -- * Naming helpers
    postgrest2LLMName,

    -- * URL helpers
    isFileUrl,
    fileUrlToPath,
) where

import Control.Exception (try)
import Data.Aeson (Object, Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.CaseInsensitive (mk)
import Data.Char (intToDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types as HttpTypes
import Numeric (showHex)
import Prod.Tracer (Tracer (..), runTracer)

import qualified System.Agents.HttpClient as HttpClient
import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (ToolExecutionContext)
import System.Agents.Tools.EndpointPredicate (
    EndpointPredicate,
    matchesPostgRESTool,
 )
import System.Agents.Tools.IO (RunError (..))
import System.Agents.Tools.OpenAPI.Converter (normalizeForLLM)
import System.Agents.Tools.OpenAPI.Types (OpenAPISpec (..))
import System.Agents.Tools.PostgREST.Converter (
    PostgRESTool (..),
    buildToolParameters,
    convertPostgRESToTools,
    methodToText,
 )
import System.Agents.Tools.PostgREST.Types (
    FilterSchema (..),
    HttpMethod (..),
    RankingSchema (..),
    RowFilter (..),
    SubsetSchema (..),
    ToolParameters (..),
    ToolResult (..),
    Trace (..),
    defaultAllowedMethods,
    isReadOnlyMethod,
 )
import qualified System.Agents.Tools.PostgREST.Types as Types
import System.Agents.Tools.Trace (ToolTrace (..))

-- -------------------------------------------------------------------------
-- Core types
-- -------------------------------------------------------------------------

-- | Errors that can occur during toolbox initialization.
data InitializationError
    = -- | Network error fetching spec
      NetworkError !Text
    | -- | File error loading spec from disk
      FileError !Text
    | -- | JSON parse error
      ParseError !Text
    | -- | OpenAPI spec validation error
      SpecError !Text
    deriving (Show, Eq)

{- | Configuration for initializing a PostgREST toolbox.

This configuration type extends the base 'Types.Config' with an optional
'EndpointPredicate' filter to subset the available tools.
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
    , configFilter :: Maybe EndpointPredicate
    {- ^ Optional filter to restrict which tables/endpoints are exposed as tools.
    If not specified, all tables are included.
    -}
    }
    deriving (Show, Eq)

-- | Convert toolbox Config to base Types.Config.
toBaseConfig :: Config -> Types.Config
toBaseConfig cfg =
    Types.Config
        { Types.configUrl = cfg.configUrl
        , Types.configBaseUrl = cfg.configBaseUrl
        , Types.configHeaders = cfg.configHeaders
        , Types.configToken = cfg.configToken
        , Types.configAllowedMethods = cfg.configAllowedMethods
        }

{- | Runtime state for a PostgREST toolbox.

The toolbox maintains:
* A name for identification
* The base URL for API calls
* A list of available tools (one per table/method combination, filtered by configFilter if provided)
* The HTTP runtime for making requests
* Optional dynamic header function for auth
* The list of allowed HTTP methods
-}
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxBaseUrl :: Text
    , toolboxTools :: [PostgRESTool]
    -- ^ Tools converted from the PostgREST spec (filtered if configFilter was provided)
    , httpRuntime :: HttpClient.Runtime
    -- ^ HTTP client runtime
    , headerFunc :: Maybe (IO (Map Text Text))
    -- ^ Optional function to get dynamic headers (e.g., for auth)
    , staticHeaders :: Map Text Text
    -- ^ Static headers from config
    , toolboxAllowedMethods :: [HttpMethod]
    -- ^ HTTP methods exposed by this toolbox
    , toolboxFilter :: Maybe EndpointPredicate
    -- ^ The filter used during initialization (stored for reference)
    }

-- -------------------------------------------------------------------------
-- URL Helpers
-- -------------------------------------------------------------------------

{- | Check if a URL is a file:// URL.

Example:

>>> isFileUrl "file:///path/to/spec.json"
True

>>> isFileUrl "http://localhost:3000/"
False
-}
isFileUrl :: Text -> Bool
isFileUrl url = Text.isPrefixOf "file://" url

{- | Convert a file:// URL to a file path.

Handles both "file:///absolute/path" and "file://relative/path" formats.

Example:

>>> fileUrlToPath "file:///home/user/spec.json"
"/home/user/spec.json"

>>> fileUrlToPath "file://spec.json"
"spec.json"
-}
fileUrlToPath :: Text -> FilePath
fileUrlToPath url =
    let withoutPrefix = Text.drop 7 url -- Drop "file://"
     in Text.unpack withoutPrefix

-- -------------------------------------------------------------------------
-- Initialization
-- -------------------------------------------------------------------------

{- | Initialize a PostgREST toolbox from a configuration.

This function:
1. Creates an HTTP runtime with authentication
2. Fetches the OpenAPI spec from the configured URL (supports file:// URLs)
3. Parses the JSON to an OpenAPISpec
4. Converts table operations to tools based on allowed methods
5. Applies the optional filter to subset tools (using 'matchesPostgRESTool')
6. Detects row filters and builds parameter schemas

Returns an 'InitializationError' if any step fails.
-}
initializeToolbox ::
    Tracer IO Trace ->
    Config ->
    IO (Either InitializationError Toolbox)
initializeToolbox tracer config = do
    -- Create HTTP runtime with auth
    let token = case config.configToken of
            Just t -> HttpClient.BearerToken t
            Nothing -> HttpClient.NoToken
    runtime <- HttpClient.newRuntime token

    -- Fetch OpenAPI spec (from URL or file)
    fetchResult <- fetchSpec tracer runtime config.configUrl

    case fetchResult of
        Left err -> pure $ Left err
        Right body -> do
            -- Parse JSON to OpenAPISpec
            case Aeson.decode body of
                Nothing -> pure $ Left $ ParseError "Failed to parse PostgREST OpenAPI spec JSON"
                Just spec -> do
                    -- Get allowed methods (default to read-only if not specified)
                    let allowedMethods = case config.configAllowedMethods of
                            [] -> defaultAllowedMethods
                            methods -> methods

                    -- Convert to tools
                    let toolboxName = extractToolboxName config.configUrl
                    let allTools = convertPostgRESToTools toolboxName allowedMethods spec

                    -- Apply filter if provided
                    let filteredTools = case config.configFilter of
                            Nothing -> allTools
                            Just predicate -> filter (matchesPostgRESTool predicate) allTools

                    -- Trace tool conversions
                    mapM_ (\t -> runTracer tracer (TableConvertedTrace (prtPath t))) filteredTools

                    -- Trace filtering results
                    runTracer tracer (ToolsFilteredTrace (length allTools) (length filteredTools))

                    -- Trace filter detection
                    mapM_ (\t -> runTracer tracer (ColumnFiltersDetectedTrace (prtPath t) (length (prtRowFilters t)))) filteredTools

                    -- Create toolbox
                    pure $
                        Right $
                            Toolbox
                                { toolboxName = toolboxName
                                , toolboxBaseUrl = config.configBaseUrl
                                , toolboxTools = filteredTools
                                , httpRuntime = runtime
                                , headerFunc = Nothing
                                , staticHeaders = config.configHeaders
                                , toolboxAllowedMethods = allowedMethods
                                , toolboxFilter = config.configFilter
                                }

{- | Extract a toolbox name from the spec URL.

Uses the hostname as the toolbox name.
-}
extractToolboxName :: Text -> Text
extractToolboxName url =
    let withoutProtocol = Text.dropWhile (/= '/') $ Text.drop 8 url
        hostPart = Text.takeWhile (/= '/') withoutProtocol
     in if Text.null hostPart
            then "postgrest"
            else normalizeForLLM hostPart

{- | Fetch the OpenAPI spec from a URL or file.

Supports both HTTP/HTTPS URLs and file:// URLs. For file URLs,
the spec is read directly from the filesystem.
-}
fetchSpec ::
    Tracer IO Trace ->
    HttpClient.Runtime ->
    Text ->
    IO (Either InitializationError LByteString.ByteString)
fetchSpec tracer runtime url
    | isFileUrl url = fetchSpecFromFile tracer (fileUrlToPath url)
    | otherwise = fetchSpecFromUrl tracer runtime url

-- | Fetch the OpenAPI spec from a file on disk.
fetchSpecFromFile ::
    Tracer IO Trace ->
    FilePath ->
    IO (Either InitializationError LByteString.ByteString)
fetchSpecFromFile tracer path = do
    runTracer tracer (FetchingSpecFromFileTrace (Text.pack path))
    result <- try $ LByteString.readFile path
    case result of
        Left (e :: IOError) ->
            pure $ Left $ FileError (Text.pack $ show e)
        Right content -> do
            runTracer tracer (SpecLoadedFromFileTrace path)
            pure $ Right content

-- | Fetch the OpenAPI spec from an HTTP URL.
fetchSpecFromUrl ::
    Tracer IO Trace ->
    HttpClient.Runtime ->
    Text ->
    IO (Either InitializationError LByteString.ByteString)
fetchSpecFromUrl tracer runtime url = do
    runTracer tracer (FetchingSpecTrace url)
    result <- try $ HttpClient.get runtime (Tracer (const (pure ()))) url
    case result of
        Left (e :: HttpClient.HttpException) ->
            pure $ Left $ NetworkError (Text.pack $ show e)
        Right (Left err) ->
            pure $ Left $ NetworkError (Text.pack $ show err)
        Right (Right response) ->
            let status = HttpTypes.statusCode (HttpClient.responseStatus response)
                body = HttpClient.responseBody response
             in do
                    runTracer tracer (SpecFetchedTrace status)
                    pure $ Right body

-- -------------------------------------------------------------------------
-- Tool Lookup
-- -------------------------------------------------------------------------

{- | Get a tool by its name.

This is used during tool execution to find the tool
when the LLM calls it by name.

Returns 'Nothing' if no tool with that name exists.
-}
getToolByName ::
    Toolbox ->
    Text ->
    Maybe PostgRESTool
getToolByName toolbox name =
    case filter (\t -> prtName t == name) (toolboxTools toolbox) of
        (t : _) -> Just t
        [] -> Nothing

-- -------------------------------------------------------------------------
-- Tool Handler Creation
-- -------------------------------------------------------------------------

{- | Creates a handler function for a PostgREST tool.

This handler integrates with the ToolExecutionContext system and
returns results in the standard CallResult format.
-}
createToolHandler ::
    Toolbox ->
    PostgRESTool ->
    Tracer IO ToolTrace ->
    ToolExecutionContext ->
    Value ->
    IO (CallResult ())
createToolHandler toolbox tool _tracer _ctx args = do
    result <- handleToolCall toolbox tool args
    case result of
        Left err -> do
            pure $ IOToolError () (ScriptExecutionError (Text.unpack err))
        Right (textResult, _toolResult) -> do
            pure $ BlobToolSuccess () (Text.encodeUtf8 textResult)

{- | Handle a tool call by executing the HTTP request.

This function:
1. Extracts structured arguments (filters, subset, ranking, body)
2. Builds the query string from arguments
3. Builds full URL
4. Makes HTTP request with the appropriate method
5. Returns result
-}
handleToolCall ::
    Toolbox ->
    PostgRESTool ->
    Value ->
    IO (Either Text (Text, ToolResult))
handleToolCall toolbox tool args = do
    case args of
        Object obj -> do
            -- Build query string from structured arguments (for read operations)
            let queryString = buildQueryStringFromArgs obj (buildToolParameters tool)

            -- Build full URL
            let fullUrl = toolboxBaseUrl toolbox <> prtPath tool <> queryString

            -- Get headers
            headers <- case toolbox.headerFunc of
                Just hf -> hf
                Nothing -> pure Map.empty
            let allHeaders = Map.union headers (staticHeaders toolbox)

            -- Extract request body if present (for POST/PUT/PATCH)
            let requestBody = extractRequestBody obj (buildToolParameters tool)

            -- Make HTTP request with appropriate method
            result <- executeRequest toolbox.httpRuntime (prtMethod tool) fullUrl allHeaders requestBody

            case result of
                Left err -> pure $ Left err
                Right (textResult, toolResult) -> do
                    pure $ Right (textResult, toolResult)
        _ -> pure $ Left "Tool arguments must be a JSON object"

-- | Extract request body from arguments.
extractRequestBody :: Object -> ToolParameters -> Maybe Value
extractRequestBody obj params =
    case KeyMap.lookup "body" obj of
        Just val -> Just val
        Nothing -> Nothing

-- | Execute an HTTP request with the specified method.
executeRequest ::
    HttpClient.Runtime ->
    HttpMethod ->
    Text ->
    Map Text Text ->
    Maybe Value ->
    IO (Either Text (Text, ToolResult))
executeRequest runtime method url headers mbody = do
    -- Build request based on method
    req <- buildRequest method url headers mbody

    -- Execute the request
    result <- HttpClient.runRequest runtime (Tracer (const (pure ()))) req

    case result of
        Left err -> pure $ Left (Text.pack $ show err)
        Right response -> handleResponse method url response

-- | Build an HTTP request with the specified method.
buildRequest ::
    HttpMethod ->
    Text ->
    Map Text Text ->
    Maybe Value ->
    IO HttpClient.Request
buildRequest method url headers mbody = do
    req <- HttpClient.parseRequest (Text.unpack url)

    -- Set the HTTP method
    let methodBytes = Text.encodeUtf8 $ methodToText method
    let reqWithMethod = req{HttpClient.method = methodBytes}

    -- Add custom headers
    let headerList = map (\(k, v) -> (mk (Text.encodeUtf8 k), Text.encodeUtf8 v)) $ Map.toList headers
    let reqWithHeaders = reqWithMethod{HttpClient.requestHeaders = HttpClient.requestHeaders reqWithMethod ++ headerList}

    -- Add request body if present (for POST, PUT, PATCH)
    let finalReq = case mbody of
            Just body ->
                reqWithHeaders
                    { HttpClient.requestBody = HttpClient.RequestBodyLBS (Aeson.encode body)
                    , HttpClient.requestHeaders = (mk "Content-Type", "application/json") : HttpClient.requestHeaders reqWithHeaders
                    }
            Nothing -> reqWithHeaders

    pure finalReq

-- -------------------------------------------------------------------------
-- Query String Building
-- -------------------------------------------------------------------------

{- | Build query string from tool arguments.

Extracts filters, subset, and ranking from the arguments object
and converts them to PostgREST query parameters.
-}
buildQueryStringFromArgs :: Object -> ToolParameters -> Text
buildQueryStringFromArgs obj params =
    let objMap = KeyMap.toMapText obj

        -- Extract filters
        filterParams = case Map.lookup "filters" objMap of
            Just (Object filterObj) -> buildFilterParams filterObj
            _ -> Map.empty

        -- Extract subset
        subsetParams = case Map.lookup "subset" objMap of
            Just (Object subsetObj) -> buildSubsetParams subsetObj
            _ -> Map.empty

        -- Extract ranking
        rankingParams = case Map.lookup "ranking" objMap of
            Just (Object rankingObj) -> buildRankingParams rankingObj
            _ -> Map.empty

        -- Combine all params
        allParams = Map.unions [filterParams, subsetParams, rankingParams]
     in if Map.null allParams
            then ""
            else "?" <> Text.intercalate "&" (map encodeParam $ Map.toList allParams)
  where
    encodeParam (k, v) = urlEncode k <> "=" <> urlEncode v

{- | Build filter parameters from filters object.

Converts filter values directly to query parameters.
PostgREST infers the eq operator when no operator is specified.

Examples:
>>> buildFilterParams (KeyMap.fromList [("name", String "John")])
fromList [("name","John")]
>>> buildFilterParams (KeyMap.fromList [("age", String "gt.18")])
fromList [("age","gt.18")]
-}
buildFilterParams :: Object -> Map Text Text
buildFilterParams filterObj =
    let objMap = KeyMap.toMapText filterObj
     in Map.mapMaybe extractValue objMap
  where
    extractValue :: Value -> Maybe Text
    extractValue (String s) = Just s
    extractValue (Number n) = Just (Text.pack $ show n)
    extractValue (Bool b) = Just (if b then "true" else "false")
    extractValue _ = Nothing

{- | Build subset parameters from subset object.

Extracts limit, offset, and columns (mapped to select).
-}
buildSubsetParams :: Object -> Map Text Text
buildSubsetParams subsetObj =
    let objMap = KeyMap.toMapText subsetObj
        extract key = case Map.lookup key objMap of
            Just (String s) | not (Text.null s) -> Just s
            _ -> Nothing
     in Map.fromList $
            catMaybes
                [ fmap (\v -> ("limit", v)) (extract "limit")
                , fmap (\v -> ("offset", v)) (extract "offset")
                , fmap (\v -> ("select", v)) (extract "columns")
                ]
  where
    catMaybes = mapMaybe id

{- | Build ranking parameters from ranking object.

Extracts the order parameter.
-}
buildRankingParams :: Object -> Map Text Text
buildRankingParams rankingObj =
    let objMap = KeyMap.toMapText rankingObj
     in case Map.lookup "order" objMap of
            Just (String s) | not (Text.null s) -> Map.singleton "order" s
            _ -> Map.empty

{- | Build query string from parameters map.

This is a lower-level function for building query strings directly.
-}
buildQueryString :: Map Text Text -> Text
buildQueryString params
    | Map.null params = ""
    | otherwise =
        "?" <> Text.intercalate "&" (map encodeParam $ Map.toList params)
  where
    encodeParam (k, v) = urlEncode k <> "=" <> urlEncode v

{- | URL-encode a query parameter.

Basic URL encoding for query string parameters.
-}
urlEncode :: Text -> Text
urlEncode = Text.concatMap encodeChar
  where
    encodeChar :: Char -> Text
    encodeChar c
        | c `elem` (safeChars :: [Char]) = Text.singleton c
        | c == ' ' = "+"
        | otherwise = percentEncode c

    safeChars :: [Char]
    safeChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~"

    percentEncode :: Char -> Text
    percentEncode c = Text.pack $ "%" ++ showHex (fromEnum c) ""

-- -------------------------------------------------------------------------
-- Response Handling
-- -------------------------------------------------------------------------

{- | Handle an HTTP response, converting to ToolResult.

Returns both text for the LLM and a structured ToolResult.

Note: Uses lenient UTF-8 decoding to handle binary data safely.
Invalid bytes are replaced with U+FFFD (replacement character).
-}
handleResponse ::
    HttpMethod ->
    Text ->
    HttpClient.Response LByteString.ByteString ->
    IO (Either Text (Text, ToolResult))
handleResponse method url response = do
    let status = HttpTypes.statusCode (HttpClient.responseStatus response)
    let body = HttpClient.responseBody response

    -- Parse body as JSON if possible, otherwise use lenient UTF-8 decoding
    let payload = case Aeson.decode body of
            Just val -> val
            Nothing -> String (Text.decodeUtf8With lenientDecode $ LByteString.toStrict body)

    -- Create text representation for LLM using lenient UTF-8 decoding
    -- For HEAD requests, don't include body (there shouldn't be one)
    let textResult = case method of
            HEAD ->
                "HTTP " <> Text.pack (show status) <> " (HEAD request - no body)"
            _ ->
                "HTTP "
                    <> Text.pack (show status)
                    <> "\n"
                    <> Text.decodeUtf8With lenientDecode (LByteString.toStrict body)

    pure $
        Right
            ( textResult
            , ToolResult
                { resultPath = url
                , resultMethod = methodToText method
                , resultStatus = status
                , resultPayload = payload
                }
            )

-- -------------------------------------------------------------------------
-- Naming helpers
-- -------------------------------------------------------------------------

{- | Convert a PostgREST tool name to an LLM tool name.

Names are prefixed with @postgrest_@ and include the normalized toolbox name
and tool name to avoid conflicts and ensure LLM compatibility.

Example:

>>> postgrest2LLMName "mydb" "get_users"
ToolName {getToolName = "postgrest_mydb_get_users"}
-}
postgrest2LLMName :: Text -> Text -> OpenAI.ToolName
postgrest2LLMName toolboxName toolName =
    let normalizedToolbox = normalizeForLLM toolboxName
        normalizedTool = normalizeForLLM toolName
     in OpenAI.ToolName ("postgrest_" <> normalizedToolbox <> "_" <> normalizedTool)

