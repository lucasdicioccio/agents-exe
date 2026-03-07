{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | OpenAPI Toolbox runtime for executing tools from OpenAPI specifications.
--
-- This module provides a runtime that:
-- * Fetches OpenAPI specifications from URLs
-- * Converts operations to executable tools
-- * Handles HTTP execution with proper parameter substitution
-- * Integrates with the LLM tool registration system
--
-- The toolbox follows patterns similar to McpToolbox and BashToolbox for
-- consistent integration with the agents system.
--
-- Key feature: Name normalization for LLM compatibility.
-- OpenAPI operation IDs may contain invalid characters (dots, slashes, etc.)
-- which are normalized to LLM-safe names using 'normalizeForLLM'.
--
-- Example usage:
--
-- @
-- import System.Agents
-- import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPI
--
-- main :: IO ()
-- main = do
--     let config = OpenAPI.Config
--             { configUrl = "https://api.example.com/openapi.json"
--             , configBaseUrl = "https://api.example.com"
--             , configHeaders = Map.empty
--             , configToken = Just "my-bearer-token"
--             }
--     result <- OpenAPI.initializeToolbox tracer config
--     case result of
--         Right toolbox -> do
--             -- Register tools with LLM
--             regResult <- registerOpenAPITools toolbox
--             case regResult of
--                 Right registrations -> useWithAgent registrations
--                 Left err -> handleError err
--         Left err -> handleError err
-- @
module System.Agents.Tools.OpenAPIToolbox (
    -- * Core types
    Trace (..),
    Toolbox (..),
    Config (..),
    InitializationError (..),
    
    -- * Re-export ToolResult from Types
    ToolResult,
    
    -- * Initialization
    initializeToolbox,
    
    -- * Tool execution
    createToolHandler,
    handleToolCall,
    
    -- * Path formatting
    formatPath,
    
    -- * HTTP building
    buildRequest,
    buildFullUrl,
    
    -- * Response handling
    handleResponse,
    
    -- * Operation helpers
    getOperationId,
    getToolByNormalizedName,
    
    -- * Naming helpers
    openapi2LLMName,
    normalizeToolName,
) where

import Control.Exception (try)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.CaseInsensitive as CI
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types as HttpTypes
import Prod.Tracer (Tracer (..), runTracer)
import System.Agents.Tools.OpenAPI.Converter (
    OpenAPITool (..),
    NameMapping (..),
    buildToolNameMapping,
    findToolByNormalizedName,
    normalizeForLLM,
    convertOpenAPIToTools,
 )
import System.Agents.Tools.OpenAPI.Resolver (dereferenceSpec)
import System.Agents.Tools.OpenAPI.Types (
    Method,
    ParamLocation (..),
    Parameter (..),
    Path,
    Operation (..),
    ToolResult (..),
  )
import qualified System.Agents.HttpClient as HttpClient
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (ToolExecutionContext)
import System.Agents.Tools.IO (RunError (..))
import System.Agents.Tools.Trace (ToolTrace (..))
import qualified System.Agents.LLMs.OpenAI as OpenAI

-- -------------------------------------------------------------------------
-- Trace types
-- -------------------------------------------------------------------------

-- | Trace events for OpenAPI toolbox operations.
--
-- These events allow monitoring of:
-- * Spec fetching progress
-- * Tool conversion status
-- * Endpoint execution
-- * Error conditions
data Trace
    = FetchingSpecTrace !Text
    -- ^ URL being fetched
    | SpecFetchedTrace !Int
    -- ^ HTTP status of spec fetch
    | ToolsConvertedTrace !Int
    -- ^ Number of tools converted from spec
    | CallingEndpointTrace !Text !Text !Text
    -- ^ method, url, path being called
    | EndpointResponseTrace !Int
    -- ^ HTTP status of endpoint response
    | SchemaResolutionErrorTrace !Text
    -- ^ Error during schema dereferencing
    | ToolExecutionErrorTrace !Text
    -- ^ Error during tool execution
    | ToolNameNormalizedTrace !Text !Text
    -- ^ Original name and normalized name (for debugging)
    deriving (Show)

-- -------------------------------------------------------------------------
-- Core types
-- -------------------------------------------------------------------------

-- | Errors that can occur during toolbox initialization.
data InitializationError
    = NetworkError !Text
    -- ^ Network error fetching spec
    | ParseError !Text
    -- ^ JSON parse error
    | SpecError !Text
    -- ^ OpenAPI spec validation error
    deriving (Show, Eq)

-- | Configuration for initializing an OpenAPI toolbox.
data Config = Config
    { configUrl :: Text
    -- ^ URL to fetch OpenAPI spec from
    , configBaseUrl :: Text
    -- ^ Base URL for API calls
    , configHeaders :: Map Text Text
    -- ^ Static headers to include in all requests
    , configToken :: Maybe Text
    -- ^ Optional Bearer token for authentication
    }
    deriving (Show, Eq)

-- | Runtime state for an OpenAPI toolbox.
--
-- The toolbox maintains:
-- * A name for identification
-- * The base URL for API calls
-- * A list of available tools
-- * A name mapping for LLM-safe tool names
-- * The HTTP runtime for making requests
-- * An optional header function for dynamic auth headers
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxBaseUrl :: Text
    , toolboxTools :: [OpenAPITool]
    -- ^ Tools converted from the OpenAPI spec
    , toolboxNameMapping :: Map Text NameMapping
    -- ^ Mapping from normalized LLM names to original operation IDs
    , httpRuntime :: HttpClient.Runtime
    -- ^ HTTP client runtime
    , headerFunc :: Maybe (IO (Map Text Text))
    -- ^ Optional function to get dynamic headers (e.g., for auth)
    , staticHeaders :: Map Text Text
    -- ^ Static headers from config
    }

-- -------------------------------------------------------------------------
-- Initialization
-- -------------------------------------------------------------------------

-- | Initialize an OpenAPI toolbox from a configuration.
--
-- This function:
-- 1. Creates an HTTP runtime with authentication
-- 2. Fetches the OpenAPI spec from the configured URL
-- 3. Parses the JSON to an OpenAPISpec
-- 4. Dereferences schema references ($ref)
-- 5. Converts operations to tools
-- 6. Builds the name mapping for LLM-safe names
--
-- Returns an 'InitializationError' if any step fails.
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

    -- Fetch OpenAPI spec
    runTracer tracer (FetchingSpecTrace config.configUrl)
    fetchResult <- fetchSpec runtime config.configUrl

    case fetchResult of
        Left err -> pure $ Left err
        Right (status, body) -> do
            runTracer tracer (SpecFetchedTrace status)

            -- Parse JSON to OpenAPISpec
            case Aeson.decode body of
                Nothing -> pure $ Left $ ParseError "Failed to parse OpenAPI spec JSON"
                Just spec -> do
                    -- Dereference schemas
                    let dereferencedSpec = dereferenceSpec spec

                    -- Convert to tools
                    let tools = convertOpenAPIToTools dereferencedSpec
                    runTracer tracer (ToolsConvertedTrace (length tools))

                    -- Build name mapping for LLM-safe names
                    let nameMapping = buildToolNameMapping toolboxName tools
                    
                    -- Trace name normalizations (for debugging)
                    mapM_ (\m -> runTracer tracer (ToolNameNormalizedTrace (nmOriginal m) (nmNormalized m))) 
                          (Map.elems nameMapping)

                    -- Create toolbox
                    pure $ Right $ Toolbox
                        { toolboxName = toolboxName
                        , toolboxBaseUrl = config.configBaseUrl
                        , toolboxTools = tools
                        , toolboxNameMapping = nameMapping
                        , httpRuntime = runtime
                        , headerFunc = Nothing
                        , staticHeaders = config.configHeaders
                        }
  where
    toolboxName = extractToolboxName config.configUrl

-- | Extract a toolbox name from the spec URL.
extractToolboxName :: Text -> Text
extractToolboxName url = 
    let withoutProtocol = Text.dropWhile (/= '/') $ Text.drop 8 url
        hostPart = Text.takeWhile (/= '/') withoutProtocol
    in if Text.null hostPart 
       then "openapi" 
       else normalizeForLLM hostPart

-- | Fetch the OpenAPI spec from a URL.
fetchSpec ::
    HttpClient.Runtime ->
    Text ->
    IO (Either InitializationError (Int, LByteString.ByteString))
fetchSpec runtime url = do
    result <- try $ HttpClient.get runtime (Tracer (const (pure ()))) url
    case result of
        Left (e :: HttpClient.HttpException) ->
            pure $ Left $ NetworkError (Text.pack $ show e)
        Right (Left err) ->
            pure $ Left $ NetworkError (Text.pack $ show err)
        Right (Right response) ->
            let status = HttpTypes.statusCode (HttpClient.responseStatus response)
                body = HttpClient.responseBody response
             in pure $ Right (status, body)

-- -------------------------------------------------------------------------
-- Tool Lookup
-- -------------------------------------------------------------------------

-- | Get a tool by its normalized LLM name.
--
-- This is used during tool execution to find the original tool
-- when the LLM calls a tool by its normalized name.
--
-- Returns 'Nothing' if no tool with that normalized name exists.
getToolByNormalizedName ::
    Toolbox ->
    Text ->
    Maybe OpenAPITool
getToolByNormalizedName toolbox normalizedName = do
    -- Look up the original operation ID from the mapping
    originalOpId <- findToolByNormalizedName (toolboxNameMapping toolbox) normalizedName
    -- Find the tool with that original operation ID
    find ((== Just originalOpId) . opOperationId . toolOperation) (toolboxTools toolbox)

-- -------------------------------------------------------------------------
-- Tool Handler Creation
-- -------------------------------------------------------------------------

-- | Creates a handler function for an OpenAPI tool.
--
-- This handler integrates with the ToolExecutionContext system and
-- returns results in the standard CallResult format.
createToolHandler ::
    Toolbox ->
    OpenAPITool ->
    Tracer IO ToolTrace ->
    ToolExecutionContext ->
    Value ->
    IO (CallResult ())
createToolHandler toolbox tool tracer _ctx args = do
    result <- handleToolCall toolbox tool args
    case result of
        Left err -> do
            runTracer (contramapTrace tracer) (ToolExecutionErrorTrace err)
            pure $ IOToolError () (ScriptExecutionError (Text.unpack err))
        Right (textResult, _toolResult) -> do
            pure $ BlobToolSuccess () (ByteString.pack $ Text.unpack textResult)
  where
    contramapTrace :: Tracer IO ToolTrace -> Tracer IO Trace
    contramapTrace _t = Tracer $ \_traceEvent -> do
        -- Convert Trace to ToolTrace if needed
        -- For now, we just don't trace these events to the main tool tracer
        pure ()

-- | Handle a tool call by executing the HTTP request.
--
-- This function:
-- 1. Extracts path params (prefixed with p_)
-- 2. Extracts query params (prefixed with p_)
-- 3. Extracts body (param "b")
-- 4. Formats the path with path args
-- 5. Builds full URL
-- 6. Makes HTTP request
-- 7. Returns result
handleToolCall ::
    Toolbox ->
    OpenAPITool ->
    Value ->
    IO (Either Text (Text, ToolResult))
handleToolCall toolbox tool args = do
    case args of
        Object obj -> do
            -- Extract parameters from the JSON object
            let objMap = KeyMap.toMapText obj
            let (pathParams, queryParams, bodyValue) = extractParams objMap (toolOperation tool)

            -- Format the path with path parameters
            let formattedPath = formatPath (toolPath tool) pathParams

            -- Build full URL
            let fullUrl = buildFullUrl (toolboxBaseUrl toolbox) formattedPath

            -- Get headers
            headers <- case toolbox.headerFunc of
                Just hf -> hf
                Nothing -> pure Map.empty
            let allHeaders = Map.union headers (staticHeaders toolbox)

            -- Make HTTP request
            let method = toolMethod tool
            runTracer (Tracer (const (pure ()) :: Trace -> IO ())) $ CallingEndpointTrace method fullUrl formattedPath

            result <- executeRequest toolbox.httpRuntime method fullUrl queryParams bodyValue allHeaders

            case result of
                Left err -> pure $ Left err
                Right (textResult, toolResult) -> do
                    runTracer (Tracer (const (pure ()) :: Trace -> IO ())) $ EndpointResponseTrace (resultStatus toolResult)
                    pure $ Right (textResult, toolResult)
        _ -> pure $ Left "Tool arguments must be a JSON object"

-- | Extract parameters from the JSON object based on operation definition.
extractParams ::
    Map Text Value ->
    Operation ->
    (Map Text Text, Map Text Text, Maybe Value)
extractParams obj op =
    let params = opParameters op
        -- Extract path params
        pathParams = Map.foldrWithKey extractPathParam Map.empty obj
          where
            extractPathParam k (String v) acc
                | Just p <- findParam params (Text.drop 2 k)
                , paramIn p == ParamInPath = Map.insert (Text.drop 2 k) v acc
            extractPathParam _ _ acc = acc

        -- Extract query params
        queryParams = Map.foldrWithKey extractQueryParam Map.empty obj
          where
            extractQueryParam k (String v) acc
                | Just p <- findParam params (Text.drop 2 k)
                , paramIn p == ParamInQuery = Map.insert (Text.drop 2 k) v acc
            extractQueryParam k v acc
                | Just p <- findParam params (Text.drop 2 k)
                , paramIn p == ParamInQuery = Map.insert (Text.drop 2 k) (valueToText v) acc
            extractQueryParam _ _ acc = acc

        -- Extract body
        bodyValue = Map.lookup "b" obj
     in (pathParams, queryParams, bodyValue)

-- | Find a parameter by name in the operation parameters list.
findParam :: [Parameter] -> Text -> Maybe Parameter
findParam params name =
    case filter (\p -> paramName p == name) params of
        (p : _) -> Just p
        [] -> Nothing

-- | Convert a JSON value to Text for query params.
valueToText :: Value -> Text
valueToText (String s) = s
valueToText (Number n) = Text.pack $ show n
valueToText (Bool b) = if b then "true" else "false"
valueToText Null = "null"
valueToText _ = ""

-- -------------------------------------------------------------------------
-- Path Formatting
-- -------------------------------------------------------------------------

-- | Format a path template with path parameters.
--
-- Converts a path like @/pets/{petId}@ with @{p_petId: "123"}@ to @/pets/123@.
--
-- Example:
--
-- >>> formatPath "/pets/{petId}" (Map.fromList [("petId", "123")])
-- "/pets/123"
--
-- >>> formatPath "/pets/{petId}/owners/{ownerId}" (Map.fromList [("petId", "123"), ("ownerId", "456")])
-- "/pets/123/owners/456"
formatPath :: Path -> Map Text Text -> Path
formatPath pathTemplate pathArgs =
    Map.foldrWithKey replaceParam pathTemplate pathArgs
  where
    replaceParam :: Text -> Text -> Path -> Path
    replaceParam name value path =
        Text.replace ("{" <> name <> "}") value path

-- -------------------------------------------------------------------------
-- URL Building
-- -------------------------------------------------------------------------

-- | Build the full URL from base URL and path.
buildFullUrl :: Text -> Path -> Text
buildFullUrl baseUrl path =
    let base = Text.dropWhileEnd (== '/') baseUrl
        path' = if Text.isPrefixOf "/" path then path else "/" <> path
     in base <> path'

-- | Build query string from query parameters.
buildQueryString :: Map Text Text -> Text
buildQueryString params
    | Map.null params = ""
    | otherwise =
        "?" <> Text.intercalate "&" (map encodeParam $ Map.toList params)
  where
    encodeParam (k, v) = urlEncode k <> "=" <> urlEncode v
    urlEncode = id -- Simplified - real implementation would URL-encode

-- -------------------------------------------------------------------------
-- HTTP Request Building and Execution
-- -------------------------------------------------------------------------

-- | Build an HTTP request.
buildRequest ::
    Method ->
    Text ->
    Map Text Text ->
    Maybe Value ->
    Map Text Text ->
    IO HttpClient.Request
buildRequest method fullUrl queryParams mbody headers = do
    let queryString = buildQueryString queryParams
    let urlWithQuery = fullUrl <> queryString

    req <- HttpClient.parseRequest (Text.unpack urlWithQuery)

    -- Set method
    let reqWithMethod = req{HttpClient.method = Text.encodeUtf8 method}

    -- Add body if present
    let reqWithBody = case mbody of
            Just body ->
                reqWithMethod
                    { HttpClient.requestBody = HttpClient.RequestBodyLBS (Aeson.encode body)
                    , HttpClient.requestHeaders =
                        ("Content-Type", "application/json")
                            : HttpClient.requestHeaders reqWithMethod
                    }
            Nothing -> reqWithMethod

    -- Add custom headers
    let headerList = map (\(k, v) -> (CI.mk (Text.encodeUtf8 k), Text.encodeUtf8 v)) $ Map.toList headers
    let finalReq = reqWithBody{HttpClient.requestHeaders = HttpClient.requestHeaders reqWithBody ++ headerList}

    pure finalReq

-- | Execute an HTTP request with custom headers.
--
-- This function builds a request with all parameters (method, URL, query params,
-- body, and custom headers) and executes it using the runtime's 'runRequest'
-- function.
executeRequest ::
    HttpClient.Runtime ->
    Method ->
    Text ->
    Map Text Text ->
    Maybe Value ->
    Map Text Text ->
    IO (Either Text (Text, ToolResult))
executeRequest runtime method fullUrl queryParams mbody headers = do
    -- Build the request with all headers and parameters
    req <- buildRequest method fullUrl queryParams mbody headers
    
    -- Execute the request using the runtime
    let tracer = Tracer (const (pure ()))
    result <- HttpClient.runRequest runtime tracer req

    case result of
        Left err -> pure $ Left (Text.pack $ show err)
        Right response -> handleResponse (method, fullUrl) response

-- -------------------------------------------------------------------------
-- Response Handling
-- -------------------------------------------------------------------------

-- | Handle an HTTP response, converting to ToolResult.
--
-- Returns both text for the LLM and a structured ToolResult.
handleResponse ::
    (Method, Text) ->
    HttpClient.Response LByteString.ByteString ->
    IO (Either Text (Text, ToolResult))
handleResponse (method, url) response = do
    let status = HttpTypes.statusCode (HttpClient.responseStatus response)
    let body = HttpClient.responseBody response

    -- Parse body as JSON if possible
    let payload = case Aeson.decode body of
            Just val -> val
            Nothing -> String (Text.decodeUtf8 $ LByteString.toStrict body)

    -- Create text representation for LLM
    let textResult =
            "HTTP " <> Text.pack (show status) <> "\n"
                <> Text.decodeUtf8 (LByteString.toStrict body)

    pure $ Right
        ( textResult
        , ToolResult
            { resultPath = url
            , resultMethod = method
            , resultStatus = status
            , resultPayload = payload
            }
        )

-- -------------------------------------------------------------------------
-- Operation helpers
-- -------------------------------------------------------------------------

-- | Get operation ID from operation, falling back to tool name.
--
-- This is used for tool registration when creating unique tool names.
getOperationId :: Operation -> Maybe Text
getOperationId = opOperationId

-- -------------------------------------------------------------------------
-- Naming helpers
-- -------------------------------------------------------------------------

-- | Normalize a tool name for LLM compatibility.
--
-- This is a re-export of 'normalizeForLLM' for convenience.
normalizeToolName :: Text -> Text
normalizeToolName = normalizeForLLM

-- | Convert an OpenAPI operation ID to an LLM tool name.
--
-- Names are prefixed with @openapi_@ and include the normalized toolbox name
-- and normalized operation ID to avoid conflicts and ensure LLM compatibility.
--
-- The operation ID is normalized to replace invalid characters:
-- - Dots (.) become underscores (_)
-- - Slashes (/) become underscores (_)
-- - Other invalid characters become underscores
-- - Names starting with digits are prefixed with 't'
--
-- Example:
--
-- >>> openapi2LLMName "myApi" "getPet"
-- ToolName {getToolName = "openapi_myApi_getPet"}
--
-- >>> openapi2LLMName "myApi" "pet.findByStatus"
-- ToolName {getToolName = "openapi_myApi_pet_findByStatus"}
--
-- >>> openapi2LLMName "myApi" "2.0/getPet"
-- ToolName {getToolName = "openapi_myApi_t2_0_getPet"}
openapi2LLMName :: Text -> Text -> OpenAI.ToolName
openapi2LLMName tboxName operationId =
    let normalizedToolbox = normalizeForLLM tboxName
        normalizedOpId = normalizeForLLM operationId
    in OpenAI.ToolName ("openapi_" <> normalizedToolbox <> "_" <> normalizedOpId)

