{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | OpenAPI Toolbox runtime for executing tools from OpenAPI specifications.

This module provides a runtime that:
* Fetches OpenAPI specifications from URLs
* Converts operations to executable tools
* Handles HTTP execution with proper parameter substitution
* Integrates with the LLM tool registration system
* Supports flexible secret management via the 'Secrets' configuration

The toolbox follows patterns similar to McpToolbox and BashToolbox for
consistent integration with the agents system.

Key feature: Name normalization for LLM compatibility.
OpenAPI operation IDs may contain invalid characters (dots, slashes, etc.)
which are normalized to LLM-safe names using 'normalizeForLLM'.

Secret Management:
The toolbox supports flexible secret configuration via the 'secrets' field.
Secrets can be sourced from environment variables, files, commands, or API
key files, and can be serialized as custom headers or query string parameters.

Example usage:

@
import System.Agents
import qualified System.Agents.Tools.OpenAPIToolbox as OpenAPI
import System.Agents.Tools.Secrets

main :: IO ()
main = do
    let config = OpenAPI.Config
            { configUrl = "https://api.example.com/openapi.json"
            , configBaseUrl = "https://api.example.com"
            , configHeaders = Map.empty
            , configToken = Nothing  -- Legacy token, prefer secrets
            , configFilter = Just (PathPrefix "/api/v1")
            , configSecrets =
                [ Secret
                    { secretSource = EnvVar "API_TOKEN"
                    , secretDecoder = Clear True
                    , secretSerializer = Header "Authorization" (Just "Bearer {{secret}}")
                    }
                ]
            , configActivation = Nothing  -- Use default activation
            }
    result <- OpenAPI.initializeToolbox tracer config
    case result of
        Right toolbox -> do
            -- Register tools with LLM
            regResult <- registerOpenAPITools toolbox
            case regResult of
                Right registrations -> useWithAgent registrations
                Left err -> handleError err
        Left err -> handleError err
@
-}
module System.Agents.Tools.OpenAPIToolbox (
    -- * Core types
    Trace (..),
    Toolbox (..),
    Config (..),
    InitializationError (..),

    -- * Re-export ToolResult from Types
    ToolResult,

    -- * Re-export EndpointPredicate for filter configuration
    EndpointPredicate,

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

    -- * URL helpers
    isFileUrl,
    fileUrlToPath,

    -- * Secrets
    Secrets.ResolvedSecret,
    Secrets.Secret (..),
    Secrets.SecretSource (..),
    Secrets.SecretDecoder (..),
    Secrets.SecretSerializer (..),
) where

import Control.Exception (try)
import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.CaseInsensitive as CI
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types as HttpTypes
import Prod.Tracer (Tracer (..), contramap, runTracer)
import qualified System.Agents.HttpClient as HttpClient
import System.Agents.Tools.Activation (Activation)
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (ToolExecutionContext)
import System.Agents.Tools.EndpointPredicate (
    EndpointPredicate,
    matchesOpenAPITool,
 )
import System.Agents.Tools.IO (RunError (..))
import System.Agents.Tools.OpenAPI.Converter (
    InternalTool (..),
    NameMapping (..),
    buildToolNameMapping,
    convertOpenAPIToTools,
    normalizeForLLM,
 )
import System.Agents.Tools.OpenAPI.Resolver (dereferenceSpec)
import System.Agents.Tools.OpenAPI.Types (
    Method,
    Operation (..),
    ParamLocation (..),
    Parameter (..),
    Path,
    ToolResult (..),
 )
import qualified System.Agents.Tools.Secrets as Secrets

-- -------------------------------------------------------------------------
-- Trace types
-- -------------------------------------------------------------------------

{- | Trace events for OpenAPI toolbox operations.

These events allow monitoring of:
* Spec fetching progress
* Tool conversion status
* Endpoint execution
* Error conditions
-}
data Trace
    = -- | URL being fetched
      FetchingSpecTrace !Text
    | -- | Loading spec from file path
      FetchingSpecFromFileTrace !Text
    | -- | Loading spec from file path
      FetchingSpecFromUrlTrace !HttpClient.Trace
    | -- | HTTP status of spec fetch
      SpecFetchedTrace !Int
    | -- | Successfully loaded spec from file
      SpecLoadedFromFileTrace !FilePath
    | -- | Number of tools converted from spec
      ToolsConvertedTrace !Int
    | -- | Number of tools before and after filtering
      ToolsFilteredTrace !Int !Int
    | -- | method, url, path being called
      CallingEndpointTrace !Text !Text !Text
    | -- | method, url, path being called
      ExecuteRequest !HttpClient.Trace
    | -- | HTTP status of endpoint response
      EndpointResponseTrace !Int
    | -- | Error during schema dereferencing
      SchemaResolutionErrorTrace !Text
    | -- | Error during tool execution
      ToolExecutionErrorTrace !Text
    | -- | Original name and normalized name (for debugging)
      ToolNameNormalizedTrace !Text !Text
    | -- | Secrets resolved successfully
      SecretsResolvedTrace !Int
    | -- | Error resolving secrets
      SecretsResolutionErrorTrace !Text
    deriving (Show)

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
    | -- | Secret resolution error
      SecretError !Secrets.SecretResolutionError
    deriving (Show, Eq)

{- | Configuration for initializing an OpenAPI toolbox.

The 'configSecrets' field allows flexible secret management. Secrets are
resolved at initialization time and applied to all outgoing requests.

The 'configActivation' field controls progressive disclosure for all tools
from this toolbox. If not specified, defaults to always activated.
-}
data Config = Config
    { configUrl :: Text
    -- ^ URL to fetch OpenAPI spec from (supports http/https or file://)
    , configBaseUrl :: Text
    -- ^ Base URL for API calls
    , configHeaders :: Map Text Text
    -- ^ Static headers to include in all requests
    , configToken :: Maybe Text
    -- ^ Optional Bearer token for authentication (legacy, prefer secrets)
    , configFilter :: Maybe EndpointPredicate
    {- ^ Optional filter to restrict which endpoints are exposed as tools.
    If not specified, all endpoints are included.
    -}
    , configSecrets :: [Secrets.Secret]
    -- ^ Secrets to resolve and include in requests
    , configActivation :: Maybe Activation
    -- ^ Optional activation mode for tools from this toolbox (default: always activated)
    }
    deriving (Show, Eq)

{- | Runtime state for an OpenAPI toolbox.

The toolbox maintains:
* A name for identification
* The base URL for API calls
* A list of available tools (filtered by configFilter if provided)
* A name mapping for LLM-safe tool names
* The HTTP runtime for making requests
* Resolved secrets for authentication
* Static headers from config
* Activation configuration for progressive disclosure
-}
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxBaseUrl :: Text
    , toolboxTools :: [InternalTool]
    -- ^ Tools converted from the OpenAPI spec (filtered if configFilter was provided)
    , toolboxNameMapping :: Map Text NameMapping
    -- ^ Mapping from normalized LLM names to original operation IDs
    , httpRuntime :: HttpClient.Runtime
    -- ^ HTTP client runtime
    , resolvedSecrets :: [Secrets.ResolvedSecret]
    -- ^ Secrets resolved at initialization
    , staticHeaders :: Map Text Text
    -- ^ Static headers from config
    , toolboxFilter :: Maybe EndpointPredicate
    -- ^ The filter used during initialization (stored for reference)
    , openApiActivation :: Maybe Activation
    -- ^ Activation configuration for progressive disclosure
    }

-- -------------------------------------------------------------------------
-- URL Helpers
-- -------------------------------------------------------------------------

{- | Check if a URL is a file:// URL.

Example:

>>> isFileUrl "file:///path/to/spec.json"
True

>>> isFileUrl "https://api.example.com/openapi.json"
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

{- | Initialize an OpenAPI toolbox from a configuration.

This function:
1. Resolves any configured secrets from their sources
2. Creates an HTTP runtime with authentication (legacy token support)
3. Fetches the OpenAPI spec from the configured URL (supports file:// URLs)
4. Parses the JSON to an OpenAPISpec
5. Dereferences schema references ($ref)
6. Converts operations to tools
7. Applies the optional filter to subset tools (using 'matchesOpenAPITool')
8. Builds the name mapping for LLM-safe names

Returns an 'InitializationError' if any step fails.
-}
initializeToolbox ::
    -- | Path to API keys file (for ApiKey secret source)
    FilePath ->
    Tracer IO Trace ->
    Config ->
    IO (Either InitializationError Toolbox)
initializeToolbox apiKeysFile tracer config = do
    -- Resolve secrets first
    eResolvedSecrets <- Secrets.resolveSecrets apiKeysFile (configSecrets config)
    case eResolvedSecrets of
        Left err -> do
            runTracer tracer $ SecretsResolutionErrorTrace (Text.pack $ show err)
            pure $ Left $ SecretError err
        Right resolvedSecrets -> do
            runTracer tracer $ SecretsResolvedTrace (length resolvedSecrets)

            -- Create HTTP runtime with auth (legacy token support)
            let token = case config.configToken of
                    Just t -> HttpClient.BearerToken t
                    Nothing -> HttpClient.NoToken
            runtime <- HttpClient.newRuntime token

            -- Fetch OpenAPI spec (from URL or file)
            -- Pass resolved secrets so they can be applied to the initial request
            fetchResult <- fetchSpec tracer runtime resolvedSecrets config.configUrl

            case fetchResult of
                Left err -> pure $ Left err
                Right body -> do
                    -- Parse JSON to OpenAPISpec
                    case Aeson.decode body of
                        Nothing -> pure $ Left $ ParseError "Failed to parse OpenAPI spec JSON"
                        Just spec -> do
                            -- Dereference schemas
                            let dereferencedSpec = dereferenceSpec spec

                            -- Convert to tools
                            let allTools = convertOpenAPIToTools dereferencedSpec
                            runTracer tracer (ToolsConvertedTrace (length allTools))

                            -- Apply filter if provided
                            let filteredTools = case config.configFilter of
                                    Nothing -> allTools
                                    Just predicate -> filter (matchesOpenAPITool predicate) allTools

                            -- Trace filtering results
                            runTracer tracer (ToolsFilteredTrace (length allTools) (length filteredTools))

                            -- Build name mapping for LLM-safe names (using filtered tools)
                            let nameMapping = buildToolNameMapping toolboxName filteredTools

                            -- Trace name normalizations (for debugging)
                            mapM_
                                (\m -> runTracer tracer (ToolNameNormalizedTrace (nmOriginal m) (nmNormalized m)))
                                (Map.elems nameMapping)

                            -- Create toolbox
                            pure $
                                Right $
                                    Toolbox
                                        { toolboxName = toolboxName
                                        , toolboxBaseUrl = config.configBaseUrl
                                        , toolboxTools = filteredTools
                                        , toolboxNameMapping = nameMapping
                                        , httpRuntime = runtime
                                        , resolvedSecrets = resolvedSecrets
                                        , staticHeaders = config.configHeaders
                                        , toolboxFilter = config.configFilter
                                        , openApiActivation = config.configActivation
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

{- | Fetch the OpenAPI spec from a URL or file.

Supports both HTTP/HTTPS URLs and file:// URLs. For file URLs,
the spec is read directly from the filesystem.

For HTTP URLs, resolved secrets are applied to headers and query string
parameters, allowing authentication when fetching protected specs.
-}
fetchSpec ::
    Tracer IO Trace ->
    HttpClient.Runtime ->
    [Secrets.ResolvedSecret] ->
    Text ->
    IO (Either InitializationError LByteString.ByteString)
fetchSpec tracer runtime resolvedSecrets url
    | isFileUrl url = fetchSpecFromFile tracer (fileUrlToPath url)
    | otherwise = fetchSpecFromUrl tracer runtime resolvedSecrets url

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

{- | Fetch the OpenAPI spec from an HTTP URL.

This function applies resolved secrets to both headers and query string
parameters, allowing authentication when fetching protected OpenAPI specs.
-}
fetchSpecFromUrl ::
    Tracer IO Trace ->
    HttpClient.Runtime ->
    [Secrets.ResolvedSecret] ->
    Text ->
    IO (Either InitializationError LByteString.ByteString)
fetchSpecFromUrl tracer runtime resolvedSecrets url = do
    runTracer tracer (FetchingSpecTrace url)

    -- Apply secrets to headers and query string for the initial request
    let secretHeaders = Secrets.applySecretsToHeaders resolvedSecrets Map.empty
    let secretQueryParams = Secrets.applySecretsToQueryString resolvedSecrets Map.empty

    -- Build and execute the request with secrets applied
    result <- try $ do
        req <- buildRequest "GET" url secretQueryParams Nothing secretHeaders
        HttpClient.runRequest runtime (contramap FetchingSpecFromUrlTrace tracer) req

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
-- Tool Handler Creation
-- -------------------------------------------------------------------------

{- | Creates a handler function for an OpenAPI tool.

This handler integrates with the ToolExecutionContext system and
returns results in the standard CallResult format.
-}
createToolHandler ::
    Toolbox ->
    InternalTool ->
    Tracer IO Trace ->
    ToolExecutionContext ->
    Value ->
    IO (CallResult ())
createToolHandler toolbox tool tracer _ctx args = do
    result <- handleToolCall tracer toolbox tool args
    case result of
        Left err -> do
            runTracer tracer (ToolExecutionErrorTrace err)
            pure $ IOToolError () (ScriptExecutionError (Text.unpack err))
        Right (textResult, _toolResult) -> do
            pure $ BlobToolSuccess () (ByteString.pack $ Text.unpack textResult)

{- | Handle a tool call by executing the HTTP request.

This function:
1. Extracts path params (prefixed with p_)
2. Extracts query params (prefixed with p_)
3. Extracts body (param "b")
4. Formats the path with path args
5. Builds full URL
6. Applies resolved secrets to headers
7. Makes HTTP request
8. Returns result
-}
handleToolCall ::
    Tracer IO Trace ->
    Toolbox ->
    InternalTool ->
    Value ->
    IO (Either Text (Text, ToolResult))
handleToolCall tracer toolbox tool args = do
    case args of
        Object obj -> do
            -- Extract parameters from the JSON object
            let objMap = KeyMap.toMapText obj
            let (pathParams, queryParams, bodyValue) = extractParams objMap (toolOperation tool)

            -- Format the path with path parameters
            let formattedPath = formatPath (toolPath tool) pathParams

            -- Build full URL
            let fullUrl = buildFullUrl (toolboxBaseUrl toolbox) formattedPath

            -- Build headers: static headers + resolved secrets
            let secretHeaders = Secrets.applySecretsToHeaders (resolvedSecrets toolbox) Map.empty
            let allHeaders = Map.unions [secretHeaders, staticHeaders toolbox]

            -- Apply query string secrets
            let secretQueryParams = Secrets.applySecretsToQueryString (resolvedSecrets toolbox) Map.empty
            let finalQueryParams = Map.union queryParams secretQueryParams

            -- Make HTTP request
            let method = toolMethod tool
            runTracer tracer $ CallingEndpointTrace method fullUrl formattedPath

            result <- executeRequest tracer toolbox.httpRuntime method fullUrl finalQueryParams bodyValue allHeaders

            case result of
                Left err -> pure $ Left err
                Right (textResult, toolResult) -> do
                    runTracer tracer $ EndpointResponseTrace (resultStatus toolResult)
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
                , paramIn p == ParamInPath =
                    Map.insert (Text.drop 2 k) v acc
            extractPathParam _ _ acc = acc

        -- Extract query params
        queryParams = Map.foldrWithKey extractQueryParam Map.empty obj
          where
            extractQueryParam k (String v) acc
                | Just p <- findParam params (Text.drop 2 k)
                , paramIn p == ParamInQuery =
                    Map.insert (Text.drop 2 k) v acc
            extractQueryParam k v acc
                | Just p <- findParam params (Text.drop 2 k)
                , paramIn p == ParamInQuery =
                    Map.insert (Text.drop 2 k) (valueToText v) acc
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

{- | Format a path template with path parameters.

Converts a path like @/pets/{petId}@ with @{p_petId: "123"}@ to @/pets/123@.

Example:

>>> formatPath "/pets/{petId}" (Map.fromList [("petId", "123")])
"/pets/123"

>>> formatPath "/pets/{petId}/owners/{ownerId}" (Map.fromList [("petId", "123"), ("ownerId", "456")])
"/pets/123/owners/456"
-}
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

{- | Execute an HTTP request with custom headers.

This function builds a request with all parameters (method, URL, query params,
body, and custom headers) and executes it using the runtime's 'runRequest'
function.
-}
executeRequest ::
    Tracer IO Trace ->
    HttpClient.Runtime ->
    Method ->
    Text ->
    Map Text Text ->
    Maybe Value ->
    Map Text Text ->
    IO (Either Text (Text, ToolResult))
executeRequest tracer runtime method fullUrl queryParams mbody headers = do
    -- Build the request with all headers and parameters
    req <- buildRequest method fullUrl queryParams mbody headers

    -- Execute the request using the runtime
    result <- HttpClient.runRequest runtime (contramap ExecuteRequest tracer) req

    case result of
        Left err -> pure $ Left (Text.pack $ show err)
        Right response -> handleResponse (method, fullUrl) response

-- -------------------------------------------------------------------------
-- Response Handling
-- -------------------------------------------------------------------------

{- | Handle an HTTP response, converting to ToolResult.

Returns both text for the LLM and a structured ToolResult.

Note: Uses lenient UTF-8 decoding to handle binary data safely.
Invalid bytes are replaced with U+FFFD (replacement character).
-}
handleResponse ::
    (Method, Text) ->
    HttpClient.Response LByteString.ByteString ->
    IO (Either Text (Text, ToolResult))
handleResponse (method, url) response = do
    let status = HttpTypes.statusCode (HttpClient.responseStatus response)
    let body = HttpClient.responseBody response

    -- Parse body as JSON if possible, otherwise use lenient UTF-8 decoding
    let payload = case Aeson.decode body of
            Just val -> val
            Nothing -> String (Text.decodeUtf8With lenientDecode $ LByteString.toStrict body)

    -- Create text representation for LLM using lenient UTF-8 decoding
    let textResult =
            "HTTP "
                <> Text.pack (show status)
                <> "\n"
                <> Text.decodeUtf8With lenientDecode (LByteString.toStrict body)

    pure $
        Right
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

{- | Get operation ID from operation, falling back to tool name.

This is used for tool registration when creating unique tool names.
-}
getOperationId :: Operation -> Maybe Text
getOperationId = opOperationId

-- -------------------------------------------------------------------------
-- Naming helpers
-- -------------------------------------------------------------------------

{- | Normalize a tool name for LLM compatibility.

This is a re-export of 'normalizeForLLM' for convenience.
-}
normalizeToolName :: Text -> Text
normalizeToolName = normalizeForLLM

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
