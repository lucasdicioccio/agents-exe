{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PostgREST Toolbox runtime for executing database API tools.
--
-- This module provides a runtime that:
-- * Fetches PostgREST OpenAPI specifications from URLs
-- * Converts table endpoints to executable tools
-- * Handles HTTP execution with PostgREST-specific query parameter building
-- * Integrates with the LLM tool registration system
--
-- The toolbox follows patterns similar to OpenAPIToolbox, McpToolbox,
-- and BashToolbox for consistent integration with the agents system.
--
-- Key features:
-- * Structured query parameters (filters/subset/ranking)
-- * Column-based row filtering
-- * Automatic pagination and ordering support
-- * JWT Bearer token authentication
--
-- Example usage:
--
-- @
-- import System.Agents
-- import qualified System.Agents.Tools.PostgRESToolbox as PostgREST
--
-- main :: IO ()
-- main = do
--     let config = PostgREST.Config
--             { configUrl = "http://localhost:3000/"
--             , configBaseUrl = "http://localhost:3000"
--             , configHeaders = Map.empty
--             , configToken = Just "eyJhbG..."
--             }
--     result <- PostgREST.initializeToolbox tracer config
--     case result of
--         Right toolbox -> do
--             -- Register tools with LLM
--             regResult <- registerPostgRESTools toolbox
--             case regResult of
--                 Right registrations -> useWithAgent registrations
--                 Left err -> handleError err
--         Left err -> handleError err
-- @
module System.Agents.Tools.PostgRESToolbox (
    -- * Core types
    Trace,
    Toolbox (..),
    Config (..),
    InitializationError (..),

    -- * Re-exports from Types
    ToolResult,
    PostgRESTool (..),
    RowFilter (..),

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
import Data.Aeson (Value (..), Object)
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
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types as HttpTypes
import Numeric (showHex)
import Prod.Tracer (Tracer (..), runTracer)

import qualified System.Agents.HttpClient as HttpClient
import System.Agents.Tools.Base (CallResult (..))
import System.Agents.Tools.Context (ToolExecutionContext)
import System.Agents.Tools.IO (RunError (..))
import System.Agents.Tools.OpenAPI.Converter (normalizeForLLM)
import System.Agents.Tools.OpenAPI.Types (OpenAPISpec (..))
import System.Agents.Tools.PostgREST.Converter (
    PostgRESTool (..),
    convertPostgRESToTools,
    buildToolParameters,
 )
import System.Agents.Tools.PostgREST.Types (
    Config (..),
    RowFilter (..),
    ToolParameters (..),
    FilterSchema (..),
    SubsetSchema (..),
    RankingSchema (..),
    ToolResult (..),
    Trace (..),
 )
import System.Agents.Tools.Trace (ToolTrace (..))
import qualified System.Agents.LLMs.OpenAI as OpenAI

-- -------------------------------------------------------------------------
-- Core types
-- -------------------------------------------------------------------------

-- | Errors that can occur during toolbox initialization.
data InitializationError
    = NetworkError !Text
    -- ^ Network error fetching spec
    | FileError !Text
    -- ^ File error loading spec from disk
    | ParseError !Text
    -- ^ JSON parse error
    | SpecError !Text
    -- ^ OpenAPI spec validation error
    deriving (Show, Eq)

-- | Runtime state for a PostgREST toolbox.
--
-- The toolbox maintains:
-- * A name for identification
-- * The base URL for API calls
-- * A list of available tools (one per table)
-- * The HTTP runtime for making requests
-- * Optional dynamic header function for auth
data Toolbox = Toolbox
    { toolboxName :: Text
    , toolboxBaseUrl :: Text
    , toolboxTools :: [PostgRESTool]
    -- ^ Tools converted from the PostgREST spec
    , httpRuntime :: HttpClient.Runtime
    -- ^ HTTP client runtime
    , headerFunc :: Maybe (IO (Map Text Text))
    -- ^ Optional function to get dynamic headers (e.g., for auth)
    , staticHeaders :: Map Text Text
    -- ^ Static headers from config
    }

-- -------------------------------------------------------------------------
-- URL Helpers
-- -------------------------------------------------------------------------

-- | Check if a URL is a file:// URL.
--
-- Example:
--
-- >>> isFileUrl "file:///path/to/spec.json"
-- True
--
-- >>> isFileUrl "http://localhost:3000/"
-- False
isFileUrl :: Text -> Bool
isFileUrl url = Text.isPrefixOf "file://" url

-- | Convert a file:// URL to a file path.
--
-- Handles both "file:///absolute/path" and "file://relative/path" formats.
--
-- Example:
--
-- >>> fileUrlToPath "file:///home/user/spec.json"
-- "/home/user/spec.json"
--
-- >>> fileUrlToPath "file://spec.json"
-- "spec.json"
fileUrlToPath :: Text -> FilePath
fileUrlToPath url =
    let withoutPrefix = Text.drop 7 url  -- Drop "file://"
    in Text.unpack withoutPrefix

-- -------------------------------------------------------------------------
-- Initialization
-- -------------------------------------------------------------------------

-- | Initialize a PostgREST toolbox from a configuration.
--
-- This function:
-- 1. Creates an HTTP runtime with authentication
-- 2. Fetches the OpenAPI spec from the configured URL (supports file:// URLs)
-- 3. Parses the JSON to an OpenAPISpec
-- 4. Converts table operations to tools
-- 5. Detects row filters and builds parameter schemas
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

    -- Fetch OpenAPI spec (from URL or file)
    fetchResult <- fetchSpec tracer runtime config.configUrl

    case fetchResult of
        Left err -> pure $ Left err
        Right body -> do
            -- Parse JSON to OpenAPISpec
            case Aeson.decode body of
                Nothing -> pure $ Left $ ParseError "Failed to parse PostgREST OpenAPI spec JSON"
                Just spec -> do
                    -- Convert to tools
                    let toolboxName = extractToolboxName config.configUrl
                    let tools = convertPostgRESToTools toolboxName spec

                    -- Trace tool conversions
                    mapM_ (\t -> runTracer tracer (TableConvertedTrace (prtPath t))) tools

                    -- Trace filter detection
                    mapM_ (\t -> runTracer tracer (ColumnFiltersDetectedTrace (prtPath t) (length (prtRowFilters t)))) tools

                    -- Create toolbox
                    pure $ Right $ Toolbox
                        { toolboxName = toolboxName
                        , toolboxBaseUrl = config.configBaseUrl
                        , toolboxTools = tools
                        , httpRuntime = runtime
                        , headerFunc = Nothing
                        , staticHeaders = config.configHeaders
                        }

-- | Extract a toolbox name from the spec URL.
--
-- Uses the hostname as the toolbox name.
extractToolboxName :: Text -> Text
extractToolboxName url =
    let withoutProtocol = Text.dropWhile (/= '/') $ Text.drop 8 url
        hostPart = Text.takeWhile (/= '/') withoutProtocol
     in if Text.null hostPart
            then "postgrest"
            else normalizeForLLM hostPart

-- | Fetch the OpenAPI spec from a URL or file.
--
-- Supports both HTTP/HTTPS URLs and file:// URLs. For file URLs,
-- the spec is read directly from the filesystem.
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

-- | Get a tool by its name.
--
-- This is used during tool execution to find the tool
-- when the LLM calls it by name.
--
-- Returns 'Nothing' if no tool with that name exists.
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

-- | Creates a handler function for a PostgREST tool.
--
-- This handler integrates with the ToolExecutionContext system and
-- returns results in the standard CallResult format.
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

-- | Handle a tool call by executing the HTTP request.
--
-- This function:
-- 1. Extracts structured arguments (filters, subset, ranking)
-- 2. Builds the query string from arguments
-- 3. Builds full URL
-- 4. Makes HTTP GET request
-- 5. Returns result
handleToolCall ::
    Toolbox ->
    PostgRESTool ->
    Value ->
    IO (Either Text (Text, ToolResult))
handleToolCall toolbox tool args = do
    case args of
        Object obj -> do
            -- Build query string from structured arguments
            let queryString = buildQueryStringFromArgs obj (buildToolParameters tool)

            -- Build full URL
            let fullUrl = toolboxBaseUrl toolbox <> prtPath tool <> queryString

            -- Get headers
            headers <- case toolbox.headerFunc of
                Just hf -> hf
                Nothing -> pure Map.empty
            let allHeaders = Map.union headers (staticHeaders toolbox)

            -- Make HTTP request
            result <- executeGetRequest toolbox.httpRuntime fullUrl allHeaders

            case result of
                Left err -> pure $ Left err
                Right (textResult, toolResult) -> do
                    pure $ Right (textResult, toolResult)
        _ -> pure $ Left "Tool arguments must be a JSON object"

-- | Execute a GET request.
executeGetRequest ::
    HttpClient.Runtime ->
    Text ->
    Map Text Text ->
    IO (Either Text (Text, ToolResult))
executeGetRequest runtime url headers = do
    -- Build request
    req <- buildGetRequest url headers

    -- Execute the request
    result <- HttpClient.runRequest runtime (Tracer (const (pure ()))) req

    case result of
        Left err -> pure $ Left (Text.pack $ show err)
        Right response -> handleResponse url response

-- | Build a GET request with headers.
buildGetRequest ::
    Text ->
    Map Text Text ->
    IO HttpClient.Request
buildGetRequest url headers = do
    req <- HttpClient.parseRequest (Text.unpack url)

    -- Add custom headers
    let headerList = map (\(k, v) -> (mk (Text.encodeUtf8 k), Text.encodeUtf8 v)) $ Map.toList headers
    let finalReq = req{HttpClient.requestHeaders = HttpClient.requestHeaders req ++ headerList}

    pure finalReq

-- -------------------------------------------------------------------------
-- Query String Building
-- -------------------------------------------------------------------------

-- | Build query string from tool arguments.
--
-- Extracts filters, subset, and ranking from the arguments object
-- and converts them to PostgREST query parameters.
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

-- | Build filter parameters from filters object.
--
-- Converts filter values directly to query parameters.
-- PostgREST infers the eq operator when no operator is specified.
--
-- Examples:
-- >>> buildFilterParams (KeyMap.fromList [("name", String "John")])
-- fromList [("name","John")]
-- >>> buildFilterParams (KeyMap.fromList [("age", String "gt.18")])
-- fromList [("age","gt.18")]
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

-- | Build subset parameters from subset object.
--
-- Extracts limit, offset, and columns (mapped to select).
buildSubsetParams :: Object -> Map Text Text
buildSubsetParams subsetObj =
    let objMap = KeyMap.toMapText subsetObj
        extract key = case Map.lookup key objMap of
            Just (String s) | not (Text.null s) -> Just s
            _ -> Nothing
     in Map.fromList $ catMaybes
            [ fmap (\v -> ("limit", v)) (extract "limit")
            , fmap (\v -> ("offset", v)) (extract "offset")
            , fmap (\v -> ("select", v)) (extract "columns")
            ]
  where
    catMaybes = mapMaybe id

-- | Build ranking parameters from ranking object.
--
-- Extracts the order parameter.
buildRankingParams :: Object -> Map Text Text
buildRankingParams rankingObj =
    let objMap = KeyMap.toMapText rankingObj
     in case Map.lookup "order" objMap of
            Just (String s) | not (Text.null s) -> Map.singleton "order" s
            _ -> Map.empty

-- | Build query string from parameters map.
--
-- This is a lower-level function for building query strings directly.
buildQueryString :: Map Text Text -> Text
buildQueryString params
    | Map.null params = ""
    | otherwise =
        "?" <> Text.intercalate "&" (map encodeParam $ Map.toList params)
  where
    encodeParam (k, v) = urlEncode k <> "=" <> urlEncode v

-- | URL-encode a query parameter.
--
-- Basic URL encoding for query string parameters.
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

-- | Handle an HTTP response, converting to ToolResult.
--
-- Returns both text for the LLM and a structured ToolResult.
handleResponse ::
    Text ->
    HttpClient.Response LByteString.ByteString ->
    IO (Either Text (Text, ToolResult))
handleResponse url response = do
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
            , resultMethod = "GET"
            , resultStatus = status
            , resultPayload = payload
            }
        )

-- -------------------------------------------------------------------------
-- Naming helpers
-- -------------------------------------------------------------------------

-- | Convert a PostgREST tool name to an LLM tool name.
--
-- Names are prefixed with @postgrest_@ and include the normalized toolbox name
-- and tool name to avoid conflicts and ensure LLM compatibility.
--
-- Example:
--
-- >>> postgrest2LLMName "mydb" "get_users"
-- ToolName {getToolName = "postgrest_mydb_get_users"}
postgrest2LLMName :: Text -> Text -> OpenAI.ToolName
postgrest2LLMName toolboxName toolName =
    let normalizedToolbox = normalizeForLLM toolboxName
        normalizedTool = normalizeForLLM toolName
     in OpenAI.ToolName ("postgrest_" <> normalizedToolbox <> "_" <> normalizedTool)


