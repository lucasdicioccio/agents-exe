{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | OpenAPI to Tool conversion logic.
--
-- This module converts parsed and dereferenced OpenAPI operations into the
-- internal Tool representation used by the agents system. It handles:
--
-- * Converting OpenAPI specs to lists of tools
-- * Building tool parameter schemas from OpenAPI parameters
-- * Naming conventions (operationId or path/method based)
-- * Parameter naming (p_ prefix for path/query, b for body)
--
-- Example:
-- >>> import qualified Data.Map.Strict as Map
-- >>> import qualified Data.Aeson as Aeson
-- >>> let op = Operation (Just "getPet") (Just "Get a pet") Nothing [] Nothing
-- >>> let tool = convertOperation "/pets/{id}" "GET" op
-- >>> tool.toolName
-- "openapi_getPet"
module System.Agents.Tools.OpenAPI.Converter (
    -- * Core types
    OpenAPITool (..),
    ToolParameters (..),
    PropertySchema (..),
    ToolHandler,

    -- * Main conversion functions
    convertOpenAPIToTools,
    convertOperation,

    -- * Parameter building
    buildToolProperty,
    paramToToolName,
    buildToolParameters,

    -- * Description building
    buildToolDescription,

    -- * Required parameters
    getRequiredParams,

    -- * Naming
    deriveToolName,
    sanitizePathForName,

    -- * OpenAI conversion
    toOpenAITool,

    -- * Handler helpers
    defaultHandler,
) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))
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

-- -------------------------------------------------------------------------
-- Core Types
-- -------------------------------------------------------------------------

-- | Handler function type for executing OpenAPI tool calls.
-- This is a placeholder type - actual execution would be implemented
-- by the caller based on their HTTP client needs.
type ToolHandler = Map Text Value -> IO (Either Text Text)

-- | Internal representation of a tool derived from OpenAPI.
data OpenAPITool = OpenAPITool
    { toolOperation :: Operation
    , toolPath :: Path
    , toolMethod :: Method
    , toolName :: Text
    , toolDescription :: Text
    , toolParameters :: ToolParameters
    }
    deriving (Show, Eq)

-- | Parameters structure for LLM tool definitions.
data ToolParameters = ToolParameters
    { paramsType :: Text
    -- ^ Always "object" for OpenAPI tools
    , paramsProperties :: Map Text PropertySchema
    -- ^ Map of parameter name to property schema
    , paramsRequired :: [Text]
    -- ^ List of required parameter names
    }
    deriving (Show, Eq)

-- | Property schema for LLM tool parameters.
-- Mirrors JSON Schema structure needed for LLM tool definitions.
data PropertySchema = PropertySchema
    { propType :: Maybe Text
    -- ^ JSON Schema type (string, integer, boolean, etc.)
    , propDescription :: Text
    -- ^ Description for the LLM
    , propEnum :: Maybe [Text]
    -- ^ For enum values, stores the allowed values
    , propItems :: Maybe PropertySchema
    -- ^ For arrays, the schema of array items
    , propAnyOf :: Maybe [PropertySchema]
    -- ^ For anyOf schemas, the possible schemas
    }
    deriving (Show, Eq)

-- -------------------------------------------------------------------------
-- Main Conversion Functions
-- -------------------------------------------------------------------------

-- | Converts an OpenAPI spec into a list of tools.
--
-- This function walks through all paths and methods in the spec,
-- converting each operation into an 'OpenAPITool'.
--
-- Example:
-- >>> let spec = OpenAPISpec (Map.singleton "/pets" (Map.singleton "GET" op)) Nothing
-- >>> let tools = convertOpenAPIToTools spec
-- >>> length tools
-- 1
convertOpenAPIToTools ::
    OpenAPISpec ->
    [OpenAPITool]
convertOpenAPIToTools spec = do
    (path, methods) <- Map.toList (specPaths spec)
    (method, operation) <- Map.toList methods
    pure $ convertOperation path method operation

-- | Converts a single OpenAPI operation to a tool.
--
-- This is the core conversion function that:
-- * Derives a tool name from operationId or path/method
-- * Builds the tool description from summary and description
-- * Converts parameters to property schemas
-- * Tracks required parameters
--
-- Example:
-- >>> let op = Operation (Just "getPet") (Just "Get pet") (Just "Returns a pet") [] Nothing
-- >>> let tool = convertOperation "/pets/{id}" "GET" op
-- >>> tool.toolName
-- "openapi_getPet"
convertOperation ::
    Path ->
    Method ->
    Operation ->
    OpenAPITool
convertOperation path method op =
    OpenAPITool
        { toolOperation = op
        , toolPath = path
        , toolMethod = method
        , toolName = deriveToolName path method op
        , toolDescription = buildToolDescription op
        , toolParameters = buildToolParameters op
        }

-- -------------------------------------------------------------------------
-- Tool Name Derivation
-- -------------------------------------------------------------------------

-- | Derives a tool name from an OpenAPI operation.
--
-- Uses operationId if available, otherwise falls back to {method}_{path}.
-- All names are prefixed with "openapi_".
--
-- Examples:
-- >>> deriveToolName "/pets" "GET" (Operation (Just "listPets") Nothing Nothing [] Nothing)
-- "openapi_listPets"
-- >>> deriveToolName "/pets/{id}" "GET" (Operation Nothing Nothing Nothing [] Nothing)
-- "openapi_get_pets__id_"
deriveToolName :: Path -> Method -> Operation -> Text
deriveToolName path method op =
    "openapi_" <> case opOperationId op of
        Just opId -> opId
        Nothing -> sanitizePathForName method path

-- | Sanitizes a path for use in a tool name.
--
-- Converts the path/method combination to a valid identifier.
-- Replaces special characters with underscores.
--
-- Examples:
-- >>> sanitizePathForName "GET" "/pets"
-- "get_pets"
-- >>> sanitizePathForName "POST" "/pets/{id}/owner"
-- "post_pets__id__owner"
-- >>> sanitizePathForName "GET" "/api/v1/users"
-- "get_api_v1_users"
sanitizePathForName :: Method -> Path -> Text
sanitizePathForName method path =
    let methodPart = Text.toLower method
        -- Remove leading slash and replace special chars with underscore
        pathPart =
            Text.replace "/" "_"
                . Text.replace "{" "_"
                . Text.replace "}" "_"
                . Text.dropWhile (== '/')
                $ path
        -- Clean up multiple consecutive underscores
        cleaned = Text.replace "__" "_" pathPart
     in if Text.null cleaned
            then methodPart
            else methodPart <> "_" <> cleaned

-- -------------------------------------------------------------------------
-- Description Building
-- -------------------------------------------------------------------------

-- | Builds a tool description from an operation's summary and description.
--
-- Combines both fields with a colon separator if both are present.
--
-- Examples:
-- >>> let op = Operation Nothing (Just "Get a pet") (Just "Returns a single pet") [] Nothing
-- >>> buildToolDescription op
-- "Get a pet:\nReturns a single pet"
-- >>> let op2 = Operation Nothing Nothing (Just "Just description") [] Nothing
-- >>> buildToolDescription op2
-- ":\nJust description"
buildToolDescription :: Operation -> Text
buildToolDescription op =
    fromMaybe "" (opSummary op) <> ":\n" <> fromMaybe "" (opDescription op)

-- -------------------------------------------------------------------------
-- Parameter Building
-- -------------------------------------------------------------------------

-- | Converts a parameter to its tool property name.
--
-- Prefixes path and query parameters with "p_" to avoid conflicts.
-- Header parameters are also prefixed with "p_".
--
-- Examples:
-- >>> let param = Parameter "petId" ParamInPath Nothing True Nothing
-- >>> paramToToolName param
-- "p_petId"
-- >>> let param2 = Parameter "limit" ParamInQuery (Just "Max results") False Nothing
-- >>> paramToToolName param2
-- "p_limit"
paramToToolName :: Parameter -> Text
paramToToolName param = "p_" <> paramName param

-- | Builds tool parameters structure from an operation.
--
-- Collects all parameters (path, query, header) and body into a
-- unified parameter schema structure.
buildToolParameters :: Operation -> ToolParameters
buildToolParameters op =
    ToolParameters
        { paramsType = "object"
        , paramsProperties = paramProps <> bodyProps
        , paramsRequired = getRequiredParams op
        }
  where
    -- Convert operation parameters to properties
    paramProps = Map.fromList $ mapMaybe paramToProperty (opParameters op)

    -- Convert request body to property if present
    bodyProps = case opRequestBody op of
        Just body -> bodyToProperty body
        Nothing -> Map.empty

    -- Convert a single parameter to (name, schema) if it has a schema
    paramToProperty :: Parameter -> Maybe (Text, PropertySchema)
    paramToProperty param = do
        schema <- paramSchema param
        pure (paramToToolName param, buildToolProperty schema (paramDescription param))

    -- Convert request body to property map
    bodyToProperty :: RequestBody -> Map Text PropertySchema
    bodyToProperty body =
        -- Use "b" as the body parameter name
        -- For simplicity, we take the first schema from content types
        case Map.elems (reqBodyContent body) of
            [] -> Map.empty
            (schema : _) -> Map.singleton "b" (schemaToProperty schema (bodyDescription body))

    bodyDescription :: RequestBody -> Text
    bodyDescription body = fromMaybe "Request body" (reqBodyDescription body)

-- | Builds a property schema from an OpenAPI schema.
--
-- Handles:
-- * Enum values: copies type and augments description
-- * Arrays: copies items schema
-- * AnyOf: copies anyOf schemas
-- * Basic types: copies type and description
--
-- The parameter description is merged with the schema description.
buildToolProperty ::
    Schema ->
    Maybe Text ->
    PropertySchema
buildToolProperty schema paramDesc =
    PropertySchema
        { propType = schemaType schema
        , propDescription = buildDescription schema
        , propEnum = extractEnum schema
        , propItems = fmap (`buildToolProperty` Nothing) (schemaItems schema)
        , propAnyOf = fmap (map (`buildToolProperty` Nothing)) (schemaAnyOf schema)
        }
  where
    -- Build description from parameter description, schema description, and enum values
    buildDescription :: Schema -> Text
    buildDescription s =
        let baseDesc = fromMaybe "" (paramDesc <|> schemaDescription s)
            enumDesc = case schemaEnum s of
                Just vals -> "\nAllowed values: " <> formatEnumValues vals
                Nothing -> ""
         in baseDesc <> enumDesc

    -- Format enum values as a readable string
    formatEnumValues :: [Aeson.Value] -> Text
    formatEnumValues vals =
        let txtVals = mapMaybe extractString vals
         in Text.intercalate ", " txtVals

    extractString :: Aeson.Value -> Maybe Text
    extractString (Aeson.String s) = Just s
    extractString _ = Nothing

    -- Extract enum values as Text list
    extractEnum :: Schema -> Maybe [Text]
    extractEnum s =
        case schemaEnum s of
            Just vals -> Just $ mapMaybe extractString vals
            Nothing -> Nothing

-- | Converts a Schema to PropertySchema without parameter description.
schemaToProperty :: Schema -> Text -> PropertySchema
schemaToProperty schema desc =
    PropertySchema
        { propType = schemaType schema
        , propDescription = desc <> fromMaybe "" (fmap ("\n" <>) (schemaDescription schema))
        , propEnum = extractEnum schema
        , propItems = fmap (`schemaToProperty` "") (schemaItems schema)
        , propAnyOf = fmap (map (`schemaToProperty` "")) (schemaAnyOf schema)
        }
  where
    extractEnum :: Schema -> Maybe [Text]
    extractEnum s =
        case schemaEnum s of
            Just vals -> Just $ mapMaybe extractString vals
            Nothing -> Nothing

    extractString :: Aeson.Value -> Maybe Text
    extractString (Aeson.String s) = Just s
    extractString _ = Nothing

-- Alternative operator for Maybe
(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) (Just x) _ = Just x
(<|>) Nothing y = y

-- -------------------------------------------------------------------------
-- Required Parameters
-- -------------------------------------------------------------------------

-- | Gets the list of required parameter names for a tool.
--
-- Includes:
-- * Parameters marked with required=true
-- * Body parameter "b" if request body is required
getRequiredParams :: Operation -> [Text]
getRequiredParams op =
    paramNames <> bodyName
  where
    -- Get required parameter names (prefixed)
    paramNames =
        map paramToToolName $
            filter paramRequired (opParameters op)

    -- Check if body is required
    bodyName = case opRequestBody op of
        Just body | reqBodyRequired body -> ["b"]
        _ -> []

-- -------------------------------------------------------------------------
-- Conversion to OpenAI Tool Format
-- -------------------------------------------------------------------------

-- | Converts an OpenAPITool to the OpenAI Tool format.
--
-- This allows OpenAPITools to be used directly with the LLM interface.
--
-- Example:
-- >>> let op = Operation (Just "test") (Just "Test op") (Just "A test") [] Nothing
-- >>> let apiTool = convertOperation "/test" "GET" op
-- >>> let tool = toOpenAITool apiTool
-- >>> OpenAI.toolName tool
-- ToolName {getToolName = "openapi_test"}
toOpenAITool :: OpenAPITool -> OpenAI.Tool
toOpenAITool apiTool =
    OpenAI.Tool
        { OpenAI.toolName = OpenAI.ToolName (toolName apiTool)
        , OpenAI.toolDescription = toolDescription apiTool
        , OpenAI.toolParamProperties = map propertyToParamProperty $ Map.toList (paramsProperties (toolParameters apiTool))
        }
  where
    propertyToParamProperty :: (Text, PropertySchema) -> ParamProperty
    propertyToParamProperty (name, prop) =
        ParamProperty
            { propertyKey = name
            , propertyType = schemaTypeToParamType prop
            , propertyDescription = propDescription prop
            }

    schemaTypeToParamType :: PropertySchema -> ParamType
    schemaTypeToParamType prop =
        case propEnum prop of
            Just vals -> EnumParamType vals
            Nothing -> case propType prop of
                Just "string" -> StringParamType
                Just "integer" -> NumberParamType
                Just "number" -> NumberParamType
                Just "boolean" -> BoolParamType
                Just "array" -> case propItems prop of
                    Just items -> OpaqueParamType (fromMaybe "any" (propType items))
                    Nothing -> OpaqueParamType "array"
                Just "object" -> ObjectParamType []
                Just other -> OpaqueParamType other
                Nothing -> OpaqueParamType "any"

-- -------------------------------------------------------------------------
-- Handler Helpers
-- -------------------------------------------------------------------------

-- | Default handler that always returns an error.
-- The actual handler should be provided by the caller.
defaultHandler :: ToolHandler
defaultHandler _ = pure $ Left "Tool handler not implemented"

