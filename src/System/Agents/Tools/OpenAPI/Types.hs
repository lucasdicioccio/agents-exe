{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | OpenAPI 3.x and Swagger 2.0 specification types and JSON parsers.
--
-- This module provides a subset of OpenAPI 3.0/3.1 and Swagger 2.0 types needed for
-- LLM tool generation. It focuses on parsing paths, operations,
-- parameters, request bodies, and schemas.
--
-- Note: This is not a full OpenAPI implementation - only the subset
-- needed for tool generation is supported.
--
-- The module automatically converts Swagger 2.0 specs to OpenAPI 3.0 structure:
-- * 'definitions' -> 'components/schemas'
-- * 'in: body' parameters -> 'requestBody'
-- * parameter-level 'type' -> nested 'schema.type'
module System.Agents.Tools.OpenAPI.Types (
    -- * OpenAPI spec types
    OpenAPISpec (..),
    Path,
    Method,
    Operation (..),
    Parameter (..),
    ParamLocation (..),
    RequestBody (..),
    Schema (..),
    Components (..),
    MediaType,

    -- * Tool execution result
    ToolResult (..),

    -- * JSON helpers
    Pair,
    object,
    parseParamLocation,
    paramLocationToText,
    (.=?),
    (.=),
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), decode, withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText, toText, Key)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Aeson Pair type for building JSON objects.
type Pair = (Key, Value)

-- | Build JSON object from a list of optional pairs.
object :: [Maybe Pair] -> Value
object = Aeson.object . catMaybes

-- -------------------------------------------------------------------------
-- Top-level OpenAPI spec
-- -------------------------------------------------------------------------

-- | Top-level OpenAPI specification document.
-- Supports both OpenAPI 3.x and Swagger 2.0 formats.
data OpenAPISpec = OpenAPISpec
    { specPaths :: Map Path (Map Method Operation)
    , specComponents :: Maybe Components
    }
    deriving (Show, Eq)

instance FromJSON OpenAPISpec where
    parseJSON = withObject "OpenAPISpec" $ \o -> do
        -- Check for Swagger 2.0 vs OpenAPI 3.x
        mSwagger <- o .:? "swagger"
        mOpenapi <- o .:? "openapi"

        case (mSwagger :: Maybe Text, mOpenapi :: Maybe Text) of
            (Just "2.0", _) -> parseSwaggerV2 o
            (_, Just _) -> parseOpenAPIV3 o
            (_, _) -> parseOpenAPIV3 o  -- Default to OpenAPI 3.x parsing

-- | Parse OpenAPI 3.x format.
parseOpenAPIV3 :: KeyMap.KeyMap Value -> Parser OpenAPISpec
parseOpenAPIV3 o = do
    pathsObj <- o .: "paths"
    paths <- parsePaths pathsObj
    components <- o .:? "components"
    pure $ OpenAPISpec paths components

-- | Parse Swagger 2.0 format and convert to OpenAPI 3.x structure.
parseSwaggerV2 :: KeyMap.KeyMap Value -> Parser OpenAPISpec
parseSwaggerV2 o = do
    pathsObj <- o .: "paths"
    -- Parse paths with Swagger 2.0 conversion
    paths <- parsePathsSwaggerV2 pathsObj
    -- Convert definitions to components/schemas
    definitions <- o .:? "definitions"
    let components = fmap definitionsToComponents definitions
    pure $ OpenAPISpec paths components

-- | Convert Swagger 2.0 definitions to Components.
definitionsToComponents :: KeyMap.KeyMap Value -> Components
definitionsToComponents defs =
    let schemas = KeyMap.toList defs
        schemaMap = Map.fromList $ mapMaybe parseDefinition schemas
    in Components (if Map.null schemaMap then Nothing else Just schemaMap)
  where
    parseDefinition :: (Key, Value) -> Maybe (Text, Schema)
    parseDefinition (k, v) =
        case Aeson.fromJSON v of
            Aeson.Success schema -> Just (toText k, schema)
            Aeson.Error _ -> Nothing

-- | Parse the paths object where each path maps to a methods object.
parsePaths :: Value -> Parser (Map Path (Map Method Operation))
parsePaths = withObject "paths" $ \o -> do
    let keys = KeyMap.keys o
    fmap Map.fromList $ traverse (parsePathItem o) keys

-- | Parse a single path item from the paths object.
parsePathItem :: KeyMap.KeyMap Value -> Key -> Parser (Path, Map Method Operation)
parsePathItem o key = do
    let pathText = toText key
    val <- o .: key
    operations <- parseOperations val
    pure (pathText, operations)

-- | Parse an operations object (the value of a path).
-- This contains the HTTP methods like "get", "post", etc.
parseOperations :: Value -> Parser (Map Method Operation)
parseOperations = withObject "pathItem" $ \o -> do
    let methodKeys = filter (not . isExtensionKey) (KeyMap.keys o)
    fmap Map.fromList $ traverse (parseMethodOperation o) methodKeys
  where
    -- Skip OpenAPI extension fields (starting with "x-") and other non-method fields
    isExtensionKey k =
        let t = toText k
         in Text.isPrefixOf "x-" t || t `elem` ["parameters", "summary", "description", "servers"]

-- | Parse a single method operation.
parseMethodOperation :: KeyMap.KeyMap Value -> Key -> Parser (Method, Operation)
parseMethodOperation o key = do
    let methodText = Text.toUpper $ toText key
    op <- o .: key
    pure (methodText, op)

-- | Parse paths for Swagger 2.0, converting body parameters to requestBody.
parsePathsSwaggerV2 :: Value -> Parser (Map Path (Map Method Operation))
parsePathsSwaggerV2 = withObject "paths" $ \o -> do
    let keys = KeyMap.keys o
    fmap Map.fromList $ traverse (parsePathItemSwaggerV2 o) keys

-- | Parse a single path item for Swagger 2.0.
parsePathItemSwaggerV2 :: KeyMap.KeyMap Value -> Key -> Parser (Path, Map Method Operation)
parsePathItemSwaggerV2 o key = do
    let pathText = toText key
    val <- o .: key
    operations <- parseOperationsSwaggerV2 val
    pure (pathText, operations)

-- | Parse operations for Swagger 2.0, converting body params to requestBody.
parseOperationsSwaggerV2 :: Value -> Parser (Map Method Operation)
parseOperationsSwaggerV2 = withObject "pathItem" $ \o -> do
    let methodKeys = filter (not . isExtensionKey) (KeyMap.keys o)
    fmap Map.fromList $ traverse (parseMethodOperationSwaggerV2 o) methodKeys
  where
    isExtensionKey k =
        let t = toText k
         in Text.isPrefixOf "x-" t || t `elem` ["parameters", "summary", "description"]

-- | Parse a single method operation for Swagger 2.0.
parseMethodOperationSwaggerV2 :: KeyMap.KeyMap Value -> Key -> Parser (Method, Operation)
parseMethodOperationSwaggerV2 o key = do
    let methodText = Text.toUpper $ toText key
    opVal <- o .: key
    op <- parseOperationSwaggerV2 opVal
    pure (methodText, op)

-- | Parse Swagger 2.0 operation, converting body parameters to requestBody.
parseOperationSwaggerV2 :: Value -> Parser Operation
parseOperationSwaggerV2 = withObject "Operation" $ \o -> do
    operationId <- o .:? "operationId"
    summary <- o .:? "summary"
    description <- o .:? "description"
    rawParams <- fromMaybe [] <$> o .:? "parameters"

    -- Convert parameters: separate body params from regular params
    let (bodyParams, otherParams) = partitionBodyParams rawParams

    -- Convert body params to requestBody if present
    let mRequestBody = case bodyParams of
            [] -> Nothing
            (bodyParam : _) -> Just $ swaggerBodyParamToRequestBody bodyParam

    -- Parse other parameters (now handling $ref-only params)
    regularParams <- traverse parseParameterSwaggerV2 otherParams

    pure $ Operation operationId summary description regularParams mRequestBody
  where
    partitionBodyParams :: [Value] -> ([SwaggerBodyParam], [Value])
    partitionBodyParams = foldr classify ([], [])
      where
        classify v (bodies, others) =
            case parseMaybe parseSwaggerBodyParam v of
                Just bp -> (bp : bodies, others)
                Nothing -> (bodies, v : others)

    parseMaybe :: (Value -> Parser a) -> Value -> Maybe a
    parseMaybe p v = either (const Nothing) Just (parseEither p v)

-- | Temporary type for Swagger 2.0 body parameters.
data SwaggerBodyParam = SwaggerBodyParam
    { sbpDescription :: Maybe Text
    , sbpRequired :: Bool
    , sbpSchema :: Schema
    }

-- | Parse a Swagger 2.0 body parameter.
parseSwaggerBodyParam :: Value -> Parser SwaggerBodyParam
parseSwaggerBodyParam = withObject "body parameter" $ \o -> do
    paramIn <- o .: "in" :: Parser Text
    if paramIn == "body"
        then do
            desc <- o .:? "description"
            required <- fromMaybe False <$> o .:? "required"
            schema <- o .: "schema"
            pure $ SwaggerBodyParam desc required schema
        else fail "Not a body parameter"

-- | Convert Swagger 2.0 body parameter to RequestBody.
swaggerBodyParamToRequestBody :: SwaggerBodyParam -> RequestBody
swaggerBodyParamToRequestBody sbp =
    let content = Map.singleton "application/json" (sbpSchema sbp)
     in RequestBody (sbpDescription sbp) (sbpRequired sbp) content

-- | Parse Swagger 2.0 parameter (supports both direct type and schema, and $ref-only).
--
-- In Swagger 2.0, parameters can be:
-- 1. Full inline parameters with 'in', 'name', 'type' or 'schema'
-- 2. Reference-only parameters: {"$ref": "#/parameters/something"}
parseParameterSwaggerV2 :: Value -> Parser Parameter
parseParameterSwaggerV2 v = 
    -- First try to parse as a $ref-only parameter
    case parseMaybe parseRefOnlyParam v of
        Just param -> pure param
        Nothing -> parseFullParameterSwaggerV2 v
  where
    parseMaybe :: (Value -> Parser a) -> Value -> Maybe a
    parseMaybe p val = either (const Nothing) Just (parseEither p val)

    -- Parse a parameter that is just a $ref (no name, in, etc.)
    parseRefOnlyParam :: Value -> Parser Parameter
    parseRefOnlyParam = withObject "Parameter ($ref only)" $ \o -> do
        ref <- o .: "$ref"
        -- Create a parameter with just the schema ref
        -- We use "query" as a default location since it's most common for PostgREST
        pure $ Parameter
            { paramName = ""  -- Will be resolved later if needed
            , paramIn = ParamInQuery  -- Default, may be overridden
            , paramDescription = Nothing
            , paramRequired = False
            , paramSchema = Just $ Schema
                { schemaType = Nothing
                , schemaDescription = Nothing
                , schemaEnum = Nothing
                , schemaProperties = Nothing
                , schemaItems = Nothing
                , schemaAnyOf = Nothing
                , schemaRef = Just ref
                , schemaRequired = Nothing
                }
            }

    -- Parse a full inline parameter
    parseFullParameterSwaggerV2 :: Value -> Parser Parameter
    parseFullParameterSwaggerV2 = withObject "Parameter" $ \o -> do
        paramInStr <- o .: "in"
        paramIn <- parseParamLocationSwaggerV2 paramInStr
        name <- o .: "name"
        desc <- o .:? "description"
        required <- fromMaybe False <$> o .:? "required"

        -- Swagger 2.0 can have type directly or in schema
        mDirectType <- o .:? "type"
        mSchema <- o .:? "schema"

        -- Build schema from direct type if present
        let schema = case (mSchema, mDirectType) of
                (Just s, _) -> Just s
                (Nothing, Just t) -> Just $ Schema
                    { schemaType = Just t
                    , schemaDescription = Nothing
                    , schemaEnum = Nothing
                    , schemaProperties = Nothing
                    , schemaItems = Nothing
                    , schemaAnyOf = Nothing
                    , schemaRef = Nothing
                    , schemaRequired = Nothing
                    }
                (Nothing, Nothing) -> Nothing

        pure $ Parameter name paramIn desc required schema

-- | Parse parameter location for Swagger 2.0 (includes formData).
parseParamLocationSwaggerV2 :: Text -> Parser ParamLocation
parseParamLocationSwaggerV2 "path" = pure ParamInPath
parseParamLocationSwaggerV2 "query" = pure ParamInQuery
parseParamLocationSwaggerV2 "header" = pure ParamInHeader
parseParamLocationSwaggerV2 "formData" = pure ParamInFormData
parseParamLocationSwaggerV2 other = fail $ "Unknown parameter location: " ++ Text.unpack other

instance ToJSON OpenAPISpec where
    toJSON spec =
        object
            [ "paths" .= specPaths spec
            , "components" .=? specComponents spec
            ]

-- -------------------------------------------------------------------------
-- Path and method types
-- -------------------------------------------------------------------------

-- | URL path like "/pets" or "/pets/{petId}".
type Path = Text

-- | HTTP method like "GET", "POST", "PUT", "DELETE", etc.
type Method = Text

-- -------------------------------------------------------------------------
-- Operation (an endpoint)
-- -------------------------------------------------------------------------

-- | Represents an API operation (endpoint).
-- Corresponds to an Operation Object in OpenAPI.
data Operation = Operation
    { opOperationId :: Maybe Text
    , opSummary :: Maybe Text
    , opDescription :: Maybe Text
    , opParameters :: [Parameter]
    , opRequestBody :: Maybe RequestBody
    }
    deriving (Show, Eq)

instance FromJSON Operation where
    parseJSON = withObject "Operation" $ \o ->
        Operation
            <$> o .:? "operationId"
            <*> o .:? "summary"
            <*> o .:? "description"
            <*> (fromMaybe [] <$> o .:? "parameters")
            <*> o .:? "requestBody"

instance ToJSON Operation where
    toJSON op =
        object
            [ "operationId" .=? op.opOperationId
            , "summary" .=? op.opSummary
            , "description" .=? op.opDescription
            , "parameters" .=? if null op.opParameters then Nothing else Just op.opParameters
            , "requestBody" .=? op.opRequestBody
            ]

-- -------------------------------------------------------------------------
-- Parameter (path, query, header, formData)
-- -------------------------------------------------------------------------

-- | Represents a parameter (path, query, header, or formData parameter).
data Parameter = Parameter
    { paramName :: Text
    , paramIn :: ParamLocation
    , paramDescription :: Maybe Text
    , paramRequired :: Bool
    , paramSchema :: Maybe Schema
    }
    deriving (Show, Eq)

instance FromJSON Parameter where
    parseJSON = withObject "Parameter" $ \o -> do
        locationStr <- o .: "in"
        location <- parseParamLocation locationStr
        Parameter
            <$> o .: "name"
            <*> pure location
            <*> o .:? "description"
            <*> (fromMaybe False <$> o .:? "required")
            <*> o .:? "schema"

instance ToJSON Parameter where
    toJSON param =
        object
            [ "name" .= param.paramName
            , "in" .= paramLocationToText param.paramIn
            , "description" .=? param.paramDescription
            , "required" .=? if param.paramRequired then Just True else Nothing
            , "schema" .=? param.paramSchema
            ]

-- | Parameter location: path, query, header, or formData.
data ParamLocation
    = ParamInPath
    | ParamInQuery
    | ParamInHeader
    | ParamInFormData
    deriving (Show, Eq)

-- | Parse a parameter location from text.
parseParamLocation :: Text -> Parser ParamLocation
parseParamLocation "path" = pure ParamInPath
parseParamLocation "query" = pure ParamInQuery
parseParamLocation "header" = pure ParamInHeader
parseParamLocation other = fail $ "Unknown parameter location: " ++ Text.unpack other

-- | Convert a parameter location to text.
paramLocationToText :: ParamLocation -> Text
paramLocationToText ParamInPath = "path"
paramLocationToText ParamInQuery = "query"
paramLocationToText ParamInHeader = "header"
paramLocationToText ParamInFormData = "formData"

-- -------------------------------------------------------------------------
-- Request body
-- -------------------------------------------------------------------------

-- | Represents a request body for an operation.
data RequestBody = RequestBody
    { reqBodyDescription :: Maybe Text
    , reqBodyRequired :: Bool
    , reqBodyContent :: Map MediaType Schema
    }
    deriving (Show, Eq)

instance FromJSON RequestBody where
    parseJSON = withObject "RequestBody" $ \o -> do
        contentObj <- o .: "content"
        content <- parseContentMap contentObj
        RequestBody
            <$> o .:? "description"
            <*> (fromMaybe False <$> o .:? "required")
            <*> pure content

instance ToJSON RequestBody where
    toJSON rb =
        object
            [ "description" .=? rb.reqBodyDescription
            , "required" .=? if rb.reqBodyRequired then Just True else Nothing
            , "content" .= KeyMap.fromList (map mediaTypeSchemaToPair $ Map.toList rb.reqBodyContent)
            ]

-- | Parse a content map (media type -> media type object with schema).
parseContentMap :: Value -> Parser (Map MediaType Schema)
parseContentMap = withObject "content" $ \o -> do
    let keys = KeyMap.keys o
    fmap Map.fromList $ traverse (parseMediaTypeSchema o) keys

-- | Parse a single media type entry.
parseMediaTypeSchema :: KeyMap.KeyMap Value -> Key -> Parser (MediaType, Schema)
parseMediaTypeSchema o key = do
    let mediaType = toText key
    mediaObj <- o .: key
    schema <- mediaObj .: "schema"
    pure (mediaType, schema)

-- | Convert media type/schema pair to an Aeson pair.
mediaTypeSchemaToPair :: (MediaType, Schema) -> Pair
mediaTypeSchemaToPair (mediaType, schema) =
    (fromText mediaType, Aeson.object ["schema" Aeson..= schema])

-- | Media type like "application/json" or "application/xml".
type MediaType = Text

-- -------------------------------------------------------------------------
-- JSON Schema (subset needed for OpenAPI)
-- -------------------------------------------------------------------------

-- | JSON Schema subset used in OpenAPI.
-- Supports common schema types needed for tool generation.
data Schema = Schema
    { schemaType :: Maybe Text
    , schemaDescription :: Maybe Text
    , schemaEnum :: Maybe [Aeson.Value]
    , schemaProperties :: Maybe (Map Text Schema)
    , schemaItems :: Maybe Schema
    , schemaAnyOf :: Maybe [Schema]
    , schemaRef :: Maybe Text
    , schemaRequired :: Maybe [Text]
    }
    deriving (Show, Eq)

instance FromJSON Schema where
    parseJSON v =
        parseRefSchema v
            <|> parseInlineSchema v

-- | Parse a schema that uses $ref.
parseRefSchema :: Value -> Parser Schema
parseRefSchema = withObject "Schema ($ref)" $ \o -> do
    ref <- o .: "$ref"
    pure $
        Schema
            { schemaType = Nothing
            , schemaDescription = Nothing
            , schemaEnum = Nothing
            , schemaProperties = Nothing
            , schemaItems = Nothing
            , schemaAnyOf = Nothing
            , schemaRef = Just ref
            , schemaRequired = Nothing
            }

-- | Parse an inline schema definition.
parseInlineSchema :: Value -> Parser Schema
parseInlineSchema = withObject "Schema" $ \o -> do
    mType <- o .:? "type"
    mProps <- o .:? "properties"
    -- Convert properties from HashMap to Map if present
    let props = fmap HashMap.toList mProps >>= \pairs ->
            if null pairs then Nothing else Just $ Map.fromList $ map (\(k, v) -> (toText k, v)) pairs
    Schema
        <$> pure mType
        <*> o .:? "description"
        <*> o .:? "enum"
        <*> pure props
        <*> o .:? "items"
        <*> o .:? "anyOf"
        <*> pure Nothing
        <*> o .:? "required"

instance ToJSON Schema where
    toJSON schema =
        case schema.schemaRef of
            Just ref -> Aeson.object ["$ref" Aeson..= ref]
            Nothing ->
                object
                    [ "type" .=? schema.schemaType
                    , "description" .=? schema.schemaDescription
                    , "enum" .=? schema.schemaEnum
                    , "properties" .=? (fmap (Aeson.object . map schemaPropertyToPair . Map.toList) schema.schemaProperties)
                    , "items" .=? schema.schemaItems
                    , "anyOf" .=? schema.schemaAnyOf
                    , "required" .=? schema.schemaRequired
                    ]

-- | Convert schema property to an Aeson pair.
schemaPropertyToPair :: (Text, Schema) -> Pair
schemaPropertyToPair (key, val) = (fromText key, toJSON val)

-- -------------------------------------------------------------------------
-- Components section for reusable schemas
-- -------------------------------------------------------------------------

-- | Components section containing reusable schemas and other definitions.
data Components = Components
    { componentsSchemas :: Maybe (Map Text Schema)
    }
    deriving (Show, Eq)

instance FromJSON Components where
    parseJSON = withObject "Components" $ \o -> do
        mSchemas <- o .:? "schemas"
        -- Convert from HashMap to Map
        let schemas = mSchemas >>= \hm ->
                let pairs = HashMap.toList hm
                 in if null pairs then Nothing else Just $ Map.fromList $ map (\(k, v) -> (toText k, v)) pairs
        pure $ Components schemas

instance ToJSON Components where
    toJSON comps =
        object
            [ "schemas"
                .=? (fmap (Aeson.object . map schemaDefinitionToPair . Map.toList) comps.componentsSchemas)
            ]

-- | Convert schema definition to an Aeson pair.
schemaDefinitionToPair :: (Text, Schema) -> Pair
schemaDefinitionToPair (key, val) = (fromText key, toJSON val)

-- -------------------------------------------------------------------------
-- Tool execution result
-- -------------------------------------------------------------------------

-- | Result of executing an OpenAPI tool.
--
-- This type captures the structured result from executing an OpenAPI-based
-- tool call, including the HTTP response details.
data ToolResult = ToolResult
    { resultPath :: Text
    -- ^ Path that was called
    , resultMethod :: Text
    -- ^ HTTP method used
    , resultStatus :: Int
    -- ^ HTTP status code
    , resultPayload :: Value
    -- ^ Response payload as JSON
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

-- -------------------------------------------------------------------------
-- Helper functions for JSON construction
-- -------------------------------------------------------------------------

-- | Optional field helper - only include if value is present.
(.=?) :: (ToJSON v) => Key -> Maybe v -> Maybe Pair
(.=?) k v = fmap ((Aeson..=) k) v

infixr 8 .=?

-- | Required field helper.
(.=) :: (ToJSON v) => Key -> v -> Maybe Pair
(.=) k v = Just ((Aeson..=) k v)

infixr 8 .=

