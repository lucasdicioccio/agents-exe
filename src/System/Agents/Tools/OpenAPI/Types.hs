{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | OpenAPI 3.x specification types and JSON parsers.
--
-- This module provides a subset of OpenAPI 3.0/3.1 types needed for
-- LLM tool generation. It focuses on parsing paths, operations,
-- parameters, request bodies, and schemas.
--
-- Note: This is not a full OpenAPI implementation - only the subset
-- needed for tool generation is supported.
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
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText, toText, Key)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
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
data OpenAPISpec = OpenAPISpec
    { specPaths :: Map Path (Map Method Operation)
    , specComponents :: Maybe Components
    }
    deriving (Show, Eq)

instance FromJSON OpenAPISpec where
    parseJSON = withObject "OpenAPISpec" $ \o -> do
        -- Parse paths object into a Map Path (Map Method Operation)
        pathsObj <- o .: "paths"
        paths <- parsePaths pathsObj
        OpenAPISpec
            <$> pure paths
            <*> o .:? "components"

instance ToJSON OpenAPISpec where
    toJSON spec =
        object
            [ "paths" .= specPaths spec
            , "components" .=? specComponents spec
            ]

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
-- Parameter (path, query, header)
-- -------------------------------------------------------------------------

-- | Represents a parameter (path, query, or header parameter).
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

-- | Parameter location: path, query, or header.
data ParamLocation
    = ParamInPath
    | ParamInQuery
    | ParamInHeader
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

