{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

module System.Agents.ToolSchema (
    ToolName (..),
    ToolDescription (..),
    ParamProperty (..),
    ParamType (..),
    jsonSchema,
    toJsonSchemaPair,
) where

import Data.Aeson (FromJSON, ToJSON, Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Generics (Generic)

data ParamProperty = ParamProperty
    { propertyKey :: Text
    , propertyType :: ParamType
    , propertyDescription :: Text
    , propertyRequired :: Bool
    {- ^ Whether this property is required. When True, the property key
    is included in the 'required' array of the JSON schema.
    This allows the LLM to omit optional parameters.
    -}
    }
    deriving (Show, Ord, Eq, Generic)
instance FromJSON ParamProperty
instance ToJSON ParamProperty

data ParamType
    = NullParamType
    | StringParamType
    | BoolParamType
    | NumberParamType
    | EnumParamType [Text]
    | OpaqueParamType Text
    | MultipleParamType Text -- todo: break limitation preventing string-enum and null
    | ObjectParamType [ParamProperty]
    | {- | A JSON Schema given verbatim (a service tool's declared
      @process.input.event@, for its typed @send@ companion); the
      description is added to it.
      -}
      SchemaParamType Value
    deriving (Show, Ord, Eq, Generic)
instance FromJSON ParamType
instance ToJSON ParamType

toJsonSchemaPair :: ParamProperty -> (Text, Value)
toJsonSchemaPair p = (p.propertyKey, Aeson.object (jsonSchema p))

jsonSchema :: ParamProperty -> [(Aeson.Key, Value)]
jsonSchema p =
    case p.propertyType of
        NullParamType ->
            ["type" .= ("null" :: Text), "description" .= p.propertyDescription]
        StringParamType ->
            ["type" .= ("string" :: Text), "description" .= p.propertyDescription]
        BoolParamType ->
            ["type" .= ("boolean" :: Text), "description" .= p.propertyDescription]
        NumberParamType ->
            ["type" .= ("number" :: Text), "description" .= p.propertyDescription]
        EnumParamType allowedValues ->
            ["type" .= ("string" :: Text), "enum" .= allowedValues, "description" .= p.propertyDescription]
        OpaqueParamType typ ->
            ["type" .= typ, "description" .= p.propertyDescription]
        MultipleParamType allowedTypes ->
            ["type" .= allowedTypes, "description" .= p.propertyDescription]
        SchemaParamType schema -> case schema of
            Object o -> [(k, v) | (k, v) <- KM.toList o, k /= "description"] <> ["description" .= p.propertyDescription]
            -- a boolean schema: true accepts anything, false nothing
            Bool False -> ["not" .= Aeson.object [], "description" .= p.propertyDescription]
            _ -> ["description" .= p.propertyDescription]
        ObjectParamType propz ->
            let requiredProps = filter propertyRequired propz
             in [ "type" .= ("object" :: Text)
                , "description" .= p.propertyDescription
                , "properties" .= HashMap.fromList (fmap toJsonSchemaPair propz)
                , "additionalProperties" .= False
                , "required" .= fmap propertyKey requiredProps
                ]

newtype ToolName = ToolName {getToolName :: Text}
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data ToolDescription = ToolDescription
    { toolDescriptionName :: ToolName
    , toolDescriptionText :: Text
    , toolDescriptionParamProperties :: [ParamProperty]
    }
    deriving (Show, Ord, Eq, Generic)
instance FromJSON ToolDescription
instance ToJSON ToolDescription
