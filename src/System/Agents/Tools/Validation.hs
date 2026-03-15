{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Tool input validation against schemas.

This module provides functionality to validate tool call inputs against
their defined schemas. This helps LLMs self-correct when they make incorrect
tool calls by providing helpful error messages.

The validation checks:

* Required fields are present
* Field types match the schema (string, number, boolean, enum, object)
* Enum values are valid
* Nested object structures are correct

Typical usage:

@
let errors = validateToolInput toolSchema inputValue
case errors of
    [] -> proceedWithToolCall
    errs -> returnValidationErrors errs
@
-}
module System.Agents.Tools.Validation (
    -- * Types
    ValidationError (..),
    ValidationConfig (..),

    -- * Validation functions
    validateToolInput,
    validateParamProperty,
    validateParamTypeWithPath,

    -- * Error formatting
    formatValidationErrors,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

{- | A single validation error with context about what went wrong.

The path indicates where in the nested structure the error occurred,
using dot notation (e.g., "filters.name" for nested properties).
-}
data ValidationError = ValidationError
    { errorPath :: Text
    -- ^ JSON path to the problematic field (e.g., "user.name" or "items.0.id")
    , errorMessage :: Text
    -- ^ Human-readable error message describing the issue
    }
    deriving (Show, Eq)

{- | Configuration options for validation behavior.

Can be extended to control strictness levels or allow additional properties.
-}
data ValidationConfig = ValidationConfig
    { allowExtraProperties :: Bool
    -- ^ If True, allow properties not defined in the schema (default: False)
    , strictMode :: Bool
    -- ^ If True, treat warnings as errors (default: False)
    }
    deriving (Show, Eq)

-- | Default validation configuration
defaultConfig :: ValidationConfig
defaultConfig =
    ValidationConfig
        { allowExtraProperties = False
        , strictMode = False
        }

-------------------------------------------------------------------------------
-- Main validation function
-------------------------------------------------------------------------------

{- | Validate a tool input against its schema.

Takes a list of parameter properties (the tool schema) and a JSON value
(the tool input), and returns a list of validation errors.

The input should be a JSON object at the top level, with keys matching
the parameter property keys defined in the schema.

Example:

@
let schema =
        [ ParamProperty "name" StringParamType "User name" True
        , ParamProperty "age" NumberParamType "User age" False
        ]
    input = Aeson.object ["name" .= "John", "age" .= 30]
in validateToolInput schema input  -- returns []
@
-}
validateToolInput ::
    -- | Schema as list of parameter properties
    [ParamProperty] ->
    -- | Input value to validate
    Aeson.Value ->
    -- | List of validation errors (empty if valid)
    [ValidationError]
validateToolInput schema input =
    validateToolInputWithConfig defaultConfig schema input

-- | Validate with custom configuration.
validateToolInputWithConfig ::
    ValidationConfig ->
    [ParamProperty] ->
    Aeson.Value ->
    [ValidationError]
validateToolInputWithConfig config schema = \case
    Aeson.Object obj ->
        let
            -- Check for extra properties if not allowed
            extraPropErrors =
                if allowExtraProperties config
                    then []
                    else checkExtraProperties schema obj
            -- Validate each property in the schema
            propertyErrors = concatMap (validatePropertyField obj) schema
         in
            extraPropErrors ++ propertyErrors
    Aeson.Null ->
        -- Null is only valid if all properties are optional
        concatMap checkRequiredProperty schema
    _ ->
        [ValidationError "" "Tool input must be a JSON object"]
  where
    checkExtraProperties :: [ParamProperty] -> Aeson.Object -> [ValidationError]
    checkExtraProperties props obj =
        let schemaKeys = map (AesonKey.fromText . propertyKey) props
            inputKeys = KeyMap.keys obj
            extraKeys = filter (\k -> k `notElem` schemaKeys) inputKeys
         in map (\k -> ValidationError (AesonKey.toText k) "Unknown property (not in schema)") extraKeys

    checkRequiredProperty :: ParamProperty -> [ValidationError]
    checkRequiredProperty prop =
        if propertyRequired prop
            then [ValidationError (propertyKey prop) "Required property missing (input was null or missing)"]
            else []

    validatePropertyField :: Aeson.Object -> ParamProperty -> [ValidationError]
    validatePropertyField obj prop =
        case KeyMap.lookup (AesonKey.fromText $ propertyKey prop) obj of
            Nothing ->
                if propertyRequired prop
                    then [ValidationError (propertyKey prop) "Required property missing"]
                    else []
            Just value ->
                validateParamTypeWithPath (propertyKey prop) prop.propertyType value

-------------------------------------------------------------------------------
-- Type-specific validation
-------------------------------------------------------------------------------

{- | Validate a single parameter property against a JSON value.

Returns validation errors found at this level only.
-}
validateParamProperty :: ParamProperty -> Aeson.Value -> [ValidationError]
validateParamProperty prop value =
    validateParamTypeWithPath (propertyKey prop) (propertyType prop) value

{- | Validate a JSON value against a ParamType at a given path.

This is the core type validation logic that checks if a value matches
the expected type.
-}
validateParamTypeWithPath ::
    -- | Current path (for error reporting)
    Text ->
    -- | Expected type
    ParamType ->
    -- | Value to validate
    Aeson.Value ->
    -- | Validation errors
    [ValidationError]
validateParamTypeWithPath path paramType value = case (paramType, value) of
    (NullParamType, Aeson.Null) ->
        []
    (NullParamType, _) ->
        [ValidationError path $ "Expected null but got " <> valueTypeName value]
    (StringParamType, Aeson.String _) ->
        []
    (StringParamType, _) ->
        [ValidationError path $ "Expected string but got " <> valueTypeName value]
    (BoolParamType, Aeson.Bool _) ->
        []
    (BoolParamType, _) ->
        [ValidationError path $ "Expected boolean but got " <> valueTypeName value]
    (NumberParamType, Aeson.Number _) ->
        -- Note: Aeson.Number includes both integers and floats
        []
    (NumberParamType, Aeson.String s) ->
        -- Try to parse as number for lenient validation
        case TextRead.double s of
            Right _ -> []
            Left _ -> [ValidationError path $ "Expected number but got string that cannot be parsed as number: " <> s]
    (NumberParamType, _) ->
        [ValidationError path $ "Expected number but got " <> valueTypeName value]
    (EnumParamType allowedValues, Aeson.String s) ->
        if s `elem` allowedValues
            then []
            else
                [ ValidationError path $
                    "Invalid enum value: "
                        <> s
                        <> ". Allowed values: "
                        <> Text.intercalate ", " allowedValues
                ]
    (EnumParamType _, _) ->
        [ValidationError path $ "Expected string (enum) but got " <> valueTypeName value]
    (OpaqueParamType _, _) ->
        -- Opaque types accept any value (used for flexible/unknown types)
        []
    (MultipleParamType _, _) ->
        -- Multiple types accept any value matching one of the types
        -- For now, we accept any value (could be made stricter)
        []
    (ObjectParamType nestedProps, Aeson.Object obj) ->
        let
            -- Check required nested properties
            requiredProps = filter propertyRequired nestedProps
            missingErrors =
                [ ValidationError (path <> "." <> propertyKey prop) "Required property missing"
                | prop <- requiredProps
                , isJust (KeyMap.lookup (AesonKey.fromText $ propertyKey prop) obj)
                ]
            -- Validate present properties
            presentErrors = concatMap (validateNestedProperty obj) nestedProps
         in
            missingErrors ++ presentErrors
    (ObjectParamType _, _) ->
        [ValidationError path $ "Expected object but got " <> valueTypeName value]
  where
    validateNestedProperty :: Aeson.Object -> ParamProperty -> [ValidationError]
    validateNestedProperty obj nestedProp =
        case KeyMap.lookup (AesonKey.fromText $ propertyKey nestedProp) obj of
            Nothing -> []
            Just nestedValue ->
                validateParamTypeWithPath
                    (path <> "." <> propertyKey nestedProp)
                    (propertyType nestedProp)
                    nestedValue

-- | Get a human-readable type name for a JSON value.
valueTypeName :: Aeson.Value -> Text
valueTypeName Aeson.Null = "null"
valueTypeName (Aeson.String _) = "string"
valueTypeName (Aeson.Number _) = "number"
valueTypeName (Aeson.Bool _) = "boolean"
valueTypeName (Aeson.Array _) = "array"
valueTypeName (Aeson.Object _) = "object"

-------------------------------------------------------------------------------
-- Error formatting
-------------------------------------------------------------------------------

{- | Format validation errors into a helpful message for LLM correction.

This generates a clear, actionable error message that can be returned
to an LLM to help it correct its tool call.

Example output:

@
Tool call validation failed with 2 errors:

1. filters.status: Invalid enum value: pending. Allowed values: active, inactive, archived

2. user.age: Expected number but got string

Please correct these issues and try again.
@
-}
formatValidationErrors ::
    -- | Tool name (for context)
    Text ->
    -- | List of errors
    [ValidationError] ->
    -- | Formatted error message
    Text
formatValidationErrors toolName errors =
    if null errors
        then ""
        else
            Text.unlines $
                headerLine
                    : ""
                    : numberedErrors
                    ++ ["", "Please correct these issues and try again."]
  where
    errorCount = length errors
    headerLine =
        "Tool call validation failed for '"
            <> toolName
            <> "' with "
            <> Text.pack (show errorCount)
            <> " error"
            <> (if errorCount == 1 then "" else "s")
            <> ":"

    numberedErrors =
        [ Text.pack (show i) <> ". " <> formatSingleError err
        | (i, err) <- zip [(1 :: Int) ..] errors
        ]

    formatSingleError :: ValidationError -> Text
    formatSingleError err =
        if Text.null err.errorPath
            then err.errorMessage
            else err.errorPath <> ": " <> err.errorMessage

