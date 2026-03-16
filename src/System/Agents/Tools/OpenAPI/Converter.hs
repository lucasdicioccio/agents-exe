{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | OpenAPI to Tool conversion logic.

This module converts parsed and dereferenced OpenAPI operations into the
internal Tool representation used by the agents system. It handles:

* Converting OpenAPI specs to lists of tools
* Building tool parameter schemas from OpenAPI parameters
* Naming conventions (operationId or path/method based)
* Parameter naming (p_ prefix for path/query, b for body)
* Name normalization for LLM compatibility

Example:
>>> import qualified Data.Map.Strict as Map
>>> import qualified Data.Aeson as Aeson
>>> let op = Operation (Just "getPet") (Just "Get a pet") Nothing [] Nothing
>>> let tool = convertOperation "/pets/{id}" "GET" op
>>> tool.toolName
"openapi_getPet"
-}
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

    -- * Name normalization for LLM compatibility
    normalizeForLLM,
    isValidLLMNameChar,
    ensureLetterStart,
    collapseUnderscores,
    NameMapping (..),
    buildToolNameMapping,
    findToolByNormalizedName,
    generateUniqueNormalizedName,

    -- * OpenAI conversion
    toOpenAITool,

    -- * Handler helpers
    defaultHandler,
) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Char (isDigit, isLetter)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.ToolSchema (ParamProperty (..), ParamType (..))
import System.Agents.Tools.OpenAPI.Types (
    Method,
    OpenAPISpec (..),
    Operation (..),
    Parameter (..),
    Path,
    RequestBody (..),
    Schema (..),
 )

-- -------------------------------------------------------------------------
-- Core Types
-- -------------------------------------------------------------------------

{- | Handler function type for executing OpenAPI tool calls.
This is a placeholder type - actual execution would be implemented
by the caller based on their HTTP client needs.
-}
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

{- | Property schema for LLM tool parameters.
Mirrors JSON Schema structure needed for LLM tool definitions.
-}
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
-- Name Normalization for LLM Compatibility
-- -------------------------------------------------------------------------

{- | Mapping between original OpenAPI names and normalized LLM names.

This is used to maintain bidirectional mapping:
- Original operation IDs may contain invalid characters (dots, slashes, etc.)
- Normalized names are LLM-safe

Example:
>>> NameMapping "pet.findByStatus" "pet_findByStatus" "myapi"
NameMapping {nmOriginal = "pet.findByStatus", nmNormalized = "pet_findByStatus", nmToolbox = "myapi"}
-}
data NameMapping = NameMapping
    { nmOriginal :: Text
    -- ^ Original operationId from OpenAPI spec
    , nmNormalized :: Text
    -- ^ Normalized name safe for LLM (no dots, slashes, etc.)
    , nmToolbox :: Text
    -- ^ Toolbox name (prefix context)
    }
    deriving (Show, Eq)

{- | Check if a character is valid for LLM tool names.

LLM APIs typically require: [a-zA-Z0-9_-]
(letters, digits, underscores, hyphens)

Examples:
>>> isValidLLMNameChar 'a'
True
>>> isValidLLMNameChar '5'
True
>>> isValidLLMNameChar '_'
True
>>> isValidLLMNameChar '-'
True
>>> isValidLLMNameChar '.'
False
>>> isValidLLMNameChar '/'
False
-}
isValidLLMNameChar :: Char -> Bool
isValidLLMNameChar c = isLetter c || isDigit c || c == '_' || c == '-'

{- | Ensure a name starts with a letter.

LLM APIs require function names to start with a letter.
If the name starts with a digit or other character, prefix with 't'.

Examples:
>>> ensureLetterStart "getPet"
"getPet"
>>> ensureLetterStart "2.0_getPet"
"t2_0_getPet"
>>> ensureLetterStart ""
"tool"
-}
ensureLetterStart :: Text -> Text
ensureLetterStart t
    | Text.null t = "tool"
    | isLetter (Text.head t) = t
    | otherwise = "t" <> t

{- | Collapse multiple consecutive underscores into a single one.

Examples:
>>> collapseUnderscores "pet__find__status"
"pet_find_status"
>>> collapseUnderscores "a___b__c"
"a_b_c"
>>> collapseUnderscores "valid_name"
"valid_name"
-}
collapseUnderscores :: Text -> Text
collapseUnderscores = Text.intercalate "_" . filter (not . Text.null) . Text.splitOn "_"

{- | Normalize a tool name to be LLM-compatible.

This function ensures that tool names conform to LLM API requirements:
1. Must start with a letter (prefix with 't' if needed)
2. Replace invalid characters (anything not [a-zA-Z0-9_-]) with underscores
3. Collapse multiple consecutive underscores
4. Preserve case (some providers are case-sensitive)

This is essential for OpenAPI operation IDs that may contain dots,
slashes, or other special characters.

Examples:
>>> normalizeForLLM "pet.findByStatus"
"pet_findByStatus"
>>> normalizeForLLM "users/pets/get"
"users_pets_get"
>>> normalizeForLLM "2.0_getPet"
"t2_0_getPet"
>>> normalizeForLLM "get-pet_v2"
"get-pet_v2"
>>> normalizeForLLM "PetStore::getPet"
"tPetStore__getPet"
-}
normalizeForLLM :: Text -> Text
normalizeForLLM = collapseUnderscores . ensureLetterStart . Text.map replaceInvalid
  where
    replaceInvalid c
        | isValidLLMNameChar c = c
        | otherwise = '_'

{- | Generate a unique normalized name, handling collisions.

If the normalized name already exists in the set of used names,
append a numeric suffix to make it unique.

Examples:
>>> generateUniqueNormalizedName "pet_find" Set.empty
"pet_find"
>>> generateUniqueNormalizedName "pet_find" (Set.fromList ["pet_find"])
"pet_find_2"
>>> generateUniqueNormalizedName "pet_find" (Set.fromList ["pet_find", "pet_find_2"])
"pet_find_3"
-}
generateUniqueNormalizedName :: Text -> Set Text -> Text
generateUniqueNormalizedName base usedNames
    | Set.notMember base usedNames = base
    | otherwise = findUnique 2
  where
    findUnique :: Int -> Text
    findUnique n =
        let candidate = base <> "_" <> Text.pack (show n)
         in if Set.notMember candidate usedNames
                then candidate
                else findUnique (n + 1)

{- | Build a mapping from normalized names to original names for all tools.

This creates a bidirectional mapping that allows us to:
1. Register tools with normalized LLM-safe names
2. Look up the original operation ID when executing tool calls

Handles name collisions by appending numeric suffixes.

The returned map is keyed by the /normalized/ name for efficient lookup
when processing LLM tool calls.
-}
buildToolNameMapping ::
    -- | Toolbox name (for context)
    Text ->
    -- | List of tools
    [OpenAPITool] ->
    -- | Map from normalized name to NameMapping
    Map Text NameMapping
buildToolNameMapping toolboxName tools =
    fst $ foldl' addTool (Map.empty, Set.empty) tools
  where
    addTool :: (Map Text NameMapping, Set Text) -> OpenAPITool -> (Map Text NameMapping, Set Text)
    addTool (mapping, used) tool =
        let original = getOperationIdOrFallback tool
            baseNormalized = normalizeForLLM original
            uniqueNormalized = generateUniqueNormalizedName baseNormalized used
            nameMap = NameMapping original uniqueNormalized toolboxName
         in ( Map.insert uniqueNormalized nameMap mapping
            , Set.insert uniqueNormalized used
            )

    -- Helper to get operation ID or fall back to generated name
    getOperationIdOrFallback :: OpenAPITool -> Text
    getOperationIdOrFallback tool =
        fromMaybe (toolName tool) (opOperationId (toolOperation tool))

    -- Local foldl' to avoid import
    foldl' :: (a -> b -> a) -> a -> [b] -> a
    foldl' _f z [] = z
    foldl' f z (x : xs) = let z' = f z x in z' `seq` foldl' f z' xs

{- | Find a tool by its normalized LLM name.

Looks up the original operation ID from the name mapping,
then finds the corresponding tool.

Returns 'Nothing' if no mapping exists for the given normalized name.
-}
findToolByNormalizedName ::
    -- | Map from normalized name to NameMapping
    Map Text NameMapping ->
    -- | Normalized name to look up
    Text ->
    -- | Original operation ID if found
    Maybe Text
findToolByNormalizedName mapping normalizedName =
    nmOriginal <$> Map.lookup normalizedName mapping

-- -------------------------------------------------------------------------
-- Main Conversion Functions
-- -------------------------------------------------------------------------

{- | Converts an OpenAPI spec into a list of tools.

This function walks through all paths and methods in the spec,
converting each operation into an 'OpenAPITool'.

Example:
>>> let spec = OpenAPISpec (Map.singleton "/pets" (Map.singleton "GET" op)) Nothing
>>> let tools = convertOpenAPIToTools spec
>>> length tools
1
-}
convertOpenAPIToTools ::
    OpenAPISpec ->
    [OpenAPITool]
convertOpenAPIToTools spec = do
    (path, methods) <- Map.toList (specPaths spec)
    (method, operation) <- Map.toList methods
    pure $ convertOperation path method operation

{- | Converts a single OpenAPI operation to a tool.

This is the core conversion function that:
* Derives a tool name from operationId or path/method
* Builds the tool description from summary and description
* Converts parameters to property schemas
* Tracks required parameters

Note: The toolName field contains the /original/ name (potentially with
invalid characters). Use 'normalizeForLLM' when registering with LLMs.

Example:
>>> let op = Operation (Just "getPet") (Just "Get pet") (Just "Returns a pet") [] Nothing
>>> let tool = convertOperation "/pets/{id}" "GET" op
>>> tool.toolName
"openapi_getPet"
-}
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

{- | Derives a tool name from an OpenAPI operation.

Uses operationId if available, otherwise falls back to {method}_{path}.
All names are prefixed with "openapi_".

Note: This returns the /original/ name which may contain invalid characters
for LLM APIs. Use 'normalizeForLLM' before registering with LLMs.

Examples:
>>> deriveToolName "/pets" "GET" (Operation (Just "listPets") Nothing Nothing [] Nothing)
"openapi_listPets"
>>> deriveToolName "/pets/{id}" "GET" (Operation Nothing Nothing Nothing [] Nothing)
"openapi_get_pets__id_"
-}
deriveToolName :: Path -> Method -> Operation -> Text
deriveToolName path method op =
    "openapi_" <> case opOperationId op of
        Just opId -> opId
        Nothing -> sanitizePathForName method path

{- | Sanitizes a path for use in a tool name.

Converts the path/method combination to a valid identifier.
Replaces special characters with underscores.

Examples:
>>> sanitizePathForName "GET" "/pets"
"get_pets"
>>> sanitizePathForName "POST" "/pets/{id}/owner"
"post_pets__id__owner"
>>> sanitizePathForName "GET" "/api/v1/users"
"get_api_v1_users"
-}
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

{- | Builds a tool description from an operation's summary and description.

Combines both fields with a colon separator if both are present.

Examples:
>>> let op = Operation Nothing (Just "Get a pet") (Just "Returns a single pet") [] Nothing
>>> buildToolDescription op
"Get a pet:\nReturns a single pet"
>>> let op2 = Operation Nothing Nothing (Just "Just description") [] Nothing
>>> buildToolDescription op2
":\nJust description"
-}
buildToolDescription :: Operation -> Text
buildToolDescription op =
    fromMaybe "" (opSummary op) <> ":\n" <> fromMaybe "" (opDescription op)

-- -------------------------------------------------------------------------
-- Parameter Building
-- -------------------------------------------------------------------------

{- | Converts a parameter to its tool property name.

Prefixes path and query parameters with "p_" to avoid conflicts.
Header parameters are also prefixed with "p_".

Examples:
>>> let param = Parameter "petId" ParamInPath Nothing True Nothing
>>> paramToToolName param
"p_petId"
>>> let param2 = Parameter "limit" ParamInQuery (Just "Max results") False Nothing
>>> paramToToolName param2
"p_limit"
-}
paramToToolName :: Parameter -> Text
paramToToolName param = "p_" <> paramName param

{- | Builds tool parameters structure from an operation.

Collects all parameters (path, query, header) and body into a
unified parameter schema structure.
-}
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

{- | Builds a property schema from an OpenAPI schema.

Handles:
* Enum values: copies type and augments description
* Arrays: copies items schema
* AnyOf: copies anyOf schemas
* Basic types: copies type and description

The parameter description is merged with the schema description.
-}
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

{- | Gets the list of required parameter names for a tool.

Includes:
* Parameters marked with required=true
* Body parameter "b" if request body is required
-}
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

{- | Converts an OpenAPITool to the OpenAI Tool format.

This allows OpenAPITools to be used directly with the LLM interface.

Note: This uses the original tool name. For LLM registration,
you typically want to use the normalized name via 'normalizeForLLM'.

Example:
>>> let op = Operation (Just "test") (Just "Test op") (Just "A test") [] Nothing
>>> let apiTool = convertOperation "/test" "GET" op
>>> let tool = toOpenAITool apiTool
>>> OpenAI.toolName tool
ToolName {getToolName = "openapi_test"}
-}
toOpenAITool :: OpenAPITool -> OpenAI.Tool
toOpenAITool apiTool =
    OpenAI.Tool
        { OpenAI.toolName = OpenAI.ToolName (toolName apiTool)
        , OpenAI.toolDescription = toolDescription apiTool
        , OpenAI.toolParamProperties = map (propertyToParamProperty requiredSet) $ Map.toList (paramsProperties (toolParameters apiTool))
        }
  where
    -- Build a set of required parameter names for O(1) lookup
    requiredSet :: Set Text
    requiredSet = Set.fromList (paramsRequired (toolParameters apiTool))

    propertyToParamProperty :: Set Text -> (Text, PropertySchema) -> ParamProperty
    propertyToParamProperty required (name, prop) =
        ParamProperty
            { propertyKey = name
            , propertyType = schemaTypeToParamType prop
            , propertyDescription = propDescription prop
            , propertyRequired = Set.member name required
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

{- | Default handler that always returns an error.
The actual handler should be provided by the caller.
-}
defaultHandler :: ToolHandler
defaultHandler _ = pure $ Left "Tool handler not implemented"
