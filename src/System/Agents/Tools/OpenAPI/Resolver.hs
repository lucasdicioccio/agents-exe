{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | JSON Schema reference resolution ($ref dereferencing) for OpenAPI specs.
--
-- This module provides functions to recursively resolve '$ref' pointers in
-- OpenAPI schemas. This is essential because OpenAPI specs use references
-- to avoid duplication and improve maintainability.
--
-- Example:
-- >>> import qualified Data.Map.Strict as Map
-- >>> let petSchema = Schema (Just "object") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
-- >>> let components = Components (Just (Map.fromList [("Pet", petSchema)]))
-- >>> let refSchema = Schema Nothing Nothing Nothing Nothing Nothing Nothing (Just "#/components/schemas/Pet") Nothing
-- >>> resolveSchema refSchema components
-- Schema {schemaType = Just "object", ...}
module System.Agents.Tools.OpenAPI.Resolver (
    -- * Core resolution functions
    resolveSchema,
    dereferenceSpec,
    
    -- * Reference path handling
    RefPath (..),
    parseRef,
    
    -- * Resolution errors
    ResolutionError (..),
    
    -- * Low-level resolution
    resolveRef,
    resolveSchemaWithDepth,
) where

import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Tools.OpenAPI.Types (
    Components (..),
    OpenAPISpec (..),
    Operation (..),
    Parameter (..),
    RequestBody (..),
    Schema (..),
 )

-- -------------------------------------------------------------------------
-- Reference path representation
-- -------------------------------------------------------------------------

-- | Represents a parsed JSON reference path.
--
-- A reference like @#/components/schemas/Pet@ is parsed into:
-- @
-- RefPath { refComponents = "components"
--         , refSection = "schemas"
--         , refName = "Pet"
--         }
-- @
data RefPath = RefPath
    { refComponents :: Text
    -- ^ The first path segment (usually "components")
    , refSection :: Text
    -- ^ The second path segment (e.g., "schemas", "parameters")
    , refName :: Text
    -- ^ The name of the referenced entity
    }
    deriving (Show, Eq)

-- | Parse a reference string into a 'RefPath'.
--
-- Supports internal references of the form @#/components/schemas/Name@.
-- External references (URLs) are not supported and will return 'Nothing'.
--
-- Examples:
-- >>> parseRef "#/components/schemas/Pet"
-- Just (RefPath {refComponents = "components", refSection = "schemas", refName = "Pet"})
--
-- >>> parseRef "http://example.com/schema.json"
-- Nothing
parseRef :: Text -> Maybe RefPath
parseRef ref
    -- External refs (http/https) are not supported
    | Text.isPrefixOf "http://" ref = Nothing
    | Text.isPrefixOf "https://" ref = Nothing
    -- Internal refs starting with #/
    | Text.isPrefixOf "#/" ref =
        let parts = Text.splitOn "/" (Text.drop 2 ref)
         in case parts of
                (comp : sect : name : _) ->
                    Just $ RefPath comp sect name
                _ -> Nothing
    -- Relative refs not starting with #/ are not supported
    | otherwise = Nothing

-- -------------------------------------------------------------------------
-- Resolution errors
-- -------------------------------------------------------------------------

-- | Errors that can occur during reference resolution.
data ResolutionError
    = -- | The reference path is malformed or unsupported
      InvalidRefPath Text
    | -- | The referenced schema was not found in components
      SchemaNotFound Text
    | -- | A circular reference was detected
      CircularReference [Text]
    | -- | Maximum recursion depth exceeded (safety limit)
      MaxDepthExceeded Int
    deriving (Show, Eq)

-- -------------------------------------------------------------------------
-- Core resolution functions
-- -------------------------------------------------------------------------

-- | Default maximum recursion depth to prevent infinite loops.
defaultMaxDepth :: Int
defaultMaxDepth = 100

-- | Recursively resolves all '$ref' pointers in a schema.
--
-- This function will follow references transitively until no more
-- references remain, or an error condition is encountered.
--
-- For simple resolution without error handling, use 'resolveSchema'.
resolveSchemaWithDepth ::
    -- | Schema to resolve
    Schema ->
    -- | Components containing schema definitions
    Components ->
    -- | Current resolution depth (for tracking)
    Int ->
    -- | Set of already-visited refs (for cycle detection)
    Set Text ->
    -- | Either an error or the fully-resolved schema
    Either ResolutionError Schema
resolveSchemaWithDepth schema components depth visited = do
    -- Check recursion depth
    when (depth > defaultMaxDepth) $
        Left (MaxDepthExceeded defaultMaxDepth)

    case schema.schemaRef of
        -- If this schema has a $ref, resolve it
        Just ref -> do
            -- Check for circular reference
            when (Set.member ref visited) $
                Left (CircularReference (Set.toList $ Set.insert ref visited))

            -- Parse and resolve the reference
            resolved <- resolveRef ref components
            -- Recursively resolve the referenced schema
            resolveSchemaWithDepth
                resolved
                components
                (depth + 1)
                (Set.insert ref visited)
        -- No $ref, recursively resolve nested schemas
        Nothing -> do
            resolvedProps <- resolveProperties schema.schemaProperties
            resolvedItems <- resolveItems schema.schemaItems
            resolvedAnyOf <- resolveAnyOf schema.schemaAnyOf
            pure $
                schema
                    { schemaProperties = resolvedProps
                    , schemaItems = resolvedItems
                    , schemaAnyOf = resolvedAnyOf
                    }
  where
    resolveProperties :: Maybe (Map Text Schema) -> Either ResolutionError (Maybe (Map Text Schema))
    resolveProperties Nothing = Right Nothing
    resolveProperties (Just props) =
        Just <$> traverse (\s -> resolveSchemaWithDepth s components depth visited) props

    resolveItems :: Maybe Schema -> Either ResolutionError (Maybe Schema)
    resolveItems Nothing = Right Nothing
    resolveItems (Just items) =
        Just <$> resolveSchemaWithDepth items components depth visited

    resolveAnyOf :: Maybe [Schema] -> Either ResolutionError (Maybe [Schema])
    resolveAnyOf Nothing = Right Nothing
    resolveAnyOf (Just schemas) =
        Just <$> traverse (\s -> resolveSchemaWithDepth s components depth visited) schemas

-- | Resolve a reference path to a schema from components.
--
-- Looks up the schema in the components/schemas section.
resolveRef :: Text -> Components -> Either ResolutionError Schema
resolveRef ref components = do
    refPath <- maybe (Left $ InvalidRefPath ref) Right (parseRef ref)

    schemas <-
        maybe (Left $ SchemaNotFound ref) Right (components.componentsSchemas)

    schema <-
        maybe (Left $ SchemaNotFound ref) Right (Map.lookup refPath.refName schemas)

    pure schema

-- | Recursively resolves all '$ref' pointers in a schema.
--
-- This is the main entry point for schema resolution. It handles:
--
-- * Simple references like @#/components/schemas/Pet@
-- * Nested references (schema A refs B refs C)
-- * Arrays with item references
-- * Object properties with references
-- * anyOf schemas with references
--
-- For circular references, the original $ref is returned to prevent infinite loops.
--
-- Example:
-- >>> let components = Components (Just (Map.fromList [("Pet", petSchema)]))
-- >>> resolveSchema refSchema components
-- Schema {schemaType = Just "object", ...}
resolveSchema :: Schema -> Components -> Schema
resolveSchema schema components =
    case resolveSchemaWithDepth schema components 0 Set.empty of
        Left err ->
            -- On error, return a schema with an error description
            -- but preserve the original to avoid losing information
            schema
                { schemaDescription =
                    Just $ "Reference resolution failed: " <> Text.pack (show err)
                }
        Right resolved -> resolved

-- | Resolves all schemas within an OpenAPI spec.
--
-- This function walks through the entire spec and resolves all
-- schema references in:
--
-- * Path parameters
-- * Operation parameters
-- * Request bodies
-- * Response schemas (when implemented)
--
-- The returned spec has all $ref pointers fully resolved.
dereferenceSpec :: OpenAPISpec -> OpenAPISpec
dereferenceSpec spec =
    let comps = fromMaybe emptyComponents spec.specComponents
        resolvedPaths = Map.map (Map.map (resolveOperation comps)) spec.specPaths
     in spec{specPaths = resolvedPaths}
  where
    emptyComponents :: Components
    emptyComponents = Components Nothing

    resolveOperation :: Components -> Operation -> Operation
    resolveOperation comps op =
        op
            { opParameters = map (resolveParameter comps) op.opParameters
            , opRequestBody = fmap (resolveRequestBody comps) op.opRequestBody
            }

    resolveParameter :: Components -> Parameter -> Parameter
    resolveParameter comps param =
        param{paramSchema = fmap (\s -> resolveSchema s comps) param.paramSchema}

    resolveRequestBody :: Components -> RequestBody -> RequestBody
    resolveRequestBody comps body =
        body{reqBodyContent = Map.map (\s -> resolveSchema s comps) body.reqBodyContent}

