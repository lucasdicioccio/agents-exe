{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- |
-- Module      : System.Agents.ExportImport.Namespace
-- Description : Namespace handling for organizing agents and tools
-- Copyright   : (c) 2024 Lucas DiCioccio
-- License     : Apache-2.0
--
-- This module provides support for hierarchical namespaces, allowing
-- multiple agents and tools to be organized within a single git repository
-- or export package.
--
-- Namespaces support two notation styles:
--
-- 1. __Dot notation__: @"team1.project-a.agent-name"@
-- 2. __Path notation__: @"team1/project-a/agent-name"@
--
-- Both notations are equivalent and represent a hierarchical structure.
--
-- == Usage Examples
--
-- >>> parseNamespace "team1.project-a"
-- Right (Namespace ["team1","project-a"])
--
-- >>> parseNamespace "team1/project-a"
-- Right (Namespace ["team1","project-a"])
--
-- >>> namespaceToPath (Namespace ["team1", "project-a"])
-- "team1/project-a"
--
-- == Namespace Validation
--
-- Namespace components must:
--
-- * Be non-empty
-- * Not contain '.' or '/' characters (these are reserved as separators)
-- * Not be empty strings
--
-- Invalid examples:
--
-- * @""@ - empty namespace
-- * @"team..project"@ - empty component
-- * @"team./project"@ - invalid character combination
module System.Agents.ExportImport.Namespace (
    -- * Namespace Type
    Namespace (..),

    -- * Parsing
    parseNamespace,
    parseNamespaceLenient,

    -- * Conversion
    namespaceToPath,
    namespaceToGitPath,
    namespaceToDotNotation,

    -- * Manipulation
    namespaceAppend,
    namespaceParent,
    namespaceBaseName,

    -- * Validation
    isValidNamespaceComponent,
    namespaceDepth,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.FilePath (joinPath)

-------------------------------------------------------------------------------
-- Namespace Type
-------------------------------------------------------------------------------

-- | A hierarchical namespace represented as a list of path components.
--
-- The namespace @"team1/project-a/agent-name"@ is represented as:
--
-- > Namespace ["team1", "project-a", "agent-name"]
--
-- Use 'parseNamespace' to create a namespace from text, and conversion
-- functions like 'namespaceToPath' to convert back to various formats.
newtype Namespace = Namespace
    { unNamespace :: [Text]
    -- ^ The path components of the namespace, from root to leaf.
    }
    deriving (Show, Eq, Ord, Generic)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- | Parse a namespace from text using either dot-notation or path-notation.
--
-- Both @"a.b.c"@ and @"a/b/c"@ parse to @Namespace ["a","b","c"]@.
--
-- Returns 'Left' with an error message if the input is invalid.
--
-- === Examples
--
-- >>> parseNamespace "team1.project-a"
-- Right (Namespace ["team1","project-a"])
--
-- >>> parseNamespace "team1/project-a"
-- Right (Namespace ["team1","project-a"])
--
-- >>> parseNamespace ""
-- Left "Namespace cannot be empty"
--
-- >>> parseNamespace "team..project"
-- Left "Empty namespace component at position 1"
parseNamespace :: Text -> Either String Namespace
parseNamespace txt
    | Text.null txt = Left "Namespace cannot be empty"
    | Text.any (== '.') txt && Text.any (== '/') txt =
        Left "Namespace cannot mix '.' and '/' separators"
    | otherwise =
        let parts = splitNamespace txt
         in case validateParts parts of
                Nothing -> Right (Namespace parts)
                Just err -> Left err
  where
    splitNamespace :: Text -> [Text]
    splitNamespace t
        | Text.any (== '.') t = Text.split (== '.') t
        | Text.any (== '/') t = Text.split (== '/') t
        | otherwise = [t]

    validateParts :: [Text] -> Maybe String
    validateParts parts =
        case filter (not . isValidNamespaceComponent . fst) (zip parts [0 :: Int ..]) of
            ((_, idx) : _) -> Just $ "Invalid namespace component at position " ++ show idx
            [] ->
                if any Text.null parts
                    then Just $ case filter (Text.null . fst) (zip parts [0 :: Int ..]) of
                        ((_, idx) : _) -> "Empty namespace component at position " ++ show idx
                        [] -> "Empty namespace component"
                    else Nothing

-- | Parse a namespace leniently, accepting mixed separators.
--
-- This function treats both '.' and '/' as separators and normalizes them.
-- Use with caution as it may produce unexpected results.
--
-- === Example
--
-- >>> parseNamespaceLenient "team1.project-a/agent-name"
-- Right (Namespace ["team1","project-a","agent-name"])
parseNamespaceLenient :: Text -> Either String Namespace
parseNamespaceLenient txt
    | Text.null txt = Left "Namespace cannot be empty"
    | otherwise =
        -- Normalize both separators to a single character, then split
        let normalized = Text.replace "/" "." txt
            parts = Text.split (== '.') normalized
         in case validateParts parts of
                Nothing -> Right (Namespace (filter (not . Text.null) parts))
                Just err -> Left err
  where
    validateParts :: [Text] -> Maybe String
    validateParts parts =
        case filter (not . isValidNamespaceComponent . fst) (zip parts [0 :: Int ..]) of
            ((_, idx) : _) -> Just $ "Invalid namespace component at position " ++ show idx
            [] -> Nothing

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

-- | Convert a namespace to a filesystem path using the system's path separator.
--
-- === Examples
--
-- >>> namespaceToPath (Namespace ["team1", "project-a", "agent-name"])
-- "team1/project-a/agent-name"
--
-- On Windows: @"team1\\\\project-a\\\\agent-name"@
namespaceToPath :: Namespace -> FilePath
namespaceToPath (Namespace parts) =
    joinPath (map Text.unpack parts)

-- | Convert a namespace to a git-style path (always using forward slashes).
--
-- This is useful for generating URLs or consistent cross-platform paths.
--
-- === Example
--
-- >>> namespaceToGitPath (Namespace ["team1", "project-a", "agent-name"])
-- "team1/project-a/agent-name"
namespaceToGitPath :: Namespace -> FilePath
namespaceToGitPath (Namespace parts) =
    Text.unpack $ Text.intercalate "/" parts

-- | Convert a namespace to dot notation.
--
-- === Example
--
-- >>> namespaceToDotNotation (Namespace ["team1", "project-a", "agent-name"])
-- "team1.project-a.agent-name"
namespaceToDotNotation :: Namespace -> Text
namespaceToDotNotation (Namespace parts) =
    Text.intercalate "." parts

-------------------------------------------------------------------------------
-- Manipulation
-------------------------------------------------------------------------------

-- | Append a component to a namespace.
--
-- === Example
--
-- >>> namespaceAppend (Namespace ["team1"]) "project-a"
-- Namespace ["team1","project-a"]
namespaceAppend :: Namespace -> Text -> Namespace
namespaceAppend (Namespace parts) component =
    Namespace (parts ++ [component])

-- | Get the parent namespace, if any.
--
-- Returns 'Nothing' if the namespace has only one component.
--
-- === Examples
--
-- >>> namespaceParent (Namespace ["team1", "project-a", "agent-name"])
-- Just (Namespace ["team1","project-a"])
--
-- >>> namespaceParent (Namespace ["agent-name"])
-- Nothing
namespaceParent :: Namespace -> Maybe Namespace
namespaceParent (Namespace parts) =
    case parts of
        [] -> Nothing
        [_] -> Nothing
        xs -> Just (Namespace (init xs))

-- | Get the base name (last component) of a namespace.
--
-- === Examples
--
-- >>> namespaceBaseName (Namespace ["team1", "project-a", "agent-name"])
-- Just "agent-name"
--
-- >>> namespaceBaseName (Namespace [])
-- Nothing
namespaceBaseName :: Namespace -> Maybe Text
namespaceBaseName (Namespace parts) =
    case parts of
        [] -> Nothing
        xs -> Just (last xs)

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | Check if a text value is a valid namespace component.
--
-- Valid components must:
--
-- * Be non-empty
-- * Not contain '.' or '/' characters
-- * Not contain null characters
--
-- === Examples
--
-- >>> isValidNamespaceComponent "valid-name"
-- True
--
-- >>> isValidNamespaceComponent ""
-- False
--
-- >>> isValidNamespaceComponent "invalid.name"
-- False
isValidNamespaceComponent :: Text -> Bool
isValidNamespaceComponent txt =
    not (Text.null txt)
        && not (Text.any (\c -> c == '.' || c == '/' || c == '\0') txt)

-- | Get the depth (number of components) of a namespace.
--
-- === Examples
--
-- >>> namespaceDepth (Namespace ["a", "b", "c"])
-- 3
--
-- >>> namespaceDepth (Namespace [])
-- 0
namespaceDepth :: Namespace -> Int
namespaceDepth (Namespace parts) = length parts

