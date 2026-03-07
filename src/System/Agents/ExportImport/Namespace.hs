{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.ExportImport.Namespace where

import Data.List (inits, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (joinPath, splitDirectories, (</>))
import qualified System.Directory

import System.Agents.ExportImport.Types (Namespace(..), parseNamespace)

-------------------------------------------------------------------------------
-- Namespace Operations
-------------------------------------------------------------------------------

-- | Create a namespace from a list of parts
mkNamespace :: [Text] -> Namespace
mkNamespace = Namespace

-- | Create a namespace from a single text (dot-separated)
mkNamespaceFromText :: Text -> Either String Namespace
mkNamespaceFromText = parseNamespace

-- | Create a namespace from a file path
mkNamespaceFromPath :: FilePath -> Namespace
mkNamespaceFromPath path =
    let parts = map Text.pack $ splitDirectories path
    in Namespace $ filter (not . Text.null) parts

-- | Get all parent namespaces including the namespace itself
namespaceHierarchy :: Namespace -> [Namespace]
namespaceHierarchy (Namespace parts) =
    map Namespace $ drop 1 $ inits parts

-- | Check if one namespace is a parent of (or equal to) another
namespaceContains :: Namespace -> Namespace -> Bool
namespaceContains (Namespace parent) (Namespace child) =
    parent `isPrefixOf` child

-- | Get the relative path from a parent namespace to a child
namespaceRelative :: Namespace -> Namespace -> Maybe [Text]
namespaceRelative (Namespace parent) (Namespace child) =
    if parent `isPrefixOf` child
        then Just $ drop (length parent) child
        else Nothing

-- | Flatten a namespace to a safe filename (using underscores)
namespaceToFileName :: Namespace -> FilePath
namespaceToFileName (Namespace parts) =
    Text.unpack $ Text.intercalate "_" parts

-- | Build a hierarchical directory structure for a namespace
createNamespaceDirs :: FilePath -> Namespace -> IO FilePath
createNamespaceDirs base (Namespace parts) =
    let fullPath = base </> joinPath (map Text.unpack parts)
    in System.Directory.createDirectoryIfMissing True fullPath >> pure fullPath

-- | Find all namespaces in a directory tree
findNamespaces :: FilePath -> IO [Namespace]
findNamespaces baseDir = do
    exists <- System.Directory.doesDirectoryExist baseDir
    if not exists
        then pure []
        else findNsRecursive baseDir []
  where
    findNsRecursive :: FilePath -> [Text] -> IO [Namespace]
    findNsRecursive dir parts = do
        entries <- System.Directory.listDirectory dir
        fmap concat $ mapM (processEntry dir parts) entries
    
    processEntry :: FilePath -> [Text] -> FilePath -> IO [Namespace]
    processEntry dir parts entry = do
        let fullPath = dir </> entry
        isDir <- System.Directory.doesDirectoryExist fullPath
        if isDir
            then do
                let newParts = parts ++ [Text.pack entry]
                subNs <- findNsRecursive fullPath newParts
                pure $ Namespace newParts : subNs
            else pure []

-- | Validate a namespace (ensure no empty parts and valid characters)
validateNamespace :: Namespace -> Either String ()
validateNamespace (Namespace parts) =
    if all isValidPart parts
        then Right ()
        else Left "Namespace contains invalid characters"
  where
    isValidPart :: Text -> Bool
    isValidPart t = not (Text.null t) && Text.all isValidChar t
    
    isValidChar :: Char -> Bool
    isValidChar c = 
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') ||
        c == '-' || c == '_' || c == '.'

-- | Sanitize a text to be a valid namespace part
sanitizeNamespacePart :: Text -> Text
sanitizeNamespacePart = Text.map sanitizeChar . Text.filter (not . isInvalidChar)
  where
    sanitizeChar c
        | c >= 'a' && c <= 'z' = c
        | c >= 'A' && c <= 'Z' = c
        | c >= '0' && c <= '9' = c
        | c == '-' || c == '_' || c == '.' = c
        | otherwise = '_'
    
    isInvalidChar c = c == '/' || c == '\\' || c == '\0'

-- | Parse namespace with default on failure
parseNamespaceOrDefault :: Text -> Namespace -> Namespace
parseNamespaceOrDefault t def =
    case parseNamespace t of
        Left _ -> def
        Right ns -> ns

-------------------------------------------------------------------------------
-- Import/Export Helpers
-------------------------------------------------------------------------------

-- | Apply a namespace prefix to an export
data NamespacePolicy
    = PreserveNamespace      -- ^ Keep original namespaces
    | OverrideNamespace Namespace  -- ^ Replace with new namespace
    | PrefixNamespace Namespace    -- ^ Prepend to existing namespaces
    deriving (Show, Eq)

-- | Apply namespace policy to an optional namespace
applyNamespacePolicy :: NamespacePolicy -> Maybe Text -> Maybe Text
applyNamespacePolicy policy mbNs =
    case policy of
        PreserveNamespace -> mbNs
        OverrideNamespace (Namespace newParts) -> 
            Just $ Text.intercalate "." newParts
        PrefixNamespace (Namespace prefixParts) ->
            case mbNs of
                Nothing -> Just $ Text.intercalate "." prefixParts
                Just existing -> 
                    Just $ Text.intercalate "." (prefixParts ++ Text.split (== '.') existing)

