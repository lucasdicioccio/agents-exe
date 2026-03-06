{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Core types for export/import operations.
--
-- This module defines the data types used for exporting and importing
-- agent configurations, tools, and MCP servers.
module System.Agents.ExportImport.Types where

import Data.Aeson (FromJSON(..), ToJSON(..), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.FilePath (joinPath, (</>))
import qualified Data.Text as Text

import System.Agents.Base (AgentDescription, McpServerDescription)

-------------------------------------------------------------------------------
-- Namespace
-------------------------------------------------------------------------------

-- | A namespace represents a hierarchical path for organizing agents and tools.
-- Namespaces use dots as separators (e.g., "team-a.project-x.my-agent").
newtype Namespace = Namespace { unNamespace :: [Text] }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Namespace where
    toJSON (Namespace parts) = Aeson.toJSON (Text.intercalate "." parts)

instance FromJSON Namespace where
    parseJSON = fmap (Namespace . Text.splitOn ".") . parseJSON

-- | Parse a namespace from a dot-separated string.
parseNamespace :: Text -> Namespace
parseNamespace = Namespace . Text.splitOn "."

-- | Convert a namespace to a dot-separated string.
namespaceToText :: Namespace -> Text
namespaceToText (Namespace parts) = Text.intercalate "." parts

-- | Convert a namespace to a filesystem path (for use within repo structure).
namespaceToPath :: Namespace -> FilePath
namespaceToPath (Namespace parts) = joinPath (map Text.unpack parts)

-- | Check if a namespace is a prefix of another namespace.
namespaceIsPrefixOf :: Namespace -> Namespace -> Bool
namespaceIsPrefixOf (Namespace prefix) (Namespace full) =
    length prefix <= length full && 
    and (zipWith (==) prefix (take (length prefix) full))

-- | Get the parent namespace if one exists.
namespaceParent :: Namespace -> Maybe Namespace
namespaceParent (Namespace parts) =
    case parts of
        [] -> Nothing
        [_] -> Nothing
        xs -> Just (Namespace (init xs))

-------------------------------------------------------------------------------
-- Export Package
-------------------------------------------------------------------------------

-- | A complete package containing agents, tools, and MCP servers for export/import.
data ExportPackage = ExportPackage
    { pkgAgents :: [(Namespace, AgentDescription)]
    , pkgTools :: [(Namespace, ToolExport)]
    , pkgMcpServers :: [(Text, McpServerDescription)]
    , pkgMetadata :: PackageMetadata
    }
    deriving (Show, Eq, Generic)

instance ToJSON ExportPackage
instance FromJSON ExportPackage

-- | Metadata about an export package.
data PackageMetadata = PackageMetadata
    { pkgName :: Text
    , pkgVersion :: Text
    , pkgDescription :: Maybe Text
    , pkgCreatedAt :: UTCTime
    , pkgAuthor :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON PackageMetadata
instance FromJSON PackageMetadata

-- | Tool export data including the tool description and optional file contents.
data ToolExport = ToolExport
    { toolDescription :: ToolDescriptionExport
    , toolFiles :: [(FilePath, Text)]  -- ^ Relative path and file contents
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolExport
instance FromJSON ToolExport

-- | Simplified tool description for export (subset of ScriptDescription fields).
data ToolDescriptionExport = ToolDescriptionExport
    { toolSlug :: Text
    , toolInfoText :: Text  -- ^ JSON-encoded ScriptInfo
    , toolExecutable :: Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolDescriptionExport
instance FromJSON ToolDescriptionExport

-- | An empty export package with default metadata.
emptyExportPackage :: UTCTime -> ExportPackage
emptyExportPackage now = ExportPackage
    { pkgAgents = []
    , pkgTools = []
    , pkgMcpServers = []
    , pkgMetadata = PackageMetadata
        { pkgName = "untitled"
        , pkgVersion = "1.0.0"
        , pkgDescription = Nothing
        , pkgCreatedAt = now
        , pkgAuthor = Nothing
        }
    }

-------------------------------------------------------------------------------
-- Path Resolution
-------------------------------------------------------------------------------

-- | Resolve an agent namespace to a filesystem path within a repository.
resolveAgentPath :: Namespace -> FilePath
resolveAgentPath ns = "agents" </> namespaceToPath ns <> ".json"

-- | Resolve a tool namespace to a filesystem directory within a repository.
resolveToolPath :: Namespace -> FilePath
resolveToolPath ns = "tools" </> namespaceToPath ns

-- | Resolve an MCP server name to a filesystem path.
resolveMcpServerPath :: Text -> FilePath
resolveMcpServerPath name = "mcp-servers" </> Text.unpack name <> ".json"

-- | Get the index file path within a repository.
indexFilePath :: FilePath
indexFilePath = ".agents-index.json"

