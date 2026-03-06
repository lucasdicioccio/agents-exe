{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Types for export/import functionality.
--
-- This module defines the data types used for exporting and importing
-- agent configurations, tools, and MCP server configurations.
module System.Agents.ExportImport.Types where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import System.Agents.Base (Agent, McpServerDescription)

-------------------------------------------------------------------------------
-- Version and Metadata
-------------------------------------------------------------------------------

-- | Schema version for the export format.
-- This allows future compatibility checks when importing.
schemaVersion :: Text
schemaVersion = "1.0.0"

-- | Metadata about an exported package.
data PackageMetadata = PackageMetadata
    { pmName :: Text
    , pmDescription :: Text
    , pmCreatedAt :: UTCTime
    , pmSchemaVersion :: Text
    , pmSourceNamespace :: Maybe Text
    -- ^ Optional namespace prefix for namespaced exports
    }
    deriving (Show, Eq, Generic)

instance ToJSON PackageMetadata where
    toJSON pm =
        Aeson.object
            [ "name" .= pm.pmName
            , "description" .= pm.pmDescription
            , "created_at" .= pm.pmCreatedAt
            , "schema_version" .= pm.pmSchemaVersion
            , "source_namespace" .= pm.pmSourceNamespace
            ]

instance FromJSON PackageMetadata where
    parseJSON = Aeson.withObject "PackageMetadata" $ \v ->
        PackageMetadata
            <$> v .: "name"
            <*> v .: "description"
            <*> v .: "created_at"
            <*> v .: "schema_version"
            <*> v .: "source_namespace"

-------------------------------------------------------------------------------
-- Tool Export Types
-------------------------------------------------------------------------------

-- | Metadata for an exported tool.
data ToolMetadata = ToolMetadata
    { tmSlug :: Text
    , tmDescription :: Text
    , tmOriginalPath :: FilePath
    -- ^ Original path for reference (not used during import)
    , tmFileMode :: Maybe Int
    -- ^ File mode bits to preserve executable permissions
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolMetadata where
    toJSON tm =
        Aeson.object
            [ "slug" .= tm.tmSlug
            , "description" .= tm.tmDescription
            , "original_path" .= tm.tmOriginalPath
            , "file_mode" .= tm.tmFileMode
            ]

instance FromJSON ToolMetadata where
    parseJSON = Aeson.withObject "ToolMetadata" $ \v ->
        ToolMetadata
            <$> v .: "slug"
            <*> v .: "description"
            <*> v .: "original_path"
            <*> v .: "file_mode"

-- | An exported tool with its script content and metadata.
data ExportedTool = ExportedTool
    { etMetadata :: ToolMetadata
    , etScriptContent :: Text
    -- ^ Base64-encoded or raw script content
    }
    deriving (Show, Eq, Generic)

instance ToJSON ExportedTool where
    toJSON et =
        Aeson.object
            [ "metadata" .= et.etMetadata
            , "script" .= et.etScriptContent
            ]

instance FromJSON ExportedTool where
    parseJSON = Aeson.withObject "ExportedTool" $ \v ->
        ExportedTool
            <$> v .: "metadata"
            <*> v .: "script"

-------------------------------------------------------------------------------
-- Agent Export Types
-------------------------------------------------------------------------------

-- | An exported agent with its full configuration.
data ExportedAgent = ExportedAgent
    { eaName :: Text
    -- ^ Agent name (may include namespace prefix)
    , eaConfig :: Agent
    -- ^ Full agent configuration
    }
    deriving (Show, Eq, Generic)

instance ToJSON ExportedAgent where
    toJSON ea =
        Aeson.object
            [ "name" .= ea.eaName
            , "config" .= ea.eaConfig
            ]

instance FromJSON ExportedAgent where
    parseJSON = Aeson.withObject "ExportedAgent" $ \v ->
        ExportedAgent
            <$> v .: "name"
            <*> v .: "config"

-------------------------------------------------------------------------------
-- MCP Server Export Types
-------------------------------------------------------------------------------

-- | An exported MCP server configuration.
data ExportedMcpServer = ExportedMcpServer
    { emName :: Text
    , emConfig :: McpServerDescription
    }
    deriving (Show, Eq, Generic)

instance ToJSON ExportedMcpServer where
    toJSON em =
        Aeson.object
            [ "name" .= em.emName
            , "config" .= em.emConfig
            ]

instance FromJSON ExportedMcpServer where
    parseJSON = Aeson.withObject "ExportedMcpServer" $ \v ->
        ExportedMcpServer
            <$> v .: "name"
            <*> v .: "config"

-------------------------------------------------------------------------------
-- Manifest and Package
-------------------------------------------------------------------------------

-- | Manifest containing all content listings for the archive.
data PackageManifest = PackageManifest
    { manifestMetadata :: PackageMetadata
    , manifestAgents :: [Text]
    -- ^ List of agent file names (relative to agents/)
    , manifestTools :: [Text]
    -- ^ List of tool directory names (relative to tools/)
    , manifestMcpServers :: [Text]
    -- ^ List of MCP server file names (relative to mcp-servers/)
    }
    deriving (Show, Eq, Generic)

instance ToJSON PackageManifest where
    toJSON m =
        Aeson.object
            [ "metadata" .= m.manifestMetadata
            , "agents" .= m.manifestAgents
            , "tools" .= m.manifestTools
            , "mcp_servers" .= m.manifestMcpServers
            ]

instance FromJSON PackageManifest where
    parseJSON = Aeson.withObject "PackageManifest" $ \v ->
        PackageManifest
            <$> v .: "metadata"
            <*> v .: "agents"
            <*> v .: "tools"
            <*> v .: "mcp_servers"

-- | A complete export package containing all data.
data ExportPackage = ExportPackage
    { epMetadata :: PackageMetadata
    , epAgents :: [ExportedAgent]
    , epTools :: Map Text ExportedTool
    -- ^ Map from tool name (slug) to tool data
    , epMcpServers :: [ExportedMcpServer]
    }
    deriving (Show, Eq, Generic)

-- | Create an empty export package with the given metadata.
emptyExportPackage :: PackageMetadata -> ExportPackage
emptyExportPackage metadata =
    ExportPackage
        { epMetadata = metadata
        , epAgents = []
        , epTools = Map.empty
        , epMcpServers = []
        }

-- | Add an agent to the export package.
addAgent :: ExportedAgent -> ExportPackage -> ExportPackage
addAgent agent pkg = pkg{epAgents = agent : pkg.epAgents}

-- | Add a tool to the export package.
addTool :: Text -> ExportedTool -> ExportPackage -> ExportPackage
addTool name tool pkg = pkg{epTools = Map.insert name tool pkg.epTools}

-- | Add an MCP server to the export package.
addMcpServer :: ExportedMcpServer -> ExportPackage -> ExportPackage
addMcpServer server pkg = pkg{epMcpServers = server : pkg.epMcpServers}

-- | Create a manifest from an export package.
packageToManifest :: ExportPackage -> PackageManifest
packageToManifest pkg =
    PackageManifest
        { manifestMetadata = pkg.epMetadata
        , manifestAgents = map (\a -> a.eaName <> ".json") pkg.epAgents
        , manifestTools = Map.keys pkg.epTools
        , manifestMcpServers = map (\m -> m.emName <> ".json") pkg.epMcpServers
        }

