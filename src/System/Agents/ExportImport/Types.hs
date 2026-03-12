{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.ExportImport.Types where

import Control.Exception (IOException)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Posix.Types (FileMode)

import System.Agents.Base (Agent, McpServerDescription)
import System.Agents.Tools.Bash (ScriptInfo)

-------------------------------------------------------------------------------
-- Core Export Package Types
-------------------------------------------------------------------------------

-- | Version of the export schema for compatibility checking
exportSchemaVersion :: Text
exportSchemaVersion = "1.0.0"

-- | A complete export package containing agents, tools, and MCP servers
data ExportPackage = ExportPackage
    { packageMetadata :: PackageMetadata
    , packageAgents :: [AgentExport]
    , packageTools :: [StandaloneToolExport] -- standalone tools without agents
    , packageMcpServers :: [McpServerExport]
    }
    deriving (Show, Eq, Generic)

instance ToJSON ExportPackage where
    toJSON pkg =
        Aeson.object
            [ "schema_version" .= exportSchemaVersion
            , "metadata" .= pkg.packageMetadata
            , "agents" .= pkg.packageAgents
            , "standalone_tools" .= pkg.packageTools
            , "mcp_servers" .= pkg.packageMcpServers
            ]

instance FromJSON ExportPackage where
    parseJSON = Aeson.withObject "ExportPackage" $ \v -> do
        schemaVersion <- v .: "schema_version"
        if schemaVersion /= exportSchemaVersion
            then fail $ "Unsupported schema version: " ++ Text.unpack schemaVersion
            else
                ExportPackage
                    <$> v .: "metadata"
                    <*> v .: "agents"
                    <*> v .: "standalone_tools"
                    <*> v .: "mcp_servers"

-------------------------------------------------------------------------------
-- Agent Export
-------------------------------------------------------------------------------

data AgentExport = AgentExport
    { agentConfig :: Agent
    , agentNamespace :: Maybe Text
    , agentTools :: [ToolExport]
    }
    deriving (Show, Eq, Generic)

instance ToJSON AgentExport where
    toJSON ae =
        Aeson.object $
            [ "config" .= ae.agentConfig
            , "tools" .= ae.agentTools
            ]
                ++ maybe [] (\ns -> ["namespace" .= ns]) ae.agentNamespace

instance FromJSON AgentExport where
    parseJSON = Aeson.withObject "AgentExport" $ \v ->
        AgentExport
            <$> v .: "config"
            <*> v Aeson..:? "namespace"
            <*> v .: "tools"

-------------------------------------------------------------------------------
-- Tool Export
-------------------------------------------------------------------------------

-- | Tool exported as part of an agent
data ToolExport = ToolExport
    { toolName :: Text
    , toolContent :: ByteString
    , toolPermissions :: FileMode
    , toolMetadata :: Maybe ScriptInfo -- Cached metadata from describe
    , toolNamespace :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolExport where
    toJSON te =
        Aeson.object $
            [ "name" .= te.toolName
            , "content_base64" .= Text.decodeUtf8 (B64.encode te.toolContent)
            , "permissions" .= show te.toolPermissions
            ]
                ++ maybe [] (\ns -> ["namespace" .= ns]) te.toolNamespace
                ++ maybe [] (\m -> ["metadata" .= m]) te.toolMetadata

instance FromJSON ToolExport where
    parseJSON = Aeson.withObject "ToolExport" $ \v -> do
        contentB64 <- v .: "content_base64"
        let decoded = B64.decode (Text.encodeUtf8 contentB64)
        case decoded of
            Left err -> fail $ "Invalid base64 content: " ++ show err
            Right content ->
                ToolExport
                    <$> v .: "name"
                    <*> pure content
                    <*> (read <$> v .: "permissions")
                    <*> v Aeson..:? "metadata"
                    <*> v Aeson..:? "namespace"

-- | Standalone tool export with full metadata
data StandaloneToolExport = StandaloneToolExport
    { standaloneToolInfo :: ScriptInfo
    , standaloneToolScript :: ByteString
    , standaloneToolPermissions :: FileMode
    , standaloneToolAuxFiles :: [(FilePath, ByteString)] -- Additional files like config, libs
    }
    deriving (Show, Eq, Generic)

instance ToJSON StandaloneToolExport where
    toJSON ste =
        Aeson.object
            [ "info" .= ste.standaloneToolInfo
            , "script_base64" .= Text.decodeUtf8 (B64.encode ste.standaloneToolScript)
            , "permissions" .= show ste.standaloneToolPermissions
            , "aux_files" .= map auxFileToJSON ste.standaloneToolAuxFiles
            ]
      where
        auxFileToJSON (p, c) =
            Aeson.object
                [ "path" .= p
                , "content_base64" .= Text.decodeUtf8 (B64.encode c)
                ]

instance FromJSON StandaloneToolExport where
    parseJSON = Aeson.withObject "StandaloneToolExport" $ \v -> do
        scriptB64 <- v .: "script_base64"
        let scriptDecoded = B64.decode (Text.encodeUtf8 scriptB64)
        case scriptDecoded of
            Left err -> fail $ "Invalid base64 script: " ++ show err
            Right script -> do
                auxFilesJson <- v .: "aux_files"
                auxFiles <- traverse parseAuxFile auxFilesJson
                StandaloneToolExport
                    <$> v .: "info"
                    <*> pure script
                    <*> (read <$> v .: "permissions")
                    <*> pure auxFiles
      where
        parseAuxFile = Aeson.withObject "AuxFile" $ \obj -> do
            p <- obj .: "path"
            cB64 <- obj .: "content_base64"
            let cDecoded = B64.decode (Text.encodeUtf8 cB64)
            case cDecoded of
                Left err -> fail $ "Invalid base64 aux file: " ++ show err
                Right c -> pure (p, c)

-------------------------------------------------------------------------------
-- MCP Server Export
-------------------------------------------------------------------------------

data McpServerExport = McpServerExport
    { mcpConfig :: McpServerDescription
    , mcpNamespace :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON McpServerExport where
    toJSON mse =
        Aeson.object $
            [ "config" .= mse.mcpConfig
            ]
                ++ maybe [] (\ns -> ["namespace" .= ns]) mse.mcpNamespace

instance FromJSON McpServerExport where
    parseJSON = Aeson.withObject "McpServerExport" $ \v ->
        McpServerExport
            <$> v .: "config"
            <*> v Aeson..:? "namespace"

-------------------------------------------------------------------------------
-- Package Metadata
-------------------------------------------------------------------------------

data PackageMetadata = PackageMetadata
    { packageVersion :: Text
    -- ^ Schema version (e.g., "1.0.0")
    , packageCreatedAt :: UTCTime
    , packageDescription :: Maybe Text
    , packageSource :: Maybe Text
    -- ^ Source URL or path
    }
    deriving (Show, Eq, Generic)

instance ToJSON PackageMetadata where
    toJSON pm =
        Aeson.object $
            [ "version" .= pm.packageVersion
            , "created_at" .= pm.packageCreatedAt
            ]
                ++ maybe [] (\d -> ["description" .= d]) pm.packageDescription
                ++ maybe [] (\s -> ["source" .= s]) pm.packageSource

instance FromJSON PackageMetadata where
    parseJSON = Aeson.withObject "PackageMetadata" $ \v ->
        PackageMetadata
            <$> v .: "version"
            <*> v .: "created_at"
            <*> v Aeson..:? "description"
            <*> v Aeson..:? "source"

-------------------------------------------------------------------------------
-- Tool-only Package
-------------------------------------------------------------------------------

-- | A package containing only tools (no agents)
data ToolPackage = ToolPackage
    { toolPackageMetadata :: PackageMetadata
    , toolPackageTools :: [StandaloneToolExport]
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolPackage where
    toJSON tp =
        Aeson.object
            [ "schema_version" .= (exportSchemaVersion :: Text)
            , "package_type" .= ("tools_only" :: Text)
            , "metadata" .= tp.toolPackageMetadata
            , "tools" .= tp.toolPackageTools
            ]

instance FromJSON ToolPackage where
    parseJSON = Aeson.withObject "ToolPackage" $ \v -> do
        pkgType <- v .: "package_type"
        case pkgType :: Text of
            "tools_only" ->
                ToolPackage
                    <$> v .: "metadata"
                    <*> v .: "tools"
            _ -> fail $ "Unknown package type: " ++ Text.unpack pkgType

-------------------------------------------------------------------------------
-- Namespace Types
-------------------------------------------------------------------------------

-- | Hierarchical namespace for organizing exports
newtype Namespace = Namespace {unNamespace :: [Text]}
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Namespace where
    toJSON (Namespace parts) = Aeson.toJSON $ Text.intercalate "." parts

instance FromJSON Namespace where
    parseJSON = Aeson.withText "Namespace" $ \t ->
        case parseNamespace t of
            Left err -> fail err
            Right ns -> pure ns

-- | Parse a namespace from dot-separated text
parseNamespace :: Text -> Either String Namespace
parseNamespace t
    | Text.null t = Left "Empty namespace"
    | otherwise =
        let parts = Text.split (== '.') t
         in if all (not . Text.null) parts
                then Right $ Namespace parts
                else Left "Namespace parts cannot be empty"

-- | Convert namespace to filesystem path
namespaceToPath :: Namespace -> FilePath
namespaceToPath (Namespace parts) =
    Text.unpack $ Text.intercalate "/" parts

-- | Convert namespace to git-style path (forward slashes)
namespaceToGitPath :: Namespace -> FilePath
namespaceToGitPath = namespaceToPath

-- | Join two namespaces
joinNamespace :: Namespace -> Namespace -> Namespace
joinNamespace (Namespace a) (Namespace b) = Namespace (a ++ b)

-- | Get parent namespace if any
parentNamespace :: Namespace -> Maybe Namespace
parentNamespace (Namespace []) = Nothing
parentNamespace (Namespace [_]) = Nothing
parentNamespace (Namespace parts) = Just $ Namespace (init parts)

-- | Get the last part of a namespace (the name)
namespaceName :: Namespace -> Text
namespaceName (Namespace parts) = last parts

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

data ImportError
    = ArchiveReadError String
    | ArchiveFormatError String
    | PackageParseError String
    | NamespaceConflictError Text
    | ValidationError String
    | FileIOError FilePath IOException
    deriving (Show)

data GitError
    = GitCloneError String
    | GitCheckoutError String
    | GitCommitError String
    | GitPushError String
    | GitParseError String
    | GitNamespaceError String
    | GitNotFoundError
    deriving (Show)

data InstallError
    = ToolAlreadyExists Text
    | PermissionError FilePath
    | InvalidToolDirectory FilePath
    | InstallIOError IOException
    | ValidationFailed String
    deriving (Show)

data ToolValidationError
    = DescribeFailed String
    | InvalidScriptInfo String
    | MissingDependencies [Text]
    deriving (Show)

-------------------------------------------------------------------------------
-- Archive Format Types
-------------------------------------------------------------------------------

data ArchiveFormat = TarFormat | TarGzFormat | ZipFormat
    deriving (Show, Eq, Generic)

instance ToJSON ArchiveFormat where
    toJSON TarFormat = "tar"
    toJSON TarGzFormat = "tar.gz"
    toJSON ZipFormat = "zip"

instance FromJSON ArchiveFormat where
    parseJSON = Aeson.withText "ArchiveFormat" $ \t ->
        case t of
            "tar" -> pure TarFormat
            "tar.gz" -> pure TarGzFormat
            "zip" -> pure ZipFormat
            _ -> fail $ "Unknown archive format: " ++ Text.unpack t

-- | Detect archive format from file extension
detectArchiveFormat :: FilePath -> Maybe ArchiveFormat
detectArchiveFormat path
    | ".tar.gz" `isSuffixOf` path || ".tgz" `isSuffixOf` path = Just TarGzFormat
    | ".tar" `isSuffixOf` path = Just TarFormat
    | ".zip" `isSuffixOf` path = Just ZipFormat
    | otherwise = Nothing
  where
    isSuffixOf suffix str = take (length suffix) (reverse str) == reverse suffix

-------------------------------------------------------------------------------
-- Git Types
-------------------------------------------------------------------------------

data GitUrl = GitUrl
    { gitRemote :: Text
    , gitBranch :: Maybe Text
    , gitPath :: Maybe Namespace
    }
    deriving (Show, Eq)

data GitExportOptions = GitExportOptions
    { gitCommitMessage :: Text
    , gitTag :: Maybe Text
    , gitPush :: Bool
    }
    deriving (Show, Eq)

data GitImportOptions = GitImportOptions
    { gitRef :: Maybe Text -- branch, tag, or commit
    , gitSparsePaths :: [Namespace]
    }
    deriving (Show, Eq)

-- | Default git export options
defaultGitExportOptions :: GitExportOptions
defaultGitExportOptions =
    GitExportOptions
        { gitCommitMessage = "Update agent configurations"
        , gitTag = Nothing
        , gitPush = False
        }

-- | Default git import options
defaultGitImportOptions :: GitImportOptions
defaultGitImportOptions =
    GitImportOptions
        { gitRef = Nothing
        , gitSparsePaths = []
        }
