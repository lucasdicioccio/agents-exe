{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for export/import of agents and tools.
module System.Agents.ExportImport.Types where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Posix.Types (FileMode)

import System.Agents.Base (Agent)
import System.Agents.Tools.Bash (ScriptInfo)

-------------------------------------------------------------------------------
-- Package Metadata
-------------------------------------------------------------------------------

-- | Metadata for an export package.
data PackageMetadata = PackageMetadata
    { packageVersion :: Text
    -- ^ Version of the package format (e.g., "1.0.0")
    , packageCreatedAt :: UTCTime
    -- ^ When the package was created
    , packageDescription :: Maybe Text
    -- ^ Optional description of the package
    }
    deriving (Show, Eq, Generic)

instance ToJSON PackageMetadata where
    toJSON pm =
        Aeson.object
            [ "version" .= pm.packageVersion
            , "created-at" .= pm.packageCreatedAt
            , "description" .= pm.packageDescription
            ]

instance FromJSON PackageMetadata where
    parseJSON = Aeson.withObject "PackageMetadata" $ \v ->
        PackageMetadata
            <$> v .: "version"
            <*> v .: "created-at"
            <*> v .: "description"

-------------------------------------------------------------------------------
-- Agent Export Types
-------------------------------------------------------------------------------

-- | A complete agent package including tools.
data AgentPackage = AgentPackage
    { agentPackageMetadata :: PackageMetadata
    , agentPackageAgent :: Agent
    , agentPackageTools :: [StandaloneToolExport]
    }
    deriving (Show, Eq, Generic)

instance ToJSON AgentPackage where
    toJSON ap =
        Aeson.object
            [ "metadata" .= ap.agentPackageMetadata
            , "agent" .= ap.agentPackageAgent
            , "tools" .= ap.agentPackageTools
            ]

instance FromJSON AgentPackage where
    parseJSON = Aeson.withObject "AgentPackage" $ \v ->
        AgentPackage
            <$> v .: "metadata"
            <*> v .: "agent"
            <*> v .: "tools"

-------------------------------------------------------------------------------
-- Tool Export Types
-------------------------------------------------------------------------------

-- | A standalone tool package (no agent).
data ToolPackage = ToolPackage
    { toolPackageMetadata :: PackageMetadata
    , toolPackageTools :: [StandaloneToolExport]
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolPackage where
    toJSON tp =
        Aeson.object
            [ "metadata" .= tp.toolPackageMetadata
            , "tools" .= tp.toolPackageTools
            ]

instance FromJSON ToolPackage where
    parseJSON = Aeson.withObject "ToolPackage" $ \v ->
        ToolPackage
            <$> v .: "metadata"
            <*> v .: "tools"

-- | A standalone tool export including script content and metadata.
data StandaloneToolExport = StandaloneToolExport
    { standaloneToolInfo :: ScriptInfo
    -- ^ Tool metadata (args, slug, description, etc.)
    , standaloneToolScript :: ByteString
    -- ^ Script content (executable file)
    , standaloneToolPermissions :: FileMode
    -- ^ File permissions to preserve
    , standaloneToolAuxFiles :: [(FilePath, ByteString)]
    -- ^ Additional files (e.g., helper scripts, config files)
    }
    deriving (Show, Eq, Generic)

instance ToJSON StandaloneToolExport where
    toJSON ste =
        Aeson.object
            [ "info" .= ste.standaloneToolInfo
            , "script" .= bytesToBase64 ste.standaloneToolScript
            , "permissions" .= show ste.standaloneToolPermissions
            , "aux-files" .= map auxFileToJSON ste.standaloneToolAuxFiles
            ]
      where
        bytesToBase64 :: ByteString -> Text
        bytesToBase64 = Text.decodeUtf8 . Base64.encode
        
        auxFileToJSON :: (FilePath, ByteString) -> Aeson.Value
        auxFileToJSON (path, content) =
            Aeson.object
                [ "path" .= path
                , "content" .= bytesToBase64 content
                ]

instance FromJSON StandaloneToolExport where
    parseJSON = Aeson.withObject "StandaloneToolExport" $ \v ->
        StandaloneToolExport
            <$> v .: "info"
            <*> (v .: "script" >>= base64ToBytes)
            <*> (read <$> v .: "permissions")
            <*> (v .: "aux-files" >>= traverse jsonToAuxFile)
      where
        base64ToBytes :: Text -> Aeson.Parser ByteString
        base64ToBytes txt =
            case Base64.decode (Text.encodeUtf8 txt) of
                Left err -> fail $ "Invalid base64: " <> err
                Right bs -> pure bs
        
        jsonToAuxFile :: Aeson.Value -> Aeson.Parser (FilePath, ByteString)
        jsonToAuxFile = Aeson.withObject "AuxFile" $ \o -> do
            path <- o .: "path"
            content <- o .: "content" >>= base64ToBytes
            pure (path, content)

-------------------------------------------------------------------------------
-- Namespace Type
-------------------------------------------------------------------------------

-- | A namespace for organizing tools in git repositories.
newtype Namespace = Namespace Text
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Namespace where
    toJSON (Namespace ns) = Aeson.toJSON ns

instance FromJSON Namespace where
    parseJSON v = Namespace <$> Aeson.parseJSON v

-------------------------------------------------------------------------------
-- Archive Format
-------------------------------------------------------------------------------

-- | Supported archive formats for export/import.
data ArchiveFormat
    = TarGz
    -- ^ tar.gz compressed archive
    | Zip
    -- ^ ZIP archive
    | Directory
    -- ^ Plain directory (unpacked)
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ArchiveFormat where
    toJSON fmt = Aeson.String $ case fmt of
        TarGz -> "tar.gz"
        Zip -> "zip"
        Directory -> "directory"

instance FromJSON ArchiveFormat where
    parseJSON = Aeson.withText "ArchiveFormat" $ \t ->
        case t of
            "tar.gz" -> pure TarGz
            "zip" -> pure Zip
            "directory" -> pure Directory
            _ -> fail $ "Unknown archive format: " <> Text.unpack t

-------------------------------------------------------------------------------
-- Import/Export Errors
-------------------------------------------------------------------------------

-- | Errors that can occur during import operations.
data ImportError
    = InvalidPackageFormat String
    -- ^ The package format is invalid or corrupted
    | MissingManifest
    -- ^ The manifest.json file is missing
    | MissingToolScript Text
    -- ^ A tool script file is missing
    | VersionMismatch Text Text
    -- ^ Package version mismatch (expected, actual)
    | ImportIOError IOError
    -- ^ IO error during import
    | InvalidToolMetadata Text String
    -- ^ Tool metadata is invalid (tool slug, error)
    deriving (Show)

-- | Errors that can occur during git operations.
data GitError
    = GitCloneFailed String
    -- ^ Failed to clone git repository
    | GitCheckoutFailed String
    -- ^ Failed to checkout specific ref
    | InvalidGitUrl String
    -- ^ Invalid git URL format
    | GitIOError IOError
    -- ^ IO error during git operation
    | GitToolNotFound Text
    -- ^ Tool not found in git repository
    deriving (Show)

-- | Errors that can occur during tool installation.
data InstallError
    = ToolAlreadyExists Text
    -- ^ A tool with this slug already exists
    | PermissionError FilePath
    -- ^ Permission denied for file operation
    | InvalidToolDirectory FilePath
    -- ^ The tool directory is invalid
    | InstallIOError IOError
    -- ^ IO error during installation
    deriving (Show)

-------------------------------------------------------------------------------
-- Install Options
-------------------------------------------------------------------------------

-- | Options for tool installation.
data InstallOptions = InstallOptions
    { installForce :: Bool
    -- ^ Overwrite existing tools
    , installLink :: Bool
    -- ^ Use symlinks instead of copy (for git-based tools)
    , installPrefix :: Maybe Text
    -- ^ Add prefix to tool slugs
    }
    deriving (Show, Eq)

-- | Default install options.
defaultInstallOptions :: InstallOptions
defaultInstallOptions = InstallOptions
    { installForce = False
    , installLink = False
    , installPrefix = Nothing
    }

-------------------------------------------------------------------------------
-- Validation Errors
-------------------------------------------------------------------------------

-- | Errors that can occur during tool validation.
data ValidationError
    = DescribeFailed String
    -- ^ The 'describe' command failed
    | InvalidScriptInfo String
    -- ^ The ScriptInfo from describe doesn't match stored metadata
    | MissingDependencies [Text]
    -- ^ Required dependencies are missing
    deriving (Show)

