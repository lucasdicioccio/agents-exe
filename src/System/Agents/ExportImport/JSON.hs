{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : System.Agents.ExportImport.JSON
-- Description : JSON serialization for export/import types
-- Copyright   : (c) 2024 Lucas DiCioccio
-- License     : Apache-2.0
--
-- This module provides JSON serialization and deserialization for the
-- export/import types defined in 'System.Agents.ExportImport.Types'.
--
-- == Schema Versioning
--
-- The serialization includes a schema version for forward and backward
-- compatibility. The current schema version is "1.0.0".
--
-- === Version History
--
-- * __1.0.0__: Initial format supporting agents, tools, and MCP servers
--
-- === Future Compatibility
--
-- When adding new fields:
--
-- 1. Use 'Maybe' types for optional fields
-- 2. Provide sensible defaults in 'FromJSON' instances
-- 3. Increment minor version for backward-compatible changes
-- 4. Increment major version for breaking changes
--
-- === Encoding Format
--
-- Tool content is base64-encoded when embedded:
--
-- > {
-- >   "tag": "EmbeddedContent",
-- >   "content": "base64-encoded-content..."
-- > }
--
-- Or referenced by path:
--
-- > {
-- >   "tag": "ReferencedContent",
-- >   "path": "tools/my-tool.sh"
-- > }
module System.Agents.ExportImport.JSON (
    -- * JSON Serialization
    -- $serialization
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import System.Posix.Files (intersectFileModes, ownerExecuteMode, unionFileModes)
import System.Posix.Types (CMode (..))

import System.Agents.Base (Agent, McpServerDescription)
import System.Agents.ExportImport.Types (
    AgentExport (..),
    ExportPackage (..),
    McpServerExport (..),
    PackageMetadata (..),
    SchemaVersion (..),
    ToolContentMode (..),
    ToolExport (..),
    currentSchemaVersion,
 )

-------------------------------------------------------------------------------
-- Schema Version JSON
-------------------------------------------------------------------------------

-- | Schema versions are serialized as "major.minor.patch" strings.
instance ToJSON SchemaVersion where
    toJSON (SchemaVersion maj min patch) =
        Aeson.String $ Text.pack $ show maj ++ "." ++ show min ++ "." ++ show patch

instance FromJSON SchemaVersion where
    parseJSON = Aeson.withText "SchemaVersion" $ \txt ->
        case parseSchemaVersion txt of
            Left err -> fail err
            Right v -> pure v

-- | Parse a schema version from text.
parseSchemaVersion :: Text -> Either String SchemaVersion
parseSchemaVersion txt =
    case Text.split (== '.') txt of
        [maj, min, patch] ->
            case (readInt maj, readInt min, readInt patch) of
                (Just m, Just n, Just p) -> Right (SchemaVersion m n p)
                _ -> Left "Invalid version numbers in schema version"
        _ -> Left "Schema version must be in format 'major.minor.patch'"
  where
    readInt :: Text -> Maybe Int
    readInt = readMaybe . Text.unpack

    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

-------------------------------------------------------------------------------
-- Package Metadata JSON
-------------------------------------------------------------------------------

instance ToJSON PackageMetadata where
    toJSON meta =
        Aeson.object $
            [ "version" .= meta.packageSchemaVersion
            , "created_at" .= meta.packageCreatedAt
            ]
                ++ maybe [] (\d -> ["description" .= d]) meta.packageDescription
                ++ maybe [] (\s -> ["source" .= s]) meta.packageSource

instance FromJSON PackageMetadata where
    parseJSON = Aeson.withObject "PackageMetadata" $ \v ->
        PackageMetadata
            <$> v .: "version"
            <*> v .: "created_at"
            <*> v .:? "description"
            <*> v .:? "source"

-------------------------------------------------------------------------------
-- Tool Content Mode JSON
-------------------------------------------------------------------------------

-- | Tool content is serialized with a tag discriminating between
-- embedded and referenced modes.
instance ToJSON ToolContentMode where
    toJSON = \case
        EmbeddedContent bs ->
            Aeson.object
                [ "tag" .= ("EmbeddedContent" :: Text)
                , "content" .= encodeBase64 bs
                ]
        ReferencedContent path ->
            Aeson.object
                [ "tag" .= ("ReferencedContent" :: Text)
                , "path" .= path
                ]

instance FromJSON ToolContentMode where
    parseJSON = Aeson.withObject "ToolContentMode" $ \v -> do
        tag <- v .: "tag"
        case (tag :: Text) of
            "EmbeddedContent" ->
                EmbeddedContent <$> decodeBase64 (v .: "content")
            "ReferencedContent" ->
                ReferencedContent <$> v .: "path"
            _ -> fail $ "Unknown ToolContentMode tag: " ++ Text.unpack tag

-- | Encode a ByteString to a base64 text value.
encodeBase64 :: ByteString -> Text
encodeBase64 = Text.decodeUtf8 . Base64.encode

-- | Decode a base64 text value from a JSON parser.
decodeBase64 :: Aeson.Parser Text -> Aeson.Parser ByteString
decodeBase64 txtParser = do
    txt <- txtParser
    case Base64.decode (Text.encodeUtf8 txt) of
        Left err -> fail $ "Invalid base64: " ++ err
        Right bs -> pure bs

-------------------------------------------------------------------------------
-- CMode JSON
-------------------------------------------------------------------------------

-- | CMode is serialized as an integer representing the mode bits.
instance ToJSON CMode where
    toJSON (CMode mode) = Aeson.Number (fromIntegral mode)

instance FromJSON CMode where
    parseJSON = Aeson.withScientific "CMode" $ \s ->
        case toBoundedInteger s of
            Just n -> pure (CMode n)
            Nothing -> fail "CMode out of range"

-- | Convert a Scientific to a bounded integer.
toBoundedInteger :: (Integral a, Bounded a) => Scientific -> Maybe a
toBoundedInteger s =
    let i = round s
     in if fromIntegral (minBound :: Int) <= i && i <= fromIntegral (maxBound :: Int)
            then Just (fromIntegral i)
            else Nothing

-------------------------------------------------------------------------------
-- Tool Export JSON
-------------------------------------------------------------------------------

instance ToJSON ToolExport where
    toJSON tool =
        Aeson.object $
            [ "name" .= tool.toolName
            , "content" .= tool.toolContent
            , "permissions" .= tool.toolPermissions
            ]
                ++ maybe [] (\ns -> ["namespace" .= ns]) tool.toolNamespace
                ++ maybe [] (\meta -> ["metadata" .= meta]) tool.toolMetadata

instance FromJSON ToolExport where
    parseJSON = Aeson.withObject "ToolExport" $ \v ->
        ToolExport
            <$> v .: "name"
            <*> v .: "content"
            <*> v .: "permissions"
            <*> v .:? "namespace"
            <*> v .:? "metadata"

-------------------------------------------------------------------------------
-- Agent Export JSON
-------------------------------------------------------------------------------

instance ToJSON AgentExport where
    toJSON agent =
        Aeson.object $
            [ "config" .= agent.agentConfig
            , "tools" .= agent.agentTools
            ]
                ++ maybe [] (\ns -> ["namespace" .= ns]) agent.agentNamespace

instance FromJSON AgentExport where
    parseJSON = Aeson.withObject "AgentExport" $ \v ->
        AgentExport
            <$> v .: "config"
            <*> v .:? "namespace"
            <*> v .: "tools"

-------------------------------------------------------------------------------
-- MCP Server Export JSON
-------------------------------------------------------------------------------

instance ToJSON McpServerExport where
    toJSON mcp =
        Aeson.object $
            [ "config" .= mcp.mcpConfig
            ]
                ++ maybe [] (\ns -> ["namespace" .= ns]) mcp.mcpNamespace

instance FromJSON McpServerExport where
    parseJSON = Aeson.withObject "McpServerExport" $ \v ->
        McpServerExport
            <$> v .: "config"
            <*> v .:? "namespace"

-------------------------------------------------------------------------------
-- Export Package JSON
-------------------------------------------------------------------------------

-- | Top-level export package JSON format.
--
-- Example:
--
-- > {
-- >   "schema_version": "1.0.0",
-- >   "metadata": { ... },
-- >   "agents": [ ... ],
-- >   "tools": [ ... ],
-- >   "mcp_servers": [ ... ]
-- > }
instance ToJSON ExportPackage where
    toJSON pkg =
        Aeson.object
            [ "schema_version" .= currentSchemaVersion
            , "metadata" .= pkg.packageMetadata
            , "agents" .= pkg.packageAgents
            , "tools" .= pkg.packageTools
            , "mcp_servers" .= pkg.packageMcpServers
            ]

instance FromJSON ExportPackage where
    parseJSON = Aeson.withObject "ExportPackage" $ \v -> do
        schemaVer <- v .: "schema_version"
        -- TODO: Add version compatibility checking here
        -- For now, we just parse it but don't enforce compatibility
        _ <- pure schemaVer :: Aeson.Parser SchemaVersion

        ExportPackage
            <$> v .: "metadata"
            <*> v .: "agents"
            <*> v .: "tools"
            <*> v .: "mcp_servers"

-------------------------------------------------------------------------------
-- Migration Support
-------------------------------------------------------------------------------

-- $migration
-- Future schema versions should implement migration logic here.
--
-- Example migration pattern:
--
-- > migratePackage :: SchemaVersion -> Value -> Either String ExportPackage
-- > migratePackage ver val
-- >     | ver == SchemaVersion 1 0 0 = parseEither parseJSON val
-- >     | ver == SchemaVersion 1 1 0 = migrateFrom_1_0_0 val >>= migratePackage (SchemaVersion 1 0 0)
-- >     | otherwise = Left $ "Unsupported schema version: " ++ show ver

-- | Check if a schema version is compatible with the current version.
-- Currently only exact version match is supported.
isCompatibleVersion :: SchemaVersion -> Bool
isCompatibleVersion ver = ver == currentSchemaVersion

-- | Make a CMode executable by adding the owner executable bit.
makeExecutable :: CMode -> CMode
makeExecutable mode = mode `unionFileModes` ownerExecuteMode

-- | Check if a CMode has the executable bit set.
isExecutableMode :: CMode -> Bool
isExecutableMode mode =
    (mode `intersectFileModes` ownerExecuteMode) /= 0

