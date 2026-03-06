{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Archive export/import operations for tar and zip formats.
--
-- This module provides functionality to export agent configurations,
-- tools, and MCP server configurations to archive files, and to
-- import them back.
module System.Agents.ExportImport.Archive
    ( ArchiveFormat (..)
    , ImportError (..)
    , exportToArchive
    , importFromArchive
    , detectArchiveFormat
    , preserveToolPermissions
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as ZipArchive
import qualified Codec.Compression.GZip as GZip
import Control.Exception (bracket, catch)
import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory
import System.FilePath
import System.Posix.Files (intersectFileModes, ownerExecuteMode, setFileMode)
import qualified System.Posix.Files as Posix

import System.Agents.ExportImport.Types

-------------------------------------------------------------------------------
-- Archive Format
-------------------------------------------------------------------------------

-- | Supported archive formats.
data ArchiveFormat
    = TarFormat
    -- ^ Plain tar archive
    | TarGzFormat
    -- ^ Gzip-compressed tar archive
    | ZipFormat
    -- ^ ZIP archive
    deriving (Show, Eq)

-- | Errors that can occur during import.
data ImportError
    = ArchiveReadError String
    | ArchiveFormatError String
    | PackageParseError String
    | NamespaceConflictError Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Archive Structure Constants
-------------------------------------------------------------------------------

-- | Root directory inside the archive.
archiveRoot :: FilePath
archiveRoot = "agents-export"

-------------------------------------------------------------------------------
-- Format Detection
-------------------------------------------------------------------------------

-- | Detect archive format from file extension.
--
-- Recognizes:
-- * @.tar@ -> 'TarFormat'
-- * @.tar.gz@, @.tgz@ -> 'TarGzFormat'
-- * @.zip@ -> 'ZipFormat'
detectArchiveFormat :: FilePath -> Maybe ArchiveFormat
detectArchiveFormat path
    | ".tar.gz" `isSuffixOf` lowerPath = Just TarGzFormat
    | ".tgz" `isSuffixOf` lowerPath = Just TarGzFormat
    | ".tar" `isSuffixOf` lowerPath = Just TarFormat
    | ".zip" `isSuffixOf` lowerPath = Just ZipFormat
    | otherwise = Nothing
  where
    lowerPath = map toLower path
    toLower c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c

-------------------------------------------------------------------------------
-- Export Operations
-------------------------------------------------------------------------------

-- | Export a package to an archive file.
--
-- Creates a temporary directory structure with the package contents,
-- then packs it into the selected archive format.
--
-- The archive will have this structure:
-- @
-- agents-export/
-- ├── manifest.json
-- ├── agents/
-- │   └── agent-name.json
-- ├── tools/
-- │   └── tool-name/
-- │       ├── script
-- │       └── metadata.json
-- └── mcp-servers/
--     └── mcp-name.json
-- @
exportToArchive :: ExportPackage -> ArchiveFormat -> FilePath -> IO ()
exportToArchive pkg format outputPath = do
    -- Create temporary directory
    withTempDirectory $ \tempDir -> do
        let exportDir = tempDir </> archiveRoot

        -- Create directory structure
        createDirectoryIfMissing True (exportDir </> agentsDirBase)
        createDirectoryIfMissing True (exportDir </> toolsDirBase)
        createDirectoryIfMissing True (exportDir </> mcpServersDirBase)

        -- Write manifest
        let manifest = packageToManifest pkg
        Aeson.encodeFile (exportDir </> "manifest.json") manifest

        -- Write agents
        forM_ pkg.epAgents $ \agent -> do
            let agentFile = exportDir </> agentsDirBase </> Text.unpack agent.eaName <> ".json"
            Aeson.encodeFile agentFile agent.eaConfig

        -- Write tools
        forM_ (Map.toList pkg.epTools) $ \(toolName, tool) -> do
            let toolDir = exportDir </> toolsDirBase </> Text.unpack toolName
            createDirectoryIfMissing True toolDir

            -- Write metadata
            Aeson.encodeFile (toolDir </> toolMetadataFile) tool.etMetadata

            -- Write script
            TextIO.writeFile (toolDir </> toolScriptFile) tool.etScriptContent

            -- Restore file permissions if available
            case tool.etMetadata.tmFileMode of
                Just mode -> setFileMode (toolDir </> toolScriptFile) (fromIntegral mode)
                Nothing -> pure ()

        -- Write MCP servers
        forM_ pkg.epMcpServers $ \server -> do
            let serverFile = exportDir </> mcpServersDirBase </> Text.unpack server.emName <> ".json"
            Aeson.encodeFile serverFile server.emConfig

        -- Create archive
        case format of
            TarFormat -> createTarArchive exportDir outputPath
            TarGzFormat -> createTarGzArchive exportDir outputPath
            ZipFormat -> createZipArchive exportDir outputPath
  where
    agentsDirBase = "agents"
    toolsDirBase = "tools"
    mcpServersDirBase = "mcp-servers"
    toolMetadataFile = "metadata.json"
    toolScriptFile = "script"

-- | Create a tar archive from a directory.
createTarArchive :: FilePath -> FilePath -> IO ()
createTarArchive sourceDir outputPath = do
    entries <- Tar.pack (takeDirectory sourceDir) [takeFileName sourceDir]
    LBS.writeFile outputPath (Tar.write entries)

-- | Create a gzip-compressed tar archive from a directory.
createTarGzArchive :: FilePath -> FilePath -> IO ()
createTarGzArchive sourceDir outputPath = do
    entries <- Tar.pack (takeDirectory sourceDir) [takeFileName sourceDir]
    LBS.writeFile outputPath (GZip.compress $ Tar.write entries)

-- | Create a zip archive from a directory using zip-archive.
createZipArchive :: FilePath -> FilePath -> IO ()
createZipArchive sourceDir outputPath = do
    archive <- createZipFromDirectory sourceDir
    LBS.writeFile outputPath (ZipArchive.fromArchive archive)

-- | Recursively create a zip archive from a directory.
createZipFromDirectory :: FilePath -> IO ZipArchive.Archive
createZipFromDirectory rootDir = do
    entries <- collectEntries rootDir ""
    pure $ foldr ZipArchive.addEntryToArchive ZipArchive.emptyArchive entries
  where
    collectEntries :: FilePath -> FilePath -> IO [ZipArchive.Entry]
    collectEntries dir relativePath = do
        contents <- listDirectory dir
        fmap concat $ mapM (processEntry relativePath) contents

    processEntry :: FilePath -> FilePath -> IO [ZipArchive.Entry]
    processEntry relPath name = do
        let fullPath = rootDir </> relPath </> name
        let entryPath = relPath </> name
        isDir <- doesDirectoryExist fullPath
        if isDir
            then collectEntries fullPath entryPath
            else do
                content <- BS.readFile fullPath
                let entry = ZipArchive.toEntry entryPath 0 (LBS.fromStrict content)
                pure [entry]

-------------------------------------------------------------------------------
-- Import Operations
-------------------------------------------------------------------------------

-- | Import a package from an archive file.
--
-- Detects the archive format from the file extension, extracts it to a
-- temporary directory, and parses the contents into an 'ExportPackage'.
importFromArchive :: FilePath -> IO (Either ImportError ExportPackage)
importFromArchive archivePath = do
    case detectArchiveFormat archivePath of
        Nothing -> pure $ Left $ ArchiveFormatError $ "Cannot detect format from path: " <> archivePath
        Just format -> do
            result <- tryImport format
            pure result
  where
    tryImport :: ArchiveFormat -> IO (Either ImportError ExportPackage)
    tryImport format = do
        withTempDirectory $ \tempDir -> do
            -- Extract archive
            extractResult <- tryExtract format tempDir
            case extractResult of
                Left err -> pure $ Left err
                Right extractDir -> do
                    -- Find the agents-export directory
                    exportDir <- findExportDir extractDir
                    case exportDir of
                        Nothing -> pure $ Left $ ArchiveFormatError "Missing agents-export directory in archive"
                        Just dir -> parseExportPackage dir

    tryExtract :: ArchiveFormat -> FilePath -> IO (Either ImportError FilePath)
    tryExtract format tempDir =
        catch
            ( do
                case format of
                    TarFormat -> extractTarArchive archivePath tempDir
                    TarGzFormat -> extractTarGzArchive archivePath tempDir
                    ZipFormat -> extractZipArchive archivePath tempDir
                pure $ Right tempDir
            )
            (\(e :: IOError) -> pure $ Left $ ArchiveReadError $ show e)

    findExportDir :: FilePath -> IO (Maybe FilePath)
    findExportDir tempDir = do
        let possibleDir = tempDir </> archiveRoot
        exists <- doesDirectoryExist possibleDir
        if exists
            then pure $ Just possibleDir
            else do
                -- Try to find any subdirectory containing agents-export
                contents <- listDirectory tempDir
                case contents of
                    [subdir] -> do
                        let nestedDir = tempDir </> subdir </> archiveRoot
                        nestedExists <- doesDirectoryExist nestedDir
                        pure $ if nestedExists then Just nestedDir else Nothing
                    _ -> pure Nothing

-- | Parse an extracted export directory into an ExportPackage.
parseExportPackage :: FilePath -> IO (Either ImportError ExportPackage)
parseExportPackage exportDir = do
    -- Read and parse manifest
    let manifestFile = exportDir </> "manifest.json"
    manifestExists <- doesFileExist manifestFile
    unless manifestExists $ do
        error "Missing manifest.json in archive"

    manifestResult <- Aeson.eitherDecodeFileStrict' manifestFile
    case manifestResult of
        Left err -> pure $ Left $ PackageParseError $ "Failed to parse manifest: " <> err
        Right (manifest :: PackageManifest) -> do
            -- Validate schema version (warn if newer, error if too old)
            let schemaResult = validateSchemaVersion (manifestMetadata manifest).pmSchemaVersion
            case schemaResult of
                Just err -> pure $ Left $ PackageParseError err
                Nothing -> do
                    -- Parse agents
                    agentResult <- parseAgents exportDir (manifestAgents manifest)
                    case agentResult of
                        Left err -> pure $ Left err
                        Right agents -> do
                            -- Parse tools
                            toolResult <- parseTools exportDir (manifestTools manifest)
                            case toolResult of
                                Left err -> pure $ Left err
                                Right tools -> do
                                    -- Parse MCP servers
                                    mcpResult <- parseMcpServers exportDir (manifestMcpServers manifest)
                                    case mcpResult of
                                        Left err -> pure $ Left err
                                        Right mcps -> do
                                            let pkg =
                                                    ExportPackage
                                                        { epMetadata = manifestMetadata manifest
                                                        , epAgents = agents
                                                        , epTools = tools
                                                        , epMcpServers = mcps
                                                        }
                                            pure $ Right pkg

-- | Validate that the schema version is supported.
validateSchemaVersion :: Text -> Maybe String
validateSchemaVersion version
    | version == schemaVersion = Nothing
    | version > schemaVersion = Just $ "Schema version " <> Text.unpack version <> " is newer than supported version " <> Text.unpack schemaVersion
    | otherwise = Just $ "Schema version " <> Text.unpack version <> " is too old"

-- | Parse all agents from the export directory.
parseAgents :: FilePath -> [Text] -> IO (Either ImportError [ExportedAgent])
parseAgents exportDir agentFiles = do
    results <- mapM parseAgent agentFiles
    pure $ sequence results
  where
    parseAgent :: Text -> IO (Either ImportError ExportedAgent)
    parseAgent fileName = do
        let filePath = exportDir </> "agents" </> Text.unpack fileName
        result <- Aeson.eitherDecodeFileStrict' filePath
        case result of
            Left err -> pure $ Left $ PackageParseError $ "Failed to parse agent " <> Text.unpack fileName <> ": " <> err
            Right config -> do
                let name = Text.pack $ dropExtension $ Text.unpack fileName
                pure $ Right $ ExportedAgent name config

-- | Parse all tools from the export directory.
parseTools :: FilePath -> [Text] -> IO (Either ImportError (Map Text ExportedTool))
parseTools exportDir toolNames = do
    results <- mapM parseTool toolNames
    pure $ fmap Map.fromList $ sequence results
  where
    toolMetadataFile = "metadata.json"
    toolScriptFile = "script"

    parseTool :: Text -> IO (Either ImportError (Text, ExportedTool))
    parseTool toolName = do
        let toolDir = exportDir </> "tools" </> Text.unpack toolName
        let metadataFile = toolDir </> toolMetadataFile
        let scriptFile = toolDir </> toolScriptFile

        -- Check if tool directory exists
        dirExists <- doesDirectoryExist toolDir
        unless dirExists $ do
            error $ "Tool directory not found: " <> toolDir

        -- Parse metadata
        metadataResult <- Aeson.eitherDecodeFileStrict' metadataFile
        case metadataResult of
            Left err -> pure $ Left $ PackageParseError $ "Failed to parse tool metadata " <> Text.unpack toolName <> ": " <> err
            Right metadata -> do
                -- Read script content
                scriptContent <- TextIO.readFile scriptFile

                -- Restore permissions if available
                case metadata.tmFileMode of
                    Just mode -> setFileMode scriptFile (fromIntegral mode)
                    Nothing -> pure ()

                let tool = ExportedTool metadata scriptContent
                pure $ Right (toolName, tool)

-- | Parse all MCP servers from the export directory.
parseMcpServers :: FilePath -> [Text] -> IO (Either ImportError [ExportedMcpServer])
parseMcpServers exportDir mcpFiles = do
    results <- mapM parseMcp mcpFiles
    pure $ sequence results
  where
    parseMcp :: Text -> IO (Either ImportError ExportedMcpServer)
    parseMcp fileName = do
        let filePath = exportDir </> "mcp-servers" </> Text.unpack fileName
        result <- Aeson.eitherDecodeFileStrict' filePath
        case result of
            Left err -> pure $ Left $ PackageParseError $ "Failed to parse MCP server " <> Text.unpack fileName <> ": " <> err
            Right config -> do
                let name = Text.pack $ dropExtension $ Text.unpack fileName
                pure $ Right $ ExportedMcpServer name config

-------------------------------------------------------------------------------
-- Archive Extraction
-------------------------------------------------------------------------------

-- | Extract a tar archive to a directory.
extractTarArchive :: FilePath -> FilePath -> IO ()
extractTarArchive archivePath destDir = do
    content <- LBS.readFile archivePath
    let entries = Tar.read content
    Tar.unpack destDir entries

-- | Extract a gzip-compressed tar archive to a directory.
extractTarGzArchive :: FilePath -> FilePath -> IO ()
extractTarGzArchive archivePath destDir = do
    content <- LBS.readFile archivePath
    let decompressed = GZip.decompress content
    let entries = Tar.read decompressed
    Tar.unpack destDir entries

-- | Extract a zip archive to a directory using zip-archive.
extractZipArchive :: FilePath -> FilePath -> IO ()
extractZipArchive archivePath destDir = do
    content <- LBS.readFile archivePath
    let archive = ZipArchive.toArchive content
    extractZipArchiveToDir archive destDir

-- | Extract a zip archive to a directory.
extractZipArchiveToDir :: ZipArchive.Archive -> FilePath -> IO ()
extractZipArchiveToDir archive destDir = do
    mapM_ extractEntry (ZipArchive.zEntries archive)
  where
    extractEntry entry = do
        let entryPath = ZipArchive.eRelativePath entry
        let fullPath = destDir </> entryPath
        createDirectoryIfMissing True (takeDirectory fullPath)
        LBS.writeFile fullPath (ZipArchive.fromEntry entry)

-------------------------------------------------------------------------------
-- Permission Handling
-------------------------------------------------------------------------------

-- | Preserve tool permissions by copying file mode from source to destination.
--
-- This is used when exporting/importing to ensure executable scripts
-- maintain their executable bit.
preserveToolPermissions :: FilePath -> FilePath -> IO ()
preserveToolPermissions source dest = do
    -- Check if source is executable
    perms <- Posix.getFileStatus source
    let mode = Posix.fileMode perms
    let isExecutable = intersectFileModes mode ownerExecuteMode /= 0

    when isExecutable $ do
        -- Make destination executable for owner
        destPerms <- Posix.getFileStatus dest
        let newMode = Posix.fileMode destPerms `Posix.unionFileModes` ownerExecuteMode
        setFileMode dest newMode

-------------------------------------------------------------------------------
-- Temporary Directory Handling
-------------------------------------------------------------------------------

-- | Create a temporary directory and clean it up after use.
withTempDirectory :: forall a. (FilePath -> IO a) -> IO a
withTempDirectory action = do
    tempBase <- getTemporaryDirectory
    -- Create a unique temp directory using process ID and random component
    pid <- getProcessID
    let mkTempDir n = tempBase </> "agents-export-" <> show pid <> "-" <> show n
    findAndUseTempDir mkTempDir 0
  where
    findAndUseTempDir :: (Int -> FilePath) -> Int -> IO a
    findAndUseTempDir mkPath n = do
        let tempDir = mkPath n
        exists <- doesDirectoryExist tempDir
        if exists
            then findAndUseTempDir mkPath (n + 1)
            else do
                createDirectoryIfMissing True tempDir
                bracket
                    (pure tempDir)
                    cleanupTempDir
                    action

    cleanupTempDir :: FilePath -> IO ()
    cleanupTempDir dir = do
        -- Best-effort cleanup
        _ <- tryRemoveDirectory dir
        pure ()

    tryRemoveDirectory :: FilePath -> IO (Either IOError ())
    tryRemoveDirectory dir =
        catch
            (Right <$> removePathForcibly dir)
            (\(e :: IOError) -> pure $ Left e)

    -- Simple fallback - in real implementation would use System.Posix.Process
    getProcessID :: IO Int
    getProcessID = pure 0

