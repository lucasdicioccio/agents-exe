{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Archive operations for exporting and importing agents and tools.
module System.Agents.ExportImport.Archive where

import Control.Exception (try, IOException)
import Control.Monad (forM_, unless, when)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Prod.Tracer (Tracer(..))
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.Exit (ExitCode(..))
import System.FilePath
    ( (</>)
    , takeDirectory
    , takeExtension
    , takeFileName
    )
import System.Posix.Files
    ( fileMode
    , getFileStatus
    , setFileMode
    )
import System.Posix.Types (FileMode)
import System.Process (readProcessWithExitCode)

import System.Agents.ExportImport.Types
import System.Agents.Tools.Bash (ScriptDescription(..), ScriptInfo(..), Scripts(..), InvalidScriptError(..))
import qualified System.Agents.Tools.Bash as Bash

-------------------------------------------------------------------------------
-- Manifest Types
-------------------------------------------------------------------------------

-- | Manifest for tool-only exports.
data ToolManifest = ToolManifest
    { toolManifestMetadata :: PackageMetadata
    , toolManifestTools :: [ToolManifestEntry]
    }
    deriving (Show, Eq)

instance ToJSON ToolManifest where
    toJSON tm =
        Aeson.object
            [ "metadata" .= tm.toolManifestMetadata
            , "tools" .= tm.toolManifestTools
            , "package-type" .= ("tools-only" :: Text)
            ]

instance FromJSON ToolManifest where
    parseJSON = Aeson.withObject "ToolManifest" $ \v ->
        ToolManifest
            <$> v .: "metadata"
            <*> v .: "tools"

-- | Entry in the tool manifest.
data ToolManifestEntry = ToolManifestEntry
    { toolEntrySlug :: Text
    , toolEntryDir :: FilePath
    , toolEntryHasAuxFiles :: Bool
    }
    deriving (Show, Eq)

instance ToJSON ToolManifestEntry where
    toJSON tme =
        Aeson.object
            [ "slug" .= tme.toolEntrySlug
            , "directory" .= tme.toolEntryDir
            , "has-aux-files" .= tme.toolEntryHasAuxFiles
            ]

instance FromJSON ToolManifestEntry where
    parseJSON = Aeson.withObject "ToolManifestEntry" $ \v ->
        ToolManifestEntry
            <$> v .: "slug"
            <*> v .: "directory"
            <*> v .: "has-aux-files"

-------------------------------------------------------------------------------
-- Tool-Only Archive Operations
-------------------------------------------------------------------------------

-- | Export only tools to an archive.
exportToolsToArchive :: [StandaloneToolExport] -> ArchiveFormat -> FilePath -> IO ()
exportToolsToArchive tools format outputPath = do
    -- Create a temporary directory for the export
    tmpDir <- createTempDirectory "agents-export-tools-"
    
    -- Create the export structure
    let exportDir = tmpDir </> "tools-export"
    createDirectoryIfMissing True exportDir
    
    -- Create metadata
    now <- getCurrentTime
    let metadata = PackageMetadata
            { packageVersion = "1.0.0"
            , packageCreatedAt = now
            , packageDescription = Just $ "Tool export with " <> Text.pack (show (length tools)) <> " tools"
            }
    
    -- Write each tool to the export directory
    manifestEntries <- mapM (writeToolToExport exportDir) tools
    
    -- Create and write manifest
    let manifest = ToolManifest
            { toolManifestMetadata = metadata
            , toolManifestTools = manifestEntries
            }
    LByteString.writeFile (exportDir </> "manifest.json") (Aeson.encode manifest)
    
    -- Create archive based on format
    case format of
        TarGz -> createTarGz exportDir outputPath
        Zip -> createZip exportDir outputPath
        Directory -> copyDirectory exportDir outputPath
    
    -- Cleanup
    removeDirectoryRecursive tmpDir

-- | Import only tools from an archive.
importToolsFromArchive :: FilePath -> IO (Either ImportError ToolPackage)
importToolsFromArchive archivePath = do
    -- Create a temporary directory for extraction
    tmpDir <- createTempDirectory "agents-import-tools-"
    
    result <- try @IOException $ do
        -- Detect and extract archive format
        extractArchive archivePath tmpDir
        
        -- Find the export directory
        exportDir <- findExportDirectory tmpDir
        
        -- Read manifest
        let manifestPath = exportDir </> "manifest.json"
        manifestExists <- doesFileExist manifestPath
        unless manifestExists $ error "Missing manifest.json"
        
        manifestBytes <- LByteString.readFile manifestPath
        case Aeson.eitherDecode manifestBytes of
            Left err -> error $ "Invalid manifest: " <> err
            Right manifest -> do
                -- Read each tool from the export
                tools <- mapM (readToolFromExport exportDir) (toolManifestTools manifest)
                
                pure $ ToolPackage
                    { toolPackageMetadata = toolManifestMetadata manifest
                    , toolPackageTools = tools
                    }
    
    -- Cleanup
    removeDirectoryRecursive tmpDir
    
    case result of
        Left e -> pure $ Left $ ImportIOError e
        Right pkg -> pure $ Right pkg

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Write a single tool to the export directory.
writeToolToExport :: FilePath -> StandaloneToolExport -> IO ToolManifestEntry
writeToolToExport exportDir tool = do
    let slug = Bash.scriptSlug (standaloneToolInfo tool)
        toolDir = exportDir </> "tools" </> Text.unpack slug
        auxDir = toolDir </> "aux"
    
    -- Create tool directory
    createDirectoryIfMissing True toolDir
    
    -- Write script
    let scriptPath = toolDir </> "script"
    ByteString.writeFile scriptPath tool.standaloneToolScript
    setFileMode scriptPath tool.standaloneToolPermissions
    
    -- Write metadata
    let metadataPath = toolDir </> "metadata.json"
    LByteString.writeFile metadataPath (Aeson.encode tool.standaloneToolInfo)
    
    -- Write aux files if any
    hasAuxFiles <- if null tool.standaloneToolAuxFiles
        then pure False
        else do
            createDirectoryIfMissing True auxDir
            forM_ tool.standaloneToolAuxFiles $ \(relPath, content) -> do
                let auxPath = auxDir </> relPath
                createDirectoryIfMissing True (takeDirectory auxPath)
                ByteString.writeFile auxPath content
            pure True
    
    pure ToolManifestEntry
        { toolEntrySlug = slug
        , toolEntryDir = "tools/" <> Text.unpack slug
        , toolEntryHasAuxFiles = hasAuxFiles
        }

-- | Read a single tool from the export directory.
readToolFromExport :: FilePath -> ToolManifestEntry -> IO StandaloneToolExport
readToolFromExport exportDir entry = do
    let toolDir = exportDir </> toolEntryDir entry
        scriptPath = toolDir </> "script"
        metadataPath = toolDir </> "metadata.json"
        auxDir = toolDir </> "aux"
    
    -- Read script
    scriptContent <- ByteString.readFile scriptPath
    permissions <- fileMode <$> getFileStatus scriptPath
    
    -- Read metadata
    metadataBytes <- LByteString.readFile metadataPath
    scriptInfo <- case Aeson.eitherDecode metadataBytes of
        Left err -> error $ "Invalid metadata for " <> Text.unpack (toolEntrySlug entry) <> ": " <> err
        Right si -> pure si
    
    -- Read aux files if any
    auxFiles <- if toolEntryHasAuxFiles entry
        then readAuxFiles auxDir
        else pure []
    
    pure StandaloneToolExport
        { standaloneToolInfo = scriptInfo
        , standaloneToolScript = scriptContent
        , standaloneToolPermissions = permissions
        , standaloneToolAuxFiles = auxFiles
        }

-- | Read auxiliary files from a directory.
readAuxFiles :: FilePath -> IO [(FilePath, ByteString.ByteString)]
readAuxFiles auxDir = do
    exists <- doesDirectoryExist auxDir
    if not exists
        then pure []
        else do
            files <- listDirectoryRecursive auxDir
            mapM (\f -> do
                content <- ByteString.readFile (auxDir </> f)
                pure (f, content)) files

-- | Recursively list all files in a directory.
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    entries <- listDirectory dir
    fmap concat $ mapM (\entry -> do
        let fullPath = dir </> entry
        isDir <- doesDirectoryExist fullPath
        if isDir
            then fmap (map (entry </>)) (listDirectoryRecursive fullPath)
            else pure [entry]) entries

-------------------------------------------------------------------------------
-- Archive Creation and Extraction
-------------------------------------------------------------------------------

-- | Create a tar.gz archive.
createTarGz :: FilePath -> FilePath -> IO ()
createTarGz sourceDir outputPath = do
    Tar.create outputPath (takeFileName sourceDir) [sourceDir]

-- | Create a ZIP archive.
createZip :: FilePath -> FilePath -> IO ()
createZip sourceDir outputPath = do
    -- Use system zip command for simplicity
    (code, _, err) <- readProcessWithExitCode "zip" ["-r", outputPath, "."] ""
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> error $ "zip failed: " <> err

-- | Copy directory contents.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory sourceDir destDir = do
    createDirectoryIfMissing True destDir
    entries <- listDirectory sourceDir
    forM_ entries $ \entry -> do
        let srcPath = sourceDir </> entry
            dstPath = destDir </> entry
        isDir <- doesDirectoryExist srcPath
        if isDir
            then copyDirectory srcPath dstPath
            else copyFile srcPath dstPath

-- | Extract an archive to a directory.
extractArchive :: FilePath -> FilePath -> IO ()
extractArchive archivePath destDir = do
    let ext = takeExtension archivePath
    case ext of
        ".gz" -> extractTarGz archivePath destDir
        ".tar" -> extractTarGz archivePath destDir
        ".zip" -> extractZip archivePath destDir
        _ -> do
            -- Assume directory or try tar.gz
            isDir <- doesDirectoryExist archivePath
            if isDir
                then copyDirectory archivePath destDir
                else extractTarGz archivePath destDir  -- Fallback

-- | Extract a tar.gz archive.
extractTarGz :: FilePath -> FilePath -> IO ()
extractTarGz tarPath destDir = do
    content <- LByteString.readFile tarPath
    let entries = Tar.read (GZip.decompress content)
    Tar.unpack destDir entries

-- | Extract a ZIP archive.
extractZip :: FilePath -> FilePath -> IO ()
extractZip zipPath destDir = do
    -- Use system unzip command for simplicity
    (code, _, err) <- readProcessWithExitCode "unzip" ["-q", zipPath, "-d", destDir] ""
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> error $ "unzip failed: " <> err

-- | Find the export directory within extracted contents.
findExportDirectory :: FilePath -> IO FilePath
findExportDirectory tmpDir = do
    entries <- listDirectory tmpDir
    case entries of
        [single] -> do
            let singlePath = tmpDir </> single
            isDir <- doesDirectoryExist singlePath
            if isDir
                then do
                    -- Check if this is the export directory
                    hasManifest <- doesFileExist (singlePath </> "manifest.json")
                    if hasManifest
                        then pure singlePath
                        else findExportDirectory singlePath
                else pure tmpDir
        _ -> pure tmpDir

-- | Create a temporary directory.
createTempDirectory :: String -> IO FilePath
createTempDirectory prefix = do
    tmp <- getTemporaryDirectory
    now <- getCurrentTime
    let dirName = prefix <> show now
        tmpDir = tmp </> dirName
    createDirectoryIfMissing True tmpDir
    pure tmpDir

-- | Get the system temporary directory.
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = do
    result <- try @IOException $ readProcessWithExitCode "mktemp" ["-d"] ""
    case result of
        Right (ExitSuccess, out, _) -> pure (init out)  -- Remove trailing newline
        _ -> pure "/tmp"

-------------------------------------------------------------------------------
-- Tool Discovery
-------------------------------------------------------------------------------

-- | Discover tools from a directory.
discoverTools :: FilePath -> IO [ScriptDescription]
discoverTools path = do
    exists <- doesDirectoryExist path
    if not exists
        then pure []
        else do
            let silentTracer = Tracer $ \_ -> pure ()
            (scripts, _) <- Bash.loadDirectory silentTracer path
            pure scripts.scriptDescriptions

-- | Create a StandaloneToolExport from a ScriptDescription.
exportToolFromDescription :: ScriptDescription -> IO StandaloneToolExport
exportToolFromDescription desc = do
    -- Read script content
    scriptContent <- ByteString.readFile desc.scriptPath
    
    -- Get permissions
    permissions <- fileMode <$> getFileStatus desc.scriptPath
    
    -- Look for aux files (files with same base name but different extensions)
    let scriptDir = takeDirectory desc.scriptPath
        scriptBase = takeFileName desc.scriptPath
    auxFiles <- discoverAuxFiles scriptDir scriptBase
    
    pure StandaloneToolExport
        { standaloneToolInfo = desc.scriptInfo
        , standaloneToolScript = scriptContent
        , standaloneToolPermissions = permissions
        , standaloneToolAuxFiles = auxFiles
        }

-- | Discover auxiliary files for a script.
-- Looks for files that start with the script name (e.g., script.py, script.json, etc.)
discoverAuxFiles :: FilePath -> FilePath -> IO [(FilePath, ByteString.ByteString)]
discoverAuxFiles scriptDir scriptBase = do
    exists <- doesDirectoryExist scriptDir
    if not exists
        then pure []
        else do
            entries <- listDirectory scriptDir
            let auxFiles = filter (\e -> e /= scriptBase && scriptBase `isPrefixOf` e) entries
            mapM (\f -> do
                content <- ByteString.readFile (scriptDir </> f)
                pure (f, content)) auxFiles

