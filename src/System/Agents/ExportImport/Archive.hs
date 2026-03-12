{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.ExportImport.Archive (
    exportToArchive,
    exportToolsToArchive,
    importFromArchive,
    importToolsFromArchive,
    getFileMode,
) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.FilePath (
    joinPath,
    normalise,
    splitDirectories,
    takeDirectory,
    takeFileName,
    (<.>),
    (</>),
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, ownerExecuteMode, setFileMode, unionFileModes)
import System.Posix.Types (FileMode)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import System.Agents.Base (Agent (..), McpServerDescription (..), McpSimpleBinaryConfiguration (..))
import System.Agents.ExportImport.Types
import System.Agents.Tools.Bash (ScriptInfo (..))

-------------------------------------------------------------------------------
-- Archive Structure Constants
-------------------------------------------------------------------------------

manifestFileName :: FilePath
manifestFileName = "manifest.json"

agentsDirName :: FilePath
agentsDirName = "agents"

toolsDirName :: FilePath
toolsDirName = "tools"

mcpServersDirName :: FilePath
mcpServersDirName = "mcp-servers"

-------------------------------------------------------------------------------
-- Export Operations
-------------------------------------------------------------------------------

-- | Export a package to an archive file
exportToArchive :: ExportPackage -> ArchiveFormat -> FilePath -> IO (Either ImportError ())
exportToArchive pkg fmt path = do
    result <- try $ withSystemTempDirectory "agents-export-" $ \tmpDir -> do
        -- Write package contents to temp directory
        let exportDir = tmpDir </> "agents-export"
        createDirectoryIfMissing True exportDir

        -- Write manifest
        LByteString.writeFile (exportDir </> manifestFileName) $
            AesonPretty.encodePretty pkg.packageMetadata

        -- Write agents
        createDirectoryIfMissing True (exportDir </> agentsDirName)
        forM_ pkg.packageAgents $ \agent -> do
            let agentFile = exportDir </> agentsDirName </> agentFileName agent
            createDirectoryIfMissing True (takeDirectory agentFile)
            LByteString.writeFile agentFile $ AesonPretty.encodePretty agent.agentConfig

            -- Write agent tools
            forM_ agent.agentTools $ \tool -> do
                let toolDirName = fromMaybe (slug agent.agentConfig) tool.toolNamespace
                let toolDir = exportDir </> toolsDirName </> Text.unpack toolDirName
                createDirectoryIfMissing True toolDir
                let scriptPath = toolDir </> Text.unpack tool.toolName
                ByteString.writeFile scriptPath tool.toolContent
                setFileMode scriptPath tool.toolPermissions
                -- Write metadata if present
                case tool.toolMetadata of
                    Just meta -> LByteString.writeFile (toolDir </> "metadata.json") (AesonPretty.encodePretty meta)
                    Nothing -> pure ()

        -- Write standalone tools
        createDirectoryIfMissing True (exportDir </> toolsDirName)
        forM_ pkg.packageTools $ \tool -> do
            let toolDir = exportDir </> toolsDirName </> Text.unpack (scriptSlug tool.standaloneToolInfo)
            createDirectoryIfMissing True toolDir
            let scriptPath = toolDir </> "script"
            ByteString.writeFile scriptPath tool.standaloneToolScript
            setFileMode scriptPath tool.standaloneToolPermissions
            LByteString.writeFile (toolDir </> "metadata.json") $ AesonPretty.encodePretty tool.standaloneToolInfo
            -- Write aux files
            forM_ tool.standaloneToolAuxFiles $ \(auxPath, auxContent) -> do
                let fullAuxPath = toolDir </> auxPath
                createDirectoryIfMissing True (takeDirectory fullAuxPath)
                ByteString.writeFile fullAuxPath auxContent

        -- Write MCP servers
        createDirectoryIfMissing True (exportDir </> mcpServersDirName)
        forM_ pkg.packageMcpServers $ \mcp -> do
            let mcpName = case mcp.mcpConfig of
                    McpSimpleBinary cfg -> Text.unpack cfg.name
            let mcpFile = exportDir </> mcpServersDirName </> mcpName <.> "json"
            LByteString.writeFile mcpFile $ AesonPretty.encodePretty mcp.mcpConfig

        -- Create archive from export directory
        case fmt of
            TarFormat -> createTarArchive exportDir path
            TarGzFormat -> createTarGzArchive exportDir path
            ZipFormat -> createTarGzArchive exportDir (path ++ ".tar.gz") -- Fall back to tar.gz
        pure $ Right ()

    case result of
        Left e -> pure $ Left $ FileIOError path e
        Right r -> pure r

-- | Export tools to an archive file
exportToolsToArchive :: [StandaloneToolExport] -> ArchiveFormat -> FilePath -> IO (Either ImportError ())
exportToolsToArchive tools fmt path = do
    now <- getCurrentTime
    let metadata =
            PackageMetadata
                { packageVersion = exportSchemaVersion
                , packageCreatedAt = now
                , packageDescription = Just "Tool package"
                , packageSource = Nothing
                }
    let pkg =
            ExportPackage
                { packageMetadata = metadata
                , packageAgents = []
                , packageTools = tools
                , packageMcpServers = []
                }
    exportToArchive pkg fmt path

-------------------------------------------------------------------------------
-- Import Operations
-------------------------------------------------------------------------------

-- | Import a package from an archive file
importFromArchive :: FilePath -> IO (Either ImportError ExportPackage)
importFromArchive path = do
    case detectArchiveFormat path of
        Nothing -> pure $ Left $ ArchiveFormatError $ "Cannot detect format for: " ++ path
        Just fmt -> do
            result <- try $ withSystemTempDirectory "agents-import-" $ \tmpDir -> do
                -- Extract archive to temp directory
                let extractDir = tmpDir </> "extracted"
                createDirectoryIfMissing True extractDir

                case fmt of
                    TarFormat -> extractTarArchive path extractDir
                    TarGzFormat -> extractTarGzArchive path extractDir
                    ZipFormat -> extractTarGzArchive path extractDir -- Try tar.gz

                -- Find the export root directory (may be nested)
                exportRoot <- findExportRoot extractDir

                -- Parse manifest
                manifestPath <- findManifest exportRoot
                manifestBytes <- LByteString.readFile manifestPath
                case Aeson.eitherDecode manifestBytes of
                    Left err -> pure $ Left $ PackageParseError $ "Failed to parse manifest: " ++ err
                    Right metadata -> do
                        -- Load agents
                        agentExports <- loadAgentsFromDir (exportRoot </> agentsDirName)

                        -- Load standalone tools
                        standaloneTools <- loadToolsFromDir (exportRoot </> toolsDirName)

                        -- Load MCP servers
                        mcpServers0 <- loadMcpServersFromDir (exportRoot </> mcpServersDirName)

                        pure $
                            Right $
                                ExportPackage
                                    { packageMetadata = metadata
                                    , packageAgents = agentExports
                                    , packageTools = standaloneTools
                                    , packageMcpServers = mcpServers0
                                    }

            case result of
                Left (e :: IOException) -> pure $ Left $ FileIOError path e
                Right r -> pure r

-- | Import tools from an archive file
importToolsFromArchive :: FilePath -> IO (Either ImportError ToolPackage)
importToolsFromArchive path = do
    ePkg <- importFromArchive path
    case ePkg of
        Left err -> pure $ Left err
        Right pkg ->
            pure $
                Right $
                    ToolPackage
                        { toolPackageMetadata = pkg.packageMetadata
                        , toolPackageTools = pkg.packageTools
                        }

-------------------------------------------------------------------------------
-- Internal Helper Functions
-------------------------------------------------------------------------------

agentFileName :: AgentExport -> FilePath
agentFileName agent =
    case agent.agentNamespace of
        Nothing -> Text.unpack (slug $ agentConfig agent) <.> "json"
        Just ns -> Text.unpack ns <.> "json"

createTarArchive :: FilePath -> FilePath -> IO ()
createTarArchive sourceDir outputPath = do
    entries <- Tar.pack (takeDirectory sourceDir) [takeFileName sourceDir]
    LByteString.writeFile outputPath $ Tar.write entries

createTarGzArchive :: FilePath -> FilePath -> IO ()
createTarGzArchive sourceDir outputPath = do
    entries <- Tar.pack (takeDirectory sourceDir) [takeFileName sourceDir]
    LByteString.writeFile outputPath $ GZip.compress $ Tar.write entries

extractTarArchive :: FilePath -> FilePath -> IO ()
extractTarArchive archivePath destDir = do
    content <- LByteString.readFile archivePath
    let entries = Tar.read content
    Tar.unpack destDir entries

extractTarGzArchive :: FilePath -> FilePath -> IO ()
extractTarGzArchive archivePath destDir = do
    content <- LByteString.readFile archivePath
    let decompressed = GZip.decompress content
    let entries = Tar.read decompressed
    Tar.unpack destDir entries

findExportRoot :: FilePath -> IO FilePath
findExportRoot extractDir = do
    contents <- listDirectory extractDir
    -- Check if there's a single subdirectory that might be the export root
    case contents of
        [singleEntry] -> do
            let singlePath = extractDir </> singleEntry
            isDir <- doesDirectoryExist singlePath
            if isDir
                then pure singlePath
                else pure extractDir
        _ -> pure extractDir

findManifest :: FilePath -> IO FilePath
findManifest exportRoot = do
    let directPath = exportRoot </> manifestFileName
    exists <- doesFileExist directPath
    if exists
        then pure directPath
        else fail $ "Manifest not found at: " ++ directPath

loadAgentsFromDir :: FilePath -> IO [AgentExport]
loadAgentsFromDir agentsDir = do
    exists <- doesDirectoryExist agentsDir
    if not exists
        then pure []
        else do
            jsonFiles <- filter (".json" `isSuffixOf`) <$> listDirectoryRecursive agentsDir
            fmap concat $ traverse loadAgentFile jsonFiles
  where
    loadAgentFile :: FilePath -> IO [AgentExport]
    loadAgentFile path = do
        content <- LByteString.readFile path
        case Aeson.eitherDecode content of
            Left err -> do
                putStrLn $ "Warning: Failed to parse agent file " ++ path ++ ": " ++ err
                pure []
            Right agent -> do
                let ns = extractNamespaceFromPath agentsDir path
                -- Load tools for this agent
                tools <- loadAgentTools (takeDirectory path) agent
                pure
                    [ AgentExport
                        { agentConfig = agent
                        , agentNamespace = ns
                        , agentTools = tools
                        }
                    ]

    extractNamespaceFromPath :: FilePath -> FilePath -> Maybe Text
    extractNamespaceFromPath basePath filePath =
        let relativePath = makeRelative basePath filePath
            dirParts = filter (not . null) $ splitDirectories (takeDirectory relativePath)
            fileName = takeFileName filePath
            baseName =
                if ".json" `isSuffixOf` fileName
                    then take (length fileName - 5) fileName
                    else fileName
            allParts = dirParts ++ [baseName]
         in if null allParts
                then Nothing
                else Just $ Text.intercalate "." $ map Text.pack allParts

loadAgentTools :: FilePath -> Agent -> IO [ToolExport]
loadAgentTools agentDir _agent = do
    let toolDir = agentDir </> toolsDirName
    exists <- doesDirectoryExist toolDir
    if not exists
        then pure []
        else do
            entries <- listDirectory toolDir
            fmap concat $ traverse (loadTool toolDir) entries
  where
    loadTool :: FilePath -> FilePath -> IO [ToolExport]
    loadTool baseDir entryName = do
        let fullPath = baseDir </> entryName
        isDir <- doesDirectoryExist fullPath
        if isDir
            then loadToolFromDir fullPath entryName
            else do
                -- Check if it's an executable file
                isExec <- isExecutable fullPath
                if isExec
                    then do
                        content <- ByteString.readFile fullPath
                        perms <- getFileMode fullPath
                        pure
                            [ ToolExport
                                { toolName = Text.pack entryName
                                , toolContent = content
                                , toolPermissions = perms
                                , toolMetadata = Nothing
                                , toolNamespace = Nothing
                                }
                            ]
                    else pure []

    loadToolFromDir :: FilePath -> FilePath -> IO [ToolExport]
    loadToolFromDir dir name0 = do
        let scriptPath = dir </> "script"
        scriptExists <- doesFileExist scriptPath
        if scriptExists
            then do
                content <- ByteString.readFile scriptPath
                perms <- getFileMode scriptPath
                -- Try to load metadata
                let metaPath = dir </> "metadata.json"
                metaExists <- doesFileExist metaPath
                metadata <-
                    if metaExists
                        then do
                            metaBytes <- LByteString.readFile metaPath
                            case Aeson.eitherDecode metaBytes of
                                Left _ -> pure Nothing
                                Right info -> pure (Just info)
                        else pure Nothing
                pure
                    [ ToolExport
                        { toolName = Text.pack name0
                        , toolContent = content
                        , toolPermissions = perms
                        , toolMetadata = metadata
                        , toolNamespace = Nothing
                        }
                    ]
            else do
                -- Try to find an executable in the directory
                entries <- listDirectory dir
                execs <- filterM (isExecutable . (dir </>)) entries
                case execs of
                    (firstExec : _) -> do
                        content <- ByteString.readFile (dir </> firstExec)
                        perms <- getFileMode (dir </> firstExec)
                        pure
                            [ ToolExport
                                { toolName = Text.pack name0
                                , toolContent = content
                                , toolPermissions = perms
                                , toolMetadata = Nothing
                                , toolNamespace = Nothing
                                }
                            ]
                    _ -> pure []

loadToolsFromDir :: FilePath -> IO [StandaloneToolExport]
loadToolsFromDir toolsDir = do
    exists <- doesDirectoryExist toolsDir
    if not exists
        then pure []
        else do
            entries <- listDirectory toolsDir
            fmap concat $ traverse loadStandaloneTool entries
  where
    loadStandaloneTool :: FilePath -> IO [StandaloneToolExport]
    loadStandaloneTool entryName = do
        let toolDir = toolsDir </> entryName
        isDir <- doesDirectoryExist toolDir
        if not isDir
            then pure []
            else do
                let scriptPath = toolDir </> "script"
                let metaPath = toolDir </> "metadata.json"
                scriptExists <- doesFileExist scriptPath
                metaExists <- doesFileExist metaPath
                if not (scriptExists && metaExists)
                    then pure []
                    else do
                        script <- ByteString.readFile scriptPath
                        perms <- getFileMode scriptPath
                        metaBytes <- LByteString.readFile metaPath
                        case Aeson.eitherDecode metaBytes of
                            Left err -> do
                                putStrLn $ "Warning: Failed to parse tool metadata " ++ metaPath ++ ": " ++ err
                                pure []
                            Right info -> do
                                -- Load aux files
                                auxFiles <- loadAuxFiles toolDir
                                pure
                                    [ StandaloneToolExport
                                        { standaloneToolInfo = info
                                        , standaloneToolScript = script
                                        , standaloneToolPermissions = perms
                                        , standaloneToolAuxFiles = auxFiles
                                        }
                                    ]

    loadAuxFiles :: FilePath -> IO [(FilePath, ByteString.ByteString)]
    loadAuxFiles toolDir = do
        let auxDir = toolDir </> "aux"
        exists <- doesDirectoryExist auxDir
        if not exists
            then pure []
            else do
                files <- listDirectoryRecursive auxDir
                fmap concat $
                    traverse
                        ( \f -> do
                            let fullPath = auxDir </> f
                            isFile <- doesFileExist fullPath
                            if isFile
                                then do
                                    content <- ByteString.readFile fullPath
                                    pure [(f, content)]
                                else pure []
                        )
                        files

loadMcpServersFromDir :: FilePath -> IO [McpServerExport]
loadMcpServersFromDir mcpDir = do
    exists <- doesDirectoryExist mcpDir
    if not exists
        then pure []
        else do
            jsonFiles <- filter (".json" `isSuffixOf`) <$> listDirectory mcpDir
            fmap concat $ traverse loadMcpFile jsonFiles
  where
    loadMcpFile :: FilePath -> IO [McpServerExport]
    loadMcpFile fileName = do
        let path = mcpDir </> fileName
        content <- LByteString.readFile path
        case Aeson.eitherDecode content of
            Left err -> do
                putStrLn $ "Warning: Failed to parse MCP server file " ++ path ++ ": " ++ err
                pure []
            Right mcp ->
                pure
                    [ McpServerExport
                        { mcpConfig = mcp
                        , mcpNamespace = Nothing
                        }
                    ]

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    entries <- listDirectory dir
    fmap concat $
        traverse
            ( \entry -> do
                let fullPath = dir </> entry
                isDir <- doesDirectoryExist fullPath
                if isDir
                    then do
                        subEntries <- listDirectoryRecursive fullPath
                        pure $ map (entry </>) subEntries
                    else pure [entry]
            )
            entries

isExecutable :: FilePath -> IO Bool
isExecutable path = do
    exists <- doesFileExist path
    if not exists
        then pure False
        else do
            perms <- getFileMode path
            -- Check if owner has execute permission
            pure $ (perms `unionFileModes` ownerExecuteMode) == perms

getFileMode :: FilePath -> IO FileMode
getFileMode path = do
    status <- getFileStatus path
    pure $ fileMode status

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative base path =
    let baseParts = splitDirectories (normalise base)
        pathParts = splitDirectories (normalise path)
        go [] ys = joinPath ys
        go xs [] = joinPath xs
        go (x : xs) (y : ys)
            | x == y = go xs ys
            | otherwise = joinPath (x : xs ++ y : ys)
     in go baseParts pathParts

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\x -> liftM2 (\b -> if b then (x :) else id) (p x)) (return [])

liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do
    x1 <- m1
    x2 <- m2
    return (f x1 x2)
