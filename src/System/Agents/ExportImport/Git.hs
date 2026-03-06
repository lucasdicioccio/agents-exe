{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Git-based export/import operations for agent configurations.
--
-- This module provides functionality to export agent configurations to
-- and import from git repositories, supporting namespacing for multiple
-- team/organization structures within a single repository.
--
-- Required tools: git
module System.Agents.ExportImport.Git (
    -- * Types
    GitUrl(..),
    GitExportOptions(..),
    GitImportOptions(..),
    GitError(..),
    
    -- * Core Operations
    exportToGit,
    importFromGit,
    listGitNamespaces,
    
    -- * Namespace Resolution
    resolveAgentPath,
    resolveToolPath,
    resolveMcpServerPath,
    
    -- * Helper Functions
    defaultGitExportOptions,
    defaultGitImportOptions,
) where

import Control.Exception (try, SomeException)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (
    createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
    listDirectory, removeDirectoryRecursive, withCurrentDirectory
    )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

import System.Agents.Base (AgentDescription(..), Agent(..), McpServerDescription)
import System.Agents.ExportImport.Types
import qualified System.Agents.ExportImport.Git.Index as Index
import System.Agents.FileLoader.JSON (readJsonDescriptionFile)

-------------------------------------------------------------------------------
-- Git URL and Options Types
-------------------------------------------------------------------------------

-- | Git repository URL with optional branch and subpath.
data GitUrl = GitUrl
    { gitRemote :: Text              -- ^ Remote URL (HTTPS or SSH)
    , gitBranch :: Maybe Text        -- ^ Optional branch name
    , gitPath :: Maybe Namespace     -- ^ Optional subpath within repo
    }
    deriving (Show, Eq, Generic)

instance ToJSON GitUrl
instance FromJSON GitUrl

-- | Options for exporting to git.
data GitExportOptions = GitExportOptions
    { gitCommitMessage :: Text       -- ^ Commit message
    , gitTag :: Maybe Text           -- ^ Optional tag to create
    , gitPush :: Bool                -- ^ Whether to push after commit
    }
    deriving (Show, Eq, Generic)

instance ToJSON GitExportOptions
instance FromJSON GitExportOptions

-- | Options for importing from git.
data GitImportOptions = GitImportOptions
    { gitRef :: Maybe Text           -- ^ Branch, tag, or commit to checkout
    , gitSparsePaths :: [Namespace]  -- ^ Only import these namespaces
    }
    deriving (Show, Eq, Generic)

instance ToJSON GitImportOptions
instance FromJSON GitImportOptions

-- | Default export options.
defaultGitExportOptions :: GitExportOptions
defaultGitExportOptions = GitExportOptions
    { gitCommitMessage = "Update agent configurations"
    , gitTag = Nothing
    , gitPush = False
    }

-- | Default import options.
defaultGitImportOptions :: GitImportOptions
defaultGitImportOptions = GitImportOptions
    { gitRef = Nothing
    , gitSparsePaths = []
    }

-------------------------------------------------------------------------------
-- Error Types
-------------------------------------------------------------------------------

-- | Errors that can occur during git operations.
data GitError
    = GitCloneError String
    | GitCheckoutError String
    | GitCommitError String
    | GitPushError String
    | GitParseError String
    | GitNamespaceError String
    | GitFileError String String
    | GitExportError String
    | GitImportError String
    deriving (Show, Eq, Generic)

instance ToJSON GitError
instance FromJSON GitError

-------------------------------------------------------------------------------
-- Core Operations
-------------------------------------------------------------------------------

-- | Export a package to a git repository.
--
-- This function:
-- 1. Clones the repository (or uses an existing clone)
-- 2. Checks out the specified branch
-- 3. Writes all agents, tools, and MCP servers to the appropriate paths
-- 4. Updates the index file
-- 5. Commits all changes
-- 6. Optionally creates a tag
-- 7. Optionally pushes to remote
exportToGit :: ExportPackage -> GitUrl -> GitExportOptions -> IO (Either GitError ())
exportToGit pkg url opts = do
    withSystemTempDirectory "agents-git-export-" $ \tmpDir -> do
        -- Clone the repository
        cloneResult <- cloneRepository tmpDir url
        case cloneResult of
            Left err -> pure (Left err)
            Right repoPath -> do
                -- Checkout the specified branch/ref if provided
                checkoutResult <- checkoutRef repoPath (gitBranch url)
                case checkoutResult of
                    Left err -> pure (Left err)
                    Right _ -> do
                        -- Write the export package
                        writeResult <- writeExportPackage repoPath pkg
                        case writeResult of
                            Left err -> pure (Left err)
                            Right _ -> do
                                -- Commit and optionally push
                                commitAndPush repoPath opts

-- | Import from a git repository.
--
-- This function:
-- 1. Clones the repository
-- 2. Checks out the specified ref
-- 3. Reads the index file or scans the directory
-- 4. Filters by namespace if specified
-- 5. Reads all matching agents, tools, and MCP servers
importFromGit :: GitUrl -> GitImportOptions -> IO (Either GitError ExportPackage)
importFromGit url opts = do
    withSystemTempDirectory "agents-git-import-" $ \tmpDir -> do
        -- Clone the repository
        cloneResult <- cloneRepository tmpDir url
        case cloneResult of
            Left err -> pure (Left err)
            Right repoPath -> do
                -- Checkout the specified ref if provided
                checkoutResult <- checkoutRef repoPath (gitRef opts)
                case checkoutResult of
                    Left err -> pure (Left err)
                    Right _ -> do
                        -- Read the export package
                        readExportPackage repoPath (gitSparsePaths opts) (gitPath url)

-- | List available namespaces in a git repository.
--
-- Returns all agent and tool namespaces found in the repository.
listGitNamespaces :: GitUrl -> IO (Either GitError [Namespace])
listGitNamespaces url = do
    withSystemTempDirectory "agents-git-list-" $ \tmpDir -> do
        cloneResult <- cloneRepository tmpDir url
        case cloneResult of
            Left err -> pure (Left err)
            Right repoPath -> do
                -- Try to read the index first
                mIndex <- Index.readIndex repoPath
                case mIndex of
                    Just idx -> pure $ Right $ 
                        map Index.agentNs idx.idxAgents ++ 
                        map Index.toolNs idx.idxTools
                    Nothing -> do
                        -- Scan the directory structure
                        scanResult <- scanDirectoryStructure repoPath
                        case scanResult of
                            Left err -> pure (Left err)
                            Right ns -> pure (Right ns)

-------------------------------------------------------------------------------
-- Git Operations
-------------------------------------------------------------------------------

-- | Clone a git repository to a temporary directory.
cloneRepository :: FilePath -> GitUrl -> IO (Either GitError FilePath)
cloneRepository destDir url = do
    let repoName = Text.unpack $ fromMaybe "repo" $ 
            case Text.splitOn "/" (gitRemote url) of
                xs | not (null xs) -> Just (last xs)
                _ -> Nothing
    let repoPath = destDir </> repoName
    
    -- Remove .git suffix if present
    let cleanRepoName = if ".git" `Text.isSuffixOf` Text.pack repoName
                        then take (length repoName - 4) repoName
                        else repoName
    let finalRepoPath = destDir </> cleanRepoName
    
    let args = ["clone", Text.unpack (gitRemote url), cleanRepoName]
    
    (exitCode, stdout, stderr) <- readProcessWithExitCode "git" args ""
    
    case exitCode of
        ExitSuccess -> pure (Right finalRepoPath)
        ExitFailure code -> pure $ Left $ GitCloneError $ 
            "Failed to clone repository (exit code " ++ show code ++ "): " ++ 
            show stderr ++ " stdout: " ++ show stdout

-- | Checkout a specific ref (branch, tag, or commit).
checkoutRef :: FilePath -> Maybe Text -> IO (Either GitError ())
checkoutRef _ Nothing = pure (Right ())
checkoutRef repoPath (Just ref) = do
    let args = ["-C", repoPath, "checkout", Text.unpack ref]
    (exitCode, _, stderr) <- readProcessWithExitCode "git" args ""
    
    case exitCode of
        ExitSuccess -> pure (Right ())
        ExitFailure code -> pure $ Left $ GitCheckoutError $ 
            "Failed to checkout ref '" ++ Text.unpack ref ++ 
            "' (exit code " ++ show code ++ "): " ++ show stderr

-- | Configure git user for commits in a repository.
configureGitUser :: FilePath -> IO ()
configureGitUser repoPath = do
    _ <- readProcessWithExitCode "git" ["-C", repoPath, "config", "user.email", "agents-exe@localhost"] ""
    _ <- readProcessWithExitCode "git" ["-C", repoPath, "config", "user.name", "agents-exe"] ""
    pure ()

-- | Commit changes and optionally push to remote.
commitAndPush :: FilePath -> GitExportOptions -> IO (Either GitError ())
commitAndPush repoPath opts = do
    -- Configure git user
    configureGitUser repoPath
    
    -- Add all changes
    (addCode, _, addErr) <- readProcessWithExitCode "git" ["-C", repoPath, "add", "-A"] ""
    case addCode of
        ExitFailure code -> pure $ Left $ GitCommitError $ 
            "Failed to add files (exit code " ++ show code ++ "): " ++ show addErr
        ExitSuccess -> do
            -- Check if there are changes to commit
            (diffCode, diffOut, _) <- readProcessWithExitCode "git" 
                ["-C", repoPath, "diff", "--cached", "--quiet"] ""
            
            -- diff --quiet returns 0 if no changes, 1 if changes
            case diffCode of
                ExitSuccess -> pure (Right ())  -- No changes to commit
                ExitFailure _ -> do
                    -- Commit
                    let commitArgs = ["-C", repoPath, "commit", "-m", Text.unpack (gitCommitMessage opts)]
                    (commitCode, _, commitErr) <- readProcessWithExitCode "git" commitArgs ""
                    case commitCode of
                        ExitFailure code -> pure $ Left $ GitCommitError $ 
                            "Failed to commit (exit code " ++ show code ++ "): " ++ show commitErr
                        ExitSuccess -> do
                            -- Create tag if specified
                            tagResult <- case gitTag opts of
                                Nothing -> pure (Right ())
                                Just tag -> createTag repoPath tag
                            
                            case tagResult of
                                Left err -> pure (Left err)
                                Right _ -> do
                                    -- Push if requested
                                    if gitPush opts
                                        then pushToRemote repoPath
                                        else pure (Right ())

-- | Create a git tag.
createTag :: FilePath -> Text -> IO (Either GitError ())
createTag repoPath tag = do
    let args = ["-C", repoPath, "tag", Text.unpack tag]
    (exitCode, _, stderr) <- readProcessWithExitCode "git" args ""
    
    case exitCode of
        ExitSuccess -> pure (Right ())
        ExitFailure code -> pure $ Left $ GitCommitError $ 
            "Failed to create tag '" ++ Text.unpack tag ++ 
            "' (exit code " ++ show code ++ "): " ++ show stderr

-- | Push commits and tags to remote.
pushToRemote :: FilePath -> IO (Either GitError ())
pushToRemote repoPath = do
    let args = ["-C", repoPath, "push", "--follow-tags"]
    (exitCode, _, stderr) <- readProcessWithExitCode "git" args ""
    
    case exitCode of
        ExitSuccess -> pure (Right ())
        ExitFailure code -> pure $ Left $ GitPushError $ 
            "Failed to push to remote (exit code " ++ show code ++ "): " ++ show stderr

-------------------------------------------------------------------------------
-- File Operations
-------------------------------------------------------------------------------

-- | Write an export package to a repository.
writeExportPackage :: FilePath -> ExportPackage -> IO (Either GitError ())
writeExportPackage repoPath pkg = do
    -- Write agents
    agentResults <- forM (pkgAgents pkg) $ \(ns, AgentDescription agent) -> do
        let path = repoPath </> resolveAgentPath ns
        createDirectoryIfMissing True (takeDirectory path)
        result <- try $ writeFile path (show (Aeson.encode (AgentDescription agent)))
        case result of
            Left (e :: SomeException) -> 
                pure $ Left $ GitFileError path (show e)
            Right () -> pure (Right ())
    
    -- Write tools
    toolResults <- forM (pkgTools pkg) $ \(ns, toolExport) -> do
        let dirPath = repoPath </> resolveToolPath ns
        createDirectoryIfMissing True dirPath
        
        -- Write metadata file
        let metadataPath = dirPath </> "metadata.json"
        result <- try $ writeFile metadataPath (show (Aeson.encode toolExport))
        case result of
            Left (e :: SomeException) -> 
                pure $ Left $ GitFileError metadataPath (show e)
            Right () -> do
                -- Write tool files
                fileResults <- forM (toolFiles toolExport) $ \(relPath, content) -> do
                    let filePath = dirPath </> relPath
                    createDirectoryIfMissing True (takeDirectory filePath)
                    fileResult <- try $ Text.writeFile filePath content
                    case fileResult of
                        Left (e :: SomeException) -> 
                            pure $ Left $ GitFileError filePath (show e)
                        Right () -> pure (Right ())
                pure $ case lefts fileResults of
                    (x:_) -> Left x
                    [] -> Right ()
    
    -- Write MCP servers
    mcpResults <- forM (pkgMcpServers pkg) $ \(name, mcpServer) -> do
        let path = repoPath </> resolveMcpServerPath name
        createDirectoryIfMissing True (takeDirectory path)
        result <- try $ writeFile path (show (Aeson.encode mcpServer))
        case result of
            Left (e :: SomeException) -> 
                pure $ Left $ GitFileError path (show e)
            Right () -> pure (Right ())
    
    -- Write index file
    now <- getCurrentTime
    let index = Index.buildIndex now pkg
    let indexPath = repoPath </> indexFilePath
    indexResult <- try $ writeFile indexPath (show (Aeson.encode index))
    
    let allResults = concat [agentResults, toolResults, mcpResults, [indexResult']]
        indexResult' = case indexResult of
            Left (e :: SomeException) -> Left $ GitFileError indexPath (show e)
            Right () -> Right ()
    
    pure $ case lefts allResults of
        (GitFileError p e:_) -> Left $ GitExportError $ 
            "Failed to write file '" ++ p ++ "': " ++ e
        (e:_) -> Left e
        [] -> Right ()
  where
    lefts = foldr (\e acc -> case e of Left x -> x:acc; Right _ -> acc) []

-- | Read an export package from a repository.
readExportPackage :: FilePath -> [Namespace] -> Maybe Namespace -> IO (Either GitError ExportPackage)
readExportPackage repoPath sparseNamespaces mSubPath = do
    now <- getCurrentTime
    
    -- Determine the base path (may be a subpath within the repo)
    let basePath = case mSubPath of
            Nothing -> repoPath
            Just ns -> repoPath </> namespaceToPath ns
    
    -- Try to read the index file
    mIndex <- Index.readIndex basePath
    
    case mIndex of
        Just idx -> do
            -- Use the index
            let filteredIdx = if null sparseNamespaces 
                              then idx 
                              else Index.filterIndex idx sparseNamespaces
            readFromIndex basePath filteredIdx now
        Nothing -> do
            -- Scan the directory structure
            scanResult <- scanAndRead basePath sparseNamespaces now
            case scanResult of
                Left err -> pure (Left err)
                Right pkg -> pure (Right pkg)

-- | Read package from index.
readFromIndex :: FilePath -> Index.AgentsIndex -> UTCTime -> IO (Either GitError ExportPackage)
readFromIndex basePath idx now = do
    -- Read agents
    agentResults <- forM (Index.idxAgents idx) $ \entry -> do
        let path = basePath </> Index.agentPath entry
        result <- readJsonDescriptionFile path
        case result of
            Left err -> pure $ Left $ GitImportError $ 
                "Failed to read agent at '" ++ path ++ "': " ++ err
            Right desc -> pure $ Right (entry.agentNs, desc)
    
    -- Read tools (simplified - just check directory exists)
    toolResults <- forM (Index.idxTools idx) $ \entry -> do
        let dirPath = basePath </> Index.toolPath entry
        toolResult <- readToolExport dirPath
        case toolResult of
            Left err -> pure $ Left $ GitImportError err
            Right toolExp -> pure $ Right (entry.toolNs, toolExp)
    
    -- Read MCP servers
    mcpResults <- forM (Index.idxMcpServers idx) $ \entry -> do
        let path = basePath </> entry.mcpPath
        mcpResult <- try $ Aeson.eitherDecodeFileStrict' path
        case mcpResult of
            Left (e :: SomeException) -> 
                pure $ Left $ GitImportError $ 
                    "Failed to read MCP server '" ++ Text.unpack entry.mcpName ++ 
                    "': " ++ show e
            Right (Left err) -> pure $ Left $ GitImportError err
            Right (Right desc) -> pure $ Right (entry.mcpName, desc)
    
    let agents = rights agentResults
    let tools = rights toolResults
    let mcps = rights mcpResults
    
    pure $ Right $ ExportPackage
        { pkgAgents = agents
        , pkgTools = tools
        , pkgMcpServers = mcps
        , pkgMetadata = PackageMetadata
            { pkgName = "imported-from-git"
            , pkgVersion = idx.idxVersion
            , pkgDescription = Just $ "Imported from git repository, last updated: " <> Text.pack (show idx.idxLastUpdated)
            , pkgCreatedAt = now
            , pkgAuthor = Nothing
            }
        }
  where
    rights = foldr (\e acc -> case e of Right x -> x:acc; Left _ -> acc) []

-- | Read a tool export from a directory.
readToolExport :: FilePath -> IO (Either String ToolExport)
readToolExport dirPath = do
    exists <- doesDirectoryExist dirPath
    if not exists
        then pure $ Left $ "Tool directory not found: " ++ dirPath
        else do
            -- Read metadata file if it exists
            let metadataPath = dirPath </> "metadata.json"
            metadataExists <- doesFileExist metadataPath
            
            mMetadata <- if metadataExists
                then do
                    result <- Aeson.eitherDecodeFileStrict' metadataPath
                    case result of
                        Left _ -> pure Nothing
                        Right te -> pure (Just te)
                else pure Nothing
            
            -- Read all files in the directory
            files <- listDirectory dirPath
            fileContents <- forM files $ \f -> do
                let path = dirPath </> f
                isFile <- doesFileExist path
                if isFile
                    then do
                        content <- Text.readFile path
                        pure (Just (f, content))
                    else pure Nothing
            
            case mMetadata of
                Just te -> pure $ Right te { toolFiles = catMaybes fileContents }
                Nothing -> pure $ Left $ "No metadata.json found in " ++ dirPath

-- | Scan directory structure and read all agents/tools.
scanAndRead :: FilePath -> [Namespace] -> UTCTime -> IO (Either GitError ExportPackage)
scanAndRead basePath sparseNamespaces now = do
    -- Scan for agents
    agentsPath <- doesDirectoryExist (basePath </> "agents")
    agentResults <- if agentsPath
        then scanAgents (basePath </> "agents") sparseNamespaces
        else pure (Right [])
    
    -- Scan for tools
    toolsPath <- doesDirectoryExist (basePath </> "tools")
    toolResults <- if toolsPath
        then scanTools (basePath </> "tools") sparseNamespaces
        else pure (Right [])
    
    -- Scan for MCP servers
    mcpPath <- doesDirectoryExist (basePath </> "mcp-servers")
    mcpResults <- if mcpPath
        then scanMcpServers (basePath </> "mcp-servers")
        else pure (Right [])
    
    case (agentResults, toolResults, mcpResults) of
        (Right agents, Right tools, Right mcps) ->
            pure $ Right $ ExportPackage
                { pkgAgents = agents
                , pkgTools = tools
                , pkgMcpServers = mcps
                , pkgMetadata = PackageMetadata
                    { pkgName = "scanned"
                    , pkgVersion = "1.0.0"
                    , pkgDescription = Just "Scanned from directory structure"
                    , pkgCreatedAt = now
                    , pkgAuthor = Nothing
                    }
                }
        (Left err, _, _) -> pure (Left err)
        (_, Left err, _) -> pure (Left err)
        (_, _, Left err) -> pure (Left err)

-- | Scan for agents in the agents directory.
scanAgents :: FilePath -> [Namespace] -> IO (Either GitError [(Namespace, AgentDescription)])
scanAgents agentsPath sparseNamespaces = do
    jsonFiles <- findJsonFiles agentsPath
    results <- forM jsonFiles $ \path -> do
        -- Convert path to namespace
        let relPath = drop (length agentsPath + 1) path
        let nsParts = map Text.pack $ filter (not . null) $ splitDirectories $ dropExtension relPath
        let ns = Namespace nsParts
        
        -- Check if namespace matches sparse filter
        let shouldInclude = null sparseNamespaces || 
                            any (\prefix -> prefix `namespaceIsPrefixOf` ns) sparseNamespaces
        
        if shouldInclude
            then do
                result <- readJsonDescriptionFile path
                case result of
                    Left err -> pure $ Left $ GitParseError $ 
                        "Failed to parse agent at '" ++ path ++ "': " ++ err
                    Right desc -> pure $ Right (Just (ns, desc))
            else pure (Right Nothing)
    
    let combined = foldr (\e acc -> case (e, acc) of
            (Left err, _) -> Left err
            (Right _, Left err) -> Left err
            (Right x, Right xs) -> Right (maybe id (:) x xs)) (Right []) results
    
    pure combined
  where
    dropExtension p = if ".json" `isSuffixOf` p
                      then take (length p - 5) p
                      else p
    splitDirectories = 
        let go [] = []
            go p = let (d, rest) = break (== '/') p
                   in d : go (drop 1 rest)
        in go
    isSuffixOf suffix str = 
        length suffix <= length str && 
        drop (length str - length suffix) str == suffix

-- | Scan for tools in the tools directory.
scanTools :: FilePath -> [Namespace] -> IO (Either GitError [(Namespace, ToolExport)])
scanTools toolsPath sparseNamespaces = do
    -- Find all directories with metadata.json
    dirs <- findToolDirectories toolsPath
    
    results <- forM dirs $ \dir -> do
        let relPath = drop (length toolsPath + 1) dir
        let nsParts = map Text.pack $ filter (not . null) $ splitDirectories relPath
        let ns = Namespace nsParts
        
        -- Check if namespace matches sparse filter
        let shouldInclude = null sparseNamespaces || 
                            any (\prefix -> prefix `namespaceIsPrefixOf` ns) sparseNamespaces
        
        if shouldInclude
            then do
                result <- readToolExport dir
                case result of
                    Left err -> pure $ Left $ GitParseError $ 
                        "Failed to read tool at '" ++ dir ++ "': " ++ err
                    Right toolExp -> pure $ Right (Just (ns, toolExp))
            else pure (Right Nothing)
    
    let combined = foldr (\e acc -> case (e, acc) of
            (Left err, _) -> Left err
            (Right _, Left err) -> Left err
            (Right x, Right xs) -> Right (maybe id (:) x xs)) (Right []) results
    
    pure combined
  where
    splitDirectories = 
        let go [] = []
            go p = let (d, rest) = break (== '/') p
                   in d : go (drop 1 rest)
        in go

-- | Scan for MCP servers in the mcp-servers directory.
scanMcpServers :: FilePath -> IO (Either GitError [(Text, McpServerDescription)])
scanMcpServers mcpPath = do
    jsonFiles <- findJsonFiles mcpPath
    results <- forM jsonFiles $ \path -> do
        let name = Text.pack $ takeFileName $ dropExtension path
        result <- try $ Aeson.eitherDecodeFileStrict' path
        case result of
            Left (e :: SomeException) -> 
                pure $ Left $ GitParseError $ 
                    "Failed to read MCP server at '" ++ path ++ "': " ++ show e
            Right (Left err) -> pure $ Left $ GitParseError err
            Right (Right desc) -> pure $ Right (name, desc)
    
    let combined = foldr (\e acc -> case (e, acc) of
            (Left err, _) -> Left err
            (Right _, Left err) -> Left err
            (Right x, Right xs) -> Right (x:xs)) (Right []) results
    
    pure combined
  where
    dropExtension p = if ".json" `isSuffixOf` p
                      then take (length p - 5) p
                      else p
    isSuffixOf suffix str = 
        length suffix <= length str && 
        drop (length str - length suffix) str == suffix

-- | Scan directory structure for namespaces.
scanDirectoryStructure :: FilePath -> IO (Either GitError [Namespace])
scanDirectoryStructure repoPath = do
    -- Scan agents
    agentsNs <- doesDirectoryExist (repoPath </> "agents")
    agentNamespaces <- if agentsNs
        then findNamespaces (repoPath </> "agents") ".json"
        else pure []
    
    -- Scan tools  
    toolsNs <- doesDirectoryExist (repoPath </> "tools")
    toolNamespaces <- if toolsNs
        then findNamespaces (repoPath </> "tools") ""
        else pure []
    
    pure $ Right (agentNamespaces ++ toolNamespaces)

-- | Find all JSON files recursively in a directory.
findJsonFiles :: FilePath -> IO [FilePath]
findJsonFiles path = do
    exists <- doesDirectoryExist path
    if not exists
        then pure []
        else do
            entries <- listDirectory path
            results <- forM entries $ \entry -> do
                let fullPath = path </> entry
                isDir <- doesDirectoryExist fullPath
                if isDir
                    then findJsonFiles fullPath
                    else if ".json" `isSuffixOf` entry
                        then pure [fullPath]
                        else pure []
            pure $ concat results
  where
    isSuffixOf suffix str = 
        length suffix <= length str && 
        drop (length str - length suffix) str == suffix

-- | Find all tool directories (directories containing metadata.json).
findToolDirectories :: FilePath -> IO [FilePath]
findToolDirectories path = do
    exists <- doesDirectoryExist path
    if not exists
        then pure []
        else do
            entries <- listDirectory path
            results <- forM entries $ \entry -> do
                let fullPath = path </> entry
                isDir <- doesDirectoryExist fullPath
                if isDir
                    then do
                        hasMetadata <- doesFileExist (fullPath </> "metadata.json")
                        if hasMetadata
                            then pure [fullPath]
                            else findToolDirectories fullPath
                    else pure []
            pure $ concat results

-- | Find namespaces from directory structure.
findNamespaces :: FilePath -> String -> IO [Namespace]
findNamespaces path suffix = do
    files <- if null suffix
        then findToolDirectories path
        else findJsonFiles path
    pure $ map (pathToNamespace path suffix) files

-- | Convert a file path to a namespace.
pathToNamespace :: FilePath -> String -> FilePath -> Namespace
pathToNamespace basePath suffix path =
    let relPath = drop (length basePath + 1) path
        withoutSuffix = if suffix `isSuffixOf` relPath
                        then take (length relPath - length suffix) relPath
                        else relPath
        parts = filter (not . null) $ splitDirectories withoutSuffix
    in Namespace (map Text.pack parts)
  where
    isSuffixOf s str = 
        length s <= length str && 
        drop (length str - length s) str == s
    splitDirectories = 
        let go [] = []
            go p = let (d, rest) = break (== '/') p
                   in d : go (drop 1 rest)
        in go

-------------------------------------------------------------------------------
-- Import with Filtering
-------------------------------------------------------------------------------

-- | Import only specific namespaces from a git repository.
importFiltered :: GitUrl -> [Namespace] -> IO (Either GitError ExportPackage)
importFiltered url namespaces =
    importFromGit url (defaultGitImportOptions { gitSparsePaths = namespaces })

