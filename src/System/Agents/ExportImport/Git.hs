{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.ExportImport.Git where

import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , listDirectory
    , withCurrentDirectory
    )
import System.Exit (ExitCode(..))
import System.FilePath
    ( (</>)
    , (<.>)
    , takeDirectory
    )
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode, callProcess)

import System.Agents.Base (Agent(..), McpServerDescription(..), McpSimpleBinaryConfiguration(..))
import System.Agents.ExportImport.Types
import System.Agents.Tools.Bash (ScriptInfo(..))
import qualified System.Agents.ExportImport.Archive as Archive

-------------------------------------------------------------------------------
-- Git Operations
-------------------------------------------------------------------------------

-- | Export a package to a git repository
exportToGit :: ExportPackage -> GitUrl -> GitExportOptions -> IO (Either GitError ())
exportToGit pkg gitUrl opts = do
    gitAvailable <- checkGitAvailable
    if not gitAvailable
        then pure $ Left GitNotFoundError
        else do
            result <- try $ withSystemTempDirectory "agents-git-export-" $ \tmpDir -> do
                let repoDir = tmpDir </> "repo"
                
                -- Clone the repository (or create if doesn't exist)
                cloneResult <- cloneOrInitRepo gitUrl repoDir
                case cloneResult of
                    Left err -> pure $ Left err
                    Right () -> do
                        -- Switch to the target branch if specified
                        _ <- case gitBranch gitUrl of
                            Just branch -> do
                                branchResult <- checkoutBranch repoDir branch
                                case branchResult of
                                    Left err -> pure $ Left err
                                    Right () -> pure $ Right ()
                            Nothing -> pure $ Right ()
                        
                        -- Determine the export path within the repo
                        let exportBase = case gitPath gitUrl of
                                Just ns -> repoDir </> namespaceToPath ns
                                Nothing -> repoDir
                        
                        -- Create directory structure
                        createDirectoryIfMissing True exportBase
                        
                        -- Write agents
                        forM_ (packageAgents pkg) $ \agent -> do
                            -- Get the namespace parts from the agentNamespace (which is Maybe Text)
                            let nsParts = case agentNamespace agent of
                                    Just nsText -> unNamespace $ case parseNamespace nsText of
                                        Right ns -> ns
                                        Left _ -> Namespace [slug $ agentConfig agent]
                                    Nothing -> [slug $ agentConfig agent]
                            let agentDir = exportBase </> "agents" </> namespaceToPath (Namespace nsParts)
                            createDirectoryIfMissing True agentDir
                            let agentFile = agentDir </> "agent.json"
                            LByteString.writeFile agentFile $ AesonPretty.encodePretty (agentConfig agent)
                            
                            -- Write agent tools
                            forM_ (agentTools agent) $ \tool -> do
                                let toolDir = agentDir </> "tools" </> Text.unpack (toolName tool)
                                createDirectoryIfMissing True toolDir
                                let scriptPath = toolDir </> "script"
                                ByteString.writeFile scriptPath (toolContent tool)
                                setFileMode scriptPath (toolPermissions tool)
                                case toolMetadata tool of
                                    Just meta -> LByteString.writeFile (toolDir </> "metadata.json") (AesonPretty.encodePretty meta)
                                    Nothing -> pure ()
                        
                        -- Write standalone tools
                        createDirectoryIfMissing True (exportBase </> "tools")
                        forM_ (packageTools pkg) $ \tool -> do
                            let toolDir = exportBase </> "tools" </> Text.unpack (scriptSlug $ standaloneToolInfo tool)
                            createDirectoryIfMissing True toolDir
                            let scriptPath = toolDir </> "script"
                            ByteString.writeFile scriptPath (standaloneToolScript tool)
                            setFileMode scriptPath (standaloneToolPermissions tool)
                            LByteString.writeFile (toolDir </> "metadata.json") $ AesonPretty.encodePretty (standaloneToolInfo tool)
                            -- Write aux files
                            forM_ (standaloneToolAuxFiles tool) $ \(auxPath, auxContent) -> do
                                let fullAuxPath = toolDir </> "aux" </> auxPath
                                createDirectoryIfMissing True (takeDirectory fullAuxPath)
                                ByteString.writeFile fullAuxPath auxContent
                        
                        -- Write MCP servers
                        createDirectoryIfMissing True (exportBase </> "mcp-servers")
                        forM_ (packageMcpServers pkg) $ \mcp -> do
                            let mcpName = case mcp.mcpConfig of
                                    McpSimpleBinary cfg -> Text.unpack cfg.name
                            let mcpFile = exportBase </> "mcp-servers" </> mcpName <.> "json"
                            LByteString.writeFile mcpFile $ AesonPretty.encodePretty mcp.mcpConfig
                        
                        -- Write/update index file
                        updateIndexFile exportBase pkg
                        
                        -- Git add all changes
                        gitAddAll repoDir
                        
                        -- Commit changes
                        commitResult <- gitCommit repoDir (gitCommitMessage opts)
                        case commitResult of
                            Left err -> pure $ Left err
                            Right () -> do
                                -- Tag if specified
                                _ <- case gitTag opts of
                                    Just tag -> do
                                        tagResult <- gitTagRepo repoDir tag
                                        case tagResult of
                                            Left err -> pure $ Left err
                                            Right () -> pure $ Right ()
                                    Nothing -> pure $ Right ()
                                
                                -- Push if requested
                                if gitPush opts
                                    then gitPushRepo repoDir (fromMaybe "main" (gitBranch gitUrl))
                                    else pure $ Right ()
            
            case result of
                Left (e :: SomeException) -> pure $ Left $ GitCommitError $ show e
                Right r -> pure r

-- | Export tools to a git repository
exportToolsToGit :: [StandaloneToolExport] -> GitUrl -> GitExportOptions -> IO (Either GitError ())
exportToolsToGit tools gitUrl opts = do
    now <- getCurrentTime
    let metadata = PackageMetadata
            { packageVersion = exportSchemaVersion
            , packageCreatedAt = now
            , packageDescription = Just "Tool package"
            , packageSource = Nothing
            }
    let pkg = ExportPackage
            { packageMetadata = metadata
            , packageAgents = []
            , packageTools = tools
            , packageMcpServers = []
            }
    exportToGit pkg gitUrl opts

-- | Import a package from a git repository
importFromGit :: GitUrl -> GitImportOptions -> IO (Either GitError ExportPackage)
importFromGit gitUrl opts = do
    gitAvailable <- checkGitAvailable
    if not gitAvailable
        then pure $ Left GitNotFoundError
        else do
            result <- try $ withSystemTempDirectory "agents-git-import-" $ \tmpDir -> do
                let repoDir = tmpDir </> "repo"
                
                -- Clone the repository
                cloneResult <- gitClone (gitRemote gitUrl) repoDir (gitRef opts)
                case cloneResult of
                    Left err -> pure $ Left err
                    Right () -> do
                        -- Checkout specific ref if provided
                        _ <- case gitRef opts of
                            Just ref -> do
                                checkoutResult <- gitCheckout repoDir ref
                                case checkoutResult of
                                    Left err -> pure $ Left err
                                    Right () -> pure $ Right ()
                            Nothing -> pure $ Right ()
                        
                        -- Determine the import path within the repo
                        let importBase = case gitPath gitUrl of
                                Just ns -> repoDir </> namespaceToPath ns
                                Nothing -> repoDir
                        
                        -- Check if directory exists
                        exists <- doesDirectoryExist importBase
                        if not exists
                            then pure $ Left $ GitNamespaceError $ "Path not found: " ++ importBase
                            else do
                                -- Load agents
                                agentExports <- loadAgentsFromGitDir (importBase </> "agents")
                                
                                -- Load standalone tools - convert ToolExport to StandaloneToolExport
                                toolExports <- loadToolsFromGitDir (importBase </> "tools")
                                let standaloneTools = map toolExportToStandalone toolExports
                                
                                -- Load MCP servers
                                mcpServers0 <- loadMcpServersFromGitDir (importBase </> "mcp-servers")
                                
                                now <- getCurrentTime
                                let metadata = PackageMetadata
                                        { packageVersion = exportSchemaVersion
                                        , packageCreatedAt = now
                                        , packageDescription = Nothing
                                        , packageSource = Just (gitRemote gitUrl)
                                        }
                                
                                pure $ Right $ ExportPackage
                                    { packageMetadata = metadata
                                    , packageAgents = agentExports
                                    , packageTools = standaloneTools
                                    , packageMcpServers = mcpServers0
                                    }
            
            case result of
                Left (e :: SomeException) -> pure $ Left $ GitCloneError $ show e
                Right r -> pure r

toolExportToStandalone :: ToolExport -> StandaloneToolExport
toolExportToStandalone te =
    StandaloneToolExport
        { standaloneToolInfo = case toolMetadata te of
            Just info0 -> info0
            Nothing -> ScriptInfo
                { scriptArgs = []
                , scriptSlug = toolName te
                , scriptDescription = "Imported tool"
                , scriptEmptyResultBehavior = Nothing
                }
        , standaloneToolScript = toolContent te
        , standaloneToolPermissions = toolPermissions te
        , standaloneToolAuxFiles = []
        }

-- | Import tools from a git repository
importToolsFromGit :: GitUrl -> Maybe Namespace -> IO (Either GitError ToolPackage)
importToolsFromGit gitUrl mbNamespace = do
    let opts = GitImportOptions
            { gitRef = Nothing
            , gitSparsePaths = maybe [] (:[]) mbNamespace
            }
    ePkg <- importFromGit gitUrl opts
    case ePkg of
        Left err -> pure $ Left err
        Right pkg ->
            pure $ Right $ ToolPackage
                { toolPackageMetadata = packageMetadata pkg
                , toolPackageTools = packageTools pkg
                }

-- | List available namespaces in a git repository
listGitNamespaces :: GitUrl -> IO (Either GitError [Namespace])
listGitNamespaces gitUrl = do
    gitAvailable <- checkGitAvailable
    if not gitAvailable
        then pure $ Left GitNotFoundError
        else do
            result <- try $ withSystemTempDirectory "agents-git-list-" $ \tmpDir -> do
                let repoDir = tmpDir </> "repo"
                
                -- Clone with minimal depth for listing
                cloneResult <- gitCloneShallow (gitRemote gitUrl) repoDir
                case cloneResult of
                    Left err -> pure $ Left err
                    Right () -> do
                        -- Determine search base
                        let searchBase = case gitPath gitUrl of
                                Just ns -> repoDir </> namespaceToPath ns
                                Nothing -> repoDir
                        
                        -- Find agent directories
                        agentNs <- findNamespacesInDir (searchBase </> "agents")
                        toolNs <- findNamespacesInDir (searchBase </> "tools")
                        
                        pure $ Right $ agentNs ++ toolNs
            
            case result of
                Left (e :: SomeException) -> pure $ Left $ GitCloneError $ show e
                Right r -> pure r

-- | List available tools in a git repository
listGitTools :: GitUrl -> IO (Either GitError [(Namespace, ScriptInfo)])
listGitTools gitUrl = do
    gitAvailable <- checkGitAvailable
    if not gitAvailable
        then pure $ Left GitNotFoundError
        else do
            result <- try $ withSystemTempDirectory "agents-git-list-tools-" $ \tmpDir -> do
                let repoDir = tmpDir </> "repo"
                
                cloneResult <- gitCloneShallow (gitRemote gitUrl) repoDir
                case cloneResult of
                    Left err -> pure $ Left err
                    Right () -> do
                        let toolsDir = case gitPath gitUrl of
                                Just ns -> repoDir </> namespaceToPath ns </> "tools"
                                Nothing -> repoDir </> "tools"
                        
                        tools <- listToolsInDir toolsDir []
                        pure $ Right tools
            
            case result of
                Left (e :: SomeException) -> pure $ Left $ GitCloneError $ show e
                Right r -> pure r

-------------------------------------------------------------------------------
-- Git Command Helpers
-------------------------------------------------------------------------------

checkGitAvailable :: IO Bool
checkGitAvailable = do
    (code, _, _) <- readProcessWithExitCode "git" ["--version"] ""
    pure $ code == ExitSuccess

cloneOrInitRepo :: GitUrl -> FilePath -> IO (Either GitError ())
cloneOrInitRepo gitUrl repoDir = do
    -- Try to clone first, if fails try to init
    (code, _, _err) <- readProcessWithExitCode "git" ["clone", Text.unpack (gitRemote gitUrl), repoDir] ""
    case code of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> do
            -- Try sparse clone or init fresh
            createDirectoryIfMissing True repoDir
            (initCode, _, initErr) <- readProcessWithExitCode "git" ["init", repoDir] ""
            case initCode of
                ExitSuccess -> do
                    -- Add remote
                    (remoteCode, _, remoteErr) <- withCurrentDirectory repoDir $ 
                        readProcessWithExitCode "git" ["remote", "add", "origin", Text.unpack (gitRemote gitUrl)] ""
                    case remoteCode of
                        ExitSuccess -> pure $ Right ()
                        ExitFailure _ -> pure $ Left $ GitCloneError remoteErr
                ExitFailure _ -> pure $ Left $ GitCloneError initErr

gitClone :: Text -> FilePath -> Maybe Text -> IO (Either GitError ())
gitClone remote repoDir mbRef = do
    let args0 = case mbRef of
            Just ref -> ["clone", "--branch", Text.unpack ref, "--single-branch", Text.unpack remote, repoDir]
            Nothing -> ["clone", Text.unpack remote, repoDir]
    (code, _, _err) <- readProcessWithExitCode "git" args0 ""
    case code of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> pure $ Left $ GitCloneError _err

gitCloneShallow :: Text -> FilePath -> IO (Either GitError ())
gitCloneShallow remote repoDir = do
    (code, _, _err) <- readProcessWithExitCode "git" ["clone", "--depth", "1", Text.unpack remote, repoDir] ""
    case code of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> pure $ Left $ GitCloneError _err

gitCheckout :: FilePath -> Text -> IO (Either GitError ())
gitCheckout repoDir ref = do
    (code, _, err) <- withCurrentDirectory repoDir $ 
        readProcessWithExitCode "git" ["checkout", Text.unpack ref] ""
    case code of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> pure $ Left $ GitCheckoutError err

checkoutBranch :: FilePath -> Text -> IO (Either GitError ())
checkoutBranch repoDir branch = do
    -- Check if branch exists
    (_, out, _) <- withCurrentDirectory repoDir $ 
        readProcessWithExitCode "git" ["branch", "-a"] ""
    if Text.unpack branch `elem` lines out
        then do
            (checkoutCode, _, checkoutErr) <- withCurrentDirectory repoDir $ 
                readProcessWithExitCode "git" ["checkout", Text.unpack branch] ""
            case checkoutCode of
                ExitSuccess -> pure $ Right ()
                ExitFailure _ -> pure $ Left $ GitCheckoutError checkoutErr
        else do
            -- Create new branch
            (createCode, _, createErr) <- withCurrentDirectory repoDir $ 
                readProcessWithExitCode "git" ["checkout", "-b", Text.unpack branch] ""
            case createCode of
                ExitSuccess -> pure $ Right ()
                ExitFailure _ -> pure $ Left $ GitCheckoutError createErr

gitAddAll :: FilePath -> IO ()
gitAddAll repoDir = do
    callProcess "git" ["-C", repoDir, "add", "-A"]

gitCommit :: FilePath -> Text -> IO (Either GitError ())
gitCommit repoDir message = do
    (code, _, err) <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", Text.unpack message] ""
    case code of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> 
            -- Check if there are no changes to commit
            if "nothing to commit" `isInfixOf` err
                then pure $ Right ()
                else pure $ Left $ GitCommitError err

gitTagRepo :: FilePath -> Text -> IO (Either GitError ())
gitTagRepo repoDir tag = do
    (code, _, err) <- readProcessWithExitCode "git" ["-C", repoDir, "tag", Text.unpack tag] ""
    case code of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> pure $ Left $ GitCommitError err

gitPushRepo :: FilePath -> Text -> IO (Either GitError ())
gitPushRepo repoDir branch = do
    (code, _, err) <- readProcessWithExitCode "git" ["-C", repoDir, "push", "origin", Text.unpack branch] ""
    case code of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ -> pure $ Left $ GitPushError err

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

-------------------------------------------------------------------------------
-- Index File Operations
-------------------------------------------------------------------------------

updateIndexFile :: FilePath -> ExportPackage -> IO ()
updateIndexFile baseDir pkg = do
    let indexPath = baseDir </> ".agents-index.json"
    let index = buildIndex pkg
    LByteString.writeFile indexPath $ AesonPretty.encodePretty index

data AgentIndex = AgentIndex
    { indexAgents :: [(Text, Maybe Text)]  -- (slug, namespace)
    , indexTools :: [(Text, Maybe Text)]   -- (slug, namespace)
    , indexMcpServers :: [Text]            -- names
    }
    deriving (Show, Generic)

instance Aeson.ToJSON AgentIndex where
    toJSON idx =
        Aeson.object
            [ "agents" Aeson..= map (\(s, ns) -> Aeson.object $ ["slug" Aeson..= s] ++ maybe [] (\n -> ["namespace" Aeson..= n]) ns) (indexAgents idx)
            , "tools" Aeson..= map (\(s, ns) -> Aeson.object $ ["slug" Aeson..= s] ++ maybe [] (\n -> ["namespace" Aeson..= n]) ns) (indexTools idx)
            , "mcp_servers" Aeson..= indexMcpServers idx
            ]

buildIndex :: ExportPackage -> AgentIndex
buildIndex pkg = AgentIndex
    { indexAgents = [(slug $ agentConfig a, agentNamespace a) | a <- packageAgents pkg]
    , indexTools = [(scriptSlug $ standaloneToolInfo t, Nothing) | t <- packageTools pkg]
    , indexMcpServers = [case mcpConfig m of McpSimpleBinary cfg -> cfg.name | m <- packageMcpServers pkg]
    }

-------------------------------------------------------------------------------
-- Loading from Git Directory Structure
-------------------------------------------------------------------------------

loadAgentsFromGitDir :: FilePath -> IO [AgentExport]
loadAgentsFromGitDir agentsDir = do
    exists <- doesDirectoryExist agentsDir
    if not exists
        then pure []
        else do
            entries <- listDirectory agentsDir
            fmap concat $ traverse (loadAgentFromGit agentsDir []) entries

loadAgentFromGit :: FilePath -> [Text] -> FilePath -> IO [AgentExport]
loadAgentFromGit baseDir nsParts entry = do
    let fullPath = baseDir </> entry
    isDir <- doesDirectoryExist fullPath
    if isDir
        then do
            -- Check for agent.json in this directory
            let agentFile = fullPath </> "agent.json"
            agentExists <- doesFileExist agentFile
            if agentExists
                then do
                    content <- LByteString.readFile agentFile
                    case Aeson.eitherDecode content of
                        Left err -> do
                            putStrLn $ "Warning: Failed to parse agent file " ++ agentFile ++ ": " ++ err
                            pure []
                        Right agent -> do
                            -- Load tools
                            let toolsDir = fullPath </> "tools"
                            tools <- loadToolsFromGitDir toolsDir
                            let namespace = if null nsParts then Nothing else Just $ Text.intercalate "." (nsParts ++ [Text.pack entry])
                            pure [AgentExport
                                { agentConfig = agent
                                , agentNamespace = namespace
                                , agentTools = tools
                                }]
                else do
                    -- Continue recursing
                    subEntries <- listDirectory fullPath
                    fmap concat $ traverse (loadAgentFromGit fullPath (nsParts ++ [Text.pack entry])) subEntries
        else pure []

loadToolsFromGitDir :: FilePath -> IO [ToolExport]
loadToolsFromGitDir toolsDir = do
    exists <- doesDirectoryExist toolsDir
    if not exists
        then pure []
        else do
            entries <- listDirectory toolsDir
            fmap concat $ traverse loadTool entries
  where
    loadTool :: FilePath -> IO [ToolExport]
    loadTool entryName = do
        let toolDir = toolsDir </> entryName
        isDir <- doesDirectoryExist toolDir
        if not isDir
            then pure []
            else do
                let scriptPath = toolDir </> "script"
                let metaPath = toolDir </> "metadata.json"
                scriptExists <- doesFileExist scriptPath
                if not scriptExists
                    then pure []
                    else do
                        content <- ByteString.readFile scriptPath
                        perms <- Archive.getFileMode scriptPath
                        metaExists' <- doesFileExist metaPath
                        metadata <- if metaExists'
                            then do
                                metaBytes <- LByteString.readFile metaPath
                                case Aeson.eitherDecode metaBytes of
                                    Left _ -> pure Nothing
                                    Right info0 -> pure (Just info0)
                            else pure Nothing
                        pure [ToolExport
                            { toolName = Text.pack entryName
                            , toolContent = content
                            , toolPermissions = perms
                            , toolMetadata = metadata
                            , toolNamespace = Nothing
                            }]

loadMcpServersFromGitDir :: FilePath -> IO [McpServerExport]
loadMcpServersFromGitDir mcpDir = do
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
            Right mcp -> pure [McpServerExport
                { mcpConfig = mcp
                , mcpNamespace = Nothing
                }]

findNamespacesInDir :: FilePath -> IO [Namespace]
findNamespacesInDir baseDir = do
    exists <- doesDirectoryExist baseDir
    if not exists
        then pure []
        else do
            entries <- listDirectory baseDir
            fmap concat $ traverse (findNsRecursive baseDir []) entries
  where
    findNsRecursive :: FilePath -> [Text] -> FilePath -> IO [Namespace]
    findNsRecursive dir parts entry = do
        let fullPath = dir </> entry
        isDir <- doesDirectoryExist fullPath
        if isDir
            then do
                let newParts = parts ++ [Text.pack entry]
                nested <- fmap concat $ traverse (findNsRecursive fullPath newParts) =<< listDirectory fullPath
                pure $ Namespace newParts : nested
            else pure []

listToolsInDir :: FilePath -> [Text] -> IO [(Namespace, ScriptInfo)]
listToolsInDir baseDir nsPrefix = do
    exists <- doesDirectoryExist baseDir
    if not exists
        then pure []
        else do
            entries <- listDirectory baseDir
            fmap concat $ traverse processEntry entries
  where
    processEntry :: FilePath -> IO [(Namespace, ScriptInfo)]
    processEntry entry = do
        let fullPath = baseDir </> entry
        isDir <- doesDirectoryExist fullPath
        if isDir
            then do
                -- Check if this is a tool directory
                let metaPath = fullPath </> "metadata.json"
                metaExists <- doesFileExist metaPath
                if metaExists
                    then do
                        metaBytes <- LByteString.readFile metaPath
                        case Aeson.eitherDecode metaBytes of
                            Left _ -> listToolsInDir fullPath (nsPrefix ++ [Text.pack entry])
                            Right info0 -> do
                                let ns = Namespace (nsPrefix ++ [Text.pack entry])
                                pure [(ns, info0)]
                    else listToolsInDir fullPath (nsPrefix ++ [Text.pack entry])
            else pure []

