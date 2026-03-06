{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Git-based export/import operations for agents and tools.
module System.Agents.ExportImport.Git where

import Control.Exception (try, IOException)
import Control.Monad (forM, forM_, when)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.Encoding as Text
import Data.List (find, isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
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
    , takeBaseName
    , takeDirectory
    , takeFileName
    )
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Process.ByteString (readProcessWithExitCode)

import System.Agents.ExportImport.Types
import System.Agents.Tools.Bash (ScriptDescription, ScriptInfo(..))
import qualified System.Agents.Tools.Bash as Bash

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A git URL type for type safety.
newtype GitUrl = GitUrl Text
    deriving (Show, Eq, Ord)

-- | Options for git export operations.
data GitExportOptions = GitExportOptions
    { gitCommitMessage :: Text
    , gitAuthorName :: Maybe Text
    , gitAuthorEmail :: Maybe Text
    , gitBranch :: Maybe Text
    }
    deriving (Show, Eq)

-- | Default git export options.
defaultGitExportOptions :: GitExportOptions
defaultGitExportOptions = GitExportOptions
    { gitCommitMessage = "Update tools via agents-exe"
    , gitAuthorName = Nothing
    , gitAuthorEmail = Nothing
    , gitBranch = Nothing
    }

-- | Entry for listing tools in git.
data GitToolListing = GitToolListing
    { gitListingNamespace :: Namespace
    , gitListingInfo :: ScriptInfo
    }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Git Tool Operations
-------------------------------------------------------------------------------

-- | Push tools to a git repository.
exportToolsToGit :: [StandaloneToolExport] -> GitUrl -> GitExportOptions -> IO (Either GitError ())
exportToolsToGit tools gitUrl opts = do
    result <- try @IOException $ do
        -- Create a temporary directory
        tmpDir <- createTempDirectory "agents-git-export-"
        
        -- Clone the repository
        let GitUrl url = gitUrl
        (cloneCode, _, cloneErr) <- readProcessWithExitCode "git" ["clone", Text.unpack url, tmpDir] ""
        when (cloneCode /= ExitSuccess) $ error $ "Clone failed: " <> Text.unpack (Text.decodeUtf8 cloneErr)
        
        -- Create or update tools directory
        let toolsDir = tmpDir </> "tools"
        createDirectoryIfMissing True toolsDir
        
        -- Write each tool
        forM_ tools $ \tool -> do
            let slug = Bash.scriptSlug (standaloneToolInfo tool)
                toolDir = toolsDir </> Text.unpack slug
            
            -- Create tool directory
            createDirectoryIfMissing True toolDir
            
            -- Write script
            let scriptPath = toolDir </> "script"
            ByteString.writeFile scriptPath tool.standaloneToolScript
            setFileMode scriptPath tool.standaloneToolPermissions
            
            -- Write metadata
            let metadataPath = toolDir </> "metadata.json"
            LByteString.writeFile metadataPath (Aeson.encode tool.standaloneToolInfo)
            
            -- Write aux files
            forM_ tool.standaloneToolAuxFiles $ \(relPath, content) -> do
                let auxPath = toolDir </> "aux" </> relPath
                createDirectoryIfMissing True (takeDirectory auxPath)
                ByteString.writeFile auxPath content
        
        -- Configure git if author info provided
        forM_ opts.gitAuthorName $ \name -> do
            _ <- readProcessWithExitCode "git" ["-C", tmpDir, "config", "user.name", Text.unpack name] ""
            pure ()
        forM_ opts.gitAuthorEmail $ \email -> do
            _ <- readProcessWithExitCode "git" ["-C", tmpDir, "config", "user.email", Text.unpack email] ""
            pure ()
        
        -- Add, commit, and push
        (addCode, _, addErr) <- readProcessWithExitCode "git" ["-C", tmpDir, "add", "."] ""
        when (addCode /= ExitSuccess) $ error $ "Add failed: " <> Text.unpack (Text.decodeUtf8 addErr)
        
        (commitCode, _, commitErr) <- readProcessWithExitCode "git"
            ["-C", tmpDir, "commit", "-m", Text.unpack opts.gitCommitMessage] ""
        -- Commit may fail if nothing to commit, that's ok
        
        when (commitCode == ExitSuccess) $ do
            let pushArgs = ["-C", tmpDir, "push"]
                         <> maybe [] (\b -> ["origin", Text.unpack b]) opts.gitBranch
            (pushCode, _, pushErr) <- readProcessWithExitCode "git" pushArgs ""
            when (pushCode /= ExitSuccess) $ error $ "Push failed: " <> Text.unpack (Text.decodeUtf8 pushErr)
        
        -- Cleanup
        removeDirectoryRecursive tmpDir
        
        pure ()
    
    case result of
        Left e -> pure $ Left $ GitIOError e
        Right () -> pure $ Right ()

-- | Import tools from git, optionally filtered by namespace.
importToolsFromGit :: GitUrl -> Maybe Namespace -> IO (Either GitError ToolPackage)
importToolsFromGit gitUrl mNamespace = do
    result <- try @IOException $ do
        -- Create a temporary directory
        tmpDir <- createTempDirectory "agents-git-import-"
        
        -- Clone the repository
        let GitUrl urlText = gitUrl
            url = Text.unpack urlText
        (cloneCode, _, cloneErr) <- readProcessWithExitCode "git" ["clone", url, tmpDir] ""
        when (cloneCode /= ExitSuccess) $ error $ "Clone failed: " <> Text.unpack (Text.decodeUtf8 cloneErr)
        
        -- Find and load tools
        let toolsDir = tmpDir </> "tools"
        toolsExist <- doesDirectoryExist toolsDir
        
        tools <- if toolsExist
            then loadToolsFromDirectory toolsDir mNamespace
            else pure []
        
        -- Create package metadata
        now <- getCurrentTime
        let metadata = PackageMetadata
                { packageVersion = "1.0.0"
                , packageCreatedAt = now
                , packageDescription = Just $ "Imported from git: " <> urlText
                }
        
        -- Cleanup
        removeDirectoryRecursive tmpDir
        
        pure $ ToolPackage
            { toolPackageMetadata = metadata
            , toolPackageTools = tools
            }
    
    case result of
        Left e -> pure $ Left $ GitIOError e
        Right pkg -> pure $ Right pkg

-- | List available tools in a git repository (no agents).
listGitTools :: GitUrl -> IO (Either GitError [(Namespace, ScriptInfo)])
listGitTools gitUrl = do
    result <- try @IOException $ do
        -- Create a temporary directory
        tmpDir <- createTempDirectory "agents-git-list-"
        
        -- Clone the repository
        let GitUrl url = gitUrl
        (cloneCode, _, cloneErr) <- readProcessWithExitCode "git" ["clone", Text.unpack url, tmpDir] ""
        when (cloneCode /= ExitSuccess) $ error $ "Clone failed: " <> Text.unpack (Text.decodeUtf8 cloneErr)
        
        -- List tools
        let toolsDir = tmpDir </> "tools"
        toolsExist <- doesDirectoryExist toolsDir
        
        listings <- if toolsExist
            then listToolsInDirectory toolsDir
            else pure []
        
        -- Cleanup
        removeDirectoryRecursive tmpDir
        
        pure listings
    
    case result of
        Left e -> pure $ Left $ GitIOError e
        Right listings -> pure $ Right listings

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Load tools from a directory, optionally filtered by namespace.
loadToolsFromDirectory :: FilePath -> Maybe Namespace -> IO [StandaloneToolExport]
loadToolsFromDirectory toolsDir mNamespace = do
    entries <- listDirectory toolsDir
    catMaybes <$> mapM loadTool entries
  where
    loadTool :: FilePath -> IO (Maybe StandaloneToolExport)
    loadTool entry = do
        let toolDir = toolsDir </> entry
        isDir <- doesDirectoryExist toolDir
        if not isDir
            then pure Nothing
            else do
                let scriptPath = toolDir </> "script"
                    metadataPath = toolDir </> "metadata.json"
                    auxDir = toolDir </> "aux"
                
                scriptExists <- doesFileExist scriptPath
                metadataExists <- doesFileExist metadataPath
                
                if not (scriptExists && metadataExists)
                    then pure Nothing
                    else do
                        -- Read script
                        scriptContent <- ByteString.readFile scriptPath
                        permissions <- fileMode <$> getFileStatus scriptPath
                        
                        -- Read metadata
                        metadataBytes <- LByteString.readFile metadataPath
                        case Aeson.eitherDecode metadataBytes of
                            Left _ -> pure Nothing
                            Right scriptInfo -> do
                                -- Filter by namespace if specified
                                let shouldInclude = case mNamespace of
                                        Nothing -> True
                                        Just _ -> True  -- Namespace filtering would go here
                                
                                if not shouldInclude
                                    then pure Nothing
                                    else do
                                        -- Read aux files
                                        auxFiles <- readAuxFilesIfExist auxDir
                                        
                                        pure $ Just StandaloneToolExport
                                            { standaloneToolInfo = scriptInfo
                                            , standaloneToolScript = scriptContent
                                            , standaloneToolPermissions = permissions
                                            , standaloneToolAuxFiles = auxFiles
                                            }
    
    readAuxFilesIfExist :: FilePath -> IO [(FilePath, ByteString.ByteString)]
    readAuxFilesIfExist auxDir = do
        exists <- doesDirectoryExist auxDir
        if not exists
            then pure []
            else readAuxFilesRecursive auxDir

-- | List tools in a directory with their namespaces.
listToolsInDirectory :: FilePath -> IO [(Namespace, ScriptInfo)]
listToolsInDirectory toolsDir = do
    entries <- listDirectory toolsDir
    catMaybes <$> mapM listTool entries
  where
    listTool :: FilePath -> IO (Maybe (Namespace, ScriptInfo))
    listTool entry = do
        let toolDir = toolsDir </> entry
            metadataPath = toolDir </> "metadata.json"
        
        metadataExists <- doesFileExist metadataPath
        if not metadataExists
            then pure Nothing
            else do
                metadataBytes <- LByteString.readFile metadataPath
                case Aeson.eitherDecode metadataBytes of
                    Left _ -> pure Nothing
                    Right scriptInfo -> do
                        let namespace = Namespace $ Text.pack entry
                        pure $ Just (namespace, scriptInfo)

-- | Read auxiliary files recursively from a directory.
readAuxFilesRecursive :: FilePath -> IO [(FilePath, ByteString.ByteString)]
readAuxFilesRecursive dir = do
    entries <- listDirectory dir
    fmap concat $ mapM (\entry -> do
        let fullPath = dir </> entry
        isDir <- doesDirectoryExist fullPath
        if isDir
            then do
                subFiles <- readAuxFilesRecursive fullPath
                pure $ map (\(fp, bs) -> (entry </> fp, bs)) subFiles
            else do
                content <- ByteString.readFile fullPath
                pure [(entry, content)]) entries

-- | Create a temporary directory.
createTempDirectory :: String -> IO FilePath
createTempDirectory prefix = do
    (ExitSuccess, out, _) <- readProcessWithExitCode "mktemp" ["-d", "/tmp/" <> prefix <> "XXXXXX"] ""
    pure (init $ Text.unpack $ Text.decodeUtf8 out)  -- Remove trailing newline

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | Validate that an imported tool works.
validateTool :: StandaloneToolExport -> IO (Either ValidationError ())
validateTool tool = do
    result <- try @IOException $ withTempTool tool $ \toolPath -> do
        -- Run 'describe' command
        (code, out, err) <- readProcessWithExitCode toolPath ["describe"] ""
        
        case code of
            ExitSuccess -> do
                -- Parse the output
                case Aeson.eitherDecode (LByteString.fromStrict out) of
                    Left jsonErr -> pure $ Left $ InvalidScriptInfo jsonErr
                    Right parsedInfo -> do
                        -- Verify it matches stored metadata (ignoring the slug which may have been prefixed)
                        if scriptInfoMatches parsedInfo tool.standaloneToolInfo
                            then pure $ Right ()
                            else pure $ Left $ InvalidScriptInfo "ScriptInfo mismatch"
            ExitFailure _ -> pure $ Left $ DescribeFailed (Text.unpack $ Text.decodeUtf8 err)
    
    case result of
        Left e -> pure $ Left $ DescribeFailed (show e)
        Right result' -> pure result'

-- | Check if two ScriptInfo values match (for validation).
-- Allows slug to differ (in case prefix was added).
scriptInfoMatches :: ScriptInfo -> ScriptInfo -> Bool
scriptInfoMatches a b =
    a.scriptArgs == b.scriptArgs &&
    a.scriptDescription == b.scriptDescription &&
    a.scriptEmptyResultBehavior == b.scriptEmptyResultBehavior

-- | Run an action with a temporary tool file.
withTempTool :: StandaloneToolExport -> (FilePath -> IO a) -> IO a
withTempTool tool action = do
    tmpDir <- getTemporaryDirectory
    let toolPath = tmpDir </> "validate-tool"
    
    -- Write script
    ByteString.writeFile toolPath tool.standaloneToolScript
    setFileMode toolPath tool.standaloneToolPermissions
    
    -- Run action and cleanup
    result <- action toolPath
    removeFile toolPath
    pure result

-- | Get the system temporary directory.
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = do
    result <- try @IOException $ readProcessWithExitCode "mktemp" ["-d"] ""
    case result of
        Right (ExitSuccess, out, _) -> pure (init $ Text.unpack $ Text.decodeUtf8 out)
        _ -> pure "/tmp"

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Parse a git URL string into a GitUrl.
parseGitUrl :: String -> Either GitError GitUrl
parseGitUrl url
    | isValidGitUrl url = Right $ GitUrl $ Text.pack url
    | otherwise = Left $ InvalidGitUrl url

-- | Check if a string looks like a valid git URL.
isValidGitUrl :: String -> Bool
isValidGitUrl url =
    any (`isPrefixOf` url)
        [ "https://"
        , "http://"
        , "git@"
        , "git://"
        , "ssh://"
        ]
    || ".git" `isPrefixOf` drop (length url - 4) url

