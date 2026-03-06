{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tool installation logic for importing tools to agents or global directories.
module System.Agents.ExportImport.ToolInstall where

import Control.Exception (try, IOException)
import Control.Monad (forM_, unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.Encoding as Text
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Prod.Tracer (Tracer(..))
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , listDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.Exit (ExitCode(..))
import System.FilePath
    ( (</>)
    , takeDirectory
    , takeFileName
    )
import System.Posix.Files
    ( createSymbolicLink
    , fileMode
    , getFileStatus
    , setFileMode
    )
import System.Process.ByteString (readProcessWithExitCode)

import System.Agents.AgentTree (AgentConfigTree(..), agentRootDir)
import System.Agents.Base (Agent(..))
import System.Agents.ExportImport.Types
import System.Agents.Tools.Bash (ScriptDescription(..), ScriptInfo(..), ScriptEmptyResultBehavior(..))
import qualified System.Agents.Tools.Bash as Bash

-------------------------------------------------------------------------------
-- Install Tools to Agent
-------------------------------------------------------------------------------

-- | Install tools to an agent's tool directory.
installToolsToAgent :: [StandaloneToolExport] -> Agent -> FilePath -> InstallOptions -> IO (Either InstallError ())
installToolsToAgent tools agent agentFilePath opts = do
    let toolDir = takeDirectory agentFilePath </> agent.toolDirectory
    installToolsToDirectory tools toolDir opts

-- | Install tools to a global/shared tool directory.
installToolsGlobally :: [StandaloneToolExport] -> FilePath -> InstallOptions -> IO (Either InstallError ())
installToolsGlobally tools globalDir opts = do
    -- Ensure global directory exists
    createDirectoryIfMissing True globalDir
    installToolsToDirectory tools globalDir opts

-- | Install tools from one agent to another.
copyToolsBetweenAgents :: AgentConfigTree -> AgentConfigTree -> InstallOptions -> IO (Either InstallError ())
copyToolsBetweenAgents sourceAgent targetAgent opts = do
    -- Discover tools in source agent
    let sourceToolDir = agentRootDir sourceAgent </> sourceAgent.agentConfig.toolDirectory
    sourceExists <- doesDirectoryExist sourceToolDir
    
    if not sourceExists
        then pure $ Left $ InvalidToolDirectory sourceToolDir
        else do
            -- Load source tools
            let silentTracer = Tracer $ \_ -> pure ()
            (scripts, _) <- Bash.loadDirectory silentTracer sourceToolDir
            
            -- Convert to StandaloneToolExports
            exports <- mapM scriptDescriptionToExport scripts.scriptDescriptions
            
            -- Install to target agent
            let targetToolDir = agentRootDir targetAgent </> targetAgent.agentConfig.toolDirectory
            installToolsToDirectory exports targetToolDir opts
  where
    scriptDescriptionToExport :: ScriptDescription -> IO StandaloneToolExport
    scriptDescriptionToExport desc = do
        scriptContent <- ByteString.readFile desc.scriptPath
        permissions <- fileMode <$> getFileStatus desc.scriptPath
        
        -- Check for aux files
        let scriptDir = takeDirectory desc.scriptPath
        auxFiles <- findAuxFiles scriptDir desc.scriptPath
        
        pure StandaloneToolExport
            { standaloneToolInfo = desc.scriptInfo
            , standaloneToolScript = scriptContent
            , standaloneToolPermissions = permissions
            , standaloneToolAuxFiles = auxFiles
            }

-------------------------------------------------------------------------------
-- Core Installation Logic
-------------------------------------------------------------------------------

-- | Install tools to a specific directory.
installToolsToDirectory :: [StandaloneToolExport] -> FilePath -> InstallOptions -> IO (Either InstallError ())
installToolsToDirectory tools targetDir opts = do
    -- Ensure target directory exists
    createDirectoryIfMissing True targetDir
    
    -- Check if target is a valid tool directory
    targetExists <- doesDirectoryExist targetDir
    unless targetExists $ do
        error "Target directory does not exist"
    
    -- Install each tool
    results <- mapM (installSingleTool targetDir opts) tools
    
    -- Return first error if any
    pure $ case find isLeft results of
        Just (Left err) -> Left err
        _ -> Right ()
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

-- | Install a single tool to the target directory.
installSingleTool :: FilePath -> InstallOptions -> StandaloneToolExport -> IO (Either InstallError ())
installSingleTool targetDir opts tool = do
    let info = standaloneToolInfo tool
        originalSlug = Bash.scriptSlug info
        slug = case opts.installPrefix of
            Just prefix -> prefix <> "-" <> originalSlug
            Nothing -> originalSlug
        toolPath = targetDir </> Text.unpack slug
    
    -- Check if tool already exists
    toolExists <- doesFileExist toolPath
    if toolExists && not opts.installForce
        then pure $ Left $ ToolAlreadyExists slug
        else do
            -- Remove existing if force is set
            when (toolExists && opts.installForce) $ do
                removeFile toolPath
            
            if opts.installLink
                then installAsSymlink toolPath tool
                else installAsCopy toolPath tool

-- | Install tool as a copy.
installAsCopy :: FilePath -> StandaloneToolExport -> IO (Either InstallError ())
installAsCopy toolPath tool = do
    result <- try @IOException $ do
        -- Write script
        ByteString.writeFile toolPath tool.standaloneToolScript
        setFileMode toolPath tool.standaloneToolPermissions
        
        -- Write aux files
        forM_ tool.standaloneToolAuxFiles $ \(relPath, content) -> do
            let auxPath = takeDirectory toolPath </> relPath
            createDirectoryIfMissing True (takeDirectory auxPath)
            ByteString.writeFile auxPath content
    
    case result of
        Left e -> pure $ Left $ InstallIOError e
        Right () -> pure $ Right ()

-- | Install tool as a symlink (for git-based tools).
installAsSymlink :: FilePath -> StandaloneToolExport -> IO (Either InstallError ())
installAsSymlink toolPath tool = do
    result <- try @IOException $ do
        -- Create temporary directory for the actual tool
        tmpDir <- getTemporaryDirectory
        let actualDir = tmpDir </> "agents-symlink-tools"
        createDirectoryIfMissing True actualDir
        
        -- Write actual script
        let actualPath = actualDir </> takeFileName toolPath
        ByteString.writeFile actualPath tool.standaloneToolScript
        setFileMode actualPath tool.standaloneToolPermissions
        
        -- Create symlink
        createSymbolicLink actualPath toolPath
    
    case result of
        Left e -> pure $ Left $ InstallIOError e
        Right () -> pure $ Right ()

-------------------------------------------------------------------------------
-- Tool Validation
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
                        -- Verify it matches stored metadata
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
    Bash.scriptArgs a == Bash.scriptArgs b &&
    Bash.scriptDescription a == Bash.scriptDescription b &&
    Bash.scriptEmptyResultBehavior a == Bash.scriptEmptyResultBehavior b

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
-- Helper Functions
-------------------------------------------------------------------------------

-- | Find auxiliary files for a script in a directory.
findAuxFiles :: FilePath -> FilePath -> IO [(FilePath, ByteString.ByteString)]
findAuxFiles dir scriptPath = do
    let scriptName = Text.pack $ takeFileName scriptPath
    entries <- listDirectory dir
    let auxFiles = filter (\e -> Text.pack e /= scriptName && scriptName `Text.isPrefixOf` Text.pack e) entries
    mapM (\f -> do
        content <- ByteString.readFile (dir </> f)
        pure (f, content)) auxFiles

-------------------------------------------------------------------------------
-- Extended Install Options
-------------------------------------------------------------------------------

-- | Install options with additional flags for advanced use cases.
data ExtendedInstallOptions = ExtendedInstallOptions
    { extInstallBase :: InstallOptions
    , extInstallValidate :: Bool
    -- ^ Whether to validate tools after installation
    , extInstallBackup :: Bool
    -- ^ Whether to backup existing tools before overwriting
    , extInstallBackupDir :: Maybe FilePath
    -- ^ Directory for backups (uses ~/.agents-exe/backups if not specified)
    }

-- | Default extended install options.
defaultExtendedInstallOptions :: ExtendedInstallOptions
defaultExtendedInstallOptions = ExtendedInstallOptions
    { extInstallBase = defaultInstallOptions
    , extInstallValidate = True
    , extInstallBackup = False
    , extInstallBackupDir = Nothing
    }

-- | Install tools with extended options.
installToolsExtended :: [StandaloneToolExport] -> FilePath -> ExtendedInstallOptions -> IO (Either InstallError [ValidationError])
installToolsExtended tools targetDir opts = do
    -- Perform base installation
    result <- installToolsToDirectory tools targetDir opts.extInstallBase
    
    case result of
        Left err -> pure $ Left err
        Right () ->
            if opts.extInstallValidate
                then do
                    -- Validate each installed tool
                    validationResults <- mapM validateTool tools
                    pure $ Right $ lefts validationResults
                else pure $ Right []
  where
    lefts :: [Either a b] -> [a]
    lefts = foldr (\e acc -> case e of Left x -> x:acc; Right _ -> acc) []

-- | Create a backup of an existing tool.
backupTool :: FilePath -> FilePath -> IO (Either InstallError ())
backupTool toolPath backupDir = do
    result <- try @IOException $ do
        createDirectoryIfMissing True backupDir
        let backupPath = backupDir </> takeFileName toolPath
        copyFile toolPath backupPath
        -- Also backup aux files if they exist
        auxFiles <- findAuxFiles (takeDirectory toolPath) toolPath
        forM_ auxFiles $ \(relPath, _) -> do
            let auxPath = takeDirectory toolPath </> relPath
                auxBackupPath = backupDir </> relPath
            auxExists <- doesFileExist auxPath
            when auxExists $ do
                createDirectoryIfMissing True (takeDirectory auxBackupPath)
                copyFile auxPath auxBackupPath
    
    case result of
        Left e -> pure $ Left $ InstallIOError e
        Right () -> pure $ Right ()

