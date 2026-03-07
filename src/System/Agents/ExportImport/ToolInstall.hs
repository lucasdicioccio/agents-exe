{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Agents.ExportImport.ToolInstall where

import Control.Exception (try, IOException)
import Control.Monad (forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , getHomeDirectory
    , listDirectory
    , renameFile
    )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Posix.Files (unionFileModes, ownerExecuteMode, setFileMode, fileMode, getFileStatus)
import System.Posix.Types (FileMode)
import System.Process (readProcessWithExitCode)

import System.Agents.Base (Agent(..), AgentDescription(..))
import System.Agents.ExportImport.Types
import System.Agents.Tools.Bash (ScriptInfo(..))

-------------------------------------------------------------------------------
-- Install Options
-------------------------------------------------------------------------------

data InstallOptions = InstallOptions
    { installForce :: Bool          -- ^ Overwrite existing tools
    , installLink :: Bool           -- ^ Create symlinks instead of copying
    , installPrefix :: Maybe Text   -- ^ Prefix to add to tool names
    }
    deriving (Show, Eq)

-- | Default install options
defaultInstallOptions :: InstallOptions
defaultInstallOptions = InstallOptions
    { installForce = False
    , installLink = False
    , installPrefix = Nothing
    }

-------------------------------------------------------------------------------
-- Tool Installation
-------------------------------------------------------------------------------

-- | Install tools to an agent's tool directory
installToolsToAgent :: [StandaloneToolExport] -> Agent -> InstallOptions -> IO (Either InstallError ())
installToolsToAgent tools agent opts = do
    let toolDir = toolDirectory agent
    installTools tools toolDir opts

-- | Install tools to a specific directory
installTools :: [StandaloneToolExport] -> FilePath -> InstallOptions -> IO (Either InstallError ())
installTools tools targetDir opts = do
    -- Ensure target directory exists
    createDirectoryIfMissing True targetDir
    
    -- Check if target is valid
    isValid <- isValidToolDirectory targetDir
    if not isValid
        then pure $ Left $ InvalidToolDirectory targetDir
        else do
            -- Install each tool
            results <- mapM (installTool targetDir opts) tools
            let (errors, _) = partitionEithers results
            case errors of
                [] -> pure $ Right ()
                (firstErr:_) -> pure $ Left firstErr

-- | Install tools globally (to a shared tools directory)
installToolsGlobally :: [StandaloneToolExport] -> FilePath -> InstallOptions -> IO (Either InstallError ())
installToolsGlobally tools globalDir opts = do
    createDirectoryIfMissing True globalDir
    installTools tools globalDir opts

-- | Copy tools from one agent to another
copyToolsBetweenAgents :: Agent -> Agent -> InstallOptions -> IO (Either InstallError ())
copyToolsBetweenAgents fromAgent toAgent opts = do
    -- Load tools from source agent
    result <- try $ do
        toolFiles <- listDirectory (toolDirectory fromAgent)
        -- Read and create exports for each tool
        toolExports <- mapM (loadToolFromPath (toolDirectory fromAgent)) toolFiles
        let validExports = [e | Right e <- toolExports]
        installToolsToAgent validExports toAgent opts
    
    case result of
        Left (e :: IOException) -> pure $ Left $ InstallIOError e
        Right r -> pure r

-------------------------------------------------------------------------------
-- Individual Tool Installation
-------------------------------------------------------------------------------

installTool :: FilePath -> InstallOptions -> StandaloneToolExport -> IO (Either InstallError ())
installTool targetDir opts tool = do
    let toolName0 = case installPrefix opts of
            Just prefix -> Text.unpack prefix ++ Text.unpack (scriptSlug $ standaloneToolInfo tool)
            Nothing -> Text.unpack (scriptSlug $ standaloneToolInfo tool)
    
    let targetPath = targetDir </> toolName0
    
    -- Check if tool already exists
    exists <- doesFileExist targetPath
    if exists && not (installForce opts)
        then pure $ Left $ ToolAlreadyExists (scriptSlug $ standaloneToolInfo tool)
        else do
            -- Backup existing if force is set
            when (exists && installForce opts) $ do
                backupPath <- getBackupPath targetPath
                renameFile targetPath backupPath
            
            -- Install the tool
            if installLink opts
                then installToolSymlink targetDir opts tool
                else installToolCopy targetDir opts tool

installToolCopy :: FilePath -> InstallOptions -> StandaloneToolExport -> IO (Either InstallError ())
installToolCopy targetDir opts tool = do
    let toolName0 = case installPrefix opts of
            Just prefix -> Text.unpack prefix ++ Text.unpack (scriptSlug $ standaloneToolInfo tool)
            Nothing -> Text.unpack (scriptSlug $ standaloneToolInfo tool)
    
    let targetPath = targetDir </> toolName0
    
    result <- try $ do
        -- Write the script
        ByteString.writeFile targetPath (standaloneToolScript tool)
        setFileMode targetPath (standaloneToolPermissions tool)
        
        -- Write aux files if any
        forM_ (standaloneToolAuxFiles tool) $ \(auxPath, auxContent) -> do
            let fullAuxPath = targetDir </> auxPath
            createDirectoryIfMissing True (takeDirectory fullAuxPath)
            ByteString.writeFile fullAuxPath auxContent
    
    case result of
        Left (e :: IOException) -> pure $ Left $ InstallIOError e
        Right () -> pure $ Right ()

installToolSymlink :: FilePath -> InstallOptions -> StandaloneToolExport -> IO (Either InstallError ())
installToolSymlink targetDir opts tool = do
    -- For symlinks, we need the original source path, which we don't have
    -- So we'll copy instead and warn
    putStrLn "Warning: Symlink installation not supported for exports, copying instead"
    installToolCopy targetDir opts tool

-------------------------------------------------------------------------------
-- Tool Validation
-------------------------------------------------------------------------------

-- | Validate a tool by running its describe command
validateTool :: FilePath -> IO (Either ToolValidationError ())
validateTool toolPath = do
    result <- try $ do
        -- Run the describe command
        (code, out, err) <- readProcessWithExitCode toolPath ["describe"] ""
        case code of
            ExitFailure _ -> pure $ Left $ DescribeFailed $ "Exit code: " ++ show code ++ ", stderr: " ++ err
            ExitSuccess -> do
                -- Parse the output as ScriptInfo
                case Aeson.eitherDecode (LByteString.fromStrict $ Text.encodeUtf8 $ Text.pack out) of
                    Left jsonErr -> pure $ Left $ InvalidScriptInfo jsonErr
                    Right (_ :: ScriptInfo) -> pure $ Right ()
    
    case result of
        Left (e :: IOException) -> pure $ Left $ DescribeFailed (show e)
        Right r -> pure r

-- | Validate multiple tools
validateTools :: [FilePath] -> IO [(FilePath, Either ToolValidationError ())]
validateTools paths = do
    mapM (\p -> do
        result <- validateTool p
        pure (p, result)) paths

-------------------------------------------------------------------------------
-- Tool Discovery and Loading
-------------------------------------------------------------------------------

-- | Load a tool from a file path, creating a StandaloneToolExport
loadToolFromPath :: FilePath -> FilePath -> IO (Either InstallError StandaloneToolExport)
loadToolFromPath baseDir toolName0 = do
    let toolPath = baseDir </> toolName0
    
    result <- try $ do
        isFile <- doesFileExist toolPath
        if not isFile
            then pure $ Left $ InvalidToolDirectory toolPath
            else do
                -- Run describe to get metadata
                (code, out, err) <- readProcessWithExitCode toolPath ["describe"] ""
                case code of
                    ExitFailure _ -> pure $ Left $ ValidationFailed $ "Describe failed: " ++ err
                    ExitSuccess -> do
                        case Aeson.eitherDecode (LByteString.fromStrict $ Text.encodeUtf8 $ Text.pack out) of
                            Left jsonErr -> pure $ Left $ ValidationFailed $ "Invalid JSON: " ++ jsonErr
                            Right info -> do
                                -- Read the script content
                                content <- ByteString.readFile toolPath
                                perms <- getFileMode' toolPath
                                pure $ Right $ StandaloneToolExport
                                    { standaloneToolInfo = info
                                    , standaloneToolScript = content
                                    , standaloneToolPermissions = perms
                                    , standaloneToolAuxFiles = []
                                    }
    
    case result of
        Left (e :: IOException) -> pure $ Left $ InstallIOError e
        Right r -> pure r

-- | Find all tools in a directory
discoverTools :: FilePath -> IO [FilePath]
discoverTools toolDir = do
    exists <- doesDirectoryExist toolDir
    if not exists
        then pure []
        else do
            entries <- listDirectory toolDir
            filterM isExecutableFile (map (toolDir </>) entries)
  where
    isExecutableFile :: FilePath -> IO Bool
    isExecutableFile path = do
        isFile <- doesFileExist path
        if not isFile
            then pure False
            else do
                perms <- getFileMode' path
                -- Check if owner has execute permission
                pure $ (perms `unionFileModes` ownerExecuteMode) == perms

-------------------------------------------------------------------------------
-- Import Modes
-------------------------------------------------------------------------------

data ImportMode
    = ImportFailOnConflict
    | ImportOverwrite
    | ImportMerge
    deriving (Show, Eq)

-- | Convert import mode to install options
importModeToOptions :: ImportMode -> InstallOptions -> InstallOptions
importModeToOptions mode opts = case mode of
    ImportFailOnConflict -> opts { installForce = False }
    ImportOverwrite -> opts { installForce = True }
    ImportMerge -> opts { installForce = True }  -- Merge is essentially overwrite with intelligence

-------------------------------------------------------------------------------
-- Import Integration
-------------------------------------------------------------------------------

-- | Import tools from a ToolPackage to a target
data ImportDestination
    = ImportToCurrentDir
    | ImportToPath FilePath
    | ImportToConfigDir
    | ImportToAgent FilePath     -- ^ Install to agent's tool dir
    | ImportToToolDir FilePath   -- ^ Install to specific tool dir
    deriving (Show, Eq)

-- | Resolve import destination to a path
resolveImportDestination :: ImportDestination -> IO (Either InstallError FilePath)
resolveImportDestination dest = case dest of
    ImportToCurrentDir -> Right <$> getCurrentDirectory
    ImportToPath p -> pure $ Right p
    ImportToConfigDir -> do
        homeDir <- getHomeDirectory
        let configDir = homeDir </> ".config" </> "agents-exe"
        pure $ Right configDir
    ImportToAgent agentFile -> do
        -- Load agent to get tool directory
        result <- try $ do
            content <- LByteString.readFile agentFile
            case Aeson.eitherDecode content of
                Left err -> pure $ Left $ ValidationFailed $ "Failed to parse agent file: " ++ err
                Right (AgentDescription agent) -> 
                    pure $ Right $ toolDirectory agent
        case result of
            Left (e :: IOException) -> pure $ Left $ InstallIOError e
            Right r -> pure r
    ImportToToolDir p -> pure $ Right p

-- | Import a ToolPackage to the specified destination
importToolPackage :: ToolPackage -> ImportDestination -> InstallOptions -> IO (Either InstallError ())
importToolPackage pkg dest opts = do
    eTargetPath <- resolveImportDestination dest
    case eTargetPath of
        Left err -> pure $ Left err
        Right targetPath -> do
            installTools (toolPackageTools pkg) targetPath opts

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

isValidToolDirectory :: FilePath -> IO Bool
isValidToolDirectory path = do
    exists <- doesDirectoryExist path
    if not exists
        then do
            createDirectoryIfMissing True path
            pure True
        else pure True

getBackupPath :: FilePath -> IO FilePath
getBackupPath path = do
    now <- getCurrentTime
    let timestamp = take 19 $ show now  -- Simple timestamp
    pure $ path ++ ".backup." ++ timestamp

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\x -> liftM2 (\b -> if b then (x:) else id) (p x)) (return [])

liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do
    x1 <- m1
    x2 <- m2
    return (f x1 x2)

getFileMode' :: FilePath -> IO FileMode
getFileMode' path = do
    status <- getFileStatus path
    pure $ fileMode status

