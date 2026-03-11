{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module for the 'export' command handler.
--
-- The export command packages agent configurations and tools into
-- archive files or git repositories for sharing and deployment.
module System.Agents.CLI.Export
    ( -- * Types
      ExportOptions (..)
    , ExportSource (..)
    , ExportDestination (..)
    , GitExportDest (..)
      -- * Handler
    , handleExport
      -- * Helpers
    , buildExportPackage
    , loadAgentFromFile
    , loadToolsForAgent
    , loadStandaloneToolsForAgent
    ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory, takeFileName, (</>))
import qualified System.Process as Process
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.Encoding as TextEncoding

import System.Agents.Base (Agent (..), AgentDescription (..))
import System.Agents.ExportImport.Types
import qualified System.Agents.ExportImport.Archive as Archive
import qualified System.Agents.ExportImport.Git as Git
import qualified System.Agents.ExportImport.ToolInstall as ExportInstall
import System.Agents.Tools.Bash (ScriptInfo (..))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Options for the export command
data ExportOptions = ExportOptions
    { exportSource :: ExportSource
    , exportDestination :: ExportDestination
    , exportFormat :: Maybe ArchiveFormat
    , exportNamespace :: Maybe Text
    , exportIncludeTools :: Bool
    , exportIncludeMcp :: Bool
    , exportGitOptions :: Maybe GitExportOptions
    }
    deriving (Show)

-- | Source for export
data ExportSource
    = ExportCurrentAgent
    | ExportAllAgents
    | ExportAgentBySlug Text
    | ExportCurrentTools
    | ExportToolByName Text
    deriving (Show)

-- | Destination for export
data ExportDestination
    = ExportToFile FilePath
    | ExportToGit GitExportDest
    deriving (Show)

-- | Git export destination configuration
data GitExportDest = GitExportDest
    { destGitUrl :: Text
    , destGitBranch :: Maybe Text
    , destGitCommitMessage :: Maybe Text
    , destGitPush :: Bool
    , destGitTag :: Maybe Text
    }
    deriving (Show)

-------------------------------------------------------------------------------
-- Handler
-------------------------------------------------------------------------------

-- | Handle the export command
handleExport :: ExportOptions -> [FilePath] -> FilePath -> IO ()
handleExport opts agentFiles _configDir = do
    -- Determine archive format
    let archiveFormat = case (opts.exportDestination, opts.exportFormat) of
            (ExportToFile path, Nothing) -> fromMaybe TarGzFormat (detectArchiveFormat path)
            (_, Just fmt) -> fmt
            (_, Nothing) -> TarGzFormat
    
    -- Build export package
    ePackage <- buildExportPackage opts agentFiles
    
    case ePackage of
        Left err -> do
            Text.hPutStrLn stderr $ "Export error: " <> err
            exitFailure
        Right pkg -> do
            case opts.exportDestination of
                ExportToFile path -> do
                    result <- Archive.exportToArchive pkg archiveFormat path
                    case result of
                        Left err -> do
                            Text.hPutStrLn stderr $ "Export failed: " <> Text.pack (show err)
                            exitFailure
                        Right () -> do
                            Text.putStrLn $ "Exported to " <> Text.pack path
                            exitSuccess
                
                ExportToGit gitDest -> do
                    -- Convert to GitUrl and GitExportOptions
                    let gitUrl0 = GitUrl
                            { gitRemote = destGitUrl gitDest
                            , gitBranch = destGitBranch gitDest
                            , gitPath = parseNamespaceToMaybeNs opts.exportNamespace
                            }
                    let gitOpts = GitExportOptions
                            { gitCommitMessage = fromMaybe "Update agent configurations" (destGitCommitMessage gitDest)
                            , gitTag = destGitTag gitDest
                            , gitPush = destGitPush gitDest
                            }
                    
                    result <- Git.exportToGit pkg gitUrl0 gitOpts
                    case result of
                        Left err -> do
                            Text.hPutStrLn stderr $ "Git export failed: " <> Text.pack (show err)
                            exitFailure
                        Right () -> do
                            Text.putStrLn "Exported to git repository"
                            when (destGitPush gitDest) $ Text.putStrLn "Changes pushed to remote"
                            exitSuccess

-------------------------------------------------------------------------------
-- Package Building
-------------------------------------------------------------------------------

-- | Build an export package from the given options
buildExportPackage :: ExportOptions -> [FilePath] -> IO (Either Text ExportPackage)
buildExportPackage opts agentFiles = do
    now <- getCurrentTime
    let metadata = PackageMetadata
            { packageVersion = exportSchemaVersion
            , packageCreatedAt = now
            , packageDescription = Nothing
            , packageSource = Nothing
            }
    
    case opts.exportSource of
        ExportCurrentAgent -> do
            case agentFiles of
                [] -> pure $ Left "No agent file specified"
                (agentFile:_) -> do
                    -- Load agent
                    eAgent <- loadAgentFromFile agentFile
                    case eAgent of
                        Left err -> pure $ Left err
                        Right agent -> do
                            -- Load agent tools
                            tools <- if opts.exportIncludeTools
                                then loadToolsForAgent agentFile agent
                                else pure []
                            
                            let agentExport = AgentExport
                                    { agentConfig = agent
                                    , agentNamespace = opts.exportNamespace
                                    , agentTools = tools
                                    }
                            
                            pure $ Right $ ExportPackage
                                { packageMetadata = metadata
                                , packageAgents = [agentExport]
                                , packageTools = []
                                , packageMcpServers = if opts.exportIncludeMcp
                                    then maybe [] (map (\m -> McpServerExport m opts.exportNamespace)) (mcpServers agent)
                                    else []
                                }
        
        ExportAllAgents -> do
            -- Export all loaded agents
            agentExports <- mapM (\agentFile -> do
                eAgent <- loadAgentFromFile agentFile
                case eAgent of
                    Left _ -> pure Nothing
                    Right agent -> do
                        tools <- if opts.exportIncludeTools
                            then loadToolsForAgent agentFile agent
                            else pure []
                        pure $ Just $ AgentExport
                            { agentConfig = agent
                            , agentNamespace = opts.exportNamespace
                            , agentTools = tools
                            }) agentFiles
            
            pure $ Right $ ExportPackage
                { packageMetadata = metadata
                , packageAgents = [a | Just a <- agentExports]
                , packageTools = []
                , packageMcpServers = []
                }
        
        ExportAgentBySlug targetSlug -> do
            -- Find agent with matching slug
            agents <- mapM loadAgentFromFile agentFiles
            case [a | Right a <- agents, targetSlug == slug a] of
                [] -> pure $ Left $ "Agent with slug '" <> targetSlug <> "' not found"
                (agent:_) -> do
                    let agentFile = case [f | (f, Right a) <- zip agentFiles agents, slug a == targetSlug] of
                            (f:_) -> f
                            [] -> case agentFiles of
                                (first:_) -> first
                                [] -> error "No agent files available"
                    tools <- if opts.exportIncludeTools
                        then loadToolsForAgent agentFile agent
                        else pure []
                    
                    pure $ Right $ ExportPackage
                        { packageMetadata = metadata
                        , packageAgents = 
                            [ AgentExport
                                { agentConfig = agent
                                , agentNamespace = opts.exportNamespace
                                , agentTools = tools
                                }
                            ]
                        , packageTools = []
                        , packageMcpServers = if opts.exportIncludeMcp
                            then maybe [] (map (\m -> McpServerExport m opts.exportNamespace)) (mcpServers agent)
                            else []
                        }
        
        ExportCurrentTools -> do
            -- Export tools from current agent
            case agentFiles of
                [] -> pure $ Left "No agent file specified"
                (agentFile:_) -> do
                    eAgent <- loadAgentFromFile agentFile
                    case eAgent of
                        Left err -> pure $ Left err
                        Right agent -> do
                            standaloneTools <- loadStandaloneToolsForAgent agentFile agent
                            pure $ Right $ ExportPackage
                                { packageMetadata = metadata
                                , packageAgents = []
                                , packageTools = standaloneTools
                                , packageMcpServers = []
                                }
        
        ExportToolByName targetToolName -> do
            -- Find and export specific tool
            case agentFiles of
                [] -> pure $ Left "No agent file specified"
                (agentFile:_) -> do
                    eAgent <- loadAgentFromFile agentFile
                    case eAgent of
                        Left err -> pure $ Left err
                        Right agent -> do
                            standaloneTools <- loadStandaloneToolsForAgent agentFile agent
                            case [t | t <- standaloneTools, scriptSlug (standaloneToolInfo t) == targetToolName] of
                                [] -> pure $ Left $ "Tool '" <> targetToolName <> "' not found"
                                (tool:_) -> pure $ Right $ ExportPackage
                                    { packageMetadata = metadata
                                    , packageAgents = []
                                    , packageTools = [tool]
                                    , packageMcpServers = []
                                    }

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Helper to parse namespace text to Maybe Namespace
parseNamespaceToMaybeNs :: Maybe Text -> Maybe Namespace
parseNamespaceToMaybeNs Nothing = Nothing
parseNamespaceToMaybeNs (Just txt) = 
    case parseNamespace txt of
        Left _ -> Nothing
        Right ns -> Just ns

-- | Load an agent from a file
loadAgentFromFile :: FilePath -> IO (Either Text Agent)
loadAgentFromFile path = do
    result <- Aeson.eitherDecodeFileStrict' path
    case result of
        Left err -> pure $ Left $ Text.pack $ "Failed to parse agent file: " ++ err
        Right (AgentDescription agent) -> pure $ Right agent

-- | Load tools for an agent
loadToolsForAgent :: FilePath -> Agent -> IO [ToolExport]
loadToolsForAgent agentFile agent = do
    let toolDir = takeDirectory agentFile </> toolDirectory agent
    exists <- doesDirectoryExist toolDir
    if not exists
        then pure []
        else do
            toolFiles <- ExportInstall.discoverTools toolDir
            mapM (loadToolExport toolDir) toolFiles
  where
    loadToolExport :: FilePath -> FilePath -> IO ToolExport
    loadToolExport _baseDir toolPath = do
        content <- ByteString.readFile toolPath
        perms <- Archive.getFileMode toolPath
        let tName = Text.pack $ takeFileName toolPath
        -- Run describe to get metadata
        _ <- ExportInstall.validateTool toolPath
        pure $ ToolExport
            { toolName = tName
            , toolContent = content
            , toolPermissions = perms
            , toolMetadata = Nothing  -- Could be populated by running describe
            , toolNamespace = Nothing
            }

-- | Load standalone tools for an agent
loadStandaloneToolsForAgent :: FilePath -> Agent -> IO [StandaloneToolExport]
loadStandaloneToolsForAgent agentFile agent = do
    let toolDir = takeDirectory agentFile </> toolDirectory agent
    exists <- doesDirectoryExist toolDir
    if not exists
        then pure []
        else do
            toolFiles <- ExportInstall.discoverTools toolDir
            mapM (loadStandaloneTool toolDir) toolFiles
  where
    loadStandaloneTool :: FilePath -> FilePath -> IO StandaloneToolExport
    loadStandaloneTool _baseDir toolPath = do
        content <- ByteString.readFile toolPath
        perms <- Archive.getFileMode toolPath
        -- Run describe to get metadata
        (code, out, _) <- Process.readProcessWithExitCode toolPath ["describe"] ""
        let metadata = case code of
                ExitSuccess ->
                    case Aeson.eitherDecode (LByteString.fromStrict $ TextEncoding.encodeUtf8 $ Text.pack out) of
                        Left _ -> defaultMetadata toolPath
                        Right info0 -> info0
                _ -> defaultMetadata toolPath
        pure $ StandaloneToolExport
            { standaloneToolInfo = metadata
            , standaloneToolScript = content
            , standaloneToolPermissions = perms
            , standaloneToolAuxFiles = []
            }
    
    defaultMetadata :: FilePath -> ScriptInfo
    defaultMetadata path = ScriptInfo
        { scriptArgs = []
        , scriptSlug = Text.pack $ takeFileName path
        , scriptDescription = "Imported tool"
        , scriptEmptyResultBehavior = Nothing
        }

