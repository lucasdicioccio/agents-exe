{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'import' command handler.

The import command imports agent configurations and tools from
archive files or git repositories.
-}
module System.Agents.CLI.Import (
    -- * Types
    ImportOptions (..),
    ImportSource (..),
    GitImportSource (..),
    ImportMode (..),

    -- * Handler
    handleImport,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import qualified System.Agents.ExportImport.Archive as Archive
import qualified System.Agents.ExportImport.Git as Git
import qualified System.Agents.ExportImport.ToolInstall as ExportInstall
import System.Agents.ExportImport.Types
import qualified System.Agents.Tools.Bash as Bash

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Options for the import command
data ImportOptions = ImportOptions
    { importSource :: ImportSource
    , importDestination :: ExportInstall.ImportDestination
    , importNamespace :: Maybe Text
    , importMode :: ImportMode
    , importToolsOnly :: Bool
    , importListNamespaces :: Bool
    , importListTools :: Bool
    }
    deriving (Show)

-- | Source for import
data ImportSource
    = ImportFromFile FilePath
    | ImportFromGit GitImportSource
    deriving (Show)

-- | Git import source configuration
data GitImportSource = GitImportSource
    { gitImportUrl :: Text
    , gitImportRef :: Maybe Text
    , gitImportNamespace :: Maybe Text
    }
    deriving (Show)

-- | Import conflict resolution mode
data ImportMode
    = ImportFailOnConflict
    | ImportOverwrite
    | ImportMerge
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Handler
-------------------------------------------------------------------------------

-- | Handle the import command
handleImport :: ImportOptions -> IO ()
handleImport opts
    | importListNamespaces opts = handleListNamespaces opts
    | importListTools opts = handleListTools opts
    | otherwise = do
        -- Perform import
        eResult <- case importSource opts of
            ImportFromFile path -> do
                if importToolsOnly opts
                    then do
                        ePkg <- Archive.importToolsFromArchive path
                        case ePkg of
                            Left err -> pure $ Left $ Text.pack $ show err
                            Right pkg -> do
                                let installOpts =
                                        ExportInstall.InstallOptions
                                            { ExportInstall.installForce = importMode opts == ImportOverwrite || importMode opts == ImportMerge
                                            , ExportInstall.installLink = False
                                            , ExportInstall.installPrefix = importNamespace opts
                                            }
                                result <- ExportInstall.importToolPackage pkg (importDestination opts) installOpts
                                case result of
                                    Left err -> pure $ Left $ Text.pack $ show err
                                    Right () -> pure $ Right ()
                    else do
                        ePkg <- Archive.importFromArchive path
                        case ePkg of
                            Left err -> pure $ Left $ Text.pack $ show err
                            Right _pkg -> do
                                -- TODO: Handle full package import
                                pure $ Right ()
            ImportFromGit gitSource -> do
                let gitUrl0 =
                        GitUrl
                            { gitRemote = gitImportUrl gitSource
                            , gitBranch = gitImportRef gitSource
                            , gitPath = parseNamespaceToMaybeNs (importNamespace opts)
                            }
                let gitOpts =
                        GitImportOptions
                            { gitRef = gitImportRef gitSource
                            , gitSparsePaths = []
                            }

                if importToolsOnly opts
                    then do
                        ePkg <- Git.importToolsFromGit gitUrl0 (parseNamespaceToMaybeNs (importNamespace opts))
                        case ePkg of
                            Left err -> pure $ Left $ Text.pack $ show err
                            Right pkg -> do
                                let installOpts =
                                        ExportInstall.InstallOptions
                                            { ExportInstall.installForce = importMode opts == ImportOverwrite || importMode opts == ImportMerge
                                            , ExportInstall.installLink = False
                                            , ExportInstall.installPrefix = Nothing
                                            }
                                result <- ExportInstall.importToolPackage pkg (importDestination opts) installOpts
                                case result of
                                    Left err -> pure $ Left $ Text.pack $ show err
                                    Right () -> pure $ Right ()
                    else do
                        ePkg <- Git.importFromGit gitUrl0 gitOpts
                        case ePkg of
                            Left err -> pure $ Left $ Text.pack $ show err
                            Right _pkg -> do
                                -- TODO: Handle full package import
                                pure $ Right ()

        case eResult of
            Left err -> do
                Text.hPutStrLn stderr $ "Import failed: " <> err
                exitFailure
            Right () -> do
                Text.putStrLn "Import successful"
                exitSuccess

-------------------------------------------------------------------------------
-- List Operations
-------------------------------------------------------------------------------

-- | Handle listing namespaces from a git source
handleListNamespaces :: ImportOptions -> IO ()
handleListNamespaces opts = do
    case importSource opts of
        ImportFromGit gitSource -> do
            let gitUrl0 =
                    GitUrl
                        { gitRemote = gitImportUrl gitSource
                        , gitBranch = gitImportRef gitSource
                        , gitPath = parseNamespaceToMaybeNs (importNamespace opts)
                        }
            result <- Git.listGitNamespaces gitUrl0
            case result of
                Left err -> do
                    Text.hPutStrLn stderr $ "Failed to list namespaces: " <> Text.pack (show err)
                    exitFailure
                Right namespaces -> do
                    Text.putStrLn "Available namespaces:"
                    mapM_ (Text.putStrLn . ("  " <>) . Text.intercalate "." . unNamespace) namespaces
                    exitSuccess
        _ -> do
            Text.hPutStrLn stderr "List namespaces only supported for git sources"
            exitFailure

-- | Handle listing tools from a git source
handleListTools :: ImportOptions -> IO ()
handleListTools opts = do
    case importSource opts of
        ImportFromGit gitSource -> do
            let gitUrl0 =
                    GitUrl
                        { gitRemote = gitImportUrl gitSource
                        , gitBranch = gitImportRef gitSource
                        , gitPath = parseNamespaceToMaybeNs (importNamespace opts)
                        }
            result <- Git.listGitTools gitUrl0
            case result of
                Left err -> do
                    Text.hPutStrLn stderr $ "Failed to list tools: " <> Text.pack (show err)
                    exitFailure
                Right tools -> do
                    Text.putStrLn "Available tools:"
                    mapM_
                        ( \(ns, info0) -> do
                            Text.putStrLn $ "  " <> Text.intercalate "." (unNamespace ns) <> ":"
                            Text.putStrLn $ "    " <> Bash.scriptSlug info0 <> " - " <> Bash.scriptDescription info0
                        )
                        tools
                    exitSuccess
        _ -> do
            Text.hPutStrLn stderr "List tools only supported for git sources"
            exitFailure

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
