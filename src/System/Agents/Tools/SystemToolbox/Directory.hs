{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Directory listing capability for the system toolbox.
module System.Agents.Tools.SystemToolbox.Directory (
    executeListDirectory,
) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Prod.Tracer (Tracer (..), runTracer)
import System.Directory (
    doesDirectoryExist,
    getModificationTime,
    getPermissions,
    listDirectory,
 )
import System.FilePath (takeFileName, (</>))

import System.Agents.Base (SystemToolCapability (..))
import System.Agents.FileSandbox (AccessResult (..), validateFileRead)
import System.Agents.Tools.DirectoryListing (FileEntry (..), FileFilter (NameFilter), FileType (..), ListDirectoryOp (..), applyFilters, formatFilePermissions, isHiddenFile)
import System.Agents.Tools.SystemToolbox.Types (
    QueryError (..),
    Toolbox (..),
    Trace (..),
 )

{- | Execute directory listing.

Reads directory contents and returns metadata for each entry.
When a file sandbox is configured, the directory path is validated
against the sandbox before listing, matching the same behaviour as
read-file-range. Relative paths are supported.
-}
executeListDirectory ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Bool ->
    Bool ->
    [Text] ->
    IO (Either QueryError Aeson.Value)
executeListDirectory tracer toolbox path doRecurse doHidden namePatterns = do
    runTracer tracer $ SystemInfoRequestedTrace "list-directory"
    if SystemToolListDirectory `notElem` toolbox.toolboxCapabilities
        then pure $ Left $ CapabilityNotEnabledError "list-directory"
        else case toolbox.toolboxFileSandbox of
            Just sandbox -> do
                accessResult <- validateFileRead sandbox path
                case accessResult of
                    AccessDenied err ->
                        pure $ Left $ SystemInfoError $ "Access denied: " <> Text.pack (show err)
                    AccessGranted ->
                        proceedWithListing path doRecurse doHidden namePatterns
            Nothing ->
                proceedWithListing path doRecurse doHidden namePatterns

proceedWithListing ::
    FilePath ->
    Bool ->
    Bool ->
    [Text] ->
    IO (Either QueryError Aeson.Value)
proceedWithListing path doRecurse doHidden namePatterns = do
    isDir <- doesDirectoryExist path
    if not isDir
        then pure $ Left $ SystemInfoError $ "Not a directory: " <> Text.pack path
        else do
            result <- try $ collectEntries path doRecurse doHidden namePatterns
            case result of
                Left (e :: SomeException) ->
                    pure $ Left $ SystemInfoError $ Text.pack $ show e
                Right entries ->
                    pure $
                        Right $
                            Aeson.object
                                [ "path" Aeson..= path
                                , "entries" Aeson..= map Aeson.toJSON entries
                                , "entryCount" Aeson..= length entries
                                , "recursive" Aeson..= doRecurse
                                ]

collectEntries ::
    FilePath ->
    Bool ->
    Bool ->
    [Text] ->
    IO [FileEntry]
collectEntries dir doRecurse doHidden namePatterns = do
    names <- listDirectory dir
    let paths = map (dir </>) names
    let visible = if doHidden then paths else filter (not . isHiddenFile . takeFileName) paths
    let op =
            ListDirectoryOp
                { targetPath = dir
                , filters = map NameFilter namePatterns
                , recursive = doRecurse
                , includeHidden = doHidden
                }
    entries <- concat <$> mapM (processEntry op doRecurse) visible
    pure entries

nameFiltersOf :: [FileFilter] -> [Text]
nameFiltersOf fs = [p | NameFilter p <- fs]

processEntry :: ListDirectoryOp -> Bool -> FilePath -> IO [FileEntry]
processEntry op doRecurse entryPath = do
    isDir <- doesDirectoryExist entryPath
    mtime <- getModificationTime entryPath
    perms <- getPermissions entryPath
    let ftype = if isDir then Directory else File
    let entry =
            FileEntry
                { fileEntryPath = entryPath
                , fileEntryType = ftype
                , fileEntrySize = Nothing
                , fileEntryModifiedTime = mtime
                , fileEntryPermissions = formatFilePermissions perms ftype
                }
    mFiltered <- applyFilters op entry
    let thisEntry = maybe [] (: []) mFiltered
    children <-
        if isDir && doRecurse
            then collectEntries entryPath doRecurse (op.includeHidden) (nameFiltersOf op.filters)
            else pure []
    pure (thisEntry ++ children)
