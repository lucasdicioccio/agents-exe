{-# LANGUAGE OverloadedStrings #-}

{- |
Directory listing and traversal capabilities for the DeveloperToolbox.

This module implements the list-directory and traverse-directory capabilities.
When a file sandbox is configured, the requested directory path is validated
against the sandbox before listing; the listing itself is then performed
without an additional FileScope boundary (the sandbox is the authority).
-}
module System.Agents.Tools.DeveloperToolbox.Directory (
    executeListDirectory,
    executeTraverseDirectory,
) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.FileSandbox (AccessResult (..), validateFileRead)
import System.Agents.Tools.DeveloperToolbox.Types
import System.Agents.Tools.DirectoryListing (
    FileFilter (NameFilter),
    ListDirectoryOp (..),
    listDirectory,
    traverseDirectory,
 )

-- | List directory contents with optional filtering and recursive traversal.
executeListDirectory ::
    Toolbox ->
    FilePath ->
    Bool ->
    Bool ->
    [Text] ->
    IO (Either DeveloperToolError DirectoryListingResult)
executeListDirectory toolbox path doRecurse doHidden namePatterns = do
    accessGranted <- checkSandbox toolbox path
    if not accessGranted
        then pure $ Left $ DirectoryScopeError "Access denied by file sandbox"
        else do
            let op =
                    ListDirectoryOp
                        { targetPath = path
                        , filters = map NameFilter namePatterns
                        , recursive = doRecurse
                        , includeHidden = doHidden
                        }
            result <- listDirectory op
            case result of
                Left err ->
                    pure $ Left $ DirectoryScopeError $ Text.pack $ show err
                Right entries ->
                    pure $ Right $ DirectoryListingResult
                        { listingPath = path
                        , listingEntries = map Aeson.toJSON entries
                        , listingEntryCount = length entries
                        , listingRecursive = doRecurse
                        }

-- | Recursively traverse a directory tree.
executeTraverseDirectory ::
    Toolbox ->
    FilePath ->
    IO (Either DeveloperToolError DirectoryListingResult)
executeTraverseDirectory toolbox path = do
    accessGranted <- checkSandbox toolbox path
    if not accessGranted
        then pure $ Left $ DirectoryScopeError "Access denied by file sandbox"
        else do
            result <- traverseDirectory path
            case result of
                Left err ->
                    pure $ Left $ DirectoryScopeError $ Text.pack $ show err
                Right entries ->
                    pure $ Right $ DirectoryListingResult
                        { listingPath = path
                        , listingEntries = map Aeson.toJSON entries
                        , listingEntryCount = length entries
                        , listingRecursive = True
                        }

-- | Validate a path against the toolbox's file sandbox, if configured.
checkSandbox :: Toolbox -> FilePath -> IO Bool
checkSandbox toolbox path =
    case toolboxFileSandbox toolbox of
        Nothing -> pure True
        Just sandbox -> do
            accessResult <- validateFileRead sandbox path
            pure $ case accessResult of
                AccessGranted -> True
                AccessDenied _ -> False
