{-# LANGUAGE OverloadedStrings #-}

{- |
Directory listing and traversal capabilities for the DeveloperToolbox.

This module implements the list-directory and traverse-directory capabilities,
providing scoped directory access that respects FileScope boundaries.
-}
module System.Agents.Tools.DeveloperToolbox.Directory (
    executeListDirectory,
    executeTraverseDirectory,
) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.OS.Security.FileScope (scopeFromList)
import System.Agents.Tools.DeveloperToolbox.Types
import System.Agents.Tools.DirectoryListing (
    FileFilter (NameFilter),
    ListDirectoryOp (..),
    scopedListDirectory,
    scopedTraverseDirectory,
 )

-- | List directory contents with optional filtering and recursive traversal.
executeListDirectory ::
    Toolbox ->
    FilePath ->
    Bool ->
    Bool ->
    [Text] ->
    IO (Either DeveloperToolError DirectoryListingResult)
executeListDirectory _toolbox path doRecurse doHidden namePatterns = do
    let scope = scopeFromList []
        op =
            ListDirectoryOp
                { targetPath = path
                , filters = map NameFilter namePatterns
                , recursive = doRecurse
                , includeHidden = doHidden
                }
    result <- scopedListDirectory scope op
    case result of
        Left err ->
            pure $ Left $ DirectoryScopeError $ Text.pack $ show err
        Right entries ->
            pure $
                Right $
                    DirectoryListingResult
                        { listingPath = path
                        , listingEntries = map Aeson.toJSON entries
                        , listingEntryCount = length entries
                        , listingRecursive = doRecurse
                        }

-- | Recursively traverse a directory tree within scope.
executeTraverseDirectory ::
    Toolbox ->
    FilePath ->
    IO (Either DeveloperToolError DirectoryListingResult)
executeTraverseDirectory _toolbox path = do
    let scope = scopeFromList []
    result <- scopedTraverseDirectory scope path
    case result of
        Left err ->
            pure $ Left $ DirectoryScopeError $ Text.pack $ show err
        Right entries ->
            pure $
                Right $
                    DirectoryListingResult
                        { listingPath = path
                        , listingEntries = map Aeson.toJSON entries
                        , listingEntryCount = length entries
                        , listingRecursive = True
                        }
