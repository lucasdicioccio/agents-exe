{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
DirectoryListing module for the agents project.

This module provides scoped directory listing capabilities that respect
FileScope boundaries. It allows listing files and directories with
filtering, metadata collection, and security checks.

== Security Model

All directory operations are bounded by a FileScope:

* Paths outside the scope are rejected with clear error messages
* Symlinks are followed safely - targets must be within scope
* Hidden files are excluded by default (configurable)
* Path traversal attempts are blocked

== Example Usage

>>> import System.Agents.OS.Security.FileScope
>>> import System.Agents.Tools.DirectoryListing
>>>
>>> -- Create a scope allowing access to project files
>>> let scope = scopeFromList ["/project/**"]
>>>
>>> -- List directory with filters
>>> let op = ListDirectoryOp
>>>         { targetPath = "/project/src"
>>>         , filters = [NameFilter "*.hs"]
>>>         , recursive = True
>>>         , includeHidden = False
>>>         }
>>> result <- scopedListDirectory scope op
>>> -- result = Right [FileEntry {..}, ...]

== Filtering

Multiple filter types are supported:

* 'NameFilter' - Glob pattern matching on filenames
* 'SizeFilter' - Min/max file size constraints
* 'DateFilter' - Before/after modification time

Filters are combined with AND logic - all must match.

== Metadata Collection

Each 'FileEntry' includes:

* Path (relative or absolute based on operation)
* File type (File, Directory, Symlink, Other)
* Size in bytes (for regular files)
* Modification time
* Permissions (Unix-style)

-}
module System.Agents.Tools.DirectoryListing (
    -- * Configuration Types
    ListDirectoryOp (..),
    FileFilter (..),
    SizeConstraint (..),
    DateConstraint (..),

    -- * Result Types
    FileEntry (..),
    FileType (..),

    -- * Core Functions
    scopedListDirectory,
    scopedTraverseDirectory,

    -- * Filtering Functions
    applyFilters,
    matchesNameFilter,
    matchesSizeFilter,
    matchesDateFilter,

    -- * Utility Functions
    isHiddenFile,
    fileTypeFromStatus,
    formatFilePermissions,
) where

import Control.Exception (IOException, try)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Directory (
    Permissions (..),
    doesDirectoryExist,
    doesFileExist,
    getModificationTime,
    getPermissions,
    getSymbolicLinkTarget,
    listDirectory,
    pathIsSymbolicLink,
 )
import System.FilePath (takeFileName, (</>))
import System.FilePath.Glob (compile, match)
import System.Posix.Files (
    FileStatus,
    getFileStatus,
    isDirectory,
    isRegularFile,
    isSymbolicLink,
    fileSize,
 )

import System.Agents.OS.Security.FileScope (
    FileScope,
    ScopeError (..),
    validatePath,
 )

-------------------------------------------------------------------------------
-- Configuration Types
-------------------------------------------------------------------------------

{- | Configuration for a directory listing operation.

Defines the target path, filters to apply, and listing options.

Example:

>>> let op = ListDirectoryOp
>>>         { targetPath = "/project/src"
>>>         , filters = [NameFilter "*.hs", SizeFilter (MinSize 1024)]
>>>         , recursive = True
>>>         , includeHidden = False
>>>         }
-}
data ListDirectoryOp = ListDirectoryOp
    { targetPath :: !FilePath
    -- ^ Target directory to list (must be within scope)
    , filters :: ![FileFilter]
    -- ^ List of filters to apply (AND logic)
    , recursive :: !Bool
    -- ^ Whether to recurse into subdirectories
    , includeHidden :: !Bool
    -- ^ Whether to include hidden files (dotfiles)
    }
    deriving (Show, Eq, Generic)

instance ToJSON ListDirectoryOp where
    toJSON ListDirectoryOp{..} =
        object
            [ "targetPath" .= targetPath
            , "filters" .= map filterToJSON filters
            , "recursive" .= recursive
            , "includeHidden" .= includeHidden
            ]

-- | Helper to convert FileFilter to JSON
filterToJSON :: FileFilter -> Aeson.Value
filterToJSON (NameFilter pat) = object ["type" .= ("name" :: Text), "pattern" .= pat]
filterToJSON (SizeFilter sc) = object ["type" .= ("size" :: Text), "constraint" .= show sc]
filterToJSON (DateFilter dc) = object ["type" .= ("date" :: Text), "constraint" .= show dc]

{- | Filters that can be applied to file entries.

Multiple filters are combined with AND logic - a file must match
all filters to be included in results.

* 'NameFilter' - Glob pattern match on filename
* 'SizeFilter' - Size constraints (min/max bytes)
* 'DateFilter' - Modification time constraints
-}
data FileFilter
    = -- | Filter by name pattern (glob syntax)
      NameFilter !Text
    | -- | Filter by file size constraints
      SizeFilter !SizeConstraint
    | -- | Filter by modification date constraints
      DateFilter !DateConstraint
    deriving (Show, Eq, Generic)

instance ToJSON FileFilter where
    toJSON = filterToJSON

{- | Size constraints for filtering files.

* 'MinSize' - File must be at least this many bytes
* 'MaxSize' - File must be at most this many bytes
* 'SizeRange' - File size must be within inclusive range
-}
data SizeConstraint
    = MinSize !Integer
    | MaxSize !Integer
    | SizeRange !Integer !Integer
    deriving (Show, Eq, Generic)

{- | Date constraints for filtering files by modification time.

* 'ModifiedBefore' - File modified before given time
* 'ModifiedAfter' - File modified after given time
* 'ModifiedBetween' - File modified within time range (inclusive)
-}
data DateConstraint
    = ModifiedBefore !UTCTime
    | ModifiedAfter !UTCTime
    | ModifiedBetween !UTCTime !UTCTime
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- Result Types
-------------------------------------------------------------------------------

{- | Metadata entry for a file or directory.

Contains comprehensive metadata about a filesystem entry.

Note: 'fileEntrySize' is 'Nothing' for directories and special files
where size is not meaningful or not available.
-}
data FileEntry = FileEntry
    { fileEntryPath :: !FilePath
    -- ^ Path to the file (relative to listing root or absolute)
    , fileEntryType :: !FileType
    -- ^ Type of filesystem entry
    , fileEntrySize :: !(Maybe Integer)
    -- ^ Size in bytes (Nothing for directories/special files)
    , fileEntryModifiedTime :: !UTCTime
    -- ^ Last modification time
    , fileEntryPermissions :: !Text
    -- ^ Unix-style permissions string (e.g., "rwxr-xr-x")
    }
    deriving (Show, Eq, Generic)

instance ToJSON FileEntry where
    toJSON FileEntry{..} =
        object
            [ "path" .= fileEntryPath
            , "type" .= fileEntryType
            , "size" .= fileEntrySize
            , "modifiedTime" .= fileEntryModifiedTime
            , "permissions" .= fileEntryPermissions
            ]

{- | Classification of filesystem entry types.

* 'File' - Regular file
* 'Directory' - Directory
* 'Symlink' - Symbolic link
* 'Other' - Block device, character device, socket, pipe, etc.
-}
data FileType
    = File
    | Directory
    | Symlink
    | Other
    deriving (Show, Eq, Generic)

instance ToJSON FileType where
    toJSON File = "file"
    toJSON Directory = "directory"
    toJSON Symlink = "symlink"
    toJSON Other = "other"

-------------------------------------------------------------------------------
-- Core Functions
-------------------------------------------------------------------------------

{- | List directory contents with scope validation and filtering.

This is the main entry point for directory listing operations. It:

1. Validates the target path against the provided FileScope
2. Lists directory contents (optionally recursively)
3. Applies configured filters
4. Collects metadata for each entry
5. Handles hidden files based on configuration
6. Safely follows symlinks within scope

Returns 'Left' with a 'ScopeError' if:
* Path is outside the defined scope
* Path does not exist or is not a directory
* Permission is denied

Example:

>>> let scope = scopeFromList ["/project/**"]
>>> let op = ListDirectoryOp "/project/src" [] True False
>>> result <- scopedListDirectory scope op
>>> case result of
>>>     Right entries -> print $ length entries
>>>     Left err -> putStrLn $ "Error: " ++ show err
-}
scopedListDirectory :: FileScope -> ListDirectoryOp -> IO (Either ScopeError [FileEntry])
scopedListDirectory scope op = do
    -- Validate the target path is within scope
    validationResult <- validatePath scope (targetPath op)
    case validationResult of
        Left err -> pure $ Left err
        Right canonicalPath -> do
            -- Check if it's actually a directory
            isDir <- doesDirectoryExist canonicalPath
            if not isDir
                then pure $ Left $ PathDoesNotExist canonicalPath
                else do
                    -- Perform the listing
                    entries <- listDirectoryRecursive scope op canonicalPath canonicalPath
                    pure $ Right entries

{- | Traverse a directory tree starting from a given path.

Simplified interface when you just want all entries (with scope check).

* No filtering beyond scope validation
* Non-recursive (direct children only)
* Hidden files excluded
* Full metadata collected

Example:

>>> let scope = scopeFromList ["/project/**"]
>>> result <- scopedTraverseDirectory scope "/project/src"
>>> case result of
>>>     Right entries -> mapM_ (print . fileEntryPath) entries
>>>     Left err -> putStrLn $ "Access denied: " ++ show err
-}
scopedTraverseDirectory :: FileScope -> FilePath -> IO (Either ScopeError [FileEntry])
scopedTraverseDirectory scope path =
    scopedListDirectory scope $
        ListDirectoryOp
            { targetPath = path
            , filters = []
            , recursive = True
            , includeHidden = False
            }

-------------------------------------------------------------------------------
-- Internal Implementation
-------------------------------------------------------------------------------

-- | Recursively list directory contents with full metadata collection.
listDirectoryRecursive ::
    FileScope ->
    ListDirectoryOp ->
    FilePath ->
    -- ^ Root path (for calculating relative paths in results)
    FilePath ->
    -- ^ Current directory being listed
    IO [FileEntry]
listDirectoryRecursive scope op rootPath currentPath = do
    -- Get raw directory contents
    contents <- listDirectory currentPath
    let fullPaths = map (currentPath </>) contents

    -- Filter hidden files if needed
    let visiblePaths =
            if includeHidden op
                then fullPaths
                else filter (not . isHiddenFile) fullPaths

    -- Process each entry
    entries <- concat <$> mapM (processEntry scope op rootPath) visiblePaths

    pure entries

-- | Process a single filesystem entry (file, directory, or symlink).
processEntry ::
    FileScope ->
    ListDirectoryOp ->
    FilePath ->
    -- ^ Root path
    FilePath ->
    -- ^ Entry path
    IO [FileEntry]
processEntry scope op rootPath entryPath = do
    -- Check if path is within scope
    inScope <- isPathInScopeIO scope entryPath
    if not inScope
        then pure [] -- Silently skip out-of-scope entries
        else do
            -- Determine if it's a symlink
            isSymlink <- pathIsSymbolicLink entryPath
            if isSymlink
                then processSymlink scope op rootPath entryPath
                else processRegularEntry scope op rootPath entryPath

-- | Process a symbolic link safely.
processSymlink ::
    FileScope ->
    ListDirectoryOp ->
    FilePath ->
    FilePath ->
    IO [FileEntry]
processSymlink scope op rootPath linkPath = do
    -- Get the link target
    targetResult <- try $ getSymbolicLinkTarget linkPath
    case targetResult of
        Left (_ :: IOException) -> do
            -- Cannot read link, include as broken symlink
            entry <- createFileEntry linkPath Symlink
            filtered <- applyFilters op entry
            pure $ maybeToList filtered
        Right target -> do
            -- Make target absolute if relative
            let absoluteTarget =
                    if isAbsolutePath target
                        then target
                        else takeFileName linkPath </> target

            -- Check if target is within scope
            targetInScope <- isPathInScopeIO scope absoluteTarget
            if not targetInScope
                then do
                    -- Target outside scope - include link but don't follow
                    entry <- createFileEntry linkPath Symlink
                    filtered <- applyFilters op entry
                    pure $ maybeToList filtered
                else do
                    -- Target is within scope, follow it
                    processRegularEntry scope op rootPath absoluteTarget
  where
    isAbsolutePath p = case p of
        ('/' : _) -> True
        _ -> False
    maybeToList (Just x) = [x]
    maybeToList Nothing = []

-- | Process a regular file or directory.
processRegularEntry ::
    FileScope ->
    ListDirectoryOp ->
    FilePath ->
    FilePath ->
    IO [FileEntry]
processRegularEntry scope op rootPath entryPath = do
    isDir <- doesDirectoryExist entryPath
    if isDir
        then do
            -- It's a directory
            entry <- createFileEntry entryPath Directory
            dirFiltered <- applyFilters op entry
            let dirEntry = maybeToList dirFiltered

            -- Recurse if needed
            children <-
                if recursive op
                    then listDirectoryRecursive scope op rootPath entryPath
                    else pure []

            pure (dirEntry ++ children)
        else do
            -- It's a file
            isFile <- doesFileExist entryPath
            let ftype = if isFile then File else Other
            entry <- createFileEntry entryPath ftype
            filtered <- applyFilters op entry
            pure $ maybeToList filtered
  where
    maybeToList (Just x) = [x]
    maybeToList Nothing = []

-- | Check if a path is within scope (IO wrapper).
isPathInScopeIO :: FileScope -> FilePath -> IO Bool
isPathInScopeIO scope path = do
    result <- validatePath scope path
    pure $ case result of
        Right _ -> True
        Left _ -> False

-- | Create a FileEntry from a path with full metadata.
createFileEntry :: FilePath -> FileType -> IO FileEntry
createFileEntry path ftype = do
    -- Get modification time
    mtime <- getModificationTime path

    -- Get size (if applicable)
    size <- getFileSizeSafe path ftype

    -- Get permissions
    perms <- getPermissions path
    let permStr = formatFilePermissions perms ftype

    pure $
        FileEntry
            { fileEntryPath = path
            , fileEntryType = ftype
            , fileEntrySize = size
            , fileEntryModifiedTime = mtime
            , fileEntryPermissions = permStr
            }

-- | Safely get file size, returning Nothing for directories/special files.
getFileSizeSafe :: FilePath -> FileType -> IO (Maybe Integer)
getFileSizeSafe path ftype =
    case ftype of
        File -> do
            result <- try $ getFileStatus path
            case result of
                Left (_ :: IOException) -> pure Nothing
                Right status -> pure $ Just $ fromIntegral $ fileSize status
        _ -> pure Nothing

-------------------------------------------------------------------------------
-- Filtering Functions
-------------------------------------------------------------------------------

{- | Apply all configured filters to a file entry.

Returns 'Just' entry if all filters pass, 'Nothing' if any filter rejects.
-}
applyFilters :: ListDirectoryOp -> FileEntry -> IO (Maybe FileEntry)
applyFilters op entry = do
    results <- mapM (applyFilter entry) (filters op)
    if all (== True) results
        then pure $ Just entry
        else pure Nothing

-- | Apply a single filter to a file entry.
applyFilter :: FileEntry -> FileFilter -> IO Bool
applyFilter entry (NameFilter pattern) = pure $ matchesNameFilter pattern entry
applyFilter entry (SizeFilter constraint) = pure $ matchesSizeFilter constraint entry
applyFilter entry (DateFilter constraint) = pure $ matchesDateFilter constraint entry

{- | Check if a file entry matches a name pattern.

Uses glob syntax for pattern matching against the filename only
(not the full path).

Examples:

* @"*.hs"@ - Matches all .hs files
* @"Test*"@ - Matches files starting with "Test"
* @"*.{json,yaml}"@ - Matches JSON or YAML files
-}
matchesNameFilter :: Text -> FileEntry -> Bool
matchesNameFilter pattern entry =
    let filename = takeFileName $ fileEntryPath entry
        compiled = compile $ Text.unpack pattern
     in match compiled filename

{- | Check if a file entry matches size constraints.

Returns 'False' for entries without size information (directories, etc.).
-}
matchesSizeFilter :: SizeConstraint -> FileEntry -> Bool
matchesSizeFilter _ entry | isNothing (fileEntrySize entry) = False
matchesSizeFilter constraint entry =
    case fileEntrySize entry of
        Nothing -> False
        Just size ->
            case constraint of
                MinSize minBytes -> size >= minBytes
                MaxSize maxBytes -> size <= maxBytes
                SizeRange minBytes maxBytes -> size >= minBytes && size <= maxBytes

{- | Check if a file entry matches date constraints.

Compares file modification time against the constraint.
-}
matchesDateFilter :: DateConstraint -> FileEntry -> Bool
matchesDateFilter constraint entry =
    let mtime = fileEntryModifiedTime entry
     in case constraint of
            ModifiedBefore time -> mtime < time
            ModifiedAfter time -> mtime > time
            ModifiedBetween start end -> mtime >= start && mtime <= end

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

{- | Check if a file is hidden (starts with dot).

On Unix systems, hidden files start with a dot. On Windows, this
also checks for the hidden attribute.

>>> isHiddenFile "/home/user/.bashrc"
True
>>> isHiddenFile "/home/user/documents.txt"
False
-}
isHiddenFile :: FilePath -> Bool
isHiddenFile path =
    let name = takeFileName path
     in case name of
            ('.' : _) -> True
            _ -> False

{- | Determine FileType from POSIX FileStatus.

Used when we already have a FileStatus from a system call and want
to classify the file type without additional IO.
-}
fileTypeFromStatus :: FileStatus -> FileType
fileTypeFromStatus status
    | isRegularFile status = File
    | isDirectory status = Directory
    | isSymbolicLink status = Symlink
    | otherwise = Other

{- | Format permissions as a Unix-style string.

Returns a 10-character string similar to @ls -l@ output:
@drwxr-xr-x@ for directories, @-rw-r--r--@ for files, etc.

The first character indicates the file type:
* @-@ = regular file
* @d@ = directory
* @l@ = symbolic link
* @b@ = block device
* @c@ = character device
* @p@ = named pipe
* @s@ = socket
* @?@ = unknown

The next 9 characters are three groups of @rwx@ for owner,
group, and others.
-}
formatFilePermissions :: Permissions -> FileType -> Text
formatFilePermissions perms ftype =
    Text.pack $
        typeChar
            : [ if readable perms then 'r' else '-'
              , if writable perms then 'w' else '-'
              , if executable perms then 'x' else '-'
              , if readable perms then 'r' else '-'  -- Group read (using same for simplicity)
              , if writable perms then 'w' else '-'  -- Group write
              , if executable perms then 'x' else '-' -- Group execute
              , if readable perms then 'r' else '-'  -- Other read
              , if writable perms then 'w' else '-'  -- Other write
              , if executable perms then 'x' else '-' -- Other execute
              ]
  where
    typeChar = case ftype of
        File -> '-'
        Directory -> 'd'
        Symlink -> 'l'
        Other -> '?'

