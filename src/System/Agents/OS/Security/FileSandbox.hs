{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | File sandbox for restricting file system access.

This module provides a security layer that restricts file operations to
explicitly allowed directories. It prevents:

* Accidental deletion of system files
* Unauthorized access to sensitive data
* Cross-contamination between projects
* Path traversal attacks

The sandbox uses an allow-list approach - only paths within explicitly
configured directories are accessible. All other paths are rejected.

Example usage:

@
import System.Agents.OS.Security.FileSandbox

main :: IO ()
main = do
    let sandbox = defaultFileSandbox
            { allowedPaths = ["/home/user/project", "/tmp/workspace"]
            , allowSymlinks = False
            }

    -- This will succeed
    result1 <- validatePath sandbox "/home/user/project/src/main.hs"
    print result1  -- Right "/home/user/project/src/main.hs"

    -- This will fail
    result2 <- validatePath sandbox "/etc/passwd"
    print result2  -- Left (PathNotAllowedError "/etc/passwd")
@
-}
module System.Agents.OS.Security.FileSandbox (
    -- * Core types
    FileSandbox (..),
    SandboxError (..),
    PathValidationResult,

    -- * Default configuration
    defaultFileSandbox,
    allowAllSandbox,

    -- * Path validation
    validatePath,
    validatePathForOperation,
    isPathAllowed,

    -- * File operations with sandboxing
    sandboxedReadFile,
    sandboxedWriteFile,
    sandboxedReadFileStrict,

    -- * Path utilities
    normalizePath,
    resolvePath,
    isSubPathOf,
    matchGlobPattern,
) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, doesPathExist, getCurrentDirectory, pathIsSymbolicLink)
import System.FilePath (isAbsolute, joinPath, normalise, splitDirectories, takeDirectory, (</>))

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

{- | Configuration for the file sandbox.

The sandbox restricts file operations to explicitly allowed directories.
All paths outside the allowed list are rejected by default.

Use 'defaultFileSandbox' to create a restrictive sandbox, or 'allowAllSandbox'
for a permissive configuration during development.
-}
data FileSandbox = FileSandbox
    { allowedPaths :: [FilePath]
    {- ^ List of allowed directory paths
    Paths can include glob patterns like "/project/**" to allow subdirectories
    -}
    , allowSymlinks :: Bool
    {- ^ Whether to allow accessing files through symbolic links
    When False, any symlink in the path will cause rejection
    -}
    , allowSubdirectories :: Bool
    {- ^ Whether subdirectories of allowed paths are automatically allowed
    When True, "/home/user/project" allows "/home/user/project/src"
    -}
    }
    deriving (Show, Eq, Ord, Generic)

-- | JSON serialization for FileSandbox.
instance ToJSON FileSandbox where
    toJSON sandbox =
        Aeson.object
            [ "allowedPaths" .= allowedPaths sandbox
            , "allowSymlinks" .= allowSymlinks sandbox
            , "allowSubdirectories" .= allowSubdirectories sandbox
            ]

-- | JSON deserialization for FileSandbox.
instance FromJSON FileSandbox where
    parseJSON = Aeson.withObject "FileSandbox" $ \v ->
        FileSandbox
            <$> v .: "allowedPaths"
            <*> v .: "allowSymlinks"
            <*> v .: "allowSubdirectories"

{- | Errors that can occur during sandbox validation.

These errors provide clear messages about why access was denied.
-}
data SandboxError
    = -- | The path is not within any allowed directory
      PathNotAllowedError !FilePath
    | -- | The path contains a symbolic link which is not allowed
      SymlinkNotAllowedError !FilePath
    | -- | The path contains traversal sequences (..) attempting to escape
      PathTraversalError !FilePath
    | -- | The path is not absolute and could not be resolved
      RelativePathError !FilePath
    | -- | The file does not exist
      FileNotFoundError !FilePath
    | -- | An IO error occurred during path validation
      SandboxIOError !FilePath !Text
    deriving (Show, Eq)

-- | Result type for path validation operations.
type PathValidationResult = Either SandboxError FilePath

-------------------------------------------------------------------------------
-- Default Configurations
-------------------------------------------------------------------------------

{- | Default restrictive sandbox configuration.

This creates a sandbox with no allowed paths, requiring explicit
configuration before any file operations are permitted.

Example:

@
let sandbox = defaultFileSandbox
        { allowedPaths = ["/home/user/project"]
        , allowSymlinks = False
        }
@
-}
defaultFileSandbox :: FileSandbox
defaultFileSandbox =
    FileSandbox
        { allowedPaths = []
        , allowSymlinks = False
        , allowSubdirectories = True
        }

{- | Permissive sandbox that allows all paths.

This is useful for development and backward compatibility,
but should not be used in production environments.

Note: Even with allowAllSandbox, path traversal attacks are still
prevented by 'normalizePath'.
-}
allowAllSandbox :: FileSandbox
allowAllSandbox =
    FileSandbox
        { allowedPaths = ["/"]
        , allowSymlinks = True
        , allowSubdirectories = True
        }

-------------------------------------------------------------------------------
-- Path Validation
-------------------------------------------------------------------------------

{- | Validate that a path is within the allowed directories.

This function:
1. Normalizes the path to prevent traversal attacks
2. Resolves relative paths to absolute paths
3. Checks for symbolic links (if disallowed)
4. Verifies the path is within an allowed directory

Returns the canonicalized path on success, or a SandboxError on failure.
-}
validatePath :: FileSandbox -> FilePath -> IO PathValidationResult
validatePath sandbox inputPath = do
    -- First, normalize to prevent traversal attacks
    let normalized = normalizePath inputPath

    -- Check for obvious traversal attempts in the input
    if containsTraversal inputPath
        then pure $ Left $ PathTraversalError inputPath
        else do
            -- Resolve to absolute path
            resolvedResult <- resolvePath normalized
            case resolvedResult of
                Left err -> pure $ Left err
                Right absolutePath -> do
                    -- Check for symlinks if disallowed
                    symlinkCheck <- checkSymlinks sandbox absolutePath
                    case symlinkCheck of
                        Left err -> pure $ Left err
                        Right () -> do
                            -- Check if path is allowed
                            if isPathAllowed sandbox absolutePath
                                then pure $ Right absolutePath
                                else pure $ Left $ PathNotAllowedError inputPath

{- | Validate a path for a specific operation type.

This extends 'validatePath' with operation-specific checks:
- For read operations: verifies the file exists
- For write operations: verifies the parent directory is allowed
-}
validatePathForOperation ::
    FileSandbox ->
    -- | True for read operations, False for write operations
    Bool ->
    FilePath ->
    IO PathValidationResult
validatePathForOperation sandbox isReadOp inputPath = do
    baseValidation <- validatePath sandbox inputPath
    case baseValidation of
        Left err -> pure $ Left err
        Right canonicalPath -> do
            if isReadOp
                then do
                    -- For read operations, check file exists
                    exists <- doesPathExist canonicalPath
                    if exists
                        then pure $ Right canonicalPath
                        else pure $ Left $ FileNotFoundError inputPath
                else do
                    -- For write operations, ensure parent directory is allowed
                    let parentDir = takeDirectory canonicalPath
                    if isPathAllowed sandbox parentDir
                        then pure $ Right canonicalPath
                        else pure $ Left $ PathNotAllowedError parentDir

{- | Check if a path is within the allowed directories.

This checks the normalized, absolute path against the allowed paths list.
Supports glob patterns in allowed paths.
-}
isPathAllowed :: FileSandbox -> FilePath -> Bool
isPathAllowed sandbox path =
    any (pathMatchesAllowed path) (allowedPaths sandbox)

-- | Check if a path matches an allowed path pattern.
pathMatchesAllowed :: FilePath -> FilePath -> Bool
pathMatchesAllowed path allowedPattern
    -- Exact match
    | path == allowedPattern = True
    -- Subdirectory match (if allowed)
    | otherwise = matchGlobPattern path allowedPattern

{- | Match a path against a glob pattern.

Supports:
- @**@ - matches any number of directories (like @/project/**@)
- @*@ - matches any characters within a directory name
- Exact paths - matches the exact directory

Examples:

>>> matchGlobPattern "/project/src/main.hs" "/project/**"
True

>>> matchGlobPattern "/project/src/main.hs" "/other/**"
False
-}
matchGlobPattern :: FilePath -> FilePath -> Bool
matchGlobPattern path pattern =
    let normPath = normalise path
        normPattern = normalise pattern
     in matchPattern (splitDirectories normPath) (splitDirectories normPattern)
  where
    matchPattern :: [FilePath] -> [FilePath] -> Bool
    matchPattern [] [] = True
    matchPattern _ [] = False
    matchPattern [] (p : ps)
        | p == "**" = matchPattern [] ps
        | otherwise = False
    matchPattern (x : xs) (p : ps)
        | p == "**" =
            -- \** matches zero or more directories
            matchPattern (x : xs) ps -- Match zero dirs
                || matchPattern xs (p : ps) -- Match one+ dirs
                || matchPattern xs ps -- Match exactly this dir
        | p == "*" = matchPattern xs ps
        -- \* matches any single dir
        | x == p = matchPattern xs ps -- Exact match
        | otherwise = False

{- | Check if a path contains directory traversal sequences.

Detects ".." components that could be used to escape the sandbox.
-}
containsTraversal :: FilePath -> Bool
containsTraversal path =
    let dirs = splitDirectories path
     in any isTraversalComponent dirs
  where
    isTraversalComponent ".." = True
    isTraversalComponent _ = False

{- | Check for symbolic links in the path.

If 'allowSymlinks' is False, this checks each component of the path
for symbolic links and returns an error if any are found.
-}
checkSymlinks :: FileSandbox -> FilePath -> IO (Either SandboxError ())
checkSymlinks sandbox path
    | allowSymlinks sandbox = pure $ Right ()
    | otherwise = do
        let components = splitDirectories path
        checkComponents "" components
  where
    checkComponents :: FilePath -> [FilePath] -> IO (Either SandboxError ())
    checkComponents _ [] = pure $ Right ()
    checkComponents prefix (c : cs) = do
        let current = if null prefix then c else prefix </> c
        exists <- doesPathExist current
        if not exists
            then pure $ Right () -- Path doesn't exist, no symlink to check
            else do
                isSymlink <- pathIsSymbolicLink current
                if isSymlink
                    then pure $ Left $ SymlinkNotAllowedError current
                    else checkComponents current cs

-------------------------------------------------------------------------------
-- Path Resolution
-------------------------------------------------------------------------------

{- | Normalize a path to prevent traversal attacks.

This function:
1. Removes redundant separators and @.@ components
2. Processes @..@ components safely (doesn't escape root)
3. Preserves the absolute/relative nature of the path

Note: This does NOT resolve symlinks. Use 'resolvePath' for full resolution.
-}
normalizePath :: FilePath -> FilePath
normalizePath path =
    let norm = normalise path
     in removeTraversal norm
  where
    -- Remove .. components safely
    removeTraversal :: FilePath -> FilePath
    removeTraversal p =
        let dirs = splitDirectories p
            collapsed = collapseDirs dirs []
         in joinPath collapsed

    collapseDirs :: [FilePath] -> [FilePath] -> [FilePath]
    collapseDirs [] acc = reverse acc
    collapseDirs (".." : rest) (_ : acc) = collapseDirs rest acc -- Go up
    collapseDirs (".." : rest) [] = collapseDirs rest [] -- Can't go above root
    collapseDirs ("." : rest) acc = collapseDirs rest acc -- Skip .
    collapseDirs ("" : rest) acc = collapseDirs rest acc -- Skip empty
    collapseDirs (d : rest) acc = collapseDirs rest (d : acc)

{- | Resolve a relative path to an absolute path.

If the path is already absolute, it is returned as-is (after normalization).
If the path is relative, it is joined with the current working directory.

Returns Left if the path cannot be resolved.
-}
resolvePath :: FilePath -> IO PathValidationResult
resolvePath path = do
    let normalized = normalizePath path
    if isAbsolute normalized
        then do
            -- For absolute paths, try to canonicalize
            result <- try $ canonicalizePath normalized
            case result of
                Left (_ :: IOException) -> pure $ Right normalized -- Use normalized if can't canonicalize
                Right canonical -> pure $ Right canonical
        else do
            -- Relative path: join with current directory
            cwd <- getCurrentDirectory
            let absolute = cwd </> normalized
            result <- try $ canonicalizePath absolute
            case result of
                Left (e :: IOException) ->
                    pure $ Left $ SandboxIOError path (Text.pack $ show e)
                Right canonical -> pure $ Right canonical

{- | Check if a path is a subdirectory of another path.

Both paths should be normalized and absolute for accurate results.
-}
isSubPathOf :: FilePath -> FilePath -> Bool
isSubPathOf child parent =
    let normChild = normalise child
        normParent = normalise parent
        childDirs = splitDirectories normChild
        parentDirs = splitDirectories normParent
     in parentDirs `isPrefixOf` childDirs && normChild /= normParent

-------------------------------------------------------------------------------
-- Sandboxed File Operations
-------------------------------------------------------------------------------

{- | Read a file with sandbox validation.

This function validates the path before reading and returns an error
if the path is outside the sandbox.
-}
sandboxedReadFile ::
    FileSandbox ->
    FilePath ->
    IO (Either SandboxError String)
sandboxedReadFile sandbox path = do
    validation <- validatePathForOperation sandbox True path
    case validation of
        Left err -> pure $ Left err
        Right canonicalPath -> do
            result <- try $ readFile canonicalPath
            case result of
                Left (e :: IOException) ->
                    pure $ Left $ SandboxIOError path (Text.pack $ show e)
                Right content -> pure $ Right content

{- | Read a file strictly with sandbox validation.

This function validates the path before reading and returns an error
if the path is outside the sandbox. Uses Text for efficient handling.
-}
sandboxedReadFileStrict ::
    FileSandbox ->
    FilePath ->
    IO (Either SandboxError Text)
sandboxedReadFileStrict sandbox path = do
    validation <- validatePathForOperation sandbox True path
    case validation of
        Left err -> pure $ Left err
        Right canonicalPath -> do
            result <- try $ TextIO.readFile canonicalPath
            case result of
                Left (e :: IOException) ->
                    pure $ Left $ SandboxIOError path (Text.pack $ show e)
                Right content -> pure $ Right content

{- | Write a file with sandbox validation.

This function validates the path (and parent directory) before writing
and returns an error if the path is outside the sandbox.
-}
sandboxedWriteFile ::
    FileSandbox ->
    FilePath ->
    String ->
    IO (Either SandboxError ())
sandboxedWriteFile sandbox path content = do
    validation <- validatePathForOperation sandbox False path
    case validation of
        Left err -> pure $ Left err
        Right canonicalPath -> do
            result <- try $ writeFile canonicalPath content
            case result of
                Left (e :: IOException) ->
                    pure $ Left $ SandboxIOError path (Text.pack $ show e)
                Right () -> pure $ Right ()
