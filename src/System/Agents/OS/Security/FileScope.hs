{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | File scope validation for sandboxed file operations.

This module provides filesystem operation scoping that validates all file
accesses against a configurable allow-list, supporting glob patterns and
line range restrictions. It restricts which files an assistant can access,
similar to the Lua sandbox file restrictions.

Key security features:
* Path canonicalization to prevent symlink traversal attacks
* Absolute path requirement (or paths are made absolute relative to a base)
* Glob pattern matching for flexible path specification
* Deny by default: operations outside scope are rejected
* Runtime validation: scope enforcement at operation time

Example configuration:

@
FileScope
    { allowedPatterns = ["/project/src/**/*.hs", "/project/docs/*.md"]
    , allowRangeOps = True
    , allowCreate = False
    }
@

This allows:
- Read/write any .hs file in /project/src/ recursively
- Read/write any .md file in /project/docs/
- Range operations (read-file-range, write-file-range)
- No creating new files (must exist)

Path traversal attack prevention uses canonicalizePath which resolves
symlinks, making it resistant to traversal attacks through symlinks.
-}
module System.Agents.OS.Security.FileScope (
    -- * Core types
    FileScope (..),
    GlobPattern (..),
    ScopeError (..),

    -- * Validation
    validateFileAccess,
    validateFileAccessRaw,
    validatePathAgainstPatterns,

    -- * Scoped operations
    scopedReadFile,
    scopedWriteFile,

    -- * Pattern matching
    matchGlob,
    compileGlob,

    -- * Default configurations
    defaultFileScope,
    unrestrictedFileScope,
    denyAllFileScope,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (isAbsolute, normalise, (</>))

-------------------------------------------------------------------------------
-- Core Types
-------------------------------------------------------------------------------

-- | A glob pattern for matching file paths.
-- Supports:
-- - @*@ matches any sequence of characters except path separator
-- - @**@ matches any sequence of characters including path separators (recursive)
-- - @?@ matches any single character
-- - @[abc]@ matches any character in the set
newtype GlobPattern = GlobPattern {unGlobPattern :: String}
    deriving (Show, Eq)

{- | File scope configuration for sandboxed file operations.

Security defaults:
* Empty allowedPatterns means NO file access (secure default)
* All paths are canonicalized before validation
* Range operations can be disabled independently
-}
data FileScope = FileScope
    { allowedPatterns :: [GlobPattern]
    -- ^ List of allowed glob patterns. If empty, NO paths are allowed.
    , allowRangeOps :: Bool
    -- ^ Whether range-based read/write operations are allowed
    , allowCreate :: Bool
    -- ^ Whether creating new files is allowed (vs. only existing files)
    }
    deriving (Show, Eq)

-- | Errors that can occur during scope validation.
data ScopeError
    = -- | Path is not in the allowed patterns
      PathNotAllowed !FilePath
    | -- | Path is not absolute
      PathNotAbsolute !FilePath
    | -- | Path escapes the sandbox through traversal
      PathEscapesSandbox !FilePath
    | -- | Creating new files is not allowed
      CreateNotAllowed !FilePath
    | -- | Range operations are not allowed
      RangeOpsNotAllowed
    | -- | File does not exist and create is not allowed
      FileNotFound !FilePath
    | -- | IO error during validation
      ScopeIOError !FilePath !String
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Default Configurations
-------------------------------------------------------------------------------

-- | Default secure file scope: no access allowed.
-- This is the most restrictive configuration.
denyAllFileScope :: FileScope
denyAllFileScope =
    FileScope
        { allowedPatterns = []
        , allowRangeOps = False
        , allowCreate = False
        }

-- | Unrestricted file scope: allows all operations.
-- Use with caution - this removes all file access restrictions.
unrestrictedFileScope :: FileScope
unrestrictedFileScope =
    FileScope
        { allowedPatterns = [GlobPattern "**/*"]
        , allowRangeOps = True
        , allowCreate = True
        }

-- | Default file scope: denies all access.
-- Users must explicitly configure allowed paths.
defaultFileScope :: FileScope
defaultFileScope = denyAllFileScope

-------------------------------------------------------------------------------
-- Path Validation
-------------------------------------------------------------------------------

{- | Validate a file path against the scope.

This function performs the following security checks:
1. Path must be absolute (or made absolute relative to base path)
2. Path is canonicalized (resolves symlinks, .., etc.)
3. Canonical path must match at least one allowed pattern
4. If allowCreate is False and file doesn't exist, returns error

Returns the canonical path if valid, or a ScopeError.
-}
validateFileAccess ::
    -- | Optional base path for resolving relative paths
    Maybe FilePath ->
    -- | File scope configuration
    FileScope ->
    -- | Path to validate
    FilePath ->
    -- | Whether the operation is a read (True) or write (False)
    Bool ->
    IO (Either ScopeError FilePath)
validateFileAccess mBasePath scope path isRead = do
    -- Make path absolute if needed
    let absPath = case mBasePath of
            Just base | not (isAbsolute path) -> base </> path
            _ -> path

    -- Path must be absolute
    if not (isAbsolute absPath)
        then pure $ Left $ PathNotAbsolute absPath
        else do
            -- Canonicalize to resolve symlinks, .., etc.
            canonical <- canonicalizePath absPath

            -- Check against allowed patterns
            if not (validatePathAgainstPatterns canonical (allowedPatterns scope))
                then pure $ Left $ PathNotAllowed canonical
                else do
                    -- For read operations, check if file exists
                    if isRead
                        then do
                            exists <- doesFileExist canonical
                            if exists
                                then pure $ Right canonical
                                else pure $ Left $ FileNotFound canonical
                        else pure $ Right canonical

-- | Validate file access without checking for file existence.
-- Useful for operations that will create the file.
validateFileAccessRaw ::
    Maybe FilePath ->
    FileScope ->
    FilePath ->
    IO (Either ScopeError FilePath)
validateFileAccessRaw mBasePath scope path = do
    -- Make path absolute if needed
    let absPath = case mBasePath of
            Just base | not (isAbsolute path) -> base </> path
            _ -> path

    -- Path must be absolute
    if not (isAbsolute absPath)
        then pure $ Left $ PathNotAbsolute absPath
        else do
            -- Canonicalize to resolve symlinks, .., etc.
            canonical <- canonicalizePath absPath

            -- Check against allowed patterns
            if not (validatePathAgainstPatterns canonical (allowedPatterns scope))
                then pure $ Left $ PathNotAllowed canonical
                else pure $ Right canonical

-- | Check if a path matches any of the allowed patterns.
validatePathAgainstPatterns :: FilePath -> [GlobPattern] -> Bool
validatePathAgainstPatterns _ [] = False
validatePathAgainstPatterns path patterns = any (matchGlob path) patterns

-------------------------------------------------------------------------------
-- Glob Pattern Matching
-------------------------------------------------------------------------------

{- | Compile a string into a GlobPattern.

Basic glob syntax:
- @*@ matches any sequence of characters except /
- @**@ matches any sequence of characters including /
- @?@ matches any single character
- @[abc]@ matches any character in the set
- @[a-z]@ matches any character in the range
- @\\*@, @\\?@, @\\[@ match literal characters
-}
compileGlob :: String -> GlobPattern
compileGlob = GlobPattern . normalise

{- | Match a file path against a glob pattern.

Examples:
>>> matchGlob "/project/src/Main.hs" (GlobPattern "/project/src/*.hs")
True

>>> matchGlob "/project/src/Utils/Helper.hs" (GlobPattern "/project/src/**/*.hs")
True

>>> matchGlob "/etc/passwd" (GlobPattern "/project/**/*")
False
-}
matchGlob :: FilePath -> GlobPattern -> Bool
matchGlob path (GlobPattern pattern) =
    let normPath = normalise path
        normPattern = normalise pattern
     in matchGlob' normPath normPattern

-- | Internal glob matching implementation.
-- Note: The top-level patterns handle empty strings, so path and pattern
-- are guaranteed to be non-empty in the case alternatives.
matchGlob' :: String -> String -> Bool
matchGlob' "" "" = True
matchGlob' _ "" = False
matchGlob' "" _ = False
matchGlob' path pattern =
    case pattern of
        -- Double asterisk matches any sequence of directories recursively
        p1 : p2 : '/' : rest | p1 == '*' && p2 == '*' ->
            -- Try matching ** against current position or skip to next /
            matchGlobRecursive path rest || matchGlob' path rest
        p1 : p2 : [] | p1 == '*' && p2 == '*' ->
            -- ** at end matches any remaining path
            True
        -- Single asterisk matches any chars except /
        '*' : rest ->
            matchGlobStar path rest
        -- Question mark matches any single character except /
        -- Path is guaranteed non-empty here due to top-level patterns
        '?' : rest ->
            case path of
                '/' : _ -> False
                _ : pathRest -> matchGlob' pathRest rest
        -- Character class
        '[' : rest ->
            case break (== ']') rest of
                (classDef, ']' : patternRest) ->
                    case path of
                        c : pathRest | c `matchesCharClass` classDef ->
                            matchGlob' pathRest patternRest
                        _ -> False
                _ -> matchGlob' path rest -- Invalid class, treat as literal
        -- Escaped character
        '\\' : c : rest ->
            case path of
                c' : pathRest | c == c' -> matchGlob' pathRest rest
                _ -> False
        -- Literal character match
        p : rest ->
            case path of
                c : pathRest | p == c -> matchGlob' pathRest rest
                _ -> False

-- | Match single * against path segment (doesn't cross /)
matchGlobStar :: String -> String -> Bool
matchGlobStar path pattern =
    -- Try matching * at different positions within the current segment
    let tryPositions p =
            case p of
                '/' : _ -> matchGlob' p pattern  -- Hit a /, try matching rest from here
                [] -> matchGlob' [] pattern  -- End of path, try matching empty
                (_ : cs) -> matchGlob' p pattern || tryPositions cs
     in tryPositions path

-- | Match a character against a character class definition.
matchesCharClass :: Char -> String -> Bool
matchesCharClass c classDef = any (matchesRange c) (parseRanges classDef)
  where
    parseRanges :: String -> [(Char, Char)]
    parseRanges [] = []
    parseRanges [x] = [(x, x)]
    parseRanges (x : '-' : y : rest) = (x, y) : parseRanges rest
    parseRanges (x : rest) = (x, x) : parseRanges rest

    matchesRange :: Char -> (Char, Char) -> Bool
    matchesRange ch (start, end) = ch >= start && ch <= end

-- | Match ** recursively through directories.
matchGlobRecursive :: String -> String -> Bool
matchGlobRecursive "" pattern = matchGlob' "" pattern
matchGlobRecursive path pattern =
    matchGlob' path pattern
        || case dropWhile (/= '/') path of
            '/' : rest -> matchGlobRecursive rest pattern
            _ -> False

-------------------------------------------------------------------------------
-- Scoped File Operations
-------------------------------------------------------------------------------

{- | Line range specification for scoped read operations.
-}
data LineRange = LineRange
    { rangeStart :: Int
    -- ^ 1-based start line (inclusive)
    , rangeEnd :: Int
    -- ^ 1-based end line (inclusive), 0 means "to end"
    }
    deriving (Show, Eq)

{- | Read a file with scope validation.

If range is specified and allowed, only reads those lines.
Returns the content and the number of lines read.
-}
scopedReadFile ::
    Maybe FilePath ->
    FileScope ->
    FilePath ->
    Maybe LineRange ->
    IO (Either ScopeError (Text, Int))
scopedReadFile mBasePath scope filePath mRange = do
    -- Validate path
    validation <- validateFileAccess mBasePath scope filePath True
    case validation of
        Left err -> pure $ Left err
        Right canonicalPath -> do
            -- Check if range operations are allowed
            case mRange of
                Just _ | not (allowRangeOps scope) ->
                    pure $ Left RangeOpsNotAllowed
                _ -> do
                    -- Read file
                    result <- try $ TextIO.readFile canonicalPath
                    case result of
                        Left (e :: SomeException) ->
                            pure $ Left $ ScopeIOError canonicalPath (show e)
                        Right content ->
                            case mRange of
                                Nothing ->
                                    pure $ Right (content, length $ Text.lines content)
                                Just (LineRange start end) ->
                                    let allLines = Text.lines content
                                        actualStart = max 1 start
                                        actualEnd = if end == 0 then length allLines else min end (length allLines)
                                        selectedLines =
                                            if actualStart > length allLines
                                                then []
                                                else take (actualEnd - actualStart + 1) $ drop (actualStart - 1) allLines
                                     in pure $ Right (Text.unlines selectedLines, length selectedLines)

{- | Write to a file with scope validation.

If range is specified and allowed, only writes to those lines.
For range writes, the content replaces the specified lines.
-}
scopedWriteFile ::
    Maybe FilePath ->
    FileScope ->
    FilePath ->
    Maybe LineRange ->
    Text ->
    IO (Either ScopeError Int)
scopedWriteFile mBasePath scope filePath mRange content = do
    -- Validate path (checks allowCreate)
    validation <- validateFileAccess mBasePath scope filePath False
    case validation of
        Left err -> pure $ Left err
        Right canonicalPath -> do
            -- Check if range operations are allowed
            case mRange of
                Just _ | not (allowRangeOps scope) ->
                    pure $ Left RangeOpsNotAllowed
                _ -> do
                    case mRange of
                        Nothing -> do
                            -- Full file write
                            result <- try $ TextIO.writeFile canonicalPath content
                            case result of
                                Left (e :: SomeException) ->
                                    pure $ Left $ ScopeIOError canonicalPath (show e)
                                Right () ->
                                    pure $ Right (length $ Text.lines content)
                        Just (LineRange start end) -> do
                            -- Range write: read existing, modify range, write back
                            readResult <- try $ TextIO.readFile canonicalPath
                            case readResult of
                                Left (e :: SomeException) ->
                                    pure $ Left $ ScopeIOError canonicalPath (show e)
                                Right existingContent -> do
                                    let allLines = Text.lines existingContent
                                        actualStart = max 1 start
                                        actualEnd = if end == 0 then length allLines else min end (length allLines)
                                        before = take (actualStart - 1) allLines
                                        after =
                                            if actualEnd >= length allLines
                                                then []
                                                else drop actualEnd allLines
                                        newLines = Text.lines content
                                        finalLines = before ++ newLines ++ after
                                        finalContent = Text.unlines finalLines
                                    writeResult <- try $ TextIO.writeFile canonicalPath finalContent
                                    case writeResult of
                                        Left (e :: SomeException) ->
                                            pure $ Left $ ScopeIOError canonicalPath (show e)
                                        Right () ->
                                            pure $ Right (length newLines)

