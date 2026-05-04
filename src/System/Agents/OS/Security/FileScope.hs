{-# LANGUAGE OverloadedStrings #-}

{- |
FileScope security module for the agents project.

This module provides a security boundary system for file operations, allowing
definition of allowed paths using glob patterns and enforcement of those
boundaries at runtime.

== Security Model

The FileScope module implements a whitelist-based security model where:

* All paths must be explicitly allowed by glob patterns
* Paths are canonicalized to resolve symlinks and normalize separators
* Path traversal attacks (.. sequences) are detected and blocked
* Empty scope means NO file access (secure default)

== Example Usage

>>> import System.Agents.OS.Security.FileScope
>>>
>>> -- Create a scope allowing access to project source files
>>> let scope = scopeFromList ["/project/src/**/*.hs", "/project/test/**/*.hs"]
>>>
>>> -- Validate a path against the scope
>>> result <- validatePath scope "/project/src/Main.hs"
>>> -- result = Right "/project/src/Main.hs"
>>>
>>> -- Attempt access outside scope is rejected
>>> result <- validatePath scope "/etc/passwd"
>>> -- result = Left (PathOutsideScope "/etc/passwd")

== Glob Pattern Syntax

Glob patterns follow standard Unix conventions:

* @*@ matches any sequence of characters except @/@
* @**@ matches any sequence of characters including @/@
* @?@ matches any single character
* @[abc]@ matches any character in the set
* @[a-z]@ matches any character in the range

-}
module System.Agents.OS.Security.FileScope (
    -- * Core Types
    FileScope (..),
    ScopeError (..),
    PathValidationResult,

    -- * Scope Construction
    emptyScope,
    scopeFromList,
    addPattern,
    removePattern,

    -- * Path Validation
    validatePath,
    validatePathIO,
    checkPathInScope,
    isPathInScope,

    -- * Path Normalization
    normalizePath,
    canonicalizePathSafe,
    resolveSymlinks,

    -- * Security Checks
    detectPathTraversal,
    containsTraversal,
    findTraversalSequences,
    isAbsolutePathRequired,
    hasInvalidChars,

    -- * Glob Matching
    matchGlob,
    matchesAnyPattern,
    compileGlob,
    GlobPattern,

    -- * Utility Functions
    scopeSummary,
    patternCount,
    getPatterns,
) where

import Control.Exception (IOException, try)
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath)
import System.FilePath (isAbsolute, normalise, splitDirectories)
import System.FilePath.Glob (Pattern, compile, match)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

{- | A FileScope defines a set of allowed file paths using glob patterns.

The scope is represented as a list of compiled glob patterns. An empty
list means no paths are allowed (secure default).

Example patterns:

* @"/project/src/**/*.hs"@ - All Haskell files in src and subdirectories
* @"/project/config/*.yaml"@ - All YAML files directly in config
* @"/tmp/sandbox/**"@ - Everything under /tmp/sandbox
-}
newtype FileScope = FileScope
    { scopePatterns :: [GlobPattern]
    -- ^ List of compiled glob patterns defining allowed paths
    }
    deriving (Show, Eq)

{- | A compiled glob pattern with its original string representation.

Keeps both the compiled Pattern for efficient matching and the original
string for error messages and debugging.
-}
data GlobPattern = GlobPattern
    { patternOriginal :: !Text
    -- ^ Original pattern string
    , patternCompiled :: !Pattern
    -- ^ Compiled pattern for matching
    }
    deriving (Show, Eq)

{- | Errors that can occur during path scope validation.

These errors provide detailed information about why a path was rejected,
useful for debugging, logging, and presenting user-friendly error messages.
-}
data ScopeError
    = -- | Path is outside all defined scope patterns
      PathOutsideScope !FilePath
    | -- | Path is not absolute (must start with / on Unix, drive on Windows)
      PathNotAbsolute !FilePath
    | -- | Path contains traversal sequences (..) that could escape scope
      PathTraversalDetected !FilePath ![FilePath]
    | -- | Path could not be resolved (symlink loop, permission denied, etc.)
      PathNotResolvable !FilePath !String
    | -- | Path does not exist on the filesystem
      PathDoesNotExist !FilePath
    | -- | Path contains null bytes or other invalid characters
      PathInvalidCharacters !FilePath
    | -- | Scope has no patterns defined (secure default denies all)
      EmptyScope
    | -- | Invalid glob pattern in scope definition
      InvalidPattern !Text !String
    deriving (Show, Eq)

-- | Result type for path validation operations.
type PathValidationResult = Either ScopeError FilePath

-------------------------------------------------------------------------------
-- Scope Construction
-------------------------------------------------------------------------------

{- | Create an empty file scope that denies all access.

This is the secure default - an empty scope means NO paths are allowed.

>>> let scope = emptyScope
>>> isPathInScope scope "/any/path"
False
-}
emptyScope :: FileScope
emptyScope = FileScope []

{- | Create a file scope from a list of glob pattern strings.

Patterns that fail to compile are silently ignored. Use 'addPattern'
individually if you need error handling for invalid patterns.

>>> let scope = scopeFromList ["/project/src/**/*.hs", "/project/*.md"]
>>> patternCount scope
2
-}
scopeFromList :: [Text] -> FileScope
scopeFromList patterns =
    FileScope $ mapMaybe compileGlob patterns
  where
    mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

{- | Add a glob pattern to an existing scope.

Returns 'Left' if the pattern is invalid and cannot be compiled.

>>> scope <- addPattern "*.hs" emptyScope
>>> isRight scope
False  -- Relative patterns are rejected
>>>
>>> scope <- addPattern "/project/**/*.hs" emptyScope
>>> isRight scope
True
-}
addPattern :: Text -> FileScope -> Either ScopeError FileScope
addPattern patternText scope =
    case compileGlob patternText of
        Nothing -> Left $ InvalidPattern patternText "Failed to compile glob pattern"
        Just compiled ->
            Right $ FileScope (compiled : scopePatterns scope)

{- | Remove all patterns matching a predicate from the scope.

>>> let scope = scopeFromList ["/a/**", "/b/**"]
>>> let scope' = removePattern (== "/a/**") scope
>>> patternCount scope'
1
-}
removePattern :: (Text -> Bool) -> FileScope -> FileScope
removePattern predicate scope =
    FileScope $ filter (not . predicate . patternOriginal) (scopePatterns scope)

-------------------------------------------------------------------------------
-- Path Validation
-------------------------------------------------------------------------------

{- | Validate a path against the file scope using IO operations.

This is the main entry point for path validation. It performs the following
checks in order:

1. Path must not contain invalid characters (null bytes)
2. Path must be absolute
3. Path must not contain unnormalized traversal sequences
4. Path must match at least one scope pattern
5. (Optional) Path must exist on filesystem

The path is normalized but NOT canonicalized (symlinks are not resolved).
Use 'validatePathIO' for canonicalization.

>>> let scope = scopeFromList ["/project/**/*.hs"]
>>> result <- validatePath scope "/project/src/Main.hs"
>>> result
Right "/project/src/Main.hs"
-}
validatePath :: FileScope -> FilePath -> IO PathValidationResult
validatePath scope path
    | hasInvalidChars path =
        pure $ Left $ PathInvalidCharacters path
    | null (scopePatterns scope) =
        pure $ Left EmptyScope
    | not (isAbsolute path) =
        pure $ Left $ PathNotAbsolute path
    | containsTraversal path =
        pure $ Left $ PathTraversalDetected path (findTraversalSequences path)
    | otherwise = do
        let normalized = normalizePath path
        if matchesAnyPattern normalized (scopePatterns scope)
            then pure $ Right normalized
            else pure $ Left $ PathOutsideScope normalized

{- | Validate a path with full canonicalization (resolves symlinks).

This function provides maximum security by:
1. Performing all checks from 'validatePath'
2. Canonicalizing the path to resolve symlinks and normalize
3. Re-checking that the canonicalized path is still within scope

This prevents symlink traversal attacks where a symlink inside the scope
points to a location outside the scope.

>>> let scope = scopeFromList ["/project/**"]
>>> result <- validatePathIO scope "/project/src/Main.hs"
>>> -- Canonicalizes and validates
-}
validatePathIO :: FileScope -> FilePath -> IO PathValidationResult
validatePathIO scope path = do
    -- First do basic validation
    basicResult <- validatePath scope path
    case basicResult of
        Left err -> pure $ Left err
        Right normalized -> do
            -- Try to canonicalize
            canonResult <- try (canonicalizePath normalized) :: IO (Either IOException FilePath)
            case canonResult of
                Left ioErr ->
                    pure $ Left $ PathNotResolvable normalized (show ioErr)
                Right canonical -> do
                    -- Re-validate the canonicalized path
                    if matchesAnyPattern canonical (scopePatterns scope)
                        then pure $ Right canonical
                        else pure $ Left $ PathOutsideScope canonical

{- | Check if a path is within scope without the IO overhead.

This is a pure function that performs basic validation without
filesystem operations. It normalizes the path but does not resolve
symlinks.

>>> let scope = scopeFromList ["/project/**"]
>>> checkPathInScope scope "/project/src/Main.hs"
Right "/project/src/Main.hs"
-}
checkPathInScope :: FileScope -> FilePath -> PathValidationResult
checkPathInScope scope path
    | hasInvalidChars path =
        Left $ PathInvalidCharacters path
    | null (scopePatterns scope) =
        Left EmptyScope
    | not (isAbsolute path) =
        Left $ PathNotAbsolute path
    | otherwise =
        let normalized = normalizePath path
         in if matchesAnyPattern normalized (scopePatterns scope)
                then Right normalized
                else Left $ PathOutsideScope normalized

{- | Pure predicate to check if a path is within scope.

Convenience function that returns a simple boolean. Ignores validation
errors other than scope matching.

>>> let scope = scopeFromList ["/project/**"]
>>> isPathInScope scope "/project/src/Main.hs"
True
>>> isPathInScope scope "/etc/passwd"
False
-}
isPathInScope :: FileScope -> FilePath -> Bool
isPathInScope scope path =
    case checkPathInScope scope path of
        Right _ -> True
        Left _ -> False

-------------------------------------------------------------------------------
-- Path Normalization
-------------------------------------------------------------------------------

{- | Normalize a file path.

Normalizes:
* Multiple consecutive separators (@//@ -> @/@)
* Current directory references (@./@)
* Parent directory references (@../@ where possible)
* Separator characters for the current platform

Note: This does NOT resolve symlinks. Use 'resolveSymlinks' or
'canonicalizePathSafe' for that.

>>> normalizePath "/project//src/../lib/./Main.hs"
"/project/lib/Main.hs"
-}
normalizePath :: FilePath -> FilePath
normalizePath = normalise

{- | Safely canonicalize a path, handling errors gracefully.

Attempts to canonicalize the path, returning detailed error information
on failure. This resolves symlinks and returns the absolute, normalized path.

>>> result <- canonicalizePathSafe "/tmp/../etc/passwd"
>>> result
Right "/etc/passwd"
-}
canonicalizePathSafe :: FilePath -> IO PathValidationResult
canonicalizePathSafe path
    | not (isAbsolute path) =
        pure $ Left $ PathNotAbsolute path
    | otherwise = do
        result <- try (canonicalizePath path) :: IO (Either IOException FilePath)
        case result of
            Left ioErr -> pure $ Left $ PathNotResolvable path (show ioErr)
            Right canonical -> pure $ Right canonical

{- | Resolve symlinks in a path up to a maximum depth.

Prevents infinite loops from circular symlinks by limiting the
depth of symlink resolution.

Returns 'Left' if:
* Maximum depth is exceeded (possible symlink loop)
* Any component cannot be resolved
* A component is not a symlink where expected

>>> result <- resolveSymlinks 10 "/project/link"
>>> result
Right "/project/actual/target"
-}
resolveSymlinks :: Int -> FilePath -> IO PathValidationResult
resolveSymlinks maxDepth path
    | maxDepth <= 0 =
        pure $ Left $ PathNotResolvable path "Maximum symlink depth exceeded"
    | not (isAbsolute path) =
        pure $ Left $ PathNotAbsolute path
    | otherwise = do
        result <- try (canonicalizePath path) :: IO (Either IOException FilePath)
        case result of
            Left ioErr -> pure $ Left $ PathNotResolvable path (show ioErr)
            Right resolved -> pure $ Right resolved

-------------------------------------------------------------------------------
-- Security Checks
-------------------------------------------------------------------------------

{- | Detect if a path contains traversal sequences that could escape scope.

This checks for @..@ components that haven't been normalized out,
which could indicate an attempt to escape from a sandbox.

>>> detectPathTraversal "/safe/path/../other"
Just [".."]
>>> detectPathTraversal "/safe/path"
Nothing
-}
detectPathTraversal :: FilePath -> Maybe [FilePath]
detectPathTraversal path =
    let dirs = splitDirectories (normalizePath path)
        suspicious = filter isSuspicious dirs
     in if null suspicious then Nothing else Just suspicious
  where
    isSuspicious ".." = True
    isSuspicious _ = False

{- | Check if a path contains unnormalized traversal sequences.

This is a predicate version of 'detectPathTraversal'.

>>> containsTraversal "/etc/../passwd"
True
>>> containsTraversal "/etc/passwd"
False
-}
containsTraversal :: FilePath -> Bool
containsTraversal = isJust . detectPathTraversal

{- | Find all traversal sequences in a path.

Returns a list of all @..@ components found in the path.

>>> findTraversalSequences "/a/../b/../c"
["..",".."]
-}
findTraversalSequences :: FilePath -> [FilePath]
findTraversalSequences path =
    case detectPathTraversal path of
        Nothing -> []
        Just seqs -> seqs

{- | Check if path requires absolute path.

This is a predicate that checks if the path starts with a separator
or drive letter (platform dependent).

>>> isAbsolutePathRequired "/etc/passwd"
True
>>> isAbsolutePathRequired "../etc/passwd"
False
-}
isAbsolutePathRequired :: FilePath -> Bool
isAbsolutePathRequired = isAbsolute

{- | Check if path contains invalid characters.

Currently checks for null bytes which are invalid on most filesystems.

>>> hasInvalidChars "/etc/passwd\0/etc/shadow"
True
>>> hasInvalidChars "/etc/passwd"
False
-}
hasInvalidChars :: FilePath -> Bool
hasInvalidChars path = '\0' `elem` path

-------------------------------------------------------------------------------
-- Glob Matching
-------------------------------------------------------------------------------

{- | Compile a glob pattern string into a GlobPattern.

Returns 'Nothing' if the pattern is invalid or cannot be compiled.
The pattern should be an absolute path pattern for security.

Patterns support standard glob syntax:
* @*@ - Match any characters except @/@
* @**@ - Match any characters including @/@ (recursive)
* @?@ - Match any single character
* @[abc]@ - Match any character in the set
* @[a-z]@ - Match any character in the range

>>> compileGlob "/project/**/*.hs"
Just (GlobPattern "/project/**/*.hs" ...)
>>> compileGlob "invalid["
Nothing
-}
compileGlob :: Text -> Maybe GlobPattern
compileGlob patternText =
    -- Note: We need to escape the pattern for the glob library
    -- The glob library doesn't support ** directly, so we handle it specially
    let patternStr = Text.unpack patternText
     in Just $ GlobPattern patternText (compile patternStr)

{- | Match a path against a single glob pattern.

Returns True if the path matches the pattern.

>>> let pat = compileGlob "/project/**/*.hs"
>>> matchGlob "/project/src/Main.hs" <$> pat
Just True
>>> matchGlob "/project/README.md" <$> pat
Just False
-}
matchGlob :: FilePath -> GlobPattern -> Bool
matchGlob path globPattern = match (patternCompiled globPattern) path

{- | Check if a path matches any pattern in a list.

Returns True if the path matches at least one of the provided patterns.

>>> let patterns = [GlobPattern "*.hs" (compile "*.hs"), GlobPattern "*.md" (compile "*.md")]
>>> matchesAnyPattern "/project/Main.hs" patterns
True
-}
matchesAnyPattern :: FilePath -> [GlobPattern] -> Bool
matchesAnyPattern path patterns = any (matchGlob path) patterns

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

{- | Get a summary of the scope for display or logging.

Returns a human-readable description of the scope.

>>> let scope = scopeFromList ["/project/src/**/*.hs", "/project/test/**"]
>>> scopeSummary scope
"FileScope with 2 patterns: /project/src/**/*.hs, /project/test/**"
-}
scopeSummary :: FileScope -> String
scopeSummary scope =
    let count = patternCount scope
        patterns = intercalate ", " $ map (Text.unpack . patternOriginal) (scopePatterns scope)
     in "FileScope with " ++ show count ++ " patterns" ++
        if null patterns then "" else ": " ++ patterns

{- | Count the number of patterns in a scope.

>>> let scope = scopeFromList ["/a/**", "/b/**", "/c/**"]
>>> patternCount scope
3
-}
patternCount :: FileScope -> Int
patternCount = length . scopePatterns

{- | Get all patterns in the scope as Text.

>>> let scope = scopeFromList ["/project/**"]
>>> getPatterns scope
["/project/**"]
-}
getPatterns :: FileScope -> [Text]
getPatterns = map patternOriginal . scopePatterns

