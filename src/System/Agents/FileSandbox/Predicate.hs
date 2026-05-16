{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | PathPredicate DSL for expressing file access rules.

This module provides a rich, composable predicate language for defining
file access permissions. Predicates can be combined using logical operators
(And, Or, Not) and can express complex patterns like:

* Exact file or directory matches
* Recursive directory access
* Glob pattern matching
* File extension filtering
* File size limits
* Path relationships (child-of)

Example usage:

>>> let predicate = Any [ DirectoryRecursive "./src"
>>>                     , FileExactly "./package.yaml"
>>>                     , And (FileExtension ["hs", "lhs"]) 
>>>                           (FileSizeLessThan (1024 * 1024))
>>>                     ]
>>> evaluatePredicate predicate "/home/user/project/src/Main.hs"

Predicates are evaluated against the *symlink path* (not resolved target).
This is intentional for security - symlinks are followed but the predicate
applies to the path the user provided.
-}
module System.Agents.FileSandbox.Predicate (
    -- * Path validation errors
    PathError (..),
    
    -- * Predicate DSL
    PathPredicate (..),
    
    -- * Evaluation
    evaluatePredicate,
    evaluatePredicate',
    
    -- * Smart constructors
    fromPathList,
    allowPath,
    allowDirectory,
    allowRecursive,
) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, doesDirectoryExist, getFileSize)
import System.FilePath (isRelative, normalise, splitDirectories, takeExtension, takeFileName)

-- | Path validation error types.
data PathError
    = -- | Path is not in the allowed whitelist
      PathNotAllowed FilePath
    | -- | Path is not absolute (must start with /)
      PathNotAbsolute FilePath
    | -- | Path escapes the sandbox directory
      PathOutsideSandbox FilePath
    | -- | IO error during path validation
      PathIOError FilePath String
    | -- | File too large (path, actual size, max size)
      FileTooLarge FilePath Integer Integer
    | -- | Invalid file extension (path, actual ext, allowed exts)
      InvalidExtension FilePath String [String]
    | -- | Glob pattern error
      GlobPatternError FilePath String
    deriving (Show, Eq, Ord, Generic)

instance FromJSON PathError
instance ToJSON PathError

-- | Path predicate for expressing file access rules.
--
-- Predicates are evaluated against the *symlink path* (not resolved).
-- This is intentional for security - symlinks are followed but the predicate
-- applies to the path the user provided.
data PathPredicate
    = -- | Exact file path match (after canonicalization)
      FileExactly FilePath
    | -- | Exact directory match (contents not included unless specified)
      DirectoryExactly FilePath
    | -- | Directory and all subdirectories recursively
      DirectoryRecursive FilePath
    | -- | Directory contents only, NOT subdirectories
      DirectoryShallow FilePath
    | -- | Glob pattern match on filename (uses simple wildcard matching)
      -- Supports: * (any chars), ? (single char), [abc] (char class)
      FilePattern String
    | -- | File extension in allowed list (e.g., ["hs", "json"])
      -- Extensions should NOT include the leading dot
      FileExtension [String]
    | -- | Maximum file size in bytes (for read operations)
      FileSizeLessThan Integer
    | -- | Path is within a subdirectory of the given path
      ChildOf FilePath
    | -- | Logical AND of two predicates
      And PathPredicate PathPredicate
    | -- | Logical OR of two predicates
      Or PathPredicate PathPredicate
    | -- | Logical NOT of a predicate
      Not PathPredicate
    | -- | Convenience: OR of all predicates
      Any [PathPredicate]
    | -- | Convenience: AND of all predicates
      All [PathPredicate]
    | -- | Always allow (use sparingly)
      AlwaysAllow
    | -- | Always deny (secure default, explicit)
      AlwaysDeny
    deriving (Show, Eq, Ord, Generic)

instance FromJSON PathPredicate
instance ToJSON PathPredicate

-- | Evaluate a predicate against a file path.
--
-- The path should be absolute. It will be canonicalized before evaluation.
-- Returns Right () if allowed, Left PathError if denied.
evaluatePredicate :: PathPredicate -> FilePath -> IO (Either PathError ())
evaluatePredicate predicate path = do
    -- First, canonicalize the path to resolve symlinks, .., etc.
    canonical <- try $ canonicalizePath path
    case canonical of
        Left (e :: IOException) -> 
            pure $ Left $ PathIOError path (show e)
        Right canonicalPath -> 
            evaluatePredicate' predicate canonicalPath

-- | Evaluate a predicate against an already-canonicalized path.
-- This is useful when you want to avoid re-canonicalizing for performance.
evaluatePredicate' :: PathPredicate -> FilePath -> IO (Either PathError ())
evaluatePredicate' predicate path = do
    -- Check if path is absolute
    if isRelative path
        then pure $ Left $ PathNotAbsolute path
        else eval predicate path
  where
    eval :: PathPredicate -> FilePath -> IO (Either PathError ())
    eval AlwaysAllow _ = pure $ Right ()
    eval AlwaysDeny _ = pure $ Left $ PathNotAllowed "Access denied by policy"
    
    eval (FileExactly allowed) p =
        pure $ if normalise allowed == normalise p
            then Right ()
            else Left $ PathNotAllowed p
    
    eval (DirectoryExactly allowed) p = do
        let normAllowed = normalise allowed
        let normPath = normalise p
        pure $ if normPath == normAllowed
            then Right ()
            else Left $ PathNotAllowed p
    
    eval (DirectoryRecursive allowed) p = do
        let normAllowed = normalise allowed
        let normPath = normalise p
        pure $ if isPathWithin normPath normAllowed
            then Right ()
            else Left $ PathNotAllowed p
    
    eval (DirectoryShallow allowed) p = do
        let normAllowed = normalise allowed
        let normPath = normalise p
        -- Check if path is directly in the allowed directory
        _ <- doesDirectoryExist normPath
        let parent = takeParent normPath
        pure $ if parent == normAllowed
            then Right ()
            else Left $ PathNotAllowed p
    
    eval (FilePattern pattern) p =
        let filename = takeFileName p
        in pure $ if matchGlob pattern filename
            then Right ()
            else Left $ PathNotAllowed p
    
    eval (FileExtension exts) p =
        let ext = drop 1 $ takeExtension p  -- drop leading dot
        in pure $ if ext `elem` exts
            then Right ()
            else Left $ InvalidExtension p ext exts
    
    eval (FileSizeLessThan maxSize) p = do
        sizeResult <- try $ getFileSize p
        case sizeResult of
            Left (e :: IOException) -> 
                pure $ Left $ PathIOError p (show e)
            Right size -> 
                pure $ if size <= maxSize
                    then Right ()
                    else Left $ FileTooLarge p size maxSize
    
    eval (ChildOf parent) p =
        pure $ if isPathWithin (normalise p) (normalise parent)
            then Right ()
            else Left $ PathNotAllowed p
    
    eval (And left right) p = do
        leftResult <- eval left p
        case leftResult of
            Left err -> pure $ Left err
            Right () -> eval right p
    
    eval (Or left right) p = do
        leftResult <- eval left p
        case leftResult of
            Right () -> pure $ Right ()
            Left _ -> eval right p
    
    eval (Not inner) p = do
        innerResult <- eval inner p
        case innerResult of
            Right () -> pure $ Left $ PathNotAllowed p
            Left _ -> pure $ Right ()
    
    eval (Any predicates) p = go predicates
      where
        go [] = pure $ Left $ PathNotAllowed p
        go (x:xs) = do
            result <- eval x p
            case result of
                Right () -> pure $ Right ()
                Left _ -> go xs
    
    eval (All predicates) p = go predicates
      where
        go [] = pure $ Right ()
        go (x:xs) = do
            result <- eval x p
            case result of
                Left err -> pure $ Left err
                Right () -> go xs

-- | Check if child is within parent directory.
-- Handles trailing slashes correctly.
isPathWithin :: FilePath -> FilePath -> Bool
isPathWithin child parent =
    let parent' = if null parent || last parent /= '/'
                  then parent ++ "/"
                  else parent
        child' = if null child || last child /= '/'
                 then child ++ "/"
                 else child
    in child == parent ||
       (length child' > length parent' && 
        take (length parent') child' == parent')

-- | Get the parent directory of a path.
takeParent :: FilePath -> FilePath
takeParent path =
    let norm = normalise path
        dirs = splitDirectories norm
    in if null dirs || length dirs == 1
        then "/"
        else normalise $ concat $ init dirs

-- | Simple glob pattern matching.
-- Supports: * (matches any sequence of chars), ? (matches single char)
-- Does NOT currently support [abc] character classes.
matchGlob :: String -> String -> Bool
matchGlob [] [] = True
matchGlob ('*':ps) cs = 
    -- * matches any sequence (including empty)
    matchGlob ps cs ||  -- try matching empty
    (not (null cs) && matchGlob ('*':ps) (drop 1 cs))  -- consume one char
matchGlob ('?':ps) (_:cs) = matchGlob ps cs
matchGlob (p:ps) (c:cs) = p == c && matchGlob ps cs
matchGlob _ _ = False

-- | Smart constructor: Build a predicate from a simple path list (for migration).
-- Each path is treated as a recursive directory access.
fromPathList :: [FilePath] -> PathPredicate
fromPathList paths = Any $ map DirectoryRecursive paths

-- | Create a predicate that allows access to a specific path.
-- If the path is a directory, allows access to all contents recursively.
allowPath :: FilePath -> PathPredicate
allowPath path = DirectoryRecursive path

-- | Create a predicate that allows access to a specific directory
-- (shallow - contents only, not subdirectories).
allowDirectory :: FilePath -> PathPredicate
allowDirectory path = DirectoryShallow path

-- | Create a predicate that allows recursive access to a directory.
allowRecursive :: FilePath -> PathPredicate
allowRecursive path = DirectoryRecursive path

