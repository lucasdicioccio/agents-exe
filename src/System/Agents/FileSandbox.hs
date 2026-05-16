{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | File sandbox resource management.

This module provides first-class file sandbox resources that can be registered
with the OS resource management system. Sandboxes provide:

* Predicate-based access control
* File size limits
* Immutable configuration (sandboxes cannot be modified after creation)
* Integration with the OS resource registry

Example usage:

>>> let config = FileSandboxConfig
>>>         { sandboxPredicate = DirectoryRecursive "./project"
>>>         , sandboxMaxFileSize = Just (10 * 1024 * 1024)  -- 10MB
>>>         , sandboxName = Just "project-sandbox"
>>>         }
>>> sandbox <- createFileSandbox ctx config
>>> result <- validateFileRead sandbox "/home/user/project/src/Main.hs"

The sandbox system is designed with security in mind:

1. Default deny: All access is denied unless explicitly allowed
2. Immutable: Sandboxes cannot be modified after creation
3. Canonicalization: All paths are canonicalized before validation
4. Size limits: Optional file size limits prevent resource exhaustion
-}
module System.Agents.FileSandbox (
    -- * Configuration (re-exported from Base)
    FileSandboxConfig (..),
    defaultFileSandboxConfig,
    fromAllowedPaths,
    
    -- * Sandbox resource
    FileSandbox (..),
    AccessResult (..),
    
    -- * Validation
    validateFileRead,
    validateFileWrite,
    validateFileAccess,
    
    -- * Resource management
    createFileSandbox,
    lookupFileSandbox,
    
    -- * Re-exports
    module System.Agents.FileSandbox.Predicate,
) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Exception (IOException, try)
import qualified Data.HashMap.Strict as HashMap
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (canonicalizePath, doesFileExist, getFileSize)
import System.FilePath (takeDirectory)

import System.Agents.Base (FileSandboxConfig (..), defaultFileSandboxConfig, fromAllowedPaths)
import System.Agents.FileSandbox.Predicate
import System.Agents.OS.Core.Types (ResourceId (..), newEntityId)
import qualified System.Agents.OS.Resources.Types as ResTypes

-------------------------------------------------------------------------------
-- Sandbox Resource
-------------------------------------------------------------------------------

-- | Runtime file sandbox resource.
--
-- Sandboxes are immutable after creation. To change permissions,
-- create a new sandbox with the updated configuration.
data FileSandbox = FileSandbox
    { sandboxId :: ResourceId
    -- ^ Unique identifier for this sandbox
    , sandboxConfig :: FileSandboxConfig
    -- ^ Configuration (immutable)
    , sandboxCreatedAt :: UTCTime
    -- ^ When the sandbox was created
    }
    deriving (Show)

-- | Result of file access validation.
data AccessResult
    = AccessGranted
    | AccessDenied PathError
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | Validate a file read operation against a sandbox.
--
-- This checks:
-- 1. The path predicate
-- 2. File size limits (if configured)
validateFileRead :: FileSandbox -> FilePath -> IO AccessResult
validateFileRead sandbox path = do
    -- Canonicalize path (resolves symlinks)
    canonicalResult <- try $ canonicalizePath path
    case canonicalResult of
        Left (e :: IOException) ->
            pure $ AccessDenied $ PathIOError path (show e)
        Right canonical -> do
            -- Check predicate (applied to canonical path)
            predResult <- evaluatePredicate' ((sandboxConfig sandbox).fsbPredicate) canonical
            case predResult of
                Left err -> pure $ AccessDenied err
                Right () -> do
                    -- Check file size if limit configured
                    case (sandboxConfig sandbox).fsbMaxFileSize of
                        Nothing -> pure AccessGranted
                        Just maxSize -> do
                            fileExists <- doesFileExist canonical
                            if not fileExists
                                then pure AccessGranted  -- Size check only for existing files
                                else do
                                    sizeResult <- try $ getFileSize canonical
                                    case sizeResult of
                                        Left (e :: IOException) ->
                                            pure $ AccessDenied $ PathIOError path (show e)
                                        Right size ->
                                            if size > maxSize
                                                then pure $ AccessDenied $ FileTooLarge canonical size maxSize
                                                else pure AccessGranted

-- | Validate a file write operation against a sandbox.
--
-- For writes, we check:
-- 1. If the file exists, check write permission on the file itself
-- 2. If the file doesn't exist, check write permission on the parent directory
validateFileWrite :: FileSandbox -> FilePath -> IO AccessResult
validateFileWrite sandbox path = do
    canonicalResult <- try $ canonicalizePath path
    case canonicalResult of
        Left (e :: IOException) ->
            pure $ AccessDenied $ PathIOError path (show e)
        Right canonical -> do
            fileExists <- doesFileExist canonical
            if fileExists
                then checkFilePermission canonical
                else checkParentPermission canonical
  where
    checkFilePermission :: FilePath -> IO AccessResult
    checkFilePermission p = do
        result <- evaluatePredicate' ((sandboxConfig sandbox).fsbPredicate) p
        case result of
            Left err -> pure $ AccessDenied err
            Right () -> pure AccessGranted
    
    checkParentPermission :: FilePath -> IO AccessResult
    checkParentPermission p = do
        let parent = takeDirectory p
        result <- evaluatePredicate' ((sandboxConfig sandbox).fsbPredicate) parent
        case result of
            Left err -> pure $ AccessDenied err
            Right () -> pure AccessGranted

-- | General file access validation.
-- This checks only the predicate without size limits.
validateFileAccess :: FileSandbox -> FilePath -> IO AccessResult
validateFileAccess sandbox path = do
    result <- evaluatePredicate ((sandboxConfig sandbox).fsbPredicate) path
    case result of
        Left err -> pure $ AccessDenied err
        Right () -> pure AccessGranted

-------------------------------------------------------------------------------
-- Resource Management
-------------------------------------------------------------------------------

-- | Create a file sandbox resource in the OS registry.
--
-- The sandbox is registered as a resource with the current scope.
-- When the scope is cleaned up, the sandbox will be automatically removed.
createFileSandbox :: ResTypes.ResourceContext -> FileSandboxConfig -> IO ResourceId
createFileSandbox ctx config = do
    eid <- newEntityId
    let rid = ResourceId eid
    createdAt <- getCurrentTime
    
    -- Convert Base config to ResourceTypes config for storage
    let resConfig = ResTypes.FileSandboxConfig
            { ResTypes.fsConfigName = fsbName config
            , ResTypes.fsConfigDescription = Nothing
            }
    
    -- Register as OS resource
    -- Note: The handleAccess uses a dummy ResourceAccessor since the actual
    -- FileSandbox data is stored separately and retrieved via lookupFileSandbox
    let handle = ResTypes.ResourceHandle
            { ResTypes.handleId = rid
            , ResTypes.handleCleanup = pure ()  -- Sandboxes are stateless
            , ResTypes.handleAccess = \f -> f (ResTypes.ResourceAccessor ())
            }
    let info = ResTypes.ResourceInfo
            { ResTypes.resourceId = rid
            , ResTypes.resourceScope = ResTypes.ResourceScope (ResTypes.contextScope ctx)
            , ResTypes.resourceType = ResTypes.FileSandboxResource resConfig
            , ResTypes.resourceCreatedAt = createdAt
            }
    
    -- Store the FileSandbox in a separate registry (simplified: we just track the ID)
    atomically $ do
        modifyTVar' (ResTypes.registryHandles $ ResTypes.contextRegistry ctx) $
            HashMap.insert rid handle
        modifyTVar' (ResTypes.registryInfo $ ResTypes.contextRegistry ctx) $
            HashMap.insert rid info
    
    pure rid

-- | Look up a file sandbox by ID.
-- Note: Currently returns Nothing as we don't store the full FileSandbox in the registry.
-- In a full implementation, we'd have a separate registry for FileSandbox data.
lookupFileSandbox :: ResTypes.ResourceRegistry -> ResourceId -> IO (Maybe FileSandbox)
lookupFileSandbox registry rid = do
    -- For now, we can only check if the resource exists, not retrieve the full sandbox
    -- A proper implementation would store FileSandbox data separately
    mHandle <- atomically $ do
        handles <- readTVar $ ResTypes.registryHandles registry
        pure $ HashMap.lookup rid handles
    case mHandle of
        Nothing -> pure Nothing
        Just _handle -> do
            -- We found the handle but can't retrieve the original FileSandbox
            -- without storing it separately. Return Nothing for now.
            pure Nothing

