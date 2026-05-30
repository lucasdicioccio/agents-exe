{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Restore file capability for the DeveloperToolbox.

This module provides functionality to restore a file from a previously
taken snapshot, enabling rollback of file changes.
-}
module System.Agents.Tools.DeveloperToolbox.Restore (
    executeRestoreFile,
) where

import Control.Exception (SomeException, try)
import Control.Concurrent.STM (atomically, readTVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Prod.Tracer (Tracer (..))

import System.Agents.FileSandbox (AccessResult (..), validateFileWrite)
import System.Agents.Tools.DeveloperToolbox.IO (writeFileAtomic)
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolCapability (..),
    DeveloperToolError (..),
    RestoreResult (..),
    Snapshot (..),
    SnapshotRef (..),
    Toolbox (..),
    Trace (..),
 )

{- | Execute restore file operation.

Restores a file to a previous version using a snapshot reference (MD5 hash).
The snapshot must have been created by a previous write-file-range or
patch-file operation when the snapshot capability was enabled.

When a file sandbox is configured, the filepath is validated against
the sandbox before writing. Access is denied if the file is outside
the allowed paths.

Parameters:
- path: Path to the file to restore
- snapshotRef: The MD5 hash reference of the snapshot to restore from

Returns Right with RestoreResult on success, Left with error on failure.
-}
executeRestoreFile ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    SnapshotRef ->
    IO (Either DeveloperToolError RestoreResult)
executeRestoreFile tracer toolbox filePath ref = do
    if DevToolRestoreFile `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "restore-file"
        else do
            runTracer tracer (FileRestoredTrace filePath ref)

            -- Validate against file sandbox if configured
            case toolboxFileSandbox toolbox of
                Just sandbox -> do
                    accessResult <- validateFileWrite sandbox filePath
                    case accessResult of
                        AccessDenied err -> do
                            let errMsg = FileAccessDeniedError filePath (Text.pack $ show err)
                            runTracer tracer (DeveloperToolErrorTrace "restore-file" $ Text.pack $ show errMsg)
                            pure $ Left errMsg
                        AccessGranted -> proceedWithRestore tracer toolbox filePath ref
                Nothing ->
                    -- No sandbox configured, proceed (backwards compatible behavior)
                    proceedWithRestore tracer toolbox filePath ref

-- | Proceed with restore operation after validation.
proceedWithRestore ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    SnapshotRef ->
    IO (Either DeveloperToolError RestoreResult)
proceedWithRestore tracer toolbox filePath ref = do
    -- Check if snapshot store is available
    case toolboxSnapshotStore toolbox of
        Nothing -> do
            let err = RestoreError "Snapshot capability not enabled - no snapshot store available"
            runTracer tracer (DeveloperToolErrorTrace "restore-file" $ Text.pack $ show err)
            pure $ Left err
        Just store -> do
            -- Look up the snapshot in the store
            snapshots <- atomically $ readTVar store
            case Map.lookup ref snapshots of
                Nothing -> do
                    runTracer tracer (SnapshotNotFoundTrace ref)
                    pure $ Left $ SnapshotNotFoundError ref
                Just snapshot -> do
                    -- Restore the file from the snapshot
                    restoreResult <- try $ do
                        createDirectoryIfMissing True (takeDirectory filePath)
                        writeFileAtomic filePath (Text.decodeUtf8 $ snapshotContent snapshot)

                    case restoreResult of
                        Left (e :: SomeException) -> do
                            let err = RestoreError $ Text.pack $ show e
                            runTracer tracer (DeveloperToolErrorTrace "restore-file" $ Text.pack $ show e)
                            pure $ Left err
                        Right () -> do
                            pure $ Right $
                                RestoreResult
                                    { restoreFilePath = filePath
                                    , restoreSnapshotRef = ref
                                    , restoreSuccess = True
                                    , restoreError = Nothing
                                    }

