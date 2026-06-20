{-# LANGUAGE OverloadedRecordDot #-}

{- |
Initialization for the DeveloperToolbox.

This module provides the initializeToolbox function for creating
a Toolbox runtime from a configuration description.
-}
module System.Agents.Tools.DeveloperToolbox.Init (
    initializeToolbox,
) where

import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Prod.Tracer (Tracer (..))

import System.Agents.Base (DeveloperToolCapability (..), DeveloperToolboxDescription (..), defaultDeveloperFileSandbox)
import System.Agents.FileSandbox (FileSandbox (..))
import System.Agents.Tools.DeveloperToolbox.Types (
    Toolbox (..),
    Trace (..),
    emptyEditSessionStore,
    emptySnapshotStore,
 )

-- | File-related capabilities that require sandbox validation.
fileCapabilities :: [DeveloperToolCapability]
fileCapabilities =
    [ DevToolReadFileRange
    , DevToolWriteFileRange
    , DevToolPatchFile
    ]

{- | Initialize a developer toolbox from a description.

This function creates a 'Toolbox' value from a 'DeveloperToolboxDescription',
validating the configuration and preparing the runtime state.

Returns an error if the configuration is invalid.
-}
initializeToolbox ::
    Tracer IO Trace ->
    DeveloperToolboxDescription ->
    IO (Either String Toolbox)
initializeToolbox _tracer desc = do
    -- Validate that we have at least one capability
    if null desc.developerToolboxCapabilities
        then pure $ Left "Developer toolbox must have at least one capability enabled"
        else do
            -- Create file sandbox if any file-related capability is enabled
            let config = fromMaybe defaultDeveloperFileSandbox desc.developerToolboxFileSandbox
            mFileSandbox <-
                if any (`elem` desc.developerToolboxCapabilities) fileCapabilities
                    then do
                        createdAt <- getCurrentTime
                        let sandbox =
                                FileSandbox
                                    { sandboxId = error "sandboxId not used for validation"
                                    , sandboxConfig = config
                                    , sandboxCreatedAt = createdAt
                                    }
                        pure $ Just sandbox
                    else pure Nothing

            -- Create snapshot store if snapshot capability is enabled
            mSnapshotStore <-
                if DevToolSnapshot `elem` desc.developerToolboxCapabilities
                    then Just <$> emptySnapshotStore
                    else pure Nothing

            -- Create edit session store if write-file-range is enabled, to support
            -- multi-turn editing sessions pinned to an original snapshot.
            mEditSessionStore <-
                if DevToolWriteFileRange `elem` desc.developerToolboxCapabilities
                    then Just <$> emptyEditSessionStore
                    else pure Nothing

            let toolbox =
                    Toolbox
                        { toolboxName = desc.developerToolboxName
                        , toolboxDescription = desc.developerToolboxDescription
                        , toolboxCapabilities = desc.developerToolboxCapabilities
                        , toolboxConfig = desc
                        , toolboxFileSandbox = mFileSandbox
                        , toolboxSnapshotStore = mSnapshotStore
                        , toolboxEditSessionStore = mEditSessionStore
                        }
            pure $ Right toolbox
