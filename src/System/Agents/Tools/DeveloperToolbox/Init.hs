{-# LANGUAGE OverloadedRecordDot #-}

{- |
Initialization for the DeveloperToolbox.

This module provides the initializeToolbox function for creating
a Toolbox runtime from a configuration description.
-}
module System.Agents.Tools.DeveloperToolbox.Init (
    initializeToolbox,
) where

import Prod.Tracer (Tracer (..))

import System.Agents.Base (DeveloperToolboxDescription (..))
import System.Agents.Tools.DeveloperToolbox.Types (
    Toolbox (..),
    Trace (..),
 )

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
            let toolbox =
                    Toolbox
                        { toolboxName = desc.developerToolboxName
                        , toolboxDescription = desc.developerToolboxDescription
                        , toolboxCapabilities = desc.developerToolboxCapabilities
                        , toolboxConfig = desc
                        }
            pure $ Right toolbox

