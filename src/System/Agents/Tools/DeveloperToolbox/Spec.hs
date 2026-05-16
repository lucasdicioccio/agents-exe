{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Specification documentation for the DeveloperToolbox.

This module provides access to specification documentation
for various tool formats.
-}
module System.Agents.Tools.DeveloperToolbox.Spec (
    -- * Show spec
    executeShowSpec,

    -- * Spec content
    bashToolsDocumentation,
) where

import Data.FileEmbed (embedStringFile)
import Data.Text (Text)

import System.Agents.Base (DeveloperToolCapability (..))
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolError (..),
    Toolbox (..),
 )

-- | Embedded bash-tools documentation
bashToolsDocumentation :: Text
bashToolsDocumentation = $(embedStringFile "docs/binary-tool.md")

{- | Execute show spec.

This function returns specification documentation.

Returns:
* 'Right Text' with the spec content
* 'Left DeveloperToolError' if capability not enabled or unknown spec
-}
executeShowSpec ::
    Toolbox ->
    -- | Spec name (bash-tools)
    Text ->
    IO (Either DeveloperToolError Text)
executeShowSpec toolbox specName = do
    if DevToolShowSpec `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "show-spec"
        else case specName of
            "bash-tools" -> pure $ Right bashToolsDocumentation
            _ -> pure $ Left $ InvalidTemplateError $ "Unknown spec: " <> specName
