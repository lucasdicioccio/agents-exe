{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Scaffolding capabilities for the DeveloperToolbox.

This module provides functionality for scaffolding:
- Agent configurations from templates
- Tool scripts in various languages
-}
module System.Agents.Tools.DeveloperToolbox.Scaffold (
    -- * Agent scaffolding
    executeScaffoldAgent,

    -- * Tool scaffolding
    executeScaffoldTool,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import qualified System.Posix.Files as Posix

import System.Agents.Base (AgentDescription (..), DeveloperToolCapability (..))
import System.Agents.Tools.DeveloperToolbox.Templates (
    makeAgentTemplate,
    makeToolTemplate,
 )
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolError (..),
    ScaffoldResult (..),
    Toolbox (..),
 )

-------------------------------------------------------------------------------
-- Agent Scaffolding
-------------------------------------------------------------------------------

{- | Execute agent scaffolding.

This function generates agent scaffolding from a template.

Returns:
* 'Right ScaffoldResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeScaffoldAgent ::
    Toolbox ->
    -- | Template name (openai, mistral, ollama)
    Text ->
    -- | Agent slug
    Text ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError ScaffoldResult)
executeScaffoldAgent toolbox templateName agentSlug filePath force = do
    if DevToolScaffoldAgent `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "scaffold-agent"
        else do
            unless force $ do
                exists <- doesFileExist filePath
                when exists $ do
                    error $ "File already exists: " <> filePath
            result <- try $ do
                let agent = makeAgentTemplate templateName agentSlug
                createDirectoryIfMissing True (takeDirectory filePath)
                LByteString.writeFile filePath $
                    AesonPretty.encodePretty (AgentDescription agent)
            case result of
                Left (e :: SomeException) ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = False
                                , scaffoldPath = filePath
                                , scaffoldError = Just $ Text.pack $ show e
                                }
                Right () ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = True
                                , scaffoldPath = filePath
                                , scaffoldError = Nothing
                                }

-------------------------------------------------------------------------------
-- Tool Scaffolding
-------------------------------------------------------------------------------

{- | Execute tool scaffolding.

This function generates tool scaffolding in a given language. When the
requested language is @bash@ (the default), the generated file is made
executable so that it can be loaded and validated by the bash tool system
without a separate @chmod +x@ step.

Returns:
* 'Right ScaffoldResult' on success or failure with error
* 'Left DeveloperToolError' if capability not enabled
-}
executeScaffoldTool ::
    Toolbox ->
    -- | Language (bash, python, haskell)
    Text ->
    -- | Tool slug
    Text ->
    -- | Output file path
    FilePath ->
    -- | Force overwrite
    Bool ->
    IO (Either DeveloperToolError ScaffoldResult)
executeScaffoldTool toolbox language toolSlug filePath force = do
    if DevToolScaffoldTool `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "scaffold-tool"
        else do
            unless force $ do
                exists <- doesFileExist filePath
                when exists $ do
                    error $ "File already exists: " <> filePath
            result <- try $ do
                let content = makeToolTemplate language toolSlug
                createDirectoryIfMissing True (takeDirectory filePath)
                Text.writeFile filePath content
                when (isBashLanguage language) $ do
                    makeExecutable filePath
            case result of
                Left (e :: SomeException) ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = False
                                , scaffoldPath = filePath
                                , scaffoldError = Just $ Text.pack $ show e
                                }
                Right () ->
                    pure $
                        Right $
                            ScaffoldResult
                                { scaffoldSuccess = True
                                , scaffoldPath = filePath
                                , scaffoldError = Nothing
                                }

-- | Determine whether the requested language is bash (the default).
--
-- The default language is "bash" and is selected when the language parameter
-- is empty, "bash", or any unrecognized value (since 'makeToolTemplate'
-- defaults to bash for unknown languages).
isBashLanguage :: Text -> Bool
isBashLanguage lang =
    Text.null lang || Text.toLower lang == "bash"

-- | Make a file executable for the owner, preserving existing permissions.
--
-- This is required for bash tools because the tool loader only considers
-- files that are both regular (or symlinks) and owner-executable.
makeExecutable :: FilePath -> IO ()
makeExecutable path = do
    status <- Posix.getFileStatus path
    let currentMode = Posix.fileMode status
    let newMode = currentMode `Posix.unionFileModes` Posix.ownerExecuteMode
    Posix.setFileMode path newMode

