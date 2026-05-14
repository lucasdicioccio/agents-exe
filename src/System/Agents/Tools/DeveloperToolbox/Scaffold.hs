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
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

import System.Agents.Base (AgentDescription (..), DeveloperToolCapability (..))
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolError (..),
    ScaffoldResult (..),
    Toolbox (..),
 )
import System.Agents.Tools.DeveloperToolbox.Templates (
    makeAgentTemplate,
    makeToolTemplate,
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

This function generates tool scaffolding in a given language.

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

