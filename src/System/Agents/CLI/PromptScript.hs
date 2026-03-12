{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for prompt script functionality.

Prompt scripts allow composing prompts from multiple sources: strings,
files, shell commands, and session contents. This module handles parsing
and interpreting prompt script directives.
-}
module System.Agents.CLI.PromptScript (
    -- * Types
    PromptScriptDirective (..),
    PromptScript,

    -- * Interpretation
    interpretPromptScript,
    interpretPromptScriptDirective,

    -- * Alias resolution
    resolveAlias,
) where

import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)
import qualified System.Process as Process

import System.Agents.CLI.Aliases (
    AliasDefinition (..),
    AliasInputMode (..),
    lookupAlias,
    substituteTemplate,
 )
import qualified System.Agents.SessionPrint.Inject as SessionInject

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A directive in a prompt script
data PromptScriptDirective
    = Str Text.Text
    | FileContents FilePath
    | Separator Int Text.Text
    | ShellOutput String
    | SessionContents FilePath SessionInject.SessionVerbosity
    | AliasPrompt Text
    deriving (Show)

-- | A prompt script is a list of directives
type PromptScript = [PromptScriptDirective]

-------------------------------------------------------------------------------
-- Interpretation
-------------------------------------------------------------------------------

-- | Interpret prompt script with alias resolution
interpretPromptScript :: Map Text AliasDefinition -> PromptScript -> Maybe FilePath -> IO Text.Text
interpretPromptScript aliases directives mSessionFile
    | null directives = do
        content <- Text.getContents
        pure content
    | otherwise = do
        results <- mapM (interpretPromptScriptDirective aliases mSessionFile) directives
        pure $ Text.unlines results

-- | Interpret a single prompt script directive
interpretPromptScriptDirective :: Map Text AliasDefinition -> Maybe FilePath -> PromptScriptDirective -> IO Text.Text
interpretPromptScriptDirective aliases mSessionFile directive =
    case directive of
        Str s -> pure s
        FileContents p -> Text.readFile p
        Separator n s -> pure $ Text.replicate n s
        ShellOutput cmd -> Text.pack <$> Process.readCreateProcess (Process.shell cmd) ("" :: String)
        SessionContents path verbosity -> do
            result <- Aeson.eitherDecodeFileStrict' path
            case result of
                Left err -> do
                    Text.hPutStrLn stderr $ "Warning: could not load session file " <> Text.pack path <> ": " <> Text.pack err
                    pure $ "_(Error loading session: " <> Text.pack err <> " )_\n"
                Right session ->
                    pure $ SessionInject.formatSessionForPrompt verbosity session
        AliasPrompt aliasName -> do
            case lookupAlias aliases aliasName of
                Left err -> do
                    Text.hPutStrLn stderr err
                    exitFailure
                Right def -> resolveAlias def mSessionFile

-- | Resolve an alias definition to its final prompt text
resolveAlias :: AliasDefinition -> Maybe FilePath -> IO Text
resolveAlias def mSessionFile = do
    (content, mActualFile) <- case aliasInputMode def of
        AliasStdin -> do
            c <- Text.getContents
            pure (c, Nothing)
        AliasFile -> case mSessionFile of
            Just sf -> do
                c <- Text.readFile sf
                pure (c, Just sf)
            Nothing -> do
                c <- Text.getContents
                pure (c, Nothing)
    pure $ substituteTemplate (aliasTemplate def) content mActualFile
