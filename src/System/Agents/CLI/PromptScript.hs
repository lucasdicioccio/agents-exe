{- | Module for prompt script functionality.

Prompt scripts allow composing prompts from multiple sources: strings,
files, shell commands, session contents, and media files. This module handles parsing
and interpreting prompt script directives.
-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Agents.CLI.PromptScript (
    -- * Types
    PromptScriptDirective (..),
    PromptScript,
    MediaReference (..),

    -- * Interpretation
    interpretPromptScript,
    interpretPromptScriptDirective,

    -- * Alias resolution
    resolveAlias,

    -- * Media parsing
    parseMediaReference,
    inferMediaTypeFromPath,
    resolveMediaType,
) where

import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.FilePath (takeExtension)
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

-- | A media reference for attaching files to LLM queries.
--
-- Can be specified either with an explicit MIME type:
-- @image/png;/path/to/image.png@
--
-- Or without (type will be inferred from extension):
-- @/path/to/image.png@
data MediaReference = MediaReference
    { mediaMimeType :: !(Maybe Text)
    -- ^ Optional MIME type (e.g., "image/png"). If Nothing, will be inferred from file extension.
    , mediaFilePath :: !FilePath
    -- ^ Path to the media file
    }
    deriving (Show, Eq)

-- | A directive in a prompt script
data PromptScriptDirective
    = Str Text.Text
    | FileContents FilePath
    | Separator Int Text.Text
    | ShellOutput String
    | SessionContents FilePath SessionInject.SessionVerbosity
    | AliasPrompt Text
    | MediaFile MediaReference
    deriving (Show)

-- | A prompt script is a list of directives
type PromptScript = [PromptScriptDirective]

-------------------------------------------------------------------------------
-- Media Reference Parsing
-------------------------------------------------------------------------------

-- | Parse a media reference string.
--
-- Supports formats:
-- - @image/png;/path/to/image.png@ (explicit MIME type)
-- - @/path/to/image.png@ (inferred from extension)
--
-- The semicolon separates the MIME type from the path.
-- If no semicolon is present, the entire string is treated as a file path.
parseMediaReference :: String -> Either String MediaReference
parseMediaReference input =
    let txt = Text.pack input
     in case Text.breakOn ";" txt of
            (_, "") ->
                -- No semicolon found - treat entire input as path
                Right $ MediaReference Nothing input
            (mimeType, rest)
                | Text.null mimeType ->
                    -- Semicolon at start, rest is path
                    Right $ MediaReference Nothing (Text.unpack $ Text.drop 1 rest)
                | otherwise ->
                    -- MIME type present
                    let path = Text.unpack $ Text.drop 1 rest
                     in if null path
                            then Left "Media reference has MIME type but no file path"
                            else Right $ MediaReference (Just mimeType) path

-- | Infer MIME type from file extension.
-- Returns Nothing if the extension is not recognized.
inferMediaTypeFromPath :: FilePath -> Maybe Text
inferMediaTypeFromPath path =
    let ext = Text.toLower $ Text.pack $ takeExtension path
     in case ext of
            -- Images
            ".png" -> Just "image/png"
            ".jpg" -> Just "image/jpeg"
            ".jpeg" -> Just "image/jpeg"
            ".gif" -> Just "image/gif"
            ".webp" -> Just "image/webp"
            ".svg" -> Just "image/svg+xml"
            -- Audio
            ".mp3" -> Just "audio/mp3"
            ".mpeg" -> Just "audio/mpeg"
            ".wav" -> Just "audio/wav"
            ".wave" -> Just "audio/wav"
            ".ogg" -> Just "audio/ogg"
            ".aac" -> Just "audio/aac"
            ".flac" -> Just "audio/flac"
            -- Video
            ".mp4" -> Just "video/mp4"
            ".webm" -> Just "video/webm"
            ".avi" -> Just "video/avi"
            ".mov" -> Just "video/quicktime"
            -- Documents / Application
            ".pdf" -> Just "application/pdf"
            ".json" -> Just "application/json"
            ".xml" -> Just "application/xml"
            ".zip" -> Just "application/zip"
            -- Text
            ".txt" -> Just "text/plain"
            ".html" -> Just "text/html"
            ".htm" -> Just "text/html"
            ".css" -> Just "text/css"
            ".csv" -> Just "text/csv"
            ".md" -> Just "text/markdown"
            ".markdown" -> Just "text/markdown"
            -- Unknown
            _ -> Nothing

-- | Resolve the MIME type for a media reference.
-- Uses the explicitly provided type if available, otherwise attempts to infer from path.
-- Returns Left with error message if type cannot be determined.
resolveMediaType :: MediaReference -> Either String Text
resolveMediaType ref = case mediaMimeType ref of
    Just mime -> Right mime
    Nothing -> case inferMediaTypeFromPath (mediaFilePath ref) of
        Just mime -> Right mime
        Nothing ->
            Left $
                "Cannot infer media type for file: "
                    <> mediaFilePath ref
                    <> ". Please specify explicitly as 'mime/type;"
                    <> mediaFilePath ref
                    <> "'"

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
        MediaFile ref -> do
            -- Media directives don't produce text output during script interpretation
            -- They are handled separately by the command handler
            case resolveMediaType ref of
                Left err -> do
                    Text.hPutStrLn stderr $ "Error: " <> Text.pack err
                    exitFailure
                Right mimeType -> do
                    -- Return a placeholder indicating media will be attached
                    pure $ "_(Media attachment: " <> Text.pack (mediaFilePath ref) <> " [" <> mimeType <> "])_"

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

