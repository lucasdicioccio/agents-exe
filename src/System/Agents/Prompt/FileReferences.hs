{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Module for parsing and resolving file references in prompts.

This module handles the @[[file:path]]@ syntax for automatically including
file contents in prompts. It supports:

1. Full file references: @[[file:src/Main.hs]]@
2. Line range references: @[[file:src/Parser.hs#L45-L67]]@

== Usage Example

>>> parseFileReferences "Review [[file:src/Main.hs]] please"
[FileReference {filePath = "src/Main.hs", lineRange = Nothing}]

>>> parseFileReferences "Fix bug in [[file:src/Parser.hs#L10-L20]]"
[FileReference {filePath = "src/Parser.hs", lineRange = Just (10,20)}]

== Error Handling

File access errors are returned per-reference, allowing partial success:

>>> resolveReferences [FileReference "missing.txt" Nothing]
[(FileReference "missing.txt" Nothing, Left "File not found: missing.txt")]
-}
module System.Agents.Prompt.FileReferences (
    -- * Types
    FileReference (..),
    LineRange (..),
    FileResolution (..),

    -- * Parsing
    parseFileReferences,
    parseFileReference,

    -- * Resolution
    resolveReferences,
    resolveReference,
    readFileRange,

    -- * Inlining
    inlineReferences,
    formatFileContent,

    -- * Preview
    previewReferences,
) where

import Control.Exception (IOException, try)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A line range specification (1-indexed, inclusive)
data LineRange = LineRange
    { rangeStart :: !Int
    -- ^ Starting line number (1-indexed)
    , rangeEnd :: !Int
    -- ^ Ending line number (inclusive, -1 means "to end")
    }
    deriving (Show, Eq)

-- | A reference to a file in a prompt
data FileReference = FileReference
    { filePath :: !FilePath
    -- ^ Path to the file (relative or absolute)
    , lineRange :: !(Maybe LineRange)
    -- ^ Optional line range to extract
    }
    deriving (Show, Eq)

-- | Result of resolving a file reference
data FileResolution
    = -- | File content successfully loaded
      ResolutionSuccess !Text
    | -- | File access error
      ResolutionError !Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

{- | Parse all file references from a prompt text.

Recognizes the syntax @[[file:path]]@ and @[[file:path#L10-L20]]@.

>>> parseFileReferences "Check [[file:src/Main.hs]] and [[file:test/Main.hs#L1-L10]]"
[FileReference {filePath = "src/Main.hs", lineRange = Nothing},
 FileReference {filePath = "test/Main.hs", lineRange = Just (LineRange 1 10)}]
-}
parseFileReferences :: Text -> [FileReference]
parseFileReferences text = mapMaybe parseFileReference refs
  where
    refs = extractBrackets text

-- | Extract text within @[[...]]@ brackets.
extractBrackets :: Text -> [Text]
extractBrackets = go
  where
    go txt =
        case Text.breakOn "[[" txt of
            (_, "") -> []
            (_, afterOpen) ->
                let inner = Text.drop 2 afterOpen
                 in case Text.breakOn "]]" inner of
                        (_, "") -> [] -- Unclosed bracket
                        (content, afterClose) ->
                            content : go (Text.drop 2 afterClose)

{- | Parse a single file reference from bracket content.

Expected format: @file:path@ or @file:path#L10-L20@
-}
parseFileReference :: Text -> Maybe FileReference
parseFileReference content = do
    -- Must start with "file:"
    pathPart <- Text.stripPrefix "file:" content
    let (path, range) = parseRange pathPart
    -- Validate path is not empty
    if Text.null (Text.strip path)
        then Nothing
        else Just $ FileReference (Text.unpack path) range

{- | Parse optional line range from path.

Handles formats:
- @path#L10-L20@ - lines 10 to 20
- @path#L10-@ - lines 10 to end
- @path#L10@ - single line 10
-}
parseRange :: Text -> (Text, Maybe LineRange)
parseRange txt =
    case Text.breakOn "#L" txt of
        (path, "") -> (path, Nothing) -- No range specified
        (path, rangePart) ->
            let rangeText = Text.drop 2 rangePart -- Remove "#L"
             in (path, parseLineRange rangeText)

{- | Parse a line range specification.

Formats:
- @10-20@ -> lines 10 to 20
- @10-@ -> lines 10 to end (represented as -1)
- @10@ -> single line 10
-}
parseLineRange :: Text -> Maybe LineRange
parseLineRange txt
    | Text.null txt = Nothing
    | otherwise =
        case Text.breakOn "-" txt of
            (startText, "") -> do
                -- Single line: "10"
                start <- readMaybe (Text.unpack startText)
                if start > 0
                    then Just $ LineRange start start
                    else Nothing
            (startText, "-") -> do
                -- Open-ended range: "10-" (to end of file)
                start <- readMaybe (Text.unpack startText)
                if start > 0
                    then Just $ LineRange start (-1)
                    else Nothing
            (startText, endPart) -> do
                -- Closed range: "10-20"
                start <- readMaybe (Text.unpack startText)
                let endText = Text.drop 1 endPart
                end <-
                    if Text.null endText
                        then Just (-1) -- "10-" case
                        else readMaybe (Text.unpack endText)
                if start > 0 && (end == -1 || end >= start)
                    then Just $ LineRange start end
                    else Nothing

-------------------------------------------------------------------------------
-- Resolution
-------------------------------------------------------------------------------

{- | Resolve multiple file references to their content.

Returns a list of tuples containing the original reference and its resolution.
-}
resolveReferences :: [FileReference] -> IO [(FileReference, FileResolution)]
resolveReferences refs = mapM resolve refs
  where
    resolve ref = do
        result <- resolveReference ref
        return (ref, result)

-- | Resolve a single file reference to its content.
resolveReference :: FileReference -> IO FileResolution
resolveReference FileReference{..} = do
    exists <- doesFileExist filePath
    if not exists
        then return $ ResolutionError $ "File not found: " <> Text.pack filePath
        else do
            result <- try $ Text.readFile filePath
            case result of
                Left (e :: IOException) ->
                    return $ ResolutionError $ "Cannot read file: " <> Text.pack (show e)
                Right content ->
                    case lineRange of
                        Nothing -> return $ ResolutionSuccess content
                        Just range -> do
                            case readFileRange content range of
                                Left err -> return $ ResolutionError err
                                Right extracted -> return $ ResolutionSuccess extracted

{- | Extract a line range from file content.

Line numbers are 1-indexed. A range end of -1 means "to end of file".
-}
readFileRange :: Text -> LineRange -> Either Text Text
readFileRange content (LineRange start end) = do
    let lines_ = Text.lines content
        totalLines = length lines_

    -- Validate start line
    if start < 1 || start > totalLines
        then
            Left $
                "Invalid line range: line "
                    <> Text.pack (show start)
                    <> " is outside file bounds (1-"
                    <> Text.pack (show totalLines)
                    <> ")"
        else do
            let actualEnd = if end == -1 then totalLines else min end totalLines
            -- Validate end line
            if actualEnd < start
                then
                    Left $
                        "Invalid line range: end line ("
                            <> Text.pack (show actualEnd)
                            <> ") is before start line ("
                            <> Text.pack (show start)
                            <> ")"
                else do
                    let selected = take (actualEnd - start + 1) (drop (start - 1) lines_)
                    return $ Text.unlines selected

-------------------------------------------------------------------------------
-- Inlining
-------------------------------------------------------------------------------

{- | Inline resolved file references into the original prompt text.

Each @[[file:path]]@ marker is replaced with the formatted file content.
References that failed to resolve are replaced with error messages.
-}
inlineReferences :: Text -> [(FileReference, FileResolution)] -> Text
inlineReferences = foldl' inlineOne
  where
    inlineOne :: Text -> (FileReference, FileResolution) -> Text
    inlineOne txt (ref, resolution) =
        let marker = referenceToMarker ref
            replacement = case resolution of
                ResolutionSuccess content -> formatFileContent ref content
                ResolutionError err -> formatError ref err
         in Text.replace marker replacement txt

    foldl' :: (a -> b -> a) -> a -> [b] -> a
    foldl' _f z [] = z
    foldl' f z (x : xs) = foldl' f (f z x) xs

-- | Convert a FileReference back to its marker syntax.
referenceToMarker :: FileReference -> Text
referenceToMarker FileReference{..} =
    "[[file:" <> Text.pack filePath <> rangeSuffix <> "]]"
  where
    rangeSuffix = case lineRange of
        Nothing -> ""
        Just (LineRange start (-1)) -> "#L" <> Text.pack (show start) <> "-"
        Just (LineRange start end) ->
            if start == end
                then "#L" <> Text.pack (show start)
                else "#L" <> Text.pack (show start) <> "-" <> Text.pack (show end)

{- | Format file content for inclusion in prompt.

Wraps the content in a markdown code block with file path annotation.
-}
formatFileContent :: FileReference -> Text -> Text
formatFileContent FileReference{..} content =
    let lang = guessLanguage filePath
        header = "--- Content of " <> Text.pack filePath <> rangeInfo <> " ---"
        footer = "--- End of " <> Text.pack filePath <> " ---"
        rangeInfo = case lineRange of
            Nothing -> ""
            Just (LineRange start (-1)) -> " (lines " <> Text.pack (show start) <> "-)"
            Just (LineRange start end) ->
                if start == end
                    then " (line " <> Text.pack (show start) <> ")"
                    else " (lines " <> Text.pack (show start) <> "-" <> Text.pack (show end) <> ")"
     in Text.unlines [header, "```" <> lang, content, "```", footer]

-- | Format an error message for failed resolution.
formatError :: FileReference -> Text -> Text
formatError FileReference{..} err =
    Text.unlines
        [ "--- Error including " <> Text.pack filePath <> " ---"
        , "_" <> err <> "_"
        , "---"
        ]

-- | Guess the language for syntax highlighting from file extension.
guessLanguage :: FilePath -> Text
guessLanguage path =
    let ext = Text.toLower $ Text.pack $ takeExtension path
     in case ext of
            ".hs" -> "haskell"
            ".json" -> "json"
            ".yaml" -> "yaml"
            ".yml" -> "yaml"
            ".md" -> "markdown"
            ".sh" -> "bash"
            ".py" -> "python"
            ".js" -> "javascript"
            ".ts" -> "typescript"
            ".html" -> "html"
            ".css" -> "css"
            ".sql" -> "sql"
            ".lua" -> "lua"
            ".c" -> "c"
            ".cpp" -> "cpp"
            ".h" -> "c"
            ".hpp" -> "cpp"
            ".rs" -> "rust"
            ".go" -> "go"
            ".java" -> "java"
            ".rb" -> "ruby"
            _ -> ""
  where
    takeExtension p = case break (== '.') (reverse p) of
        (_, "") -> ""
        (ext, _ : _) -> reverse ext

-------------------------------------------------------------------------------
-- Preview
-------------------------------------------------------------------------------

{- | Generate a preview of which files will be included.

Useful for showing users what will be sent before actually sending.
-}
previewReferences :: [(FileReference, FileResolution)] -> Text
previewReferences refs =
    if null refs
        then "No file references found in prompt."
        else Text.unlines $ header : map previewLine refs
  where
    header = "File references to be included:"
    previewLine (ref, res) =
        "  - " <> Text.pack (filePath ref) <> rangeStr (lineRange ref) <> status res
    rangeStr Nothing = ""
    rangeStr (Just (LineRange start (-1))) = " (lines " <> Text.pack (show start) <> "-)"
    rangeStr (Just (LineRange start end)) =
        if start == end
            then " (line " <> Text.pack (show start) <> ")"
            else " (lines " <> Text.pack (show start) <> "-" <> Text.pack (show end) <> ")"
    status (ResolutionSuccess content) =
        " [" <> Text.pack (show $ Text.count "\n" content) <> " lines]"
    status (ResolutionError err) = " [ERROR: " <> err <> "]"
