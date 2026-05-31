{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Read file range capability for the DeveloperToolbox.

This module provides functionality to read specific line ranges from files,
returning them with line numbers prepended.
-}
module System.Agents.Tools.DeveloperToolbox.Read (
    -- * Read file range
    executeReadFileRange,
    extractLines,
    extractRange,
    formatLineWithNumber,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist)

import Prod.Tracer (Tracer (..))

import System.Agents.FileSandbox (AccessResult (..), validateFileRead)
import System.Agents.Tools.DeveloperToolbox.Range (parseRanges)
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolCapability (..),
    DeveloperToolError (..),
    RangeSpec (..),
    ReadFileRangeResult (..),
    Toolbox (..),
    Trace (..),
 )

{- | Execute read file range operation.

Reads specific line ranges from a file and returns them with line numbers
prepended as "{line_num}\t{line_content}".

When a file sandbox is configured, the filepath is validated against
the sandbox before reading. Access is denied if the file is outside
the allowed paths.

Parameters:
- path: Path to the file to read
- ranges: Comma-separated line ranges (e.g., "1-10", "5", "head", "tail", "whole").
          If omitted or empty, defaults to reading the entire file (equivalent to "whole").

Returns Right with ReadFileRangeResult on success, Left with error on failure.

The result includes metadata fields to help distinguish between different scenarios:
- totalFileSize: Total file size in bytes
- totalLineCount: Total number of lines in the file
- rangesParsed: The normalized range specifications that were applied
-}
executeReadFileRange ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    IO (Either DeveloperToolError ReadFileRangeResult)
executeReadFileRange tracer toolbox filePath rangesTxt = do
    if DevToolReadFileRange `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "read-file-range"
        else do
            runTracer tracer (ReadFileRangeStartedTrace filePath rangesTxt)

            -- Validate against file sandbox if configured
            case toolboxFileSandbox toolbox of
                Just sandbox -> do
                    accessResult <- validateFileRead sandbox filePath
                    case accessResult of
                        AccessDenied err -> do
                            let errMsg = FileAccessDeniedError filePath (Text.pack $ show err)
                            runTracer tracer (DeveloperToolErrorTrace "read-file-range" $ Text.pack $ show errMsg)
                            pure $ Left errMsg
                        AccessGranted -> proceedWithRead tracer filePath rangesTxt
                Nothing ->
                    -- No sandbox configured, proceed (backwards compatible behavior)
                    proceedWithRead tracer filePath rangesTxt

-- | Proceed with file read after validation.
proceedWithRead ::
    Tracer IO Trace ->
    FilePath ->
    Text ->
    IO (Either DeveloperToolError ReadFileRangeResult)
proceedWithRead tracer filePath rangesTxt = do
    -- Check if file exists
    fileExists <- doesFileExist filePath
    if not fileExists
        then do
            let err = FileNotFoundError filePath
            runTracer tracer (DeveloperToolErrorTrace "read-file-range" $ Text.pack $ show err)
            pure $ Left err
        else do
            -- Parse ranges (empty string defaults to [Whole])
            case parseRanges rangesTxt of
                Left err -> pure $ Left err
                Right ranges -> do
                    -- Read file and extract lines
                    result <- try $ Text.readFile filePath
                    case result of
                        Left (e :: SomeException) -> do
                            let err = PermissionError $ Text.pack $ show e
                            runTracer tracer (DeveloperToolErrorTrace "read-file-range" $ Text.pack $ show e)
                            pure $ Left err
                        Right content -> do
                            let allLines = Text.lines content
                            let totalLineCount = length allLines
                            let totalFileSize = Text.length content
                            let resultLines = extractLines allLines ranges
                            let output = Text.unlines $ map formatLineWithNumber resultLines
                            let linesRead = length resultLines
                            let rangesParsed = map rangeSpecToText ranges

                            runTracer tracer (ReadFileRangeCompletedTrace filePath linesRead)

                            pure $
                                Right $
                                    ReadFileRangeResult
                                        { readFilePath = filePath
                                        , readFileContent = output
                                        , readFileLinesRead = linesRead
                                        , readFileTotalSize = totalFileSize
                                        , readFileTotalLines = totalLineCount
                                        , readFileRangesParsed = rangesParsed
                                        }

-- | Convert a RangeSpec to its text representation.
rangeSpecToText :: RangeSpec -> Text
rangeSpecToText Head = "head"
rangeSpecToText Tail = "tail"
rangeSpecToText Whole = "whole"
rangeSpecToText (After n) = Text.pack (show n) <> "+"
rangeSpecToText (Lines (start, end)) =
    if start == end
        then Text.pack (show start)
        else Text.pack (show start) <> "-" <> Text.pack (show end)

{- | Extract lines based on range specifications.
Returns list of (lineNumber, lineContent) pairs.
-}
extractLines :: [Text] -> [RangeSpec] -> [(Int, Text)]
extractLines allLines ranges =
    concatMap (extractRange allLines) ranges

-- | Extract lines for a single range.
extractRange :: [Text] -> RangeSpec -> [(Int, Text)]
extractRange _ Head = []
extractRange _ Tail = []
extractRange _ (After _) = []
extractRange allLines Whole =
    -- Return all lines with their line numbers
    zip [1 ..] allLines
extractRange allLines (Lines (start, end)) =
    let totalLines = length allLines
        actualStart = max 1 start
        actualEnd = min end totalLines
     in if actualStart > actualEnd || actualStart > totalLines
            then []
            else zip [actualStart ..] (take (actualEnd - actualStart + 1) $ drop (actualStart - 1) allLines)

-- | Format a line with its line number.
formatLineWithNumber :: (Int, Text) -> Text
formatLineWithNumber (n, line) = Text.pack (show n) <> "\t" <> line

