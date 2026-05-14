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

import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolCapability (..),
    DeveloperToolError (..),
    RangeSpec (..),
    ReadFileRangeResult (..),
    Toolbox (..),
    Trace (..),
 )
import System.Agents.Tools.DeveloperToolbox.Range (parseRanges)

{- | Execute read file range operation.

Reads specific line ranges from a file and returns them with line numbers
prepended as "{line_num}\t{line_content}".

Parameters:
- path: Path to the file to read
- ranges: Comma-separated line ranges (e.g., "1-10", "5", "head", "tail", "whole")

Returns Right with ReadFileRangeResult on success, Left with error on failure.
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

            -- Check if file exists
            fileExists <- doesFileExist filePath
            if not fileExists
                then do
                    let err = FileNotFoundError filePath
                    runTracer tracer (DeveloperToolErrorTrace "read-file-range" $ Text.pack $ show err)
                    pure $ Left err
                else do
                    -- Parse ranges
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
                                    let resultLines = extractLines allLines ranges
                                    let output = Text.unlines $ map formatLineWithNumber resultLines
                                    let linesRead = length resultLines

                                    runTracer tracer (ReadFileRangeCompletedTrace filePath linesRead)

                                    pure $
                                        Right $
                                            ReadFileRangeResult
                                                { readFilePath = filePath
                                                , readFileContent = output
                                                , readFileLinesRead = linesRead
                                                }

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

