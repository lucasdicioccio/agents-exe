{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Write file range capability for the DeveloperToolbox.

This module provides functionality to write to specific line ranges in files.
Unlike full file writes, this allows surgical line-by-line modifications.
-}
module System.Agents.Tools.DeveloperToolbox.Write (
    -- * Write file range
    executeWriteFileRange,
) where

import Control.Exception (SomeException, try)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

import Prod.Tracer (Tracer (..))

import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolCapability (..),
    DeveloperToolError (..),
    RangeSpec (..),
    Toolbox (..),
    Trace (..),
    WriteFileRangeResult (..),
 )
import System.Agents.Tools.DeveloperToolbox.Range (parseRanges)
import System.Agents.Tools.DeveloperToolbox.IO (writeFileAtomic)

{- | Execute write file range operation.

Replaces specific lines in a file with new content. Unlike bash-write-file which
replaces the entire file, this capability allows surgical line-by-line modifications.

Parameters:
- path: Path to the file to modify
- ranges: Comma-separated line numbers (e.g., "2,5,8" for lines 2, 5, and 8)
          Each number represents a single line to replace, or use ranges like "1-3".
- contentBlocks: List of content blocks, where each block corresponds to one range.
                 Use empty blocks to delete lines. Each block is treated as the
                 replacement content for its corresponding range.

Examples:

1. Replace single line:
   ranges="5", contentBlocks=["new content for line 5"]

2. Replace multiple lines:
   ranges="2,5,8"
   contentBlocks=["replace line 2", "replace line 5", "replace line 8"]

3. Delete lines (empty content blocks):
   ranges="3,7"
   contentBlocks=["", ""]
   This deletes lines 3 and 7.

4. Replace ranges with multi-line content:
   ranges="1-2,5-6"
   contentBlocks=["new line 1\nnew line 2", "new line 5\nnew line 6"]

Processing:
- Ranges are sorted and processed in ascending order (top-to-bottom)
- Position tracking automatically adjusts for line count changes after each edit
- If a range becomes out-of-bounds due to previous deletions, it's skipped

Returns Right with WriteFileRangeResult on success, Left with error on failure.

Note: For complex multi-line edits or when context validation is needed,
consider using patch-file instead, which provides atomic operations with
unified diff format.
-}
executeWriteFileRange ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    -- | Comma-separated line ranges (e.g., "1-3,5,7-9")
    Text ->
    -- | List of content blocks, one per range
    [Text] ->
    IO (Either DeveloperToolError WriteFileRangeResult)
executeWriteFileRange tracer toolbox filePath rangesTxt contentBlocks = do
    if DevToolWriteFileRange `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "write-file-range"
        else do
            runTracer tracer (WriteFileRangeStartedTrace filePath rangesTxt)

            -- Parse ranges
            case parseRanges rangesTxt of
                Left err -> pure $ Left err
                Right ranges -> do
                    -- Expand consecutive ranges so each line gets its own content block
                    let expandedPairs = expandRangeContentPairs ranges (if null contentBlocks then [""] else contentBlocks)

                    -- Validate that number of content blocks matches total lines after expansion
                    let totalLinesInRanges = countTotalLines ranges
                    let totalContentBlocks = length expandedPairs
                    if totalContentBlocks /= totalLinesInRanges && totalLinesInRanges > 1
                        then
                            pure $
                                Left $
                                    InvalidRangeError $
                                        "Number of content blocks ("
                                            <> Text.pack (show totalContentBlocks)
                                            <> ") must match total lines in ranges ("
                                            <> Text.pack (show totalLinesInRanges)
                                            <> ")"
                        else do
                            -- Check if file exists
                            fileExists <- doesFileExist filePath

                            if not fileExists && not (any isHeadOrTail ranges)
                                then do
                                    let err = FileNotFoundError filePath
                                    runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show err)
                                    pure $ Left err
                                else do
                                    -- Read existing file or start empty
                                    existingContent <-
                                        if fileExists
                                            then Text.readFile filePath
                                            else pure ""

                                    let existingLines = Text.lines existingContent

                                    -- Sort by start line in ascending order (top-to-bottom)
                                    -- This is crucial for correct position tracking
                                    let sortedPairs = sortOn rangeStartKey expandedPairs

                                    -- Apply edits sequentially with position tracking
                                    -- foldl' passes (currentLines, offset) through each edit
                                    let (finalLines, _) = foldl (applyRangeEdit existingLines) (existingLines, 0) sortedPairs

                                    -- Preserve trailing newline if original had one
                                    let hasTrailingNewline = not (Text.null existingContent) && Text.last existingContent == '\n'
                                    let output =
                                            if hasTrailingNewline || null existingLines
                                                then Text.unlines finalLines
                                                else Text.unlines finalLines

                                    -- Write result atomically using a temp file
                                    writeResult <- try $ do
                                        createDirectoryIfMissing True (takeDirectory filePath)
                                        writeFileAtomic filePath output

                                    case writeResult of
                                        Left (e :: SomeException) -> do
                                            let err = PermissionError $ Text.pack $ show e
                                            runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show e)
                                            pure $ Left err
                                        Right () -> do
                                            let rangesModified = length ranges
                                            let linesWritten = sum $ map (length . Text.lines) contentBlocks

                                            runTracer tracer (WriteFileRangeCompletedTrace filePath rangesModified linesWritten)

                                            pure $
                                                Right $
                                                    WriteFileRangeResult
                                                        { writeFilePath = filePath
                                                        , writeFileRangesModified = rangesModified
                                                        , writeFileLinesWritten = linesWritten
                                                        }
  where
    isHeadOrTail Head = True
    isHeadOrTail Tail = True
    isHeadOrTail _ = False

    -- Count total lines spanned by all ranges (excluding head/tail)
    countTotalLines :: [RangeSpec] -> Int
    countTotalLines = sum . map countLinesInRange
      where
        countLinesInRange Head = 0
        countLinesInRange Tail = 0
        countLinesInRange (Lines (start, end)) = end - start + 1

    -- Expand range-content pairs so that consecutive ranges produce individual line entries
    -- Each line in a multi-line range gets the same content block
    expandRangeContentPairs :: [RangeSpec] -> [Text] -> [(RangeSpec, Text)]
    expandRangeContentPairs [] _ = []
    expandRangeContentPairs _ [] = []
    expandRangeContentPairs (Head : rs) (c : cs) = (Head, c) : expandRangeContentPairs rs cs
    expandRangeContentPairs (Tail : rs) (c : cs) = (Tail, c) : expandRangeContentPairs rs cs
    expandRangeContentPairs (Lines (s, e) : rs) (c : cs)
        | s == e = (Lines (s, s), c) : expandRangeContentPairs rs cs
        | otherwise =
            -- Expand multi-line range: each line gets the same content
            map (\n -> (Lines (n, n), c)) [s .. e] ++ expandRangeContentPairs rs cs

    -- Get sort key for range (for top-to-bottom ordering)
    -- Head comes first (0), then line numbers, Tail comes last (maxBound)
    rangeStartKey :: (RangeSpec, Text) -> Int
    rangeStartKey (Head, _) = 0
    rangeStartKey (Lines (n, _), _) = n
    rangeStartKey (Tail, _) = maxBound

    -- Apply a single range edit with position tracking
    -- Takes the original file lines (for bounds checking), current state (lines, offset),
    -- and the edit to apply. Returns the new state (updated lines, updated offset).
    applyRangeEdit ::
        [Text] ->
        ([Text], Int) ->
        (RangeSpec, Text) ->
        ([Text], Int)
    applyRangeEdit _originalLines (currentLines, offset) (range, content) =
        let newLines = Text.lines content
         in case range of
                Head ->
                    -- Prepend content: offset increases by number of lines added
                    let newOffset = offset + length newLines
                     in (newLines ++ currentLines, newOffset)
                Tail ->
                    -- Append content: offset increases by number of lines added
                    -- (though subsequent Tail edits would be unusual)
                    let newOffset = offset + length newLines
                     in (currentLines ++ newLines, newOffset)
                Lines (originalStart, originalEnd) ->
                    -- Adjust positions based on running offset
                    let adjustedStart = originalStart + offset
                        adjustedEnd = originalEnd + offset
                        -- Clamp to current file bounds
                        actualStart = max 1 adjustedStart
                        actualEnd = min adjustedEnd (length currentLines)
                     in if actualStart > actualEnd || actualStart > length currentLines
                            then -- Range is out of bounds after adjustment, skip
                                (currentLines, offset)
                            else
                                let before = take (actualStart - 1) currentLines
                                    after = drop actualEnd currentLines
                                    linesRemoved = actualEnd - actualStart + 1
                                    linesAdded = length newLines
                                    newOffset = offset + (linesAdded - linesRemoved)
                                 in (before ++ newLines ++ after, newOffset)

