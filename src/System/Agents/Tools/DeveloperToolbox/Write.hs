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

import System.Agents.Tools.DeveloperToolbox.IO (writeFileAtomic)
import System.Agents.Tools.DeveloperToolbox.Range (parseRanges)
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolCapability (..),
    DeveloperToolError (..),
    RangeEditResult (..),
    RangeSpec (..),
    Toolbox (..),
    Trace (..),
    WriteFileRangeResult (..),
 )

{- | Execute write file range operation.

Replaces specific lines in a file with new content. Unlike bash-write-file which
replaces the entire file, this capability allows surgical line-by-line modifications.

Parameters:
- path: Path to the file to modify
- ranges: Comma-separated line numbers (e.g., "2,5,8" for lines 2, 5, and 8)
          Each number represents a single line to replace, or use ranges like "1-3".
          Special values: "head" (prepend), "tail" (append), "whole" (overwrite entire file)
          Insert-after: "N+" (insert after line N), "N-M+" (insert after line M)
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

5. Insert after specific line:
   ranges="3+"
   contentBlocks=["inserted after line 3"]

6. Insert after range:
   ranges="2-4+"
   contentBlocks=["inserted after line 4"]

7. Overwrite entire file:
   ranges="whole"
   contentBlocks=["entire new file content"]

Processing:
- Ranges are sorted and processed in ascending order (top-to-bottom)
- Position tracking automatically adjusts for line count changes after each edit
- If a range becomes out-of-bounds due to previous deletions, it's skipped

Returns Right with WriteFileRangeResult on success, Left with error on failure.
The result includes detailed per-range information showing where each edit landed.

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
                    -- Validate that head/tail/whole are not combined with other ranges
                    case validateSpecialRanges ranges of
                        Left err -> pure $ Left err
                        Right () -> do
                            -- Validate that number of content blocks matches number of ranges
                            -- Each range gets exactly one content block (even for multi-line ranges)
                            let numRanges = length ranges
                            let numContentBlocks = length (if null contentBlocks then [""] else contentBlocks)
                            if numContentBlocks /= numRanges
                                then
                                    pure $
                                        Left $
                                            InvalidRangeError $
                                                "Number of content blocks ("
                                                    <> Text.pack (show numContentBlocks)
                                                    <> ") must match number of ranges ("
                                                    <> Text.pack (show numRanges)
                                                    <> ")"
                                else do
                                    -- Check if file exists
                                    fileExists <- doesFileExist filePath

                                    if not fileExists && not (any isSpecialRange ranges)
                                        then do
                                            let err = FileNotFoundError filePath
                                            runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show err)
                                            pure $ Left err
                                        else do
                                            -- Handle "whole" as a special case - just write the content directly
                                            case ranges of
                                                [Whole] -> do
                                                    let content = case contentBlocks of
                                                            (c : _) -> c
                                                            [] -> ""
                                                    writeResult <- try $ do
                                                        createDirectoryIfMissing True (takeDirectory filePath)
                                                        writeFileAtomic filePath content
                                                    case writeResult of
                                                        Left (e :: SomeException) -> do
                                                            let err = PermissionError $ Text.pack $ show e
                                                            runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show e)
                                                            pure $ Left err
                                                        Right () -> do
                                                            let newLines = Text.lines content
                                                            let linesWritten = length newLines
                                                            let finalLineCount = length newLines
                                                            let rangeResult =
                                                                    RangeEditResult
                                                                        { rangeEditSpec = "whole"
                                                                        , rangeEditOriginalStart = 1
                                                                        , rangeEditOriginalEnd = 0 -- Will be set from original file below
                                                                        , rangeEditLinesWritten = linesWritten
                                                                        , rangeEditFinalStartLine = if linesWritten == 0 then Nothing else Just 1
                                                                        , rangeEditFinalEndLine = if linesWritten == 0 then Nothing else Just linesWritten
                                                                        , rangeEditOperation = "overwrite"
                                                                        }
                                                            runTracer tracer (WriteFileRangeCompletedTrace filePath 1 linesWritten)
                                                            pure $
                                                                Right $
                                                                    WriteFileRangeResult
                                                                        { writeFilePath = filePath
                                                                        , writeFileRangesModified = 1
                                                                        , writeFileLinesWritten = linesWritten
                                                                        , writeFileFinalLineCount = finalLineCount
                                                                        , writeFileRangeResults = [rangeResult]
                                                                        }
                                                _ -> do
                                                    -- Read existing file or start empty
                                                    existingContent <-
                                                        if fileExists
                                                            then Text.readFile filePath
                                                            else pure ""

                                                    let originalLines = Text.lines existingContent
                                                    let originalLineCount = length originalLines

                                                    -- Pair ranges with content blocks (1:1 mapping)
                                                    -- Multi-line ranges are kept as-is, not expanded
                                                    let rangeContentPairs = zip ranges (if null contentBlocks then [""] else contentBlocks)

                                                    -- Sort by start line in ascending order (top-to-bottom)
                                                    -- This is crucial for correct position tracking
                                                    let sortedPairs = sortOn rangeStartKey rangeContentPairs

                                                    -- Apply edits sequentially with position tracking
                                                    -- foldl passes (currentLines, offset, results) through each edit
                                                    let (finalLines, _, rangeResults) = foldl (applyRangeEdit originalLineCount) (originalLines, 0, []) sortedPairs

                                                    -- Preserve trailing newline if original had one
                                                    let hasTrailingNewline = not (Text.null existingContent) && Text.last existingContent == '\n'
                                                    let output =
                                                            if hasTrailingNewline || null originalLines
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
                                                            let finalLineCount = length finalLines
                                                            let linesWritten = sum $ map rangeEditLinesWritten rangeResults

                                                            runTracer tracer (WriteFileRangeCompletedTrace filePath (length ranges) linesWritten)

                                                            pure $
                                                                Right $
                                                                    WriteFileRangeResult
                                                                        { writeFilePath = filePath
                                                                        , writeFileRangesModified = length ranges
                                                                        , writeFileLinesWritten = linesWritten
                                                                        , writeFileFinalLineCount = finalLineCount
                                                                        , writeFileRangeResults = rangeResults
                                                                        }
  where
    isSpecialRange Head = True
    isSpecialRange Tail = True
    isSpecialRange Whole = True
    isSpecialRange (After _) = True
    isSpecialRange _ = False

    -- Validate that head/tail/whole are not combined with other ranges
    validateSpecialRanges :: [RangeSpec] -> Either DeveloperToolError ()
    validateSpecialRanges ranges =
        let hasHead = any (== Head) ranges
            hasTail = any (== Tail) ranges
            hasWhole = any (== Whole) ranges
            hasAfter = any isAfterRange ranges
            hasLines = any isLineRange ranges
            specialCount = length $ filter (\r -> r == Head || r == Tail || r == Whole) ranges
         in if hasWhole && (hasHead || hasTail || hasLines || hasAfter)
                then Left $ InvalidRangeError "'whole' cannot be combined with other ranges. Use it alone."
                else
                    if (hasHead || hasTail) && (hasLines || hasAfter)
                        then Left $ InvalidRangeError "'head' and 'tail' cannot be combined with line number ranges. Use them separately."
                        else
                            if specialCount > 1
                                then Left $ InvalidRangeError "Only one 'head', 'tail', or 'whole' range is allowed."
                                else Right ()

    isLineRange (Lines _) = True
    isLineRange _ = False

    isAfterRange (After _) = True
    isAfterRange _ = False

    -- Get sort key for range (for top-to-bottom ordering)
    -- Head comes first (0), then line numbers (sorted by start), After comes after its line number
    -- (using n*2+1 to place it between line n and line n+1), Tail comes last (maxBound)
    -- Whole is handled separately and won't appear here
    rangeStartKey :: (RangeSpec, Text) -> Int
    rangeStartKey (Head, _) = 0
    rangeStartKey (Lines (s, _), _) = s * 2 -- Even numbers for line ranges
    rangeStartKey (After n, _) = n * 2 + 1 -- Odd numbers for insert-after (between lines)
    rangeStartKey (Tail, _) = maxBound
    rangeStartKey (Whole, _) = 0 -- Should not appear in normal processing

    -- Apply a single range edit with position tracking
    -- Takes the original file line count (for bounds/original position info),
    -- current state (lines, offset, results), and the edit to apply.
    -- Returns the new state (updated lines, updated offset, updated results).
    applyRangeEdit ::
        Int ->
        ([Text], Int, [RangeEditResult]) ->
        (RangeSpec, Text) ->
        ([Text], Int, [RangeEditResult])
    applyRangeEdit origLineCount (currentLines, offset, results) (range, content) =
        let newLines = Text.lines content
            numNewLines = length newLines
            (newCurrentLines, newOffset, editResult) = case range of
                Head ->
                    let result =
                            RangeEditResult
                                { rangeEditSpec = "head"
                                , rangeEditOriginalStart = 0
                                , rangeEditOriginalEnd = 0
                                , rangeEditLinesWritten = numNewLines
                                , rangeEditFinalStartLine = if numNewLines == 0 then Nothing else Just 1
                                , rangeEditFinalEndLine = if numNewLines == 0 then Nothing else Just numNewLines
                                , rangeEditOperation = "prepend"
                                }
                     in (newLines ++ currentLines, offset + numNewLines, result)
                Tail ->
                    let finalStart = length currentLines + offset + 1
                        result =
                            RangeEditResult
                                { rangeEditSpec = "tail"
                                , rangeEditOriginalStart = origLineCount + 1
                                , rangeEditOriginalEnd = origLineCount
                                , rangeEditLinesWritten = numNewLines
                                , rangeEditFinalStartLine = if numNewLines == 0 then Nothing else Just finalStart
                                , rangeEditFinalEndLine = if numNewLines == 0 then Nothing else Just (finalStart + numNewLines - 1)
                                , rangeEditOperation = "append"
                                }
                     in (currentLines ++ newLines, offset + numNewLines, result)
                Whole ->
                    -- Overwrite entire file: replace all lines
                    -- This shouldn't happen in normal processing as Whole is handled separately
                    let result =
                            RangeEditResult
                                { rangeEditSpec = "whole"
                                , rangeEditOriginalStart = 1
                                , rangeEditOriginalEnd = origLineCount
                                , rangeEditLinesWritten = numNewLines
                                , rangeEditFinalStartLine = if numNewLines == 0 then Nothing else Just 1
                                , rangeEditFinalEndLine = if numNewLines == 0 then Nothing else Just numNewLines
                                , rangeEditOperation = "overwrite"
                                }
                     in (newLines, 0, result)
                After origLine ->
                    -- Insert after the specified line
                    -- Adjust position based on running offset
                    let adjustedLine = origLine + offset
                        -- Clamp to valid range (0 to length, where 0 means before first line, like head)
                        insertPos = max 0 (min adjustedLine (length currentLines))
                        -- Calculate final position (1-based for display)
                        finalStart = insertPos + 1
                        finalEnd = insertPos + numNewLines
                        result =
                            RangeEditResult
                                { rangeEditSpec = Text.pack (show origLine) <> "+"
                                , rangeEditOriginalStart = origLine
                                , rangeEditOriginalEnd = origLine
                                , rangeEditLinesWritten = numNewLines
                                , rangeEditFinalStartLine = if numNewLines == 0 then Nothing else Just finalStart
                                , rangeEditFinalEndLine = if numNewLines == 0 then Nothing else Just finalEnd
                                , rangeEditOperation = "insert-after"
                                }
                        (before, after) = splitAt insertPos currentLines
                     in (before ++ newLines ++ after, offset + numNewLines, result)
                Lines (origStart, origEnd) ->
                    -- Adjust positions based on running offset
                    let adjustedStart = origStart + offset
                        adjustedEnd = origEnd + offset
                        -- Clamp to current file bounds
                        actualStart = max 1 adjustedStart
                        actualEnd = min adjustedEnd (length currentLines)
                     in if actualStart > actualEnd || actualStart > length currentLines
                            then -- Range is out of bounds after adjustment, skip
                                let result =
                                        RangeEditResult
                                            { rangeEditSpec =
                                                Text.pack (show origStart)
                                                    <> if origStart == origEnd then "" else "-" <> Text.pack (show origEnd)
                                            , rangeEditOriginalStart = origStart
                                            , rangeEditOriginalEnd = origEnd
                                            , rangeEditLinesWritten = 0
                                            , rangeEditFinalStartLine = Nothing
                                            , rangeEditFinalEndLine = Nothing
                                            , rangeEditOperation = "skipped-out-of-bounds"
                                            }
                                 in (currentLines, offset, result)
                            else
                                let before = take (actualStart - 1) currentLines
                                    after = drop actualEnd currentLines
                                    linesRemoved = actualEnd - actualStart + 1
                                    linesAdded = numNewLines
                                    offsetDelta = offset + (linesAdded - linesRemoved)
                                    finalStart = actualStart
                                    finalEnd = actualStart + numNewLines - 1
                                    operation = if numNewLines == 0 then "delete" else "replace"
                                    result =
                                        RangeEditResult
                                            { rangeEditSpec =
                                                Text.pack (show origStart)
                                                    <> if origStart == origEnd then "" else "-" <> Text.pack (show origEnd)
                                            , rangeEditOriginalStart = origStart
                                            , rangeEditOriginalEnd = origEnd
                                            , rangeEditLinesWritten = numNewLines
                                            , rangeEditFinalStartLine = if numNewLines == 0 then Nothing else Just finalStart
                                            , rangeEditFinalEndLine = if numNewLines == 0 then Nothing else Just finalEnd
                                            , rangeEditOperation = operation
                                            }
                                 in (before ++ newLines ++ after, offsetDelta, result)
         in (newCurrentLines, newOffset, results ++ [editResult])
