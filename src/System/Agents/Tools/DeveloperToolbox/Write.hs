{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Write file range capability for the DeveloperToolbox.

This module provides functionality to write to specific line ranges in files.
Unlike full file writes, this allows surgical line-by-line modifications.

Optimistic locking is supported via optional expectedSnapshotRef parameter.
When provided, the operation verifies the current file content matches before
making any changes, preventing concurrent edit conflicts.

For edits spanning multiple calls, 'executeWriteFileRangeWith' also supports
multi-turn edit sessions (see the "Multi-turn Edit Sessions" section below):
all ranges within a session are interpreted in the ORIGINAL coordinates of
the snapshot the session started from, regardless of how many edits have
already been applied. This avoids the line-number drift that occurs when
separate write-file-range calls each re-read the file from disk and
re-interpret stale line numbers against it.
-}
module System.Agents.Tools.DeveloperToolbox.Write (
    executeWriteFileRange,
    executeWriteFileRangeWith,
) where

import Control.Exception (SomeException, try)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

import Prod.Tracer (Tracer (..))

import System.Agents.FileSandbox (AccessResult (..), validateFileWrite)
import System.Agents.Tools.DeveloperToolbox.IO (writeFileAtomic)
import System.Agents.Tools.DeveloperToolbox.Range (parseRanges)
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolCapability (..),
    DeveloperToolError (..),
    EditSession (..),
    EditSessionState (..),
    EditSessionStore,
    RangeEditResult (..),
    RangeSpec (..),
    SessionEditRecord (..),
    SessionId (..),
    Snapshot (..),
    SnapshotRef (..),
    Toolbox (..),
    Trace (..),
    WriteFileRangeResult (..),
    makeSnapshot,
    sessionExpirySeconds,
 )

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

{- | Execute write file range operation.

Replaces specific lines in a file with new content. Unlike bash-write-file which
replaces the entire file, this capability allows surgical line-by-line modifications.

Optimistic Locking:
When expectedSnapshotRef is provided, the operation first reads the current file
content and computes its MD5 hash. If the hash doesn't match expectedSnapshotRef,
a SnapshotMismatchError is returned without making any changes. This prevents
lost updates when multiple agents/processes edit the same file.

When a file sandbox is configured, the filepath is validated against
the sandbox before writing. Access is denied if the file is outside
the allowed paths.

Parameters:
- path: Path to the file to modify
- ranges: Comma-separated line numbers (e.g., "2,5,8" for lines 2, 5, and 8)
          Each number represents a single line to replace, or use ranges like "1-3".
          Special values: "head" (prepend), "tail" (append), "whole" (overwrite entire file)
          Insert-after: "N+" (insert after line N), "N-M+" (insert after line M)
- contentBlocks: List of content blocks, where each block corresponds to one range.
                 Use empty blocks to delete lines. Each block is treated as the
                 replacement content for its corresponding range.
- expectedSnapshotRef: Optional MD5 hash of expected file content. If provided,
                       the operation fails with SnapshotMismatchError if the
                       current content doesn't match. Use this for optimistic locking.

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

8. Safe edit with optimistic locking:
   ranges="5", contentBlocks=["new line"], expectedSnapshotRef="a1b2c3d4..."

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
    -- | Optional expected snapshot reference for optimistic locking
    Maybe SnapshotRef ->
    IO (Either DeveloperToolError WriteFileRangeResult)
executeWriteFileRange tracer toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef =
    executeWriteFileRangeWith tracer toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef Nothing Nothing

{- | Like 'executeWriteFileRange', but also supports starting, continuing, and
committing a multi-turn edit session (see the "Multi-turn Edit Sessions"
section below).
-}
executeWriteFileRangeWith ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    -- | Comma-separated line ranges (e.g., "1-3,5,7-9")
    Text ->
    -- | List of content blocks, one per range
    [Text] ->
    -- | Optional expected snapshot reference for optimistic locking
    Maybe SnapshotRef ->
    -- | Optional session id to continue a multi-turn edit session
    Maybe Text ->
    -- | Optional commit flag (Nothing means "use legacy immediate-write behavior")
    Maybe Bool ->
    IO (Either DeveloperToolError WriteFileRangeResult)
executeWriteFileRangeWith tracer toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef mSessionIdTxt mCommit = do
    if DevToolWriteFileRange `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "write-file-range"
        else case (mSessionIdTxt, mCommit) of
            (Nothing, Nothing) -> do
                -- Legacy immediate-write behavior, unchanged.
                runTracer tracer (WriteFileRangeStartedTrace filePath rangesTxt)
                withSandboxCheck tracer toolbox filePath $
                    proceedWithWrite tracer toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef
            _ ->
                -- Session mode: either start, continue, or continue-and-commit a
                -- multi-turn edit session pinned to an original snapshot. Sandbox
                -- validation happens against whichever path is actually touched:
                -- the supplied path when starting, or the session's own path when
                -- continuing/committing (see 'startSession' / 'applyThenWrite').
                runSessionEdit tracer toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef mSessionIdTxt (fromMaybe False mCommit)

-- | Validate a write against the file sandbox (if configured) before running an action.
withSandboxCheck ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    IO (Either DeveloperToolError WriteFileRangeResult) ->
    IO (Either DeveloperToolError WriteFileRangeResult)
withSandboxCheck tracer toolbox filePath action =
    case toolboxFileSandbox toolbox of
        Just sandbox -> do
            accessResult <- validateFileWrite sandbox filePath
            case accessResult of
                AccessDenied err -> do
                    let errMsg = FileAccessDeniedError filePath (Text.pack $ show err)
                    runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show errMsg)
                    pure $ Left errMsg
                AccessGranted -> action
        Nothing ->
            -- No sandbox configured, proceed (backwards compatible behavior)
            action

-- | Proceed with file write after validation.
proceedWithWrite ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    [Text] ->
    Maybe SnapshotRef ->
    IO (Either DeveloperToolError WriteFileRangeResult)
proceedWithWrite tracer toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef = do
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
                                    -- Handle optimistic locking if expectedSnapshotRef is provided
                                    case mExpectedSnapshotRef of
                                        Just expectedRef -> do
                                            checkResult <- checkSnapshotMatch tracer filePath expectedRef
                                            case checkResult of
                                                Left err -> pure $ Left err
                                                Right () -> proceedWithEdits tracer toolbox filePath ranges contentBlocks
                                        Nothing -> proceedWithEdits tracer toolbox filePath ranges contentBlocks

-- | Check if a range spec is a special range (Head, Tail, Whole, or After).
isSpecialRange :: RangeSpec -> Bool
isSpecialRange Head = True
isSpecialRange Tail = True
isSpecialRange Whole = True
isSpecialRange (After _) = True
isSpecialRange _ = False

-- | Validate that head/tail/whole are not combined with other ranges.
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
  where
    isLineRange (Lines _) = True
    isLineRange _ = False

    isAfterRange (After _) = True
    isAfterRange _ = False

-- | Check if current file content matches expected snapshot (optimistic locking).
checkSnapshotMatch ::
    Tracer IO Trace ->
    FilePath ->
    SnapshotRef ->
    IO (Either DeveloperToolError ())
checkSnapshotMatch tracer filePath expectedRef = do
    fileExists <- doesFileExist filePath
    if not fileExists
        then do
            -- File doesn't exist, treat as mismatch
            let actualRef = SnapshotRef "file-not-found"
            let err = SnapshotMismatchError expectedRef actualRef
            runTracer tracer (SnapshotMismatchTrace filePath expectedRef actualRef)
            pure $ Left err
        else do
            content <- BS.readFile filePath
            let actualSnapshot = makeSnapshot content
            let actualRef = snapshotRef actualSnapshot
            if actualRef == expectedRef
                then pure $ Right ()
                else do
                    let err = SnapshotMismatchError expectedRef actualRef
                    runTracer tracer (SnapshotMismatchTrace filePath expectedRef actualRef)
                    pure $ Left err

-- | Proceed with actual file edits after all validations pass.
proceedWithEdits ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    [RangeSpec] ->
    [Text] ->
    IO (Either DeveloperToolError WriteFileRangeResult)
proceedWithEdits tracer toolbox filePath ranges contentBlocks = do
    -- Handle "whole" as a special case - just write the content directly
    case ranges of
        [Whole] -> do
            let content = case contentBlocks of
                    (c : _) -> c
                    [] -> ""
            -- Take snapshot before write if snapshot enabled
            mBeforeSnapshotRef <- takeSnapshotBeforeEdit tracer toolbox filePath
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
                    -- Take after snapshot if snapshot enabled
                    mAfterSnapshotRef <- takeSnapshotAfterEdit toolbox filePath
                    runTracer tracer (WriteFileRangeCompletedTrace filePath 1 linesWritten)
                    pure $
                        Right $
                            WriteFileRangeResult
                                { writeFilePath = filePath
                                , writeFileRangesModified = 1
                                , writeFileLinesWritten = linesWritten
                                , writeFileFinalLineCount = finalLineCount
                                , writeFileRangeResults = [rangeResult]
                                , writeFileBeforeSnapshotRef = mBeforeSnapshotRef
                                , writeFileAfterSnapshotRef = mAfterSnapshotRef
                                , writeFileSessionId = Nothing
                                , writeFileSessionNetDelta = Nothing
                                , writeFileSessionCommitted = True
                                }
        _ -> do
            -- Read existing file or start empty
            fileExists <- doesFileExist filePath
            existingContent <-
                if fileExists
                    then Text.readFile filePath
                    else pure ""

            let originalLines = Text.lines existingContent
            let originalLineCount = length originalLines

            -- Take snapshot before edit if snapshot enabled
            mBeforeSnapshotRef <- takeSnapshotBeforeEdit tracer toolbox filePath

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

                    -- Take after snapshot if snapshot enabled
                    mAfterSnapshotRef <- takeSnapshotAfterEdit toolbox filePath

                    runTracer tracer (WriteFileRangeCompletedTrace filePath (length ranges) linesWritten)

                    pure $
                        Right $
                            WriteFileRangeResult
                                { writeFilePath = filePath
                                , writeFileRangesModified = length ranges
                                , writeFileLinesWritten = linesWritten
                                , writeFileFinalLineCount = finalLineCount
                                , writeFileRangeResults = rangeResults
                                , writeFileBeforeSnapshotRef = mBeforeSnapshotRef
                                , writeFileAfterSnapshotRef = mAfterSnapshotRef
                                , writeFileSessionId = Nothing
                                , writeFileSessionNetDelta = Nothing
                                , writeFileSessionCommitted = True
                                }

{- | Get sort key for range (for top-to-bottom ordering).
Head comes first (0), then line numbers (sorted by start), After comes after its line number
(using n*2+1 to place it between line n and line n+1), Tail comes last (maxBound)
Whole is handled separately and won't appear here
-}
rangeStartKey :: (RangeSpec, Text) -> Int
rangeStartKey (Head, _) = 0
rangeStartKey (Lines (s, _), _) = s * 2 -- Even numbers for line ranges
rangeStartKey (After n, _) = n * 2 + 1 -- Odd numbers for insert-after (between lines)
rangeStartKey (Tail, _) = maxBound
rangeStartKey (Whole, _) = 0 -- Should not appear in normal processing

{- | Apply a single range edit with position tracking.
Takes the original file line count (for bounds/original position info),
current state (lines, offset, results), and the edit to apply.
Returns the new state (updated lines, updated offset, updated results).
-}
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

{- | Take a snapshot of the file before editing, if snapshot capability is enabled.
Returns the snapshot reference if a snapshot was taken, Nothing otherwise.
-}
takeSnapshotBeforeEdit :: Tracer IO Trace -> Toolbox -> FilePath -> IO (Maybe SnapshotRef)
takeSnapshotBeforeEdit tracer toolbox filePath =
    case (toolboxSnapshotStore toolbox, DevToolSnapshot `elem` toolboxCapabilities toolbox) of
        (Just store, True) -> do
            -- Check if file exists
            fileExists <- doesFileExist filePath
            if fileExists
                then do
                    -- Read file content and create snapshot
                    content <- BS.readFile filePath
                    let snapshot = makeSnapshot content
                    let ref = snapshotRef snapshot
                    -- Store snapshot in the store
                    atomically $ modifyTVar' store (Map.insert ref snapshot)
                    runTracer tracer (SnapshotTakenTrace filePath ref)
                    pure $ Just ref
                else pure Nothing
        _ -> pure Nothing

{- | Take a snapshot of the file after editing, if snapshot capability is enabled.
Returns the snapshot reference if a snapshot was taken, Nothing otherwise.
-}
takeSnapshotAfterEdit :: Toolbox -> FilePath -> IO (Maybe SnapshotRef)
takeSnapshotAfterEdit toolbox filePath =
    case (toolboxSnapshotStore toolbox, DevToolSnapshot `elem` toolboxCapabilities toolbox) of
        (Just store, True) -> do
            -- Read file content and create snapshot
            content <- BS.readFile filePath
            let snapshot = makeSnapshot content
            let ref = snapshotRef snapshot
            -- Store snapshot in the store
            atomically $ modifyTVar' store (Map.insert ref snapshot)
            pure $ Just ref
        _ -> pure Nothing

-------------------------------------------------------------------------------
-- Multi-turn Edit Sessions
-------------------------------------------------------------------------------

{- | Dispatch a session-mode write-file-range call: start a new session,
continue an existing one, or continue-and-commit it.

Throughout a session, ranges are always interpreted in the ORIGINAL
coordinates of the snapshot the session started from. This avoids the line
drift that happens when separate write-file-range calls each re-read the
(possibly already-edited) file from disk and re-interpret stale line numbers
against it.
-}
runSessionEdit ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    [Text] ->
    Maybe SnapshotRef ->
    Maybe Text ->
    Bool ->
    IO (Either DeveloperToolError WriteFileRangeResult)
runSessionEdit tracer toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef mSessionIdTxt doCommit =
    case toolboxEditSessionStore toolbox of
        Nothing -> pure $ Left $ CapabilityNotEnabledError "write-file-range"
        Just store -> case mSessionIdTxt of
            Nothing -> startSession tracer store toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef doCommit
            Just sidTxt -> do
                let sid = SessionId sidTxt
                mSession <- lookupLiveSession store sid
                case mSession of
                    Left err -> pure $ Left err
                    Right session -> applyAndMaybeCommit tracer store toolbox sid session rangesTxt contentBlocks doCommit

-- | Start a brand-new edit session pinned to the file's current on-disk snapshot.
startSession ::
    Tracer IO Trace ->
    EditSessionStore ->
    Toolbox ->
    FilePath ->
    Text ->
    [Text] ->
    Maybe SnapshotRef ->
    Bool ->
    IO (Either DeveloperToolError WriteFileRangeResult)
startSession tracer store toolbox filePath rangesTxt contentBlocks mExpectedSnapshotRef doCommit
    | null filePath = pure $ Left $ ValidationError "Missing 'path' parameter"
    | otherwise =
        withSandboxCheck tracer toolbox filePath $
            case mExpectedSnapshotRef of
                Nothing -> pure $ Left $ ValidationError "expected_snapshot_ref is required to start an edit session"
                Just expectedRef -> do
                    checkResult <- checkSnapshotMatch tracer filePath expectedRef
                    case checkResult of
                        Left err -> pure $ Left err
                        Right () -> do
                            fileExists <- doesFileExist filePath
                            existingContent <- if fileExists then Text.readFile filePath else pure ""
                            let baseLines = Text.lines existingContent
                                hasTrailingNewline = not (Text.null existingContent) && Text.last existingContent == '\n'
                            now <- getCurrentTime
                            stateVar <-
                                newTVarIO
                                    EditSessionState
                                        { sessCurrentLines = baseLines
                                        , sessEditLog = []
                                        , sessLastTouched = now
                                        }
                            rawUuid <- UUID.nextRandom
                            let sid = SessionId (UUID.toText rawUuid)
                                session =
                                    EditSession
                                        { editSessionFilePath = filePath
                                        , editSessionBaseSnapshotRef = expectedRef
                                        , editSessionBaseLineCount = length baseLines
                                        , editSessionHasTrailingNewline = hasTrailingNewline
                                        , editSessionState = stateVar
                                        }
                            atomically $ modifyTVar' store (Map.insert sid session)
                            runTracer tracer (EditSessionStartedTrace filePath sid)
                            applyAndMaybeCommit tracer store toolbox sid session rangesTxt contentBlocks doCommit

-- | Look up a session by id, expiring and removing it if it has been idle too long.
lookupLiveSession :: EditSessionStore -> SessionId -> IO (Either DeveloperToolError EditSession)
lookupLiveSession store sid = do
    sessions <- readTVarIO store
    case Map.lookup sid sessions of
        Nothing -> pure $ Left $ SessionNotFoundError (unSessionId sid)
        Just session -> do
            now <- getCurrentTime
            st <- readTVarIO (editSessionState session)
            if diffUTCTime now (sessLastTouched st) > sessionExpirySeconds
                then do
                    atomically $ modifyTVar' store (Map.delete sid)
                    pure $ Left $ SessionExpiredError (unSessionId sid)
                else pure $ Right session

{- | Apply any edits supplied in this call to a session's in-memory state, and
either stage them (commit=False) or write them to disk and destroy the
session (commit=True).
-}
applyAndMaybeCommit ::
    Tracer IO Trace ->
    EditSessionStore ->
    Toolbox ->
    SessionId ->
    EditSession ->
    Text ->
    [Text] ->
    Bool ->
    IO (Either DeveloperToolError WriteFileRangeResult)
applyAndMaybeCommit tracer store toolbox sid session rangesTxt contentBlocks doCommit =
    case parseSessionRanges rangesTxt of
        Left err -> pure $ Left err
        Right ranges ->
            case validateSessionContentBlocks ranges contentBlocks of
                Left err -> pure $ Left err
                Right pairs ->
                    if doCommit
                        then do
                            -- Re-verify the file on disk hasn't changed since the session
                            -- started, before applying this turn's edit and writing.
                            recheck <- checkSnapshotMatch tracer (editSessionFilePath session) (editSessionBaseSnapshotRef session)
                            case recheck of
                                Left err -> pure $ Left err
                                Right () -> applyThenWrite tracer store toolbox sid session pairs
                        else applyThenStage tracer sid session pairs

{- | Parse ranges for a session call. An empty/blank ranges string means "no new
edit this call" (useful to just commit a session's previously staged edits).
'whole' is rejected since it would make the original coordinate space
meaningless for any edits still pending in the session.
-}
parseSessionRanges :: Text -> Either DeveloperToolError [RangeSpec]
parseSessionRanges rangesTxt
    | Text.null (Text.strip rangesTxt) = Right []
    | otherwise = do
        ranges <- parseRanges rangesTxt
        if Whole `elem` ranges
            then Left $ InvalidRangeError "'whole' is not supported within an edit session; commit the session and use write-file-range normally instead."
            else Right ranges

-- | Pair ranges with content blocks for a session call, with no implicit defaulting.
validateSessionContentBlocks :: [RangeSpec] -> [Text] -> Either DeveloperToolError [(RangeSpec, Text)]
validateSessionContentBlocks [] _ = Right []
validateSessionContentBlocks ranges contentBlocks
    | length contentBlocks /= length ranges =
        Left $
            InvalidRangeError $
                "Number of content blocks ("
                    <> Text.pack (show (length contentBlocks))
                    <> ") must match number of ranges ("
                    <> Text.pack (show (length ranges))
                    <> ")"
    | otherwise = Right $ zip ranges contentBlocks

-- | Stage edits in memory, without writing to disk.
applyThenStage ::
    Tracer IO Trace ->
    SessionId ->
    EditSession ->
    [(RangeSpec, Text)] ->
    IO (Either DeveloperToolError WriteFileRangeResult)
applyThenStage tracer sid session pairs = do
    st <- readTVarIO (editSessionState session)
    case foldSessionEdits (editSessionBaseLineCount session) (sessCurrentLines st, sessEditLog st, []) pairs of
        Left err -> pure $ Left err
        Right (finalLines, finalLog, rangeResults) -> do
            now <- getCurrentTime
            atomically $
                writeTVar
                    (editSessionState session)
                    EditSessionState
                        { sessCurrentLines = finalLines
                        , sessEditLog = finalLog
                        , sessLastTouched = now
                        }
            runTracer tracer (EditSessionContinuedTrace sid (length pairs))
            let netDelta = length finalLines - editSessionBaseLineCount session
            pure $
                Right $
                    WriteFileRangeResult
                        { writeFilePath = editSessionFilePath session
                        , writeFileRangesModified = length pairs
                        , writeFileLinesWritten = sum (map rangeEditLinesWritten rangeResults)
                        , writeFileFinalLineCount = length finalLines
                        , writeFileRangeResults = rangeResults
                        , writeFileBeforeSnapshotRef = Nothing
                        , writeFileAfterSnapshotRef = Nothing
                        , writeFileSessionId = Just (unSessionId sid)
                        , writeFileSessionNetDelta = Just netDelta
                        , writeFileSessionCommitted = False
                        }

-- | Apply edits and write the resulting content to disk, destroying the session.
applyThenWrite ::
    Tracer IO Trace ->
    EditSessionStore ->
    Toolbox ->
    SessionId ->
    EditSession ->
    [(RangeSpec, Text)] ->
    IO (Either DeveloperToolError WriteFileRangeResult)
applyThenWrite tracer store toolbox sid session pairs =
    withSandboxCheck tracer toolbox (editSessionFilePath session) $ do
        st <- readTVarIO (editSessionState session)
        case foldSessionEdits (editSessionBaseLineCount session) (sessCurrentLines st, sessEditLog st, []) pairs of
            Left err -> pure $ Left err
            Right (finalLines, _finalLog, rangeResults) -> do
                let filePath = editSessionFilePath session
                    output = Text.unlines finalLines
                writeResult <- try $ do
                    createDirectoryIfMissing True (takeDirectory filePath)
                    writeFileAtomic filePath output
                case writeResult of
                    Left (e :: SomeException) -> do
                        let err = PermissionError $ Text.pack $ show e
                        runTracer tracer (DeveloperToolErrorTrace "write-file-range" $ Text.pack $ show e)
                        pure $ Left err
                    Right () -> do
                        writtenContent <- BS.readFile filePath
                        let newSnapshot = makeSnapshot writtenContent
                            newRef = snapshotRef newSnapshot
                        case toolboxSnapshotStore toolbox of
                            Just snapStore -> atomically $ modifyTVar' snapStore (Map.insert newRef newSnapshot)
                            Nothing -> pure ()
                        atomically $ modifyTVar' store (Map.delete sid)
                        runTracer tracer (EditSessionCommittedTrace sid filePath)
                        let netDelta = length finalLines - editSessionBaseLineCount session
                        pure $
                            Right $
                                WriteFileRangeResult
                                    { writeFilePath = filePath
                                    , writeFileRangesModified = length pairs
                                    , writeFileLinesWritten = sum (map rangeEditLinesWritten rangeResults)
                                    , writeFileFinalLineCount = length finalLines
                                    , writeFileRangeResults = rangeResults
                                    , writeFileBeforeSnapshotRef = Just (editSessionBaseSnapshotRef session)
                                    , writeFileAfterSnapshotRef = Just newRef
                                    , writeFileSessionId = Just (unSessionId sid)
                                    , writeFileSessionNetDelta = Just netDelta
                                    , writeFileSessionCommitted = True
                                    }

{- | Fold a sequence of (range, content) edits over a session's accumulated
state. Ranges must be given in the session's ORIGINAL coordinates; each one is
mapped onto the current in-memory content using the offset accumulated from
all previously applied edits (this call's and earlier calls'). Overlapping
edits (in original coordinates) are rejected, since the original line numbers
inside an already-edited region no longer correspond to anything coherent.
-}
foldSessionEdits ::
    -- | Original line count of the session's base snapshot, for bounds checks
    Int ->
    ([Text], [SessionEditRecord], [RangeEditResult]) ->
    [(RangeSpec, Text)] ->
    Either DeveloperToolError ([Text], [SessionEditRecord], [RangeEditResult])
foldSessionEdits baseLineCount acc0 pairs = go acc0 (sortOn rangeStartKey pairs)
  where
    go acc [] = Right acc
    go acc (p : ps) = applySessionRangeEdit baseLineCount acc p >>= \acc' -> go acc' ps

-- | The cumulative offset that applies at a given original line position.
offsetForPosition :: [SessionEditRecord] -> Int -> Int
offsetForPosition log_ pos = sum [sessionEditDelta e | e <- log_, sessionEditOriginalEnd e < pos]

-- | Does a (start, end) original range overlap any previously recorded edit?
overlapsLog :: [SessionEditRecord] -> Int -> Int -> Bool
overlapsLog log_ start end = any (\e -> not (end < sessionEditOriginalStart e || start > sessionEditOriginalEnd e)) log_

-- | Apply a single session edit, in original coordinates, against accumulated state.
applySessionRangeEdit ::
    Int ->
    ([Text], [SessionEditRecord], [RangeEditResult]) ->
    (RangeSpec, Text) ->
    Either DeveloperToolError ([Text], [SessionEditRecord], [RangeEditResult])
applySessionRangeEdit baseLineCount (currentLines, log_, results) (range, content) =
    let newLines = Text.lines content
        numNewLines = length newLines
     in case range of
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
                 in Right (newLines ++ currentLines, log_, results ++ [result])
            Tail ->
                let finalStart = length currentLines + 1
                    result =
                        RangeEditResult
                            { rangeEditSpec = "tail"
                            , rangeEditOriginalStart = baseLineCount + 1
                            , rangeEditOriginalEnd = baseLineCount
                            , rangeEditLinesWritten = numNewLines
                            , rangeEditFinalStartLine = if numNewLines == 0 then Nothing else Just finalStart
                            , rangeEditFinalEndLine = if numNewLines == 0 then Nothing else Just (finalStart + numNewLines - 1)
                            , rangeEditOperation = "append"
                            }
                 in Right (currentLines ++ newLines, log_, results ++ [result])
            Whole ->
                -- Rejected earlier by parseSessionRanges; kept exhaustive for safety.
                Left $ InvalidRangeError "'whole' is not supported within an edit session."
            After origLine ->
                if overlapsLog log_ origLine origLine
                    then Left $ RangeOverlapError $ "Insert-after position " <> Text.pack (show origLine) <> " falls inside a range already edited earlier in this session."
                    else
                        let offset = offsetForPosition log_ origLine
                            insertPos = max 0 (min (origLine + offset) (length currentLines))
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
                            newLog = log_ ++ [SessionEditRecord origLine origLine numNewLines]
                         in Right (before ++ newLines ++ after, newLog, results ++ [result])
            Lines (origStart, origEnd) ->
                if origStart < 1 || origEnd > baseLineCount
                    then Left $ RangeOutOfBoundsError $ "Range " <> Text.pack (show origStart) <> "-" <> Text.pack (show origEnd) <> " is outside the session's original file bounds (1-" <> Text.pack (show baseLineCount) <> ")"
                    else
                        if overlapsLog log_ origStart origEnd
                            then Left $ RangeOverlapError $ "Range " <> Text.pack (show origStart) <> "-" <> Text.pack (show origEnd) <> " overlaps a range already edited earlier in this session."
                            else
                                let offset = offsetForPosition log_ origStart
                                    adjustedStart = origStart + offset
                                    adjustedEnd = origEnd + offset
                                    before = take (adjustedStart - 1) currentLines
                                    after = drop adjustedEnd currentLines
                                    delta = numNewLines - (origEnd - origStart + 1)
                                    finalStart = adjustedStart
                                    finalEnd = adjustedStart + numNewLines - 1
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
                                    newLog = log_ ++ [SessionEditRecord origStart origEnd delta]
                                 in Right (before ++ newLines ++ after, newLog, results ++ [result])
