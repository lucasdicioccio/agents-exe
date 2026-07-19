{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Patch file operations for the DeveloperToolbox.

This module provides functionality for applying unified diff patches to files
atomically with context validation.

The patch format follows standard unified diff conventions:
* @@ -oldStart,oldCount +newStart,newCount @@ header
* Context lines (no prefix)
* Removed lines (- prefix)
* Added lines (+ prefix)

Optimistic locking is supported via optional expectedSnapshotRef parameter.
When provided, the operation verifies the current file content matches before
making any changes, preventing concurrent edit conflicts.
-}
module System.Agents.Tools.DeveloperToolbox.Patch (
    -- * Patch execution
    executePatchFile,

    -- * Patch parsing and validation
    parseUnifiedDiff,
    validateHunk,
    validateAllHunks,
    applyHunk,
) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist)

import Prod.Tracer (Tracer (..))

import System.Agents.FileSandbox (AccessResult (..), validateFileWrite)
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolCapability (..),
    DeveloperToolError (..),
    Hunk (..),
    HunkSegment (..),
    PatchError (..),
    PatchResult (..),
    Snapshot (..),
    SnapshotRef (..),
    Toolbox (..),
    Trace (..),
    makeSnapshot,
 )

-------------------------------------------------------------------------------
-- Patch File Operations
-------------------------------------------------------------------------------

{- | Execute patch file operation.

Applies a unified diff patch to a file atomically. All hunks are validated
before any changes are applied, ensuring no partial corruption.

Optimistic Locking:
When expectedSnapshotRef is provided, the operation first reads the current file
content and computes its MD5 hash. If the hash doesn't match expectedSnapshotRef,
a SnapshotMismatchError is returned without making any changes. This prevents
lost updates when multiple agents/processes edit the same file.

When a file sandbox is configured, the filepath is validated against
the sandbox before patching. Access is denied if the file is outside
the allowed paths.

The patch format follows standard unified diff conventions:
* @@ -oldStart,oldCount +newStart,newCount @@ header
* Context lines have no prefix
* Removed lines (- prefix)
* Added lines (+ prefix)

Parameters:
- path: Path to the file to patch
- patch: Unified diff patch content
- expectedSnapshotRef: Optional MD5 hash of expected file content. If provided,
                       the operation fails with SnapshotMismatchError if the
                       current content doesn't match. Use this for optimistic locking.

Returns Right with PatchResult on success, Left with error on failure.

Example patch:
@
--- a/src/File.hs
+++ b/src/File.hs
@@ -10,5 +10,6 @@ func1 x =
   line1
   line2
-  oldLine
+  newLine1
+  newLine2
   line4
   line5
@
-}
executePatchFile ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    -- | Optional expected snapshot reference for optimistic locking
    Maybe SnapshotRef ->
    IO (Either DeveloperToolError PatchResult)
executePatchFile tracer toolbox filePath patchText mExpectedSnapshotRef = do
    if DevToolPatchFile `notElem` toolboxCapabilities toolbox
        then pure $ Left $ CapabilityNotEnabledError "patch-file"
        else do
            runTracer tracer (PatchFileStartedTrace filePath 0)

            -- Validate against file sandbox if configured
            case toolboxFileSandbox toolbox of
                Just sandbox -> do
                    accessResult <- validateFileWrite sandbox filePath
                    case accessResult of
                        AccessDenied err -> do
                            let errMsg = FileAccessDeniedError filePath (Text.pack $ show err)
                            runTracer tracer (PatchFileErrorTrace (Text.pack filePath) $ Text.pack $ show errMsg)
                            pure $ Left errMsg
                        AccessGranted -> proceedWithPatch tracer toolbox filePath patchText mExpectedSnapshotRef
                Nothing ->
                    -- No sandbox configured, proceed (backwards compatible behavior)
                    proceedWithPatch tracer toolbox filePath patchText mExpectedSnapshotRef

-- | Proceed with patch operation after validation.
proceedWithPatch ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    Maybe SnapshotRef ->
    IO (Either DeveloperToolError PatchResult)
proceedWithPatch tracer toolbox filePath patchText mExpectedSnapshotRef = do
    -- Check if file exists
    fileExists <- doesFileExist filePath
    if not fileExists
        then do
            let err = PatchFileNotFound filePath
            runTracer tracer (PatchFileErrorTrace (Text.pack filePath) $ Text.pack $ show err)
            pure $ Left $ PatchValidationError err
        else do
            -- Handle optimistic locking if expectedSnapshotRef is provided
            case mExpectedSnapshotRef of
                Just expectedRef -> do
                    checkResult <- checkSnapshotMatch tracer filePath expectedRef
                    case checkResult of
                        Left err -> pure $ Left err
                        Right () -> proceedWithPatchApplication tracer toolbox filePath patchText
                Nothing -> proceedWithPatchApplication tracer toolbox filePath patchText

-- | Check if current file content matches expected snapshot (optimistic locking).
checkSnapshotMatch ::
    Tracer IO Trace ->
    FilePath ->
    SnapshotRef ->
    IO (Either DeveloperToolError ())
checkSnapshotMatch tracer filePath expectedRef = do
    content <- BS.readFile filePath
    let actualSnapshot = makeSnapshot content
    let actualRef = snapshotRef actualSnapshot
    if actualRef == expectedRef
        then pure $ Right ()
        else do
            let err = SnapshotMismatchError expectedRef actualRef
            runTracer tracer (SnapshotMismatchTrace filePath expectedRef actualRef)
            pure $ Left err

-- | Proceed with actual patch application after all validations pass.
proceedWithPatchApplication ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    IO (Either DeveloperToolError PatchResult)
proceedWithPatchApplication tracer toolbox filePath patchText = do
    -- Parse the unified diff
    case parseUnifiedDiff patchText of
        Left err -> do
            runTracer tracer (PatchFileErrorTrace (Text.pack filePath) $ Text.pack $ show err)
            pure $ Left $ PatchValidationError err
        Right hunks -> do
            -- Read current file content
            content <- Text.readFile filePath
            let lines' = Text.lines content

            -- Validate ALL hunks before applying any
            case validateAllHunks lines' hunks of
                Left err -> do
                    runTracer tracer (PatchFileErrorTrace (Text.pack filePath) $ Text.pack $ show err)
                    pure $ Left $ PatchValidationError err
                Right validHunks -> do
                    -- Take snapshot before patch if snapshot enabled
                    mBeforeSnapshotRef <- takeSnapshotBeforePatch tracer toolbox filePath

                    -- Apply hunks bottom-to-top (descending line order)
                    -- to avoid line number shifts affecting other hunks
                    let sortedHunks = sortOn (negate . hunkOldStart) validHunks
                    let newLines = foldl (flip applyHunk) lines' sortedHunks

                    -- Count changes
                    let linesChanged = sum $ map hunkChangeCount validHunks

                    -- Write result
                    writeResult <- try $ do
                        Text.writeFile filePath (Text.unlines newLines)

                    case writeResult of
                        Left (e :: SomeException) -> do
                            let err = PermissionError $ Text.pack $ show e
                            runTracer tracer (DeveloperToolErrorTrace "patch-file" $ Text.pack $ show e)
                            pure $ Left err
                        Right () -> do
                            let applied = length validHunks

                            -- Take after snapshot if snapshot enabled
                            mAfterSnapshotRef <- takeSnapshotAfterPatch tracer toolbox filePath

                            runTracer tracer (PatchFileCompletedTrace filePath applied 0)

                            pure $
                                Right $
                                    PatchResult
                                        { patchFilePath = filePath
                                        , patchHunksApplied = applied
                                        , patchHunksRejected = 0
                                        , patchLinesChanged = linesChanged
                                        , patchBeforeSnapshotRef = mBeforeSnapshotRef
                                        , patchAfterSnapshotRef = mAfterSnapshotRef
                                        }
  where
    hunkChangeCount hunk =
        sum
            [ length removedLines + length addedLines
            | HunkChange removedLines addedLines <- hunkSegments hunk
            ]

{- | Take a snapshot of the file before patching. Snapshots are always taken
(the snapshot store is always present on the toolbox); returns Nothing only
if the file doesn't exist yet.
-}
takeSnapshotBeforePatch :: Tracer IO Trace -> Toolbox -> FilePath -> IO (Maybe SnapshotRef)
takeSnapshotBeforePatch tracer toolbox filePath =
    case toolboxSnapshotStore toolbox of
        Just store -> do
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
        Nothing -> pure Nothing

-- | Take a snapshot of the file after patching. Snapshots are always taken.
takeSnapshotAfterPatch :: Tracer IO Trace -> Toolbox -> FilePath -> IO (Maybe SnapshotRef)
takeSnapshotAfterPatch _tracer toolbox filePath =
    case toolboxSnapshotStore toolbox of
        Just store -> do
            -- Read file content and create snapshot
            content <- BS.readFile filePath
            let snapshot = makeSnapshot content
            let ref = snapshotRef snapshot
            -- Store snapshot in the store
            atomically $ modifyTVar' store (Map.insert ref snapshot)
            pure $ Just ref
        Nothing -> pure Nothing

-- | Validate all hunks, returning the first error or the list of valid hunks.
validateAllHunks :: [Text] -> [Hunk] -> Either PatchError [Hunk]
validateAllHunks fileLines hunks = do
    -- First, check for overlapping hunks
    case findOverlappingHunks hunks of
        Just (h1, h2) -> Left $ PatchHunkOverlap (hunkOldStart h1) (hunkOldStart h2)
        Nothing -> do
            -- Then validate each hunk individually
            mapM (validateHunk fileLines) hunks

-- | Find any overlapping hunks in the list.
findOverlappingHunks :: [Hunk] -> Maybe (Hunk, Hunk)
findOverlappingHunks [] = Nothing
findOverlappingHunks [_] = Nothing
findOverlappingHunks (h : hs) =
    let hEnd = hunkOldStart h + hunkOldCount h
     in case find (\h' -> hunkOldStart h' < hEnd && hunkOldStart h' >= hunkOldStart h) hs of
            Just overlap -> Just (h, overlap)
            Nothing -> findOverlappingHunks hs

{- | Validate a single hunk against the file content.

Walks the hunk's segments in order starting at 'hunkOldStart', checking that
each context segment and each removed-lines segment matches the
corresponding lines in the file exactly. This correctly handles hunks that
contain more than one change separated by context lines.

When validation fails, the error includes both the expected and actual lines
so the caller can diagnose the mismatch without extra round trips.
-}
validateHunk :: [Text] -> Hunk -> Either PatchError Hunk
validateHunk fileLines hunk = do
    -- Validate line numbers are in range
    let totalLines = length fileLines
    when (hunkOldStart hunk < 1) $ Left $ PatchInvalidLineNumber (hunkOldStart hunk)
    when (hunkOldStart hunk > totalLines && hunkOldCount hunk > 0) $
        Left $
            PatchInvalidLineNumber (hunkOldStart hunk)

    walkSegments (hunkOldStart hunk - 1) (hunkSegments hunk)
    Right hunk
  where
    walkSegments :: Int -> [HunkSegment] -> Either PatchError ()
    walkSegments _ [] = Right ()
    walkSegments idx (seg : rest) =
        case seg of
            HunkContext ctx ->
                let expected = ctx
                    actual = take (length ctx) $ drop idx fileLines
                 in if actual == expected
                        then walkSegments (idx + length ctx) rest
                        else
                            Left $
                                PatchContextMismatch
                                    { patchMismatchLine = idx + 1
                                    , patchMismatchMessage = "Context doesn't match"
                                    , patchMismatchExpected = expected
                                    , patchMismatchActual = actual
                                    }
            HunkChange removedLines _addedLines ->
                let expected = removedLines
                    actual = take (length removedLines) $ drop idx fileLines
                 in if actual == expected
                        then walkSegments (idx + length removedLines) rest
                        else
                            Left $
                                PatchContextMismatch
                                    { patchMismatchLine = idx + 1
                                    , patchMismatchMessage = "Removed lines don't match"
                                    , patchMismatchExpected = expected
                                    , patchMismatchActual = actual
                                    }

{- | Apply a hunk to the file lines.
Assumes the hunk has been validated and lines are 1-based.
-}
applyHunk :: Hunk -> [Text] -> [Text]
applyHunk hunk lines' =
    let startIdx = hunkOldStart hunk - 1
        before = take startIdx lines'
        (newMiddle, consumedOld) = buildMiddle (hunkSegments hunk)
        after = drop (startIdx + consumedOld) lines'
     in before ++ newMiddle ++ after
  where
    buildMiddle :: [HunkSegment] -> ([Text], Int)
    buildMiddle [] = ([], 0)
    buildMiddle (seg : rest) =
        let (restContent, restConsumed) = buildMiddle rest
         in case seg of
                HunkContext ctx -> (ctx ++ restContent, length ctx + restConsumed)
                HunkChange removedLines addedLines ->
                    (addedLines ++ restContent, length removedLines + restConsumed)

-------------------------------------------------------------------------------
-- Patch Parsing
-------------------------------------------------------------------------------

{- | Parse a unified diff into a list of hunks.

The parser handles standard unified diff format:
* File headers (--- and +++ lines) are ignored
* Hunk headers start with @@
* Context lines have no prefix
* Removed lines (- prefix)
* Added lines (+ prefix)

Returns Left with parse error or Right with list of hunks.
-}
parseUnifiedDiff :: Text -> Either PatchError [Hunk]
parseUnifiedDiff patchText =
    let allLines = Text.lines patchText
        -- Skip header lines (---, +++, empty lines at start)
        nonHeaderLines = dropWhile isHeaderLine allLines
     in parseHunks nonHeaderLines
  where
    isHeaderLine line =
        Text.isPrefixOf "---" line
            || Text.isPrefixOf "+++" line
            || Text.null (Text.strip line)

    parseHunks [] = Right []
    parseHunks lines' =
        case parseHunk lines' of
            Left err -> Left err
            Right (hunk, rest) ->
                case parseHunks rest of
                    Left err -> Left err
                    Right hunks -> Right (hunk : hunks)

{- | Parse a single hunk from the diff lines.
Returns the parsed hunk and the remaining lines.
-}
parseHunk :: [Text] -> Either PatchError (Hunk, [Text])
parseHunk [] = Left $ PatchParseError "Unexpected end of patch, expected hunk header"
parseHunk (line : rest)
    | Text.isPrefixOf "@@" line = do
        -- Parse hunk header: @@ -start,count +start,count @@
        (oldStart, oldCount, newStart, newCount) <- parseHunkHeader line
        -- Parse the hunk body
        (segments, remaining) <- parseHunkBody rest
        let hunk =
                Hunk
                    { hunkOldStart = oldStart
                    , hunkOldCount = oldCount
                    , hunkNewStart = newStart
                    , hunkNewCount = newCount
                    , hunkSegments = segments
                    }
        Right (hunk, remaining)
    | otherwise =
        -- Skip non-hunk lines (could be file names, etc.)
        parseHunk rest

{- | Parse hunk header line.
Format: @@ -oldStart,oldCount +newStart,newCount @@
Or: @@ -oldStart +newStart @@ (when count is 1)
The closing @@ may be followed by optional trailing context (e.g. a
function name, as produced by @git diff@), which is ignored.
-}
parseHunkHeader :: Text -> Either PatchError (Int, Int, Int, Int)
parseHunkHeader line =
    let stripped = Text.strip line
     in case Text.stripPrefix "@@ " stripped of
            Nothing -> Left $ PatchParseError $ "Invalid hunk header: " <> line
            Just afterAt ->
                case Text.breakOn " @@" afterAt of
                    (content, rest)
                        | Text.null rest -> Left $ PatchParseError $ "Invalid hunk header (missing @@ suffix): " <> line
                        | otherwise -> parseHeaderContent content
  where
    parseHeaderContent content =
        let parts = Text.words content
         in case parts of
                (oldPart : newPart : _) -> do
                    (oldStart, oldCount) <- parseHunkRangePart oldPart '-'
                    (newStart, newCount) <- parseHunkRangePart newPart '+'
                    Right (oldStart, oldCount, newStart, newCount)
                _ -> Left $ PatchParseError $ "Invalid hunk header content: " <> content

    parseHunkRangePart part prefix =
        case Text.stripPrefix (Text.singleton prefix) part of
            Nothing -> Left $ PatchParseError $ "Expected " <> Text.singleton prefix <> " prefix in: " <> part
            Just rangeStr ->
                let rangeParts = Text.splitOn "," rangeStr
                 in case rangeParts of
                        [start] -> case readInt start of
                            Just s -> Right (s, 1)
                            Nothing -> Left $ PatchParseError $ "Invalid line number: " <> start
                        [start, count] -> case (readInt start, readInt count) of
                            (Just s, Just c) -> Right (s, c)
                            _ -> Left $ PatchParseError $ "Invalid range: " <> rangeStr
                        _ -> Left $ PatchParseError $ "Invalid range format: " <> rangeStr

    readInt txt =
        let str = Text.unpack txt
         in if all isDigit str && not (null str)
                then Just (read str :: Int)
                else Nothing

{- | Parse the body of a hunk into an ordered list of segments.

A hunk body is a sequence of context lines and change blocks (a run of
removed lines followed by a run of added lines). Unlike a flat
before\/removed\/added\/after model, this preserves every context/change
boundary in order, so a hunk with several changes separated by context
(as produced by real diff tools) is represented faithfully instead of
collapsing everything after the first change into "context after" and
losing the rest.

Each emitted line has its single-character diff prefix (@-@, @+@, or the
leading space marking a context line) stripped before being stored, so the
stored text matches the file's actual content exactly.
-}
parseHunkBody :: [Text] -> Either PatchError ([HunkSegment], [Text])
parseHunkBody = go [] [] [] []
  where
    -- segsAcc: completed segments, most recent first
    -- ctxAcc: pending context lines, most recent first
    -- removedAcc/addedAcc: pending change block, most recent first
    -- Invariant: at most one of ctxAcc and (removedAcc/addedAcc) is non-empty.
    go :: [HunkSegment] -> [Text] -> [Text] -> [Text] -> [Text] -> Either PatchError ([HunkSegment], [Text])
    go segsAcc ctxAcc removedAcc addedAcc lines' =
        case lines' of
            [] -> Right (finalize segsAcc ctxAcc removedAcc addedAcc, [])
            (line : rest)
                | Text.isPrefixOf "@@" line ->
                    Right (finalize segsAcc ctxAcc removedAcc addedAcc, lines')
                | isRemovedLine line ->
                    let content = Text.drop 1 line
                        segsAcc1 = flushContext segsAcc ctxAcc
                     in if not (null addedAcc)
                            -- A new change block starts right after a previous one with no context between.
                            then go (flushChange segsAcc1 removedAcc addedAcc) [] [content] [] rest
                            else go segsAcc1 [] (content : removedAcc) addedAcc rest
                | isAddedLine line ->
                    let content = Text.drop 1 line
                        segsAcc1 = flushContext segsAcc ctxAcc
                     in go segsAcc1 [] removedAcc (content : addedAcc) rest
                | otherwise ->
                    let content = contextContent line
                        segsAcc1 = flushChange segsAcc removedAcc addedAcc
                     in go segsAcc1 (content : ctxAcc) [] [] rest

    isRemovedLine line = Text.isPrefixOf "-" line && not (Text.isPrefixOf "---" line)
    isAddedLine line = Text.isPrefixOf "+" line && not (Text.isPrefixOf "+++" line)

    -- Context lines are prefixed with a single space in standard unified
    -- diff output; strip it so the stored text matches file content.
    contextContent line
        | Text.null line = ""
        | Text.isPrefixOf " " line = Text.drop 1 line
        | otherwise = line

    flushContext segsAcc [] = segsAcc
    flushContext segsAcc ctxAcc = HunkContext (reverse ctxAcc) : segsAcc

    flushChange segsAcc [] [] = segsAcc
    flushChange segsAcc removedAcc addedAcc = HunkChange (reverse removedAcc) (reverse addedAcc) : segsAcc

    finalize segsAcc ctxAcc removedAcc addedAcc =
        reverse (flushChange (flushContext segsAcc ctxAcc) removedAcc addedAcc)

