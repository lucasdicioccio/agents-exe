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
    executeReadFileRangeWith,
    extractLines,
    extractRange,
    formatLineWithNumber,

    -- * Whole-file read cap
    WholeReadCap (..),
    defaultWholeReadMaxLines,
    defaultWholeReadMaxBytes,
    toolboxWholeReadCap,
    capLines,
    truncationHint,
) where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (doesFileExist)

import Prod.Tracer (Tracer (..))

import System.Agents.Base (DeveloperToolboxDescription (..))
import System.Agents.FileSandbox (AccessResult (..), validateFileRead)
import System.Agents.Tools.DeveloperToolbox.Range (parseRanges)
import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolCapability (..),
    DeveloperToolError (..),
    EditSession (..),
    EditSessionState (..),
    EditSessionStore (..),
    RangeSpec (..),
    ReadFileRangeResult (..),
    SessionId (..),
    Snapshot (..),
    SnapshotRef (..),
    Toolbox (..),
    Trace (..),
    makeSnapshot,
 )

{- | Execute read file range operation.

Reads specific line ranges from a file and returns them with line numbers
prepended as "{line_num}\t{line_content}".

When a file sandbox is configured, the filepath is validated against
the sandbox before reading. Access is denied if the file is outside
the allowed paths.

When a snapshot store is configured, the file content is stored as a snapshot
and the snapshot reference is returned. This enables optimistic locking for
subsequent write operations.

Parameters:
- path: Path to the file to read
- ranges: Comma-separated line ranges (e.g., "1-10", "5", "head", "tail", "whole").
          If omitted or empty, defaults to reading the entire file (equivalent to "whole").
          A whole-file read is capped (see 'WholeReadCap'): past the cap it
          returns the first lines, @truncated: true@ and a hint naming the
          range to read next. Explicit line ranges are never capped.

Returns Right with ReadFileRangeResult on success, Left with error on failure.

The result includes metadata fields to help distinguish between different scenarios:
- totalFileSize: Total file size in bytes
- totalLineCount: Total number of lines in the file
- rangesParsed: The normalized range specifications that were applied
- snapshotRef: Reference to the snapshot of the file content (if snapshot store available)
-}
executeReadFileRange ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    IO (Either DeveloperToolError ReadFileRangeResult)
executeReadFileRange tracer toolbox filePath rangesTxt =
    executeReadFileRangeWith tracer toolbox filePath rangesTxt Nothing False

{- | Extended read-file-range that can also:

* Read from an in-progress write-file-range session's staged buffer when
  'sessionId' is provided. This lets the caller verify pending edits without
  committing them.
* Return only metadata (path, line counts, snapshot ref) when
  'metadataOnly' is true, avoiding the cost of serialising content.
-}
executeReadFileRangeWith ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    Maybe Text ->
    Bool ->
    IO (Either DeveloperToolError ReadFileRangeResult)
executeReadFileRangeWith tracer toolbox filePath rangesTxt mSessionIdTxt metadataOnly = do
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
                        AccessGranted -> proceedWithRead tracer toolbox filePath rangesTxt mSessionIdTxt metadataOnly
                Nothing ->
                    -- No sandbox configured, proceed (backwards compatible behavior)
                    proceedWithRead tracer toolbox filePath rangesTxt mSessionIdTxt metadataOnly

-- | Proceed with file read after validation.
proceedWithRead ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    Maybe Text ->
    Bool ->
    IO (Either DeveloperToolError ReadFileRangeResult)
proceedWithRead tracer toolbox filePath rangesTxt mSessionIdTxt metadataOnly = do
    -- If a session id is provided, try to read from the session's staged buffer.
    case mSessionIdTxt of
        Just sidTxt -> do
            let sid = SessionId sidTxt
            case toolboxEditSessionStore toolbox of
                Nothing -> pure $ Left $ CapabilityNotEnabledError "read-file-range"
                Just store -> do
                    mSession <- lookupSession store sid
                    case mSession of
                        Left err -> pure $ Left err
                        Right session -> do
                            st <- readTVarIO (editSessionState session)
                            let allLines = sessCurrentLines st
                            buildReadResult tracer toolbox (editSessionFilePath session) rangesTxt allLines (Just sidTxt) metadataOnly
        Nothing -> do
            -- Check if file exists
            fileExists <- doesFileExist filePath
            if not fileExists
                then do
                    let err = FileNotFoundError filePath
                    runTracer tracer (DeveloperToolErrorTrace "read-file-range" $ Text.pack $ show err)
                    pure $ Left err
                else do
                    result <- try $ BS.readFile filePath
                    case result of
                        Left (e :: SomeException) -> do
                            let err = PermissionError $ Text.pack $ show e
                            runTracer tracer (DeveloperToolErrorTrace "read-file-range" $ Text.pack $ show err)
                            pure $ Left err
                        Right contentBytes -> do
                            let content = Text.decodeUtf8 contentBytes
                                allLines = Text.lines content
                            buildReadResult tracer toolbox filePath rangesTxt allLines Nothing metadataOnly

-- | Look up an active session without expiring it (reads are non-mutating).
lookupSession :: EditSessionStore -> SessionId -> IO (Either DeveloperToolError EditSession)
lookupSession store sid = do
    active <- readTVarIO (editSessionStoreActive store)
    case Map.lookup sid active of
        Just session -> pure $ Right session
        Nothing -> do
            committed <- readTVarIO (editSessionStoreCommitted store)
            if Map.member sid committed
                then pure $ Left $ SessionAlreadyCommittedError (unSessionId sid)
                else pure $ Left $ SessionNotFoundError (unSessionId sid)

-- | Build the read result from a list of lines and configuration.
buildReadResult ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    Text ->
    [Text] ->
    Maybe Text ->
    Bool ->
    IO (Either DeveloperToolError ReadFileRangeResult)
buildReadResult tracer toolbox filePath rangesTxt allLines mSessionId metadataOnly =
    case parseRanges rangesTxt of
        Left err -> pure $ Left err
        Right ranges -> do
            let totalLineCount = length allLines
                totalFileSize = sum (map (BS.length . Text.encodeUtf8) allLines)
                    + if totalLineCount > 0 then totalLineCount - 1 else 0
                extracted = extractLines allLines ranges
                -- Only a whole-file read is capped; explicit ranges are the
                -- way to page, so they are returned as asked.
                cap = toolboxWholeReadCap toolbox
                (resultLines, truncated)
                    | metadataOnly = (extracted, False)
                    | Whole `elem` ranges = capLines cap extracted
                    | otherwise = (extracted, False)
                output
                    | metadataOnly = ""
                    | otherwise = Text.unlines $ map formatLineWithNumber resultLines
                linesRead = if metadataOnly then 0 else length resultLines
                rangesParsed = map rangeSpecToText ranges
                hint
                    | truncated = Just (truncationHint cap totalLineCount (map fst resultLines))
                    | otherwise = Nothing

            -- Snapshot is taken from the raw bytes of the lines we just read.
            mSnapshotRef <- takeSnapshot tracer toolbox filePath allLines

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
                        , readFileSnapshotRef = mSnapshotRef
                        , readFileSessionId = mSessionId
                        , readFileMetadataOnly = metadataOnly
                        , readFileTruncated = truncated
                        , readFileHint = hint
                        }

{- | Limits on what a whole-file read returns. A field of 'Nothing' means
that limit is off.
-}
data WholeReadCap = WholeReadCap
    { capMaxLines :: Maybe Int
    , capMaxBytes :: Maybe Int
    }
    deriving (Show, Eq)

-- | Lines a whole-file read returns when the toolbox does not say.
defaultWholeReadMaxLines :: Int
defaultWholeReadMaxLines = 400

-- | Bytes of content a whole-file read returns when the toolbox does not say.
defaultWholeReadMaxBytes :: Int
defaultWholeReadMaxBytes = 32768

{- | The toolbox's whole-file read cap: the configured values, the defaults
when absent, and no limit for a value of 0 (or less).
-}
toolboxWholeReadCap :: Toolbox -> WholeReadCap
toolboxWholeReadCap toolbox =
    WholeReadCap
        { capMaxLines = limit defaultWholeReadMaxLines cfg.developerToolboxWholeReadMaxLines
        , capMaxBytes = limit defaultWholeReadMaxBytes cfg.developerToolboxWholeReadMaxBytes
        }
  where
    cfg = toolboxConfig toolbox
    limit def Nothing = Just def
    limit _ (Just n)
        | n <= 0 = Nothing
        | otherwise = Just n

{- | Keep the leading lines that fit the cap. Each line counts its UTF-8
bytes plus one for its newline. Returns the kept lines and whether any
were dropped.
-}
capLines :: WholeReadCap -> [(Int, Text)] -> ([(Int, Text)], Bool)
capLines cap xs = go 0 0 xs []
  where
    go _ _ [] acc = (reverse acc, False)
    go n bytes (l@(_, txt) : rest) acc
        | maybe False (n >=) (capMaxLines cap) = (reverse acc, True)
        | maybe False (bytes' >) (capMaxBytes cap) = (reverse acc, True)
        | otherwise = go (n + 1) bytes' rest (l : acc)
      where
        bytes' = bytes + BS.length (Text.encodeUtf8 txt) + 1

{- | The hint returned with a truncated read: how big the file is, what was
shown, and the @ranges@ value that reads the next page.
-}
truncationHint :: WholeReadCap -> Int -> [Int] -> Text
truncationHint cap totalLines shown =
    "file has "
        <> tshow totalLines
        <> " lines; "
        <> shownTxt
        <> " (whole-file reads are capped at "
        <> capTxt
        <> "). Pass ranges='"
        <> tshow nextStart
        <> "-"
        <> tshow nextEnd
        <> "' to read more, or a narrower range around what you need; explicit ranges are not capped."
  where
    tshow :: Int -> Text
    tshow = Text.pack . show
    (firstShown, lastShown) = case shown of
        [] -> (0, 0)
        (x : _) -> (x, foldl (\_ y -> y) x shown)
    shownTxt
        | null shown = "no line fit the cap"
        | otherwise = "showing lines " <> tshow firstShown <> "-" <> tshow lastShown
    nextStart = lastShown + 1
    pageSize = max 1 (fromMaybe (length shown) (capMaxLines cap))
    nextEnd = min totalLines (nextStart + pageSize - 1)
    capTxt = Text.intercalate " / " $
        [tshow n <> " lines" | Just n <- [capMaxLines cap]]
            <> [tshow n <> " bytes" | Just n <- [capMaxBytes cap]]

-- | Create and store a snapshot of the file content.
-- Returns the snapshot reference if a snapshot store is configured.
takeSnapshot ::
    Tracer IO Trace ->
    Toolbox ->
    FilePath ->
    [Text] ->
    IO (Maybe SnapshotRef)
takeSnapshot tracer toolbox filePath lines' =
    case toolboxSnapshotStore toolbox of
        Just store -> do
            let content = Text.encodeUtf8 $ Text.unlines lines'
                snapshot = makeSnapshot content
                ref = snapshotRef snapshot
            atomically $ modifyTVar' store (Map.insert ref snapshot)
            runTracer tracer (SnapshotTakenTrace filePath ref)
            pure $ Just ref
        Nothing -> pure Nothing

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

