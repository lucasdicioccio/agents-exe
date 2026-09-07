{-# LANGUAGE OverloadedStrings #-}

{- |
Range parsing utilities for the DeveloperToolbox.

This module provides functionality to parse range specifications used by
the read-file-range and write-file-range capabilities.

Calling semantics for write-file-range (the common source of confusion):
- "N"       -> REPLACE line N
- "N-M"     -> REPLACE lines N through M
- "N+"      -> INSERT AFTER line N
- "head"    -> PREPEND before line 1 (use to CREATE new files)
- "tail"    -> APPEND after the last line
- "whole"   -> REPLACE the entire file

Supported formats:
- Single number: "5" -> Lines (5, 5)  (replaces line 5)
- Range: "1-10" -> Lines (1, 10)      (replaces lines 1-10)
- Insert after single: "5+" -> After 5 (insert after line 5)
- Insert after range: "1-5+" -> After 5 (insert after line 5, end of range)
- Special: "head" -> Head, "tail" -> Tail, "whole" -> Whole
- Multiple: "1-10,20-30" -> [Lines (1, 10), Lines (20, 30)]

Default behavior:
- Empty string or "whole" -> Reads/writes entire file
- For read-file-range, omitting ranges defaults to reading the whole file
- For write-file-range, "head" creates new files, "tail" appends, "whole" overwrites

WARNING: "N" and "N+" are not interchangeable. Use "54+" to insert after
line 54; using "54" will replace line 54.
-}
module System.Agents.Tools.DeveloperToolbox.Range (
    -- * Range parsing
    parseRanges,
    parseRangePart,
    parsePositiveInt,
) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Tools.DeveloperToolbox.Types (
    DeveloperToolError (..),
    RangeSpec (..),
 )

{- | Parse a ranges string into a list of RangeSpec.

Calling semantics (write-file-range):
- "N"       -> REPLACE line N
- "N-M"     -> REPLACE lines N through M
- "N+"      -> INSERT AFTER line N
- "head"    -> PREPEND before line 1
- "tail"    -> APPEND after the last line
- "whole"   -> REPLACE the entire file

Supported formats:
- Empty string "" -> [Whole] (default to entire file)
- "whole" -> Whole (entire file)
- Single number: "5" -> Lines (5, 5)
- Range: "1-10" -> Lines (1, 10)
- Insert after: "5+" -> After 5, "1-10+" -> After 10
- Special: "head" -> Head, "tail" -> Tail
- Multiple: "1-10,20-30" -> [Lines (1, 10), Lines (20, 30)]

Returns Left with error message if parsing fails.

Note: An empty string now defaults to [Whole] for better UX when ranges
parameter is omitted in read-file-range. This provides intuitive behavior
where omitting ranges reads the entire file.

WARNING: "N" replaces line N; "N+" inserts after line N. Use "54+" to
insert after line 54; using "54" will replace line 54.
-}
parseRanges :: Text -> Either DeveloperToolError [RangeSpec]
parseRanges txt
    -- Default to whole file when ranges is empty (not provided)
    | Text.null txt = Right [Whole]
    | otherwise =
        let parts = map Text.strip $ Text.splitOn "," txt
         in mapM parseRangePart parts

-- | Parse a single range part.
parseRangePart :: Text -> Either DeveloperToolError RangeSpec
parseRangePart part
    | part == "head" = Right Head
    | part == "tail" = Right Tail
    | part == "whole" = Right Whole
    | "+" `Text.isSuffixOf` part = parseAfterRange part
    | "-" `Text.isInfixOf` part = parseLineRange part
    | otherwise = parseSingleLine part

-- | Parse an insert-after range (N+ or N-M+).
parseAfterRange :: Text -> Either DeveloperToolError RangeSpec
parseAfterRange part =
    let basePart = Text.dropEnd 1 part -- Remove the trailing '+'
     in if "-" `Text.isInfixOf` basePart
            then -- Range form: "N-M+" means insert after M
                case Text.breakOn "-" basePart of
                    (_, "") -> Left $ InvalidRangeError $ "Invalid insert-after range format: " <> part
                    (startStr, rest) -> do
                        let endStr = Text.drop 1 rest
                        _start <- parsePositiveInt startStr "Invalid start line in insert-after range"
                        end <- parsePositiveInt endStr "Invalid end line in insert-after range"
                        Right $ After end
            else -- Single line form: "N+" means insert after N
                do
                    n <- parsePositiveInt basePart "Invalid line number in insert-after range"
                    Right $ After n

-- | Parse a line range (N-M).
parseLineRange :: Text -> Either DeveloperToolError RangeSpec
parseLineRange part =
    case Text.breakOn "-" part of
        (startStr, rest) | not (Text.null rest) -> do
            let endStr = Text.drop 1 rest
            start <- parsePositiveInt startStr "Invalid start line"
            end <- parsePositiveInt endStr "Invalid end line"
            if start <= end
                then Right $ Lines (start, end)
                else Left $ InvalidRangeError $ "Start line must be <= end line in range: " <> part
        _ -> Left $ InvalidRangeError $ "Invalid range format: " <> part

-- | Parse a single line number.
parseSingleLine :: Text -> Either DeveloperToolError RangeSpec
parseSingleLine part = do
    n <- parsePositiveInt part "Invalid line number"
    Right $ Lines (n, n)

-- | Parse a positive integer from Text.
parsePositiveInt :: Text -> Text -> Either DeveloperToolError Int
parsePositiveInt txt errPrefix =
    let str = Text.unpack txt
     in if all isDigit str && not (null str)
            then
                let n = read str :: Int
                 in if n > 0
                        then Right n
                        else Left $ InvalidRangeError $ errPrefix <> " (must be positive): " <> txt
            else Left $ InvalidRangeError $ errPrefix <> ": " <> txt

