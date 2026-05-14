{-# LANGUAGE OverloadedStrings #-}

{- |
Range parsing utilities for the DeveloperToolbox.

This module provides functionality to parse range specifications used by
the read-file-range and write-file-range capabilities.

Supported formats:
- Single number: "5" -> Lines (5, 5)
- Range: "1-10" -> Lines (1, 10)
- Special: "head" -> Head, "tail" -> Tail
- Multiple: "1-10,20-30" -> [Lines (1, 10), Lines (20, 30)]
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

Supports formats:
- Single number: "5" -> Lines (5, 5)
- Range: "1-10" -> Lines (1, 10)
- Special: "head" -> Head, "tail" -> Tail
- Multiple: "1-10,20-30" -> [Lines (1, 10), Lines (20, 30)]

Returns Left with error message if parsing fails.
-}
parseRanges :: Text -> Either DeveloperToolError [RangeSpec]
parseRanges txt
    | Text.null txt = Right []
    | otherwise =
        let parts = map Text.strip $ Text.splitOn "," txt
         in mapM parseRangePart parts

-- | Parse a single range part.
parseRangePart :: Text -> Either DeveloperToolError RangeSpec
parseRangePart part
    | part == "head" = Right Head
    | part == "tail" = Right Tail
    | "-" `Text.isInfixOf` part =
        case Text.breakOn "-" part of
            (startStr, rest) | not (Text.null rest) -> do
                let endStr = Text.drop 1 rest
                start <- parsePositiveInt startStr "Invalid start line"
                end <- parsePositiveInt endStr "Invalid end line"
                if start <= end
                    then Right $ Lines (start, end)
                    else Left $ InvalidRangeError $ "Start line must be <= end line in range: " <> part
            _ -> Left $ InvalidRangeError $ "Invalid range format: " <> part
    | otherwise = do
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

