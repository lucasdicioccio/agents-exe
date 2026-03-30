{-# LANGUAGE OverloadedStrings #-}

{- | Media type detection from content bytes and headers.

This module provides functions for detecting media types from:
1. Magic bytes in binary content (file signatures)
2. Content-Type HTTP headers
3. Combined detection using both sources

The detection functions are designed to be extensible, with IO support
for future libmagic integration while providing pure alternatives for
common cases.

Magic byte detection currently supports common image formats:
- PNG: 0x89 0x50 0x4E 0x47
- JPEG: 0xFF 0xD8 0xFF
- GIF: "GIF87a" or "GIF89a"
- WebP: "RIFF....WEBP"
-}
module System.Agents.Media.Detection (
    -- * Detection Functions
    detectMediaType,
    detectFromMagicBytes,
    detectFromContentType,

    -- * Combined Detection
    detectMediaTypePure,

    -- * Utility Functions
    isValidUtf8,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import System.Agents.Media.Types

-------------------------------------------------------------------------------
-- IO-based Detection (for future libmagic integration)
-------------------------------------------------------------------------------

{- | Detect media type from Content-Type header and/or content bytes.

This function may perform IO for advanced detection (e.g., libmagic),
but currently operates purely. It's designed to be extensible for
future enhancements.

The detection order is:
1. Parse the Content-Type header if provided
2. Fall back to magic byte detection from content
3. Return Nothing if neither succeeds

Example:

>>> detectMediaType (Just "image/png") "\137PNG\r\n" :: IO (Maybe MediaType)
Just (MediaImage ImagePNG)
-}
detectMediaType :: Maybe Text -> ByteString -> IO (Maybe MediaType)
detectMediaType mContentType bytes = do
    -- First try Content-Type header
    let fromHeader = mContentType >>= detectFromContentType

    -- Then try magic bytes
    let fromMagic = detectFromMagicBytes bytes

    -- Prefer Content-Type, fall back to magic bytes
    pure $ listToMaybe $ concat [maybeToList fromHeader, maybeToList fromMagic]
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-------------------------------------------------------------------------------
-- Pure Detection Functions
-------------------------------------------------------------------------------

{- | Pure version of detectMediaType for use in pure contexts.

This is a convenience wrapper that runs the IO version with empty content
type and extracts the result.

Note: This is mainly useful for testing. In production, prefer the IO
version for future compatibility.
-}
detectMediaTypePure :: ByteString -> Maybe MediaType
detectMediaTypePure = detectFromMagicBytes

{- | Detect media type from magic bytes in content.

This function checks the file signature (magic bytes) at the beginning
of the content to determine the media type. It's a pure function that
works without any external dependencies.

Supported formats:
- PNG: bytes starting with 0x89 0x50 0x4E 0x47
- JPEG: bytes starting with 0xFF 0xD8 0xFF
- GIF: bytes starting with "GIF87a" or "GIF89a"
- WebP: bytes starting with "RIFF" and containing "WEBP" at offset 8

Example:

>>> detectFromMagicBytes "\137PNG\r\n\SUB\n"
Just (MediaImage ImagePNG)

>>> detectFromMagicBytes "GIF87a"
Just (MediaImage ImageGIF)

>>> detectFromMagicBytes "unknown content"
Nothing
-}
detectFromMagicBytes :: ByteString -> Maybe MediaType
detectFromMagicBytes bs
    | BS.length bs < 4 = Nothing
    | isPNG = Just $ MediaImage ImagePNG
    | isJPEG = Just $ MediaImage ImageJPEG
    | isGIF = Just $ MediaImage ImageGIF
    | isWebP = Just $ MediaImage ImageWebP
    | otherwise = Nothing
  where
    -- PNG: 0x89 0x50 0x4E 0x47
    isPNG = BS.take 4 bs == BS.pack [0x89, 0x50, 0x4E, 0x47]

    -- JPEG: 0xFF 0xD8 0xFF
    isJPEG = BS.take 3 bs == BS.pack [0xFF, 0xD8, 0xFF]

    -- GIF: "GIF87a" or "GIF89a"
    isGIF =
        BS.take 6 bs == "GIF87a"
            || BS.take 6 bs == "GIF89a"

    -- WebP: "RIFF" at 0, "WEBP" at 8
    isWebP =
        BS.length bs >= 12
            && BS.take 4 bs == "RIFF"
            && BS.take 4 (BS.drop 8 bs) == "WEBP"

{- | Detect media type from Content-Type header value.

Parses common MIME types and converts them to MediaType values.
This function handles the most common media types used with LLMs.

Example:

>>> detectFromContentType "image/png"
Just (MediaImage ImagePNG)

>>> detectFromContentType "Image/PNG"  -- case insensitive
Just (MediaImage ImagePNG)

>>> detectFromContentType "application/json"
Just (MediaApplication ApplicationJSON)

>>> detectFromContentType "text/html; charset=utf-8"  -- with params
Just (MediaText TextHTML)
-}
detectFromContentType :: Text -> Maybe MediaType
detectFromContentType ct =
    let -- Extract just the media type, ignoring parameters like charset
        mainType = Text.toLower $ Text.takeWhile (/= ';') $ Text.strip ct
     in case mainType of
            "image/png" -> Just $ MediaImage ImagePNG
            "image/jpeg" -> Just $ MediaImage ImageJPEG
            "image/jpg" -> Just $ MediaImage ImageJPEG
            "image/gif" -> Just $ MediaImage ImageGIF
            "image/webp" -> Just $ MediaImage ImageWebP
            "image/svg+xml" -> Just $ MediaImage ImageSVG
            "audio/mpeg" -> Just $ MediaAudio AudioMP3
            "audio/wav" -> Just $ MediaAudio AudioWAV
            "audio/ogg" -> Just $ MediaAudio AudioOGG
            "audio/aac" -> Just $ MediaAudio AudioAAC
            "video/mp4" -> Just $ MediaVideo VideoMP4
            "video/webm" -> Just $ MediaVideo VideoWebM
            "video/ogg" -> Just $ MediaVideo VideoOGG
            "application/pdf" -> Just $ MediaApplication ApplicationPDF
            "application/json" -> Just $ MediaApplication ApplicationJSON
            "application/octet-stream" -> Just $ MediaApplication ApplicationOctetStream
            "text/plain" -> Just $ MediaText TextPlain
            "text/html" -> Just $ MediaText TextHTML
            "text/markdown" -> Just $ MediaText TextMarkdown
            "text/csv" -> Just $ MediaText TextCSV
            _ -> Nothing

-------------------------------------------------------------------------------
-- UTF-8 Validation
-------------------------------------------------------------------------------

{- | Check if ByteString is valid UTF-8.

This is useful for deciding whether to treat binary content as text
or as binary media.

The function attempts to decode the ByteString as UTF-8 and returns
True only if all bytes are valid UTF-8 sequences with no replacement
characters.

Example:

>>> isValidUtf8 "Hello, World!"
True

>>> isValidUtf8 "\xff\xfe"  -- invalid UTF-8
False
-}
isValidUtf8 :: ByteString -> Bool
isValidUtf8 bs =
    let decoded = Text.decodeUtf8' bs
     in case decoded of
            Left _ -> False
            Right txt -> Text.encodeUtf8 txt == bs

