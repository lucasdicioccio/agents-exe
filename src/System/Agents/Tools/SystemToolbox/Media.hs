{-# LANGUAGE OverloadedStrings #-}

{- | Media type detection for file attachments.

This module provides functionality to detect MIME types from file extensions
and convert between MediaType values and MIME type strings.
-}
module System.Agents.Tools.SystemToolbox.Media (
    -- * Media type detection
    detectMediaType,
    getFileExtension,
    mediaTypeToMime,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (takeExtension)

import System.Agents.Media.Types (
    ApplicationType (..),
    AudioType (..),
    ImageType (..),
    MediaType (..),
    TextSubtype (..),
    VideoType (..),
 )

-------------------------------------------------------------------------------
-- Media Type Detection
-------------------------------------------------------------------------------

{- | Detect media type from file extension.

Returns Nothing if the file type is not recognized as a supported media type.
-}
detectMediaType :: FilePath -> Maybe MediaType
detectMediaType path =
    case Text.toLower $ Text.pack $ takeExtension path of
        -- Images
        ".png" -> Just $ MediaImage ImagePNG
        ".jpg" -> Just $ MediaImage ImageJPEG
        ".jpeg" -> Just $ MediaImage ImageJPEG
        ".gif" -> Just $ MediaImage ImageGIF
        ".webp" -> Just $ MediaImage ImageWebP
        ".svg" -> Just $ MediaImage ImageSVG
        -- Audio
        ".mp3" -> Just $ MediaAudio AudioMP3
        ".wav" -> Just $ MediaAudio AudioWAV
        ".ogg" -> Just $ MediaAudio AudioOGG
        ".aac" -> Just $ MediaAudio AudioAAC
        ".flac" -> Just $ MediaAudio AudioFLAC
        -- Video
        ".mp4" -> Just $ MediaVideo VideoMP4
        ".webm" -> Just $ MediaVideo VideoWebM
        ".mov" -> Just $ MediaVideo VideoMOV
        ".avi" -> Just $ MediaVideo VideoAVI
        -- Documents
        ".pdf" -> Just $ MediaApplication AppPDF
        ".json" -> Just $ MediaApplication AppJSON
        ".xml" -> Just $ MediaApplication AppXML
        ".zip" -> Just $ MediaApplication AppZip
        -- Text files (as octet-stream for generic handling)
        ".txt" -> Just $ MediaApplication AppOctetStream
        ".md" -> Just $ MediaApplication AppOctetStream
        ".csv" -> Just $ MediaApplication AppOctetStream
        -- Unknown
        _ -> Nothing

-- | Get file extension helper.
getFileExtension :: FilePath -> Text
getFileExtension = Text.toLower . Text.pack . takeExtension

-- | Convert MediaType to MIME type string.
mediaTypeToMime :: MediaType -> Text
mediaTypeToMime (MediaImage ImagePNG) = "image/png"
mediaTypeToMime (MediaImage ImageJPEG) = "image/jpeg"
mediaTypeToMime (MediaImage ImageGIF) = "image/gif"
mediaTypeToMime (MediaImage ImageWebP) = "image/webp"
mediaTypeToMime (MediaImage ImageSVG) = "image/svg+xml"
mediaTypeToMime (MediaAudio AudioMP3) = "audio/mp3"
mediaTypeToMime (MediaAudio AudioWAV) = "audio/wav"
mediaTypeToMime (MediaAudio AudioOGG) = "audio/ogg"
mediaTypeToMime (MediaAudio AudioAAC) = "audio/aac"
mediaTypeToMime (MediaAudio AudioFLAC) = "audio/flac"
mediaTypeToMime (MediaAudio AudioMPEG) = "audio/mpeg"
mediaTypeToMime (MediaVideo VideoMP4) = "video/mp4"
mediaTypeToMime (MediaVideo VideoWebM) = "video/webm"
mediaTypeToMime (MediaVideo VideoMOV) = "video/quicktime"
mediaTypeToMime (MediaVideo VideoAVI) = "video/avi"
mediaTypeToMime (MediaVideo VideoOGG) = "video/ogg"
mediaTypeToMime (MediaApplication AppPDF) = "application/pdf"
mediaTypeToMime (MediaApplication AppJSON) = "application/json"
mediaTypeToMime (MediaApplication AppXML) = "application/xml"
mediaTypeToMime (MediaApplication AppOctetStream) = "application/octet-stream"
mediaTypeToMime (MediaApplication AppZip) = "application/zip"
mediaTypeToMime (MediaText TextPlain) = "text/plain"
mediaTypeToMime (MediaText TextHTML) = "text/html"
mediaTypeToMime (MediaText TextCSS) = "text/css"
mediaTypeToMime (MediaText TextCSV) = "text/csv"
mediaTypeToMime (MediaText TextMarkdown) = "text/markdown"
