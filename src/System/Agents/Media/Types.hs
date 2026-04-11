{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Core media types for multi-modal LLM interactions.

This module defines LLM-agnostic media types for handling binary content
in agent sessions. The types support base64-encoded media attachments and
mixed multi-modal responses.

Design principles:
* LLM-agnostic core types - no OpenAI/Claude-specific formats
* Explicit media type declarations via MIME types
* Support for mixed text + media content parts
* Base64 encoding for binary data transport
-}
module System.Agents.Media.Types (
    -- * Media Type Classification
    MediaType (..),
    ImageType (..),
    AudioType (..),
    VideoType (..),
    ApplicationType (..),
    TextSubtype (..),

    -- * Media Attachments
    MediaAttachment (..),
    ContentPart (..),

    -- * MIME Type Conversion
    mediaTypeToMime,
    mimeToMediaType,
    parseMimeType,

    -- * Content-Type Header Extraction
    extractContentType,
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.CaseInsensitive as CI
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import GHC.Generics (Generic)
import qualified Network.HTTP.Types as HttpTypes

-------------------------------------------------------------------------------
-- Media Type Classification
-------------------------------------------------------------------------------

{- | High-level media type classification.

Represents the major MIME type categories that are relevant for LLM interactions.
-}
data MediaType
    = MediaImage ImageType
    | MediaAudio AudioType
    | MediaVideo VideoType
    | MediaApplication ApplicationType
    | MediaText TextSubtype
    deriving (Show, Eq, Ord, Generic)

instance ToJSON MediaType where
    toJSON mt = Aeson.String (mediaTypeToMime mt)

instance FromJSON MediaType where
    parseJSON = Aeson.withText "MediaType" $ \txt ->
        case mimeToMediaType txt of
            Just mt -> pure mt
            Nothing -> fail $ "Unknown media type: " <> Text.unpack txt

-- | Image format types supported by major LLM providers.
data ImageType
    = ImagePNG
    | ImageJPEG
    | ImageGIF
    | ImageWebP
    | ImageSVG
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ImageType where
    toJSON = Aeson.String . imageTypeToMime

instance FromJSON ImageType where
    parseJSON = Aeson.withText "ImageType" $ \txt ->
        case parseImageType txt of
            Just (MediaImage it) -> pure it
            _ -> fail $ "Unknown image type: " <> Text.unpack txt

-- | Audio format types.
data AudioType
    = AudioMPEG
    | AudioWAV
    | AudioOGG
    | AudioMP3
    | AudioAAC
    | AudioFLAC
    deriving (Show, Eq, Ord, Generic)

instance ToJSON AudioType where
    toJSON = Aeson.String . audioTypeToMime

instance FromJSON AudioType where
    parseJSON = Aeson.withText "AudioType" $ \txt ->
        case parseAudioType txt of
            Just at -> pure at
            Nothing -> fail $ "Unknown audio type: " <> Text.unpack txt

-- | Video format types.
data VideoType
    = VideoMP4
    | VideoWebM
    | VideoOGG
    | VideoAVI
    | VideoMOV
    deriving (Show, Eq, Ord, Generic)

instance ToJSON VideoType where
    toJSON = Aeson.String . videoTypeToMime

instance FromJSON VideoType where
    parseJSON = Aeson.withText "VideoType" $ \txt ->
        case parseVideoType txt of
            Just vt -> pure vt
            Nothing -> fail $ "Unknown video type: " <> Text.unpack txt

-- | Application/Document format types.
data ApplicationType
    = AppPDF
    | AppJSON
    | AppXML
    | AppOctetStream
    | AppZip
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ApplicationType where
    toJSON = Aeson.String . appTypeToMime

instance FromJSON ApplicationType where
    parseJSON = Aeson.withText "ApplicationType" $ \txt ->
        case parseAppType txt of
            Just at -> pure at
            Nothing -> fail $ "Unknown application type: " <> Text.unpack txt

-- | Text subtypes (when text content should be treated as a specific format).
data TextSubtype
    = TextPlain
    | TextHTML
    | TextCSS
    | TextCSV
    | TextMarkdown
    deriving (Show, Eq, Ord, Generic)

instance ToJSON TextSubtype where
    toJSON = Aeson.String . textSubtypeToMime

instance FromJSON TextSubtype where
    parseJSON = Aeson.withText "TextSubtype" $ \txt ->
        case parseTextSubtype txt of
            Just st -> pure st
            Nothing -> fail $ "Unknown text subtype: " <> Text.unpack txt

-------------------------------------------------------------------------------
-- Media Attachment
-------------------------------------------------------------------------------

{- | A media attachment containing base64-encoded binary data.

This represents a single piece of media (image, audio, video, etc.) that can
be included in LLM conversations. The data is always base64-encoded for safe
JSON transport.
-}
data MediaAttachment = MediaAttachment
    { mediaMimeType :: Text
    -- ^ MIME type of the content (e.g., "image/png")
    , mediaBase64Data :: Text
    -- ^ Base64-encoded content (already UTF-8 safe)
    , mediaFilename :: Maybe Text
    -- ^ Optional filename for the attachment
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON MediaAttachment where
    toJSON media =
        Aeson.object $
            [ "mimeType" .= media.mediaMimeType
            , "base64Data" .= media.mediaBase64Data
            ]
                ++ ["filename" .= fname | Just fname <- [media.mediaFilename]]

instance FromJSON MediaAttachment where
    parseJSON = Aeson.withObject "MediaAttachment" $ \v ->
        MediaAttachment
            <$> v .: "mimeType"
            <*> v .: "base64Data"
            <*> v .:? "filename"

-------------------------------------------------------------------------------
-- Content Part (for mixed responses)
-------------------------------------------------------------------------------

{- | Individual part of a mixed multi-modal response.

A mixed response can contain alternating text and media parts, allowing
rich multi-modal interactions like:
- "Here's the chart:" (TextPart)
- [image of chart] (MediaPart)
- "As you can see..." (TextPart)
-}
data ContentPart
    = TextPart Text
    | MediaPart MediaAttachment
    deriving (Show, Eq, Ord, Generic)

instance ToJSON ContentPart where
    toJSON (TextPart txt) =
        Aeson.object
            [ "type" .= ("text" :: Text)
            , "text" .= txt
            ]
    toJSON (MediaPart media) =
        Aeson.object $
            [ "type" .= ("media" :: Text)
            , "mimeType" .= media.mediaMimeType
            , "base64Data" .= media.mediaBase64Data
            ]
                ++ ["filename" .= fname | Just fname <- [media.mediaFilename]]

instance FromJSON ContentPart where
    parseJSON = Aeson.withObject "ContentPart" $ \v -> do
        partType <- v .: "type"
        case partType :: Text of
            "text" -> TextPart <$> v .: "text"
            "media" ->
                MediaPart
                    <$> ( MediaAttachment
                            <$> v .: "mimeType"
                            <*> v .: "base64Data"
                            <*> v .:? "filename"
                        )
            _ -> fail $ "Unknown content part type: " <> Text.unpack partType

-------------------------------------------------------------------------------
-- MIME Type Conversion
-------------------------------------------------------------------------------

-- | Convert a MediaType to its MIME type string.
mediaTypeToMime :: MediaType -> Text
mediaTypeToMime (MediaImage it) = imageTypeToMime it
mediaTypeToMime (MediaAudio at) = audioTypeToMime at
mediaTypeToMime (MediaVideo vt) = videoTypeToMime vt
mediaTypeToMime (MediaApplication at) = appTypeToMime at
mediaTypeToMime (MediaText st) = textSubtypeToMime st

imageTypeToMime :: ImageType -> Text
imageTypeToMime ImagePNG = "image/png"
imageTypeToMime ImageJPEG = "image/jpeg"
imageTypeToMime ImageGIF = "image/gif"
imageTypeToMime ImageWebP = "image/webp"
imageTypeToMime ImageSVG = "image/svg+xml"

audioTypeToMime :: AudioType -> Text
audioTypeToMime AudioMPEG = "audio/mpeg"
audioTypeToMime AudioWAV = "audio/wav"
audioTypeToMime AudioOGG = "audio/ogg"
audioTypeToMime AudioMP3 = "audio/mp3"
audioTypeToMime AudioAAC = "audio/aac"
audioTypeToMime AudioFLAC = "audio/flac"

videoTypeToMime :: VideoType -> Text
videoTypeToMime VideoMP4 = "video/mp4"
videoTypeToMime VideoWebM = "video/webm"
videoTypeToMime VideoOGG = "video/ogg"
videoTypeToMime VideoAVI = "video/avi"
videoTypeToMime VideoMOV = "video/quicktime"

appTypeToMime :: ApplicationType -> Text
appTypeToMime AppPDF = "application/pdf"
appTypeToMime AppJSON = "application/json"
appTypeToMime AppXML = "application/xml"
appTypeToMime AppOctetStream = "application/octet-stream"
appTypeToMime AppZip = "application/zip"

textSubtypeToMime :: TextSubtype -> Text
textSubtypeToMime TextPlain = "text/plain"
textSubtypeToMime TextHTML = "text/html"
textSubtypeToMime TextCSS = "text/css"
textSubtypeToMime TextCSV = "text/csv"
textSubtypeToMime TextMarkdown = "text/markdown"

-- | Parse a MIME type string to MediaType.
mimeToMediaType :: Text -> Maybe MediaType
mimeToMediaType mime =
    parseImageType mime
        <|> (MediaAudio <$> parseAudioType mime)
        <|> (MediaVideo <$> parseVideoType mime)
        <|> (MediaApplication <$> parseAppType mime)
        <|> (MediaText <$> parseTextSubtype mime)

parseImageType :: Text -> Maybe MediaType
parseImageType mime =
    MediaImage <$> case Text.toLower mime of
        "image/png" -> Just ImagePNG
        "image/jpeg" -> Just ImageJPEG
        "image/jpg" -> Just ImageJPEG
        "image/gif" -> Just ImageGIF
        "image/webp" -> Just ImageWebP
        "image/svg" -> Just ImageSVG
        "image/svg+xml" -> Just ImageSVG
        _ -> Nothing

parseAudioType :: Text -> Maybe AudioType
parseAudioType mime = case Text.toLower mime of
    "audio/mpeg" -> Just AudioMPEG
    "audio/wav" -> Just AudioWAV
    "audio/wave" -> Just AudioWAV
    "audio/x-wav" -> Just AudioWAV
    "audio/ogg" -> Just AudioOGG
    "audio/mp3" -> Just AudioMP3
    "audio/aac" -> Just AudioAAC
    "audio/flac" -> Just AudioFLAC
    _ -> Nothing

parseVideoType :: Text -> Maybe VideoType
parseVideoType mime = case Text.toLower mime of
    "video/mp4" -> Just VideoMP4
    "video/webm" -> Just VideoWebM
    "video/ogg" -> Just VideoOGG
    "video/avi" -> Just VideoAVI
    "video/quicktime" -> Just VideoMOV
    "video/mov" -> Just VideoMOV
    _ -> Nothing

parseAppType :: Text -> Maybe ApplicationType
parseAppType mime = case Text.toLower mime of
    "application/pdf" -> Just AppPDF
    "application/json" -> Just AppJSON
    "application/xml" -> Just AppXML
    "application/octet-stream" -> Just AppOctetStream
    "application/zip" -> Just AppZip
    _ -> Nothing

parseTextSubtype :: Text -> Maybe TextSubtype
parseTextSubtype mime = case Text.toLower mime of
    "text/plain" -> Just TextPlain
    "text/html" -> Just TextHTML
    "text/css" -> Just TextCSS
    "text/csv" -> Just TextCSV
    "text/markdown" -> Just TextMarkdown
    "text/md" -> Just TextMarkdown
    _ -> Nothing

-- | Parse a MIME type string, handling parameters (e.g., "application/json; charset=utf-8").
parseMimeType :: Text -> Maybe Text
parseMimeType = Just . Text.takeWhile (/= ';') . Text.strip

-------------------------------------------------------------------------------
-- Content-Type Header Extraction
-------------------------------------------------------------------------------

{- | Extract the Content-Type from HTTP response headers.

Returns the MIME type (without parameters) if a Content-Type header is present.
-}
extractContentType :: HttpTypes.ResponseHeaders -> Maybe Text
extractContentType headers = do
    (_, contentType) <- listToMaybe $ filter isContentTypeHeader headers
    let contentTypeText = Text.Encoding.decodeUtf8 contentType
    parseMimeType contentTypeText
  where
    isContentTypeHeader (name, _) = CI.foldedCase name == "content-type"
