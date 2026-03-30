{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Core media types for handling binary content in agent responses.

This module provides types for representing media attachments, media type
hints, and content parts for multi-modal responses. These types are used
throughout the agent system to handle binary tool outputs and pass them
to LLMs in appropriate formats.

The design is LLM-agnostic - conversion to LLM-specific formats happens
in the respective LLM modules (e.g., System.Agents.Session.OpenAI).
-}
module System.Agents.Media.Types (
    -- * Media Type Hierarchy
    MediaType (..),
    ImageType (..),
    AudioType (..),
    VideoType (..),
    ApplicationType (..),
    TextSubtype (..),

    -- * Media Attachments
    MediaAttachment (..),
    MediaTypeHint (..),

    -- * Content Parts for Mixed Responses
    ContentPart (..),

    -- * Utility Functions
    mediaTypeToMime,
    mimeToMediaType,
    imageTypeToMime,
) where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Media Type Hierarchy
-------------------------------------------------------------------------------

{- | High-level media type classification.

These types represent the major categories of media content that can be
passed to LLMs. The hierarchy allows for type-safe handling of different
media formats.
-}
data MediaType
    = MediaImage ImageType
    | MediaAudio AudioType
    | MediaVideo VideoType
    | MediaApplication ApplicationType
    | MediaText TextSubtype
    deriving (Show, Eq, Generic)

-- | Common image formats supported by major LLM providers.
data ImageType
    = ImagePNG
    | ImageJPEG
    | ImageGIF
    | ImageWebP
    | ImageSVG
    deriving (Show, Eq, Generic)

-- | Audio formats (for future expansion).
data AudioType
    = AudioMP3
    | AudioWAV
    | AudioOGG
    | AudioAAC
    deriving (Show, Eq, Generic)

-- | Video formats (for future expansion).
data VideoType
    = VideoMP4
    | VideoWebM
    | VideoOGG
    deriving (Show, Eq, Generic)

-- | Application/media types (for future expansion).
data ApplicationType
    = ApplicationPDF
    | ApplicationJSON
    | ApplicationOctetStream
    deriving (Show, Eq, Generic)

-- | Text subtypes (for structured text content).
data TextSubtype
    = TextPlain
    | TextHTML
    | TextMarkdown
    | TextCSV
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- Media Attachments
-------------------------------------------------------------------------------

{- | A media attachment containing base64-encoded binary data.

This type represents binary content that can be passed to LLMs. The content
is base64-encoded to allow safe JSON serialization. The mimeType field
indicates the actual content type, which may differ from what was detected
or declared by the tool.
-}
data MediaAttachment = MediaAttachment
    { mediaMimeType :: Text
    -- ^ MIME type of the content (e.g., "image/png")
    , mediaBase64Data :: Text
    -- ^ Base64-encoded content
    , mediaFilename :: Maybe Text
    -- ^ Optional filename hint
    }
    deriving (Show, Eq, Generic)

-- | For marking binary outputs without full MediaType resolution.
data MediaTypeHint
    = MediaTypeHint MediaType
    | BinaryUnknown
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- Content Parts for Mixed Responses
-------------------------------------------------------------------------------

{- | Individual part of a mixed/multi-modal response.

Mixed responses allow combining text and media in a single tool response.
This enables rich outputs like "Here's the analysis: [chart]" where the
chart is an image.
-}
data ContentPart
    = TextPart Text
    | MediaPart MediaAttachment
    deriving (Show, Eq, Generic)

-------------------------------------------------------------------------------
-- JSON Instances
-------------------------------------------------------------------------------

instance Aeson.ToJSON MediaType where
    toJSON mt =
        Aeson.object
            [ "category" .= category
            , "type" .= typeName
            ]
      where
        (category, typeName) = case mt of
            MediaImage t -> ("image" :: Text, imageTypeToMime t)
            MediaAudio t -> ("audio", audioTypeToMime t)
            MediaVideo t -> ("video", videoTypeToMime t)
            MediaApplication t -> ("application", appTypeToMime t)
            MediaText t -> ("text", textSubtypeToMime t)

instance Aeson.FromJSON MediaType where
    parseJSON = Aeson.withObject "MediaType" $ \v -> do
        category <- v .: "category"
        typeName <- v .: "type"
        case (category :: Text) of
            "image" -> MediaImage <$> parseImageType typeName
            "audio" -> MediaAudio <$> parseAudioType typeName
            "video" -> MediaVideo <$> parseVideoType typeName
            "application" -> MediaApplication <$> parseAppType typeName
            "text" -> MediaText <$> parseTextSubtype typeName
            _ -> fail $ "Unknown media category: " ++ Text.unpack category

instance Aeson.ToJSON MediaAttachment where
    toJSON ma =
        Aeson.object $
            [ "mimeType" .= ma.mediaMimeType
            , "base64Data" .= ma.mediaBase64Data
            ]
                ++ ["filename" .= fname | Just fname <- [ma.mediaFilename]]

instance Aeson.FromJSON MediaAttachment where
    parseJSON = Aeson.withObject "MediaAttachment" $ \v ->
        MediaAttachment
            <$> v .: "mimeType"
            <*> v .: "base64Data"
            <*> v .: "filename"

instance Aeson.ToJSON MediaTypeHint where
    toJSON (MediaTypeHint mt) =
        Aeson.object
            [ "hint" .= ("known" :: Text)
            , "mediaType" .= mt
            ]
    toJSON BinaryUnknown =
        Aeson.object
            [ "hint" .= ("unknown" :: Text)
            ]

instance Aeson.FromJSON MediaTypeHint where
    parseJSON = Aeson.withObject "MediaTypeHint" $ \v -> do
        hint <- v .: "hint"
        case (hint :: Text) of
            "known" -> MediaTypeHint <$> v .: "mediaType"
            "unknown" -> pure BinaryUnknown
            _ -> fail $ "Unknown hint type: " ++ Text.unpack hint

instance Aeson.ToJSON ContentPart where
    toJSON (TextPart t) =
        Aeson.object
            [ "type" .= ("text" :: Text)
            , "content" .= t
            ]
    toJSON (MediaPart m) =
        Aeson.object
            [ "type" .= ("media" :: Text)
            , "content" .= m
            ]

instance Aeson.FromJSON ContentPart where
    parseJSON = Aeson.withObject "ContentPart" $ \v -> do
        partType <- v .: "type"
        case (partType :: Text) of
            "text" -> TextPart <$> v .: "content"
            "media" -> MediaPart <$> v .: "content"
            _ -> fail $ "Unknown content part type: " ++ Text.unpack partType

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

{- | Convert a MediaType to its MIME type string.

Examples:

>>> mediaTypeToMime (MediaImage ImagePNG)
"image/png"

>>> mediaTypeToMime (MediaApplication ApplicationPDF)
"application/pdf"
-}
mediaTypeToMime :: MediaType -> Text
mediaTypeToMime (MediaImage t) = imageTypeToMime t
mediaTypeToMime (MediaAudio t) = audioTypeToMime t
mediaTypeToMime (MediaVideo t) = videoTypeToMime t
mediaTypeToMime (MediaApplication t) = appTypeToMime t
mediaTypeToMime (MediaText t) = textSubtypeToMime t

{- | Convert an ImageType to its MIME type string.

>>> imageTypeToMime ImagePNG
"image/png"
-}
imageTypeToMime :: ImageType -> Text
imageTypeToMime ImagePNG = "image/png"
imageTypeToMime ImageJPEG = "image/jpeg"
imageTypeToMime ImageGIF = "image/gif"
imageTypeToMime ImageWebP = "image/webp"
imageTypeToMime ImageSVG = "image/svg+xml"

audioTypeToMime :: AudioType -> Text
audioTypeToMime AudioMP3 = "audio/mpeg"
audioTypeToMime AudioWAV = "audio/wav"
audioTypeToMime AudioOGG = "audio/ogg"
audioTypeToMime AudioAAC = "audio/aac"

videoTypeToMime :: VideoType -> Text
videoTypeToMime VideoMP4 = "video/mp4"
videoTypeToMime VideoWebM = "video/webm"
videoTypeToMime VideoOGG = "video/ogg"

appTypeToMime :: ApplicationType -> Text
appTypeToMime ApplicationPDF = "application/pdf"
appTypeToMime ApplicationJSON = "application/json"
appTypeToMime ApplicationOctetStream = "application/octet-stream"

textSubtypeToMime :: TextSubtype -> Text
textSubtypeToMime TextPlain = "text/plain"
textSubtypeToMime TextHTML = "text/html"
textSubtypeToMime TextMarkdown = "text/markdown"
textSubtypeToMime TextCSV = "text/csv"

-- Parser helpers
parseImageType :: Text -> Parser ImageType
parseImageType "image/png" = pure ImagePNG
parseImageType "image/jpeg" = pure ImageJPEG
parseImageType "image/jpg" = pure ImageJPEG
parseImageType "image/gif" = pure ImageGIF
parseImageType "image/webp" = pure ImageWebP
parseImageType "image/svg+xml" = pure ImageSVG
parseImageType other = fail $ "Unknown image type: " ++ Text.unpack other

parseAudioType :: Text -> Parser AudioType
parseAudioType "audio/mpeg" = pure AudioMP3
parseAudioType "audio/wav" = pure AudioWAV
parseAudioType "audio/ogg" = pure AudioOGG
parseAudioType "audio/aac" = pure AudioAAC
parseAudioType other = fail $ "Unknown audio type: " ++ Text.unpack other

parseVideoType :: Text -> Parser VideoType
parseVideoType "video/mp4" = pure VideoMP4
parseVideoType "video/webm" = pure VideoWebM
parseVideoType "video/ogg" = pure VideoOGG
parseVideoType other = fail $ "Unknown video type: " ++ Text.unpack other

parseAppType :: Text -> Parser ApplicationType
parseAppType "application/pdf" = pure ApplicationPDF
parseAppType "application/json" = pure ApplicationJSON
parseAppType "application/octet-stream" = pure ApplicationOctetStream
parseAppType other = fail $ "Unknown application type: " ++ Text.unpack other

parseTextSubtype :: Text -> Parser TextSubtype
parseTextSubtype "text/plain" = pure TextPlain
parseTextSubtype "text/html" = pure TextHTML
parseTextSubtype "text/markdown" = pure TextMarkdown
parseTextSubtype "text/csv" = pure TextCSV
parseTextSubtype other = fail $ "Unknown text subtype: " ++ Text.unpack other

{- | Attempt to parse a MIME type string into a MediaType.

Returns Nothing if the MIME type is not recognized.

>>> mimeToMediaType "image/png"
Just (MediaImage ImagePNG)

>>> mimeToMediaType "application/unknown"
Nothing
-}
mimeToMediaType :: Text -> Maybe MediaType
mimeToMediaType mime =
    case Aeson.fromJSON (Aeson.String mime) of
        Aeson.Success mt -> Just mt
        Aeson.Error _ -> Nothing
