{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How an attachment is turned into an OpenAI content part.
module MediaContentPartTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Tasty
import Test.Tasty.HUnit

import System.Agents.Media.Types (MediaAttachment (..))
import System.Agents.Session.OpenAI (mediaContentPart)

tests :: TestTree
tests =
    testGroup
        "Media content parts"
        [ testCase "a pdf travels as a file part" pdfTest
        , testCase "an image travels as an image part" imageTest
        , testCase "text is inlined, labelled with its filename" textTest
        , testCase "text types that are not text/*" applicationTextTest
        , testCase "a charset does not hide the type" charsetTest
        , testCase "bytes that are not UTF-8 are not inlined" binaryTest
        , testCase "unknown types keep the old behaviour" unknownTest
        ]

attach :: Text -> Text -> Maybe Text -> MediaAttachment
attach mime raw name = MediaAttachment mime (base64 raw) name

base64 :: Text -> Text
base64 = Text.decodeUtf8 . Base64.encode . Text.encodeUtf8

pdfTest :: Assertion
pdfTest = do
    let part = mediaContentPart (attach "application/pdf" "%PDF-1.4" (Just "report.pdf"))
    encoded part `contains` "\"type\":\"file\""
    encoded part `contains` "report.pdf"
    encoded part `contains` "data:application/pdf;base64,"

imageTest :: Assertion
imageTest = do
    let part = mediaContentPart (attach "image/png" "notreallyapng" (Just "shot.png"))
    encoded part `contains` "\"type\":\"image_url\""
    encoded part `contains` "data:image/png;base64,"

{- | The point of the change: a text file used to be sent as an image, which
providers refuse outright.
-}
textTest :: Assertion
textTest = do
    let part = mediaContentPart (attach "text/plain" "hello from a file" (Just "note.txt"))
    encoded part `contains` "\"type\":\"text\""
    encoded part `contains` "hello from a file"
    -- Labelled, so the model does not read it as the user's own words.
    encoded part `contains` "Attached file note.txt (text/plain)"
    encoded part `lacks` "image_url"
    encoded part `lacks` "base64"

applicationTextTest :: Assertion
applicationTextTest = do
    mapM_
        (\mime -> encoded (mediaContentPart (attach mime "{\"a\":1}" (Just "f"))) `contains` "\"type\":\"text\"")
        ["application/json", "application/xml", "application/yaml", "application/vnd.api+json", "image/svg+xml"]

charsetTest :: Assertion
charsetTest = do
    let part = mediaContentPart (attach "text/csv; charset=utf-8" "a,b\n1,2" (Just "rows.csv"))
    encoded part `contains` "\"type\":\"text\""
    encoded part `contains` "Attached file rows.csv (text/csv)"

-- | A mislabelled binary must not be inlined as mojibake.
binaryTest :: Assertion
binaryTest = do
    let bogus = MediaAttachment "text/plain" (Text.decodeUtf8 (Base64.encode "\xff\xfe\x00\x01")) (Just "x.txt")
    encoded (mediaContentPart bogus) `contains` "\"type\":\"image_url\""

unknownTest :: Assertion
unknownTest = do
    encoded (mediaContentPart (attach "audio/mpeg" "sound" (Just "a.mp3"))) `contains` "\"type\":\"image_url\""
    encoded (mediaContentPart (attach "application/zip" "zip" (Just "a.zip"))) `contains` "\"type\":\"image_url\""

-- | The part as JSON text, for substring assertions.
encoded :: Aeson.Value -> Text
encoded = Text.decodeUtf8 . LByteString.toStrict . Aeson.encode

contains :: Text -> Text -> Assertion
contains haystack needle =
    assertBool (Text.unpack (needle <> " not in " <> haystack)) (needle `Text.isInfixOf` haystack)

lacks :: Text -> Text -> Assertion
lacks haystack needle =
    assertBool (Text.unpack (needle <> " unexpectedly in " <> haystack)) (not (needle `Text.isInfixOf` haystack))
