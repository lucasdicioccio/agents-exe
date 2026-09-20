{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for folding streamed chat completions.
module OpenAIStreamTests (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import qualified System.Agents.LLMs.OpenAI as OpenAI
import System.Agents.LLMs.OpenAIStream

tests :: TestTree
tests =
    testGroup
        "OpenAI streaming"
        [ testCase "text deltas fold into the message content" textTest
        , testCase "tool calls are assembled from their pieces" toolCallTest
        , testCase "a stream with only tool calls has null content" noContentTest
        , testCase "chunks split anywhere, CRLF, comments, and [DONE]" readerTest
        , testCase "an error in the stream is returned" errorTest
        ]

chunk :: Aeson.Value -> Aeson.Value
chunk delta = Aeson.object ["id" Aeson..= ("c1" :: Text), "model" Aeson..= ("m" :: Text), "choices" Aeson..= [Aeson.object ["index" Aeson..= (0 :: Int), "delta" Aeson..= delta]]]

textChunk :: Text -> Aeson.Value
textChunk t = chunk (Aeson.object ["content" Aeson..= t])

finishChunk :: Aeson.Value
finishChunk = Aeson.object ["choices" Aeson..= [Aeson.object ["index" Aeson..= (0 :: Int), "delta" Aeson..= Aeson.object [], "finish_reason" Aeson..= ("stop" :: Text)]]]

usageChunk :: Aeson.Value
usageChunk =
    Aeson.object
        [ "choices" Aeson..= ([] :: [Aeson.Value])
        , "usage" Aeson..= Aeson.object ["prompt_tokens" Aeson..= (3 :: Int), "completion_tokens" Aeson..= (2 :: Int), "total_tokens" Aeson..= (5 :: Int)]
        ]

-- | The folded completion, parsed as a non-streamed one would be.
parsed :: Either Aeson.Value Aeson.Value -> IO OpenAI.Response
parsed (Left err) = assertFailure ("stream error: " <> show err)
parsed (Right v) = either (\e -> assertFailure ("unparsable: " <> e <> " in " <> show v)) pure (Aeson.parseEither OpenAI.parseLLMResponse v)

textTest :: Assertion
textTest = do
    rsp <- parsed $ foldStreamChunks [chunk (Aeson.object ["role" Aeson..= ("assistant" :: Text), "content" Aeson..= ("" :: Text)]), textChunk "Hel", textChunk "lo", finishChunk, usageChunk]
    rsp.rspContent @?= Just "Hello"
    fmap (.tokenTotalTokens) rsp.rspTokenUsage @?= Just 5

toolCallTest :: Assertion
toolCallTest = do
    let callPiece fields = chunk (Aeson.object ["tool_calls" Aeson..= [Aeson.object fields]])
    rsp <-
        parsed $
            foldStreamChunks
                [ callPiece ["index" Aeson..= (0 :: Int), "id" Aeson..= ("call_a" :: Text), "type" Aeson..= ("function" :: Text), "function" Aeson..= Aeson.object ["name" Aeson..= ("get_weather" :: Text), "arguments" Aeson..= ("" :: Text)]]
                , callPiece ["index" Aeson..= (1 :: Int), "id" Aeson..= ("call_b" :: Text), "function" Aeson..= Aeson.object ["name" Aeson..= ("get_time" :: Text), "arguments" Aeson..= ("{}" :: Text)]]
                , callPiece ["index" Aeson..= (0 :: Int), "function" Aeson..= Aeson.object ["arguments" Aeson..= ("{\"city\": " :: Text)]]
                , callPiece ["index" Aeson..= (0 :: Int), "function" Aeson..= Aeson.object ["arguments" Aeson..= ("\"Paris\"}" :: Text)]]
                , finishChunk
                ]
    case rsp.rspToolCalls of
        Just [a, b] -> do
            (a.toolCallId, a.toolCallFunction.toolCallFunctionName, a.toolCallFunction.toolCallFunctionArgsUnparsed)
                @?= ("call_a", OpenAI.ToolName "get_weather", "{\"city\": \"Paris\"}")
            a.toolCallFunction.toolCallFunctionArgs @?= Just (Aeson.object ["city" Aeson..= ("Paris" :: Text)])
            (b.toolCallId, b.toolCallType) @?= ("call_b", Just "function")
        other -> assertFailure ("expected two tool calls, got " <> show (fmap length other))

noContentTest :: Assertion
noContentTest = do
    rsp <- parsed $ foldStreamChunks [chunk (Aeson.object ["tool_calls" Aeson..= [Aeson.object ["index" Aeson..= (0 :: Int), "id" Aeson..= ("x" :: Text), "function" Aeson..= Aeson.object ["name" Aeson..= ("t" :: Text), "arguments" Aeson..= ("{}" :: Text)]]]])]
    rsp.rspContent @?= Nothing

readerTest :: Assertion
readerTest = do
    let frame v = "data: " <> ByteString.toStrict (Aeson.encode v) <> "\r\n\r\n"
        body =
            ": a comment\n\n"
                <> frame (textChunk "Hel")
                <> frame (textChunk "lo, ")
                <> frame (textChunk "world")
                <> "data: [DONE]\n\n"
                <> frame (textChunk "ignored after DONE")
    -- Hand the body over in 7-byte pieces, splitting lines and JSON.
    remaining <- newIORef (Char8.unpack body)
    deltas <- newIORef []
    let readPiece = atomicModifyIORef' remaining (\s -> (drop 7 s, Char8.pack (take 7 s)))
    result <- readChatCompletionStream (\t -> modifyIORef' deltas (t :)) readPiece
    rsp <- parsed result
    rsp.rspContent @?= Just "Hello, world"
    reverse <$> readIORef deltas >>= (@?= ["Hel", "lo, ", "world"])

errorTest :: Assertion
errorTest =
    case foldStreamChunks [textChunk "partial", Aeson.object ["error" Aeson..= Aeson.object ["message" Aeson..= ("overloaded" :: Text)]]] of
        Left err -> err @?= Aeson.object ["message" Aeson..= ("overloaded" :: Text)]
        Right v -> assertFailure ("expected an error, got " <> show v)
