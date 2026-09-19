{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Streamed chat completions (@"stream": true@), folded back into the JSON
of a non-streamed completion.

The stream is a server-sent events body: @data: <chunk>@ lines, ending with
@data: [DONE]@. Each chunk carries a @delta@ of the message. Folding the
deltas gives the same @choices[0].message@ (content, tool calls, reasoning)
as a non-streamed call, so parsing and tool handling do not change; text
deltas are handed to a callback as they arrive.
-}
module System.Agents.LLMs.OpenAIStream (
    readChatCompletionStream,
    foldStreamChunks,
    StreamAcc,
    emptyStreamAcc,
    stepStreamChunk,
    finishStream,
) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | What the chunks said so far.
data StreamAcc = StreamAcc
    { saId :: Maybe Value
    , saModel :: Maybe Value
    , saContent :: [Text]
    -- ^ Text deltas, newest first.
    , saReasoning :: [Text]
    , saToolCalls :: Map Int ToolCallAcc
    , saFinishReason :: Maybe Value
    , saUsage :: Maybe Value
    , saError :: Maybe Value
    }

data ToolCallAcc = ToolCallAcc
    { tcaId :: Maybe Text
    , tcaType :: Maybe Text
    , tcaName :: [Text]
    , tcaArguments :: [Text]
    }

emptyStreamAcc :: StreamAcc
emptyStreamAcc = StreamAcc Nothing Nothing [] [] Map.empty Nothing Nothing Nothing

{- | Read a stream with the given body reader (an empty chunk means the end),
calling back on each text delta. Returns the folded completion, or the
error object a provider sent in the stream.
-}
readChatCompletionStream :: (Text -> IO ()) -> IO ByteString.ByteString -> IO (Either Value Value)
readChatCompletionStream onText readChunk = go emptyStreamAcc ByteString.empty
  where
    go acc buffer = do
        chunk <- readChunk
        if ByteString.null chunk
            then do
                (acc', _) <- consume acc [buffer]
                pure (result acc')
            else do
                let (complete, rest) = splitLines (buffer <> chunk)
                (acc', done) <- consume acc complete
                if done then pure (result acc') else go acc' rest

    consume acc [] = pure (acc, False)
    consume acc (line : rest) = case dataPayload line of
        Nothing -> consume acc rest
        Just "[DONE]" -> pure (acc, True)
        Just payload -> case Aeson.decodeStrict' payload of
            Nothing -> consume acc rest
            Just value -> do
                let (acc', delta) = stepStreamChunk acc value
                mapM_ onText delta
                consume acc' rest

    result acc = maybe (Right (finishStream acc)) Left acc.saError

-- | Complete lines (without their line ending), and what follows the last one.
splitLines :: ByteString.ByteString -> ([ByteString.ByteString], ByteString.ByteString)
splitLines bytes = case reverse (Char8.split '\n' bytes) of
    [] -> ([], ByteString.empty)
    (rest : complete) -> (map stripCR (reverse complete), rest)
  where
    stripCR l
        | not (ByteString.null l) && Char8.last l == '\r' = ByteString.init l
        | otherwise = l

-- | The payload of a @data:@ line.
dataPayload :: ByteString.ByteString -> Maybe ByteString.ByteString
dataPayload line = do
    rest <- ByteString.stripPrefix "data:" line
    pure $ fromMaybe rest (ByteString.stripPrefix " " rest)

-- | Fold decoded chunks, as 'readChatCompletionStream' does.
foldStreamChunks :: [Value] -> Either Value Value
foldStreamChunks chunks =
    let acc = foldl' (\a c -> fst (stepStreamChunk a c)) emptyStreamAcc chunks
     in maybe (Right (finishStream acc)) Left acc.saError

-- | Add one chunk; also returns its text delta, if any.
stepStreamChunk :: StreamAcc -> Value -> (StreamAcc, Maybe Text)
stepStreamChunk acc = \case
    Object o
        | Just err <- KeyMap.lookup "error" o -> (acc{saError = Just err}, Nothing)
        | otherwise ->
            let delta = firstChoice o >>= field "delta" >>= asObject
                choice = firstChoice o
                text = delta >>= field "content" >>= asText
                reasoning = delta >>= field "reasoning_content" >>= asText
                calls = maybe [] asArray (delta >>= field "tool_calls")
                acc' =
                    acc
                        { saId = orKeep (KeyMap.lookup "id" o) acc.saId
                        , saModel = orKeep (KeyMap.lookup "model" o) acc.saModel
                        , saContent = maybe acc.saContent (: acc.saContent) text
                        , saReasoning = maybe acc.saReasoning (: acc.saReasoning) reasoning
                        , saToolCalls = foldl' addToolCall acc.saToolCalls (zip [0 ..] calls)
                        , saFinishReason = orKeep (choice >>= field "finish_reason" >>= notNull) acc.saFinishReason
                        , saUsage = orKeep (KeyMap.lookup "usage" o >>= notNull) acc.saUsage
                        }
             in (acc', text >>= \t -> if Text.null t then Nothing else Just t)
    _ -> (acc, Nothing)
  where
    firstChoice o =
        KeyMap.lookup "choices" o >>= \v -> case asArray v of
            (c : _) -> asObject c
            [] -> Nothing
    orKeep new old = maybe old Just new
    notNull = \case
        Null -> Nothing
        v -> Just v

-- | Tool calls arrive in pieces, keyed by @index@ (their position when absent).
addToolCall :: Map Int ToolCallAcc -> (Int, Value) -> Map Int ToolCallAcc
addToolCall calls (position, value) = case asObject value of
    Nothing -> calls
    Just o ->
        let index = maybe position round (field "index" o >>= asNumber)
            function = field "function" o >>= asObject
            piece =
                ToolCallAcc
                    { tcaId = field "id" o >>= asText
                    , tcaType = field "type" o >>= asText
                    , tcaName = maybe [] pure (function >>= field "name" >>= asText)
                    , tcaArguments = maybe [] pure (function >>= field "arguments" >>= asText)
                    }
         in Map.insertWith merge index piece calls
  where
    -- 'Map.insertWith' passes the new piece first.
    merge :: ToolCallAcc -> ToolCallAcc -> ToolCallAcc
    merge new old =
        ToolCallAcc
            { tcaId = maybe old.tcaId Just new.tcaId
            , tcaType = maybe old.tcaType Just new.tcaType
            , tcaName = new.tcaName <> old.tcaName
            , tcaArguments = new.tcaArguments <> old.tcaArguments
            }

-- | The completion JSON a non-streamed call would have returned.
finishStream :: StreamAcc -> Value
finishStream acc =
    Aeson.object $
        catMaybePairs
            [ ("id" .=) <$> acc.saId
            , ("model" .=) <$> acc.saModel
            , ("usage" .=) <$> acc.saUsage
            ]
            <> [ "object" .= ("chat.completion" :: Text)
               , "choices"
                    .= [ Aeson.object
                            [ "index" .= (0 :: Int)
                            , "message" .= message
                            , "finish_reason" .= fromMaybe Null acc.saFinishReason
                            ]
                       ]
               ]
  where
    catMaybePairs = mapMaybe id
    message =
        Aeson.object $
            [ "role" .= ("assistant" :: Text)
            , "content" .= if null acc.saContent then Null else String (Text.concat (reverse acc.saContent))
            ]
                <> ["tool_calls" .= map toolCall (Map.elems acc.saToolCalls) | not (Map.null acc.saToolCalls)]
                <> ["reasoning_content" .= Text.concat (reverse acc.saReasoning) | not (null acc.saReasoning)]
    toolCall :: ToolCallAcc -> Value
    toolCall tc =
        Aeson.object
            [ "id" .= fromMaybe ("" :: Text) tc.tcaId
            , "type" .= fromMaybe ("function" :: Text) tc.tcaType
            , "function"
                .= Aeson.object
                    [ "name" .= Text.concat (reverse tc.tcaName)
                    , "arguments" .= Text.concat (reverse tc.tcaArguments)
                    ]
            ]

field :: Aeson.Key -> Aeson.Object -> Maybe Value
field = KeyMap.lookup

asObject :: Value -> Maybe Aeson.Object
asObject = \case
    Object o -> Just o
    _ -> Nothing

asText :: Value -> Maybe Text
asText = \case
    String t -> Just t
    _ -> Nothing

asNumber :: Value -> Maybe Double
asNumber = \case
    Number n -> Just (realToFrac n)
    _ -> Nothing

asArray :: Value -> [Value]
asArray = \case
    Array xs -> Vector.toList xs
    _ -> []
