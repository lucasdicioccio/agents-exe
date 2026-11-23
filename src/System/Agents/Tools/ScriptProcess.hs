{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @process@ object of a v2 bash tool's @describe@ document
(@todos/bash-tools-v2.md@, §1.1), and what the runtime does with a
@oneshot@ v2 tool's output (§2.1): cut stdout into events per the declared
framing, validate them against the declared event schema, render the
result.

It also holds what a @stream@ tool's result is made of (§2.2): a bounded
ring of the latest events ('EventRing'), rendered with the count of the
events it no longer holds, and the @ready@ line match.

Declarations that need a later slice (@service@ mode, @input@) are parsed
but refused at load by 'System.Agents.Tools.ScriptTypes.validateScriptV2',
never silently ignored.
-}
module System.Agents.Tools.ScriptProcess (
    -- * Declaration
    ProcessMode (..),
    OutputFraming (..),
    InputFraming (..),
    ProcessOutput (..),
    ProcessInput (..),
    ProcessDigest (..),
    ProcessReady (..),
    ProcessBackground (..),
    ScriptProcess (..),
    defaultScriptProcess,
    defaultProcessOutput,

    -- * Events
    OutputEvent (..),
    outputEventToJSON,
    lineToEvent,
    frameOutput,
    renderOutput,

    -- * Stream results
    EventRing,
    emptyEventRing,
    pushEvent,
    ringEvents,
    ringElided,
    renderStreamResult,
    defaultStreamResultEvents,

    -- * Ready
    compileReadyMatch,
    readyLineMatches,

    -- * Event schemas
    validateAgainstSchema,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Maybe (catMaybes)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

-------------------------------------------------------------------------------
-- Declaration
-------------------------------------------------------------------------------

-- | @process.mode@.
data ProcessMode
    = -- | Run to completion; the output is the result (v1 behaviour, with framing).
      ModeOneshot
    | -- | The result is the sequence of output events until exit, bounded to the latest ones.
      ModeStream
    | -- | Outlives the call, addressed by a handle (not implemented yet).
      ModeService
    deriving (Show, Eq, Ord)

instance FromJSON ProcessMode where
    parseJSON = Aeson.withText "process.mode" $ \t -> case t of
        "oneshot" -> pure ModeOneshot
        "stream" -> pure ModeStream
        "service" -> pure ModeService
        _ -> fail ("invalid process.mode " <> show t <> ", allowed: oneshot, stream, service")

instance ToJSON ProcessMode where
    toJSON m = case m of
        ModeOneshot -> "oneshot"
        ModeStream -> "stream"
        ModeService -> "service"

-- | @process.output.framing@: how stdout is cut into events.
data OutputFraming
    = -- | One JSON object per line (the v2 default).
      FramingJsonl
    | -- | One text line per event.
      FramingLines
    | -- | Raw chunks as they arrive (binary-safe).
      FramingChunks
    deriving (Show, Eq, Ord)

instance FromJSON OutputFraming where
    parseJSON = Aeson.withText "process.output.framing" $ \t -> case t of
        "jsonl" -> pure FramingJsonl
        "lines" -> pure FramingLines
        "chunks" -> pure FramingChunks
        _ -> fail ("invalid process.output.framing " <> show t <> ", allowed: jsonl, lines, chunks")

instance ToJSON OutputFraming where
    toJSON f = case f of
        FramingJsonl -> "jsonl"
        FramingLines -> "lines"
        FramingChunks -> "chunks"

-- | @process.input.framing@: how the runtime writes events to stdin.
data InputFraming
    = InputJsonl
    | InputLines
    deriving (Show, Eq, Ord)

instance FromJSON InputFraming where
    parseJSON = Aeson.withText "process.input.framing" $ \t -> case t of
        "jsonl" -> pure InputJsonl
        "lines" -> pure InputLines
        _ -> fail ("invalid process.input.framing " <> show t <> ", allowed: jsonl, lines")

instance ToJSON InputFraming where
    toJSON f = case f of
        InputJsonl -> "jsonl"
        InputLines -> "lines"

-- | @process.output@.
data ProcessOutput
    = ProcessOutput
    { outputFraming :: OutputFraming
    , outputEvent :: Maybe Aeson.Value
    -- ^ JSON Schema of one event (applied under @jsonl@ framing).
    }
    deriving (Show, Eq, Ord)

defaultProcessOutput :: ProcessOutput
defaultProcessOutput = ProcessOutput FramingJsonl Nothing

instance FromJSON ProcessOutput where
    parseJSON = Aeson.withObject "process.output" $ \o ->
        ProcessOutput
            <$> o .:? "framing" .!= FramingJsonl
            <*> o .:? "event"

instance ToJSON ProcessOutput where
    toJSON p =
        Aeson.object $
            ["framing" .= p.outputFraming]
                <> maybe [] (\e -> ["event" .= e]) p.outputEvent

-- | @process.input@.
data ProcessInput
    = ProcessInput
    { inputFraming :: InputFraming
    , inputEvent :: Maybe Aeson.Value
    }
    deriving (Show, Eq, Ord)

instance FromJSON ProcessInput where
    parseJSON = Aeson.withObject "process.input" $ \o ->
        ProcessInput
            <$> o .:? "framing" .!= InputJsonl
            <*> o .:? "event"

instance ToJSON ProcessInput where
    toJSON p =
        Aeson.object $
            ["framing" .= p.inputFraming]
                <> maybe [] (\e -> ["event" .= e]) p.inputEvent

-- | @process.digest@ (for a @service@).
data ProcessDigest
    = DigestPerRound
    | DigestPerEvents Int
    | DigestPerSeconds Int
    deriving (Show, Eq, Ord)

instance FromJSON ProcessDigest where
    parseJSON = Aeson.withObject "process.digest" $ \o -> do
        per <- o .: "per"
        case per :: Text of
            "round" -> pure DigestPerRound
            "events" -> DigestPerEvents <$> o .: "n"
            "seconds" -> DigestPerSeconds <$> o .: "n"
            _ -> fail ("invalid process.digest.per " <> show per <> ", allowed: round, events, seconds")

instance ToJSON ProcessDigest where
    toJSON d = case d of
        DigestPerRound -> Aeson.object ["per" .= ("round" :: Text)]
        DigestPerEvents n -> Aeson.object ["per" .= ("events" :: Text), "n" .= n]
        DigestPerSeconds n -> Aeson.object ["per" .= ("seconds" :: Text), "n" .= n]

-- | @process.ready@: when a started process counts as up.
data ProcessReady
    = ReadyLine Text
    | ReadyExit
    | ReadyImmediate
    deriving (Show, Eq, Ord)

instance FromJSON ProcessReady where
    parseJSON = Aeson.withObject "process.ready" $ \o -> do
        kind <- o .: "kind"
        case kind :: Text of
            "line" -> ReadyLine <$> o .: "match"
            "exit" -> pure ReadyExit
            "immediate" -> pure ReadyImmediate
            _ -> fail ("invalid process.ready.kind " <> show kind <> ", allowed: line, exit, immediate")

instance ToJSON ProcessReady where
    toJSON r = case r of
        ReadyLine m -> Aeson.object ["kind" .= ("line" :: Text), "match" .= m]
        ReadyExit -> Aeson.object ["kind" .= ("exit" :: Text)]
        ReadyImmediate -> Aeson.object ["kind" .= ("immediate" :: Text)]

{- | @process.background@: the tool asks for its calls to run in the
background, as an asynchronous call (@RunAsync@), unless the agent decides
otherwise. @true@ detaches per the agent's yield strategy;
@{"attachSeconds": n}@ waits up to @n@ seconds for the call to finish
before detaching it; @false@, like no field at all, declares nothing. How
the declaration and the agent's @toolCallPolicyConfig@ combine is
'System.Agents.Session.AgentConfig.buildToolCallPolicyWith'.
-}
newtype ProcessBackground = ProcessBackground
    { backgroundAttachSeconds :: Maybe Int
    -- ^ The @attachSeconds@ of the resulting @RunAsync@ disposition.
    }
    deriving (Show, Eq, Ord)

instance FromJSON ProcessBackground where
    parseJSON = Aeson.withObject "process.background" $ \o ->
        ProcessBackground <$> o .:? "attachSeconds"

instance ToJSON ProcessBackground where
    toJSON b = case b.backgroundAttachSeconds of
        Nothing -> Aeson.Bool True
        Just n -> Aeson.object ["attachSeconds" .= n]

-- | Parses @process.background@: a boolean or an object ('ProcessBackground').
parseBackground :: Aeson.Value -> Aeson.Parser (Maybe ProcessBackground)
parseBackground v = case v of
    Aeson.Bool True -> pure (Just (ProcessBackground Nothing))
    Aeson.Bool False -> pure Nothing
    Aeson.Null -> pure Nothing
    Aeson.Object _ -> Just <$> parseJSON v
    _ -> fail "invalid process.background, allowed: true, false, {\"attachSeconds\": n}"

-- | The @process@ object of a v2 @describe@.
data ScriptProcess
    = ScriptProcess
    { processMode :: ProcessMode
    , processOutput :: ProcessOutput
    , processInput :: Maybe ProcessInput
    , processDigest :: Maybe ProcessDigest
    , processReady :: Maybe ProcessReady
    , processBackground :: Maybe ProcessBackground
    -- ^ @process.background@: whether the tool asks for its calls to run in the background.
    }
    deriving (Show, Eq, Ord)

-- | What a v2 tool without a @process@ object gets: a @oneshot@ with @jsonl@ output.
defaultScriptProcess :: ScriptProcess
defaultScriptProcess = ScriptProcess ModeOneshot defaultProcessOutput Nothing Nothing Nothing Nothing

instance FromJSON ScriptProcess where
    parseJSON = Aeson.withObject "process" $ \o ->
        ScriptProcess
            <$> o .:? "mode" .!= ModeOneshot
            <*> o .:? "output" .!= defaultProcessOutput
            <*> o .:? "input"
            <*> o .:? "digest"
            <*> o .:? "ready"
            <*> (maybe (pure Nothing) parseBackground =<< o .:? "background")

instance ToJSON ScriptProcess where
    toJSON p =
        Aeson.object $
            [ "mode" .= p.processMode
            , "output" .= p.processOutput
            ]
                <> maybe [] (\i -> ["input" .= i]) p.processInput
                <> maybe [] (\d -> ["digest" .= d]) p.processDigest
                <> maybe [] (\r -> ["ready" .= r]) p.processReady
                <> maybe [] (\b -> ["background" .= b]) p.processBackground

-------------------------------------------------------------------------------
-- Events
-------------------------------------------------------------------------------

{- | One output event. An event that does not parse or does not match the
declared schema is kept as 'InvalidEvent' with its raw text (D4: never
dropped silently).
-}
data OutputEvent
    = OutputEvent Aeson.Value
    | InvalidEvent Text String
    deriving (Show, Eq)

{- | The JSON form of an event, in the result and in progress payloads: the
event itself, or @{"invalid": {"raw": ..., "error": ...}}@.
-}
outputEventToJSON :: OutputEvent -> Aeson.Value
outputEventToJSON ev = case ev of
    OutputEvent v -> v
    InvalidEvent raw err -> Aeson.object ["invalid" .= Aeson.object ["raw" .= raw, "error" .= err]]

{- | Turns one line of stdout into an event, for the given output
declaration. Under @jsonl@ the line must be JSON and match the schema, if
any; under @lines@ and @chunks@ the event is the line as a string.
-}
lineToEvent :: ProcessOutput -> ByteString -> OutputEvent
lineToEvent out line =
    case out.outputFraming of
        FramingJsonl -> case Aeson.eitherDecodeStrict line of
            Left err -> InvalidEvent raw ("not a JSON value: " <> err)
            Right v -> case maybe [] (`validateAgainstSchema` v) out.outputEvent of
                [] -> OutputEvent v
                problems -> InvalidEvent raw ("does not match the declared event schema: " <> List.intercalate "; " problems)
        _ -> OutputEvent (Aeson.String raw)
  where
    raw = Text.decodeUtf8With lenientDecode line

{- | Cuts a complete stdout into events: one per non-empty line (a trailing
carriage return is dropped). Under @chunks@ framing there is a single event,
the whole output.
-}
frameOutput :: ProcessOutput -> ByteString -> [OutputEvent]
frameOutput out bytes =
    case out.outputFraming of
        FramingChunks
            | ByteString.null bytes -> []
            | otherwise -> [OutputEvent (Aeson.String (Text.decodeUtf8With lenientDecode bytes))]
        _ ->
            [ lineToEvent out l
            | l0 <- ByteStringChar8.lines bytes
            , let l = stripCR l0
            , not (ByteString.null (ByteStringChar8.strip l))
            ]
  where
    stripCR l = case ByteStringChar8.unsnoc l of
        Just (l', '\r') -> l'
        _ -> l

{- | The result a @oneshot@ v2 tool delivers to the model (§2.1): under
@jsonl@ the events as a JSON array; under @lines@ the non-empty lines joined
by newlines; under @chunks@ the raw output unchanged.
-}
renderOutput :: ProcessOutput -> ByteString -> ByteString
renderOutput out bytes =
    case out.outputFraming of
        FramingChunks -> bytes
        FramingLines ->
            ByteStringChar8.intercalate "\n" (catMaybes [eventLine e | e <- frameOutput out bytes])
        FramingJsonl ->
            LByteString.toStrict (Aeson.encode (fmap outputEventToJSON (frameOutput out bytes)))
  where
    eventLine (OutputEvent (Aeson.String t)) = Just (Text.encodeUtf8 t)
    eventLine (InvalidEvent raw _) = Just (Text.encodeUtf8 raw)
    eventLine _ = Nothing

-------------------------------------------------------------------------------
-- Stream results
-------------------------------------------------------------------------------

{- | The latest events of a @stream@ tool (§2.2), at most a fixed number, and
how many were seen in all. A long-running tool never has its whole output
held in memory: only the ring is.
-}
data EventRing = EventRing
    { ringLimit :: !Int
    , ringSeen :: !Int
    , ringHeld :: !(Seq OutputEvent)
    }
    deriving (Show, Eq)

-- | How many events a @stream@ result keeps when the agent does not say.
defaultStreamResultEvents :: Int
defaultStreamResultEvents = 200

-- | An empty ring keeping at most the given number of events (at least one).
emptyEventRing :: Int -> EventRing
emptyEventRing n = EventRing (max 1 n) 0 Seq.empty

-- | Adds an event, dropping the oldest one when the ring is full.
pushEvent :: OutputEvent -> EventRing -> EventRing
pushEvent ev r =
    let held = r.ringHeld |> ev
        trimmed = if Seq.length held > r.ringLimit then Seq.drop (Seq.length held - r.ringLimit) held else held
     in r{ringSeen = r.ringSeen + 1, ringHeld = trimmed}

-- | The events the ring holds, oldest first.
ringEvents :: EventRing -> [OutputEvent]
ringEvents r = toList r.ringHeld

-- | How many events were dropped to stay within the limit.
ringElided :: EventRing -> Int
ringElided r = r.ringSeen - Seq.length r.ringHeld

{- | The result a @stream@ tool delivers to the model (§2.2): the latest
events and the count of the earlier ones it no longer holds. Under @jsonl@
an object @{"events": [...], "elided": n}@; under @lines@ the lines joined by
newlines, and under @chunks@ the chunks concatenated, both preceded by a
@[n earlier lines elided]@ line when some were dropped.
-}
renderStreamResult :: ProcessOutput -> EventRing -> ByteString
renderStreamResult out r =
    case out.outputFraming of
        FramingJsonl ->
            LByteString.toStrict $
                Aeson.encode $
                    Aeson.object
                        [ "events" .= fmap outputEventToJSON (ringEvents r)
                        , "elided" .= ringElided r
                        ]
        FramingLines -> elidedNote "lines" <> ByteStringChar8.intercalate "\n" (fmap eventBytes (ringEvents r))
        FramingChunks -> elidedNote "chunks" <> ByteString.concat (fmap eventBytes (ringEvents r))
  where
    elidedNote :: ByteString -> ByteString
    elidedNote what
        | ringElided r > 0 = "[" <> ByteStringChar8.pack (show (ringElided r)) <> " earlier " <> what <> " elided]\n"
        | otherwise = ""
    eventBytes ev = case ev of
        OutputEvent (Aeson.String t) -> Text.encodeUtf8 t
        OutputEvent v -> LByteString.toStrict (Aeson.encode v)
        InvalidEvent raw _ -> Text.encodeUtf8 raw

-------------------------------------------------------------------------------
-- Ready
-------------------------------------------------------------------------------

{- | Compiles the regular expression of a @ready@ line (POSIX extended
syntax), or says why it does not compile.
-}
compileReadyMatch :: Text -> Either String Regex
compileReadyMatch pat =
    case makeRegexM (Text.unpack pat) of
        Just re -> Right re
        Nothing -> Left ("process.ready.match " <> show pat <> " is not a valid regular expression")

-- | Whether a line of standard output matches a compiled @ready@ pattern.
readyLineMatches :: Regex -> ByteString -> Bool
readyLineMatches re line = matchTest re (Text.unpack (Text.decodeUtf8With lenientDecode line))

-------------------------------------------------------------------------------
-- Event schemas
-------------------------------------------------------------------------------

{- | Validates a value against a JSON Schema, returning the problems found
(empty when it matches).

A practical subset of JSON Schema, enough for event shapes: boolean schemas,
@type@ (a name or a list; @integer@ is a number without a fractional part),
@enum@, @const@, @properties@, @required@, @additionalProperties@ (boolean
or schema), @items@ (one schema), @minItems@, @maxItems@, @minLength@,
@maxLength@, @minimum@, @maximum@, @allOf@, @anyOf@, @oneOf@ and @not@.
Other keywords are ignored.
-}
validateAgainstSchema :: Aeson.Value -> Aeson.Value -> [String]
validateAgainstSchema = go "$"
  where
    go :: String -> Aeson.Value -> Aeson.Value -> [String]
    go path schema v = case schema of
        Aeson.Bool True -> []
        Aeson.Bool False -> [path <> ": no value is allowed here"]
        Aeson.Object s -> concatMap (\check -> check path s v) checks
        _ -> []

    checks :: [String -> Aeson.Object -> Aeson.Value -> [String]]
    checks =
        [ checkType
        , checkEnum
        , checkConst
        , checkObject
        , checkArray
        , checkString
        , checkNumber
        , checkCombinators
        ]

    field :: Text -> Aeson.Object -> Maybe Aeson.Value
    field k = KeyMap.lookup (Key.fromText k)

    checkType path s v = case field "type" s of
        Just (Aeson.String t) -> [path <> ": expected " <> Text.unpack t <> ", got " <> typeName v | not (hasType t v)]
        Just (Aeson.Array ts) ->
            let names = [t | Aeson.String t <- toList ts]
             in [ path <> ": expected one of " <> show names <> ", got " <> typeName v
                | not (any (`hasType` v) names)
                ]
        _ -> []

    checkEnum path s v = case field "enum" s of
        Just (Aeson.Array vs) -> [path <> ": not one of the allowed values" | v `notElem` toList vs]
        _ -> []

    checkConst path s v = case field "const" s of
        Just c -> [path <> ": not the expected constant" | c /= v]
        Nothing -> []

    checkObject path s v = case v of
        Aeson.Object o ->
            let props = case field "properties" s of
                    Just (Aeson.Object ps) -> ps
                    _ -> KeyMap.empty
                required = case field "required" s of
                    Just (Aeson.Array rs) -> [r | Aeson.String r <- toList rs]
                    _ -> []
                missing = [path <> ": missing required property " <> show r | r <- required, not (KeyMap.member (Key.fromText r) o)]
                inProps =
                    concat
                        [ go (path <> "." <> Key.toString k) sub x
                        | (k, x) <- KeyMap.toList o
                        , Just sub <- [KeyMap.lookup k props]
                        ]
                extra = [(k, x) | (k, x) <- KeyMap.toList o, not (KeyMap.member k props)]
                additional = case field "additionalProperties" s of
                    Just (Aeson.Bool False) -> [path <> ": unexpected property " <> show (Key.toText k) | (k, _) <- extra]
                    Just sub@(Aeson.Object _) -> concat [go (path <> "." <> Key.toString k) sub x | (k, x) <- extra]
                    _ -> []
             in missing <> inProps <> additional
        _ -> []

    checkArray path s v = case v of
        Aeson.Array xs ->
            let n = length xs
                items = case field "items" s of
                    Just sub -> concat [go (path <> "[" <> show i <> "]") sub x | (i, x) <- zip [0 :: Int ..] (toList xs)]
                    Nothing -> []
             in items
                    <> [path <> ": fewer than " <> show m <> " items" | Just m <- [intField "minItems" s], n < m]
                    <> [path <> ": more than " <> show m <> " items" | Just m <- [intField "maxItems" s], n > m]
        _ -> []

    checkString path s v = case v of
        Aeson.String t ->
            let n = Text.length t
             in [path <> ": shorter than " <> show m <> " characters" | Just m <- [intField "minLength" s], n < m]
                    <> [path <> ": longer than " <> show m <> " characters" | Just m <- [intField "maxLength" s], n > m]
        _ -> []

    checkNumber path s v = case v of
        Aeson.Number x ->
            [path <> ": below the minimum" | Just (Aeson.Number m) <- [field "minimum" s], x < m]
                <> [path <> ": above the maximum" | Just (Aeson.Number m) <- [field "maximum" s], x > m]
        _ -> []

    checkCombinators path s v =
        let subs k = case field k s of
                Just (Aeson.Array xs) -> toList xs
                _ -> []
            matching = [sub | sub <- subs "anyOf", null (go path sub v)]
            oneMatches = length [() | sub <- subs "oneOf", null (go path sub v)]
         in concat [go path sub v | sub <- subs "allOf"]
                <> [path <> ": matches none of anyOf" | not (null (subs "anyOf")), null matching]
                <> [path <> ": must match exactly one of oneOf, matches " <> show oneMatches | not (null (subs "oneOf")), oneMatches /= 1]
                <> [path <> ": matches the schema under not" | Just sub <- [field "not" s], null (go path sub v)]

    intField :: Text -> Aeson.Object -> Maybe Int
    intField k s = case field k s of
        Just (Aeson.Number x) -> Scientific.toBoundedInteger x
        _ -> Nothing

    hasType :: Text -> Aeson.Value -> Bool
    hasType t v = case (t, v) of
        ("object", Aeson.Object _) -> True
        ("array", Aeson.Array _) -> True
        ("string", Aeson.String _) -> True
        ("number", Aeson.Number _) -> True
        ("integer", Aeson.Number x) -> Scientific.isInteger x
        ("boolean", Aeson.Bool _) -> True
        ("null", Aeson.Null) -> True
        _ -> False

    typeName :: Aeson.Value -> String
    typeName v = case v of
        Aeson.Object _ -> "object"
        Aeson.Array _ -> "array"
        Aeson.String _ -> "string"
        Aeson.Number _ -> "number"
        Aeson.Bool _ -> "boolean"
        Aeson.Null -> "null"
