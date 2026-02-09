{-# LANGUAGE DeriveGeneric #-}
-- | Defines tools as bash script (or programs) according to a simple convention.
module System.Agents.Tools.Bash where

import GHC.Generics (Generic)
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Either (partitionEithers)
import Data.List as List
import Data.Text as Text
import Data.Text.Encoding as Text
import Prod.Tracer (Tracer, runTracer)
import System.Directory (listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files as Posix
import System.Process.ByteString (readProcessWithExitCode)

-------------------------------------------------------------------------------
data LoadTrace
    = LoadCommandStart !FilePath [String]
    | LoadCommandStopped !FilePath [String] !ExitCode !ByteString !ByteString
    deriving (Show)

data RunTrace
    = RunCommandStart !FilePath [String]
    | RunCommandStopped !FilePath [String] !ExitCode !ByteString !ByteString
    deriving (Show)

-------------------------------------------------------------------------------
data ScriptEmptyResultBehavior
    = DoNothing
    | AddMessage !Text
    deriving (Generic, Show, Eq, Ord)

instance ToJSON ScriptEmptyResultBehavior where

instance FromJSON ScriptEmptyResultBehavior where

-------------------------------------------------------------------------------
data ScriptArgArity
    = Single
    | Optional
    deriving (Show, Eq, Ord)

instance ToJSON ScriptArgArity where
    toJSON s = case s of
        Single -> Aeson.String "single"
        Optional -> Aeson.String "optional"

instance FromJSON ScriptArgArity where
    parseJSON v = case v of
        Aeson.String "single" -> pure Single
        Aeson.String "optional" -> pure Optional
        _ ->
            fail $
                List.unlines
                    [ "Invalid arity: " <> Prelude.show v
                    , "allowed values are:"
                    , "- single"
                    ]

data ScriptArgCallingMode
    = Stdin
    | Positional
    | DashDashSpace
    | DashDashEqual
    deriving (Show, Eq, Ord)

instance ToJSON ScriptArgCallingMode where
    toJSON s = case s of
        Stdin -> Aeson.String "stdin"
        Positional -> Aeson.String "positional"
        DashDashSpace -> Aeson.String "dashdashspace"
        DashDashEqual -> Aeson.String "dashdashequal"

instance FromJSON ScriptArgCallingMode where
    parseJSON v = case v of
        Aeson.String "stdin" -> pure Stdin
        Aeson.String "positional" -> pure Positional
        Aeson.String "dashdashspace" -> pure DashDashSpace
        Aeson.String "dashdashequal" -> pure DashDashEqual
        _ ->
            fail $
                List.unlines
                    [ "Invalid mode: " <> Prelude.show v
                    , "allowed values are:"
                    , "- positional"
                    , "- stdin"
                    , "- dashdashspace"
                    , "- dashdashequal"
                    ]

data ScriptArg
    = ScriptArg
    { argName :: Text
    , argDescription :: Text
    , argTypeString :: Text
    , argBackingTypeString :: Text
    , argTypeArity :: ScriptArgArity
    , argCallingMode :: ScriptArgCallingMode
    }
    deriving (Show, Eq, Ord)

instance ToJSON ScriptArg where
    toJSON s =
        Aeson.object
            [ "name" .= s.argName
            , "description" .= s.argDescription
            , "type" .= s.argTypeString
            , "backing_type" .= s.argBackingTypeString
            , "arity" .= s.argTypeArity
            , "mode" .= s.argCallingMode
            ]

instance FromJSON ScriptArg where
    parseJSON =
        Aeson.withObject "Arg" $ \v ->
            ScriptArg
                <$> v .: "name"
                <*> v .: "description"
                <*> v .: "type"
                <*> v .: "backing_type"
                <*> v .: "arity"
                <*> v .: "mode"

data ScriptInfo
    = ScriptInfo
    { scriptArgs :: [ScriptArg]
    , scriptSlug :: Text
    , scriptDescription :: Text
    , scriptEmptyResultBehavior :: Maybe ScriptEmptyResultBehavior
    }
    deriving (Show, Eq, Ord)

instance ToJSON ScriptInfo where
    toJSON s =
        Aeson.object
            $ [ "args" .= s.scriptArgs
              , "slug" .= s.scriptSlug
              , "description" .= s.scriptDescription
              ]
            <> maybe [] (\seb -> ["empty-result" .= seb]) s.scriptEmptyResultBehavior

instance FromJSON ScriptInfo where
    parseJSON =
        Aeson.withObject "Script" $ \v ->
            ScriptInfo
                <$> v .: "args"
                <*> v .: "slug"
                <*> v .: "description"
                <*> v Aeson..:? "empty-result"

-- helper function to adjust output
adjustOutput :: ScriptEmptyResultBehavior -> Text -> Text
adjustOutput behavior out = case behavior of
    DoNothing -> out
    AddMessage msg ->
     if out == ""
     then msg
     else out

data ScriptDescription
    = ScriptDescription
    { scriptPath :: FilePath
    , scriptInfo :: ScriptInfo
    }
    deriving (Show, Eq, Ord)

data Scripts
    = Scripts
    { scriptDir :: FilePath
    , scriptDescriptions :: [ScriptDescription]
    }
    deriving (Show, Eq, Ord)

loadDirectory :: Tracer IO LoadTrace -> FilePath -> IO (Scripts, [InvalidScriptError])
loadDirectory tracer path = do
    contents <- listDirectory path
    sources <- scriptSources contents
    loaded <- mapConcurrently (loadScript tracer) sources
    let (erroredScripts, loadedScripts) = partitionEithers loaded
    let ok = Scripts path loadedScripts
    let ko = erroredScripts
    pure (ok, ko)
  where
    scriptSources :: [FilePath] -> IO [FilePath]
    scriptSources xs = do
        let paths = fmap fullPath xs
        stats <- traverse isExecutableFile paths
        pure $ fmap fst $ List.filter snd $ List.zip paths stats

    isExecutableFile :: FilePath -> IO Bool
    isExecutableFile p = do
        a <- isExecutable p
        b <- isNormalFile p
        c <- isSymbolic p
        pure $ and [a, b || c]

    isExecutable :: FilePath -> IO Bool
    isExecutable p = Posix.fileAccess p True False True

    isNormalFile :: FilePath -> IO Bool
    isNormalFile p = Posix.isRegularFile <$> Posix.getFileStatus p

    isSymbolic :: FilePath -> IO Bool
    isSymbolic p = Posix.isSymbolicLink <$> Posix.getFileStatus p

    fullPath :: FilePath -> FilePath
    fullPath p = path </> p

data InvalidScriptError
    = InvalidScriptError FilePath ExitCode ByteString
    | InvalidDescriptionError FilePath String
    deriving (Show)

-- | Loads a single bash script complying with the describe|run protocol.
loadScript :: Tracer IO LoadTrace -> FilePath -> IO (Either InvalidScriptError ScriptDescription)
loadScript tracer path = do
    let args = ["describe"]
    runTracer tracer (LoadCommandStart path args)
    (code, out, err) <- readProcessWithExitCode path args ""
    runTracer tracer (LoadCommandStopped path args code out err)
    if code /= ExitSuccess
        then pure $ Left $ InvalidScriptError path code err
        else case Aeson.eitherDecode (LByteString.fromStrict out) of
            Left jsonErr -> pure $ Left $ InvalidDescriptionError path jsonErr
            Right info -> pure $ Right $ ScriptDescription path info

data RunScriptError
    = SerializeArgumentErrors FilePath String
    | ScriptExecutionError FilePath ExitCode ByteString
    deriving (Show)

-- | Translates arguments by collecting script arguments with their associated values.
translateArguments :: ScriptInfo -> Aeson.Value -> Aeson.Parser [(ScriptArg, Text)]
translateArguments script = Aeson.withObject "Args" $ \v -> do
    vals <- traverse (parseArg v) script.scriptArgs
    pure $ List.zip script.scriptArgs vals
  where
    parseArg :: Aeson.Object -> ScriptArg -> Aeson.Parser Text
    parseArg v arg = v .: (textToKey $ arg.argName)

    textToKey :: Text -> Aeson.Key
    textToKey = read . Prelude.show

-- | Flattens arguments associated with textual values into an array suitable to run as executable.
flattenArguments :: [(ScriptArg, Text)] -> [Text]
flattenArguments = mconcat . fmap flatten1
  where
    flatten1 :: (ScriptArg, Text) -> [Text]
    flatten1 (arg, txt) =
        case arg.argCallingMode of
            Stdin -> []
            Positional -> [txt]
            DashDashEqual -> [mconcat ["--", arg.argName, "=", txt]]
            DashDashSpace -> [mconcat ["--", arg.argName], txt]

flattenInput :: [(ScriptArg, Text)] -> Text
flattenInput = Text.unlines . mconcat . fmap flatten1
  where
    flatten1 :: (ScriptArg, Text) -> [Text]
    flatten1 (arg, txt) =
        case arg.argCallingMode of
            Stdin -> [txt]
            Positional -> []
            DashDashEqual -> []
            DashDashSpace -> []

-- | Tries parsing command line arguments from an opaque JSON object.
parseArgsForValue :: ScriptDescription -> Aeson.Value -> Either String ([Text], Text)
parseArgsForValue script val = do
    args <- Aeson.parseEither (translateArguments script.scriptInfo) val
    pure (flattenArguments args, flattenInput args)

-- | Executes a script given an opaque JSON object containing parameter values.
runValue :: Tracer IO RunTrace -> ScriptDescription -> Aeson.Value -> IO (Either RunScriptError ByteString)
runValue tracer script val = do
    let path = script.scriptPath
    let maybeBehavior = script.scriptInfo.scriptEmptyResultBehavior
    case parseArgsForValue script val of
        Left err ->
            pure $ Left $ SerializeArgumentErrors path err
        Right (argz, stdin) -> do
            let args = "run" : [Text.unpack arg | arg <- argz]
            runTracer tracer (RunCommandStart path args)
            (code, out, err) <- readProcessWithExitCode path args (Text.encodeUtf8 stdin)
            runTracer tracer (RunCommandStopped path args code out err)
            if code /= ExitSuccess
                then pure $ Left $ ScriptExecutionError path code err
                else
                    let outText = Text.decodeUtf8 out
                        adjusted = case maybeBehavior of
                            Just behavior -> Text.encodeUtf8 $ adjustOutput behavior outText
                            Nothing       -> out
                    in pure $ Right adjusted
