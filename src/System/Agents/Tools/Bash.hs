{-# LANGUAGE DeriveGeneric #-}

-- | Defines tools as bash script (or programs) according to a simple convention.
module System.Agents.Tools.Bash where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Either (partitionEithers)
import Data.List as List
import Data.Maybe (maybeToList)
import Data.Text as Text
import Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.UUID (toString)
import GHC.Generics (Generic)
import Prod.Tracer (Tracer, runTracer)
import System.Directory (listDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files as Posix
import System.Process (CreateProcess (..), proc)
import System.Process.ByteString (readCreateProcessWithExitCode, readProcessWithExitCode)

import System.Agents.Base (AgentId (..), ConversationId (..))
import System.Agents.Session.Base (SessionId (..), TurnId (..))
import System.Agents.Tools.Context (ToolExecutionContext (..))

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

instance ToJSON ScriptEmptyResultBehavior

instance FromJSON ScriptEmptyResultBehavior

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
        Aeson.object $
            [ "args" .= s.scriptArgs
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

-------------------------------------------------------------------------------
-- Environment Variable Names for Session Context
-------------------------------------------------------------------------------

{- | Environment variable name for the session ID.
The value is the UUID of the current session.
-}
sessionIdEnvVar :: String
sessionIdEnvVar = "AGENT_SESSION_ID"

{- | Environment variable name for the conversation ID.
The value is the UUID of the conversation this session belongs to.
-}
conversationIdEnvVar :: String
conversationIdEnvVar = "AGENT_CONVERSATION_ID"

{- | Environment variable name for the turn ID.
The value is the UUID of the current turn within the session.
-}
turnIdEnvVar :: String
turnIdEnvVar = "AGENT_TURN_ID"

{- | Environment variable name for the agent ID.
The value is the UUID of the agent executing the tool, if available.
-}
agentIdEnvVar :: String
agentIdEnvVar = "AGENT_AGENT_ID"

{- | Environment variable name for the full session JSON.
When present, contains the complete serialized session as JSON.
This is only included when 'ctxFullSession' is 'Just' in the context.
-}
sessionJsonEnvVar :: String
sessionJsonEnvVar = "AGENT_SESSION_JSON"

-- | Convert a 'SessionId' to its string representation.
sessionIdToString :: SessionId -> String
sessionIdToString (SessionId uuid) = toString uuid

-- | Convert a 'ConversationId' to its string representation.
conversationIdToString :: ConversationId -> String
conversationIdToString (ConversationId uuid) = toString uuid

-- | Convert a 'TurnId' to its string representation.
turnIdToString :: TurnId -> String
turnIdToString (TurnId uuid) = toString uuid

-- | Convert an 'AgentId' to its string representation.
agentIdToString :: AgentId -> String
agentIdToString (AgentId uuid) = toString uuid

{- | Build the environment variable list for a tool execution context.
This adds session context variables to the base environment.
-}
buildToolEnvironment :: Maybe ToolExecutionContext -> [(String, String)] -> [(String, String)]
buildToolEnvironment Nothing baseEnv = baseEnv
buildToolEnvironment (Just ctx) baseEnv =
    baseEnv ++ contextVars
  where
    contextVars =
        [ (sessionIdEnvVar, sessionIdToString ctx.ctxSessionId)
        , (conversationIdEnvVar, conversationIdToString ctx.ctxConversationId)
        , (turnIdEnvVar, turnIdToString ctx.ctxTurnId)
        ]
            ++ maybeToList (fmap (\aid -> (agentIdEnvVar, agentIdToString aid)) ctx.ctxAgentId)
            ++ maybeToList (fmap (\sess -> (sessionJsonEnvVar, Text.unpack $ Text.decodeUtf8With lenientDecode $ LByteString.toStrict $ Aeson.encode sess)) ctx.ctxFullSession)

{- | Executes a script given an opaque JSON object containing parameter values.

When a 'ToolExecutionContext' is provided, the following environment variables
are set for the script:

* @AGENT_SESSION_ID@ - The UUID of the current session
* @AGENT_CONVERSATION_ID@ - The UUID of the conversation
* @AGENT_TURN_ID@ - The UUID of the current turn
* @AGENT_AGENT_ID@ - The UUID of the agent (if available)
* @AGENT_SESSION_JSON@ - The full serialized session as JSON (if requested)

Scripts can access these variables to correlate their actions with the
current execution context without needing explicit parameters.
-}
runValue ::
    Tracer IO RunTrace ->
    ScriptDescription ->
    Maybe ToolExecutionContext ->
    Aeson.Value ->
    IO (Either RunScriptError ByteString)
runValue tracer script mCtx val = do
    let path = script.scriptPath
    let maybeBehavior = script.scriptInfo.scriptEmptyResultBehavior
    case parseArgsForValue script val of
        Left err ->
            pure $ Left $ SerializeArgumentErrors path err
        Right (argz, stdin) -> do
            let args = "run" : [Text.unpack arg | arg <- argz]
            runTracer tracer (RunCommandStart path args)

            -- Get the current environment and add session context
            baseEnv <- getEnvironment
            let toolEnv = buildToolEnvironment mCtx baseEnv

            -- Create the process with the modified environment
            let process = (proc path args){env = Just toolEnv}

            (code, out, err) <- readCreateProcessWithExitCode process (Text.encodeUtf8 stdin)
            runTracer tracer (RunCommandStopped path args code out err)
            if code /= ExitSuccess
                then pure $ Left $ ScriptExecutionError path code err
                else
                    let -- Use lenient UTF-8 decoding to handle binary data safely.
                        -- Invalid bytes are replaced with U+FFFD (replacement character).
                        outText = Text.decodeUtf8With lenientDecode out
                        adjusted = case maybeBehavior of
                            Just behavior -> Text.encodeUtf8 $ adjustOutput behavior outText
                            Nothing -> out
                     in pure $ Right adjusted

