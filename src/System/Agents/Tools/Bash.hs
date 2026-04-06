{-# LANGUAGE DeriveGeneric #-}

-- | Defines tools as bash script (or programs) according to a simple convention.
module System.Agents.Tools.Bash (
    -- * Re-exports from ScriptTypes
    ScriptArg (..),
    ScriptArgArity (..),
    ScriptArgCallingMode (..),
    ScriptEmptyResultBehavior (..),
    ScriptInfo (..),
    translateArguments,

    -- * Bash-specific types and functions
    LoadTrace (..),
    RunTrace (..),
    Scripts (..),
    loadDirectory,
    loadScript,
    InvalidScriptError (..),
    RunScriptError (..),
    parseArgsForValue,
    runValue,

    -- * Environment and context
    sessionIdEnvVar,
    conversationIdEnvVar,
    turnIdEnvVar,
    agentIdEnvVar,
    sessionJsonEnvVar,
    sessionIdToString,
    conversationIdToString,
    turnIdToString,
    agentIdToString,
    buildToolEnvironment,

    -- * Re-exports
    ScriptDescription (..),
) where

import Control.Concurrent.Async (mapConcurrently)
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

-- Re-export shared types from ScriptTypes
import System.Agents.Tools.ScriptTypes (
    ScriptArg (..),
    ScriptArgArity (..),
    ScriptArgCallingMode (..),
    ScriptDescription (..),
    ScriptEmptyResultBehavior (..),
    ScriptInfo (..),
    translateArguments,
 )

-------------------------------------------------------------------------------
data LoadTrace
    = LoadCommandStart !FilePath [String]
    | LoadCommandStopped !FilePath [String] !ExitCode !ByteString !ByteString
    deriving (Show)

data RunTrace
    = RunCommandStart !FilePath [String]
    | RunCommandStopped !FilePath [String] !ExitCode !ByteString !ByteString
    deriving (Show)

-- helper function to adjust output
adjustOutput :: ScriptEmptyResultBehavior -> Text -> Text
adjustOutput behavior out = case behavior of
    DoNothing -> out
    AddMessage msg ->
        if out == ""
            then msg
            else out

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
            Right bashInfo -> pure $ Right $ ScriptDescription path bashInfo

data RunScriptError
    = SerializeArgumentErrors FilePath Aeson.Value String
    | ScriptExecutionError FilePath ExitCode ByteString
    deriving (Show)

-- | Flattens arguments associated with textual values into an array suitable to run as executable.
flattenArguments :: [(ScriptArg, Maybe Text)] -> [Text]
flattenArguments = mconcat . fmap flatten1
  where
    flatten1 :: (ScriptArg, Maybe Text) -> [Text]
    flatten1 (_, Nothing) = []
    flatten1 (arg, Just txt) =
        case arg.argCallingMode of
            Stdin -> []
            Positional -> [txt]
            DashDashEqual -> [mconcat ["--", arg.argName, "=", txt]]
            DashDashSpace -> [mconcat ["--", arg.argName], txt]

flattenInput :: [(ScriptArg, Maybe Text)] -> Text
flattenInput = Text.unlines . mconcat . fmap flatten1
  where
    flatten1 :: (ScriptArg, Maybe Text) -> [Text]
    flatten1 (_, Nothing) = []
    flatten1 (arg, Just txt) =
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
            pure $ Left $ SerializeArgumentErrors path val err
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
                    let
                        -- Use lenient UTF-8 decoding to handle binary data safely.
                        -- Invalid bytes are replaced with U+FFFD (replacement character).
                        outText = Text.decodeUtf8With lenientDecode out
                        adjusted = case maybeBehavior of
                            Just behavior -> Text.encodeUtf8 $ adjustOutput behavior outText
                            Nothing -> out
                     in
                        pure $ Right adjusted
