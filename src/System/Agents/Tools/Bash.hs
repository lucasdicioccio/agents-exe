{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines tools as bash script (or programs) according to a simple convention.
module System.Agents.Tools.Bash (
    -- * Re-exports from ScriptTypes
    ScriptArg (..),
    ScriptArgArity (..),
    ScriptArgCallingMode (..),
    ScriptEmptyResultBehavior (..),
    ScriptInfo (..),
    ScriptCommand (..),
    translateArguments,
    expandScriptCommands,

    -- * Bash-specific types and functions
    LoadTrace (..),
    RunTrace (..),
    Scripts (..),
    loadDirectory,
    loadScript,
    loadScriptWith,
    loadScriptTools,
    selectSubcommand,
    InvalidScriptError (..),
    RunScriptError (..),
    parseArgsForValue,
    parseEnvForValue,
    runValue,
    runValueWith,
    RunOptions (..),
    defaultRunOptions,
    runValueWithOptions,

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
    runProcessReportingOutput,
    runProcessReportingEvents,
    runProcessReportingEventsStopping,
    notifyLinePrefix,

    -- * Re-exports
    ScriptDescription (..),
) where

import Control.Concurrent.Async (concurrently, mapConcurrently, race, waitCatch, withAsync, cancel, wait)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, readMVar, tryPutMVar)
import Control.Exception (IOException, handle, onException)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.IO (Handle, hClose)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
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
import System.Posix.Signals (Signal, signalProcessGroup, sigKILL, sigTERM)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), getPid, proc, waitForProcess, withCreateProcess)
import System.Timeout (timeout)
import System.Process.ByteString (readCreateProcessWithExitCode, readProcessWithExitCode)

import System.Agents.Base (AgentId (..), ConversationId (..))
import System.Agents.Session.Base (SessionId (..), TurnId (..), notifyProgress)
import System.Agents.Tools.Context (ToolExecutionContext (..))

-- Re-export shared types from ScriptTypes
import System.Agents.Tools.ScriptTypes (
    ScriptArg (..),
    ScriptArgArity (..),
    ScriptArgCallingMode (..),
    ScriptCommand (..),
    ScriptDescription (..),
    ScriptEmptyResultBehavior (..),
    ScriptInfo (..),
    argEnvVarName,
    expandScriptCommands,
    scriptProcessOf,
    scriptRuntimeOf,
    translateArguments,
    validateScriptCommands,
    validateScriptV2,
 )
import System.Agents.Tools.ScriptProcess (
    OutputEvent (..),
    OutputFraming (..),
    ProcessMode (..),
    ProcessOutput (..),
    ProcessReady (..),
    ScriptProcess (..),
    compileReadyMatch,
    defaultStreamResultEvents,
    emptyEventRing,
    lineToEvent,
    outputEventToJSON,
    pushEvent,
    readyLineMatches,
    renderOutput,
    renderStreamResult,
    ringEvents,
 )
import System.Agents.Tools.ScriptRuntime (
    Launch (..),
    RuntimeTimeout (..),
    ScriptRuntime (..),
    ToolRuntimeConfig,
    cancelSignal,
    getToolRuntimeConfig,
    prepareLaunch,
    resolveSandbox,
    startTimeoutMicros,
    stopGraceMicros,
 )

-------------------------------------------------------------------------------

{- | Run a process like 'readCreateProcessWithExitCode', reporting output as
it arrives.

Progress payloads look like
@{"stream": "stdout", "line": "<latest complete line>", "lines": n, "bytes": n}@
and are sent at most every 'outputReportInterval' per stream.

A line of standard error that starts with 'notifyLinePrefix'
(@::notify:: tests are red, stopping early@) is how a script asks for the
model's attention while it runs: the rest of the line is reported as a
notify-level payload ('notifyProgress', with @"stream": "stderr"@), every
time, whatever the interval. The line stays in the captured standard error.

The script runs in its own process group, so cancelling the calling thread
(e.g. through @cancel-tool-call@) kills the script *and* the processes it
started, not just the script itself.
-}
runProcessReportingOutput ::
    (Aeson.Value -> IO ()) ->
    CreateProcess ->
    ByteString ->
    IO (ExitCode, ByteString, ByteString)
runProcessReportingOutput report = runProcessReportingEvents report Nothing

{- | Like 'runProcessReportingOutput', but when given a stdout line handler
(the v2 framing, @todos/bash-tools-v2.md@ §2.1), every complete line of
standard output (and a last unterminated one) goes to it, as it arrives and
without throttling, instead of the throttled stdout line report. Standard
error is reported as in 'runProcessReportingOutput'.
-}
runProcessReportingEvents ::
    (Aeson.Value -> IO ()) ->
    Maybe (ByteString -> IO ()) ->
    CreateProcess ->
    ByteString ->
    IO (ExitCode, ByteString, ByteString)
runProcessReportingEvents = runProcessReportingEventsStopping (sigTERM, killGraceMicros)

{- | Like 'runProcessReportingEvents', with the signal sent first when the
calling thread is cancelled and the grace before @SIGKILL@ (a tool's
@runtime.cancel@ and @runtime.timeout.stop@).
-}
runProcessReportingEventsStopping ::
    (Signal, Int) ->
    (Aeson.Value -> IO ()) ->
    Maybe (ByteString -> IO ()) ->
    CreateProcess ->
    ByteString ->
    IO (ExitCode, ByteString, ByteString)
runProcessReportingEventsStopping stopping report mStdoutLine =
    runProcessHandling stopping report (keepAllOutput{stdoutLine = mStdoutLine})

{- | What the runner does with a process's output, beyond reporting it.
A @oneshot@ tool keeps all of it (it is the result); a @stream@ tool keeps
none of its standard output (its events go to the handlers, the result is
built from them) and only the tail of its standard error, so a tool that
runs for hours does not fill memory.
-}
data OutputHandling = OutputHandling
    { stdoutLine :: Maybe (ByteString -> IO ())
    -- ^ Each complete line of standard output (and a last unterminated one).
    , stdoutChunk :: Maybe (ByteString -> IO ())
    -- ^ Each chunk of standard output, as read (takes precedence over 'stdoutLine').
    , keepStdout :: Bool
    -- ^ Whether the standard output is returned ('False': it comes back empty).
    , stderrTail :: Maybe Int
    -- ^ Keep only the last that many bytes of standard error.
    }

keepAllOutput :: OutputHandling
keepAllOutput = OutputHandling Nothing Nothing True Nothing

-- | How much standard error a @stream@ tool keeps for its error report.
streamStderrTailBytes :: Int
streamStderrTailBytes = 65536

{- | The process runner behind 'runProcessReportingEvents', with the stop
policy of 'runProcessReportingEventsStopping' and an 'OutputHandling'.
-}
runProcessHandling ::
    (Signal, Int) ->
    (Aeson.Value -> IO ()) ->
    OutputHandling ->
    CreateProcess ->
    ByteString ->
    IO (ExitCode, ByteString, ByteString)
runProcessHandling stopping report handling process input =
    withCreateProcess process{std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True} $ \mIn mOut mErr ph ->
        case (mIn, mOut, mErr) of
            (Just hIn, Just hOut, Just hErr) -> do
                let feed = ignoreBrokenPipe (ByteString.hPut hIn input) >> ignoreBrokenPipe (hClose hIn)
                let body = do
                        ((out, err), ()) <-
                            concurrently
                                (concurrently (drain "stdout" hOut) (drain "stderr" hErr))
                                feed
                        code <- waitForProcess ph
                        pure (code, out, err)
                body `onException` stopProcessGroup stopping ph
            _ -> error "runProcessReportingEvents: process pipes were not created"
  where
    mStdoutLine = handling.stdoutLine

    ignoreBrokenPipe :: IO () -> IO ()
    ignoreBrokenPipe = handle (\(_ :: IOException) -> pure ())

    drain :: Text -> Handle -> IO ByteString
    drain stream h = do
        lastReport <- newMVar (Nothing :: Maybe UTCTime)
        -- chunks: everything read so far (reversed); pending: text after the
        -- last newline, not yet a complete line.
        let isStdout = stream == "stdout"
            keep chunk chunks
                | isStdout && not handling.keepStdout = chunks
                | not isStdout, Just n <- handling.stderrTail =
                    -- a single, trimmed chunk: the tail of everything so far
                    let joined = ByteString.concat (List.reverse (chunk : chunks))
                     in [ByteString.drop (ByteString.length joined - n) joined]
                | otherwise = chunk : chunks
            onChunk = if isStdout then handling.stdoutChunk else Nothing
        let loop chunks pending lineCount byteCount = do
                chunk <- ByteString.hGetSome h 32768
                if ByteString.null chunk
                    then do
                        case (stream, mStdoutLine, onChunk) of
                            ("stdout", Just onLine, Nothing) | not (ByteString.null pending) -> onLine pending
                            _ -> pure ()
                        pure (ByteString.concat (List.reverse chunks))
                    else do
                        forM_ onChunk ($ chunk)
                        -- With a chunk handler, lines are not tracked, so
                        -- output without newlines is not buffered either.
                        let buffered = case onChunk of
                                Just _ -> ByteString.empty
                                Nothing -> pending <> chunk
                            byteCount' = byteCount + ByteString.length chunk
                        case ByteStringChar8.elemIndexEnd '\n' buffered of
                            Nothing -> loop (keep chunk chunks) buffered lineCount byteCount'
                            Just ix -> do
                                let (complete, rest) = ByteString.splitAt (ix + 1) buffered
                                    completeLines = ByteStringChar8.lines complete
                                    lineCount' = lineCount + List.length completeLines
                                    nonEmpty = List.filter (not . ByteString.null) completeLines
                                when (stream == "stderr") $
                                    forM_ completeLines $ \line ->
                                        forM_ (ByteString.stripPrefix notifyLinePrefix line) $ \notice ->
                                            report (notifyLinePayload notice)
                                case (stream, mStdoutLine, onChunk) of
                                    ("stdout", _, Just _) -> pure ()
                                    ("stdout", Just onLine, _) -> forM_ completeLines onLine
                                    _ ->
                                        unless (List.null nonEmpty) $
                                            reportLine lastReport stream (List.last nonEmpty) lineCount' byteCount'
                                loop (keep chunk chunks) rest lineCount' byteCount'
        loop [] ByteString.empty (0 :: Int) (0 :: Int)

    reportLine lastReport stream line lineCount byteCount = do
        now <- getCurrentTime
        modifyMVar_ lastReport $ \prev ->
            if maybe True (\t -> diffUTCTime now t >= outputReportInterval) prev
                then do
                    report $
                        Aeson.object
                            [ "stream" Aeson..= stream
                            , "line" Aeson..= Text.decodeUtf8With lenientDecode line
                            , "lines" Aeson..= lineCount
                            , "bytes" Aeson..= byteCount
                            ]
                    pure (Just now)
                else pure prev

{- | Signal the whole process group of a running process, so that children
started by a script die with it. Best-effort: signalling failures (the
process is already gone, or has no group) are ignored.
-}
stopProcessGroup :: (Signal, Int) -> ProcessHandle -> IO ()
stopProcessGroup (firstSignal, graceMicros) ph = do
    mPid <- getPid ph
    case mPid of
        Nothing -> pure ()
        Just pid -> do
            ignoreSignalError $ signalProcessGroup firstSignal pid
            threadDelay graceMicros
            ignoreSignalError $ signalProcessGroup sigKILL pid
  where
    ignoreSignalError :: IO () -> IO ()
    ignoreSignalError = handle (\(_ :: IOException) -> pure ())

-- | Grace period between SIGTERM and SIGKILL for a cancelled script (unless its @runtime.timeout.stop@ says otherwise).
killGraceMicros :: Int
killGraceMicros = 100000

-- | What a line of standard error starts with to be reported at the notify level.
notifyLinePrefix :: ByteString
notifyLinePrefix = "::notify::"

-- | The notify-level payload for the rest of such a line.
notifyLinePayload :: ByteString -> Aeson.Value
notifyLinePayload rest =
    case notifyProgress (Text.strip (Text.decodeUtf8With lenientDecode rest)) of
        Aeson.Object obj -> Aeson.Object (KeyMap.insert "stream" (Aeson.String "stderr") obj)
        other -> other

-- | Minimum delay between two output reports for one stream.
outputReportInterval :: NominalDiffTime
outputReportInterval = 0.5

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
    loaded <- mapConcurrently (loadScriptTools tracer) sources
    let (erroredScripts, loadedScripts) = partitionEithers loaded
    let ok = Scripts path (mconcat loadedScripts)
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

{- | Loads a single bash script complying with the describe|run protocol.

The result is the script's description as it declared it: a script with
subcommands is one description here, see 'loadScriptTools' for the tools it
exposes.
-}
loadScript :: Tracer IO LoadTrace -> FilePath -> IO (Either InvalidScriptError ScriptDescription)
loadScript tracer path = do
    cfg <- getToolRuntimeConfig
    loadScriptWith cfg tracer path

{- | 'loadScript' against an explicit operator configuration rather than the
process-wide one: a v2 @runtime@ the configuration cannot honour fails to
load.
-}
loadScriptWith :: ToolRuntimeConfig -> Tracer IO LoadTrace -> FilePath -> IO (Either InvalidScriptError ScriptDescription)
loadScriptWith cfg tracer path = do
    let args = ["describe"]
    runTracer tracer (LoadCommandStart path args)
    (code, out, err) <- readProcessWithExitCode path args ""
    runTracer tracer (LoadCommandStopped path args code out err)
    if code /= ExitSuccess
        then pure $ Left $ InvalidScriptError path code err
        else case Aeson.eitherDecode (LByteString.fromStrict out) of
            Left jsonErr -> pure $ Left $ InvalidDescriptionError path jsonErr
            Right bashInfo -> case validateScriptCommands bashInfo <> validateScriptV2 cfg bashInfo of
                [] -> do
                    -- Which sandbox mechanism enforces the tool can depend on
                    -- the host (--sandbox landlock|auto): checked here too, so
                    -- a tool that could never run fails to load.
                    sandboxProblem <- case scriptRuntimeOf bashInfo >>= (.runtimeSandbox) of
                        Nothing -> pure Nothing
                        Just sb -> either Just (const Nothing) <$> resolveSandbox cfg path sb
                    pure $ case sandboxProblem of
                        Nothing -> Right $ ScriptDescription path bashInfo{scriptSubcommand = Nothing}
                        Just problem -> Left $ InvalidDescriptionError path problem
                problems -> pure $ Left $ InvalidDescriptionError path (List.intercalate "; " problems)

{- | Loads a single bash script and returns the tools it exposes: itself, or
one tool per subcommand when its @describe@ lists @commands@
(see 'expandScriptCommands').
-}
loadScriptTools :: Tracer IO LoadTrace -> FilePath -> IO (Either InvalidScriptError [ScriptDescription])
loadScriptTools tracer path = fmap expandScriptCommands <$> loadScript tracer path

{- | Picks the tool of a loaded script to work with (for the CLI helpers that
take a script path): with no subcommand asked, the script itself when it has
no subcommands; otherwise the named subcommand.
-}
selectSubcommand :: Maybe Text -> ScriptDescription -> Either String ScriptDescription
selectSubcommand mName desc =
    case (mName, expandScriptCommands desc) of
        (Nothing, [single]) | List.null desc.scriptInfo.scriptCommands -> Right single
        (Nothing, _) -> Left ("this tool has subcommands, pick one of: " <> names)
        (Just n, tools) -> case List.find (\t -> t.scriptInfo.scriptSubcommand == Just n) tools of
            Just t -> Right t
            Nothing
                | List.null desc.scriptInfo.scriptCommands -> Left "this tool has no subcommands"
                | otherwise -> Left ("unknown subcommand " <> Prelude.show n <> ", pick one of: " <> names)
  where
    names = List.intercalate ", " [Text.unpack c.commandName | c <- desc.scriptInfo.scriptCommands]

data RunScriptError
    = SerializeArgumentErrors FilePath Aeson.Value String
    | ScriptExecutionError FilePath ExitCode ByteString
    | -- | The v2 @runtime@ could not be set up (envdir, rundir, run_as, sandbox).
      ScriptRuntimeError FilePath String
    | -- | A v2 tool did not complete within its @runtime.timeout.start@ (seconds); it was stopped.
      ScriptTimeoutError FilePath Double
    | {- | A @stream@ tool exited with a failure: its exit code, the tail of its
      standard error, and its result so far (the latest events, rendered).
      -}
      ScriptStreamExitError FilePath ExitCode ByteString ByteString
    | {- | A @service@ tool, its @send@ companion or the @processes@ tool
      refused the call (unknown handle, process not running, an event that
      does not match the declared input, an exit before @ready@...).
      -}
      ScriptServiceError FilePath String
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
            Env -> []
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
            Env -> []
            Positional -> []
            DashDashEqual -> []
            DashDashSpace -> []

{- | Flattens 'Env'-mode arguments into environment variable pairs. The
variable name is the argument name, upper-cased with @-@ turned into @_@
(see 'argEnvVarName').
-}
flattenEnv :: [(ScriptArg, Maybe Text)] -> [(String, String)]
flattenEnv = mconcat . fmap flatten1
  where
    flatten1 :: (ScriptArg, Maybe Text) -> [(String, String)]
    flatten1 (_, Nothing) = []
    flatten1 (arg, Just txt) =
        case arg.argCallingMode of
            Env -> [(Text.unpack (argEnvVarName arg.argName), Text.unpack txt)]
            _ -> []

-- | Tries parsing command line arguments from an opaque JSON object.
parseArgsForValue :: ScriptDescription -> Aeson.Value -> Either String ([Text], Text)
parseArgsForValue script val = do
    args <- Aeson.parseEither (translateArguments script.scriptInfo) val
    pure (flattenArguments args, flattenInput args)

-- | Tries parsing environment-mode arguments from an opaque JSON object.
parseEnvForValue :: ScriptDescription -> Aeson.Value -> Either String [(String, String)]
parseEnvForValue script val = do
    args <- Aeson.parseEither (translateArguments script.scriptInfo) val
    pure (flattenEnv args)

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
    cfg <- getToolRuntimeConfig
    runValueWith cfg tracer script mCtx val

{- | Per-agent options of a bash tool call (as opposed to the operator's
'ToolRuntimeConfig').
-}
newtype RunOptions = RunOptions
    { runStreamResultEvents :: Int
    -- ^ How many of the latest events a @stream@ tool's result keeps (agent @streamResultEvents@).
    }
    deriving (Show, Eq)

defaultRunOptions :: RunOptions
defaultRunOptions = RunOptions defaultStreamResultEvents

{- | 'runValue' against an explicit operator configuration rather than the
process-wide one. A v2 tool's @runtime@ (@todos/bash-tools-v2.md@ §1.2) is
applied here: envdir and @environment@, rundir, run_as and sandbox
(see 'prepareLaunch'), @timeout.start@ as the call's deadline, and
@cancel@/@timeout.stop@ when the call is cancelled or times out.
-}
runValueWith ::
    ToolRuntimeConfig ->
    Tracer IO RunTrace ->
    ScriptDescription ->
    Maybe ToolExecutionContext ->
    Aeson.Value ->
    IO (Either RunScriptError ByteString)
runValueWith = runValueWithOptions defaultRunOptions

-- | 'runValueWith' with the agent's 'RunOptions'.
runValueWithOptions ::
    RunOptions ->
    ToolRuntimeConfig ->
    Tracer IO RunTrace ->
    ScriptDescription ->
    Maybe ToolExecutionContext ->
    Aeson.Value ->
    IO (Either RunScriptError ByteString)
runValueWithOptions opts cfg tracer script mCtx val = do
    let path = script.scriptPath
    let maybeBehavior = script.scriptInfo.scriptEmptyResultBehavior
    case parseArgsForValue script val of
        Left err ->
            pure $ Left $ SerializeArgumentErrors path val err
        Right (argz, stdin) -> do
            -- A subcommand tool (see 'expandScriptCommands') is run as
            -- @run <subcommand> <args...>@.
            let subcommand = maybeToList script.scriptInfo.scriptSubcommand
            let args = "run" : [Text.unpack arg | arg <- subcommand <> argz]
            let envArgs = either (const []) id (parseEnvForValue script val)
            runTracer tracer (RunCommandStart path args)

            -- Get the current environment and add session context
            baseEnv <- getEnvironment
            let input = Text.encodeUtf8 stdin
            let mProcess = scriptProcessOf script.scriptInfo
            case mProcess of
              Just p | p.processMode == ModeService ->
                pure (Left (ScriptServiceError path "a service tool is started through its tool registration (System.Agents.Tools.ScriptService.startService), not run to completion"))
              Just p | p.processMode == ModeStream ->
                runStreamValue opts cfg tracer script mCtx p path args (buildToolEnvironment mCtx baseEnv) envArgs input
              _ -> do
               ran <- case scriptRuntimeOf script.scriptInfo of
                   Nothing -> do
                       -- Create the process with the modified environment
                       let toolEnv = envArgs ++ buildToolEnvironment mCtx baseEnv
                           process = (proc path args){env = Just toolEnv}
                       fmap Right $ case mCtx >>= ctxProgressCallback of
                           Nothing -> readCreateProcessWithExitCode process input
                           Just report -> case mProcess of
                               Nothing -> runProcessReportingOutput report process input
                               Just p -> do
                                   counter <- newMVar (0 :: Int)
                                   let onLine = reportEventLine report counter p.processOutput
                                   runProcessReportingEvents report (Just onLine) process input
                   Just rt -> do
                       prepared <- prepareLaunch cfg path rt args (buildToolEnvironment mCtx baseEnv) envArgs
                       case prepared of
                           Left problem -> pure (Left (ScriptRuntimeError path problem))
                           Right launch -> do
                               let process = (proc launch.launchCommand launch.launchArgs){env = Just launch.launchEnv, cwd = launch.launchCwd}
                                   report = maybe (const (pure ())) id (mCtx >>= ctxProgressCallback)
                                   stopping = (cancelSignal rt, maybe killGraceMicros id (stopGraceMicros rt))
                               counter <- newMVar (0 :: Int)
                               let onLine = fmap (\p -> reportEventLine report counter p.processOutput) mProcess
                                   running = runProcessReportingEventsStopping stopping report onLine process input
                               case (startTimeoutMicros rt, rt.runtimeTimeout.timeoutStart) of
                                   (Just micros, Just secs) -> maybe (Left (ScriptTimeoutError path secs)) Right <$> timeout micros running
                                   _ -> Right <$> running
               case ran of
                   Left runtimeErr -> pure (Left runtimeErr)
                   Right (code, out, err) -> do
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
                                   -- A v2 tool's result is its framed output (§2.1); an
                                   -- empty output still gets the empty-result message.
                                   framed = case mProcess of
                                       Nothing -> adjusted
                                       Just p
                                           | ByteString.null (ByteStringChar8.strip out)
                                           , Just (AddMessage _) <- maybeBehavior ->
                                               adjusted
                                           | otherwise -> renderOutput p.processOutput out
                                in
                                   pure $ Right framed

{- | Reports one stdout line of a v2 tool as a progress payload
@{"stream": "stdout", "event": <event>, "events": n}@ (an invalid event is
@{"invalid": {"raw", "error"}}@). Blank lines are not events.
-}
reportEventLine :: (Aeson.Value -> IO ()) -> MVar Int -> ProcessOutput -> ByteString -> IO ()
reportEventLine report counter out line0 = do
    let line = case ByteStringChar8.unsnoc line0 of
            Just (l, '\r') -> l
            _ -> line0
    unless (ByteString.null (ByteStringChar8.strip line)) $ do
        n <- modifyMVar counter (\c -> pure (c + 1, c + 1))
        report $
            Aeson.object
                [ "stream" Aeson..= ("stdout" :: Text)
                , "event" Aeson..= outputEventToJSON (lineToEvent out line)
                , "events" Aeson..= n
                ]

{- | Runs a @stream@ v2 tool (@todos/bash-tools-v2.md@ §2.2). Its standard
output is cut into events as it arrives; each event is reported as progress
(@{"stream": "stdout", "event": ..., "events": n}@, as for a @oneshot@) and
kept in a bounded ring ('RunOptions'), never the whole output. The call
completes when the process exits; the result is the latest events and the
count of the earlier ones ('renderStreamResult'). A failed exit is a
'ScriptStreamExitError' carrying that result too.

@runtime.timeout.start@ bounds reaching @process.ready@: under a @line@
ready, the first matching line of standard output (still an event); under
@exit@, the whole run. Once ready, the tool runs until it exits or the call
is cancelled (@cancel-tool-call@, the async engine's call timeout); then its
declared @cancel@ signal and @timeout.stop@ apply, as for a @oneshot@.
-}
runStreamValue ::
    RunOptions ->
    ToolRuntimeConfig ->
    Tracer IO RunTrace ->
    ScriptDescription ->
    Maybe ToolExecutionContext ->
    ScriptProcess ->
    FilePath ->
    [String] ->
    [(String, String)] ->
    [(String, String)] ->
    ByteString ->
    IO (Either RunScriptError ByteString)
runStreamValue opts cfg tracer script mCtx p path args toolEnv envArgs input = do
    prepared <- case scriptRuntimeOf script.scriptInfo of
        Nothing ->
            pure (Right ((proc path args){env = Just (envArgs ++ toolEnv)}, (sigTERM, killGraceMicros), Nothing))
        Just rt -> do
            launched <- prepareLaunch cfg path rt args toolEnv envArgs
            pure $ case launched of
                Left problem -> Left (ScriptRuntimeError path problem)
                Right launch ->
                    Right
                        ( (proc launch.launchCommand launch.launchArgs){env = Just launch.launchEnv, cwd = launch.launchCwd}
                        , (cancelSignal rt, maybe killGraceMicros id (stopGraceMicros rt))
                        , (,) <$> startTimeoutMicros rt <*> rt.runtimeTimeout.timeoutStart
                        )
    case prepared of
        Left err -> pure (Left err)
        Right (process, stopping, mStart) -> do
            ring <- newMVar (emptyEventRing opts.runStreamResultEvents)
            counter <- newMVar (0 :: Int)
            readyVar <- newEmptyMVar
            let out = p.processOutput
                report = maybe (const (pure ())) id (mCtx >>= ctxProgressCallback)
                readyRe = case p.processReady of
                    Just (ReadyLine pat) -> either (const Nothing) Just (compileReadyMatch pat)
                    _ -> Nothing
                onEvent ev = do
                    n <- modifyMVar counter (\c -> pure (c + 1, c + 1))
                    modifyMVar_ ring (pure . pushEvent ev)
                    report $
                        Aeson.object
                            [ "stream" Aeson..= ("stdout" :: Text)
                            , "event" Aeson..= outputEventToJSON ev
                            , "events" Aeson..= n
                            ]
                onLine line0 = do
                    let line = case ByteStringChar8.unsnoc line0 of
                            Just (l, '\r') -> l
                            _ -> line0
                    unless (ByteString.null (ByteStringChar8.strip line)) $
                        onEvent (lineToEvent out line)
                    forM_ readyRe $ \re ->
                        when (readyLineMatches re line) $ void (tryPutMVar readyVar ())
                onChunk chunk = onEvent (OutputEvent (Aeson.String (Text.decodeUtf8With lenientDecode chunk)))
                isChunks = out.outputFraming == FramingChunks
                handling =
                    OutputHandling
                        { stdoutLine = if isChunks then Nothing else Just onLine
                        , stdoutChunk = if isChunks then Just onChunk else Nothing
                        , keepStdout = False
                        , stderrTail = Just streamStderrTailBytes
                        }
                running = runProcessHandling stopping report handling process input
            ran <- case (mStart, p.processReady) of
                (Just (micros, secs), Just (ReadyLine _)) ->
                    withAsync running $ \a -> do
                        started <- timeout micros (race (readMVar readyVar) (waitCatch a))
                        case started of
                            Nothing -> cancel a >> pure (Left (ScriptTimeoutError path secs))
                            Just _ -> Right <$> wait a
                (Just (micros, secs), _) ->
                    maybe (Left (ScriptTimeoutError path secs)) Right <$> timeout micros running
                _ -> Right <$> running
            case ran of
                Left err -> pure (Left err)
                Right (code, _, err) -> do
                    runTracer tracer (RunCommandStopped path args code ByteString.empty err)
                    events <- readMVar ring
                    let rendered = renderStreamResult out events
                    pure $
                        if code /= ExitSuccess
                            then Left (ScriptStreamExitError path code err rendered)
                            else case script.scriptInfo.scriptEmptyResultBehavior of
                                Just (AddMessage msg) | List.null (ringEvents events) -> Right (Text.encodeUtf8 msg)
                                _ -> Right rendered
