{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The @service@ mode of a v2 bash tool (@todos/bash-tools-v2.md@ §2.3,
§2.4, slice 4): a process that outlives the call that started it.

Starting a service tool is a call that completes at @process.ready@; its
result is a /process handle/. Each start is a new process with its own
handle, owned by the session that started it. A handle keeps a bounded ring
of its output events and the tail of its standard error, its lifecycle
('ProcessStatus'), and, when the tool declares @process.input@, an open
standard input the family's typed @send@ tool writes events to.

Output events are delivered to the owning session as mail (from
@process:\<id\>@), folded as the tool's @process.digest@ declares. The
shared @processes@ tool lists, inspects, tails and stops the session's
processes ('runProcessesCommand').

Processes live in one process-wide registry ('globalProcessRegistry'): they
are lost with agents-exe (D3), and a handle that is not in the registry is
reported as unknown, which is what the model sees after a restart.
-}
module System.Agents.Tools.ScriptService (
    -- * Handles
    ProcessStatus (..),
    ServiceHandle,
    ProcessRegistry,
    globalProcessRegistry,
    newProcessRegistry,
    handleId,
    handleStatus,
    handleToJSON,

    -- * Options
    ServiceOptions (..),
    defaultServiceOptions,

    -- * Tool calls
    startService,
    sendToService,
    ProcessesCommand (..),
    parseProcessesCommand,
    runProcessesCommand,

    -- * Session lifecycle
    stopSessionProcesses,

    -- * Naming
    sendToolSuffix,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, handle, onException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, waitForProcess)
import System.Timeout (timeout)
import Text.Regex.TDFA (Regex)

import System.Agents.Session.Mailbox (MailRouter (..), Mailbox (..))
import System.Agents.Session.Types (MailBody (..), Outgoing (..), Priority (..), Sender (..), SessionId)
import System.Agents.Tools.Bash (
    RunScriptError (..),
    ScriptDescription (..),
    ScriptInfo (..),
    buildToolEnvironment,
    parseArgsForValue,
    parseEnvForValue,
 )
import System.Agents.Tools.Context (ToolExecutionContext (..))
import System.Agents.Tools.ScriptProcess (
    EventRing,
    InputFraming (..),
    OutputEvent (..),
    OutputFraming (..),
    ProcessDigest (..),
    ProcessInput (..),
    ProcessOutput (..),
    ProcessReady (..),
    ScriptProcess (..),
    compileReadyMatch,
    emptyEventRing,
    lineToEvent,
    outputEventToJSON,
    pushEvent,
    readyLineMatches,
    ringElided,
    ringEvents,
    validateAgainstSchema,
 )
import System.Agents.Tools.ScriptRuntime (
    CancelPolicy (..),
    Launch (..),
    OnExit (..),
    RuntimeTimeout (..),
    ScriptRuntime (..),
    ToolRuntimeConfig,
    cancelSignal,
    prepareLaunch,
    startTimeoutMicros,
    stopGraceMicros,
 )
import System.Agents.Tools.ScriptTypes (scriptProcessOf, scriptRuntimeOf)

-------------------------------------------------------------------------------
-- Handles
-------------------------------------------------------------------------------

-- | The lifecycle of a service process (§2.3).
data ProcessStatus
    = StatusStarting
    | StatusRunning
    | StatusStopping
    | -- | Exited with this code (negative: killed by that signal).
      StatusExited Int
    | -- | Failed, and why (it exited before @ready@, could not be restarted, ...).
      StatusFailed Text
    | -- | Being restarted (@on_exit: restart@), for the n-th time.
      StatusRestarting Int
    deriving (Show, Eq)

isFinal :: ProcessStatus -> Bool
isFinal s = case s of
    StatusExited _ -> True
    StatusFailed _ -> True
    _ -> False

statusFields :: ProcessStatus -> [Aeson.Pair]
statusFields s = case s of
    StatusStarting -> ["status" .= ("starting" :: Text)]
    StatusRunning -> ["status" .= ("running" :: Text)]
    StatusStopping -> ["status" .= ("stopping" :: Text)]
    StatusExited code -> ["status" .= ("exited" :: Text), "code" .= code]
    StatusFailed why -> ["status" .= ("failed" :: Text), "reason" .= why]
    StatusRestarting n -> ["status" .= ("restarting" :: Text), "restarts" .= n]

-- | One running (or finished) service process.
data ServiceHandle = ServiceHandle
    { svcId :: Text
    , svcTool :: Text
    -- ^ The tool (family) that started it, by its LLM name.
    , svcSession :: SessionId
    , svcSince :: UTCTime
    , svcProcessDecl :: ScriptProcess
    , svcRuntime :: Maybe ScriptRuntime
    , svcStatus :: TVar ProcessStatus
    , svcRing :: TVar EventRing
    , svcStderr :: TVar ByteString
    , svcSeq :: TVar Int
    -- ^ Output events seen, all incarnations together.
    , svcPending :: TVar (Seq (Int, OutputEvent))
    -- ^ Events not yet mailed (nor returned by a @send@), with their sequence number.
    , svcPendingDropped :: TVar Int
    , svcSent :: TVar Int
    , svcLastActivity :: TVar UTCTime
    , svcStdin :: TVar (Maybe Handle)
    , svcStdinLock :: MVar ()
    , svcProc :: TVar (Maybe ProcessHandle)
    , svcStopReason :: TVar (Maybe Text)
    , svcRestarts :: TVar Int
    , svcReady :: TVar Bool
    -- ^ The first incarnation reached @ready@ (before that, an exit fails the start).
    , svcMail :: Maybe (Outgoing -> IO ())
    , svcMailLock :: MVar ()
    , svcOptions :: ServiceOptions
    }

handleId :: ServiceHandle -> Text
handleId = svcId

handleStatus :: ServiceHandle -> IO ProcessStatus
handleStatus h = readTVarIO h.svcStatus

-- | The handle as the model sees it: @{"process", "tool", "status", "since"}@.
handleToJSON :: ServiceHandle -> IO Aeson.Value
handleToJSON h = do
    st <- readTVarIO h.svcStatus
    pure $ Aeson.object (["process" .= h.svcId, "tool" .= h.svcTool, "since" .= h.svcSince] <> statusFields st)

-- | Every service process of this agents-exe, by handle id.
newtype ProcessRegistry = ProcessRegistry (TVar (Map.Map Text ServiceHandle))

newProcessRegistry :: IO ProcessRegistry
newProcessRegistry = ProcessRegistry <$> newTVarIO Map.empty

-- | The registry every service tool of this process uses.
globalProcessRegistry :: ProcessRegistry
globalProcessRegistry = unsafePerformIO newProcessRegistry
{-# NOINLINE globalProcessRegistry #-}

lookupHandle :: ProcessRegistry -> Text -> IO (Maybe ServiceHandle)
lookupHandle (ProcessRegistry var) pid = Map.lookup pid <$> readTVarIO var

sessionHandles :: ProcessRegistry -> SessionId -> IO [ServiceHandle]
sessionHandles (ProcessRegistry var) sid =
    List.sortOn (.svcSince) . filter (\h -> h.svcSession == sid) . Map.elems <$> readTVarIO var

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

-- | Per-agent settings of the service runtime.
data ServiceOptions = ServiceOptions
    { svcRingEvents :: Int
    -- ^ How many output events a handle keeps (agent @streamResultEvents@).
    , svcReplyWindowMicros :: Int
    -- ^ How long a @send@ waits for the events the process produces in reply.
    , svcReplyQuietMicros :: Int
    -- ^ A @send@ returns once no new event arrived for that long.
    , svcRoundQuietMicros :: Int
    -- ^ The quiet period that closes a @per: round@ digest.
    , svcMaxRestarts :: Int
    -- ^ How many times @on_exit: restart@ brings a process back.
    }
    deriving (Show, Eq)

defaultServiceOptions :: ServiceOptions
defaultServiceOptions =
    ServiceOptions
        { svcRingEvents = 200
        , svcReplyWindowMicros = 2000000
        , svcReplyQuietMicros = 300000
        , svcRoundQuietMicros = 1000000
        , svcMaxRestarts = 5
        }

-- | The LLM name of a family's typed @send@ tool is its own name with this suffix.
sendToolSuffix :: Text
sendToolSuffix = "_send"

-- | How much standard error a handle keeps.
stderrTailBytes :: Int
stderrTailBytes = 65536

-- | Grace period between the cancel signal and @SIGKILL@ when @timeout.stop@ is not declared.
defaultStopGraceMicros :: Int
defaultStopGraceMicros = 2000000

-------------------------------------------------------------------------------
-- Start
-------------------------------------------------------------------------------

{- | Starts a service tool (§2.3) with the given call arguments, and returns
its handle once the process is @ready@ (a matching line, or right away;
bounded by @runtime.timeout.start@). The process then belongs to the
context's session and runs until it exits, is stopped, or the session is
deleted.

An exit before @ready@ fails the call, with the tail of standard error.
-}
startService ::
    ServiceOptions ->
    ToolRuntimeConfig ->
    ProcessRegistry ->
    -- | The tool's LLM name (the family).
    Text ->
    ScriptDescription ->
    ToolExecutionContext ->
    Aeson.Value ->
    IO (Either RunScriptError Aeson.Value)
startService opts cfg registry@(ProcessRegistry regVar) toolName script ctx val = do
    let path = script.scriptPath
    case (scriptProcessOf script.scriptInfo, parseArgsForValue script val) of
        (Nothing, _) -> pure (Left (ScriptServiceError path "not a v2 tool"))
        (_, Left err) -> pure (Left (SerializeArgumentErrors path val err))
        (Just decl, Right (argz, stdinText)) -> do
            let subcommand = maybe [] pure script.scriptInfo.scriptSubcommand
                args = "run" : [Text.unpack a | a <- subcommand <> argz]
                envArgs = either (const []) id (parseEnvForValue script val)
                mRuntime = scriptRuntimeOf script.scriptInfo
            baseEnv <- buildToolEnvironment (Just ctx) <$> getEnvironment
            prepared <- case mRuntime of
                Nothing -> pure (Right ((proc path args){env = Just (envArgs ++ baseEnv)}))
                Just rt -> do
                    launched <- prepareLaunch cfg path rt args baseEnv envArgs
                    pure $ case launched of
                        Left problem -> Left (ScriptRuntimeError path problem)
                        Right l -> Right ((proc l.launchCommand l.launchArgs){env = Just l.launchEnv, cwd = l.launchCwd})
            case prepared of
                Left err -> pure (Left err)
                Right process -> do
                    pid <- newProcessId registry
                    now <- getCurrentTime
                    h <-
                        ServiceHandle pid toolName ctx.ctxSessionId now decl mRuntime
                            <$> newTVarIO StatusStarting
                            <*> newTVarIO (emptyEventRing opts.svcRingEvents)
                            <*> newTVarIO ByteString.empty
                            <*> newTVarIO 0
                            <*> newTVarIO Seq.empty
                            <*> newTVarIO 0
                            <*> newTVarIO 0
                            <*> newTVarIO now
                            <*> newTVarIO Nothing
                            <*> newMVar ()
                            <*> newTVarIO Nothing
                            <*> newTVarIO Nothing
                            <*> newTVarIO 0
                            <*> newTVarIO False
                            <*> pure (mailDelivery ctx)
                            <*> newMVar ()
                            <*> pure opts
                    atomically $ modifyTVar' regVar (Map.insert pid h)
                    let input = Text.encodeUtf8 stdinText
                    readyVar <- newEmptyTMVarIO
                    let waitStarted = do
                            outcome <- launchIncarnation h process input readyVar
                            case outcome of
                                Left problem -> pure (Left problem)
                                Right () -> atomically $ do
                                    st <- readTVar h.svcStatus
                                    ready <- tryReadTMVar readyVar
                                    case (ready, st) of
                                        (Just (), _) -> pure (Right ())
                                        (_, StatusFailed why) -> pure (Left (Text.unpack why))
                                        (_, StatusExited code) -> pure (Left ("exited with code " <> show code <> " before it was ready"))
                                        _ -> retry
                        startMicros = mRuntime >>= startTimeoutMicros
                    started <-
                        ( case startMicros of
                            Nothing -> Just <$> waitStarted
                            Just micros -> timeout micros waitStarted
                        )
                            `onException` abandon h
                    case started of
                        Nothing -> do
                            abandon h
                            pure (Left (ScriptTimeoutError path (fromMaybe 0 (mRuntime >>= (.runtimeTimeout.timeoutStart)))))
                        Just (Left problem) -> do
                            abandon h
                            err <- readTVarIO h.svcStderr
                            pure (Left (ScriptServiceError path (problem <> stderrNote err)))
                        Just (Right ()) -> do
                            atomically $ do
                                writeTVar h.svcReady True
                                st <- readTVar h.svcStatus
                                when (st == StatusStarting) $ writeTVar h.svcStatus StatusRunning
                            forM_ (mRuntime >>= (.runtimeTimeout.timeoutIdle)) $ \idle ->
                                void $ forkIO $ idleWatch h idle
                            forM_ h.svcMail $ \_ -> void $ forkIO $ digestLoop h
                            Right <$> handleToJSON h
  where
    stderrNote err
        | ByteString.null err = ""
        | otherwise = "; stderr: " <> Text.unpack (Text.decodeUtf8With lenientDecode (lastBytes 2000 err))
    -- A start that failed or timed out leaves no handle behind.
    abandon h = do
        atomically $ writeTVar h.svcStopReason (Just "start failed")
        void $ stopHandle True h
        atomically $ modifyTVar' regVar (Map.delete h.svcId)

{- | Where the handle's mail goes: the starting session's own mailbox,
looked up on each delivery (a session's mailbox can be re-registered).
'Nothing' when the context has no mail router; output is then only in the
ring (@processes tail@).
-}
mailDelivery :: ToolExecutionContext -> Maybe (Outgoing -> IO ())
mailDelivery ctx = case ctx.ctxMailRouter of
    Nothing -> Nothing
    Just router -> Just $ \o -> do
        found <- router.mrLookup ctx.ctxSessionId
        -- A refused envelope (a full mailbox) is dropped: the events stay in the ring.
        forM_ found $ \(_, mb) -> handle (\(_ :: SomeException) -> pure ()) (void (mb.mbSend o))

newProcessId :: ProcessRegistry -> IO Text
newProcessId reg = do
    u <- UUID.nextRandom
    let pid = "p-" <> Text.take 8 (UUID.toText u)
    taken <- isJust <$> lookupHandle reg pid
    if taken then newProcessId reg else pure pid

{- | Starts one incarnation of the process (the first one, or a restart),
with the threads that read its output and supervise its exit. Returns once
the process is spawned; @readyVar@ is filled when it is ready.
-}
launchIncarnation :: ServiceHandle -> CreateProcess -> ByteString -> TMVar () -> IO (Either String ())
launchIncarnation h process input readyVar = do
    spawned <- try (createProcess process{std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, create_group = True})
    case spawned of
        Left (e :: IOException) -> do
            atomically $ writeTVar h.svcStatus (StatusFailed ("cannot start: " <> Text.pack (show e)))
            pure (Left ("cannot start: " <> show e))
        Right (Just hIn, Just hOut, Just hErr, ph) -> do
            atomically $ writeTVar h.svcProc (Just ph)
            -- The v1 stdin arguments are written first; without process.input
            -- stdin is then closed, as for any other tool.
            ignoreIO (ByteString.hPut hIn input >> hFlush hIn)
            case h.svcProcessDecl.processInput of
                Nothing -> ignoreIO (hClose hIn)
                Just _ -> atomically $ writeTVar h.svcStdin (Just hIn)
            let decl = h.svcProcessDecl
                readyRe = case decl.processReady of
                    Just (ReadyLine pat) -> either (const Nothing) Just (compileReadyMatch pat)
                    _ -> Nothing
            unless (isJust readyRe) $ atomically $ void $ tryPutTMVar readyVar ()
            outDone <- newEmptyTMVarIO
            errDone <- newEmptyTMVarIO
            _ <- forkIO $ do
                readStdout h decl.processOutput readyRe readyVar hOut `seqFinally` atomically (putTMVar outDone ())
            _ <- forkIO $ do
                readStderr h hErr `seqFinally` atomically (putTMVar errDone ())
            _ <- forkIO $ do
                code <- waitForProcess ph
                atomically $ readTMVar outDone >> readTMVar errDone
                ignoreIO (hClose hIn)
                atomically $ writeTVar h.svcStdin Nothing >> writeTVar h.svcProc Nothing
                onIncarnationExit h process input code
            pure (Right ())
        Right _ -> pure (Left "process pipes were not created")
  where
    seqFinally act fin = (act `onException` fin) >> fin

readStdout :: ServiceHandle -> ProcessOutput -> Maybe Regex -> TMVar () -> Handle -> IO ()
readStdout h out readyRe readyVar hOut = loop ByteString.empty
  where
    loop pending = do
        chunk <- either (\(_ :: IOException) -> ByteString.empty) id <$> try (ByteString.hGetSome hOut 32768)
        if ByteString.null chunk
            then unless (ByteString.null pending) (onLine pending)
            else case out.outputFraming of
                FramingChunks -> do
                    onEvent h (OutputEvent (Aeson.String (Text.decodeUtf8With lenientDecode chunk)))
                    loop ByteString.empty
                _ -> do
                    let buffered = pending <> chunk
                    case ByteStringChar8.elemIndexEnd '\n' buffered of
                        Nothing -> loop buffered
                        Just ix -> do
                            let (complete, rest) = ByteString.splitAt (ix + 1) buffered
                            mapM_ onLine (ByteStringChar8.lines complete)
                            loop rest
    onLine line0 = do
        let line = case ByteStringChar8.unsnoc line0 of
                Just (l, '\r') -> l
                _ -> line0
        unless (ByteString.null (ByteStringChar8.strip line)) $
            onEvent h (lineToEvent out line)
        forM_ readyRe $ \re ->
            when (readyLineMatches re line) $ atomically $ void $ tryPutTMVar readyVar ()

readStderr :: ServiceHandle -> Handle -> IO ()
readStderr h hErr = loop
  where
    loop = do
        chunk <- either (\(_ :: IOException) -> ByteString.empty) id <$> try (ByteString.hGetSome hErr 32768)
        unless (ByteString.null chunk) $ do
            atomically $ modifyTVar' h.svcStderr (\t -> lastBytes stderrTailBytes (t <> chunk))
            loop

onEvent :: ServiceHandle -> OutputEvent -> IO ()
onEvent h ev = do
    now <- getCurrentTime
    atomically $ do
        n <- (+ 1) <$> readTVar h.svcSeq
        writeTVar h.svcSeq n
        modifyTVar' h.svcRing (pushEvent ev)
        pending <- readTVar h.svcPending
        let pending' = pending |> (n, ev)
            limit = max 1 h.svcOptions.svcRingEvents
        if Seq.length pending' > limit
            then do
                writeTVar h.svcPending (Seq.drop (Seq.length pending' - limit) pending')
                modifyTVar' h.svcPendingDropped (+ (Seq.length pending' - limit))
            else writeTVar h.svcPending pending'
        writeTVar h.svcLastActivity now

{- | What happens when an incarnation exits: a stop, or an exit before the
first @ready@, is final; otherwise @runtime.on_exit@ decides (@report@ and
@fail@: final, the model is told; @restart@: brought back after a backoff,
at most 'svcMaxRestarts' times, every restart told).
-}
onIncarnationExit :: ServiceHandle -> CreateProcess -> ByteString -> ExitCode -> IO ()
onIncarnationExit h process input code = do
    let codeInt = case code of
            ExitSuccess -> 0
            ExitFailure n -> n
    stopReason <- readTVarIO h.svcStopReason
    wasReady <- readTVarIO h.svcReady
    restarts <- readTVarIO h.svcRestarts
    let onExit = fromMaybe OnExitReport (h.svcRuntime >>= (.runtimeOnExit))
        opts = h.svcOptions
    current <- readTVarIO h.svcStatus
    if isJust stopReason || not wasReady || onExit /= OnExitRestart || restarts >= opts.svcMaxRestarts
        then do
            let final = case current of
                    StatusFailed _ -> current
                    _
                        | isJust stopReason || onExit /= OnExitRestart || not wasReady -> StatusExited codeInt
                        | otherwise -> StatusFailed ("exited with code " <> tshow codeInt <> " after " <> tshow restarts <> " restarts")
            atomically $ writeTVar h.svcStatus final
            flushMail h
            notice h $
                Aeson.object
                    ( ["process" .= h.svcId, "tool" .= h.svcTool, "exit" .= codeInt]
                        <> statusFields final
                        <> maybe [] (\r -> ["stopped" .= r]) stopReason
                    )
        else do
            let n = restarts + 1
            atomically $ writeTVar h.svcRestarts n >> writeTVar h.svcStatus (StatusRestarting n)
            flushMail h
            notice h $
                Aeson.object (["process" .= h.svcId, "tool" .= h.svcTool, "exit" .= codeInt] <> statusFields (StatusRestarting n))
            threadDelay (min 30 (2 ^ (n - 1)) * 1000000)
            stopped <- isJust <$> readTVarIO h.svcStopReason
            if stopped
                then atomically $ writeTVar h.svcStatus (StatusExited codeInt)
                else do
                    readyVar <- newEmptyTMVarIO
                    launched <- launchIncarnation h process input readyVar
                    case launched of
                        Left _ -> pure ()
                        Right () -> atomically $ do
                            st <- readTVar h.svcStatus
                            when (st == StatusRestarting n) $ writeTVar h.svcStatus StatusRunning

-------------------------------------------------------------------------------
-- Mail
-------------------------------------------------------------------------------

-- | Mails the pending events, if any, as one digest.
flushMail :: ServiceHandle -> IO ()
flushMail h = forM_ h.svcMail $ \deliver -> withMVar h.svcMailLock $ \() -> do
    (events, dropped) <- atomically $ do
        evs <- readTVar h.svcPending
        d <- readTVar h.svcPendingDropped
        writeTVar h.svcPending Seq.empty
        writeTVar h.svcPendingDropped 0
        pure (toList evs, d)
    unless (null events && dropped == 0) $
        deliver $
            processMail h $
                Aeson.object
                    [ "process" .= h.svcId
                    , "tool" .= h.svcTool
                    , "events" .= fmap (outputEventToJSON . snd) events
                    , "elided" .= dropped
                    ]

-- | Mails a lifecycle notice (an exit, a restart).
notice :: ServiceHandle -> Aeson.Value -> IO ()
notice h v = forM_ h.svcMail $ \deliver -> withMVar h.svcMailLock $ \() -> deliver (processMail h v)

processMail :: ServiceHandle -> Aeson.Value -> Outgoing
processMail h v =
    Outgoing
        { outId = Nothing
        , outFrom = FromSystem ("process:" <> h.svcId)
        , outPriority = Normal
        , outHops = 0
        , outBody = AgentMessage (Text.decodeUtf8 (LByteString.toStrict (Aeson.encode v))) Nothing False
        }

{- | Folds the output into mail as the tool's @process.digest@ declares
(§2.3): every @n@ events, every @n@ seconds, or (@round@, the default) once
the output has been quiet for a moment ('svcRoundQuietMicros'). Ends when
the process is final; its last events are flushed with the exit notice.
-}
digestLoop :: ServiceHandle -> IO ()
digestLoop h = case fromMaybe DigestPerRound h.svcProcessDecl.processDigest of
    DigestPerEvents n -> do
        final <- atomically $ do
            st <- readTVar h.svcStatus
            pending <- readTVar h.svcPending
            if isFinal st
                then pure True
                else if Seq.length pending >= max 1 n then pure False else retry
        unless final $ flushMail h >> digestLoop h
    DigestPerSeconds n -> do
        threadDelay (max 1 n * 1000000)
        final <- isFinal <$> readTVarIO h.svcStatus
        unless final $ flushMail h >> digestLoop h
    DigestPerRound -> do
        final <- atomically $ do
            st <- readTVar h.svcStatus
            pending <- readTVar h.svcPending
            if isFinal st then pure True else if Seq.null pending then retry else pure False
        unless final $ do
            let settle = do
                    before <- readTVarIO h.svcSeq
                    threadDelay h.svcOptions.svcRoundQuietMicros
                    after <- readTVarIO h.svcSeq
                    pending <- readTVarIO h.svcPending
                    unless (before == after || Seq.length pending >= h.svcOptions.svcRingEvents) settle
            settle
            flushMail h
            digestLoop h

-------------------------------------------------------------------------------
-- Idle
-------------------------------------------------------------------------------

-- | Stops the process after @runtime.timeout.idle@ seconds without output or input events.
idleWatch :: ServiceHandle -> Double -> IO ()
idleWatch h idle = loop
  where
    stepMicros = max 50000 (min 1000000 (round (idle * 1000000) `div` 4))
    loop = do
        threadDelay stepMicros
        st <- readTVarIO h.svcStatus
        unless (isFinal st) $ do
            now <- getCurrentTime
            lastAt <- readTVarIO h.svcLastActivity
            if realToFrac (diffUTCTime now lastAt) >= idle && st == StatusRunning
                then do
                    atomically $ writeTVar h.svcStopReason (Just ("idle for " <> tshow idle <> "s"))
                    void $ stopHandle False h
                else loop

-------------------------------------------------------------------------------
-- Send
-------------------------------------------------------------------------------

{- | The family's typed @send@ tool (§2.4): @{"process": id, "event": ...}@.
The event must match the declared @process.input.event@; it is written to
the process's standard input (one JSON line under @jsonl@, the string under
@lines@). The answer is the events the process produced within a short
window after it ('svcReplyWindowMicros'), which are then not mailed again,
or @accepted@.
-}
sendToService :: ProcessRegistry -> Text -> ScriptDescription -> ToolExecutionContext -> Aeson.Value -> IO (Either RunScriptError Aeson.Value)
sendToService registry toolName script ctx val = do
    let path = script.scriptPath
        failWith = pure . Left . ScriptServiceError path
    case Aeson.parseEither parseSend val of
        Left err -> pure (Left (SerializeArgumentErrors path val err))
        Right (pid, event) -> do
            mh <- lookupHandle registry pid
            case mh of
                Nothing -> failWith (unknownProcess pid)
                Just h
                    | h.svcSession /= ctx.ctxSessionId -> failWith (unknownProcess pid)
                    | h.svcTool /= toolName -> failWith ("process " <> Text.unpack pid <> " belongs to " <> Text.unpack h.svcTool <> ", use " <> Text.unpack h.svcTool <> Text.unpack sendToolSuffix)
                    | otherwise -> case h.svcProcessDecl.processInput of
                        Nothing -> failWith "this tool declares no process.input: its processes take no events"
                        Just input -> case encodeInput input event of
                            Left problem -> failWith problem
                            Right bytes -> do
                                before <- readTVarIO h.svcSeq
                                written <- writeStdin h bytes
                                case written of
                                    Left problem -> failWith problem
                                    Right () -> do
                                        now <- getCurrentTime
                                        atomically $ modifyTVar' h.svcSent (+ 1) >> writeTVar h.svcLastActivity now
                                        replies <- collectReplies h before
                                        pure $
                                            Right $
                                                if null replies
                                                    then Aeson.object ["process" .= pid, "status" .= ("accepted" :: Text)]
                                                    else Aeson.object ["process" .= pid, "events" .= fmap outputEventToJSON replies]
  where
    parseSend = Aeson.withObject "send" $ \o -> (,) <$> o Aeson..: "process" <*> o Aeson..: "event"

-- | Encodes an event for the process's standard input, after checking it against the declared schema.
encodeInput :: ProcessInput -> Aeson.Value -> Either String ByteString
encodeInput input event =
    case maybe [] (`validateAgainstSchema` event) input.inputEvent of
        problems@(_ : _) -> Left ("the event does not match the declared process.input.event: " <> List.intercalate "; " problems)
        [] -> case input.inputFraming of
            InputJsonl -> Right (LByteString.toStrict (Aeson.encode event) <> "\n")
            InputLines -> case event of
                Aeson.String t
                    | not (Text.any (== '\n') t) -> Right (Text.encodeUtf8 t <> "\n")
                _ -> Left "under process.input.framing \"lines\" an event is a string without newlines"

writeStdin :: ServiceHandle -> ByteString -> IO (Either String ())
writeStdin h bytes = withMVar h.svcStdinLock $ \() -> do
    st <- readTVarIO h.svcStatus
    mIn <- readTVarIO h.svcStdin
    case (st, mIn) of
        (StatusRunning, Just hIn) -> do
            r <- try (ByteString.hPut hIn bytes >> hFlush hIn)
            pure $ case r of
                Left (e :: IOException) -> Left ("the process's standard input is closed: " <> show e)
                Right () -> Right ()
        _ -> pure (Left ("process " <> Text.unpack h.svcId <> " is not running (" <> statusName st <> ")"))

-- | The events after sequence number @before@, collected within the reply window; they are taken out of the mail.
collectReplies :: ServiceHandle -> Int -> IO [OutputEvent]
collectReplies h before = do
    let opts = h.svcOptions
        step = max 10000 (opts.svcReplyQuietMicros `div` 3)
        loop waited lastSeen quietFor
            | waited >= opts.svcReplyWindowMicros = pure ()
            | lastSeen > before && quietFor >= opts.svcReplyQuietMicros = pure ()
            | otherwise = do
                threadDelay step
                now <- readTVarIO h.svcSeq
                final <- isFinal <$> readTVarIO h.svcStatus
                if final
                    then pure ()
                    else loop (waited + step) now (if now == lastSeen then quietFor + step else 0)
    loop 0 before 0
    atomically $ do
        pending <- readTVar h.svcPending
        let (older, replies) = Seq.partition (\(n, _) -> n <= before) pending
        writeTVar h.svcPending older
        pure (fmap snd (toList replies))

-------------------------------------------------------------------------------
-- Stop
-------------------------------------------------------------------------------

{- | Stops a process: its declared @cancel@ (a signal, or an event written to
its standard input), then @SIGKILL@ after @timeout.stop@; with @force@,
@SIGKILL@ at once. Returns once the process is final (or the grace period
and a margin have passed).
-}
stopHandle :: Bool -> ServiceHandle -> IO ProcessStatus
stopHandle force h = do
    atomically $ do
        r <- readTVar h.svcStopReason
        when (r == Nothing) $ writeTVar h.svcStopReason (Just (if force then "stopped (forced)" else "stopped"))
        st <- readTVar h.svcStatus
        unless (isFinal st) $ writeTVar h.svcStatus StatusStopping
    mPh <- readTVarIO h.svcProc
    let grace = fromMaybe defaultStopGraceMicros (h.svcRuntime >>= stopGraceMicros)
        signalWith sig = forM_ mPh $ \ph -> do
            mPid <- getPid ph
            forM_ mPid $ \pid -> ignoreIO (signalProcessGroup sig pid)
        waitFinal micros = timeout micros $ atomically $ readTVar h.svcStatus >>= \st -> if isFinal st then pure () else retry
    if force
        then signalWith sigKILL
        else do
            case h.svcRuntime >>= (.runtimeCancel) of
                Just (CancelSend ev) | Just input <- h.svcProcessDecl.processInput, Right bytes <- encodeInput input{inputEvent = Nothing} ev -> do
                    mIn <- readTVarIO h.svcStdin
                    forM_ mIn $ \hIn -> withMVar h.svcStdinLock $ \() -> ignoreIO (ByteString.hPut hIn bytes >> hFlush hIn)
                _ -> signalWith (maybe sigTERM cancelSignal h.svcRuntime)
            done <- waitFinal grace
            when (done == Nothing) $ do
                -- the polite request was not enough
                signalWith (maybe sigTERM cancelSignal h.svcRuntime)
                signalWith sigKILL
    _ <- waitFinal 2000000
    readTVarIO h.svcStatus

-- | Stops every process of a session and forgets them (a session delete, §3).
stopSessionProcesses :: ProcessRegistry -> SessionId -> IO ()
stopSessionProcesses registry@(ProcessRegistry regVar) sid = do
    hs <- sessionHandles registry sid
    forM_ hs $ \h -> do
        st <- readTVarIO h.svcStatus
        unless (isFinal st) $ do
            atomically $ writeTVar h.svcStopReason (Just "session deleted")
            void (stopHandle False h)
        atomically $ modifyTVar' regVar (Map.delete h.svcId)

-------------------------------------------------------------------------------
-- The processes tool
-------------------------------------------------------------------------------

-- | A sub-command of the shared @processes@ tool (§2.4).
data ProcessesCommand
    = ProcessesList
    | ProcessesStatus Text
    | ProcessesTail Text Int
    | ProcessesStop Text Bool
    deriving (Show, Eq)

parseProcessesCommand :: Aeson.Value -> Either String ProcessesCommand
parseProcessesCommand = Aeson.parseEither $ Aeson.withObject "processes" $ \o -> do
    cmd <- o Aeson..: "command"
    case cmd :: Text of
        "list" -> pure ProcessesList
        "status" -> ProcessesStatus <$> o Aeson..: "process"
        "tail" -> ProcessesTail <$> o Aeson..: "process" <*> (fromMaybe 20 <$> o Aeson..:? "n")
        "stop" -> ProcessesStop <$> o Aeson..: "process" <*> (fromMaybe False <$> o Aeson..:? "force")
        _ -> fail ("unknown command " <> show cmd <> ", allowed: list, status, tail, stop")

{- | Runs a @processes@ sub-command for the context's session: only that
session's processes are listed or reachable.
-}
runProcessesCommand :: ProcessRegistry -> ToolExecutionContext -> ProcessesCommand -> IO (Either String Aeson.Value)
runProcessesCommand registry ctx cmd = case cmd of
    ProcessesList -> do
        hs <- sessionHandles registry ctx.ctxSessionId
        now <- getCurrentTime
        rows <- mapM (\h -> withAge now h <$> handleToJSON h) hs :: IO [Aeson.Value]
        pure (Right (Aeson.object ["processes" .= rows]))
    ProcessesStatus pid -> withOwn pid $ \h -> do
        base <- handleToJSON h
        (seen, sent, restarts, ring, err) <-
            atomically $
                (,,,,)
                    <$> readTVar h.svcSeq
                    <*> readTVar h.svcSent
                    <*> readTVar h.svcRestarts
                    <*> readTVar h.svcRing
                    <*> readTVar h.svcStderr
        let errLines = lastN 20 (filter (not . Text.null) (Text.lines (Text.decodeUtf8With lenientDecode err)))
        pure $
            Right $
                extend
                    base
                    [ "events" .= seen
                    , "sent" .= sent
                    , "restarts" .= restarts
                    , "last_events" .= fmap outputEventToJSON (lastN 10 (ringEvents ring))
                    , "last_stderr" .= errLines
                    , "input" .= fmap (\i -> fromMaybe (Aeson.Bool True) i.inputEvent) h.svcProcessDecl.processInput
                    , "runtime" .= h.svcRuntime
                    ]
    ProcessesTail pid n -> withOwn pid $ \h -> do
        ring <- readTVarIO h.svcRing
        let evs = ringEvents ring
            kept = lastN (max 0 n) evs
        pure $
            Right $
                Aeson.object
                    [ "process" .= pid
                    , "events" .= fmap outputEventToJSON kept
                    , "elided" .= (ringElided ring + length evs - length kept)
                    ]
    ProcessesStop pid force -> withOwn pid $ \h -> do
        _ <- stopHandle force h
        Right <$> handleToJSON h
  where
    withOwn pid k = do
        mh <- lookupHandle registry pid
        case mh of
            Just h | h.svcSession == ctx.ctxSessionId -> k h
            _ -> pure (Left (unknownProcess pid))
    withAge :: UTCTime -> ServiceHandle -> Aeson.Value -> Aeson.Value
    withAge now h v = extend v ["age_seconds" .= (round (realToFrac (diffUTCTime now h.svcSince) :: Double) :: Int)]
    extend v extra = case v of
        Aeson.Object o -> case Aeson.object extra of
            Aeson.Object e -> Aeson.Object (o <> e)
            _ -> v
        _ -> v

unknownProcess :: Text -> String
unknownProcess pid =
    "unknown process " <> show pid <> " (not one of this session's processes; processes do not survive an agents-exe restart, see processes list)"

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

statusName :: ProcessStatus -> String
statusName s = case s of
    StatusStarting -> "starting"
    StatusRunning -> "running"
    StatusStopping -> "stopping"
    StatusExited c -> "exited " <> show c
    StatusFailed why -> "failed: " <> Text.unpack why
    StatusRestarting n -> "restarting " <> show n

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

lastBytes :: Int -> ByteString -> ByteString
lastBytes n b = ByteString.drop (ByteString.length b - n) b

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

ignoreIO :: IO () -> IO ()
ignoreIO = handle (\(_ :: IOException) -> pure ())
