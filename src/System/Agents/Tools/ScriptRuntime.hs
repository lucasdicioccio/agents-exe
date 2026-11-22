{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The @runtime@ object of a v2 bash tool's @describe@ document
(@todos/bash-tools-v2.md@, §1.2 and §1.4): how a tool asks to be run, and
how the operator's configuration ('ToolRuntimeConfig') turns that into the
command agents-exe actually starts.

This is slice (2) of the spec (D2): @runtime@ for @oneshot@ tools. The tool
names /what/ it needs (a user, a sandbox, an envdir); the operator names
/how/ it is enforced (@--run-as-mechanism@, @--sandbox@, @--envdir-root@).
A tool that declares something the operator did not configure fails to load,
naming the field: it never runs unconfined, or as the wrong user, by
silence.

What a @oneshot@ cannot use (@timeout.idle@, @on_exit@, a @cancel@ that
writes to stdin) is refused at load by 'validateScriptRuntime'; the service
slice implements it.
-}
module System.Agents.Tools.ScriptRuntime (
    -- * Declaration (what the tool asks for)
    ScriptRuntime (..),
    RunAs (..),
    Sandbox (..),
    SandboxNet (..),
    SandboxTmp (..),
    RuntimeTimeout (..),
    OnExit (..),
    CancelPolicy (..),
    emptyScriptRuntime,

    -- * Operator configuration (how it is enforced)
    ToolRuntimeConfig (..),
    RunAsMechanism (..),
    SandboxMechanism (..),
    defaultToolRuntimeConfig,
    parseRunAsMechanism,
    parseSandboxMechanism,
    setToolRuntimeConfig,
    getToolRuntimeConfig,

    -- * Load-time checks
    validateScriptRuntime,

    -- * Sandbox mechanism resolution
    SandboxSupport (..),
    ResolvedSandbox (..),
    getSandboxSupport,
    resolveSandboxMechanism,
    resolveSandbox,
    landlockFieldProblems,
    landlockRules,

    -- * Launching
    Launch (..),
    prepareLaunch,
    readEnvdir,
    overrideEnvironment,
    runAsPrefix,
    bwrapArgs,
    cancelSignal,
    stopGraceMicros,
    startTimeoutMicros,
) where

import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson (Parser)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, listDirectory, makeAbsolute)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (Signal, sigINT, sigTERM)
import System.Posix.User (getGroupEntryForID, getUserEntryForName, groupName, userGroupID)

import System.Agents.Tools.Landlock (LandlockAccess (..), LandlockRule (..), landlockAbi, landlockLauncherArgs)

-------------------------------------------------------------------------------
-- Declaration
-------------------------------------------------------------------------------

{- | The @runtime@ object. Every field is optional; an absent field keeps v1
behaviour.
-}
data ScriptRuntime
    = ScriptRuntime
    { runtimeRunAs :: Maybe RunAs
    , runtimeSandbox :: Maybe Sandbox
    , runtimeEnvdir :: Maybe Text
    -- ^ A subdirectory name under the operator's @--envdir-root@.
    , runtimeEnvironment :: Map Text Text
    -- ^ Literal variables, added last (they win over the envdir).
    , runtimeRundir :: Maybe Text
    -- ^ Relative to the tool's directory; created if missing; the working directory.
    , runtimeTimeout :: RuntimeTimeout
    , runtimeOnExit :: Maybe OnExit
    -- ^ For a @service@ only.
    , runtimeCancel :: Maybe CancelPolicy
    }
    deriving (Show, Eq, Ord)

emptyScriptRuntime :: ScriptRuntime
emptyScriptRuntime = ScriptRuntime Nothing Nothing Nothing Map.empty Nothing (RuntimeTimeout Nothing Nothing Nothing) Nothing Nothing

-- | @runtime.run_as@.
data RunAs = RunAs
    { runAsUser :: Text
    , runAsGroup :: Maybe Text
    }
    deriving (Show, Eq, Ord)

-- | @runtime.sandbox@ (§1.4).
data Sandbox = Sandbox
    { sandboxRead :: [Text]
    , sandboxWrite :: [Text]
    , sandboxNet :: SandboxNet
    , sandboxTmp :: SandboxTmp
    }
    deriving (Show, Eq, Ord)

data SandboxNet = NetHost | NetNone
    deriving (Show, Eq, Ord)

data SandboxTmp = TmpShared | TmpPrivate
    deriving (Show, Eq, Ord)

-- | @runtime.timeout@, in seconds.
data RuntimeTimeout = RuntimeTimeout
    { timeoutStart :: Maybe Double
    -- ^ For a @oneshot@ (whose @ready@ is its exit): the time it has to complete.
    , timeoutStop :: Maybe Double
    -- ^ Between the cancel signal and @SIGKILL@.
    , timeoutIdle :: Maybe Double
    -- ^ For a @service@ only.
    }
    deriving (Show, Eq, Ord)

data OnExit = OnExitReport | OnExitRestart | OnExitFail
    deriving (Show, Eq, Ord)

-- | @runtime.cancel@: the first thing sent on cancel.
data CancelPolicy
    = CancelSigterm
    | CancelSigint
    | -- | A JSON event written to stdin before the signal (needs an open stdin).
      CancelSend Aeson.Value
    deriving (Show, Eq, Ord)

-- | Refuses keys a declaration does not know: a typo must not run a tool unconfined.
onlyKeys :: String -> [Aeson.Key] -> Aeson.Object -> Aeson.Parser ()
onlyKeys what allowed o =
    case [Key.toString k | k <- KeyMap.keys o, k `notElem` allowed] of
        [] -> pure ()
        unknown -> fail (what <> ": unknown field(s) " <> List.intercalate ", " unknown <> ", allowed: " <> List.intercalate ", " (fmap Key.toString allowed))

instance FromJSON ScriptRuntime where
    parseJSON = Aeson.withObject "runtime" $ \o -> do
        onlyKeys "runtime" ["run_as", "sandbox", "envdir", "environment", "rundir", "timeout", "on_exit", "cancel"] o
        ScriptRuntime
            <$> o .:? "run_as"
            <*> o .:? "sandbox"
            <*> o .:? "envdir"
            <*> o .:? "environment" .!= Map.empty
            <*> o .:? "rundir"
            <*> o .:? "timeout" .!= RuntimeTimeout Nothing Nothing Nothing
            <*> o .:? "on_exit"
            <*> o .:? "cancel"

instance ToJSON ScriptRuntime where
    toJSON r =
        Aeson.object $
            catMaybes
                [ ("run_as" .=) <$> r.runtimeRunAs
                , ("sandbox" .=) <$> r.runtimeSandbox
                , ("envdir" .=) <$> r.runtimeEnvdir
                , if Map.null r.runtimeEnvironment then Nothing else Just ("environment" .= r.runtimeEnvironment)
                , ("rundir" .=) <$> r.runtimeRundir
                , if r.runtimeTimeout == RuntimeTimeout Nothing Nothing Nothing then Nothing else Just ("timeout" .= r.runtimeTimeout)
                , ("on_exit" .=) <$> r.runtimeOnExit
                , ("cancel" .=) <$> r.runtimeCancel
                ]

instance FromJSON RunAs where
    parseJSON = Aeson.withObject "runtime.run_as" $ \o -> do
        onlyKeys "runtime.run_as" ["user", "group"] o
        RunAs <$> o .: "user" <*> o .:? "group"

instance ToJSON RunAs where
    toJSON r = Aeson.object (("user" .= r.runAsUser) : maybe [] (\g -> ["group" .= g]) r.runAsGroup)

instance FromJSON Sandbox where
    parseJSON = Aeson.withObject "runtime.sandbox" $ \o -> do
        onlyKeys "runtime.sandbox" ["fs", "net", "tmp"] o
        (rd, wr) <- do
            mFs <- o .:? "fs"
            case mFs of
                Nothing -> pure ([], [])
                Just fs -> do
                    onlyKeys "runtime.sandbox.fs" ["read", "write"] fs
                    (,) <$> fs .:? "read" .!= [] <*> fs .:? "write" .!= []
        Sandbox rd wr
            <$> o .:? "net" .!= NetHost
            <*> o .:? "tmp" .!= TmpShared

instance ToJSON Sandbox where
    toJSON s =
        Aeson.object
            [ "fs" .= Aeson.object ["read" .= s.sandboxRead, "write" .= s.sandboxWrite]
            , "net" .= s.sandboxNet
            , "tmp" .= s.sandboxTmp
            ]

instance FromJSON SandboxNet where
    parseJSON = Aeson.withText "runtime.sandbox.net" $ \t -> case t of
        "host" -> pure NetHost
        "none" -> pure NetNone
        _ -> fail ("invalid runtime.sandbox.net " <> show t <> ", allowed: host, none")

instance ToJSON SandboxNet where
    toJSON n = case n of
        NetHost -> "host"
        NetNone -> "none"

instance FromJSON SandboxTmp where
    parseJSON = Aeson.withText "runtime.sandbox.tmp" $ \t -> case t of
        "shared" -> pure TmpShared
        "private" -> pure TmpPrivate
        _ -> fail ("invalid runtime.sandbox.tmp " <> show t <> ", allowed: shared, private")

instance ToJSON SandboxTmp where
    toJSON n = case n of
        TmpShared -> "shared"
        TmpPrivate -> "private"

instance FromJSON RuntimeTimeout where
    parseJSON = Aeson.withObject "runtime.timeout" $ \o -> do
        onlyKeys "runtime.timeout" ["start", "stop", "idle"] o
        RuntimeTimeout <$> o .:? "start" <*> o .:? "stop" <*> o .:? "idle"

instance ToJSON RuntimeTimeout where
    toJSON t =
        Aeson.object $
            catMaybes [("start" .=) <$> t.timeoutStart, ("stop" .=) <$> t.timeoutStop, ("idle" .=) <$> t.timeoutIdle]

instance FromJSON OnExit where
    parseJSON = Aeson.withText "runtime.on_exit" $ \t -> case t of
        "report" -> pure OnExitReport
        "restart" -> pure OnExitRestart
        "fail" -> pure OnExitFail
        _ -> fail ("invalid runtime.on_exit " <> show t <> ", allowed: report, restart, fail")

instance ToJSON OnExit where
    toJSON e = case e of
        OnExitReport -> "report"
        OnExitRestart -> "restart"
        OnExitFail -> "fail"

instance FromJSON CancelPolicy where
    parseJSON v = case v of
        Aeson.String "SIGTERM" -> pure CancelSigterm
        Aeson.String "SIGINT" -> pure CancelSigint
        Aeson.Object o | Just ev <- KeyMap.lookup "send" o, KeyMap.size o == 1 -> pure (CancelSend ev)
        _ -> fail ("invalid runtime.cancel " <> show v <> ", allowed: \"SIGTERM\", \"SIGINT\", {\"send\": <event>}")

instance ToJSON CancelPolicy where
    toJSON c = case c of
        CancelSigterm -> "SIGTERM"
        CancelSigint -> "SIGINT"
        CancelSend ev -> Aeson.object ["send" .= ev]

-------------------------------------------------------------------------------
-- Operator configuration
-------------------------------------------------------------------------------

-- | How @run_as@ is enforced (@--run-as-mechanism@).
data RunAsMechanism = RunAsSetpriv | RunAsSudo | RunAsRunuser
    deriving (Show, Eq, Ord)

-- | How @sandbox@ is enforced (@--sandbox@).
data SandboxMechanism
    = SandboxBwrap
    | -- | Landlock rules applied before exec ("System.Agents.Tools.Landlock").
      SandboxLandlock
    | -- | Per tool, the strongest mechanism that honours every declared field:
      -- bubblewrap when it works here, else Landlock.
      SandboxAuto
    deriving (Show, Eq, Ord)

{- | The operator's side of @runtime@: nothing is configured by default, so a
tool that needs a mechanism fails to load until the operator names one.
-}
data ToolRuntimeConfig = ToolRuntimeConfig
    { envdirRoot :: Maybe FilePath
    -- ^ @--envdir-root@ (D7: never an implicit location).
    , runAsMechanism :: Maybe RunAsMechanism
    , sandboxMechanism :: Maybe SandboxMechanism
    }
    deriving (Show, Eq, Ord)

defaultToolRuntimeConfig :: ToolRuntimeConfig
defaultToolRuntimeConfig = ToolRuntimeConfig Nothing Nothing Nothing

parseRunAsMechanism :: String -> Either String RunAsMechanism
parseRunAsMechanism s = case s of
    "setpriv" -> Right RunAsSetpriv
    "sudo" -> Right RunAsSudo
    "runuser" -> Right RunAsRunuser
    _ -> Left ("unknown run-as mechanism " <> show s <> ", allowed: setpriv, sudo, runuser")

parseSandboxMechanism :: String -> Either String SandboxMechanism
parseSandboxMechanism s = case s of
    "bwrap" -> Right SandboxBwrap
    "landlock" -> Right SandboxLandlock
    "auto" -> Right SandboxAuto
    _ -> Left ("unknown sandbox mechanism " <> show s <> ", allowed: bwrap, landlock, auto")

{- | The process-wide operator configuration, set once at startup from the
command line ('setToolRuntimeConfig'). It is process-wide like the
environment agents-exe hands its tools: every entry point (TUI, run, serve,
MCP server, tool-call) loads and runs bash tools through it without each
having to carry it. Code that wants another configuration (tests) passes one
explicitly to the @...With@ variants of the loaders and runners.
-}
toolRuntimeConfigRef :: IORef ToolRuntimeConfig
toolRuntimeConfigRef = unsafePerformIO (newIORef defaultToolRuntimeConfig)
{-# NOINLINE toolRuntimeConfigRef #-}

setToolRuntimeConfig :: ToolRuntimeConfig -> IO ()
setToolRuntimeConfig = writeIORef toolRuntimeConfigRef

getToolRuntimeConfig :: IO ToolRuntimeConfig
getToolRuntimeConfig = readIORef toolRuntimeConfigRef

-------------------------------------------------------------------------------
-- Load-time checks
-------------------------------------------------------------------------------

{- | Checks a @oneshot@ tool's @runtime@ against what this runtime implements
and what the operator configured. Returns the problems (empty when valid),
each naming the field.
-}
validateScriptRuntime :: ToolRuntimeConfig -> ScriptRuntime -> [String]
validateScriptRuntime cfg r =
    mconcat
        [ case r.runtimeEnvdir of
            Nothing -> []
            Just name ->
                [ "runtime.envdir " <> show name <> " needs the operator's --envdir-root (there is no implicit location)"
                | Nothing <- [cfg.envdirRoot]
                ]
                    <> [ "runtime.envdir " <> show name <> " must be a single directory name (no '/', not '.' or '..')"
                       | not (validEnvdirName name)
                       ]
        , [ "runtime.environment: invalid variable name " <> show k
          | k <- Map.keys r.runtimeEnvironment
          , not (validVarName k)
          ]
        , [ "runtime.rundir " <> show d <> " must be a relative path inside the tool's directory (no '..')"
          | Just d <- [r.runtimeRundir]
          , not (validRelativePath d)
          ]
        , case r.runtimeRunAs of
            Nothing -> []
            Just ra ->
                [ "runtime.run_as needs a run-as mechanism configured by the operator (--run-as-mechanism setpriv|sudo|runuser)"
                | Nothing <- [cfg.runAsMechanism]
                ]
                    <> [ "runtime.run_as: invalid user or group name " <> show n
                       | n <- ra.runAsUser : maybe [] pure ra.runAsGroup
                       , not (validAccountName n)
                       ]
        , case r.runtimeSandbox of
            Nothing -> []
            Just sb ->
                ( case cfg.sandboxMechanism of
                    Nothing -> ["runtime.sandbox needs a sandbox mechanism configured by the operator (--sandbox bwrap|landlock|auto); it never runs unconfined"]
                    Just SandboxLandlock -> landlockFieldProblems sb
                    Just _ -> []
                )
                    <> [ "runtime.sandbox.fs: empty path"
                       | p <- sb.sandboxRead <> sb.sandboxWrite
                       , Text.null p
                       ]
        , [ "runtime.timeout." <> name <> " must be a positive number of seconds"
          | (name, Just s) <- [("start", r.runtimeTimeout.timeoutStart), ("stop", r.runtimeTimeout.timeoutStop), ("idle", r.runtimeTimeout.timeoutIdle)]
          , s <= 0
          ]
        , [ "runtime.timeout.idle only applies to a service (not implemented yet)"
          | isJust r.runtimeTimeout.timeoutIdle
          ]
        , [ "runtime.on_exit only applies to a service (not implemented yet)"
          | isJust r.runtimeOnExit
          ]
        , [ "runtime.cancel {\"send\": ...} needs an open stdin (process.input, not implemented yet); use \"SIGTERM\" or \"SIGINT\""
          | Just (CancelSend _) <- [r.runtimeCancel]
          ]
        ]
  where
    validEnvdirName n = not (Text.null n) && not (Text.any (== '/') n) && n /= "." && n /= ".."
    validVarName k = not (Text.null k) && Text.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_') k && not (isDigit (Text.head k))
    validRelativePath p =
        not (Text.null p) && not (isAbsolute (Text.unpack p)) && ".." `notElem` splitDirectories (Text.unpack p)
    validAccountName n =
        not (Text.null n) && Text.head n /= '-' && Text.all (\c -> isAlphaNum c || c `elem` ("._-" :: String)) n

-------------------------------------------------------------------------------
-- Sandbox mechanism resolution
-------------------------------------------------------------------------------

{- | The fields a Landlock sandbox cannot honour, each naming the field.
Landlock only denies access: it cannot take the network away (on kernels
with network rules it denies TCP bind and connect, but UDP, unix sockets and
raw sockets stay open, which is not @net: none@) and cannot give a private
@/tmp@. Such a tool fails to load under @--sandbox landlock@ and skips
Landlock under @--sandbox auto@; it is never weakened.
-}
landlockFieldProblems :: Sandbox -> [String]
landlockFieldProblems sb =
    [ "runtime.sandbox.net \"none\" cannot be enforced by landlock (it denies TCP bind/connect only; use --sandbox bwrap)"
    | sb.sandboxNet == NetNone
    ]
        <> [ "runtime.sandbox.tmp \"private\" cannot be enforced by landlock (it has no private /tmp; use --sandbox bwrap)"
           | sb.sandboxTmp == TmpPrivate
           ]

-- | What this host can do, probed once per process ('getSandboxSupport').
data SandboxSupport = SandboxSupport
    { supportBwrap :: Bool
    -- ^ @bwrap@ is on the PATH and can build a sandbox here.
    , supportLandlock :: Either String Int
    -- ^ The kernel's Landlock ABI version, or why there is none.
    }
    deriving (Show, Eq)

-- | The mechanism that enforces one tool's @sandbox@.
data ResolvedSandbox = UseBwrap | UseLandlock
    deriving (Show, Eq, Ord)

{- | Picks the mechanism for one tool from the operator's choice and what the
host supports. The tool's directory must be absolute and canonical: Landlock
cannot make a path read-only beneath one it lets write, so a tool living
under @/tmp@ with @tmp: shared@ would have a writable directory, which
bubblewrap does not allow; that is refused rather than weakened.

@--sandbox bwrap@ is not probed (as before: a failing bubblewrap fails the
call). @--sandbox auto@ is bubblewrap when it works here, else Landlock when
the kernel has it and it can honour every field, else a load error giving
both reasons.
-}
resolveSandboxMechanism :: SandboxSupport -> SandboxMechanism -> FilePath -> Sandbox -> Either String ResolvedSandbox
resolveSandboxMechanism support mech toolDir sb = case mech of
    SandboxBwrap -> Right UseBwrap
    SandboxLandlock -> landlock
    SandboxAuto
        | support.supportBwrap -> Right UseBwrap
        | otherwise -> case landlock of
            Right m -> Right m
            Left why -> Left ("runtime.sandbox: --sandbox auto found no mechanism for this tool: bwrap cannot build a sandbox here, and " <> why)
  where
    landlock = case (landlockFieldProblems sb <> underTmp, support.supportLandlock) of
        (problem : _, _) -> Left problem
        ([], Left why) -> Left ("runtime.sandbox: landlock: " <> why)
        ([], Right _) -> Right UseLandlock
    underTmp =
        [ "runtime.sandbox: the tool's directory " <> toolDir <> " is under /tmp, which tmp: shared makes writable; landlock cannot keep it read-only (move the tool or use --sandbox bwrap)"
        | sb.sandboxTmp == TmpShared
        , take 2 (splitDirectories toolDir) == ["/", "tmp"]
        ]

supportRef :: IORef (Maybe SandboxSupport)
supportRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE supportRef #-}

{- | Probes what the host supports, once per process: whether @bwrap@ can
build a sandbox (the options agents-exe uses, with a network namespace), and
the kernel's Landlock ABI.
-}
getSandboxSupport :: IO SandboxSupport
getSandboxSupport = do
    cached <- readIORef supportRef
    case cached of
        Just s -> pure s
        Nothing -> do
            bwrap <- do
                found <- findExecutable "bwrap"
                case found of
                    Nothing -> pure False
                    Just exe -> do
                        r <- try (readProcessWithExitCode exe probeArgs "")
                        pure $ case r of
                            Right (ExitSuccess, _, _) -> True
                            Right _ -> False
                            Left (_ :: IOException) -> False
            abi <- landlockAbi
            let s = SandboxSupport bwrap abi
            writeIORef supportRef (Just s)
            pure s
  where
    probeArgs = ["--die-with-parent", "--unshare-pid", "--unshare-ipc", "--unshare-uts", "--unshare-net", "--ro-bind", "/", "/", "--proc", "/proc", "--dev", "/dev", "true"]

{- | 'resolveSandboxMechanism' for a tool at the given path, against the
host's support (probed only when the choice depends on it).
-}
resolveSandbox :: ToolRuntimeConfig -> FilePath -> Sandbox -> IO (Either String ResolvedSandbox)
resolveSandbox cfg path sb = case cfg.sandboxMechanism of
    Nothing -> pure (Left "runtime.sandbox: no sandbox mechanism configured")
    Just SandboxBwrap -> pure (Right UseBwrap)
    Just mech -> do
        support <- getSandboxSupport
        toolDir <- canonicalToolDir path
        pure (resolveSandboxMechanism support mech toolDir sb)

canonicalToolDir :: FilePath -> IO FilePath
canonicalToolDir path = do
    r <- try (canonicalizePath (takeDirectory path))
    case r of
        Right d -> pure d
        Left (_ :: IOException) -> makeAbsolute (takeDirectory path)

{- | The Landlock rules for a sandbox, the counterpart of 'bwrapArgs': read
and execute on the system's executables and libraries, @/proc@ and the
tool's directory, read on the declared @fs.read@, every access on the
declared @fs.write@ and (@tmp: shared@) on @/tmp@, and the usual character
devices. Relative paths are relative to the (absolute) tool directory.
-}
landlockRules :: FilePath -> Sandbox -> [LandlockRule]
landlockRules toolDir sb =
    mconcat
        [ [LandlockRule AccessRead p True | p <- systemPaths <> ["/proc", "/dev/random", "/dev/urandom"]]
        , [LandlockRule AccessWrite p True | p <- ["/dev/null", "/dev/zero", "/dev/full"]]
        , [LandlockRule AccessWrite "/tmp" True | sb.sandboxTmp == TmpShared]
        , [LandlockRule AccessRead toolDir False]
        , [LandlockRule AccessRead p False | p <- resolve sb.sandboxRead]
        , [LandlockRule AccessWrite p False | p <- resolve sb.sandboxWrite]
        ]
  where
    systemPaths = ["/usr", "/bin", "/sbin", "/lib", "/lib32", "/lib64", "/etc", "/nix/store"]
    resolve = mapMaybe (\p -> if Text.null p then Nothing else Just (absolute (Text.unpack p)))
    absolute p = if isAbsolute p then p else toolDir </> p

-------------------------------------------------------------------------------
-- Launching
-------------------------------------------------------------------------------

-- | What to start for one call of a tool with a @runtime@.
data Launch = Launch
    { launchCommand :: FilePath
    , launchArgs :: [String]
    , launchEnv :: [(String, String)]
    , launchCwd :: Maybe FilePath
    }
    deriving (Show, Eq)

{- | Builds the command for one call: the tool's command and arguments,
wrapped in the sandbox, wrapped in the run-as mechanism (the sandbox is set
up for the process that runs as that user), with the environment completed
by the envdir and @environment@, and the rundir created.

The environment, from lowest to highest precedence: the given base (the
agents-exe environment and the session context variables), the envdir, the
literal @environment@, then the tool's own @env@-mode arguments.
-}
prepareLaunch ::
    ToolRuntimeConfig ->
    -- | The tool's path.
    FilePath ->
    ScriptRuntime ->
    -- | Arguments after the tool's path (@run ...@).
    [String] ->
    -- | Base environment.
    [(String, String)] ->
    -- | The tool's @env@-mode arguments.
    [(String, String)] ->
    IO (Either String Launch)
prepareLaunch cfg path r args baseEnv argEnv = do
    let toolDir = takeDirectory path
    envdirVars <- case r.runtimeEnvdir of
        Nothing -> pure (Right [])
        Just name -> case cfg.envdirRoot of
            Nothing -> pure (Left "runtime.envdir: no --envdir-root configured")
            Just root -> readEnvdir (root </> Text.unpack name)
    let rundir = fmap (\d -> toolDir </> Text.unpack d) r.runtimeRundir
    mkRundir <- case rundir of
        Nothing -> pure (Right ())
        Just d -> either (\e -> Left ("runtime.rundir: cannot create " <> d <> ": " <> show (e :: IOException))) Right <$> try (createDirectoryIfMissing True d)
    runAs <- case r.runtimeRunAs of
        Nothing -> pure (Right [])
        Just ra -> case cfg.runAsMechanism of
            Nothing -> pure (Left "runtime.run_as: no run-as mechanism configured")
            Just m -> runAsPrefix m ra
    sandboxing <- case r.runtimeSandbox of
        Nothing -> pure (Right (path, args))
        Just sb -> do
            resolved <- resolveSandbox cfg path sb
            case resolved of
                Left problem -> pure (Left problem)
                Right UseBwrap -> pure (Right ("bwrap", bwrapArgs toolDir rundir sb <> ["--", path] <> args))
                Right UseLandlock -> do
                    -- agents-exe itself is the launcher (see
                    -- "System.Agents.Tools.Landlock"); paths are made
                    -- absolute since the rundir becomes the working directory.
                    self <- getExecutablePath
                    absTool <- makeAbsolute path
                    absDir <- makeAbsolute toolDir
                    pure (Right (self, landlockLauncherArgs (landlockRules absDir sb) absTool args))
    pure $ do
        vars <- envdirVars
        () <- mkRundir
        prefix <- runAs
        sandboxed <- sandboxing
        let literal = [(Text.unpack k, Text.unpack v) | (k, v) <- Map.toList r.runtimeEnvironment]
            env = overrideEnvironment baseEnv (vars <> fmap (fmap Just) literal <> fmap (fmap Just) argEnv)
            (cmd, cmdArgs) = case prefix of
                [] -> sandboxed
                (p : ps) -> (p, ps <> [fst sandboxed] <> snd sandboxed)
        pure (Launch cmd cmdArgs env rundir)

{- | Applies overrides to an environment, later ones winning: @Just v@ sets
a variable, @Nothing@ removes it.
-}
overrideEnvironment :: [(String, String)] -> [(String, Maybe String)] -> [(String, String)]
overrideEnvironment base overrides =
    [(k, v) | (k, v) <- base, k `Map.notMember` final] <> [(k, v) | (k, Just v) <- Map.toList final]
  where
    final = Map.fromList overrides

{- | Reads an envdir the daemontools way: one file per variable, the file
name is the name. A non-empty file's first line, with trailing spaces and
tabs removed and NULs turned into newlines, is the value; an empty file
removes the variable. Subdirectories and names containing @=@ are skipped.
-}
readEnvdir :: FilePath -> IO (Either String [(String, Maybe String)])
readEnvdir dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure (Left ("runtime.envdir: " <> dir <> " is not a directory"))
        else do
            result <- try $ do
                names <- List.sort . List.filter (notElem '=') <$> listDirectory dir
                fmap catMaybes . forM names $ \name -> do
                    let p = dir </> name
                    isFile <- doesFileExist p
                    if not isFile
                        then pure Nothing
                        else do
                            content <- ByteString.readFile p
                            pure (Just (name, envdirValue content))
            pure (either (\e -> Left ("runtime.envdir: cannot read " <> dir <> ": " <> show (e :: IOException))) Right result)
  where
    envdirValue content
        | ByteString.null content = Nothing
        | otherwise =
            let firstLine = ByteStringChar8.takeWhile (/= '\n') content
                trimmed = ByteStringChar8.dropWhileEnd (`elem` (" \t" :: String)) firstLine
             in Just (Text.unpack (Text.map (\c -> if c == '\0' then '\n' else c) (Text.decodeUtf8With lenientDecode trimmed)))

{- | The command prefix that runs what follows as the declared user. Each
mechanism keeps the environment agents-exe prepared (it holds the envdir):
@sudo@ needs a sudoers rule allowing it (@SETENV@).
-}
runAsPrefix :: RunAsMechanism -> RunAs -> IO (Either String [String])
runAsPrefix m ra = case m of
    RunAsSudo -> pure (Right (["sudo", "-n", "--preserve-env", "-u", user] <> groupOpt "-g" <> ["--"]))
    RunAsRunuser -> pure (Right (["runuser", "-p", "-u", user] <> groupOpt "-g" <> ["--"]))
    RunAsSetpriv -> do
        -- setpriv needs an explicit group: the declared one, else the user's primary group.
        mGroup <- case ra.runAsGroup of
            Just g -> pure (Right (Text.unpack g))
            Nothing -> do
                r <- try (getUserEntryForName user >>= getGroupEntryForID . userGroupID)
                pure $ case r of
                    Left e -> Left ("runtime.run_as: cannot resolve the primary group of " <> show user <> ": " <> show (e :: IOException))
                    Right g -> Right (groupName g)
        pure $ fmap (\g -> ["setpriv", "--reuid=" <> user, "--regid=" <> g, "--init-groups", "--"]) mGroup
  where
    user = Text.unpack ra.runAsUser
    groupOpt flag = maybe [] (\g -> [flag, Text.unpack g]) ra.runAsGroup

{- | The bubblewrap options for a sandbox: a mount namespace where only the
system's executables and libraries (read-only), the tool's directory
(read-only), and the declared paths exist; @net: none@ is a network
namespace and @tmp: private@ a fresh tmpfs. The process dies with
agents-exe. Relative paths are relative to the tool's directory.
-}
bwrapArgs :: FilePath -> Maybe FilePath -> Sandbox -> [String]
bwrapArgs toolDir mRundir sb =
    mconcat
        [ ["--die-with-parent", "--unshare-pid", "--unshare-ipc", "--unshare-uts"]
        , concat [["--ro-bind-try", p, p] | p <- systemPaths]
        , ["--proc", "/proc", "--dev", "/dev"]
        , -- Later mounts cover earlier ones: /tmp first, so a tool living
          -- under /tmp stays read-only, then the declared paths.
          case sb.sandboxTmp of
            TmpShared -> ["--bind-try", "/tmp", "/tmp"]
            TmpPrivate -> ["--tmpfs", "/tmp"]
        , ["--ro-bind", toolDir, toolDir]
        , concat [["--ro-bind", p, p] | p <- resolve sb.sandboxRead]
        , concat [["--bind", p, p] | p <- resolve sb.sandboxWrite]
        , ["--unshare-net" | sb.sandboxNet == NetNone]
        , maybe [] (\d -> ["--chdir", d]) mRundir
        ]
  where
    systemPaths = ["/usr", "/bin", "/sbin", "/lib", "/lib32", "/lib64", "/etc", "/nix/store"]
    resolve = mapMaybe (\p -> if Text.null p then Nothing else Just (absolute (Text.unpack p)))
    absolute p = if isAbsolute p then p else toolDir </> p

-- | The signal sent first on cancel or timeout.
cancelSignal :: ScriptRuntime -> Signal
cancelSignal r = case r.runtimeCancel of
    Just CancelSigint -> sigINT
    _ -> sigTERM

-- | Microseconds between the cancel signal and @SIGKILL@, when declared.
stopGraceMicros :: ScriptRuntime -> Maybe Int
stopGraceMicros r = fmap toMicros r.runtimeTimeout.timeoutStop

-- | Microseconds a @oneshot@ has to complete, or a @stream@ to reach its @ready@, when declared.
startTimeoutMicros :: ScriptRuntime -> Maybe Int
startTimeoutMicros r = fmap toMicros r.runtimeTimeout.timeoutStart

toMicros :: Double -> Int
toMicros s = round (s * 1000000)
