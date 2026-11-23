{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The @check@ verb of a v2 bash tool (@todos/bash-tools-v2.md@ §1.5, D9):
whether a tool works /here, now/ is found by running a cheap, read-only
self-test under the tool's real @runtime@, never by looking it up.

A v2 tool that says @"check": true@ in @describe@ answers @./tool check@ with
one JSON document, @{"ok": bool, "detail": ..., "fix": ...}@, and exits 0
when it is usable. The probe runs it exactly as a call would run (same
envdir, environment, rundir, run_as, sandbox, @timeout.start@), so a probe
that passes proves the sandbox and the user work too.

A probe's 'ProbeStatus' is:

* 'ProbeOk': @check@ exited 0 and printed @"ok": true@.
* 'ProbeFailed': anything else @check@ did (non-zero exit, @"ok": false@,
  no JSON document, a timeout, a launch error), with 'probeDetail' and
  'probeFix'.
* 'ProbeUnverified': the tool has no @check@ (a v1 tool, or a v2 tool that
  cannot be checked without side effects). It loaded and @describe@
  answered, nothing more; never reported ok.
* 'ProbeSkipped': the operator's sandbox mechanism cannot enforce what the
  tool declares here (§1.4), so @check@ was not run.

A tool binary that is not a backend of a capability
("System.Agents.Tools.Capability") is reported as a capability of its own
with a single backend ('CapabilityReport').
-}
module System.Agents.Tools.ScriptCheck (
    ProbeStatus (..),
    ProbeResult (..),
    SandboxReport (..),
    CapabilityReport (..),
    probeStatusText,
    defaultCheckTimeoutSeconds,
    interpretCheckOutput,
    probeScript,
    probeScriptWith,
    toolBinaries,
    capabilityReport,
    hasActiveBackend,
    renderProbeLine,
) where

import Control.Exception (IOException, try)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Posix.Signals (sigTERM)
import System.Process (CreateProcess (..), proc)
import System.Timeout (timeout)

import System.Agents.Tools.Bash (runProcessReportingEventsStopping)
import System.Agents.Tools.ScriptRuntime
import System.Agents.Tools.ScriptTypes (ScriptDescription (..), ScriptInfo (..), scriptRuntimeOf)

data ProbeStatus = ProbeOk | ProbeFailed | ProbeUnverified | ProbeSkipped
    deriving (Show, Eq, Ord)

probeStatusText :: ProbeStatus -> Text
probeStatusText s = case s of
    ProbeOk -> "ok"
    ProbeFailed -> "failed"
    ProbeUnverified -> "unverified"
    ProbeSkipped -> "skipped"

instance Aeson.ToJSON ProbeStatus where
    toJSON = Aeson.String . probeStatusText

-- | How a tool's @sandbox@ is enforced here (what @--sandbox auto@ resolved to).
data SandboxReport = SandboxReport
    { sandboxConfigured :: Text
    -- ^ The operator's @--sandbox@ (@bwrap@, @landlock@, @auto@, or @none@).
    , sandboxResolved :: Either Text Text
    -- ^ The mechanism used (@bwrap@ or @landlock@), or why there is none.
    }
    deriving (Show, Eq)

instance Aeson.ToJSON SandboxReport where
    toJSON s =
        Aeson.object $
            ["configured" .= s.sandboxConfigured]
                <> either (\why -> ["problem" .= why]) (\m -> ["mechanism" .= m]) s.sandboxResolved

-- | One probed tool binary.
data ProbeResult = ProbeResult
    { probeName :: Text
    -- ^ The binary's slug (a subcommand tool's slug without the subcommand).
    , probePath :: FilePath
    , probeTools :: [Text]
    -- ^ The tool names the model sees for this binary.
    , probeStatus :: ProbeStatus
    , probeMillis :: Maybe Int
    -- ^ How long @check@ ran (only when it ran).
    , probeDetail :: Maybe Text
    , probeFix :: Maybe Text
    , probeSandbox :: Maybe SandboxReport
    }
    deriving (Show, Eq)

instance Aeson.ToJSON ProbeResult where
    toJSON p =
        Aeson.object $
            [ "name" .= p.probeName
            , "path" .= p.probePath
            , "tools" .= p.probeTools
            , "status" .= p.probeStatus
            ]
                <> maybe [] (\ms -> ["ms" .= ms]) p.probeMillis
                <> maybe [] (\d -> ["detail" .= d]) p.probeDetail
                <> maybe [] (\f -> ["fix" .= f]) p.probeFix
                <> maybe [] (\s -> ["sandbox" .= s]) p.probeSandbox

{- | The @check --probe --json@ shape of §1.5, per capability: its backends
in the order they are tried, and the active one (by name).
-}
data CapabilityReport = CapabilityReport
    { capabilityName :: Text
    , capabilityActive :: Maybe Text
    , capabilityBackends :: [ProbeResult]
    }
    deriving (Show, Eq)

instance Aeson.ToJSON CapabilityReport where
    toJSON c =
        Aeson.object
            [ "capability" .= c.capabilityName
            , "active" .= c.capabilityActive
            , "backends" .= c.capabilityBackends
            ]

{- | The active backend is the first @ok@ one, else the first @unverified@
one, else none (§1.5).
-}
capabilityReport :: Text -> [ProbeResult] -> CapabilityReport
capabilityReport name backends =
    CapabilityReport name active backends
  where
    active = case (firstWith ProbeOk, firstWith ProbeUnverified) of
        (Just p, _) -> Just p.probeName
        (Nothing, Just p) -> Just p.probeName
        _ -> Nothing
    firstWith s = List.find (\p -> p.probeStatus == s) backends

hasActiveBackend :: CapabilityReport -> Bool
hasActiveBackend c = c.capabilityActive /= Nothing

-- | How long a @check@ may run when the tool declares no @timeout.start@.
defaultCheckTimeoutSeconds :: Double
defaultCheckTimeoutSeconds = 30

{- | The tool binaries behind a list of loaded tools: a subcommand tool is
loaded as one tool per subcommand, but is checked once. Keeps the first
description of each path, with the model-facing names of all its tools, in
order of first appearance.
-}
toolBinaries :: [ScriptDescription] -> [(ScriptDescription, [Text])]
toolBinaries descs =
    [ (d, fromMaybe [] (Map.lookup d.scriptPath names))
    | d <- List.nubBy (\a b -> a.scriptPath == b.scriptPath) descs
    ]
  where
    names = Map.fromListWith (flip (<>)) [(d.scriptPath, [d.scriptInfo.scriptSlug]) | d <- descs]

-- | The binary's own slug, without a subcommand suffix.
binarySlug :: ScriptInfo -> Text
binarySlug info = case info.scriptSubcommand of
    Just sub
        | Just base <- Text.stripSuffix ("_" <> sub) info.scriptSlug -> base
    _ -> info.scriptSlug

{- | Reads what @check@ did: its exit code, standard output and standard
error. Returns the status, the detail and the fix.
-}
interpretCheckOutput :: ExitCode -> ByteString -> ByteString -> (ProbeStatus, Maybe Text, Maybe Text)
interpretCheckOutput code out err =
    case (code, document) of
        (ExitSuccess, Just (Just True, detail, fix)) -> (ProbeOk, detail, fix)
        (ExitSuccess, Just (Just False, detail, fix)) -> (ProbeFailed, Just (fromMaybe "check answered \"ok\": false" detail), fix)
        (ExitSuccess, _) -> (ProbeFailed, Just "check exited 0 without a JSON document {\"ok\": true}", Nothing)
        (ExitFailure n, Just (_, detail, fix)) -> (ProbeFailed, Just (fromMaybe (exitedWith n) detail), fix)
        (ExitFailure n, Nothing) -> (ProbeFailed, Just (exitedWith n <> stderrTail), Nothing)
  where
    document = case Aeson.decodeStrict (ByteStringChar8.strip out) of
        Just (Aeson.Object o) ->
            Just
                ( case KeyMap.lookup "ok" o of
                    Just (Aeson.Bool b) -> Just b
                    _ -> Nothing
                , textField "detail" o
                , textField "fix" o
                )
        _ -> Nothing
    textField k o = case KeyMap.lookup k o of
        Just (Aeson.String t) | not (Text.null t) -> Just t
        _ -> Nothing
    exitedWith n = "check exited with code " <> Text.pack (show n)
    stderrTail =
        let t = Text.strip (Text.decodeUtf8With lenientDecode err)
         in if Text.null t then "" else ": " <> Text.takeEnd 400 t

-- | 'probeScriptWith' under the process-wide operator configuration.
probeScript :: [Text] -> ScriptDescription -> IO ProbeResult
probeScript names desc = do
    cfg <- getToolRuntimeConfig
    probeScriptWith cfg names desc

{- | Probes one tool binary (given the model-facing names of its tools): runs
@./tool check@ under its @runtime@ when it declares @check@, and reports how
its @sandbox@ is enforced.
-}
probeScriptWith :: ToolRuntimeConfig -> [Text] -> ScriptDescription -> IO ProbeResult
probeScriptWith cfg names desc = do
    let info = desc.scriptInfo
        path = desc.scriptPath
        mRuntime = scriptRuntimeOf info
        base = ProbeResult (binarySlug info) path names ProbeUnverified Nothing Nothing Nothing Nothing
    sandboxReport <- case mRuntime >>= (.runtimeSandbox) of
        Nothing -> pure Nothing
        Just sb -> do
            resolved <- resolveSandbox cfg path sb
            pure $
                Just
                    SandboxReport
                        { sandboxConfigured = maybe "none" mechanismText cfg.sandboxMechanism
                        , sandboxResolved = either (Left . Text.pack) (Right . resolvedText) resolved
                        }
    let withSandbox = base{probeSandbox = sandboxReport}
    case sandboxReport of
        Just SandboxReport{sandboxResolved = Left why} ->
            pure withSandbox{probeStatus = ProbeSkipped, probeDetail = Just why, probeFix = Just "configure a --sandbox mechanism that can enforce every declared field"}
        _
            | info.scriptProtocol < 2 || not info.scriptCheck ->
                pure withSandbox{probeDetail = Just "the tool declares no check"}
            | otherwise -> runCheck cfg desc mRuntime withSandbox
  where
    mechanismText m = case m of
        SandboxBwrap -> "bwrap"
        SandboxLandlock -> "landlock"
        SandboxAuto -> "auto"
    resolvedText r = case r of
        UseBwrap -> "bwrap"
        UseLandlock -> "landlock"

runCheck :: ToolRuntimeConfig -> ScriptDescription -> Maybe ScriptRuntime -> ProbeResult -> IO ProbeResult
runCheck cfg desc mRuntime base = do
    let path = desc.scriptPath
    baseEnv <- getEnvironment
    prepared <- case mRuntime of
        Nothing -> pure (Right ((proc path ["check"]){env = Just baseEnv}, (sigTERM, defaultGrace), defaultCheckTimeoutSeconds))
        Just rt -> do
            launch <- prepareLaunch cfg path rt ["check"] baseEnv []
            pure $ case launch of
                Left problem -> Left problem
                Right l ->
                    Right
                        ( (proc l.launchCommand l.launchArgs){env = Just l.launchEnv, cwd = l.launchCwd}
                        , (cancelSignal rt, fromMaybe defaultGrace (stopGraceMicros rt))
                        , fromMaybe defaultCheckTimeoutSeconds rt.runtimeTimeout.timeoutStart
                        )
    case prepared of
        Left problem ->
            pure base{probeStatus = ProbeFailed, probeDetail = Just (Text.pack problem)}
        Right (process, stopping, secs) -> do
            t0 <- getCurrentTime
            r <- try (timeout (micros secs) (runProcessReportingEventsStopping stopping (const (pure ())) Nothing process ""))
            t1 <- getCurrentTime
            let ms = Just (round (1000 * realToFrac (diffUTCTime t1 t0) :: Double))
            pure $ case r of
                Left (e :: IOException) ->
                    base{probeStatus = ProbeFailed, probeMillis = ms, probeDetail = Just ("check could not start: " <> Text.pack (show e))}
                Right Nothing ->
                    base{probeStatus = ProbeFailed, probeMillis = ms, probeDetail = Just ("check did not answer within " <> Text.pack (show secs) <> "s")}
                Right (Just (code, out, err)) ->
                    let (status, detail, fix) = interpretCheckOutput code out err
                     in base{probeStatus = status, probeMillis = ms, probeDetail = detail, probeFix = fix}
  where
    defaultGrace = 2000000
    micros s = max 1 (round (s * 1000000))

-- | One line of the text report: status, name, time, sandbox, detail and fix.
renderProbeLine :: ProbeResult -> Text
renderProbeLine p =
    Text.intercalate "  " $
        catMaybes
            [ Just (Text.justifyLeft 10 ' ' (probeStatusText p.probeStatus))
            , Just p.probeName
            , fmap (\ms -> "(" <> Text.pack (show ms) <> "ms)") p.probeMillis
            , fmap sandboxText p.probeSandbox
            , p.probeDetail
            , fmap ("fix: " <>) p.probeFix
            ]
  where
    sandboxText :: SandboxReport -> Text
    sandboxText s = case s.sandboxResolved of
        Right m -> "[sandbox " <> s.sandboxConfigured <> " -> " <> m <> "]"
        Left _ -> "[sandbox " <> s.sandboxConfigured <> ": none applies]"
