{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Capabilities with ordered backends (@todos/bash-tools-v2.md@ §1.5, D9).

A capability is one tool the model sees (@bash_<capability>@), served by
the first working of an ordered list of tool binaries, its /backends/. The
/active backend/ is the first one whose @check@ probe is @ok@, else the
first one that cannot be checked (@unverified@), else none.

* Backends are named by their file name (@tools/web/read-jina@ is
  @read-jina@); the operator's @--backend web_read=read-curl@ moves the
  named backend first, and a name no backend has is ignored, so a stale
  override never hides a working backend ('orderBackends').
* Every backend must be a single-command tool (no @commands@), not a
  @service@, and declare the same @args@: names, types and arity, so the
  model's schema does not depend on which one is active
  ('validateBackends'). The argument descriptions and how each binary
  receives them (positional, flag, env) may differ.
* A backend that does not load here (missing binary, @describe@ failing) is
  not a configuration error: it is a @failed@ backend, which is what an
  ordered list of backends is for.
* The active backend is chosen once, when the agent's tools are loaded
  ('selectActiveBackend'): probes run in order and stop at the first @ok@,
  so a working preferred backend costs one probe. Nothing is re-probed until
  the agent is loaded again.
* A failed call does not retry the next backend: the model sees the failure
  and which backend produced it ('System.Agents.Tools.Bash.ScriptBackendError').
-}
module System.Agents.Tools.Capability (
    Backend (..),
    backendNameOf,
    validCapabilityName,
    loadBackendsWith,
    orderBackends,
    validateBackends,
    selectActiveBackend,
    probeCapability,
    capabilityTool,
    noActiveBackendMessage,
) where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, when)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Prod.Tracer (Tracer)
import System.FilePath (takeFileName)

import System.Agents.Tools.Bash (LoadTrace, loadScriptWith)
import System.Agents.Tools.ScriptCheck
import System.Agents.Tools.ScriptProcess (ProcessMode (..), ScriptProcess (..))
import System.Agents.Tools.ScriptRuntime (ToolRuntimeConfig (..))
import System.Agents.Tools.ScriptTypes (ScriptArg (..), ScriptDescription (..), ScriptInfo (..), scriptProcessOf)

-- | One backend of a capability, as loaded here.
data Backend = Backend
    { backendName :: Text
    -- ^ The binary's file name.
    , backendPath :: FilePath
    , backendLoaded :: Either String ScriptDescription
    -- ^ Its @describe@, or why it did not load.
    }
    deriving (Show)

backendNameOf :: FilePath -> Text
backendNameOf = Text.pack . takeFileName

-- | A capability name is a tool slug: @[a-zA-Z0-9_-]+@.
validCapabilityName :: Text -> Bool
validCapabilityName n = not (Text.null n) && Text.all ok n
  where
    ok c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '-'

-- | Loads every backend under the given operator configuration.
loadBackendsWith :: ToolRuntimeConfig -> Tracer IO LoadTrace -> [FilePath] -> IO [Backend]
loadBackendsWith cfg tracer paths =
    forM paths $ \path -> do
        loaded <- try (loadScriptWith cfg tracer path)
        pure $
            Backend (backendNameOf path) path $ case loaded of
                Left (e :: IOException) -> Left (show e)
                Right (Left err) -> Left (show err)
                Right (Right desc) -> Right desc

{- | The backends in the order they are tried: the operator's override for
this capability (the last one given wins) moves the named backend first.
-}
orderBackends :: [(Text, Text)] -> Text -> [Backend] -> [Backend]
orderBackends overrides capability backends =
    case [b | (c, b) <- reverse overrides, c == capability] of
        (wanted : _)
            | (named@(_ : _), rest) <- List.partition (\b -> b.backendName == wanted) backends -> named <> rest
        _ -> backends

{- | The configuration errors of a capability: an invalid name, no backend,
two backends with the same name, a backend with subcommands or that is a
@service@, backends whose @args@ differ. Backends that did not load are not
checked here.
-}
validateBackends :: Text -> [Backend] -> Either String ()
validateBackends capability backends = do
    unless (validCapabilityName capability) $
        Left ("capability name " <> show capability <> " must match [a-zA-Z0-9_-]+")
    when (null backends) $
        Left (what <> " has no backends")
    case [n | (n : _ : _) <- List.group (List.sort (map (.backendName) backends))] of
        [] -> pure ()
        dups -> Left (what <> " has several backends named " <> Text.unpack (Text.intercalate ", " dups) <> " (a backend is named by its file name)")
    let loaded = [(b, d) | b <- backends, Right d <- [b.backendLoaded]]
    mapM_ (uncurry checkShape) loaded
    case loaded of
        [] -> pure ()
        ((b0, d0) : others) ->
            case [b | (b, d) <- others, argsSignature d /= argsSignature d0] of
                [] -> pure ()
                (b : _) ->
                    Left $
                        what
                            <> ": backend "
                            <> Text.unpack b.backendName
                            <> " does not declare the same args as "
                            <> Text.unpack b0.backendName
                            <> " (names, types and arity must match)"
  where
    what = "capability " <> Text.unpack capability
    checkShape :: Backend -> ScriptDescription -> Either String ()
    checkShape b d = do
        unless (null d.scriptInfo.scriptCommands) $
            Left (what <> ": backend " <> Text.unpack b.backendName <> " has subcommands; a backend must be a single-command tool")
        case scriptProcessOf d.scriptInfo of
            Just p
                | p.processMode == ModeService ->
                    Left (what <> ": backend " <> Text.unpack b.backendName <> " is a service; a backend must be a oneshot or stream tool")
            _ -> pure ()

-- | What of a tool's @args@ the model's schema depends on, in name order.
argsSignature :: ScriptDescription -> [(Text, Text, Text)]
argsSignature d =
    List.sort
        [ (a.argName, a.argBackingTypeString, Text.pack (show a.argTypeArity))
        | a <- d.scriptInfo.scriptArgs
        ]

-- | Probes one backend; a backend that did not load is @failed@.
probeBackend :: ToolRuntimeConfig -> Text -> Backend -> IO ProbeResult
probeBackend cfg capability b = case b.backendLoaded of
    Left why ->
        pure
            ProbeResult
                { probeName = b.backendName
                , probePath = b.backendPath
                , probeTools = [toolNameOf capability]
                , probeStatus = ProbeFailed
                , probeMillis = Nothing
                , probeDetail = Just ("the tool does not load: " <> Text.pack why)
                , probeFix = Nothing
                , probeSandbox = Nothing
                }
    Right desc -> do
        r <- probeScriptWith cfg [toolNameOf capability] desc
        pure r{probeName = b.backendName}

toolNameOf :: Text -> Text
toolNameOf capability = "bash_" <> capability

{- | Chooses the active backend, trying the backends in the given order:
probes run until one is @ok@ (the rest are not probed); otherwise the first
@unverified@ one is active. Returns the active backend with its
description, and the probes that ran, in order.
-}
selectActiveBackend :: ToolRuntimeConfig -> Text -> [Backend] -> IO (Maybe (Backend, ScriptDescription), [ProbeResult])
selectActiveBackend cfg capability = go Nothing []
  where
    go firstUnverified probed [] = pure (firstUnverified, reverse probed)
    go firstUnverified probed (b : rest) = do
        r <- probeBackend cfg capability b
        let probed' = r : probed
        case (r.probeStatus, b.backendLoaded) of
            (ProbeOk, Right d) -> pure (Just (b, d), reverse probed')
            (ProbeUnverified, Right d) | Nothing <- firstUnverified -> go (Just (b, d)) probed' rest
            _ -> go firstUnverified probed' rest

{- | Probes every backend, in the order they are tried, for
@agents-exe check --probe@.
-}
probeCapability :: ToolRuntimeConfig -> Text -> [Backend] -> IO CapabilityReport
probeCapability cfg capability backends =
    capabilityReport capability <$> mapM (probeBackend cfg capability) backends

{- | The tool the model sees for a capability: the active backend's
description under the capability's name.
-}
capabilityTool :: Text -> ScriptDescription -> ScriptDescription
capabilityTool capability desc =
    desc{scriptInfo = desc.scriptInfo{scriptSlug = capability, scriptSubcommand = Nothing}}

-- | Why a capability has no active backend, one probe per line.
noActiveBackendMessage :: Text -> [ProbeResult] -> String
noActiveBackendMessage capability probes =
    Text.unpack $
        Text.intercalate "\n  " $
            ("capability " <> capability <> " has no active backend:")
                : map renderProbeLine probes
