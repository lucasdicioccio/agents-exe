{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Module for the 'tui' command handler.

The tui command launches an interactive terminal user interface for
chatting with agents.

Phase 3b-i (@todos/os-as-standalone-server.md@ Design §4, §6): the TUI no
longer loads agent trees itself. It opens a 'System.Agents.Host.Host' the
way @agents-exe serve@ does (same 'System.Agents.CLI.ConfigLoader'), starts
a 'System.Agents.Host.Runner.SessionRunner' over it, and drives it through
an in-process 'System.Agents.Host.Client.RunnerClient'.

Phase 4: with @--attach URL|PATH@ it opens nothing locally and drives a
running @agents-exe serve@\/@agents-server@ through
'System.Agents.Host.Client.Http.httpClient' instead, calling the same
'TUI.runTUIWithUserConfig' (D6: embedded and attached modes are the same
TUI code).
-}
module System.Agents.CLI.TUI (
    Trace (..),

    -- * Types
    TuiOptions (..),

    -- * Handler
    handleTUI,
) where

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified Prod.Tracer as Prod

import qualified System.Agents.CLI.ConfigLoader as ConfigLoader
import qualified System.Agents.Host as Host
import qualified System.Agents.Host.Client as Client
import qualified System.Agents.Host.Client.Http as Http
import qualified System.Agents.Host.Runner as Runner
import System.Agents.Protocol (runnerErrorMessage)
import System.Agents.Tools.Params.Types (ProcessParams, ProcessValue (..))
import qualified System.Agents.TUI.Core as TUI

-- | Trace type for TUI command.
data Trace
    = TUITrace !TUI.Trace
    | HostTrace !Host.HostTrace
    deriving (Show)

-- | Options for the TUI command
data TuiOptions = TuiOptions
    { tuiKeymapPath :: Maybe FilePath
    -- ^ Optional path to keymap configuration file
    , tuiDatabasePath :: Maybe FilePath
    -- ^ @--db PATH@: override the SQLite database the TUI's embedded
    -- runner uses. Defaults to 'ConfigLoader.defaultServerDatabasePath'.
    , tuiAttach :: Maybe String
    -- ^ @--attach URL|PATH@: drive a running server instead of an embedded
    -- runner ('Http.parseEndpoint' lists the accepted forms).
    , tuiToken :: Maybe Text
    -- ^ @--token TOKEN@: bearer token for @--attach@.
    , tuiTokenFile :: Maybe FilePath
    -- ^ @--token-file FILE@: the same, read from a file (surrounding whitespace ignored).
    }
    deriving (Show)

{- | Handle the TUI command.

Embedded (no @--attach@): open a 'Host.Host', start a
'Runner.SessionRunner' over it (SQLite primary, the config's legacy file
store composited in as a read fallback for history -- §6), and run the
TUI as a client of it.

Attached (@--attach@): build an 'Http.httpClient' to the server, check it
answers (and accepts the token), and run the same TUI over it. No agent
file, API key or database is opened locally; @--params-file@\/@--set@
values are still resupplied on every command (D7).
-}
handleTUI ::
    -- | Base tracer for logging
    Prod.Tracer IO Trace ->
    -- | The resolved @agents-exe.cfg.json@ (sessions store, etc.)
    ConfigLoader.ResolvedConfig ->
    -- | Path to API keys file
    FilePath ->
    -- | The @tui@ options
    TuiOptions ->
    -- | List of agent files to load (@--agent-file@\/@--agent@, already resolved)
    [FilePath] ->
    -- | @--params-file@ values, resupplied on every command (D7)
    ProcessParams ->
    IO ()
handleTUI tracer rc apiKeysFile opts agentFiles params = do
    userConfig <- case opts.tuiKeymapPath of
        Just path -> TUI.loadKeymapFromFile path
        Nothing -> pure TUI.defaultTUIUserConfig
    let rawParams = Map.map (\(ProcessValue v _pinned) -> v) params
        tuiTracer = Prod.contramap TUITrace tracer
    case opts.tuiAttach of
        Just target -> do
            when (isJust opts.tuiDatabasePath) $
                die "--attach and --db cannot be used together: an attached TUI uses the server's database"
            client <- attachClient target opts
            TUI.runTUIWithUserConfig tuiTracer TUI.AttachedRunner client userConfig rawParams
        Nothing -> do
            let dbPath = maybe (ConfigLoader.defaultServerDatabasePath rc) id opts.tuiDatabasePath
                -- 'ConfigLoader.hostConfigFromResolved' already sets
                -- 'Host.hcLegacySessionDirs' from the config's session-store read
                -- prefixes, so opening the host here is the same code path
                -- @agents-exe serve@ uses (todos/os-as-standalone-server.md §6):
                -- SQLite is the primary backend, with the legacy file store (old
                -- `conv.<uuid>.json` history) composited in as a read-only
                -- fallback by 'Host.withHost' itself.
                hostCfg = ConfigLoader.hostConfigFromResolved rc agentFiles apiKeysFile dbPath
                hostTracer = Prod.contramap HostTrace tracer
            Host.withHost hostCfg hostTracer $ \host ->
                Runner.withSessionRunner host $ \runner -> do
                    _ <- Runner.recoverOnStartup runner
                    let client = Client.inProcessClient Nothing runner
                    TUI.runTUIWithUserConfig tuiTracer TUI.EmbeddedRunner client userConfig rawParams

{- | An 'Http.httpClient' for @--attach@, after checking the server answers
@/healthz@ and accepts the token (@GET /v1/agents@ needs one when the
server has @--auth-tokens@): failing either, exit with a message rather
than open a TUI with nothing in it.
-}
attachClient :: String -> TuiOptions -> IO Client.RunnerClient
attachClient target opts = do
    endpoint <- either (die . Text.pack) pure (Http.parseEndpoint target)
    token <- case (opts.tuiToken, opts.tuiTokenFile) of
        (Just _, Just _) -> die "--token and --token-file cannot be used together"
        (Just t, Nothing) -> pure (Just t)
        (Nothing, Just path) -> Just . Text.strip <$> Text.readFile path
        (Nothing, Nothing) -> pure Nothing
    client <- Http.httpClient (Http.defaultHttpClientConfig endpoint){Http.hccToken = token}
    let where_ = Text.pack (Http.renderEndpoint endpoint)
    Client.stats client >>= \case
        Left err -> die ("cannot attach to " <> where_ <> ": " <> runnerErrorMessage err)
        Right _ -> pure ()
    Client.listAgents client >>= \case
        Left err -> die ("attached to " <> where_ <> ", but it refused to list agents: " <> runnerErrorMessage err)
        Right _ -> pure ()
    pure client

die :: Text -> IO a
die msg = Text.hPutStrLn stderr ("agents-exe tui: " <> msg) >> exitFailure
