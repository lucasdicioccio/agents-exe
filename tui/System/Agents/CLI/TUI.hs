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
an in-process 'System.Agents.Host.Client.RunnerClient'. A future
@--attach URL|PATH@ (Phase 4) will build an @httpClient@\/@socketClient@
instead and call the same 'TUI.runTUIWithUserConfig'.
-}
module System.Agents.CLI.TUI (
    Trace (..),

    -- * Types
    TuiOptions (..),

    -- * Handler
    handleTUI,
) where

import qualified Data.Map.Strict as Map

import qualified Prod.Tracer as Prod

import qualified System.Agents.CLI.ConfigLoader as ConfigLoader
import qualified System.Agents.Host as Host
import qualified System.Agents.Host.Client as Client
import qualified System.Agents.Host.Runner as Runner
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
    }
    deriving (Show)

{- | Handle the TUI command: open a 'Host.Host', start a
'Runner.SessionRunner' over it (SQLite primary, the config's legacy file
store composited in as a read fallback for history -- §6), and run the
TUI as a client of it.
-}
handleTUI ::
    -- | Base tracer for logging
    Prod.Tracer IO Trace ->
    -- | The resolved @agents-exe.cfg.json@ (sessions store, etc.)
    ConfigLoader.ResolvedConfig ->
    -- | Path to API keys file
    FilePath ->
    -- | Optional path to keymap file
    Maybe FilePath ->
    -- | List of agent files to load (@--agent-file@\/@--agent@, already resolved)
    [FilePath] ->
    -- | @--db@ override, if any
    Maybe FilePath ->
    -- | @--params-file@ values, resupplied on every command (D7)
    ProcessParams ->
    IO ()
handleTUI tracer rc apiKeysFile mKeymapPath agentFiles mDbPath params = do
    userConfig <- case mKeymapPath of
        Just path -> TUI.loadKeymapFromFile path
        Nothing -> pure TUI.defaultTUIUserConfig

    let dbPath = maybe (ConfigLoader.defaultServerDatabasePath rc) id mDbPath
        -- 'ConfigLoader.hostConfigFromResolved' already sets
        -- 'Host.hcLegacySessionDirs' from the config's session-store read
        -- prefixes, so opening the host here is the same code path
        -- @agents-exe serve@ uses (todos/os-as-standalone-server.md §6):
        -- SQLite is the primary backend, with the legacy file store (old
        -- `conv.<uuid>.json` history) composited in as a read-only
        -- fallback by 'Host.withHost' itself.
        hostCfg = ConfigLoader.hostConfigFromResolved rc agentFiles apiKeysFile dbPath
        hostTracer = Prod.contramap HostTrace tracer
        rawParams = Map.map (\(ProcessValue v _pinned) -> v) params

    Host.withHost hostCfg hostTracer $ \host ->
        Runner.withSessionRunner host $ \runner -> do
            _ <- Runner.recoverOnStartup runner
            let client = Client.inProcessClient Nothing runner
            TUI.runTUIWithUserConfig (Prod.contramap TUITrace tracer) client rc.rcSessionStore userConfig rawParams
