{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Command-line options and the server's lifecycle.
module AgentsServer.Server (
    ServerOptions (..),
    serverOptions,
    runServer,
) where

import Control.Monad (void)
import Data.Aeson ((.=))
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Time (NominalDiffTime)
import Network.Wai.Handler.Warp
import Options.Applicative
import System.Posix.Signals (Handler (CatchOnce), installHandler, sigINT, sigTERM)

import AgentsServer.Api
import AgentsServer.Auth (loadAuthTokens)
import AgentsServer.Log
import System.Agents.Host
import System.Agents.Host.Runner (recoverOnStartup, withSessionRunner)

data ServerOptions = ServerOptions
    { soAgentFiles :: [FilePath]
    , soApiKeysFile :: FilePath
    , soDatabase :: FilePath
    , soBind :: String
    , soPort :: Int
    , soLiveSessionTtl :: NominalDiffTime
    , soShutdownGrace :: Int
    -- ^ Seconds to let open requests finish on shutdown.
    , soAuthTokens :: Maybe FilePath
    -- ^ Bearer tokens and their owners; without, no authentication.
    }

serverOptions :: Parser ServerOptions
serverOptions =
    ServerOptions
        <$> some (strOption (long "agent-file" <> metavar "FILE" <> help "Root agent file; repeat for several agents"))
        <*> strOption (long "api-keys" <> metavar "FILE" <> help "API keys file")
        <*> strOption (long "db" <> metavar "FILE" <> value "agents-server.db" <> showDefault <> help "SQLite database for sessions")
        <*> strOption (long "bind" <> metavar "HOST" <> value "127.0.0.1" <> showDefault <> help "Address to listen on")
        <*> option auto (long "port" <> metavar "PORT" <> value 8080 <> showDefault <> help "Port to listen on")
        <*> (fromInteger <$> option auto (long "live-session-ttl" <> metavar "SECONDS" <> value 900 <> showDefault <> help "Idle time before a session's in-memory state is dropped"))
        <*> option auto (long "shutdown-grace" <> metavar "SECONDS" <> value 10 <> showDefault <> help "Time open requests get to finish on shutdown")
        <*> optional (strOption (long "auth-tokens" <> metavar "FILE" <> help "Bearer tokens and their owners; callers then only see their own sessions"))

{- | Load the agents, open the database, and serve until SIGTERM or SIGINT.

On a signal the server stops accepting connections, ends event streams,
answers waiting requests with the current state, gives open requests the
grace period, then cancels active runs (storing their sessions) and closes
the database.
-}
runServer :: ServerOptions -> Logger -> IO ()
runServer opts logger = do
    auth <- traverse loadAuthTokens opts.soAuthTokens
    let cfg =
            (defaultHostConfig opts.soAgentFiles opts.soApiKeysFile opts.soDatabase)
                { hcLiveSessionTtl = opts.soLiveSessionTtl
                }
    withHost cfg (hostTraceLogger logger) $ \host ->
        withSessionRunner host $ \runner -> do
            _ <- recoverOnStartup runner
            env <- newServerEnv host runner auth
            let started =
                    logLine logger "server.started" $
                        [ "bind" .= opts.soBind
                        , "port" .= opts.soPort
                        , "agents" .= Map.keys host.hostAgents
                        , "database" .= opts.soDatabase
                        , "authentication" .= (maybe "none" (const "bearer") auth :: String)
                        ]
                            <> [ "warning" .= ("no authentication: anyone who can reach this address can run the agents" :: String)
                               | Nothing <- [auth]
                               ]
                settings =
                    setHost (fromString opts.soBind)
                        . setPort opts.soPort
                        . setBeforeMainLoop started
                        . setGracefulShutdownTimeout (Just opts.soShutdownGrace)
                        . setInstallShutdownHandler (onSignals env)
                        $ defaultSettings
            runSettings settings (requestLogger logger (application env))
            logLine logger "server.stopping" []
    logLine logger "server.stopped" []
  where
    onSignals env closeSocket = do
        let stop = do
                logLine logger "server.signal" []
                requestShutdown env
                closeSocket
        void $ installHandler sigTERM (CatchOnce stop) Nothing
        void $ installHandler sigINT (CatchOnce stop) Nothing
