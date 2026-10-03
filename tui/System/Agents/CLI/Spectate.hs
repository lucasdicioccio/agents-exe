{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'spectate' command handler.

@agents-exe spectate --attach URL|PATH@ watches a running
@agents-exe serve@\/@agents-server@: the session tree, tool calls and the
model's text, live. It is one more client of the runner, the same way
@tui --attach@ is ("System.Agents.CLI.TUI"), except that it only reads.
-}
module System.Agents.CLI.Spectate (
    SpectateOptions (..),
    handleSpectate,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)

import System.Agents.CLI.TUI (attachRunnerClient)
import qualified System.Agents.Host.Client.Http as Http
import System.Agents.TUI.Spectate (runSpectate)

-- | Options for the spectate command
data SpectateOptions = SpectateOptions
    { spectateAttach :: String
    -- ^ @--attach URL|PATH@: the server to watch ('Http.parseEndpoint'
    -- lists the accepted forms).
    , spectateToken :: Maybe Text
    -- ^ @--token TOKEN@: bearer token for the server.
    , spectateTokenFile :: Maybe FilePath
    -- ^ @--token-file FILE@: the same, read from a file.
    }
    deriving (Show)

-- | Attach to the server, then run the dashboard until the spectator quits.
handleSpectate :: SpectateOptions -> IO ()
handleSpectate opts = do
    client <- attachRunnerClient "spectate" opts.spectateAttach opts.spectateToken opts.spectateTokenFile
    let source = either (const (Text.pack opts.spectateAttach)) (Text.pack . Http.renderEndpoint) (Http.parseEndpoint opts.spectateAttach)
    result <- runSpectate source client
    case result of
        Right () -> pure ()
        Left err -> do
            Text.hPutStrLn stderr ("agents-exe spectate: cannot watch " <> source <> ": " <> err)
            exitFailure
