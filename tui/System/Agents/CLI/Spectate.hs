{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Module for the 'spectate' command handler.

@agents-exe spectate --attach URL|PATH@ watches a running
@agents-exe serve@\/@agents-server@: the session tree, tool calls and the
model's text, live. It is one more client of the runner, the same way
@tui --attach@ is ("System.Agents.CLI.TUI"), except that it only reads.

The screen starts from a layout ("System.Agents.Spectate.Layout"), found
by 'resolveLayout': the default one, then what a saved layout file says,
then what the flags say.
-}
module System.Agents.CLI.Spectate (
    SpectateOptions (..),
    handleSpectate,
    resolveLayout,
    defaultLayoutFile,
) where

import Control.Exception (IOException, displayException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist, getHomeDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)

import System.Agents.CLI.TUI (attachRunnerClient)
import qualified System.Agents.Host.Client.Http as Http
import System.Agents.Spectate.Layout (Column, Layout (..), defaultLayout, parseLayoutFile)
import System.Agents.TUI.Spectate (SpectateConfig (..), runSpectate)

-- | Options for the spectate command
data SpectateOptions = SpectateOptions
    { spectateAttach :: String
    -- ^ @--attach URL|PATH@: the server to watch ('Http.parseEndpoint'
    -- lists the accepted forms).
    , spectateToken :: Maybe Text
    -- ^ @--token TOKEN@: bearer token for the server.
    , spectateTokenFile :: Maybe FilePath
    -- ^ @--token-file FILE@: the same, read from a file.
    , spectatePanels :: Maybe [Column]
    -- ^ @--panels SPEC@: the panels shown, their places and sizes.
    , spectateRefreshMs :: Maybe Int
    -- ^ @--refresh SECONDS@: the time between two refreshes.
    , spectateLayoutFile :: Maybe FilePath
    -- ^ @--layout-file FILE@: the saved layout, instead of 'defaultLayoutFile'.
    }
    deriving (Show)

-- | @~\/.config\/agents-exe\/spectate-layout@
defaultLayoutFile :: IO FilePath
defaultLayoutFile = (</> ".config/agents-exe/spectate-layout") <$> getHomeDirectory

{- | The layout to start with: the saved one when its file exists, with
the flags over it. A file that cannot be read or parsed is an error rather
than a layout silently ignored.
-}
resolveLayout :: FilePath -> Maybe [Column] -> Maybe Int -> IO (Either Text Layout)
resolveLayout path panels refreshMs = do
    exists <- doesFileExist path
    saved <-
        if not exists
            then pure (Right defaultLayout)
            else do
                content <- try (Text.readFile path)
                pure $ case content of
                    Left (e :: IOException) -> Left (Text.pack (displayException e))
                    Right text -> parseLayoutFile defaultLayout text
    pure $ case saved of
        Left err -> Left ("layout file " <> Text.pack path <> ": " <> err)
        Right layout ->
            Right
                layout
                    { layColumns = maybe layout.layColumns id panels
                    , layRefreshMs = maybe layout.layRefreshMs id refreshMs
                    }

-- | Attach to the server, then run the dashboard until the spectator quits.
handleSpectate :: SpectateOptions -> IO ()
handleSpectate opts = do
    layoutFile <- maybe defaultLayoutFile pure opts.spectateLayoutFile
    layout <-
        resolveLayout layoutFile opts.spectatePanels opts.spectateRefreshMs >>= \resolved -> case resolved of
            Right layout -> pure layout
            Left err -> do
                Text.hPutStrLn stderr ("agents-exe spectate: " <> err)
                exitFailure
    client <- attachRunnerClient "spectate" opts.spectateAttach opts.spectateToken opts.spectateTokenFile
    let source = either (const (Text.pack opts.spectateAttach)) (Text.pack . Http.renderEndpoint) (Http.parseEndpoint opts.spectateAttach)
    result <- runSpectate (SpectateConfig layout (Just layoutFile)) source client
    case result of
        Right () -> pure ()
        Left err -> do
            Text.hPutStrLn stderr ("agents-exe spectate: cannot watch " <> source <> ": " <> err)
            exitFailure
