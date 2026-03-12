{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for collecting and displaying important configuration paths.
Used by the 'paths' command to help users debug configuration issues.
-}
module System.Agents.CLI.Paths (
    -- * Data types
    PathsInfo (..),
    PathsOptions (..),
    PathsOutputFormat (..),

    -- * Collecting paths
    collectPathsInfo,

    -- * Formatting output
    formatPathsHuman,
    formatPathsJson,

    -- * Handler
    handlePaths,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as LByteChar8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory, (</>))

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

-- | Information about all important configuration paths.
data PathsInfo = PathsInfo
    { cfgFilePath :: Maybe FilePath
    -- ^ Path to the config file if found via directory traversal
    , cfgDirectory :: FilePath
    -- ^ The active configuration directory being used
    , defaultCfgDirectory :: FilePath
    -- ^ The default config directory (~/.config/agents-exe)
    , agentFilesPaths :: [FilePath]
    -- ^ All agent JSON files that were loaded
    , apiKeysPath :: FilePath
    -- ^ Location of the API keys file
    , sessionStoragePrefix :: Maybe FilePath
    -- ^ Session file prefix/directory (if configured)
    }
    deriving (Show, Eq, Generic)

-- | Options for the paths command output.
data PathsOptions = PathsOptions
    { outputFormat :: PathsOutputFormat
    }
    deriving (Show, Eq)

-- | Output format for the paths command.
data PathsOutputFormat
    = PathsOutputHuman
    | PathsOutputJson
    deriving (Show, Eq)

-- JSON instances
instance Aeson.ToJSON PathsInfo where
    toJSON info =
        Aeson.object
            [ "configuration"
                Aeson..= Aeson.object
                    [ "configFile" Aeson..= cfgFilePath info
                    , "configDirectory" Aeson..= cfgDirectory info
                    , "defaultConfigDirectory" Aeson..= defaultCfgDirectory info
                    ]
            , "agents" Aeson..= agentFilesPaths info
            , "apiKeys" Aeson..= apiKeysPath info
            , "sessionStorage" Aeson..= sessionStoragePrefix info
            ]

instance Aeson.FromJSON PathsInfo

-------------------------------------------------------------------------------
-- Collecting Paths
-------------------------------------------------------------------------------

{- | Collect all relevant path information.
This function mirrors the logic in Main.initArgParserArgs to ensure
consistency with how the application actually loads configuration.
-}
collectPathsInfo ::
    -- | The active config directory (from ArgParserArgs)
    FilePath ->
    -- | Default config directory (~/.config/agents-exe)
    FilePath ->
    -- | Agent files that were loaded
    [FilePath] ->
    -- | API keys file path
    FilePath ->
    -- | Session prefix (if configured)
    Maybe FilePath ->
    IO PathsInfo
collectPathsInfo activeCfgDir defaultCfgDir agents keys session = do
    -- Try to locate the config file via directory traversal
    mCfgFile <- locateAgentsExeConfig

    pure
        PathsInfo
            { cfgFilePath = mCfgFile
            , cfgDirectory = activeCfgDir
            , defaultCfgDirectory = defaultCfgDir
            , agentFilesPaths = agents
            , apiKeysPath = keys
            , sessionStoragePrefix = session
            }

{- | Find the agents-exe.cfg.json file by traversing up the directory tree.
This is a copy of the function in Main.hs to avoid circular dependencies.
-}
locateAgentsExeConfig :: IO (Maybe FilePath)
locateAgentsExeConfig = do
    go =<< getCurrentDirectory
  where
    go :: FilePath -> IO (Maybe FilePath)
    go "" = pure Nothing
    go "/" = pure Nothing
    go path = do
        let tentative = path </> "agents-exe.cfg.json"
        exists <- doesFileExist tentative
        if exists
            then pure (Just tentative)
            else go (takeDirectory path)

-------------------------------------------------------------------------------
-- Formatting Output
-------------------------------------------------------------------------------

-- | Format path information in human-readable text.
formatPathsHuman :: PathsInfo -> Text
formatPathsHuman info =
    Text.unlines $
        [ "Configuration:"
        , formatConfigFile (cfgFilePath info)
        , "  Config directory: " <> Text.pack (cfgDirectory info)
        , "  Default config dir: " <> Text.pack (defaultCfgDirectory info)
        , ""
        , "Agents:"
        ]
            ++ formatAgentFiles (agentFilesPaths info)
            ++ [ ""
               , "API Keys:"
               , "  " <> Text.pack (apiKeysPath info)
               , ""
               , "Session Storage:"
               , formatSessionPrefix (sessionStoragePrefix info)
               ]

-- | Format the config file line based on whether it was found.
formatConfigFile :: Maybe FilePath -> Text
formatConfigFile Nothing = "  Config file: (not found - using defaults)"
formatConfigFile (Just path) = "  Config file found: " <> Text.pack path

-- | Format the list of agent files.
formatAgentFiles :: [FilePath] -> [Text]
formatAgentFiles [] = ["  (no agent files loaded)"]
formatAgentFiles files = map formatAgentFile files
  where
    formatAgentFile path = "  - " <> Text.pack path

-- | Format the session prefix line.
formatSessionPrefix :: Maybe FilePath -> Text
formatSessionPrefix Nothing = "  (not configured)"
formatSessionPrefix (Just path) = "  Prefix: " <> Text.pack path

-- | Format path information as JSON.
formatPathsJson :: PathsInfo -> LByteChar8.ByteString
formatPathsJson = Aeson.encodePretty

-------------------------------------------------------------------------------
-- Handler
-------------------------------------------------------------------------------

-- | Handle the paths command
handlePaths ::
    -- | Paths options
    PathsOptions ->
    -- | Config directory
    FilePath ->
    -- | Agent files
    [FilePath] ->
    -- | API keys file
    FilePath ->
    -- | Session prefix
    Maybe FilePath ->
    IO ()
handlePaths opts configDir agentFiles apiKeysFile sessionPrefix = do
    homedir <- getHomeDirectory
    let defaultCfgDir = homedir </> ".config/agents-exe"

    -- Collect path information
    pathsInfo <-
        collectPathsInfo
            configDir
            defaultCfgDir
            agentFiles
            apiKeysFile
            sessionPrefix

    -- Output based on format option
    case outputFormat opts of
        PathsOutputJson -> LByteChar8.putStrLn $ formatPathsJson pathsInfo
        PathsOutputHuman -> Text.putStrLn $ formatPathsHuman pathsInfo

    exitSuccess
