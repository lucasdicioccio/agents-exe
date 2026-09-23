{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Loading @agents-exe.cfg.json@ and resolving the set of agent files a
command runs with: everything @app/Main.hs@ did inline before this module
existed (see @todos/os-as-standalone-server.md@, gap G7).

This is a library module so that any long-lived process ('agents-exe serve',
in particular) can load agents "like the TUI does" without duplicating
'app/Main.hs'. 'app/Main.hs' keeps the same flags, defaults and error
messages by calling straight into this module.
-}
module System.Agents.CLI.ConfigLoader (
    -- * Sessions configuration
    SessionsConfig (..),
    defaultSessionsDirName,
    buildSessionStoreFromConfig,
    buildSimpleSessionStore,
    buildDefaultSessionStore,

    -- * The config file
    AgentsExeLogConfig (..),
    AgentsExeConfig (..),
    locateAgentsExeConfig,

    -- * Resolving the whole configuration
    ResolvedConfig (..),
    loadAgentsExeConfig,

    -- * Agent file resolution
    resolveAgentFiles,

    -- * API keys and params files
    secretKeysFileIn,
    parseParamsFileValue,
    loadParamsFile,

    -- * HostConfig
    hostConfigFromResolved,
    defaultServerDatabasePath,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Lazy as LByteString
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

import System.Agents.Base (Agent (..), AgentDescription (..))
import System.Agents.CLI.Aliases (
    AliasDefinition,
    defaultAliases,
    resolveAliases,
 )
import qualified System.Agents.FileLoader as FileLoader
import System.Agents.Host (HostConfig (..), defaultHostConfig)
import qualified System.Agents.SessionStore as SessionStore
import System.Agents.Tools.Params.Types (ProcessParams, ProcessValue (..))

-------------------------------------------------------------------------------
-- Sessions configuration
-------------------------------------------------------------------------------

{- | Sessions configuration for multi-location session storage.

This allows configuring separate read and write locations for sessions,
enabling unified views of sessions scattered across multiple directories.

Example configuration in agents-exe.cfg.json:

> {
>   "sessions": {
>     "writeLocation": "./sessions/",
>     "readLocations": [
>       "./sessions/",
>       "~/.config/agents-exe/sessions/"
>     ]
>   }
> }
-}
data SessionsConfig = SessionsConfig
    { sessionsWriteLocation :: FilePath
    -- ^ Directory where new sessions are written
    , sessionsReadLocations :: [FilePath]
    -- ^ Directories to search for existing sessions
    }
    deriving (Show, Generic)

instance Aeson.FromJSON SessionsConfig where
    parseJSON = Aeson.withObject "SessionsConfig" $ \v ->
        SessionsConfig
            <$> v Aeson..: "writeLocation"
            <*> v Aeson..:? "readLocations" Aeson..!= []

-- | Default sessions directory name within config
defaultSessionsDirName :: FilePath
defaultSessionsDirName = "sessions"

{- | Build a SessionStore from SessionsConfig.

Resolves tilde (~) paths and creates the SessionStore.
-}
buildSessionStoreFromConfig :: SessionsConfig -> IO SessionStore.SessionStore
buildSessionStoreFromConfig cfg = do
    writePath <- SessionStore.resolveSessionPath cfg.sessionsWriteLocation
    readPaths <- mapM SessionStore.resolveSessionPath cfg.sessionsReadLocations
    pure $ SessionStore.mkSessionStore writePath readPaths

{- | Build a simple SessionStore from a single prefix (backwards compatibility).

This is used when the 'sessions' config is not present but
'agentsLogs.logSessionsJsonPrefix' is set.
-}
buildSimpleSessionStore :: FilePath -> IO SessionStore.SessionStore
buildSimpleSessionStore prefix = do
    resolvedPath <- SessionStore.resolveSessionPath prefix
    pure $ SessionStore.mkSimpleSessionStore resolvedPath

{- | Create a default SessionStore in the config directory.

This is used when no session configuration is provided at all.
-}
buildDefaultSessionStore :: FilePath -> IO SessionStore.SessionStore
buildDefaultSessionStore cfgDir = do
    let sessionsDir = cfgDir </> defaultSessionsDirName
    pure $ SessionStore.mkSimpleSessionStore sessionsDir

-------------------------------------------------------------------------------
-- The config file
-------------------------------------------------------------------------------

-- | Logging configuration from config file
data AgentsExeLogConfig = AgentsExeLogConfig
    { logJsonHttpEndpoint :: Maybe String
    , logJsonPath :: Maybe FilePath
    , logRawPath :: Maybe FilePath
    , logSessionsJsonPrefix :: Maybe FilePath
    }
    deriving (Show, Generic)

instance Aeson.FromJSON AgentsExeLogConfig

-- | Main configuration file format
data AgentsExeConfig = AgentsExeConfig
    { agentsConfigDir :: Maybe FilePath
    , agentsDirectories :: [FilePath]
    , agentsFiles :: [FilePath]
    , agentsLogs :: Maybe AgentsExeLogConfig
    , cfgPromptAliases :: Maybe (Map Text AliasDefinition)
    , cfgSelfDescribeSlug :: Maybe String
    , cfgSelfDescribeDescription :: Maybe String
    , cfgKeymapPath :: Maybe FilePath
    , cfgSessions :: Maybe SessionsConfig
    }
    deriving (Show, Generic)

instance Aeson.FromJSON AgentsExeConfig where
    parseJSON = Aeson.withObject "AgentsExeConfig" $ \v ->
        AgentsExeConfig
            <$> v Aeson..:? "agentsConfigDir"
            <*> v Aeson..:? "agentsDirectories" Aeson..!= []
            <*> v Aeson..:? "agentsFiles" Aeson..!= []
            <*> v Aeson..:? "agentsLogs"
            <*> v Aeson..:? "promptAliases"
            <*> v Aeson..:? "selfDescribeSlug"
            <*> v Aeson..:? "selfDescribeDescription"
            <*> v Aeson..:? "keymap"
            <*> v Aeson..:? "sessions"

-- | Locate the agents-exe.cfg.json by traversing up the directory tree
locateAgentsExeConfig :: IO (Maybe FilePath)
locateAgentsExeConfig = do
    go =<< getCurrentDirectory
  where
    go :: FilePath -> IO (Maybe FilePath)
    go "" = pure Nothing
    go "/" = pure Nothing
    go path = do
        let temptative = path </> "agents-exe.cfg.json"
        exists <- doesFileExist temptative
        if exists
            then pure (Just temptative)
            else go (takeDirectory path)

-------------------------------------------------------------------------------
-- Resolving the whole configuration
-------------------------------------------------------------------------------

-- | Everything a command needs, once the config file (if any) is loaded.
data ResolvedConfig = ResolvedConfig
    { rcConfigDir :: FilePath
    , rcAgentFiles :: [FilePath]
    -- ^ @agentsFiles@ plus every @.json@ file under each @agentsDirectories@
    -- entry, or the default agents directory's contents when there is no
    -- config file.
    , rcLogJsonHttpEndpoint :: Maybe String
    , rcLogJsonFilepath :: Maybe FilePath
    , rcLogRawFilepath :: Maybe FilePath
    , rcSessionStore :: SessionStore.SessionStore
    , rcPromptAliases :: Map Text AliasDefinition
    , rcSelfDescribeSlug :: Maybe String
    , rcSelfDescribeDescription :: Maybe String
    , rcKeymapPath :: Maybe FilePath
    }

{- | Locate and load @agents-exe.cfg.json@ (see 'locateAgentsExeConfig'), or
fall back to @DEFAULTCONFIGDIR/default/*.json@ when there is none.

The caller is responsible for making sure @defaultConfigDir@ (and its
@default@ subdirectory) exists first, as @agents-exe@'s
@ensureConfigStructure@ does; this function does not create example agent
files or an API keys template, since those are @agents-exe@'s own defaults,
not something every host of this library wants.
-}
loadAgentsExeConfig :: FilePath -> IO ResolvedConfig
loadAgentsExeConfig defaultConfigDir = do
    agentsExecConfig <- locateAgentsExeConfig
    maybe (initWithoutAgentsExeConfig defaultConfigDir) (initFromAgentsExeConfig defaultConfigDir) agentsExecConfig
  where
    initFromAgentsExeConfig :: FilePath -> FilePath -> IO ResolvedConfig
    initFromAgentsExeConfig defaultCfgDir agentsexecfgpath = do
        zeconfig <- Aeson.eitherDecodeFileStrict' agentsexecfgpath :: IO (Either String AgentsExeConfig)
        case zeconfig of
            Left err -> error ("failed to load agents-exe config at " <> agentsexecfgpath <> " " <> err)
            Right obj -> do
                -- Build session store from configuration
                -- Priority: 1) cfgSessions, 2) agentsLogs.logSessionsJsonPrefix, 3) default
                sessionStore <- case obj.cfgSessions of
                    Just sessionsCfg -> buildSessionStoreFromConfig sessionsCfg
                    Nothing -> case obj.agentsLogs >>= logSessionsJsonPrefix of
                        Just prefix -> buildSimpleSessionStore prefix
                        Nothing -> buildDefaultSessionStore defaultCfgDir

                jsonPathss <- traverse FileLoader.listJsonDirectory obj.agentsDirectories
                pure $
                    ResolvedConfig
                        (fromMaybe defaultCfgDir obj.agentsConfigDir)
                        (obj.agentsFiles <> mconcat jsonPathss)
                        (logJsonHttpEndpoint =<< obj.agentsLogs)
                        (logJsonPath =<< obj.agentsLogs)
                        (logRawPath =<< obj.agentsLogs)
                        sessionStore
                        (resolveAliases obj.cfgPromptAliases)
                        obj.cfgSelfDescribeSlug
                        obj.cfgSelfDescribeDescription
                        obj.cfgKeymapPath

    initWithoutAgentsExeConfig :: FilePath -> IO ResolvedConfig
    initWithoutAgentsExeConfig pconfigdir = do
        jsonPaths <- FileLoader.listJsonDirectory (pconfigdir </> "default")

        -- Create sessions directory for storing conversations when outside projects
        -- This ensures session files are properly persisted instead of being written
        -- to the current working directory with an empty prefix
        let sessionsDir = pconfigdir </> "sessions"
        createDirectoryIfMissing True sessionsDir

        pure $
            ResolvedConfig
                pconfigdir
                jsonPaths
                Nothing
                Nothing
                Nothing
                (SessionStore.mkSimpleSessionStore sessionsDir)
                defaultAliases
                Nothing
                Nothing
                Nothing

-------------------------------------------------------------------------------
-- Agent file resolution
-------------------------------------------------------------------------------

-- | Resolve agent files based on optional slug selection
resolveAgentFiles :: [FilePath] -> Maybe Text -> IO (Either Text [FilePath])
resolveAgentFiles files Nothing = pure $ Right files
resolveAgentFiles files (Just agentSlug) = do
    -- Load all agents to find matching slug
    agentsWithFiles <- mapM loadAgentWithFile files
    case find (\(_, agent) -> agentSlug == agent.slug) agentsWithFiles of
        Just (file, _) -> pure $ Right [file]
        Nothing -> do
            -- Build error message with available slugs
            let availableSlugs = map (\(f, a) -> (a.slug, f)) agentsWithFiles
            pure $ Left $ formatSlugNotFoundError agentSlug availableSlugs
  where
    loadAgentWithFile :: FilePath -> IO (FilePath, Agent)
    loadAgentWithFile file = do
        result <- Aeson.eitherDecodeFileStrict' file
        case result of
            Left err -> error $ "Failed to parse agent file " ++ file ++ ": " ++ err
            Right (AgentDescription agent) -> pure (file, agent)

    formatSlugNotFoundError :: Text -> [(Text, FilePath)] -> Text
    formatSlugNotFoundError targetSlug available =
        Text.unlines $
            [ "Error: Agent '" <> targetSlug <> "' not found."
            , ""
            , "Available agents:"
            ]
                ++ map (\(s, f) -> "  - " <> s <> " (" <> Text.pack f <> ")") available

-------------------------------------------------------------------------------
-- API keys and params files
-------------------------------------------------------------------------------

-- | The API keys file agents-exe conventionally keeps alongside a config dir.
secretKeysFileIn :: FilePath -> FilePath
secretKeysFileIn configdir = configdir </> "secret-keys"

{- | Read a @--params-file@'s @{"name": value, ...}@ object into 'ProcessParams'.
An entry may also be @{"value": ..., "pinned": true}@ to pin it.
-}
parseParamsFileValue :: Aeson.Value -> Either String ProcessParams
parseParamsFileValue (Aeson.Object o) =
    Map.fromList <$> mapM entry (Aeson.KeyMap.toList o)
  where
    entry (k, Aeson.Object fields)
        | Just v <- Aeson.KeyMap.lookup "value" fields =
            let pinned = case Aeson.KeyMap.lookup "pinned" fields of
                    Just (Aeson.Bool b) -> b
                    _ -> False
             in Right (Aeson.Key.toText k, ProcessValue v pinned)
    entry (k, v) = Right (Aeson.Key.toText k, ProcessValue v False)
parseParamsFileValue _ = Left "expected a JSON object of {\"name\": value, ...}"

{- | Load and parse one @--params-file@. Returns the error message
@app/Main.hs@ used to print on failure (prefixed with the flag and path), so
callers keep the same wording.
-}
loadParamsFile :: FilePath -> IO (Either Text ProcessParams)
loadParamsFile path = do
    raw <- LByteString.readFile path
    pure $ case Aeson.eitherDecode raw >>= parseParamsFileValue of
        Left err -> Left (Text.pack ("--params-file " <> path <> ": " <> err))
        Right params -> Right params

-------------------------------------------------------------------------------
-- HostConfig
-------------------------------------------------------------------------------

{- | Build a 'HostConfig' from a resolved agents-exe configuration, an API
keys file and a database path, so 'System.Agents.Host.withHost' can be fed
either explicit @--agent-file@s or the files 'loadAgentsExeConfig' resolved
(@todos/os-as-standalone-server.md@, Phase 1, item 2). The agent files
passed in take priority over 'rcAgentFiles' when non-empty, mirroring
@--agent-file@ overriding the config file everywhere else in agents-exe.
-}
hostConfigFromResolved :: ResolvedConfig -> [FilePath] -> FilePath -> FilePath -> HostConfig
hostConfigFromResolved rc explicitAgentFiles apiKeysFile dbPath =
    (defaultHostConfig agentFiles apiKeysFile dbPath)
        { hcLegacySessionDirs = rc.rcSessionStore.sessionReadPrefixes
        }
  where
    agentFiles = case explicitAgentFiles of
        [] -> rc.rcAgentFiles
        xs -> xs

{- | Where @agents-exe serve@ puts its SQLite database when @--db@ is not
given: next to the resolved sessions directory (its write location), rather
than @./agents-server.db@ as plain @agents-server@ defaults to.
-}
defaultServerDatabasePath :: ResolvedConfig -> FilePath
defaultServerDatabasePath rc = rc.rcSessionStore.sessionWritePrefix </> "agents-server.db"
