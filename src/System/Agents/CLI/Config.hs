{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Module for the 'config' command handler.

The config command provides git-config-like functionality for managing
agents-exe configuration files:

* Initialize local config (agents-exe.cfg.json)
* Initialize keymap files
* List and manage API keys

This command helps users configure agents-exe itself without manually
creating JSON configuration files.
-}
module System.Agents.CLI.Config (
    -- * Command types
    ConfigCommand (..),
    ConfigOptions (..),
    LocalConfigOptions (..),
    LocalConfigCommand (..),
    KeymapConfigOptions (..),
    KeymapConfigCommand (..),
    ApiKeyConfigOptions (..),
    ApiKeyConfigCommand (..),

    -- * Handler
    handleConfig,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

import System.Agents.ApiKeys (ApiKey (..), ApiKeys (..))
import System.Agents.TUI.KeyMapping (KeyMapping, defaultKeyMapping)

-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

-- | Options for the config command
data ConfigOptions = ConfigOptions
    { configCommand :: ConfigCommand
    , configForce :: Bool
    -- ^ Overwrite existing files
    }
    deriving (Show, Eq)

-- | Subcommands for the config command
data ConfigCommand
    = ConfigLocal LocalConfigOptions
    | ConfigKeymap KeymapConfigOptions
    | ConfigApiKey ApiKeyConfigOptions
    deriving (Show, Eq)

-- | Options for the 'config local' subcommand
data LocalConfigOptions = LocalConfigOptions
    { localCommand :: LocalConfigCommand
    }
    deriving (Show, Eq)

-- | Commands for local config management
data LocalConfigCommand
    = -- | Initialize a new agents-exe.cfg.json
      LocalInit
    deriving (Show, Eq)

-- | Options for the 'config keymap' subcommand
data KeymapConfigOptions = KeymapConfigOptions
    { keymapCommand :: KeymapConfigCommand
    , keymapFilePath :: FilePath
    }
    deriving (Show, Eq)

-- | Commands for keymap management
data KeymapConfigCommand
    = -- | Initialize a new keymap file with defaults
      KeymapInit
    deriving (Show, Eq)

-- | Options for the 'config api-key' subcommand
data ApiKeyConfigOptions = ApiKeyConfigOptions
    { apiKeyCommand :: ApiKeyConfigCommand
    , apiKeysFilePath :: FilePath
    }
    deriving (Show, Eq)

-- | Commands for API key management
data ApiKeyConfigCommand
    = -- | List all API key names
      ApiKeyList
    | -- | Create a new API key entry
      ApiKeyCreate Text
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Configuration Templates
-------------------------------------------------------------------------------

-- | Minimal agents-exe.cfg.json template with resolved home directory.
-- Takes the resolved home directory path and returns the config content.
defaultAgentsExeConfigMinimal :: FilePath -> Text
defaultAgentsExeConfigMinimal homeDir =
    Text.unlines
        [ "{"
        , "  \"agentsConfigDir\": \"" <> Text.pack configDir <> "\","
        , "  \"agentsFiles\": []"
        , "}"
        ]
  where
    configDir = homeDir </> ".config" </> "agents-exe"

-------------------------------------------------------------------------------
-- Handler Implementation
-------------------------------------------------------------------------------

-- | Handle the config command
handleConfig ::
    -- | Path to config directory
    FilePath ->
    -- | Config options
    ConfigOptions ->
    IO ()
handleConfig configDir opts = do
    case opts.configCommand of
        ConfigLocal localOpts ->
            handleLocalConfig opts.configForce configDir localOpts
        ConfigKeymap keymapOpts ->
            handleKeymapConfig opts.configForce keymapOpts
        ConfigApiKey apiKeyOpts ->
            handleApiKeyConfig opts.configForce apiKeyOpts

-------------------------------------------------------------------------------
-- Local Config Handler
-------------------------------------------------------------------------------

-- | Handle local config subcommands
handleLocalConfig ::
    -- | Force overwrite
    Bool ->
    -- | Config directory
    FilePath ->
    -- | Local config options
    LocalConfigOptions ->
    IO ()
handleLocalConfig force _cfgDir opts = do
    case opts.localCommand of
        LocalInit -> do
            -- Only check current directory for existing config (not up the tree)
            let configPath = "agents-exe.cfg.json"
            exists <- doesFileExist configPath
            if exists
                then do
                    unless force $ do
                        Text.putStrLn $ "Config file already exists: " <> Text.pack configPath
                        Text.putStrLn "Use --force to overwrite"
                        exitFailure
                    Text.putStrLn $ "Overwriting existing config: " <> Text.pack configPath
                    writeAgentsExeConfig configPath
                else do
                    writeAgentsExeConfig configPath
                    Text.putStrLn $ "Created config file: " <> Text.pack configPath

-- | Write the agents-exe.cfg.json file
writeAgentsExeConfig :: FilePath -> IO ()
writeAgentsExeConfig path = do
    homeDir <- getHomeDirectory
    createDirectoryIfMissing True (takeDirectory path)
    Text.writeFile path (defaultAgentsExeConfigMinimal homeDir)
    Text.putStrLn "Created agents-exe.cfg.json with minimal configuration"

-------------------------------------------------------------------------------
-- Keymap Config Handler
-------------------------------------------------------------------------------

-- | Handle keymap config subcommands
handleKeymapConfig ::
    -- | Force overwrite
    Bool ->
    -- | Keymap config options
    KeymapConfigOptions ->
    IO ()
handleKeymapConfig force opts = do
    case opts.keymapCommand of
        KeymapInit -> do
            let filePath = opts.keymapFilePath
            unless force $ do
                exists <- doesFileExist filePath
                when exists $ do
                    Text.hPutStrLn stderr $ "Error: Keymap file already exists: " <> Text.pack filePath
                    Text.hPutStrLn stderr "Use --force to overwrite"
                    exitFailure

            -- Create directory if needed
            createDirectoryIfMissing True (takeDirectory filePath)

            -- Write default keymap as JSON
            LByteString.writeFile filePath $
                Aeson.encodePretty (defaultKeyMapping :: KeyMapping)

            Text.putStrLn $ "Created keymap file: " <> Text.pack filePath
            Text.putStrLn "Edit this file to customize keyboard shortcuts"

-------------------------------------------------------------------------------
-- API Key Config Handler
-------------------------------------------------------------------------------

-- | Handle API key config subcommands
handleApiKeyConfig ::
    -- | Force overwrite
    Bool ->
    -- | API key config options
    ApiKeyConfigOptions ->
    IO ()
handleApiKeyConfig force opts = do
    -- Ensure API keys file exists
    ensureApiKeysFile opts.apiKeysFilePath

    case opts.apiKeyCommand of
        ApiKeyList ->
            listApiKeys opts.apiKeysFilePath
        ApiKeyCreate keyName ->
            createApiKey force opts.apiKeysFilePath keyName

-- | Ensure the API keys file exists, create template if not
ensureApiKeysFile :: FilePath -> IO ()
ensureApiKeysFile path = do
    exists <- doesFileExist path
    unless exists $ do
        createDirectoryIfMissing True (takeDirectory path)
        LByteString.writeFile path $
            Aeson.encodePretty (ApiKeys [])
        Text.putStrLn $ "Created API keys file: " <> Text.pack path

-- | List all API key names
listApiKeys :: FilePath -> IO ()
listApiKeys path = do
    result <- try $ LByteString.readFile path
    case result of
        Left (e :: SomeException) -> do
            Text.hPutStrLn stderr $ "Error reading API keys file: " <> Text.pack (show e)
            exitFailure
        Right content -> do
            case Aeson.eitherDecode content of
                Left err -> do
                    Text.hPutStrLn stderr $ "Error parsing API keys file: " <> Text.pack err
                    exitFailure
                Right (ApiKeys keys) -> do
                    if null keys
                        then Text.putStrLn "No API keys configured"
                        else do
                            Text.putStrLn "Configured API keys:"
                            mapM_ (\k -> Text.putStrLn $ "  - " <> apiKeyId k) keys

-- | Create a new API key entry
createApiKey ::
    -- | Force overwrite
    Bool ->
    -- | API keys file path
    FilePath ->
    -- | Key name
    Text ->
    IO ()
createApiKey force path keyName = do
    -- Read existing keys
    result <- try $ LByteString.readFile path
    existingKeys <- case result of
        Left (e :: SomeException) -> do
            Text.hPutStrLn stderr $ "Error reading API keys file: " <> Text.pack (show e)
            exitFailure
        Right content ->
            case Aeson.eitherDecode content of
                Left _ -> pure []
                Right (ApiKeys keys) -> pure keys

    -- Check if key already exists
    case find (\k -> apiKeyId k == keyName) existingKeys of
        Just _existing -> do
            unless force $ do
                Text.hPutStrLn stderr $ "Error: API key '" <> keyName <> "' already exists"
                Text.hPutStrLn stderr "Use --force to overwrite the existing key"
                exitFailure
            -- Remove existing key to replace it
            let filteredKeys = filter (\k -> apiKeyId k /= keyName) existingKeys
            addAndSaveKey path keyName filteredKeys
        Nothing -> do
            addAndSaveKey path keyName existingKeys

-- | Add a new key and save the file
addAndSaveKey :: FilePath -> Text -> [ApiKey] -> IO ()
addAndSaveKey path keyName keys = do
    let newKey = ApiKey keyName "<insert-your-api-key-here>"
    let updatedKeys = ApiKeys (keys ++ [newKey])

    result <- try $ LByteString.writeFile path (Aeson.encodePretty updatedKeys)
    case result of
        Left (e :: SomeException) -> do
            Text.hPutStrLn stderr $ "Error writing API keys file: " <> Text.pack (show e)
            exitFailure
        Right () -> do
            Text.putStrLn $ "Created API key: " <> keyName
            Text.putStrLn $ "Please edit " <> Text.pack path <> " and set your actual API key value"

