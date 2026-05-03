{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'tool' command handler.

This module provides commands for:
- Pinning tools with pre-configured arguments
- Calling tools (both regular and pinned variants)
- Listing pinned tools
- Removing pinned tool configurations

Usage examples:

> # Pin a tool with arguments
> agents-exe tool pin bash --pin-arg "cwd=/project/src" --name "bash-in-src"
>
> # Call a pinned tool
> agents-exe tool call bash-in-src --json '{"cmd": "ls -la"}'
>
> # Call a regular tool
> agents-exe tool call bash --json '{"cmd": "pwd"}'
>
> # List pinned tools
> agents-exe tool list
>
> # Unpin a tool
> agents-exe tool unpin bash-in-src

-}
module System.Agents.CLI.ToolCommands (
    -- * Command types
    ToolCommand (..),
    ToolPinOptions (..),
    ToolCallOptions' (..),
    ToolListOptions (..),
    ToolUnpinOptions (..),

    -- * List format
    ListFormat (..),

    -- * Handler
    handleToolCommand,

    -- * Trace
    Trace (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr)

import Prod.Tracer (Tracer, contramap)

import qualified System.Agents.AgentTree as AgentTree
import qualified System.Agents.AgentTree.OneShotTool as OneShotTool
import System.Agents.CLI.PinnedTools (
    PinnedArg (..),
    PinnedTool (..),
    PinnedToolsConfig (..),
    addPinnedTool,
    applyPinnedArgs,
    findPinnedTool,
    loadPinnedTools,
    parsePinnedArg,
    removePinnedTool,
    savePinnedTools,
    validatePinnedToolName,
 )
import System.Agents.SessionStore (SessionStore)
import System.Agents.ToolPortal (makeToolPortal)
import qualified System.Agents.ToolPortal as ToolPortal
import qualified System.Agents.Tools.Context as ToolContext

-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

-- | Trace events from tool commands
data Trace
    = AgentTreeTrace !AgentTree.TreeTrace
    | OneShotToolTrace !OneShotTool.Trace
    | ToolPortalTrace !ToolPortal.Trace
    deriving (Show)

-------------------------------------------------------------------------------
-- Command Types
-------------------------------------------------------------------------------

-- | Main tool command
data ToolCommand
    = ToolPin ToolPinOptions
    | ToolCallCmd ToolCallOptions'
    | ToolList ToolListOptions
    | ToolUnpin ToolUnpinOptions
    deriving (Show, Eq)

-- | Options for the 'tool pin' command
data ToolPinOptions = ToolPinOptions
    { pinToolName :: Text
    -- ^ Name of the base tool to pin (e.g., "bash")
    , pinArgs :: [String]
    -- ^ Arguments to pin (format: "name=value")
    , pinAlias :: Maybe Text
    -- ^ Optional alias/name for the pinned tool (defaults to base tool name)
    , pinDescription :: Maybe Text
    -- ^ Optional description for the pinned tool
    }
    deriving (Show, Eq)

-- | Options for the 'tool call' command
data ToolCallOptions' = ToolCallOptions'
    { callTarget :: Text
    -- ^ Name of the tool or pinned tool to call
    , callArgsJson :: Maybe Text
    -- ^ JSON arguments as a string (overrides pinned args)
    , callArgsFile :: Maybe FilePath
    -- ^ Path to JSON file containing arguments
    , callLogFile :: Maybe FilePath
    -- ^ Optional log file for tracing
    , allowPinned :: Bool
    -- ^ Whether to allow resolving pinned tools (default: True)
    }
    deriving (Show, Eq)

-- | Options for the 'tool list' command
data ToolListOptions = ToolListOptions
    { listFormat :: ListFormat
    -- ^ Output format for the list
    , listShowDetails :: Bool
    -- ^ Whether to show detailed information including pinned args
    }
    deriving (Show, Eq)

-- | Output format for listing pinned tools
data ListFormat
    = ListFormatHuman
    | ListFormatJson
    deriving (Show, Eq)

-- | Options for the 'tool unpin' command
data ToolUnpinOptions = ToolUnpinOptions
    { unpinName :: Text
    -- ^ Name of the pinned tool to remove
    }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Handler
-------------------------------------------------------------------------------

-- | Handle the tool command
handleToolCommand ::
    Tracer IO Trace ->
    SessionStore ->
    FilePath ->
    -- ^ API keys file
    [FilePath] ->
    -- ^ Agent files
    ToolCommand ->
    IO ()
handleToolCommand tracer sessionStore apiKeysFile agentFiles cmd = case cmd of
    ToolPin opts -> handleToolPin opts
    ToolCallCmd opts -> handleToolCall' tracer sessionStore apiKeysFile agentFiles opts
    ToolList opts -> handleToolList opts
    ToolUnpin opts -> handleToolUnpin opts

-------------------------------------------------------------------------------
-- Pin Handler
-------------------------------------------------------------------------------

-- | Handle the 'tool pin' command
handleToolPin :: ToolPinOptions -> IO ()
handleToolPin opts = do
    -- Validate the pinned name
    let newPinnedName = fromMaybe opts.pinToolName opts.pinAlias
    case validatePinnedToolName newPinnedName of
        Left err -> do
            Text.hPutStrLn stderr $ "Error: " <> Text.pack err
            exitFailure
        Right () -> pure ()

    -- Parse all pinned arguments
    parsedArgs <- parsePinArgs opts.pinArgs

    -- Create the pinned tool configuration
    let argMap = Map.fromList [(argName arg, argValue arg) | arg <- parsedArgs]
    let newPinnedTool =
            PinnedTool
                { pinnedName = newPinnedName
                , pinnedTool = opts.pinToolName
                , pinnedArgs = argMap
                , pinnedDescription = opts.pinDescription
                }

    -- Load existing config and add the new pinned tool
    configResult <- loadPinnedTools
    config <- case configResult of
        Left err -> do
            Text.hPutStrLn stderr $ "Error loading pinned tools: " <> Text.pack err
            exitFailure
        Right cfg -> pure cfg

    let updatedConfig = addPinnedTool newPinnedTool config

    -- Save the updated config
    savePinnedTools updatedConfig

    -- Output success message
    putStrLn $ "Pinned tool '" <> Text.unpack newPinnedName <> "' created"
    putStrLn $ "  Base tool: " <> Text.unpack opts.pinToolName
    putStrLn $ "  Pinned args: " <> show (Map.keys argMap)

-- | Parse all pin arguments, exiting on error
parsePinArgs :: [String] -> IO [PinnedArg]
parsePinArgs = mapM parseSingleArg
  where
    parseSingleArg :: String -> IO PinnedArg
    parseSingleArg arg = case parsePinnedArg arg of
        Left err -> do
            Text.hPutStrLn stderr $ "Error parsing pin-arg: " <> Text.pack err
            exitFailure
        Right pa -> pure pa

-------------------------------------------------------------------------------
-- Call Handler
-------------------------------------------------------------------------------

-- | Handle the 'tool call' command
handleToolCall' ::
    Tracer IO Trace ->
    SessionStore ->
    FilePath ->
    -- ^ API keys file
    [FilePath] ->
    -- ^ Agent files
    ToolCallOptions' ->
    IO ()
handleToolCall' tracer sessionStore apiKeysFile agentFiles opts = do
    -- Load pinned tools config
    pinnedConfig <- loadPinnedToolsOrEmpty

    -- Resolve the target (check if it's a pinned tool)
    let (targetTool, resolvedPinnedArgs) = resolveTarget opts.callTarget pinnedConfig

    -- Load user-provided arguments
    userArgs <- loadUserArgs opts

    -- Merge pinned args with user args (user args take precedence)
    let finalArgs = mergeArguments resolvedPinnedArgs userArgs

    -- Execute the tool call
    executeToolCall tracer sessionStore apiKeysFile agentFiles targetTool finalArgs opts.callLogFile
  where
    resolveTarget target cfg =
        if opts.allowPinned
            then case findPinnedTool target cfg of
                Just pt -> (pt.pinnedTool, pt.pinnedArgs)
                Nothing -> (target, Map.empty)
            else (target, Map.empty)

-- | Load pinned tools or return empty config on error
loadPinnedToolsOrEmpty :: IO PinnedToolsConfig
loadPinnedToolsOrEmpty = do
    result <- loadPinnedTools
    case result of
        Left _ -> pure (PinnedToolsConfig [])
        Right cfg -> pure cfg

-- | Load user arguments from JSON string or file
loadUserArgs :: ToolCallOptions' -> IO Aeson.Object
loadUserArgs opts = do
    case (opts.callArgsJson, opts.callArgsFile) of
        (Nothing, Nothing) -> pure KeyMap.empty
        (Just jsonStr, _) -> parseJsonArgs jsonStr
        (_, Just filePath) -> do
            content <- LByteString.readFile filePath
            case Aeson.eitherDecode content of
                Left err -> do
                    Text.hPutStrLn stderr $ "Error reading args file: " <> Text.pack err
                    exitFailure
                Right (Aeson.Object obj) -> pure obj
                Right _ -> do
                    Text.hPutStrLn stderr "Error: Args file must contain a JSON object"
                    exitFailure
  where
    parseJsonArgs txt =
        case Aeson.eitherDecode (LByteString.fromStrict $ Text.Encoding.encodeUtf8 txt) of
            Left err -> do
                Text.hPutStrLn stderr $ "Error parsing JSON args: " <> Text.pack err
                exitFailure
            Right (Aeson.Object obj) -> pure obj
            Right _ -> do
                Text.hPutStrLn stderr "Error: JSON args must be an object"
                exitFailure

-- | Merge pinned arguments with user arguments (user args take precedence)
mergeArguments :: Map Text Aeson.Value -> Aeson.Object -> Aeson.Value
mergeArguments pinnedArgsMap userArgs =
    Aeson.Object $ applyPinnedArgs pinnedArgsMap userArgs

-- | Execute the actual tool call through the agent tree
executeToolCall ::
    Tracer IO Trace ->
    SessionStore ->
    FilePath ->
    -- ^ API keys file
    [FilePath] ->
    -- ^ Agent files
    Text ->
    -- ^ Target tool name
    Aeson.Value ->
    -- ^ Final arguments (merged)
    Maybe FilePath ->
    -- ^ Optional log file (currently unused)
    IO ()
executeToolCall tracer sessionStore apiKeysFile agentFiles toolName args _mLogFile = do
    apiKeys <- AgentTree.readOpenApiKeysFile apiKeysFile

    -- Process only the first agent file
    case agentFiles of
        [] -> do
            Text.hPutStrLn stderr "Error: No agent file specified"
            exitFailure
        (agentFilePath : _) -> do
            AgentTree.withAgentTree
                AgentTree.Props
                    { AgentTree.apiKeys = apiKeys
                    , AgentTree.apiKeysFile = apiKeysFile
                    , AgentTree.rootAgentFile = agentFilePath
                    , AgentTree.interactiveTracer = contramap AgentTreeTrace tracer
                    , AgentTree.agentToTool = OneShotTool.turnAgentRuntimeIntoIOTool (contramap OneShotToolTrace tracer) sessionStore apiKeys
                    }
                $ \result -> case result of
                    AgentTree.Errors errs -> do
                        Text.hPutStrLn stderr "Errors loading agent:"
                        mapM_ (Text.hPutStrLn stderr . Text.pack . show) errs
                        exitFailure
                    AgentTree.Initialized tree -> do
                        let rootNode = AgentTree.osTreeRoot tree
                        let portal = makeToolPortal (contramap ToolPortalTrace tracer) (AgentTree.osNodeTools rootNode)

                        -- Create the tool call
                        let toolCall =
                                ToolContext.ToolCall
                                    { ToolContext.callToolName = toolName
                                    , ToolContext.callArgs = args
                                    }

                        -- Execute the tool call
                        result' <- portal Nothing toolCall

                        -- Output the result as JSON
                        LByteString.putStr $ Aeson.encode result'
                        LByteString.putStr "\n"

-------------------------------------------------------------------------------
-- List Handler
-------------------------------------------------------------------------------

-- | Handle the 'tool list' command
handleToolList :: ToolListOptions -> IO ()
handleToolList opts = do
    configResult <- loadPinnedTools
    case configResult of
        Left err -> do
            Text.hPutStrLn stderr $ "Error loading pinned tools: " <> Text.pack err
            exitFailure
        Right cfg -> case opts.listFormat of
            ListFormatJson -> outputJsonList cfg
            ListFormatHuman -> outputHumanList opts.listShowDetails cfg

-- | Output pinned tools as JSON
outputJsonList :: PinnedToolsConfig -> IO ()
outputJsonList cfg = do
    LByteString.putStr $ Aeson.encodePretty cfg
    LByteString.putStr "\n"

-- | Output pinned tools in human-readable format
outputHumanList :: Bool -> PinnedToolsConfig -> IO ()
outputHumanList showDetails cfg = do
    let tools = pinnedTools cfg
    if null tools
        then putStrLn "No pinned tools configured."
        else do
            putStrLn "Pinned tools:"
            mapM_ (printTool showDetails) tools
  where
    printTool :: Bool -> PinnedTool -> IO ()
    printTool details pt = do
        putStrLn $ ""
        putStrLn $ "  " <> Text.unpack pt.pinnedName
        putStrLn $ "    Base tool: " <> Text.unpack pt.pinnedTool
        case pt.pinnedDescription of
            Just desc -> putStrLn $ "    Description: " <> Text.unpack desc
            Nothing -> pure ()
        if details
            then do
                putStrLn "    Pinned arguments:"
                if Map.null pt.pinnedArgs
                    then putStrLn "      (none)"
                    else mapM_ printArg (Map.toList pt.pinnedArgs)
            else putStrLn $ "    Pinned args: " <> show (Map.keys pt.pinnedArgs)

    printArg (name, val) = do
        let valStr = Text.unpack $ textDecodeUtf8 $ LByteString.toStrict $ Aeson.encode val
        putStrLn $ "      " <> Text.unpack name <> " = " <> valStr

    -- Safe decoding that replaces invalid characters
    textDecodeUtf8 :: ByteString -> Text
    textDecodeUtf8 = Text.Encoding.decodeUtf8With (\_ _ -> Just '?')

-------------------------------------------------------------------------------
-- Unpin Handler
-------------------------------------------------------------------------------

-- | Handle the 'tool unpin' command
handleToolUnpin :: ToolUnpinOptions -> IO ()
handleToolUnpin opts = do
    configResult <- loadPinnedTools
    case configResult of
        Left err -> do
            Text.hPutStrLn stderr $ "Error loading pinned tools: " <> Text.pack err
            exitFailure
        Right cfg -> case removePinnedTool opts.unpinName cfg of
            Nothing -> do
                Text.hPutStrLn stderr $ "Pinned tool '" <> opts.unpinName <> "' not found"
                exitFailure
            Just updatedCfg -> do
                savePinnedTools updatedCfg
                putStrLn $ "Removed pinned tool: " <> Text.unpack opts.unpinName

