{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module for the replay tool commands.

This module provides functionality to:

1. List tool calls from a session file ('list-tool-calls' command)
2. Replay a specific tool call, validating arguments and executing again ('replay-tool-call' command)

These commands are useful for debugging and reproducing tool calls from
previous sessions.

Usage:
    agents-exe list-tool-calls <session-file>
    agents-exe replay-tool-call --session <session-file> --tool-call <index> --tool <tool-path>
-}
module System.Agents.CLI.ReplayToolCall (
    handleListToolCalls,
    handleReplayToolCall,
    ListToolCallsOptions (..),
    ReplayToolCallOptions (..),
    ListFormat (..),
    ToolCallInfo (..),
) where

import Control.Exception (try)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import System.IO (stderr, stdout)
import System.Posix.Files as Posix

import qualified Prod.Tracer as Prod
import System.Agents.Session.Types (
    LlmToolCall (..),
    LlmTurnContent (..),
    Session (..),
    Turn (..),
 )
import System.Agents.Tools.Bash (ScriptDescription (..), ScriptInfo (..), loadScript, runValue)
import System.Agents.Tools.Validation (formatValidationErrors, validateToolInput)
import qualified System.Agents.ToolRegistration as ToolReg

-- | Information about a tool call extracted from a session
data ToolCallInfo = ToolCallInfo
    { toolCallIndex :: Int
    -- ^ Index of the tool call (for referencing)
    , toolCallTurnNumber :: Int
    -- ^ Which turn this tool call appeared in
    , toolCallName :: Text
    -- ^ Name of the tool (extracted from the tool call)
    , toolCallArguments :: Aeson.Value
    -- ^ The arguments passed to the tool
    }
    deriving (Show, Eq)

-- | Options for the list-tool-calls command
data ListToolCallsOptions = ListToolCallsOptions
    { listSessionFile :: FilePath
    -- ^ Path to the session file to extract tool calls from
    , listFormat :: ListFormat
    -- ^ Output format for the listing
    }
    deriving (Show, Eq)

-- | Output format for listing tool calls
data ListFormat
    = ListFormatHuman
    -- ^ Human-readable format with details
    | ListFormatJson
    -- ^ JSON format for machine processing
    | ListFormatBrief
    -- ^ Brief format: index name args-preview
    deriving (Show, Eq)

-- | Options for the replay-tool-call command
data ReplayToolCallOptions = ReplayToolCallOptions
    { replaySessionFile :: FilePath
    -- ^ Path to the session file containing the tool call
    , replayToolCallIndex :: Int
    -- ^ Index of the tool call to replay (0-based)
    , replayToolPath :: FilePath
    -- ^ Path to the tool script to execute
    , replayValidateOnly :: Bool
    -- ^ If True, only validate without executing
    , replayShowRaw :: Bool
    -- ^ Show raw output instead of formatted
    }
    deriving (Show, Eq)

-- | Result of extracting tool calls from a session
data ExtractResult
    = ExtractSuccess [ToolCallInfo]
    | ExtractFileNotFound FilePath
    | ExtractParseError FilePath String
    | ExtractNoContent FilePath

-- | Parse a session file and extract all tool calls
extractToolCallsFromSession :: FilePath -> IO ExtractResult
extractToolCallsFromSession sessionPath = do
    fileExists <- doesFileExist' sessionPath
    if not fileExists
        then pure $ ExtractFileNotFound sessionPath
        else do
            dat <- BSL.readFile sessionPath
            case extractLastLine dat >>= Aeson.decode of
                Nothing -> pure $ ExtractNoContent sessionPath
                Just session -> pure $ ExtractSuccess (extractFromSession session)
  where
    extractLastLine :: BSL.ByteString -> Maybe BSL.ByteString
    extractLastLine dat = case BSL.lines dat of
        [] -> Nothing
        rows -> Just (last rows)

    extractFromSession :: Session -> [ToolCallInfo]
    extractFromSession session = concat $ zipWith extractFromTurn [1 ..] session.turns

    extractFromTurn :: Int -> Turn -> [ToolCallInfo]
    extractFromTurn turnNum (LlmTurn content _) =
        mapMaybe (extractToolCall turnNum) (zip [0 ..] content.llmToolCalls)
    extractFromTurn _ (UserTurn _ _) = []

    extractToolCall :: Int -> (Int, LlmToolCall) -> Maybe ToolCallInfo
    extractToolCall turnNum (idx, LlmToolCall val) =
        case val of
            Aeson.Object obj -> do
                -- Extract function name and arguments
                funcVal <- KeyMap.lookup "function" obj
                funcObj <- case funcVal of
                    Aeson.Object o -> Just o
                    _ -> Nothing
                nameVal <- KeyMap.lookup "name" funcObj
                name <- case nameVal of
                    Aeson.String n -> Just n
                    _ -> Nothing
                argsVal <- KeyMap.lookup "arguments" funcObj
                -- Generate a global index based on position
                Just $
                    ToolCallInfo
                        { toolCallIndex = idx
                        , toolCallTurnNumber = turnNum
                        , toolCallName = name
                        , toolCallArguments = argsVal
                        }
            _ -> Nothing

doesFileExist' :: FilePath -> IO Bool
doesFileExist' path = do
    result <- try @IOError $ Posix.getFileStatus path
    case result of
        Left _ -> pure False
        Right stat -> pure $ Posix.isRegularFile stat

-- | Handle the list-tool-calls command
handleListToolCalls :: ListToolCallsOptions -> IO ()
handleListToolCalls opts = do
    result <- extractToolCallsFromSession opts.listSessionFile
    case result of
        ExtractFileNotFound path -> do
            Text.hPutStrLn stderr $ "Error: Session file not found: " <> Text.pack path
            exitFailure
        ExtractParseError path err -> do
            Text.hPutStrLn stderr $ "Error: Failed to parse session file " <> Text.pack path <> ": " <> Text.pack err
            exitFailure
        ExtractNoContent path -> do
            Text.hPutStrLn stderr $ "Error: No valid session content found in: " <> Text.pack path
            exitFailure
        ExtractSuccess [] -> do
            Text.putStrLn $ "No tool calls found in session: " <> Text.pack opts.listSessionFile
        ExtractSuccess toolCalls -> do
            case opts.listFormat of
                ListFormatHuman -> printHumanReadable toolCalls
                ListFormatJson -> printJson toolCalls
                ListFormatBrief -> printBrief toolCalls

printHumanReadable :: [ToolCallInfo] -> IO ()
printHumanReadable calls = do
    Text.putStrLn $ "Found " <> Text.pack (show $ length calls) <> " tool call(s):\n"
    mapM_ printCall calls
    Text.putStrLn "\nTo replay a tool call:"
    Text.putStrLn "  agents-exe replay-tool-call --session <file> --tool-call <index> --tool <tool-path>"
  where
    printCall :: ToolCallInfo -> IO ()
    printCall call = do
        Text.putStrLn $ "[" <> Text.pack (show call.toolCallIndex) <> "] " <> call.toolCallName
        Text.putStrLn $ "  Turn: " <> Text.pack (show call.toolCallTurnNumber)
        Text.putStrLn $ "  Arguments: "
        Text.putStrLn $ Text.pack $ "    " <> BSL.unpack (Aeson.encode call.toolCallArguments)
        Text.putStrLn ""

printJson :: [ToolCallInfo] -> IO ()
printJson calls = do
    BSL.putStr (Aeson.encode $ map callToJson calls)
    BSL.putStr "\n"
  where
    callToJson :: ToolCallInfo -> Aeson.Value
    callToJson call =
        Aeson.object
            [ "index" Aeson..= call.toolCallIndex
            , "turn" Aeson..= call.toolCallTurnNumber
            , "name" Aeson..= call.toolCallName
            , "arguments" Aeson..= call.toolCallArguments
            ]

printBrief :: [ToolCallInfo] -> IO ()
printBrief calls = do
    mapM_ printCall calls
  where
    printCall :: ToolCallInfo -> IO ()
    printCall call = do
        let argsPreview = Text.take 60 (Text.pack $ BSL.unpack $ Aeson.encode call.toolCallArguments)
        Text.putStrLn $ Text.pack (show call.toolCallIndex) <> "\t" <> call.toolCallName <> "\t" <> argsPreview

-- | Handle the replay-tool-call command
handleReplayToolCall :: ReplayToolCallOptions -> IO ()
handleReplayToolCall opts = do
    -- First, extract tool calls from the session
    extractResult <- extractToolCallsFromSession opts.replaySessionFile
    case extractResult of
        ExtractFileNotFound path -> do
            Text.hPutStrLn stderr $ "Error: Session file not found: " <> Text.pack path
            exitFailure
        ExtractParseError path err -> do
            Text.hPutStrLn stderr $ "Error: Failed to parse session file " <> Text.pack path <> ": " <> Text.pack err
            exitFailure
        ExtractNoContent path -> do
            Text.hPutStrLn stderr $ "Error: No valid session content found in: " <> Text.pack path
            exitFailure
        ExtractSuccess toolCalls -> do
            -- Find the specific tool call by index
            case filter (\c -> toolCallIndex c == opts.replayToolCallIndex) toolCalls of
                [] -> do
                    Text.hPutStrLn stderr $
                        "Error: Tool call with index "
                            <> Text.pack (show opts.replayToolCallIndex)
                            <> " not found"
                    Text.hPutStrLn stderr $ "Available indices: " <> Text.pack (intercalate ", " $ map (show . toolCallIndex) toolCalls)
                    exitFailure
                (targetCall : _) -> do
                    -- Load the tool
                    toolResult <- loadScript Prod.silent opts.replayToolPath
                    case toolResult of
                        Left err -> do
                            Text.hPutStrLn stderr $ "Failed to load tool: " <> Text.pack (show err)
                            exitFailure
                        Right scriptDesc -> do
                            -- Validate the tool call arguments
                            let schema = ToolReg.mapArg <$> scriptDesc.scriptInfo.scriptArgs
                            let errors = validateToolInput schema targetCall.toolCallArguments

                            if null errors
                                then do
                                    Text.putStrLn "✓ Tool call validation passed"
                                    Text.putStrLn $ "Tool: " <> scriptDesc.scriptInfo.scriptSlug
                                    Text.putStrLn $ "Arguments: " <> Text.pack (BSL.unpack $ Aeson.encode targetCall.toolCallArguments)

                                    unless opts.replayValidateOnly $ do
                                        Text.putStrLn "\nExecuting tool...\n"
                                        -- Execute the tool with the arguments
                                        -- We need to parse the arguments back to the format expected by runValue
                                        case targetCall.toolCallArguments of
                                            Aeson.Object argObj -> do
                                                runResult <- runValue Prod.silent scriptDesc Nothing (Aeson.Object argObj)
                                                case runResult of
                                                    Left err -> do
                                                        Text.hPutStrLn stderr $ "Tool execution failed: " <> Text.pack (show err)
                                                        exitFailure
                                                    Right output -> do
                                                        if opts.replayShowRaw
                                                            then BSL.hPut stdout (LByteString.fromStrict output) >> BSL.hPut stdout "\n"
                                                            else do
                                                                Text.putStrLn "=== Tool Output ==="
                                                                Text.putStrLn $ Text.decodeUtf8With Data.Text.Encoding.Error.lenientDecode output
                                                                Text.putStrLn "==================="
                                            _ -> do
                                                Text.hPutStrLn stderr "Error: Tool arguments must be a JSON object"
                                                exitFailure
                                else do
                                    let toolName =
                                            Text.pack opts.replayToolPath
                                                <> " ("
                                                <> scriptDesc.scriptInfo.scriptSlug
                                                <> ")"
                                    Text.putStrLn $ formatValidationErrors toolName errors
                                    exitFailure

