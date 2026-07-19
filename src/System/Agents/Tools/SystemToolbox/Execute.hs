{-# LANGUAGE ScopedTypeVariables #-}
{- | Arbitrary command execution for the SystemToolbox.

This module implements the @execute-command@ capability. When the capability
is enabled, the LLM can ask the toolbox to run an arbitrary shell command.

If the toolbox configuration includes a 'systemToolboxCommandFilter', the
requested command is first passed on stdin to that filter command. The filter
must print a single JSON object on stdout:

> {"acceptance":"allowed", "note":"optional explanation"}
> {"acceptance":"refused", "note":"reason for refusal"}

Any other output (including malformed JSON, missing fields, or non-zero exit)
is treated as an invalid filter output and the tool call is rejected.

When no filter is configured, commands are executed directly.
-}
module System.Agents.Tools.SystemToolbox.Execute (
    executeCommand,
    CommandOutput (..),
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode (..))
import System.Process (shell)
import System.Process.ByteString (readCreateProcessWithExitCode)

import System.Agents.Tools.SystemToolbox.Types (QueryError (..))

-- | Result of executing a command.
data CommandOutput = CommandOutput
    { commandOutputStdout :: !Text
    , commandOutputStderr :: !Text
    , commandOutputExitCode :: !Int
    }
    deriving (Show, Eq)

-- | Default timeout for command execution (30 seconds).
defaultCommandTimeoutSeconds :: Int
defaultCommandTimeoutSeconds = 30

-- | Execute an arbitrary shell command, optionally filtered by a configured
-- approval command.
--
-- When a filter command is provided, the requested command is written to the
-- filter's stdin. The filter must emit a JSON object with an @acceptance@
-- field of either @"allowed"@ or @"refused"@ and an optional @note@ field.
executeCommand ::
    -- | Optional filter command (e.g. "/path/to/filter.sh")
    Maybe Text ->
    -- | Command to execute
    Text ->
    IO (Either QueryError CommandOutput)
executeCommand mFilterCommand requestedCommand = do
    -- Run the filter if configured
    filterResult <- maybe (pure (Right FilterAllowed)) (runFilter requestedCommand) mFilterCommand
    case filterResult of
        Left err -> pure $ Left err
        Right FilterAllowed -> runCommandWithTimeout requestedCommand
        Right (FilterRefused note) -> pure $ Left $ CommandRefusedError note

-- | Internal result of running the filter command.
data FilterResult
    = FilterAllowed
    | FilterRefused !Text
    deriving (Show, Eq)

-- | Run the configured filter command with the requested command on stdin.
runFilter :: Text -> Text -> IO (Either QueryError FilterResult)
runFilter requestedCommand filterCommand = do
    result <- try $ readCreateProcessWithExitCode (shell (Text.unpack filterCommand)) (Text.encodeUtf8 requestedCommand)
    case result of
        Left (e :: SomeException) ->
            pure $ Left $ InvalidFilterOutputError $ "Filter command failed: " <> Text.pack (show e)
        Right (ExitFailure code, _, err) ->
            pure $ Left $ InvalidFilterOutputError $ "Filter command exited with code " <> Text.pack (show code) <> ": " <> decodeLenient err
        Right (ExitSuccess, out, _) ->
            pure $ parseFilterOutput out

-- | Parse the filter command's stdout as a JSON acceptance object.
parseFilterOutput :: ByteString -> Either QueryError FilterResult
parseFilterOutput out =
    case Aeson.eitherDecode (LByteString.fromStrict out) of
        Left jsonErr ->
            Left $ InvalidFilterOutputError $ "Filter output is not valid JSON: " <> Text.pack jsonErr
        Right value ->
            case Aeson.parseEither parseFilterResponse value of
                Left parseErr ->
                    Left $ InvalidFilterOutputError $ "Filter output missing required fields: " <> Text.pack parseErr
                Right (acceptance, mNote) ->
                    case acceptance of
                        "allowed" -> Right FilterAllowed
                        "refused" -> Right $ FilterRefused $ fromMaybe "Command refused by filter" mNote
                        other ->
                            Left $ InvalidFilterOutputError $ "Filter returned unknown acceptance value: " <> other

-- | Parser for the filter's JSON response.
parseFilterResponse :: Aeson.Value -> Aeson.Parser (Text, Maybe Text)
parseFilterResponse = Aeson.withObject "FilterResponse" $ \v ->
    (,) <$> v Aeson..: "acceptance" <*> v Aeson..:? "note"

-- | Run the requested shell command with a timeout.
runCommandWithTimeout :: Text -> IO (Either QueryError CommandOutput)
runCommandWithTimeout requestedCommand = do
    let timeoutMicros = defaultCommandTimeoutSeconds * 1000000
    result <- race (threadDelay timeoutMicros) (runCommand requestedCommand)
    case result of
        Left () -> pure $ Left $ SystemInfoError "Command execution timed out"
        Right val -> pure val

-- | Run the requested shell command and capture stdout/stderr.
runCommand :: Text -> IO (Either QueryError CommandOutput)
runCommand requestedCommand = do
    result <- try $ readCreateProcessWithExitCode (shell (Text.unpack requestedCommand)) ""
    case result of
        Left (e :: SomeException) ->
            pure $ Left $ SystemInfoError $ "Command execution failed: " <> Text.pack (show e)
        Right (exitCode, out, err) ->
            pure $ Right $ CommandOutput (decodeLenient out) (decodeLenient err) (exitCodeToInt exitCode)
  where
    exitCodeToInt :: ExitCode -> Int
    exitCodeToInt ExitSuccess = 0
    exitCodeToInt (ExitFailure n) = n

-- | Decode bytes to text, replacing invalid UTF-8 bytes safely.
decodeLenient :: ByteString -> Text
decodeLenient = Text.decodeUtf8With lenientDecode

