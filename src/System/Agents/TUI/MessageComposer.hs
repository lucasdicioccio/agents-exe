{-# LANGUAGE OverloadedStrings #-}

{- | Message composer configuration and logic for the TUI.

This module provides configurable input behavior for the message editor,
including the triple-newline send trigger as an alternative to keymap-based
message submission.
-}
module System.Agents.TUI.MessageComposer (
    -- * Send Trigger Configuration
    SendTrigger (..),
    InputConfig (..),
    defaultInputConfig,

    -- * Send Detection
    shouldSendMessage,
    willSendOnNextNewline,
    stripSendTrigger,
    getSendTriggerSuffix,

    -- * Configuration Loading
    loadInputConfig,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Default (Default (..))
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

-------------------------------------------------------------------------------
-- Send Trigger Types
-------------------------------------------------------------------------------

{- | Trigger mechanism for sending messages.

'Keymap' relies on keyboard shortcuts (e.g., Meta+Enter) to send messages.
'TripleNewline' sends messages when the user types three consecutive newlines
(\n\n\n), which is useful for multi-paragraph messages where double newline
creates a paragraph break.
-}
data SendTrigger
    = Keymap
    | TripleNewline
    deriving (Show, Eq, Ord)

instance ToJSON SendTrigger where
    toJSON Keymap = Aeson.String "keymap"
    toJSON TripleNewline = Aeson.String "triple_newline"

instance FromJSON SendTrigger where
    parseJSON = Aeson.withText "SendTrigger" $ \t ->
        case Text.toLower $ Text.strip t of
            "keymap" -> pure Keymap
            "triple_newline" -> pure TripleNewline
            "triple-newline" -> pure TripleNewline
            "triple newline" -> pure TripleNewline
            _ -> fail $ "Unknown send trigger: " ++ Text.unpack t

-------------------------------------------------------------------------------
-- Input Configuration
-------------------------------------------------------------------------------

{- | Configuration for message input behavior.

This controls how the message editor behaves, including send triggers
and visual feedback.
-}
data InputConfig = InputConfig
    { sendTrigger :: SendTrigger
    -- ^ How messages should be sent (keymap or triple-newline)
    , showSendIndicator :: Bool
    -- ^ Whether to show a visual indicator when message will be sent
    }
    deriving (Show, Eq)

instance Default InputConfig where
    def = defaultInputConfig

-- | Default input configuration using keymap-based sending.
defaultInputConfig :: InputConfig
defaultInputConfig =
    InputConfig
        { sendTrigger = Keymap
        , showSendIndicator = True
        }

instance ToJSON InputConfig where
    toJSON config =
        Aeson.object
            [ "send_trigger" .= sendTrigger config
            , "show_send_indicator" .= showSendIndicator config
            ]

instance FromJSON InputConfig where
    parseJSON = Aeson.withObject "InputConfig" $ \v ->
        InputConfig
            <$> v .:? "send_trigger" .!= Keymap
            <*> v .:? "show_send_indicator" .!= True

-------------------------------------------------------------------------------
-- Send Detection Logic
-------------------------------------------------------------------------------

-- | The triple newline pattern that triggers message sending.
tripleNewlinePattern :: Text
tripleNewlinePattern = "\n\n\n"

-- | The double newline pattern that indicates a paragraph break.
doubleNewlinePattern :: Text
doubleNewlinePattern = "\n\n"

{- | Check if the message content should trigger a send.

For 'TripleNewline' trigger, returns True if the content ends with
three consecutive newlines.

For 'Keymap' trigger, always returns False (sending is handled by key events).
-}
shouldSendMessage :: InputConfig -> Text -> Bool
shouldSendMessage config text =
    case sendTrigger config of
        Keymap -> False
        TripleNewline -> tripleNewlinePattern `Text.isSuffixOf` text

{- | Check if the message will send on the next newline.

This is used for visual feedback - when the user has typed double newline
and the next Enter keypress will send the message.

Returns True if:
- TripleNewline trigger is enabled
- Content ends with exactly double newline (not triple)
-}
willSendOnNextNewline :: InputConfig -> Text -> Bool
willSendOnNextNewline config text =
    case sendTrigger config of
        Keymap -> False
        TripleNewline ->
            doubleNewlinePattern `Text.isSuffixOf` text
                && not (tripleNewlinePattern `Text.isSuffixOf` text)

{- | Get the suffix that will trigger a send (for display purposes).

Returns the pattern that triggers sending for the current configuration.
-}
getSendTriggerSuffix :: InputConfig -> Maybe Text
getSendTriggerSuffix config =
    case sendTrigger config of
        Keymap -> Nothing
        TripleNewline -> Just tripleNewlinePattern

{- | Strip the send trigger suffix from the message content.

When sending via triple-newline, we want to remove the trailing newlines
from the actual message content that gets sent.
-}
stripSendTrigger :: InputConfig -> Text -> Text
stripSendTrigger config text =
    case sendTrigger config of
        Keymap -> text
        TripleNewline ->
            if tripleNewlinePattern `Text.isSuffixOf` text
                then Text.dropEnd 3 text
                else text

-------------------------------------------------------------------------------
-- Configuration Loading
-------------------------------------------------------------------------------

{- | Load input configuration from a JSON file.

If the file doesn't exist or can't be parsed, returns the default config
and prints a warning.
-}
loadInputConfig :: FilePath -> IO InputConfig
loadInputConfig path = do
    exists <- doesFileExist path
    if not exists
        then do
            hPutStrLn stderr $ "Input config not found: " ++ path
            hPutStrLn stderr "Using default input configuration."
            pure defaultInputConfig
        else do
            result <- Aeson.eitherDecodeFileStrict' path
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Warning: Failed to parse input config " ++ path ++ ": " ++ err
                    hPutStrLn stderr "Using default input configuration."
                    pure defaultInputConfig
                Right config -> do
                    hPutStrLn stderr $ "Loaded input config from: " ++ path
                    pure config

