{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'cowsay' command handler.

The cowsay command displays a message in a speech bubble
next to the ASCII-art lambda logo (the agents-exe mascot).
Similar to the classic Unix cowsay utility, but with our
friendly lambda mascot instead of a cow.
-}
module System.Agents.CLI.Cowsay (
    -- * Options
    CowsayOptions (..),

    -- * Handler
    handleCowsay,

    -- * Formatting
    formatCowsay,
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (stdin)

-- | Options for the cowsay command.
data CowsayOptions = CowsayOptions
    { cowsayMessage :: Maybe Text.Text
    -- ^ Optional message to display. If not provided, reads from stdin.
    , cowsayMaxWidth :: Int
    -- ^ Maximum width for the speech bubble (default: 40)
    }
    deriving (Show, Eq)

{- | Default ASCII art logo for the agents-exe mascot.
This is the friendly lambda character that "speaks" the message.
-}
defaultLogo :: [Text.Text]
defaultLogo =
    [ "    λλ"
    , "   λ⊙λ⊙"
    , "  λλλλλλ"
    , " λλλλλλλλ"
    , "    λλ"
    , "    λλ"
    , "    λλ"
    , "    λλ"
    , "    λλ"
    , "    λλ"
    , "    λλ"
    , "    λλ"
    ]

-- | Handle the cowsay command.
handleCowsay :: CowsayOptions -> IO ()
handleCowsay opts = do
    message <- case opts.cowsayMessage of
        Just msg -> pure msg
        Nothing -> Text.hGetContents stdin

    let output = formatCowsay opts.cowsayMaxWidth message
    Text.putStrLn output

{- | Format a message with the lambda mascot.

The output format is similar to the classic cowsay:
@
 _______________
< Hello, World! >
 ---------------
        \
         \
       λλ
      λ⊙λ⊙
     λλλλλλ
    λλλλλλλλ
       λλ
       ...
@
-}
formatCowsay :: Int -> Text.Text -> Text.Text
formatCowsay maxWidth message =
    let lines' = wrapText maxWidth message
        bubble = formatSpeechBubble lines'
        mascot = formatMascot
     in bubble <> "\n" <> mascot

-- | Wrap text to fit within the specified maximum width.
wrapText :: Int -> Text.Text -> [Text.Text]
wrapText maxWidth text =
    let words' = Text.words text
        go :: [Text.Text] -> Text.Text -> [Text.Text] -> [Text.Text]
        go [] current acc = if Text.null current then acc else acc ++ [current]
        go (w : ws) current acc
            | Text.null current = go ws w acc
            | Text.length current + 1 + Text.length w <= maxWidth = go ws (current <> " " <> w) acc
            | otherwise = go ws w (acc ++ [current])
     in go words' Text.empty []

-- | Format the speech bubble around the message lines.
formatSpeechBubble :: [Text.Text] -> Text.Text
formatSpeechBubble [] = formatSpeechBubble [""]
formatSpeechBubble lines' =
    let contentWidth = maximum (map Text.length lines')
        -- Pad all lines to the same width
        paddedLines = map (Text.justifyLeft contentWidth ' ') lines'

        -- Build the bubble borders
        topBorder = " " <> Text.replicate (contentWidth + 2) "_"
        bottomBorder = " " <> Text.replicate (contentWidth + 2) "-"

        -- Format content lines based on how many there are
        content = case paddedLines of
            [single] ->
                -- Single line: use < >
                "< " <> single <> " >"
            _ ->
                -- Multiple lines: use / | \
                let firstLine = "/ " <> head paddedLines <> " \\"
                    lastLine = "\\ " <> last paddedLines <> " /"
                    -- Middle lines use | on both sides
                    middleLines = case tail (init paddedLines) of
                        [] -> []
                        mids -> map (\line -> "| " <> line <> " |") mids
                 in Text.unlines $ [firstLine] ++ middleLines ++ [lastLine]
     in topBorder <> "\n" <> content <> "\n" <> bottomBorder

-- | Format the mascot with proper alignment.
formatMascot :: Text.Text
formatMascot =
    let
        -- The leader lines connect the speech bubble to the mascot
        leader = "        \\\n         \\"
     in
        leader <> "\n" <> Text.unlines defaultLogo
