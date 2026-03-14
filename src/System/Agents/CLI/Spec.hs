{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Module for the 'spec' command handler.

The spec command displays embedded documentation for various specifications
and protocols used by agents-exe.
-}
module System.Agents.CLI.Spec (
    handleSpec,
    SpecOptions (..),
    SpecTopic (..),
) where

import Data.FileEmbed (embedStringFile)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- | Available specification topics
data SpecTopic
    = BashToolsSpec
    deriving (Show, Eq)

-- | Options for the spec command
data SpecOptions = SpecOptions
    { specTopic :: SpecTopic
    }
    deriving (Show, Eq)

-- | Embedded bash-tools documentation
bashToolsDocumentation :: Text.Text
bashToolsDocumentation = $(embedStringFile "docs/binary-tool.md")

-- | Handle the spec command: display embedded documentation
handleSpec ::
    -- | Options for spec command
    SpecOptions ->
    IO ()
handleSpec opts = case opts.specTopic of
    BashToolsSpec ->
        Text.putStrLn bashToolsDocumentation
