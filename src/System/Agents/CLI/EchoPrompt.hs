{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module for the 'echo-prompt' command handler.
--
-- The echo-prompt command interprets a prompt script and outputs the
-- resulting prompt text to stdout. This is useful for debugging prompt
-- composition without actually running the agent.
module System.Agents.CLI.EchoPrompt
    ( -- * Types
      EchoPromptOptions (..)
      -- * Handler
    , handleEchoPrompt
    ) where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.IO as Text

import System.Agents.CLI.Aliases (AliasDefinition)
import System.Agents.CLI.PromptScript (PromptScript, interpretPromptScript)

-- | Options for the echo-prompt command
data EchoPromptOptions = EchoPromptOptions
    { sessionFile :: Maybe FilePath
    , promptScript :: PromptScript
    }
    deriving (Show)

-- | Handle the echo-prompt command: interpret and print the prompt
handleEchoPrompt ::
    -- | Available prompt aliases
    Map Text AliasDefinition ->
    -- | Echo prompt options
    EchoPromptOptions ->
    IO ()
handleEchoPrompt aliases opts = do
    promptContents <- interpretPromptScript aliases opts.promptScript opts.sessionFile
    Text.putStr promptContents

