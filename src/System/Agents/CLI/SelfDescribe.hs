{-# LANGUAGE OverloadedStrings #-}

-- | Module for the 'describe' command handler.
--
-- The describe command outputs JSON metadata describing the agent's
-- interface. This is used for self-introspection and tool registration.
module System.Agents.CLI.SelfDescribe
    ( handleSelfDescribe
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString

import System.Agents.Tools.Bash (ScriptEmptyResultBehavior (..), ScriptArg (..), ScriptArgArity (..), ScriptArgCallingMode (..), ScriptInfo (..))

-- | Handle the self-describe command: output agent metadata as JSON
handleSelfDescribe ::
    -- | Path to API keys file (verified to exist)
    FilePath ->
    IO ()
handleSelfDescribe _apiKeysFile = do
    -- verify the api-key file exists at least
    -- (verification happens before this function is called)
    LByteString.writeFile "/dev/stdout" $
        Aeson.encode $
            ScriptInfo
                [ ScriptArg
                    "prompt"
                    "the prompt to call the agent with"
                    "string"
                    "string"
                    Single
                    DashDashSpace
                ]
                "self_call"
                "calls oneself with a prompt"
                (Just $ AddMessage "--no output--")

