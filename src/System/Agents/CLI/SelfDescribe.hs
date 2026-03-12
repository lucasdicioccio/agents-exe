{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for the 'describe' command handler.

The describe command outputs JSON metadata describing the agent's
interface. This is used for self-introspection and tool registration.
-}
module System.Agents.CLI.SelfDescribe (
    handleSelfDescribe,
    SelfDescribeOptions (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Agents.Tools.Bash (ScriptArg (..), ScriptArgArity (..), ScriptArgCallingMode (..), ScriptEmptyResultBehavior (..), ScriptInfo (..))

-- | Options for the self-describe command
data SelfDescribeOptions = SelfDescribeOptions
    { selfDescribeSlug :: Maybe String
    -- ^ The name/slug field for self-describe output (default: "self_call")
    , selfDescribeDescription :: Maybe String
    -- ^ The description/announce for self-describe output (default: "calls oneself with a prompt")
    }

-- | Handle the self-describe command: output agent metadata as JSON
handleSelfDescribe ::
    -- | Options for self-describe
    SelfDescribeOptions ->
    -- | Path to API keys file (verified to exist)
    FilePath ->
    IO ()
handleSelfDescribe opts _apiKeysFile = do
    -- verify the api-key file exists at least
    -- (verification happens before this function is called)
    let slug :: Text
        slug = Text.pack $ fromMaybe "self_call" opts.selfDescribeSlug
    let description :: Text
        description = Text.pack $ fromMaybe "calls oneself with a prompt" opts.selfDescribeDescription
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
                slug
                description
                (Just $ AddMessage "--no output--")
