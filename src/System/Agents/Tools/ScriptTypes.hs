{-# LANGUAGE DeriveGeneric #-}

{- | Shared script types for bash tools and skills.

This module defines types that are shared between System.Agents.Tools.Bash
and System.Agents.Tools.Skills to avoid circular dependencies.
-}
module System.Agents.Tools.ScriptTypes (
    -- * Script Arguments
    ScriptArgArity (..),
    ScriptArgCallingMode (..),
    ScriptArg (..),

    -- * Script Info
    ScriptEmptyResultBehavior (..),
    ScriptInfo (..),

    -- * Script located on disk
    ScriptDescription (..),

    -- * Argument Translation
    translateArguments,
) where

import Data.Aeson (FromJSON, ToJSON, (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.List as List
import Data.Text (Text)
import GHC.Generics (Generic)

import System.Agents.Tools.ParamTier (ParamTier (..), defaultParamTier)

-------------------------------------------------------------------------------
-- Script Argument Types
-------------------------------------------------------------------------------

data ScriptArgArity
    = Single
    | Optional
    deriving (Show, Eq, Ord)

instance ToJSON ScriptArgArity where
    toJSON s = case s of
        Single -> Aeson.String "single"
        Optional -> Aeson.String "optional"

instance FromJSON ScriptArgArity where
    parseJSON v = case v of
        Aeson.String "single" -> pure Single
        Aeson.String "optional" -> pure Optional
        _ ->
            fail $
                List.unlines
                    [ "Invalid arity: " <> Prelude.show v
                    , "allowed values are:"
                    , "- single"
                    ]

data ScriptArgCallingMode
    = Stdin
    | Positional
    | DashDashSpace
    | DashDashEqual
    deriving (Show, Eq, Ord)

instance ToJSON ScriptArgCallingMode where
    toJSON s = case s of
        Stdin -> Aeson.String "stdin"
        Positional -> Aeson.String "positional"
        DashDashSpace -> Aeson.String "dashdashspace"
        DashDashEqual -> Aeson.String "dashdashequal"

instance FromJSON ScriptArgCallingMode where
    parseJSON v = case v of
        Aeson.String "stdin" -> pure Stdin
        Aeson.String "positional" -> pure Positional
        Aeson.String "dashdashspace" -> pure DashDashSpace
        Aeson.String "dashdashequal" -> pure DashDashEqual
        _ ->
            fail $
                List.unlines
                    [ "Invalid mode: " <> Prelude.show v
                    , "allowed values are:"
                    , "- positional"
                    , "- stdin"
                    , "- dashdashspace"
                    , "- dashdashequal"
                    ]

data ScriptArg
    = ScriptArg
    { argName :: Text
    , argDescription :: Text
    , argTypeString :: Text
    , argBackingTypeString :: Text
    , argTypeArity :: ScriptArgArity
    , argCallingMode :: ScriptArgCallingMode
    , argTier :: ParamTier
    {- ^ The visibility tier for progressive disclosure.
    Basic arguments are always shown, Advanced are hidden by default,
    and Expert are deeply hidden. Defaults to Basic for backward compatibility.
    -}
    }
    deriving (Show, Eq, Ord)

-- | ToJSON instance for ScriptArg (internal format using Haskell field names)
instance ToJSON ScriptArg where
    toJSON s =
        Aeson.object $
            [ "name" .= s.argName
            , "description" .= s.argDescription
            , "type" .= s.argTypeString
            , "backing_type" .= s.argBackingTypeString
            , "arity" .= s.argTypeArity
            , "mode" .= s.argCallingMode
            ]
                <> ["tier" .= s.argTier | s.argTier /= defaultParamTier]

instance Aeson.FromJSON ScriptArg where
    parseJSON = Aeson.withObject "ScriptArg" $ \o ->
        ScriptArg
            <$> o Aeson..: "name"
            <*> o Aeson..: "description"
            <*> o Aeson..: "type"
            <*> o Aeson..: "backing_type"
            <*> o Aeson..: "arity"
            <*> o Aeson..: "mode"
            <*> o Aeson..:? "tier" Aeson..!= defaultParamTier

-------------------------------------------------------------------------------
-- Script Info Types
-------------------------------------------------------------------------------

data ScriptEmptyResultBehavior
    = DoNothing
    | AddMessage !Text
    deriving (Generic, Show, Eq, Ord)

instance ToJSON ScriptEmptyResultBehavior

instance FromJSON ScriptEmptyResultBehavior

{- | Script metadata from the describe command.

Contains information about the script's arguments, slug, description,
and optional behaviors like empty result handling and output media type.

The 'scriptOutputMediaType' field allows tools to declare that they output
binary media (e.g., images, audio) rather than text. This enables proper
handling of binary content in LLM interactions.
-}
data ScriptInfo
    = ScriptInfo
    { scriptArgs :: [ScriptArg]
    , scriptSlug :: Text
    , scriptDescription :: Text
    , scriptEmptyResultBehavior :: Maybe ScriptEmptyResultBehavior
    , scriptOutputMediaType :: Maybe Text
    {- ^ Optional MIME type for binary output (e.g., "image/png", "application/pdf")
    When set, the tool's output is treated as base64-encoded binary data.
    -}
    }
    deriving (Show, Eq, Ord)

-- | ToJSON instance for ScriptInfo (internal format using Haskell field names)
instance ToJSON ScriptInfo where
    toJSON s =
        Aeson.object $
            [ "args" .= s.scriptArgs
            , "slug" .= s.scriptSlug
            , "description" .= s.scriptDescription
            ]
                <> maybe [] (\seb -> ["empty-result" .= seb]) s.scriptEmptyResultBehavior
                <> maybe [] (\mime -> ["output-media-type" .= mime]) s.scriptOutputMediaType

instance Aeson.FromJSON ScriptInfo where
    parseJSON = Aeson.withObject "Script" $ \o ->
        ScriptInfo
            <$> o Aeson..: "args"
            <*> o Aeson..: "slug"
            <*> o Aeson..: "description"
            <*> o Aeson..:? "empty-result"
            <*> o Aeson..:? "output-media-type"

-------------------------------------------------------------------------------
-- Argument Translation
-------------------------------------------------------------------------------

{- | Translates arguments by collecting script arguments with their associated values.

This function only uses the 'argName' field of ScriptArg, making it usable
with any ScriptArg regardless of where it was defined (Bash tools or Skills).
-}
translateArguments :: ScriptInfo -> Aeson.Value -> Aeson.Parser [(ScriptArg, Maybe Text)]
translateArguments script = Aeson.withObject "Args" $ \v -> do
    vals <- traverse (parseArg v) script.scriptArgs
    pure $ List.zip script.scriptArgs vals
  where
    parseArg :: Aeson.Object -> ScriptArg -> Aeson.Parser (Maybe Text)
    parseArg v arg = v .:? (textToKey $ arg.argName)

    textToKey :: Text -> Aeson.Key
    textToKey = read . Prelude.show

data ScriptDescription
    = ScriptDescription
    { scriptPath :: FilePath
    , scriptInfo :: ScriptInfo
    }
    deriving (Show, Eq, Ord)

instance ToJSON ScriptDescription where
    toJSON s =
        Aeson.object $
            [ "path" .= s.scriptPath
            , "info" .= s.scriptInfo
            ]

instance Aeson.FromJSON ScriptDescription where
    parseJSON = Aeson.withObject "ScriptDescription" $ \o ->
        ScriptDescription
            <$> o Aeson..: "path"
            <*> o Aeson..: "info"
