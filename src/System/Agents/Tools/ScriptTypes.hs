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

    -- * Subcommands
    ScriptCommand (..),
    validateScriptCommands,
    expandScriptCommands,

    -- * Protocol v2
    scriptProcessOf,
    validateScriptV2,

    -- * Script located on disk
    ScriptDescription (..),

    -- * Argument Translation
    translateArguments,
    argEnvVarName,
) where

import Data.Aeson (FromJSON, ToJSON, (.!=), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import System.Agents.Tools.ScriptProcess (
    OutputFraming (..),
    ProcessMode (..),
    ProcessOutput (..),
    ProcessReady (..),
    ScriptProcess (..),
    defaultScriptProcess,
 )

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
    | Env
    deriving (Show, Eq, Ord)

instance ToJSON ScriptArgCallingMode where
    toJSON s = case s of
        Stdin -> Aeson.String "stdin"
        Positional -> Aeson.String "positional"
        DashDashSpace -> Aeson.String "dashdashspace"
        DashDashEqual -> Aeson.String "dashdashequal"
        Env -> Aeson.String "env"

instance FromJSON ScriptArgCallingMode where
    parseJSON v = case v of
        Aeson.String "stdin" -> pure Stdin
        Aeson.String "positional" -> pure Positional
        Aeson.String "dashdashspace" -> pure DashDashSpace
        Aeson.String "dashdashequal" -> pure DashDashEqual
        Aeson.String "env" -> pure Env
        _ ->
            fail $
                List.unlines
                    [ "Invalid mode: " <> Prelude.show v
                    , "allowed values are:"
                    , "- positional"
                    , "- stdin"
                    , "- dashdashspace"
                    , "- dashdashequal"
                    , "- env"
                    ]

data ScriptArg
    = ScriptArg
    { argName :: Text
    , argDescription :: Text
    , argTypeString :: Text
    , argBackingTypeString :: Text
    , argTypeArity :: ScriptArgArity
    , argCallingMode :: ScriptArgCallingMode
    }
    deriving (Show, Eq, Ord)

-- | ToJSON instance for ScriptArg (internal format using Haskell field names)
instance ToJSON ScriptArg where
    toJSON s =
        Aeson.object
            [ "name" .= s.argName
            , "description" .= s.argDescription
            , "type" .= s.argTypeString
            , "backing_type" .= s.argBackingTypeString
            , "arity" .= s.argTypeArity
            , "mode" .= s.argCallingMode
            ]

instance Aeson.FromJSON ScriptArg where
    parseJSON = Aeson.withObject "ScriptArg" $ \o ->
        ScriptArg
            <$> o Aeson..: "name"
            <*> o Aeson..: "description"
            <*> o Aeson..: "type"
            <*> o Aeson..: "backing_type"
            <*> o Aeson..: "arity"
            <*> o Aeson..: "mode"

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

A script that does several things through subcommands lists them in
'scriptCommands' (the @commands@ field of @describe@). Such a description is
not a tool by itself: 'expandScriptCommands' turns it into one tool per
subcommand, each with 'scriptSubcommand' set, and @run@ is then called as
@run <subcommand> <args...>@. See @todos/bash-tools-subcommands.md@.
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
    , scriptCommands :: [ScriptCommand]
    {- ^ Subcommands declared by @describe@ (empty for a single-command tool).
    The top-level 'scriptArgs' are shared by every subcommand.
    -}
    , scriptSubcommand :: Maybe Text
    {- ^ Set by 'expandScriptCommands' only: the subcommand this tool runs,
    passed right after @run@.
    -}
    , scriptProtocol :: Int
    {- ^ The @protocol@ field of @describe@: 1 (absent) or 2. See
    @todos/bash-tools-v2.md@. The v2 fields below are only read when it is 2,
    so a v2 tool can be tried as a v1 tool by ignoring them (D1).
    -}
    , scriptProcess :: Maybe ScriptProcess
    -- ^ The v2 @process@ object; a v2 tool without one is a @oneshot@ with @jsonl@ output ('scriptProcessOf').
    , scriptRuntime :: Maybe Aeson.Value
    -- ^ The v2 @runtime@ object, kept raw: not implemented yet, refused at load ('validateScriptV2').
    , scriptCheck :: Bool
    -- ^ The v2 @check@ flag (the @check@ verb is a later slice; recorded only).
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
                <> (if List.null s.scriptCommands then [] else ["commands" .= s.scriptCommands])
                <> maybe [] (\sub -> ["subcommand" .= sub]) s.scriptSubcommand
                <> (if s.scriptProtocol == 1 then [] else ["protocol" .= s.scriptProtocol])
                <> maybe [] (\p -> ["process" .= p]) s.scriptProcess
                <> maybe [] (\r -> ["runtime" .= r]) s.scriptRuntime
                <> (if s.scriptCheck then ["check" .= True] else [])

instance Aeson.FromJSON ScriptInfo where
    parseJSON = Aeson.withObject "Script" $ \o -> do
        protocol <- o Aeson..:? "protocol" .!= 1
        case protocol of
            1 -> pure ()
            2 -> pure ()
            _ -> fail ("unsupported protocol " <> Prelude.show protocol <> ", allowed: 1, 2")
        let v2 :: (Aeson.FromJSON a) => Aeson.Key -> Aeson.Parser (Maybe a)
            v2 k = if protocol == (2 :: Int) then o Aeson..:? k else pure Nothing
        ScriptInfo
            <$> o Aeson..: "args"
            <*> o Aeson..: "slug"
            <*> o Aeson..: "description"
            <*> o Aeson..:? "empty-result"
            <*> o Aeson..:? "output-media-type"
            <*> o Aeson..:? "commands" .!= []
            <*> o Aeson..:? "subcommand"
            <*> pure protocol
            <*> v2 "process"
            <*> v2 "runtime"
            <*> (maybe False id <$> v2 "check")

{- | The process declaration of a tool: 'Nothing' for a v1 tool; the
declared one, or the default (@oneshot@, @jsonl@), for a v2 tool.
-}
scriptProcessOf :: ScriptInfo -> Maybe ScriptProcess
scriptProcessOf info
    | info.scriptProtocol >= 2 = Just (maybe defaultScriptProcess id info.scriptProcess)
    | otherwise = Nothing

{- | Checks the v2 declarations of a description against what this runtime
implements (slice 1 of @todos/bash-tools-v2.md@: @oneshot@ with framing and
validation). Anything that would change how the tool runs if honoured, and
is not implemented yet, is refused here rather than silently ignored.
Returns the list of problems (empty when valid, and always for a v1 tool).
-}
validateScriptV2 :: ScriptInfo -> [String]
validateScriptV2 info =
    case scriptProcessOf info of
        Nothing -> []
        Just p ->
            mconcat
                [ [ "process.mode " <> modeName p.processMode <> " is not implemented yet (only \"oneshot\" is)"
                  | p.processMode /= ModeOneshot
                  ]
                , [ "process.input is not implemented yet (it needs the stream or service modes)"
                  | Just _ <- [p.processInput]
                  ]
                , [ "process.ready " <> Prelude.show r <> " does not apply to a oneshot tool (only {\"kind\": \"exit\"} does)"
                  | p.processMode == ModeOneshot
                  , Just r <- [p.processReady]
                  , r /= ReadyExit
                  ]
                , [ "runtime is not implemented yet (run_as, sandbox, envdir, environment, rundir, timeout, on_exit, cancel)"
                  | Just _ <- [info.scriptRuntime]
                  ]
                , [ "output-media-type requires process.output.framing \"chunks\" (binary output cannot be cut into events)"
                  | Just _ <- [info.scriptOutputMediaType] <> fmap (.commandOutputMediaType) info.scriptCommands
                  , p.processOutput.outputFraming /= FramingChunks
                  ]
                , [ "process.output.event must be a JSON Schema (an object or a boolean)"
                  | Just ev <- [p.processOutput.outputEvent]
                  , not (isSchema ev)
                  ]
                ]
  where
    modeName m = case m of
        ModeOneshot -> "\"oneshot\""
        ModeStream -> "\"stream\""
        ModeService -> "\"service\""
    isSchema v = case v of
        Aeson.Object _ -> True
        Aeson.Bool _ -> True
        _ -> False

-------------------------------------------------------------------------------
-- Subcommands
-------------------------------------------------------------------------------

{- | One subcommand of a script, as listed in the @commands@ field of
@describe@. Its 'commandArgs' come after the script's shared top-level args.
'commandEmptyResultBehavior' and 'commandOutputMediaType' override the
top-level ones when set.
-}
data ScriptCommand
    = ScriptCommand
    { commandName :: Text
    , commandDescription :: Text
    , commandArgs :: [ScriptArg]
    , commandEmptyResultBehavior :: Maybe ScriptEmptyResultBehavior
    , commandOutputMediaType :: Maybe Text
    }
    deriving (Show, Eq, Ord)

instance ToJSON ScriptCommand where
    toJSON c =
        Aeson.object $
            [ "name" .= c.commandName
            , "description" .= c.commandDescription
            , "args" .= c.commandArgs
            ]
                <> maybe [] (\seb -> ["empty-result" .= seb]) c.commandEmptyResultBehavior
                <> maybe [] (\mime -> ["output-media-type" .= mime]) c.commandOutputMediaType

instance Aeson.FromJSON ScriptCommand where
    parseJSON = Aeson.withObject "ScriptCommand" $ \o ->
        ScriptCommand
            <$> o Aeson..: "name"
            <*> o Aeson..: "description"
            <*> o Aeson..:? "args" .!= []
            <*> o Aeson..:? "empty-result"
            <*> o Aeson..:? "output-media-type"

{- | Checks the subcommands of a description: names are non-empty, made of
ASCII letters, digits, @_@ and @-@ (they end up in a tool name), unique, and
no subcommand redeclares a shared top-level argument or one of its own.
Returns the list of problems (empty when valid).
-}
validateScriptCommands :: ScriptInfo -> [String]
validateScriptCommands info =
    mconcat
        [ [ "subcommand name " <> Prelude.show c.commandName <> " must be non-empty and use only ASCII letters, digits, '_' or '-'"
          | c <- info.scriptCommands
          , not (validName c.commandName)
          ]
        , [ "subcommand " <> Prelude.show n <> " is declared more than once"
          | n <- duplicates (fmap (.commandName) info.scriptCommands)
          ]
        , [ "subcommand " <> Prelude.show c.commandName <> " declares argument " <> Prelude.show n <> " more than once (shared top-level args count)"
          | c <- info.scriptCommands
          , n <- duplicates (fmap (.argName) (info.scriptArgs <> c.commandArgs))
          ]
        ]
  where
    validName n = not (Text.null n) && Text.all validChar n
    validChar ch = isAsciiLower ch || isAsciiUpper ch || isDigit ch || ch == '_' || ch == '-'
    duplicates xs = [x | (x : _ : _) <- List.group (List.sort xs)]

{- | Turns a description into the tools it exposes. A description without
subcommands is one tool, unchanged. A description with subcommands is one
tool per subcommand: slug @<slug>_<name>@, the top-level args followed by the
subcommand's own, 'scriptSubcommand' set so that @run@ receives the name.
-}
expandScriptCommands :: ScriptDescription -> [ScriptDescription]
expandScriptCommands desc
    | List.null info.scriptCommands = [desc]
    | otherwise = fmap expand1 info.scriptCommands
  where
    info = desc.scriptInfo
    expand1 :: ScriptCommand -> ScriptDescription
    expand1 cmd =
        desc
            { scriptInfo =
                ScriptInfo
                    { scriptArgs = info.scriptArgs <> cmd.commandArgs
                    , scriptSlug = info.scriptSlug <> "_" <> cmd.commandName
                    , scriptDescription = commandText cmd
                    , scriptEmptyResultBehavior = maybe info.scriptEmptyResultBehavior Just cmd.commandEmptyResultBehavior
                    , scriptOutputMediaType = maybe info.scriptOutputMediaType Just cmd.commandOutputMediaType
                    , scriptCommands = []
                    , scriptSubcommand = Just cmd.commandName
                    , scriptProtocol = info.scriptProtocol
                    , scriptProcess = info.scriptProcess
                    , scriptRuntime = info.scriptRuntime
                    , scriptCheck = info.scriptCheck
                    }
            }
    commandText :: ScriptCommand -> Text
    commandText cmd
        | Text.null info.scriptDescription = cmd.commandDescription
        | otherwise =
            cmd.commandDescription
                <> "\n\n(subcommand '"
                <> cmd.commandName
                <> "' of "
                <> info.scriptSlug
                <> ": "
                <> info.scriptDescription
                <> ")"

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

{- | Environment variable name for an 'Env'-mode argument: the argument name,
upper-cased, with @-@ turned into @_@.
-}
argEnvVarName :: Text -> Text
argEnvVarName = Text.map dash . Text.toUpper
  where
    dash '-' = '_'
    dash c = c

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
