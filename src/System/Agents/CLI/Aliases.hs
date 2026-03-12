{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Module for prompt alias functionality.

Aliases allow users to define reusable prompt templates with variable
substitution. This module handles alias definitions, lookup, and template
resolution.
-}
module System.Agents.CLI.Aliases (
    -- * Types
    AliasInputMode (..),
    AliasDefinition (..),

    -- * Default aliases
    defaultAliases,

    -- * Alias resolution
    resolveAliases,
    lookupAlias,
    formatAliasNotFoundError,

    -- * Template processing
    substituteTemplate,
    detectLanguage,
) where

import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import System.FilePath (takeExtension, takeFileName)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Input mode for aliases: read from stdin or from a file
data AliasInputMode = AliasStdin | AliasFile
    deriving (Show, Eq, Generic)

instance Aeson.FromJSON AliasInputMode where
    parseJSON = Aeson.withText "AliasInputMode" $ \t -> case Text.toLower t of
        "stdin" -> pure AliasStdin
        "file" -> pure AliasFile
        _ -> fail $ "Unknown input mode: " ++ Text.unpack t

-- | Definition of a prompt alias with template and metadata
data AliasDefinition = AliasDefinition
    { aliasDescription :: Text
    , aliasTemplate :: Text
    , aliasInputMode :: AliasInputMode
    }
    deriving (Show, Generic)

instance Aeson.FromJSON AliasDefinition where
    parseJSON = Aeson.withObject "AliasDefinition" $ \v ->
        AliasDefinition
            <$> v Aeson..: "description"
            <*> v Aeson..: "template"
            <*> v Aeson..: "inputMode"

-------------------------------------------------------------------------------
-- Default Aliases
-------------------------------------------------------------------------------

-- | Default aliases available even without configuration
defaultAliases :: Map Text AliasDefinition
defaultAliases =
    Map.fromList
        [
            ( "translate"
            , AliasDefinition
                "Translate to English"
                "Please translate the following text to English:\n\n{{content}}"
                AliasStdin
            )
        ,
            ( "summarize"
            , AliasDefinition
                "Summarize text"
                "Please provide a concise summary of the following:\n\n{{content}}"
                AliasStdin
            )
        ,
            ( "code-review"
            , AliasDefinition
                "Review code for issues"
                "Please review the following code for potential bugs, security issues, and style improvements:\n\n```{{language}}\n{{content}}\n```"
                AliasFile
            )
        ,
            ( "explain"
            , AliasDefinition
                "Explain code in plain English"
                "Please explain what the following code does in plain English:\n\n```{{language}}\n{{content}}\n```"
                AliasFile
            )
        ]

-------------------------------------------------------------------------------
-- Alias Resolution
-------------------------------------------------------------------------------

-- | Resolve aliases from config, falling back to defaults
resolveAliases :: Maybe (Map Text AliasDefinition) -> Map Text AliasDefinition
resolveAliases Nothing = defaultAliases
resolveAliases (Just cfgAliases) = Map.union cfgAliases defaultAliases

-- | Get alias definition or return error message
lookupAlias :: Map Text AliasDefinition -> Text -> Either Text AliasDefinition
lookupAlias aliases name = case Map.lookup name aliases of
    Just def -> Right def
    Nothing -> Left $ formatAliasNotFoundError name aliases

-- | Format error message for unknown alias with available aliases list
formatAliasNotFoundError :: Text -> Map Text AliasDefinition -> Text
formatAliasNotFoundError name aliases =
    Text.unlines $
        [ "Error: Unknown alias '" <> name <> "'"
        , ""
        , "Available aliases:"
        ]
            ++ map formatAlias (Map.toList aliases)
  where
    formatAlias (aliasName, def) = "  - " <> aliasName <> ": " <> aliasDescription def

-------------------------------------------------------------------------------
-- Template Processing
-------------------------------------------------------------------------------

{- | Substitute template variables in an alias template
Supported variables:
  {{content}}  - The actual content from stdin or file
  {{language}} - Auto-detected or specified language
  {{filename}} - Name of the input file
-}
substituteTemplate :: Text -> Text -> Maybe FilePath -> Text
substituteTemplate template content mFilePath =
    let withContent = Text.replace "{{content}}" content template
        withFilename = case mFilePath of
            Just fp -> Text.replace "{{filename}}" (Text.pack $ takeFileName fp) withContent
            Nothing -> Text.replace "{{filename}}" "" withContent
        language = detectLanguage mFilePath
     in Text.replace "{{language}}" language withFilename

-- | Detect programming language from file extension
detectLanguage :: Maybe FilePath -> Text
detectLanguage Nothing = ""
detectLanguage (Just fp) = case takeExtension fp of
    ".hs" -> "haskell"
    ".py" -> "python"
    ".js" -> "javascript"
    ".ts" -> "typescript"
    ".java" -> "java"
    ".c" -> "c"
    ".cpp" -> "cpp"
    ".cc" -> "cpp"
    ".h" -> "c"
    ".hpp" -> "cpp"
    ".rs" -> "rust"
    ".go" -> "go"
    ".rb" -> "ruby"
    ".php" -> "php"
    ".sh" -> "bash"
    ".bash" -> "bash"
    ".zsh" -> "zsh"
    ".pl" -> "perl"
    ".r" -> "r"
    ".swift" -> "swift"
    ".kt" -> "kotlin"
    ".scala" -> "scala"
    ".clj" -> "clojure"
    ".ex" -> "elixir"
    ".exs" -> "elixir"
    ".erl" -> "erlang"
    ".ml" -> "ocaml"
    ".fs" -> "fsharp"
    ".cs" -> "csharp"
    ".lua" -> "lua"
    ".vim" -> "vim"
    ".md" -> "markdown"
    ".json" -> "json"
    ".yaml" -> "yaml"
    ".yml" -> "yaml"
    ".xml" -> "xml"
    ".html" -> "html"
    ".css" -> "css"
    ".scss" -> "scss"
    ".sass" -> "sass"
    ".sql" -> "sql"
    _ -> ""
