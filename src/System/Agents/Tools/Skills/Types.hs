{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Core types for the Skills.md system per agentskills.io specification.

This module defines the fundamental types for loading, validating, and
managing skills in the agent system. Skills provide procedural knowledge
and executable capabilities via progressive disclosure.
-}
module System.Agents.Tools.Skills.Types (
    -- * Skill Names and Validation
    SkillName (..),
    validateSkillName,
    skillNameToText,

    -- * Script and Reference Types
    ScriptName (..),
    ScriptInfo (..),
    ScriptArgInfo (..),
    ReferenceInfo (..),

    -- * Skill Metadata and Structure
    SkillMetadata (..),
    Skill (..),

    -- * Skill Sources
    SkillSource (..),
    GitUrl,
    Subdirectory,

    -- * Skill Store
    SkillsStore (..),
    emptySkillsStore,
    lookupSkill,
    allSkills,
    insertSkill,

    -- * Skill Script State
    ScriptState (..),
    SkillScriptsState,
    SkillsSessionState (..),
) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- Skill Name Validation
-------------------------------------------------------------------------------

{- | A validated skill name per agentskills.io spec.

Names must be:
- 1-64 characters
- Lowercase a-z, digits 0-9, hyphens only
- No leading/trailing hyphens
- No consecutive hyphens
-}
newtype SkillName = SkillName {unSkillName :: Text}
    deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

-- | Parse SkillName from a JSON/YAML string, validating the format.
instance FromJSON SkillName where
    parseJSON = Aeson.withText "SkillName" $ \txt ->
        case validateSkillName txt of
            Left err -> fail $ Text.unpack err
            Right name -> return name

-- | Serialize SkillName to a JSON string.
instance ToJSON SkillName where
    toJSON = Aeson.toJSON . unSkillName

-- | Convert a SkillName back to Text.
skillNameToText :: SkillName -> Text
skillNameToText = unSkillName

{- | Validate a skill name per agentskills.io specification.

Rules:
1. Length: 1-64 characters
2. Characters: lowercase a-z, digits 0-9, hyphens only
3. No leading or trailing hyphens
4. No consecutive hyphens
-}
validateSkillName :: Text -> Either Text SkillName
validateSkillName txt
    | Text.null txt = Left "Skill name cannot be empty"
    | Text.length txt > 64 = Left "Skill name must be 64 characters or less"
    | not (Text.all isValidChar txt) =
        Left "Skill name can only contain lowercase a-z, digits 0-9, and hyphens"
    | Text.isPrefixOf "-" txt = Left "Skill name cannot start with a hyphen"
    | Text.isSuffixOf "-" txt = Left "Skill name cannot end with a hyphen"
    | "--" `Text.isInfixOf` txt = Left "Skill name cannot contain consecutive hyphens"
    | otherwise = Right $ SkillName txt
  where
    isValidChar c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-'

-------------------------------------------------------------------------------
-- Script Types
-------------------------------------------------------------------------------

-- | Name of a script within a skill.
newtype ScriptName = ScriptName {unScriptName :: Text}
    deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

-- | Parse ScriptName from a JSON/YAML string.
instance FromJSON ScriptName where
    parseJSON = Aeson.withText "ScriptName" $ \txt ->
        return $ ScriptName txt

-- | Serialize ScriptName to a JSON string.
instance ToJSON ScriptName where
    toJSON = Aeson.toJSON . unScriptName

{- | Information about a script in the skill's scripts/ directory.

Scripts follow the describe/run protocol:
- Called with "describe" argument, they output JSON metadata
- Called with actual arguments, they execute the operation
-}
data ScriptInfo = ScriptInfo
    { siName :: ScriptName
    -- ^ Name of the script (without extension)
    , siPath :: FilePath
    -- ^ Full path to the script file
    , siDescription :: Maybe Text
    -- ^ Description from script's describe output (loaded lazily)
    , siArgs :: [ScriptArgInfo]
    -- ^ Arguments from script's describe output
    }
    deriving (Show, Eq, Generic)

instance FromJSON ScriptInfo where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON ScriptInfo where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions

-- | Information about a script argument from describe output.
data ScriptArgInfo = ScriptArgInfo
    { saName :: Text
    , saDescription :: Text
    , saType :: Text
    , saRequired :: Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON ScriptArgInfo where
    parseJSON = Aeson.withObject "ScriptArgInfo" $ \o ->
        ScriptArgInfo
            <$> o Aeson..: "name"
            <*> o Aeson..: "description"
            <*> o Aeson..: "type"
            <*> o Aeson..:? "required" Aeson..!= True

instance ToJSON ScriptArgInfo where
    toJSON arg =
        Aeson.object
            [ "name" Aeson..= saName arg
            , "description" Aeson..= saDescription arg
            , "type" Aeson..= saType arg
            , "required" Aeson..= saRequired arg
            ]

-------------------------------------------------------------------------------
-- Reference Types
-------------------------------------------------------------------------------

{- | Information about a reference file in the skill's references/ directory.

References are additional documentation files (markdown, text, etc.) that
provide supporting information for the skill.
-}
data ReferenceInfo = ReferenceInfo
    { riName :: Text
    -- ^ Name of the reference file (with extension)
    , riPath :: FilePath
    -- ^ Full path to the reference file
    , riContent :: Maybe Text
    -- ^ Cached content of the reference (loaded lazily)
    }
    deriving (Show, Eq, Generic)

instance FromJSON ReferenceInfo where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON ReferenceInfo where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions

-------------------------------------------------------------------------------
-- Skill Metadata and Structure
-------------------------------------------------------------------------------

{- | SKILL.md frontmatter metadata per agentskills.io spec.

The frontmatter is YAML-formatted metadata at the start of the SKILL.md file,
separated from the markdown body by --- delimiters.
-}
data SkillMetadata = SkillMetadata
    { smName :: SkillName
    -- ^ Validated skill name (1-64 chars, lowercase, digits, hyphens)
    , smDescription :: Text
    -- ^ Description of what the skill does (max 1024 chars)
    , smLicense :: Maybe Text
    -- ^ Optional license identifier
    , smCompatibility :: Maybe Text
    -- ^ Optional compatibility notes
    , smMetadata :: Map Text Text
    -- ^ Additional custom metadata key-value pairs
    }
    deriving (Show, Eq, Generic)

instance FromJSON SkillMetadata where
    parseJSON = Aeson.withObject "SkillMetadata" $ \o -> do
        name <- o Aeson..: "name"
        desc <- o Aeson..: "description"
        let descValidated =
                if Text.length desc > 1024
                    then Text.take 1024 desc
                    else desc
        SkillMetadata
            <$> pure name
            <*> pure descValidated
            <*> o Aeson..:? "license"
            <*> o Aeson..:? "compatibility"
            <*> o Aeson..:? "metadata" Aeson..!= Map.empty

instance ToJSON SkillMetadata where
    toJSON meta =
        Aeson.object $
            [ "name" Aeson..= smName meta
            , "description" Aeson..= smDescription meta
            ]
                ++ ["license" Aeson..= lic | Just lic <- [smLicense meta]]
                ++ ["compatibility" Aeson..= comp | Just comp <- [smCompatibility meta]]
                ++ ["metadata" Aeson..= smMetadata meta | not (Map.null $ smMetadata meta)]

{- | A complete skill including metadata, instructions, scripts, and references.

Skills are loaded from a directory containing:
- SKILL.md: Markdown file with YAML frontmatter and instructions
- scripts/: Directory of executable scripts
- references/: Directory of reference documentation (optional)
-}
data Skill = Skill
    { skillMetadata :: SkillMetadata
    -- ^ Parsed frontmatter metadata
    , skillInstructions :: Text
    -- ^ Markdown body (after frontmatter)
    , skillPath :: FilePath
    -- ^ Path to the skill directory
    , skillScripts :: [ScriptInfo]
    -- ^ Scripts available in scripts/ subdirectory
    , skillReferences :: [ReferenceInfo]
    -- ^ Reference files in references/ subdirectory
    }
    deriving (Show, Eq, Generic)

instance FromJSON Skill where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON Skill where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions

-------------------------------------------------------------------------------
-- Skill Sources
-------------------------------------------------------------------------------

-- | URL for a git repository.
type GitUrl = Text

-- | Optional subdirectory within a git repository.
type Subdirectory = Text

{- | Source location for loading skills.

Skills can be loaded from:
- Local directories (recursively searched for SKILL.md files)
- Git repositories (cloned and then searched)
-}
data SkillSource
    = -- | Load skills from a local directory
      SkillDirectory FilePath
    | -- | Load skills from a git repository, optionally from a subdirectory
      SkillGitRepo GitUrl (Maybe Subdirectory)
    deriving (Show, Eq, Ord, Generic)

instance FromJSON SkillSource where
    parseJSON = Aeson.withObject "SkillSource" $ \o -> do
        tag <- o Aeson..: "tag"
        case (tag :: Text) of
            "SkillDirectory" ->
                SkillDirectory <$> o Aeson..: "contents"
            "SkillGitRepo" -> do
                contents <- o Aeson..: "contents"
                Aeson.withObject
                    "SkillGitRepoContents"
                    ( \c -> do
                        url <- c Aeson..: "url"
                        subdir <- c Aeson..:? "subdir"
                        return $ SkillGitRepo url subdir
                    )
                    contents
            _ -> fail $ "Unknown SkillSource tag: " ++ Text.unpack tag

instance ToJSON SkillSource where
    toJSON (SkillDirectory path) =
        Aeson.object
            [ "tag" Aeson..= ("SkillDirectory" :: Text)
            , "contents" Aeson..= path
            ]
    toJSON (SkillGitRepo url mSubdir) =
        Aeson.object
            [ "tag" Aeson..= ("SkillGitRepo" :: Text)
            , "contents"
                Aeson..= Aeson.object
                    (["url" Aeson..= url] ++ ["subdir" Aeson..= subdir | Just subdir <- [mSubdir]])
            ]

-------------------------------------------------------------------------------
-- Skills Store
-------------------------------------------------------------------------------

{- | In-memory store of loaded skills.

Provides O(log n) lookup by skill name.
-}
newtype SkillsStore = SkillsStore
    { getSkillsMap :: Map SkillName Skill
    }
    deriving (Show, Eq, Generic)

instance FromJSON SkillsStore where
    parseJSON = Aeson.withObject "SkillsStore" $ \o -> do
        -- Parse as a list of skills and build the map
        skills <- o Aeson..: "skills"
        return $ SkillsStore $ Map.fromList [(smName (skillMetadata s), s) | s <- skills]

instance ToJSON SkillsStore where
    toJSON store =
        Aeson.object
            [ "skills" Aeson..= Map.elems (getSkillsMap store)
            ]

instance Semigroup SkillsStore where
    (SkillsStore m1) <> (SkillsStore m2) = SkillsStore (Map.union m2 m1)

instance Monoid SkillsStore where
    mempty = SkillsStore Map.empty

-- | Empty skills store.
emptySkillsStore :: SkillsStore
emptySkillsStore = SkillsStore Map.empty

-- | Look up a skill by name.
lookupSkill :: SkillsStore -> SkillName -> Maybe Skill
lookupSkill (SkillsStore m) name = Map.lookup name m

-- | Get all skills in the store.
allSkills :: SkillsStore -> [Skill]
allSkills (SkillsStore m) = Map.elems m

-- | Insert a skill into the store.
insertSkill :: Skill -> SkillsStore -> SkillsStore
insertSkill skill (SkillsStore m) =
    SkillsStore $ Map.insert (skillMetadata skill).smName skill m

-------------------------------------------------------------------------------
-- Session State for Skills
-------------------------------------------------------------------------------

{- | Activation state for individual scripts within a skill.

Scripts start as 'Disabled' and become 'Enabled' when the skill is activated.
-}
data ScriptState = Enabled | Disabled
    deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | Map of script names to their activation state within a skill.
type SkillScriptsState = Map ScriptName ScriptState

{- | Session state tracking which skills and scripts are enabled.

This is a monoid that can be built by folding over session turns.
Later state overrides earlier state (last enable/disable wins).
-}
newtype SkillsSessionState = SkillsSessionState
    { sssActiveSkills :: Map SkillName SkillScriptsState
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Semigroup SkillsSessionState where
    (SkillsSessionState s1) <> (SkillsSessionState s2) =
        -- Union with later values overriding earlier ones
        -- For each skill, script states are also unioned with later overriding
        SkillsSessionState (Map.unionWith (Map.unionWith const) s1 s2)

instance Monoid SkillsSessionState where
    mempty = SkillsSessionState Map.empty

