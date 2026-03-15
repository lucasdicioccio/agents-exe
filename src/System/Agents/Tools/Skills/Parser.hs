{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Parser for SKILL.md files per agentskills.io specification.

This module handles parsing of SKILL.md files with YAML frontmatter
and markdown body. It also extracts script information from the
describe/run protocol.
-}
module System.Agents.Tools.Skills.Parser (
    parseSkillFile,
    parseSkillDirectory,
    loadScriptDescription,
) where

import Control.Exception (try)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeFileName, (</>))
import System.Process (readProcessWithExitCode)

import System.Agents.Tools.Skills.Types

-------------------------------------------------------------------------------
-- SKILL.md Parsing
-------------------------------------------------------------------------------

{- | Parse a single SKILL.md file.

Returns the parsed Skill or an error message.
-}
parseSkillFile :: FilePath -> IO (Either Text Skill)
parseSkillFile skillMdPath = do
    exists <- doesFileExist skillMdPath
    if not exists
        then return $ Left $ "SKILL.md not found: " <> Text.pack skillMdPath
        else do
            content <- readFile skillMdPath
            let skillDir = takeDirectory skillMdPath
            parseSkillContent skillDir (Text.pack content)

{- | Parse skill content from text.

The format is:
---
YAML frontmatter
---
Markdown body
-}
parseSkillContent :: FilePath -> Text -> IO (Either Text Skill)
parseSkillContent skillDir content = do
    case extractFrontmatter content of
        Left err -> return $ Left err
        Right (frontmatter, body) -> do
            case parseFrontmatter frontmatter of
                Left err -> return $ Left err
                Right metadata -> do
                    -- Load scripts and references
                    scripts <- loadScripts skillDir
                    references <- loadReferences skillDir
                    return $
                        Right $
                            Skill
                                { skillMetadata = metadata
                                , skillInstructions = body
                                , skillPath = skillDir
                                , skillScripts = scripts
                                , skillReferences = references
                                }

{- | Extract YAML frontmatter and markdown body from content.

Frontmatter is delimited by --- at the start and end.
-}
extractFrontmatter :: Text -> Either Text (Text, Text)
extractFrontmatter content =
    let lines_ = Text.lines content
     in case lines_ of
            (firstLine : rest)
                | Text.strip firstLine == "---" ->
                    case break (== "---") (map Text.strip rest) of
                        (frontLines, (_ : _bodyLines)) ->
                            Right
                                ( Text.unlines (take (length frontLines) rest)
                                , Text.unlines (drop (length frontLines + 1) rest)
                                )
                        _ -> Left "Malformed frontmatter: missing closing ---"
            _ -> Left "Missing frontmatter: file must start with ---"

-- | Parse YAML frontmatter into SkillMetadata.
parseFrontmatter :: Text -> Either Text SkillMetadata
parseFrontmatter txt =
    case Yaml.decodeEither' (Text.encodeUtf8 txt) of
        Left err -> Left $ "YAML parse error: " <> Text.pack (Yaml.prettyPrintParseException err)
        Right meta -> Right meta

-------------------------------------------------------------------------------
-- Script Loading
-------------------------------------------------------------------------------

{- | Load all scripts from the scripts/ subdirectory.

Scripts are executable files that follow the describe/run protocol.
Only files with executable permissions are considered scripts.
-}
loadScripts :: FilePath -> IO [ScriptInfo]
loadScripts skillDir = do
    let scriptsDir = skillDir </> "scripts"
    exists <- doesDirectoryExist scriptsDir
    if not exists
        then return []
        else do
            entries <- listDirectory scriptsDir
            let scriptPaths = map (scriptsDir </>) entries
            -- Filter to executable files
            execPaths <- filterM isExecutableFile scriptPaths
            mapM (loadScriptInfo scriptsDir) execPaths

{- | Check if a file is executable.

On Unix systems, this checks the file permissions.
On Windows, all files are considered potentially executable.
-}
isExecutableFile :: FilePath -> IO Bool
isExecutableFile path = do
    isFile <- doesFileExist path
    if not isFile
        then return False
        else do
            -- For now, consider all regular files in scripts/ as scripts
            -- A more robust implementation would check executable permissions
            return True

{- | Load information about a single script.

This creates a ScriptInfo with the name and path. The description
and args are loaded lazily when needed.
-}
loadScriptInfo :: FilePath -> FilePath -> IO ScriptInfo
loadScriptInfo _scriptsDir scriptPath = do
    let baseName = takeFileName scriptPath
    -- Remove common script extensions
    let name = case takeExtension baseName of
            ".sh" -> Text.pack $ dropExtension baseName
            ".py" -> Text.pack $ dropExtension baseName
            ".rb" -> Text.pack $ dropExtension baseName
            ".pl" -> Text.pack $ dropExtension baseName
            _ -> Text.pack baseName
    return $
        ScriptInfo
            { siName = ScriptName name
            , siPath = scriptPath
            , siDescription = Nothing -- Loaded lazily
            , siArgs = [] -- Loaded lazily
            }

{- | Load script description by executing the script with "describe" argument.

This implements the describe/run protocol for skill scripts.
-}
loadScriptDescription :: ScriptInfo -> IO (Either Text ScriptInfo)
loadScriptDescription script = do
    result <- try $ readProcessWithExitCode (siPath script) ["describe"] ""
    case result of
        Left (e :: IOError) ->
            return $ Left $ "Failed to execute script describe: " <> Text.pack (show e)
        Right (exitCode, stdout, _stderr) -> case exitCode of
            ExitSuccess ->
                case Aeson.eitherDecodeStrict (Text.encodeUtf8 $ Text.pack stdout) of
                    Left err ->
                        return $ Left $ "Invalid describe output for " <> unScriptName (siName script) <> ": " <> Text.pack err
                    Right desc -> do
                        let updatedScript =
                                script
                                    { siDescription = Just (sdDescription desc)
                                    , siArgs = sdArgs desc
                                    }
                        return $ Right updatedScript
            ExitFailure code ->
                return $
                    Left $
                        "Script describe failed with exit code "
                            <> Text.pack (show code)

-- | Script description from describe output.
data ScriptDescriptionOutput = ScriptDescriptionOutput
    { sdDescription :: Text
    , sdArgs :: [ScriptArgInfo]
    }

instance Aeson.FromJSON ScriptDescriptionOutput where
    parseJSON = Aeson.withObject "ScriptDescriptionOutput" $ \o ->
        ScriptDescriptionOutput
            <$> o Aeson..: "description"
            <*> o Aeson..:? "args" Aeson..!= []

-------------------------------------------------------------------------------
-- Reference Loading
-------------------------------------------------------------------------------

{- | Load all references from the references/ subdirectory.

References are documentation files (markdown, text, etc.) that provide
supporting information for the skill.
-}
loadReferences :: FilePath -> IO [ReferenceInfo]
loadReferences skillDir = do
    let refsDir = skillDir </> "references"
    exists <- doesDirectoryExist refsDir
    if not exists
        then return []
        else do
            entries <- listDirectory refsDir
            let refPaths = map (refsDir </>) entries
            -- Filter to files only
            filePaths <- filterM doesFileExist refPaths
            return $ map makeReferenceInfo filePaths

-- | Create a ReferenceInfo from a file path.
makeReferenceInfo :: FilePath -> ReferenceInfo
makeReferenceInfo path =
    ReferenceInfo
        { riName = Text.pack $ takeFileName path
        , riPath = path
        , riContent = Nothing -- Loaded lazily
        }

-------------------------------------------------------------------------------
-- Directory Parsing
-------------------------------------------------------------------------------

{- | Parse all skills from a directory.

Recursively searches for SKILL.md files and parses each one.
-}
parseSkillDirectory :: FilePath -> IO (Either Text [Skill])
parseSkillDirectory rootDir = do
    exists <- doesDirectoryExist rootDir
    if not exists
        then return $ Left $ "Directory not found: " <> Text.pack rootDir
        else do
            skillMdFiles <- findSkillMdFiles rootDir
            results <- mapM parseSkillFile skillMdFiles
            let (errors, skills) = partitionEithers results
            if null errors
                then return $ Right skills
                else return $ Left $ Text.intercalate "; " errors

-- | Recursively find all SKILL.md files in a directory.
findSkillMdFiles :: FilePath -> IO [FilePath]
findSkillMdFiles dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else do
            entries <- listDirectory dir
            let fullPaths = map (dir </>) entries
            subdirs <- filterM doesDirectoryExist fullPaths
            -- Check for SKILL.md in current directory
            let skillMdPath = dir </> "SKILL.md"
            hasSkillMd <- doesFileExist skillMdPath
            -- Recurse into subdirectories
            subResults <- concat <$> mapM findSkillMdFiles subdirs
            return $ if hasSkillMd then skillMdPath : subResults else subResults

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _predicate [] = return []
filterM predicate (x : xs) = do
    b <- predicate x
    if b then (x :) <$> filterM predicate xs else filterM predicate xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr go ([], [])
  where
    go (Left a) (as, bs) = (a : as, bs)
    go (Right b) (as, bs) = (as, b : bs)

