{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Parser for SKILL.md files per agentskills.io specification.

This module handles parsing of SKILL.md files with YAML frontmatter
and markdown body. It also extracts script information from the
describe/run protocol.
-}
module System.Agents.Tools.Skills.Parser (
    Trace(..),
    parseSkillFile,
    parseSkillDirectory,
) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Either (rights)
import qualified Data.ByteString.Lazy as LByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import Prod.Tracer (Tracer, runTracer)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Process.ByteString (readProcessWithExitCode)

import System.Agents.Tools.Skills.Types
import System.Agents.Tools.ScriptTypes (
    ScriptDescription (..),
 )

data Trace
    = LoadCommandStart !FilePath [String]
    | LoadCommandStopped !FilePath [String] !ExitCode !ByteString !ByteString
    deriving (Show)

-------------------------------------------------------------------------------
-- SKILL.md Parsing
-------------------------------------------------------------------------------

{- | Parse a single SKILL.md file.

Returns the parsed Skill or an error message.
-}
parseSkillFile :: Tracer IO Trace -> FilePath -> IO (Either Text Skill)
parseSkillFile tracer skillMdPath = do
    exists <- doesFileExist skillMdPath
    if not exists
        then return $ Left $ "SKILL.md not found: " <> Text.pack skillMdPath
        else do
            content <- readFile skillMdPath
            let skillDir = takeDirectory skillMdPath
            parseSkillContent tracer skillDir (Text.pack content)

{- | Parse skill content from text.

The format is:
---
YAML frontmatter
---
Markdown body
-}
parseSkillContent :: Tracer IO Trace -> FilePath -> Text -> IO (Either Text Skill)
parseSkillContent tracer skillDir content = do
    case extractFrontmatter content of
        Left err -> return $ Left err
        Right (frontmatter, body) -> do
            case parseFrontmatter frontmatter of
                Left err -> return $ Left err
                Right metadata -> do
                    -- Load scripts (with describe output) and references
                    scripts <- loadScripts tracer skillDir
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

Each script is executed with "describe" to get its metadata (description and args),
similar to how BashTools loads script descriptions.
-}
loadScripts :: Tracer IO Trace -> FilePath -> IO [ScriptDescription]
loadScripts tracer skillDir = do
    let scriptsDir = skillDir </> "scripts"
    exists <- doesDirectoryExist scriptsDir
    if not exists
        then return []
        else do
            entries <- listDirectory scriptsDir
            let scriptPaths = map (scriptsDir </>) entries
            -- Filter to executable files
            execPaths <- filterM isExecutableFile scriptPaths
            -- Load script info and then load describe output for each
            scriptInfos <- mapM (loadScript tracer) execPaths
            -- Eagerly load script descriptions (like BashTools does)
            pure $ rights scriptInfos

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

data InvalidScriptError
    = InvalidScriptError FilePath ExitCode ByteString
    | InvalidDescriptionError FilePath String
    deriving (Show)

-- | Loads a single bash script complying with the describe|run protocol.
loadScript :: Tracer IO Trace -> FilePath -> IO (Either InvalidScriptError ScriptDescription)
loadScript tracer path = do
    let args = ["describe"]
    runTracer tracer (LoadCommandStart path args)
    (code, out, err) <- readProcessWithExitCode path args ""
    runTracer tracer (LoadCommandStopped path args code out err)
    if code /= ExitSuccess
        then pure $ Left $ InvalidScriptError path code err
        else case Aeson.eitherDecode (LByteString.fromStrict out) of
            Left jsonErr -> pure $ Left $ InvalidDescriptionError path jsonErr
            Right bashInfo -> pure $ Right $ ScriptDescription path bashInfo

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
parseSkillDirectory :: Tracer IO Trace -> FilePath -> IO (Either Text [Skill])
parseSkillDirectory tracer rootDir = do
    exists <- doesDirectoryExist rootDir
    if not exists
        then return $ Left $ "Directory not found: " <> Text.pack rootDir
        else do
            skillMdFiles <- findSkillMdFiles rootDir
            results <- mapM (parseSkillFile tracer) skillMdFiles
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

