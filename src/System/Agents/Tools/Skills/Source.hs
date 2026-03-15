{-# LANGUAGE OverloadedRecordDot #-}

{- | Skill source loading from directories and git repositories.

This module handles loading skills from various sources:
- Local directories (recursively searched for SKILL.md files)
- Git repositories (cloned and then searched)
-}
module System.Agents.Tools.Skills.Source (
    loadSkillsFromSource,
    loadSkillsFromSources,
    cloneGitRepo,
) where

import Control.Exception (bracket, try)
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

import System.Agents.Tools.Skills.Parser (parseSkillDirectory)
import System.Agents.Tools.Skills.Types

-------------------------------------------------------------------------------
-- Loading from Sources
-------------------------------------------------------------------------------

{- | Load skills from a single source.

For directories: recursively searches for SKILL.md files.
For git repos: clones to a temp directory, then searches.
-}
loadSkillsFromSource :: SkillSource -> IO (Either Text SkillsStore)
loadSkillsFromSource (SkillDirectory path) = do
    result <- parseSkillDirectory path
    case result of
        Left err -> return $ Left err
        Right skills -> return $ Right $ foldr insertSkill emptySkillsStore skills
loadSkillsFromSource (SkillGitRepo url mSubdir) = do
    -- Clone to temp directory
    cloneResult <- cloneGitRepo url
    case cloneResult of
        Left err -> return $ Left err
        Right tempDir -> do
            -- Parse from the cloned directory
            let parseDir = case mSubdir of
                    Nothing -> tempDir
                    Just subdir -> tempDir </> Text.unpack subdir
            result <- parseSkillDirectory parseDir
            -- Clean up temp directory
            removeDirectoryRecursive tempDir
            case result of
                Left err -> return $ Left err
                Right skills -> return $ Right $ foldr insertSkill emptySkillsStore skills

{- | Load skills from multiple sources.

Skills from later sources override skills from earlier sources if there
are name conflicts (last write wins).
-}
loadSkillsFromSources :: [SkillSource] -> IO (Either Text SkillsStore)
loadSkillsFromSources sources = do
    results <- mapM loadSkillsFromSource sources
    let (errors, stores) = partitionEithers results
    if not (null errors)
        then return $ Left $ Text.intercalate "; " errors
        else return $ Right $ mconcat stores

-------------------------------------------------------------------------------
-- Git Repository Handling
-------------------------------------------------------------------------------

{- | Clone a git repository to a temporary directory.

Returns the path to the cloned repository or an error message.
-}
cloneGitRepo :: GitUrl -> IO (Either Text FilePath)
cloneGitRepo url = do
    -- Create a temp directory for the clone
    withSystemTempDirectory "agents-skills-" $ \tempDir -> do
        let cloneDir = tempDir </> "repo"
        (exitCode, stdout, stderr) <-
            readProcessWithExitCode
                "git"
                ["clone", "--depth", "1", Text.unpack url, cloneDir]
                ""
        case exitCode of
            ExitSuccess -> return $ Right cloneDir
            ExitFailure code ->
                return $
                    Left $
                        "Git clone failed with exit code "
                            <> Text.pack (show code)
                            <> ": "
                            <> Text.pack stderr

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr go ([], [])
  where
    go (Left a) (as, bs) = (a : as, bs)
    go (Right b) (as, bs) = (as, b : bs)
