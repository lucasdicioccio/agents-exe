{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Agents.CLI.CleanWorktrees
    ( CleanWorktreesOptions (..)
    , parseCleanWorktreesOptions
    , handleCleanWorktrees
    ) where

import Control.Monad (forM_, when, unless)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import qualified System.Process as Process

-- | Options for the clean-worktrees command
data CleanWorktreesOptions = CleanWorktreesOptions
    { doIt :: Bool
    -- ^ Actually perform the deletion (default is dry-run/preview)
    , force :: Bool
    -- ^ Force remove worktrees
    , sessionsDir :: FilePath
    -- ^ Directory containing session files (default: "tasks-sessions")
    }
    deriving (Show, Generic)

-- | Parse command line options for clean-worktrees
parseCleanWorktreesOptions :: Parser CleanWorktreesOptions
parseCleanWorktreesOptions =
    CleanWorktreesOptions
        <$> switch
            ( long "do-it"
                <> help "Actually perform the deletion (default is preview mode)"
            )
        <*> switch
            ( long "force"
                <> help "Force remove worktrees (use git worktree remove --force)"
            )
        <*> strOption
            ( long "sessions-dir"
                <> metavar "DIR"
                <> help "Directory containing session files"
                <> showDefault
                <> value "tasks-sessions"
            )

-- | Represents a git worktree
data Worktree = Worktree
    { wtPath :: FilePath
    -- ^ Full path to the worktree
    , wtBranch :: Maybe String
    -- ^ Branch name if detached HEAD
    , wtDetached :: Bool
    -- ^ Whether the worktree is in detached HEAD state
    }
    deriving (Show)

-- Prefix for branch refs in git worktree list output
branchPrefix :: String
branchPrefix = "branch refs/heads/"

-- Prefix for worktree path in git worktree list output
worktreePrefix :: String
worktreePrefix = "worktree "

-- | Parse a single line from git worktree list --porcelain output
parseWorktreeLine :: String -> [String] -> Maybe Worktree
parseWorktreeLine path rest =
    let branch = findBranch rest
        isDetached = any ("HEAD" ==) rest
     in Just $ Worktree path branch isDetached
  where
    findBranch :: [String] -> Maybe String
    findBranch [] = Nothing
    findBranch (x : xs)
        | branchPrefix `isPrefixOf` x =
            Just $ drop (length branchPrefix) x
        | otherwise = findBranch xs

-- | Parse the full porcelain output from git worktree list
parseWorktrees :: String -> [Worktree]
parseWorktrees output =
    let lines' = lines output
        go :: [String] -> [Worktree] -> [Worktree]
        go [] acc = acc
        go (l : ls) acc
            | worktreePrefix `isPrefixOf` l =
                let path = drop (length worktreePrefix) l
                    (attrs, rest) = span (not . null) ls
                 in case parseWorktreeLine path attrs of
                        Just wt -> go rest (wt : acc)
                        Nothing -> go rest acc
            | otherwise = go ls acc
     in go lines' []

-- | Get the list of worktrees using git command
getWorktrees :: IO (Either String [Worktree])
getWorktrees = do
    (exitCode, stdout, procStderr) <-
        Process.readCreateProcessWithExitCode
            (Process.proc "git" ["worktree", "list", "--porcelain"])
            ""
    case exitCode of
        ExitSuccess -> pure $ Right (parseWorktrees stdout)
        ExitFailure code ->
            pure $
                Left $
                    "git worktree list failed with exit code "
                        <> show code
                        <> ": "
                        <> procStderr

-- | Check if a session file exists for a given worktree name
hasSessionFile :: FilePath -> FilePath -> IO Bool
hasSessionFile sessionsDir worktreeName = do
    let sessionFile = sessionsDir </> (worktreeName ++ ".session.md")
    doesFileExist sessionFile

-- | Get the worktree name from its path (last directory component)
getWorktreeName :: FilePath -> FilePath
getWorktreeName = takeFileName

-- | Remove a worktree using git worktree remove
gitRemoveWorktree :: Bool -> FilePath -> IO (Either String ())
gitRemoveWorktree forceFlag path = do
    let args =
            ["worktree", "remove"] ++ ["--force" | forceFlag] ++ [path]
    (exitCode, _, procStderr) <-
        Process.readCreateProcessWithExitCode
            (Process.proc "git" args)
            ""
    case exitCode of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ ->
            pure $ Left $ "git worktree remove failed: " <> procStderr

-- | Force remove a branch using git branch -D
gitForceRemoveBranch :: String -> IO (Either String ())
gitForceRemoveBranch branch = do
    (exitCode, _, procStderr) <-
        Process.readCreateProcessWithExitCode
            (Process.proc "git" ["branch", "-D", branch])
            ""
    case exitCode of
        ExitSuccess -> pure $ Right ()
        ExitFailure _ ->
            pure $ Left $ "git branch -D failed: " <> procStderr

-- | Main handler for the clean-worktrees command
handleCleanWorktrees :: CleanWorktreesOptions -> IO ()
handleCleanWorktrees opts = do
    worktreesResult <- getWorktrees
    case worktreesResult of
        Left err -> do
            hPutStrLn stderr $ "Error: " <> err
        Right worktrees -> do
            -- Filter worktrees that have session files (indicating successful merge)
            candidates <- filterM (hasAssociatedSession opts.sessionsDir) worktrees

            if null candidates
                then putStrLn "No worktrees to clean up."
                else do
                    putStrLn $
                        "Found "
                            <> show (length candidates)
                            <> " worktree(s) with associated session files:"
                    putStrLn ""

                    forM_ candidates $ \wt -> do
                        let name = getWorktreeName (wtPath wt)
                        putStrLn $ "  - " <> name
                        putStrLn $ "    path: " <> wtPath wt
                        putStrLn $
                            "    branch: "
                                <> maybe "(detached)" id (wtBranch wt)
                        putStrLn $ "    session: " <> opts.sessionsDir </> (name ++ ".session.md")
                        putStrLn ""

                    when opts.doIt $ do
                        putStrLn "Proceeding with cleanup..."
                        putStrLn ""
                        forM_ candidates $ \wt -> do
                            let name = getWorktreeName (wtPath wt)
                            putStrLn $ "Processing: " <> name

                            -- Remove the worktree
                            putStrLn $ "  Removing worktree: " <> wtPath wt
                            wtResult <- gitRemoveWorktree opts.force (wtPath wt)
                            case wtResult of
                                Left err -> do
                                    hPutStrLn stderr $ "    ERROR: " <> err
                                Right () -> do
                                    putStrLn "    Worktree removed successfully."

                                    -- Try to remove the branch if worktree was deleted
                                    case wtBranch wt of
                                        Just branch -> do
                                            putStrLn $ "  Removing branch: " <> branch
                                            branchResult <- gitForceRemoveBranch branch
                                            case branchResult of
                                                Left err -> do
                                                    hPutStrLn stderr $ "    WARNING: " <> err
                                                Right () -> do
                                                    putStrLn "    Branch removed successfully."
                                        Nothing -> do
                                            putStrLn "  Skipping branch removal (detached HEAD)."

                        putStrLn ""
                        putStrLn "Cleanup complete!"

                    unless opts.doIt $ do
                        putStrLn "Run with --do-it to actually perform the cleanup."
  where
    filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
    filterM _ [] = return []
    filterM p (x : xs) = do
        b <- p x
        ys <- filterM p xs
        return (if b then x : ys else ys)

    hasAssociatedSession :: FilePath -> Worktree -> IO Bool
    hasAssociatedSession sDir wt = do
        let name = getWorktreeName (wtPath wt)
        hasSessionFile sDir name

