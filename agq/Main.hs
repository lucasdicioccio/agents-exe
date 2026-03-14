module Main where

import Agq.Commands
import Agq.Config
import Agq.DB (initDB)

import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, execute_, withConnection)
import Options.Applicative
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

-- ---------------------------------------------------------------------------
-- Command ADT
-- ---------------------------------------------------------------------------

data Command
    = Init -- bootstrap: write agq.json template
    | InitQueue -- create dirs + SQLite DB
    | InitGithub -- create GitHub labels
    | Add Text Text [Text] [Text] Int -- label name deps tags tries
    | Pull
    | Promote
    | Status Bool -- trim-done?
    | Next -- show which tasks are ready and why others are blocked
    | Process Bool Bool -- parallel? loop?
    | Exec Text -- task name
    | MergePRs
    | Clean Bool Bool -- do-it force
    | Recover
    | Retry Text Int -- task name, tries to restore
    | MarkDone Text -- task name

-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

parseCfgPath :: Parser FilePath
parseCfgPath =
    strOption
        ( long "config"
            <> short 'c'
            <> metavar "FILE"
            <> value "./agq.json"
            <> showDefault
            <> help "Path to agq.json config file"
        )

textArg :: String -> String -> Parser Text
textArg mvar h = fmap Text.pack $ argument str (metavar mvar <> help h)

textList :: String -> String -> Parser [Text]
textList lng h = many $ fmap Text.pack $ strOption (long lng <> metavar "TEXT" <> help h)

parseCommand :: Parser Command
parseCommand =
    hsubparser
        ( command
            "init"
            ( info
                (pure Init)
                (progDesc "Write a template agq.json (no config needed)")
            )
            <> command
                "init-queue"
                ( info
                    (pure InitQueue)
                    (progDesc "Initialise the queue database and directories")
                )
            <> command
                "init-github"
                ( info
                    (pure InitGithub)
                    (progDesc "Create or update GitHub labels defined in the config")
                )
            <> command
                "add"
                ( info
                    parseAdd
                    (progDesc "Add a task to the queue")
                )
            <> command
                "pull"
                ( info
                    (pure Pull)
                    (progDesc "Pull tasks from GitHub (agq/to-be-taken issues)")
                )
            <> command
                "promote"
                ( info
                    (pure Promote)
                    (progDesc "Promote agq/wait issues whose deps are satisfied")
                )
            <> command
                "status"
                ( info
                    parseStatus
                    (progDesc "Show queue status")
                )
            <> command
                "next"
                ( info
                    (pure Next)
                    (progDesc "Show tasks ready to be taken and why others are blocked")
                )
            <> command
                "process"
                ( info
                    parseProcess
                    (progDesc "Process tasks from the queue")
                )
            <> command
                "exec"
                ( info
                    parseExec
                    (progDesc "Execute a specific task by name (internal)")
                )
            <> command
                "merge-prs"
                ( info
                    (pure MergePRs)
                    (progDesc "Merge PRs with label agq/agent-pr")
                )
            <> command
                "clean"
                ( info
                    parseClean
                    (progDesc "Remove worktrees with completed sessions")
                )
            <> command
                "recover"
                ( info
                    (pure Recover)
                    (progDesc "Recover stale locks and reset orphaned tasks")
                )
            <> command
                "retry"
                ( info
                    parseRetry
                    (progDesc "Reset a failed task back to pending")
                )
            <> command
                "mark-done"
                ( info
                    parseMarkDone
                    (progDesc "Mark a task as done and release its lock")
                )
        )

parseAdd :: Parser Command
parseAdd =
    Add
        <$> textArg "LABEL" "Project label (must match a key in projects config)"
        <*> textArg "NAME" "Unique task name (used as branch name)"
        <*> textList "dep" "Dependency task name (repeatable)"
        <*> textList "tag" "Extra tag (repeatable)"
        <*> option
            auto
            ( long "tries"
                <> metavar "N"
                <> value 0
                <> help "Number of allowed execution attempts (0 = use defaultTries from config)"
            )

parseStatus :: Parser Command
parseStatus =
    Status
        <$> flag
            True
            False
            ( long "no-trim-done"
                <> help "Show all done tasks (default: hide old done tasks, keep only 3 latest)"
            )

parseProcess :: Parser Command
parseProcess =
    Process
        <$> switch (long "parallel" <> help "Fork tasks in parallel")
        <*> switch (long "loop" <> help "Run forever, polling for new tasks when the queue is empty")

parseExec :: Parser Command
parseExec = Exec <$> textArg "NAME" "Task name to execute"

parseRetry :: Parser Command
parseRetry =
    Retry
        <$> textArg "NAME" "Task name to retry"
        <*> option
            auto
            ( long "tries"
                <> metavar "N"
                <> value 0
                <> help "tries_remaining to restore (0 = use defaultTries from config)"
            )

parseMarkDone :: Parser Command
parseMarkDone = MarkDone <$> textArg "NAME" "Task name to mark as done"

parseClean :: Parser Command
parseClean =
    Clean
        <$> switch (long "do-it" <> help "Actually remove worktrees (default: preview)")
        <*> switch (long "force" <> help "Force-remove worktrees with uncommitted changes")

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    (cfgPath, cmd) <-
        execParser $
            info
                (liftA2 (,) parseCfgPath parseCommand <**> helper)
                (fullDesc <> progDesc "agq — Agent Queue scheduler")
    -- 'init' needs no config file or DB connection
    case cmd of
        Init -> cmdInit cfgPath
        _ -> do
            cfg <- loadConfig cfgPath
            withConnection (queueDb cfg) $ \conn -> do
                execute_ conn "PRAGMA busy_timeout = 5000"
                initDB conn
                dispatch cfg conn cmd

dispatch :: AgqConfig -> Connection -> Command -> IO ()
dispatch cfg conn cmd = case cmd of
    Init -> return () -- handled in main before reaching here
    InitQueue -> cmdInitQueue cfg conn
    InitGithub -> cmdInitGithub cfg
    Add l n d t r -> cmdAdd cfg conn l n d t (if r <= 0 then defaultTries cfg else r)
    Pull -> cmdPull cfg conn
    Promote -> cmdPromote cfg
    Status trimDone -> cmdStatus cfg conn trimDone
    Next -> cmdNext cfg conn
    Process p l -> cmdProcess cfg conn p l
    Exec n -> cmdExec cfg conn n
    MergePRs -> cmdMergePRs cfg
    Clean d f -> cmdClean cfg d f
    Recover -> cmdRecover cfg conn
    Retry n r -> cmdRetry cfg conn n r
    MarkDone n -> cmdMarkDone cfg conn n
