module Main where

import Agq.Commands
import Agq.Config
import Agq.DB (initDB)

import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (Connection, withConnection)
import Options.Applicative
import System.IO (hSetBuffering, stdout, stderr, BufferMode(..))

-- ---------------------------------------------------------------------------
-- Command ADT
-- ---------------------------------------------------------------------------

data Command
  = Init
  | Add Text Text [Text] [Text] Int  -- label name deps tags tries
  | Pull
  | Promote
  | Status
  | Process Bool                 -- parallel?
  | Exec Text                    -- task name
  | MergePRs
  | Clean Bool Bool              -- do-it force
  | Recover
  | Retry Text Int               -- task name, tries to restore

-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

parseCfgPath :: Parser FilePath
parseCfgPath = strOption
  ( long "config"
  <> short 'c'
  <> metavar "FILE"
  <> value "./agq.json"
  <> showDefault
  <> help "Path to agq.json config file"
  )

textArg :: String -> String -> Parser Text
textArg mvar h = fmap Text.pack $ argument str (metavar mvar <> help h)

textOption :: String -> String -> Parser Text
textOption lng h = fmap Text.pack $ strOption (long lng <> metavar "TEXT" <> help h)

textList :: String -> String -> Parser [Text]
textList lng h = many $ fmap Text.pack $ strOption (long lng <> metavar "TEXT" <> help h)

parseCommand :: Parser Command
parseCommand = hsubparser
  ( command "init"
      (info (pure Init)
        (progDesc "Initialise the queue database and directories"))
  <> command "add"
      (info parseAdd
        (progDesc "Add a task to the queue"))
  <> command "pull"
      (info (pure Pull)
        (progDesc "Pull tasks from GitHub (agents/to-be-taken issues)"))
  <> command "promote"
      (info (pure Promote)
        (progDesc "Promote agents/wait issues whose deps are satisfied"))
  <> command "status"
      (info (pure Status)
        (progDesc "Show queue status"))
  <> command "process"
      (info parseProcess
        (progDesc "Process tasks from the queue"))
  <> command "exec"
      (info parseExec
        (progDesc "Execute a specific task by name (internal)"))
  <> command "merge-prs"
      (info (pure MergePRs)
        (progDesc "Merge PRs with label agents/agent-pr"))
  <> command "clean"
      (info parseClean
        (progDesc "Remove worktrees with completed sessions"))
  <> command "recover"
      (info (pure Recover)
        (progDesc "Recover stale locks and reset orphaned tasks"))
  <> command "retry"
      (info parseRetry
        (progDesc "Reset a failed task back to pending"))
  )

parseAdd :: Parser Command
parseAdd = Add
  <$> textArg "LABEL" "Project label (must match a key in projects config)"
  <*> textArg "NAME"  "Unique task name (used as branch name)"
  <*> textList "dep" "Dependency task name (repeatable)"
  <*> textList "tag" "Extra tag (repeatable)"
  <*> option auto (long "tries" <> metavar "N" <> value 0
        <> help "Number of allowed execution attempts (0 = use defaultTries from config)")

parseProcess :: Parser Command
parseProcess = Process
  <$> switch (long "parallel" <> help "Fork tasks in parallel")

parseExec :: Parser Command
parseExec = Exec <$> textArg "NAME" "Task name to execute"

parseRetry :: Parser Command
parseRetry = Retry
  <$> textArg "NAME" "Task name to retry"
  <*> option auto (long "tries" <> metavar "N" <> value 0
        <> help "tries_remaining to restore (0 = use defaultTries from config)")

parseClean :: Parser Command
parseClean = Clean
  <$> switch (long "do-it" <> help "Actually remove worktrees (default: preview)")
  <*> switch (long "force" <> help "Force-remove worktrees with uncommitted changes")

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  (cfgPath, cmd) <- execParser $ info
    (liftA2 (,) parseCfgPath parseCommand <**> helper)
    (fullDesc <> progDesc "agq — Agent Queue scheduler")
  cfg <- loadConfig cfgPath
  withConnection (queueDb cfg) $ \conn -> do
    initDB conn
    dispatch cfg conn cmd

dispatch :: AgqConfig -> Connection -> Command -> IO ()
dispatch cfg conn cmd = case cmd of
  Init          -> cmdInit    cfg conn
  Add l n d t r -> cmdAdd     cfg conn l n d t (if r <= 0 then defaultTries cfg else r)
  Pull          -> cmdPull    cfg conn
  Promote       -> cmdPromote cfg
  Status        -> cmdStatus  cfg conn
  Process p     -> cmdProcess cfg conn p
  Exec n        -> cmdExec    cfg conn n
  MergePRs      -> cmdMergePRs cfg
  Clean d f     -> cmdClean   cfg d f
  Recover       -> cmdRecover cfg conn
  Retry n r     -> cmdRetry   cfg conn n r
