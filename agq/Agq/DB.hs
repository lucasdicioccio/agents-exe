module Agq.DB (
    TaskStatus (..),
    TaskSource (..),
    Task (..),
    taskStatusText,
    parseTaskStatus,
    taskSourceText,
    parseTaskSource,
    initDB,
    insertTask,
    getTaskByName,
    listTasks,
    listTasksWithDetails,
    updateTaskStatus,
    decrementTries,
    retryTask,
    countPendingRunning,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple

-- | Task execution status
data TaskStatus = Pending | Running | Done | Failed
    deriving (Show, Eq)

taskStatusText :: TaskStatus -> Text
taskStatusText Pending = "pending"
taskStatusText Running = "running"
taskStatusText Done = "done"
taskStatusText Failed = "failed"

parseTaskStatus :: Text -> TaskStatus
parseTaskStatus "pending" = Pending
parseTaskStatus "running" = Running
parseTaskStatus "done" = Done
parseTaskStatus "failed" = Failed
parseTaskStatus _ = Pending

-- | Where a task originated.  New trackers (Jira, Linear, …) add constructors.
data TaskSource
    = -- | Added manually via 'agq add'
      SourceLocal
    | -- | Imported from a GitHub issue; carries the issue number
      SourceGithub Int
    deriving (Show, Eq)

taskSourceText :: TaskSource -> Text
taskSourceText SourceLocal = "local"
taskSourceText (SourceGithub n) = "github:" <> Text.pack (show n)

parseTaskSource :: Text -> TaskSource
parseTaskSource t
    | t == "local" = SourceLocal
    | "github:" `Text.isPrefixOf` t =
        case reads (Text.unpack (Text.drop 7 t)) of
            [(n, "")] -> SourceGithub n
            _ -> SourceLocal -- malformed → fall back
            -- legacy value stored before the ADT existed
    | t == "github" = SourceLocal
    | otherwise = SourceLocal

data Task = Task
    { taskId :: Int
    , taskName :: Text
    , taskLabel :: Text
    , taskSource :: TaskSource
    , taskStatus :: TaskStatus
    , taskInstructionFile :: FilePath
    , taskBaseBranch :: Text
    , taskIsFinal :: Bool
    , taskTriesRemaining :: Int
    }
    deriving (Show)

instance FromRow Task where
    fromRow = do
        tid <- field
        tname <- field
        tlabel <- field
        tsrc <- field
        tst <- field
        tfile <- field
        tbase <- field
        tfinal <- field
        ttries <- field
        return
            Task
                { taskId = tid
                , taskName = tname
                , taskLabel = tlabel
                , taskSource = parseTaskSource (tsrc :: Text)
                , taskStatus = parseTaskStatus (tst :: Text)
                , taskInstructionFile = tfile
                , taskBaseBranch = tbase
                , taskIsFinal = (tfinal :: Int) /= 0
                , taskTriesRemaining = ttries
                }

selectCols :: Query
selectCols = "id,name,label,source,status,instruction_file,base_branch,is_final,tries_remaining"

initDB :: Connection -> IO ()
initDB conn = do
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS tasks (\
        \  id               INTEGER PRIMARY KEY AUTOINCREMENT,\
        \  name             TEXT UNIQUE NOT NULL,\
        \  label            TEXT NOT NULL DEFAULT 'default',\
        \  source           TEXT NOT NULL DEFAULT 'local',\
        \  status           TEXT NOT NULL DEFAULT 'pending',\
        \  instruction_file TEXT NOT NULL,\
        \  base_branch      TEXT NOT NULL DEFAULT 'main',\
        \  is_final         INTEGER NOT NULL DEFAULT 0,\
        \  tries_remaining  INTEGER NOT NULL DEFAULT 1,\
        \  created_at       INTEGER DEFAULT (unixepoch()),\
        \  started_at       INTEGER,\
        \  completed_at     INTEGER,\
        \  error_msg        TEXT\
        \)"
    -- Migration: add tries_remaining to databases created before this field existed
    result <-
        try (execute_ conn "ALTER TABLE tasks ADD COLUMN tries_remaining INTEGER NOT NULL DEFAULT 1") ::
            IO (Either SomeException ())
    case result of
        Left _ -> return () -- column already exists
        Right _ -> return ()
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS task_deps (\
        \  task_id  INTEGER NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,\
        \  dep_name TEXT NOT NULL,\
        \  PRIMARY KEY (task_id, dep_name)\
        \)"
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS task_tags (\
        \  task_id INTEGER NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,\
        \  tag     TEXT NOT NULL,\
        \  PRIMARY KEY (task_id, tag)\
        \)"
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS locks (\
        \  tag         TEXT PRIMARY KEY,\
        \  task_name   TEXT NOT NULL,\
        \  acquired_at INTEGER DEFAULT (unixepoch())\
        \)"
    execute_
        conn
        "CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status)"

insertTask :: Connection -> Task -> [Text] -> [Text] -> IO ()
insertTask conn task deps tags = withTransaction conn $ do
    execute
        conn
        "INSERT OR IGNORE INTO tasks\
        \ (name, label, source, status, instruction_file, base_branch, is_final, tries_remaining)\
        \ VALUES (?,?,?,?,?,?,?,?)"
        ( taskName task
        , taskLabel task
        , taskSourceText (taskSource task)
        , taskStatusText (taskStatus task)
        , taskInstructionFile task
        , taskBaseBranch task
        , if taskIsFinal task then 1 :: Int else 0
        , taskTriesRemaining task
        )
    rows <- query conn "SELECT id FROM tasks WHERE name=?" (Only (taskName task)) :: IO [Only Int]
    case rows of
        [Only tid] -> do
            mapM_
                ( \dep ->
                    execute
                        conn
                        "INSERT OR IGNORE INTO task_deps (task_id, dep_name) VALUES (?,?)"
                        (tid, dep)
                )
                deps
            mapM_
                ( \tag ->
                    execute
                        conn
                        "INSERT OR IGNORE INTO task_tags (task_id, tag) VALUES (?,?)"
                        (tid, tag)
                )
                tags
        _ -> return ()

getTaskByName :: Connection -> Text -> IO (Maybe Task)
getTaskByName conn name = do
    rows <-
        query
            conn
            ("SELECT " <> selectCols <> " FROM tasks WHERE name=?")
            (Only name)
    return $ case rows of
        [t] -> Just t
        _ -> Nothing

listTasks :: Connection -> IO [(Task, [Text], [Text])]
listTasks conn = do
    tasks <-
        query_
            conn
            ("SELECT " <> selectCols <> " FROM tasks ORDER BY id")
    mapM
        ( \t -> do
            deps <- query conn "SELECT dep_name FROM task_deps WHERE task_id=?" (Only (taskId t))
            tags <- query conn "SELECT tag FROM task_tags WHERE task_id=?" (Only (taskId t))
            return (t, map fromOnly deps, map fromOnly tags)
        )
        tasks

-- | Detailed task information including dependencies, tags, and blocking locks.
-- Returns (Task, deps, tags, incompleteDeps, blockingLocks).
listTasksWithDetails :: Connection -> IO [(Task, [Text], [Text], [Text], [Text])]
listTasksWithDetails conn = do
    tasks <-
        query_
            conn
            ("SELECT " <> selectCols <> " FROM tasks ORDER BY id")
    mapM
        ( \t -> do
            deps <- query conn "SELECT dep_name FROM task_deps WHERE task_id=?" (Only (taskId t))
            tags <- query conn "SELECT tag FROM task_tags WHERE task_id=?" (Only (taskId t))
            -- Find incomplete dependencies (deps that exist and are not done)
            incompleteDeps <-
                query
                    conn
                    "SELECT d.dep_name FROM task_deps d\
                    \ JOIN tasks dep ON dep.name = d.dep_name\
                    \ WHERE d.task_id = ? AND dep.status != 'done'"
                    (Only (taskId t)) ::
                    IO [Only Text]
            -- Find locks that block this task's tags
            blockingLocks <-
                query
                    conn
                    "SELECT l.tag FROM locks l\
                    \ JOIN task_tags tt ON tt.tag = l.tag\
                    \ WHERE tt.task_id = ?"
                    (Only (taskId t)) ::
                    IO [Only Text]
            return (t, map fromOnly deps, map fromOnly tags, map fromOnly incompleteDeps, map fromOnly blockingLocks)
        )
        tasks

updateTaskStatus :: Connection -> Text -> TaskStatus -> Maybe Text -> IO ()
updateTaskStatus conn name st merr = case merr of
    Nothing ->
        execute
            conn
            "UPDATE tasks SET status=?, completed_at=unixepoch() WHERE name=?"
            (taskStatusText st, name)
    Just err ->
        execute
            conn
            "UPDATE tasks SET status=?, completed_at=unixepoch(), error_msg=? WHERE name=?"
            (taskStatusText st, err, name)

-- | Decrement tries_remaining when a task is claimed for execution.
decrementTries :: Connection -> Text -> IO ()
decrementTries conn name =
    execute
        conn
        "UPDATE tasks SET tries_remaining = tries_remaining - 1 WHERE name=?"
        (Only name)

{- | Reset a failed (or stuck running) task back to pending and restore its
tries_remaining to the given value. Returns True if the task existed.
-}
retryTask :: Connection -> Text -> Int -> IO Bool
retryTask conn name tries = do
    execute
        conn
        "UPDATE tasks\
        \ SET status='pending', started_at=NULL, completed_at=NULL,\
        \     error_msg=NULL, tries_remaining=?\
        \ WHERE name=? AND status IN ('failed','running')"
        (tries, name)
    n <- changes conn
    return (n > 0)

countPendingRunning :: Connection -> IO Int
countPendingRunning conn = do
    rows <-
        query_
            conn
            "SELECT COUNT(*) FROM tasks WHERE status IN ('pending','running')" ::
            IO [Only Int]
    return $ case rows of
        [Only n] -> n
        _ -> 0

