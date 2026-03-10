module Agq.DB
  ( TaskStatus(..)
  , Task(..)
  , taskStatusText
  , parseTaskStatus
  , initDB
  , insertTask
  , getTaskByName
  , listTasks
  , updateTaskStatus
  , countPendingRunning
  ) where

import Data.Text (Text)
import Database.SQLite.Simple

-- | Task execution status
data TaskStatus = Pending | Running | Done | Failed
  deriving (Show, Eq)

taskStatusText :: TaskStatus -> Text
taskStatusText Pending = "pending"
taskStatusText Running = "running"
taskStatusText Done    = "done"
taskStatusText Failed  = "failed"

parseTaskStatus :: Text -> TaskStatus
parseTaskStatus "pending" = Pending
parseTaskStatus "running" = Running
parseTaskStatus "done"    = Done
parseTaskStatus "failed"  = Failed
parseTaskStatus _         = Pending

data Task = Task
  { taskId              :: Int
  , taskName            :: Text
  , taskLabel           :: Text
  , taskSource          :: Text
  , taskStatus          :: TaskStatus
  , taskInstructionFile :: FilePath
  , taskBaseBranch      :: Text
  , taskIsFinal         :: Bool
  } deriving (Show)

instance FromRow Task where
  fromRow = do
    tid    <- field
    tname  <- field
    tlabel <- field
    tsrc   <- field
    tst    <- field
    tfile  <- field
    tbase  <- field
    tfinal <- field
    return Task
      { taskId              = tid
      , taskName            = tname
      , taskLabel           = tlabel
      , taskSource          = tsrc
      , taskStatus          = parseTaskStatus (tst :: Text)
      , taskInstructionFile = tfile
      , taskBaseBranch      = tbase
      , taskIsFinal         = (tfinal :: Int) /= 0
      }

initDB :: Connection -> IO ()
initDB conn = do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS tasks (\
    \  id               INTEGER PRIMARY KEY AUTOINCREMENT,\
    \  name             TEXT UNIQUE NOT NULL,\
    \  label            TEXT NOT NULL DEFAULT 'default',\
    \  source           TEXT NOT NULL DEFAULT 'local',\
    \  status           TEXT NOT NULL DEFAULT 'pending',\
    \  instruction_file TEXT NOT NULL,\
    \  base_branch      TEXT NOT NULL DEFAULT 'main',\
    \  is_final         INTEGER NOT NULL DEFAULT 0,\
    \  created_at       INTEGER DEFAULT (unixepoch()),\
    \  started_at       INTEGER,\
    \  completed_at     INTEGER,\
    \  error_msg        TEXT\
    \)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS task_deps (\
    \  task_id  INTEGER NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,\
    \  dep_name TEXT NOT NULL,\
    \  PRIMARY KEY (task_id, dep_name)\
    \)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS task_tags (\
    \  task_id INTEGER NOT NULL REFERENCES tasks(id) ON DELETE CASCADE,\
    \  tag     TEXT NOT NULL,\
    \  PRIMARY KEY (task_id, tag)\
    \)"
  execute_ conn
    "CREATE TABLE IF NOT EXISTS locks (\
    \  tag         TEXT PRIMARY KEY,\
    \  task_name   TEXT NOT NULL,\
    \  acquired_at INTEGER DEFAULT (unixepoch())\
    \)"
  execute_ conn
    "CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status)"

insertTask :: Connection -> Task -> [Text] -> [Text] -> IO ()
insertTask conn task deps tags = withTransaction conn $ do
  execute conn
    "INSERT OR IGNORE INTO tasks (name, label, source, status, instruction_file, base_branch, is_final)\
    \ VALUES (?,?,?,?,?,?,?)"
    ( taskName task
    , taskLabel task
    , taskSource task
    , taskStatusText (taskStatus task)
    , taskInstructionFile task
    , taskBaseBranch task
    , if taskIsFinal task then 1 :: Int else 0
    )
  rows <- query conn "SELECT id FROM tasks WHERE name=?" (Only (taskName task)) :: IO [Only Int]
  case rows of
    [Only tid] -> do
      mapM_ (\dep -> execute conn
        "INSERT OR IGNORE INTO task_deps (task_id, dep_name) VALUES (?,?)"
        (tid, dep)) deps
      mapM_ (\tag -> execute conn
        "INSERT OR IGNORE INTO task_tags (task_id, tag) VALUES (?,?)"
        (tid, tag)) tags
    _ -> return ()

getTaskByName :: Connection -> Text -> IO (Maybe Task)
getTaskByName conn name = do
  rows <- query conn
    "SELECT id,name,label,source,status,instruction_file,base_branch,is_final FROM tasks WHERE name=?"
    (Only name)
  return $ case rows of
    [t] -> Just t
    _   -> Nothing

listTasks :: Connection -> IO [(Task, [Text], [Text])]
listTasks conn = do
  tasks <- query_ conn
    "SELECT id,name,label,source,status,instruction_file,base_branch,is_final FROM tasks ORDER BY id"
  mapM (\t -> do
    deps <- query conn "SELECT dep_name FROM task_deps WHERE task_id=?" (Only (taskId t))
    tags <- query conn "SELECT tag FROM task_tags WHERE task_id=?" (Only (taskId t))
    return (t, map fromOnly deps, map fromOnly tags)
    ) tasks

updateTaskStatus :: Connection -> Text -> TaskStatus -> Maybe Text -> IO ()
updateTaskStatus conn name st merr = case merr of
  Nothing  -> execute conn
    "UPDATE tasks SET status=?, completed_at=unixepoch() WHERE name=?"
    (taskStatusText st, name)
  Just err -> execute conn
    "UPDATE tasks SET status=?, completed_at=unixepoch(), error_msg=? WHERE name=?"
    (taskStatusText st, err, name)

countPendingRunning :: Connection -> IO Int
countPendingRunning conn = do
  rows <- query_ conn "SELECT COUNT(*) FROM tasks WHERE status IN ('pending','running')" :: IO [Only Int]
  return $ case rows of
    [Only n] -> n
    _        -> 0
