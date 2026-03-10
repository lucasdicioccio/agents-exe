module Agq.Schedule
  ( claimNextTask
  , releaseLock
  , recoverStaleLocks
  ) where

import Agq.DB (TaskStatus(..), updateTaskStatus, decrementTries)
import Data.Text (Text)
import Database.SQLite.Simple

-- | Attempt to atomically claim the next ready task.
-- Returns the task name if one was claimed, Nothing otherwise.
claimNextTask :: Connection -> IO (Maybe Text)
claimNextTask conn = withTransaction conn $ do
  rows <- query_ conn readyQuery :: IO [(Int, Text)]
  case rows of
    [] -> return Nothing
    ((tid, tname):_) -> do
      execute conn
        "UPDATE tasks SET status='running', started_at=unixepoch() WHERE id=?"
        (Only tid)
      decrementTries conn tname
      tags <- query conn
        "SELECT tag FROM task_tags WHERE task_id=?"
        (Only tid) :: IO [Only Text]
      mapM_ (\(Only tag) -> execute conn
        "INSERT OR REPLACE INTO locks (tag, task_name, acquired_at) VALUES (?, ?, unixepoch())"
        (tag, tname)) tags
      return (Just tname)
  where
    readyQuery =
      "SELECT t.id, t.name FROM tasks t\
      \ WHERE t.status = 'pending'\
      \   AND t.tries_remaining > 0\
      \   AND NOT EXISTS (\
      \     SELECT 1 FROM task_deps d\
      \     LEFT JOIN tasks dep ON dep.name = d.dep_name\
      \     WHERE d.task_id = t.id\
      \       AND dep.id IS NOT NULL AND dep.status != 'done'\
      \   )\
      \   AND NOT EXISTS (\
      \     SELECT 1 FROM task_tags tt\
      \     JOIN locks l ON l.tag = tt.tag\
      \     WHERE tt.task_id = t.id\
      \   )\
      \ ORDER BY t.id LIMIT 1"

-- | Release all locks held by a task and update its final status.
releaseLock :: Connection -> Text -> TaskStatus -> Maybe Text -> IO ()
releaseLock conn tname st merr = withTransaction conn $ do
  execute conn "DELETE FROM locks WHERE task_name=?" (Only tname)
  updateTaskStatus conn tname st merr

-- | Recover stale locks: remove locks older than staleSeconds with no running
-- task, and reset orphaned running tasks back to pending.
-- Returns the number of tasks reset.
recoverStaleLocks :: Connection -> Int -> IO Int
recoverStaleLocks conn staleSeconds = withTransaction conn $ do
  -- Find stale locks (acquired more than staleSeconds ago)
  staleLocks <- query conn
    "SELECT task_name FROM locks WHERE acquired_at < unixepoch() - ?"
    (Only staleSeconds) :: IO [Only Text]
  let staleNames = map (\(Only n) -> n) staleLocks
  -- Reset tasks that are in 'running' state but have stale locks
  resetCount <- fmap sum $ mapM (\tname -> do
    execute conn
      "UPDATE tasks SET status='pending', started_at=NULL WHERE name=? AND status='running'"
      (Only tname)
    changes conn
    ) staleNames
  -- Delete stale locks
  mapM_ (\tname ->
    execute conn "DELETE FROM locks WHERE task_name=?" (Only tname)
    ) staleNames
  return resetCount
