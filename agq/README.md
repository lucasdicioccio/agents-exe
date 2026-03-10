# agq — Agent Queue

A standalone Haskell binary that schedules and runs agentic tasks with DAG dependency enforcement, tag-based locking, and GitHub issue integration.

It is the successor to `sqq-agent.sh`, replacing the `sqq`-backed FIFO queue with a richer SQLite schema that understands task dependencies and prevents conflicting parallel runs.

---

## Why agq?

`sqq-agent.sh` has three structural problems:

| Problem | Effect |
|---------|--------|
| No DAG enforcement | A task with `Depends-on: #N` can start before `#N` finishes |
| No locking | Two tasks on the same project/branch can run at the same time and conflict |
| Split sources | Local tasks and GitHub-pulled tasks are managed in two different ways |

`agq` fixes all three:

- Tasks declare dependencies by **name**; `agq` only schedules a task once all its deps are `done`.
- Tasks carry **tags**; `agq` holds an exclusive lock on each tag while a task runs — other tasks sharing any tag are held back.
- Both local and GitHub-sourced tasks live in the same SQLite database with the same lifecycle.

---

## Installation

```bash
# from the agents-exe repo root
cabal build agq
cabal install agq   # puts agq on PATH via ~/.cabal/bin
```

---

## Configuration — `agq.json`

`agq` looks for `./agq.json` by default (override with `-c path/to/agq.json`).

```json
{
  "queueDb":          "tasks/agq.db",
  "taskDir":          "tasks",
  "sessionsDir":      "tasks-sessions",
  "baseBranch":       "main",
  "githubUsername":   "lucasdicioccio",
  "pollSeconds":      30,
  "lockStaleSeconds": 7200,
  "projects": {
    "root":      ".",
    "architect": "."
  },
  "agents": {
    "default":   "tasks-agents/kimi-agent-oneshot.json",
    "architect": "tasks-agents/kimi-architect.json"
  },
  "labels": {
    "labelToBeTaken": "agents/to-be-taken",
    "labelTaken":     "agents/taken",
    "labelWait":      "agents/wait",
    "labelAgentPr":   "agents/agent-pr"
  }
}
```

| Field | Meaning |
|-------|---------|
| `queueDb` | Path to the SQLite database file |
| `taskDir` | Directory where instruction `.md` files live |
| `sessionsDir` | Directory where session `.json` and `.md` files are written |
| `baseBranch` | Default git branch new worktrees are based on |
| `githubUsername` | Only issues from this author are imported (`pull`) |
| `pollSeconds` | How long `process` sleeps when the queue is temporarily empty |
| `lockStaleSeconds` | Locks older than this (in seconds) are considered stale and released by `recover` |
| `projects` | Maps a label → relative path inside the worktree |
| `agents` | Maps a label → agent config file path |
| `labels.labelToBeTaken` | GitHub label meaning "ready to pick up" |
| `labels.labelTaken` | GitHub label applied after an issue is claimed |
| `labels.labelWait` | GitHub label meaning "blocked on dependencies" |
| `labels.labelAgentPr` | GitHub label applied to PRs created by the agent |

---

## Commands

### `agq init`

Creates `taskDir`, `sessionsDir`, and the SQLite database with its full schema.
Safe to run multiple times (all DDL uses `CREATE … IF NOT EXISTS`).

```bash
agq init
```

---

### `agq add <LABEL> <NAME> [--dep NAME]… [--tag TAG]…`

Adds a local task to the queue.

- Creates `tasks/<NAME>.md` if it doesn't exist, then opens `$EDITOR` when the file is empty.
- `LABEL` must match a key in the `projects` map (determines which agent config and project directory are used).
- `--dep` may be repeated; each value is the name of another task that must be `done` first.
- `--tag` may be repeated; tasks sharing a tag cannot run concurrently. The label is always added as a tag automatically.

```bash
agq add root feat-auth
agq add root feat-api --dep feat-auth
agq add root feat-tests --dep feat-auth --dep feat-api --tag slow
```

---

### `agq pull`

Imports GitHub issues labelled `agents/to-be-taken` (authored by `githubUsername`) into the queue.

For each issue:
1. Downloads the title and body into `tasks/gh-<N>.md` (skipped if the file already has content).
2. Parses `Base-branch:` and `Final:` metadata lines from the issue body.
3. Parses `Depends-on: #N, #M` to wire up task dependencies.
4. Inserts the task with `INSERT OR IGNORE` (idempotent).
5. Moves the issue label from `agents/to-be-taken` → `agents/taken`.

**Metadata headers** (in the issue body):

```
Base-branch: feat/my-feature
Final: true
Depends-on: #42, #43
```

---

### `agq promote`

Checks all issues labelled `agents/wait`. For each one, resolves the `Depends-on:` refs (by querying `gh issue view` / `gh pr view`). If all deps are closed or merged, the issue is promoted to `agents/to-be-taken`.

```bash
agq promote
```

---

### `agq status`

Prints a table of all tasks with their label, status, dependencies, and tags. Also lists any active locks.

```
ID    NAME                          LABEL       STATUS    DEPS                TAGS
------------------------------------------------------------------------------------------
1     feat-auth                     root        done                          root
2     feat-api                      root        pending   feat-auth           root
3     feat-tests                    root        pending   feat-auth,feat-api  root,slow
```

---

### `agq process [--parallel]`

The main scheduling loop:

1. Calls `recover` to release stale locks.
2. Atomically claims the next **ready** task (pending, all deps done, no conflicting lock).
3. Executes it (see `exec` below).
4. Loops. When no task is ready but some are still pending/running, sleeps for `pollSeconds` then retries. When the queue is fully empty, exits.

`--parallel` forks each task in a new thread instead of running sequentially.

```bash
agq process
agq process --parallel
```

---

### `agq exec <NAME>`

Executes a single named task directly (also used internally by `process`):

1. `git fetch origin <base-branch>`
2. `git worktree add <name> origin/<base-branch>`
3. Runs `./git-agent-task.sh prepare <label> <name> <instruction-file>` if the hook exists.
4. Runs `agents-exe --agent-file <config> run --session-file <session> -f <instruction>`, capturing stdout as the commit message.
5. Runs `agents-exe session-print <session>` to generate the Markdown session log.
6. `git checkout -b <name> && git add -A && git commit --no-verify -m <commit-msg>`
7. `git push -u origin <name>`
8. `gh pr create --base <target> --head <name> --label agents/agent-pr`
9. On success: marks task `done` and releases locks.
   On failure: marks task `failed` and releases locks.

```bash
agq exec gh-42
```

---

### `agq merge-prs`

Merges all open PRs labelled `agents/agent-pr` whose base branch is **not** the repo's default branch (i.e. intermediate feature-branch PRs). Uses `gh pr merge --merge --auto`.

```bash
agq merge-prs
```

---

### `agq clean [--do-it] [--force]`

Removes git worktrees for tasks that have a completed session log (`tasks-sessions/<name>.session.md`).

Without `--do-it`, prints a preview of what would be removed.
`--force` passes `--force` to `git worktree remove` for worktrees with uncommitted changes.

```bash
agq clean           # preview
agq clean --do-it   # execute
agq clean --do-it --force
```

---

### `agq recover`

Finds locks that have been held longer than `lockStaleSeconds` without a corresponding running task and resets those tasks to `pending`. Useful after a crash or `kill`.

```bash
agq recover
```

---

## Database Schema

```sql
tasks        -- one row per task; status: pending | running | done | failed
task_deps    -- (task_id, dep_name)  — dep resolved by name at claim time
task_tags    -- (task_id, tag)       — lock scope
locks        -- (tag, task_name, acquired_at) — held while a task runs
```

A task is **ready** when:
- `status = 'pending'`
- Every row in `task_deps` has a corresponding `tasks` row with `status = 'done'`
- None of its tags appear in `locks`

The claim query runs inside an SQLite exclusive transaction, making it safe to run multiple `agq process` instances concurrently.

---

## Migrating from `sqq-agent.sh`

1. Build: `cabal build agq`
2. Copy `agq.json` to your repo root and adjust `projects` / `agents` to match your old `PROJECT_MAP` / `AGENT_MAP`.
3. `agq init` — creates `tasks/agq.db` (separate from `tasks/queue.sql`).
4. `agq pull` — re-imports any GitHub issues still open.
5. Switch your cron / tmux session from `bash sqq-agent.sh process` to `agq process`.
6. Leave `sqq-agent.sh` in place to drain its existing queue.
