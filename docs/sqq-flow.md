# SQQ Agent Flow Documentation

## Overview

The `sqq-agent.sh` is a task queue automation system that manages AI agent execution on Git repositories. It integrates with GitHub issues, uses git worktrees for isolated task execution, and automatically creates pull requests with the results.

## Architecture Components

### Core Configuration

| Variable | Purpose |
|----------|---------|
| `SQQ_BIN` | Path to the `sqq` queue management binary |
| `QUEUE_DB` | SQLite database path for task queue storage (`tasks/queue.sql`) |
| `TASK_DIR` | Directory for instruction files (`tasks/`) |
| `SESSIONS_DIR` | Directory for agent session outputs (`tasks-sessions/`) |
| `BASE_BRANCH` | Base git branch for new worktrees (`main`) |
| `GITHUB_USERNAME` | GitHub username to filter issues by author (security filter) |
| `PROJECT_MAP` | Maps labels to relative paths in worktree |
| `AGENT_MAP` | Maps labels to agent configuration files |

### Data Flow

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  GitHub Issues  │────▶│  Task Files     │────▶│  Queue (sqq)    │
│  (with labels)  │     │  (.md files)    │     │  (SQLite)       │
└─────────────────┘     └─────────────────┘     └─────────────────┘
         │                                               │
         │                                               ▼
         │                                      ┌─────────────────┐
         │                                      │  Process Loop   │
         │                                      └─────────────────┘
         │                                               │
         ▼                                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                        WORKTREE EXECUTION                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────┐  │
│  │ Setup        │─▶│ Prepare      │─▶│ Run Agent            │  │
│  │ Worktree     │  │ Environment  │  │ (agents-exe)         │  │
│  └──────────────┘  └──────────────┘  └──────────────────────┘  │
│                                              │                  │
│  ┌──────────────┐  ┌──────────────┐         │                  │
│  │ Create PR    │◀─│ Push Branch  │◀────────┘                  │
│  └──────────────┘  └──────────────┘                            │
└─────────────────────────────────────────────────────────────────┘
```

## Command Flows

### 1. Initialization (`init`)

```bash
sqq-agent.sh init
```

1. Creates `TASK_DIR` and `SESSIONS_DIR` directories
2. Initializes SQLite queue database via `sqq init --queue`

### 2. Manual Task Addition (`add`)

```bash
sqq-agent.sh add <label> <branch>
```

1. Validates the project label exists in `PROJECT_MAP`
2. Finds or creates a numbered task file (format: `NNNN-<branch>.md`)
3. Opens `$EDITOR` for user to write instructions
4. Enqueues job: `sqq-agent.sh worktree_exec "<label>" "<branch>" "<taskfile>"`

### 3. GitHub Issue Import (`from_github`)

```bash
sqq-agent.sh from_github
```

1. Queries GitHub for issues labeled `agents/to-be-taken` **from the configured author only**
2. For each issue:
   - Matches project label from issue labels
   - Creates task file with issue content (title, body, comments)
   - Adds note to mention issue number in commit
   - Enqueues worktree_exec job
   - Updates issue labels: removes `agents/to-be-taken`, adds `agents/taken`

**Security Note:** The `GITHUB_USERNAME` filter ensures that only issues created by the trusted author are processed. This prevents arbitrary code execution from malicious issues filed by third parties on public repositories.

### 4. Queue Processing (`process`)

```bash
sqq-agent.sh process
```

1. Runs `sqq process --action Exec` in a loop
2. On failure: waits 60 seconds and retries
3. Continues until queue is empty or manually stopped

### 5. Worktree Execution (`worktree_exec`) - Core Engine

```bash
sqq-agent.sh worktree_exec <label> <name> <instruction_file>
```

#### Phase 1: Worktree Setup
1. Fetch latest `origin/BASE_BRANCH`
2. Remove existing worktree (if any) and prune
3. Create new git worktree at directory `<name>`

#### Phase 2: Navigation
1. Change to project directory based on `PROJECT_MAP[<label>]`

#### Phase 3: Environment Preparation
1. Execute `./git-agent-task.sh prepare` if present
2. This allows per-project dependency installation or setup

#### Phase 4: Agent Execution
1. Check agent configuration with `agents-exe --agent-file check`
2. Run agent with instruction file:
   ```bash
   agents-exe --agent-file <config> run \
       --session-file <session>.json \
       -f <instruction_file>
   ```
3. Capture agent output as commit message
4. Export session to markdown: `agents-exe session-print`

#### Phase 5: Commit and PR
1. Create and checkout new branch `<name>`
2. If changes detected:
   - Stage all changes: `git add -A`
   - Commit with agent output as message (no verify)
   - Push to origin
   - Create Pull Request via `gh pr create` with label `agents/agent-pr`
3. For non-GitHub tasks: optionally run `./git-agent-task.sh preview`

### 6. Automatic Merging (`merge-pr`)

```bash
sqq-agent.sh merge-pr
```

1. Identifies the default branch of the repository.
2. Lists all Pull Requests with the label `agents/agent-pr`.
3. For each PR:
   - If the base branch is **not** the default branch, it triggers an automatic merge (`gh pr merge --auto`).
   - If the base branch is the default branch, it skips the PR to ensure manual review for main branch changes.

## File Naming Conventions

### Task Files
- Format: `TASK_DIR/NNNN-<branch>.md`
- Sequential numbering starting from 0001
- Preserves existing files when re-queueing same branch

### Session Files
- JSON: `SESSIONS_DIR/<name>.session.json` (agent state)
- Markdown: `SESSIONS_DIR/<name>.session.md` (human-readable)

## Error Handling and Retries

- Queue processing loops indefinitely with 60s delay on failure
- Failed worktrees are force-removed before retry
- Agent execution errors don't prevent commit/push if changes exist
- Missing `git-agent-task.sh` generates warnings but continues

## Integration Points

| Tool | Purpose |
|------|---------|
| `sqq` | Task queue management (enqueue/dequeue/process) |
| `gh` | GitHub CLI for issue/PR management |
| `agents-exe` | AI agent execution engine |
| `git worktree` | Isolated working directories per task |

## Security Considerations

- Uses `--no-verify` on git commits to bypass hooks in worktrees
- Force-removes worktrees (potential data loss if in-progress)
- Executes `./git-agent-task.sh` without sandboxing
- **Critical:** `GITHUB_USERNAME` environment variable filters GitHub issues by author to prevent processing malicious issues from third parties on public repositories. Default is `lucasdicioccio`.

