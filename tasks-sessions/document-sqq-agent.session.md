# Session Report

**Session ID:** 992b394c-f28b-4c69-8eed-71c36ac214a0

---

## Step 1: User Turn

### System Prompt

```
You are an expert Haskell programmer specialized in writing clean, idiomatic, and efficient Haskell code.
When writing code:
- Use modern Haskell practices (base >= 4.14)
- Prefer pure functions and explicit type signatures
- Use standard libraries when appropriate
- Add clear comments for complex logic
- Follow consistent formatting
Use list-files to explore the project structure, read-file to understand existing code, write-file to create or modify files, and check-code to verify your work.
Always compile code before considering the job done.
Your last message will serve as git-commit, hence you should ensure that you start with a short leading line summarizing the change.
```

### User Query

read ./sqq-agent and document how the general flow in a file named docs/sqq-flow.md



### Available Tools

- **bash_grep-files** (`bash_grep-files`)
  - Description: Search for a pattern in files
- **bash_check-code** (`bash_check-code`)
  - Description: Check Haskell code by compiling or testing
- **bash_write-file** (`bash_write-file`)
  - Description: Write content to a file
- **bash_list-files** (`bash_list-files`)
  - Description: List all files in the project
- **bash_read-file** (`bash_read-file`)
  - Description: Read the contents of a file

---

## Step 2: LLM Turn

### Response



### Tool Calls

- **bash_read-file**

---

## Step 3: User Turn



---

## Step 4: LLM Turn

### Response

Now I'll create the documentation file describing the general flow of the sqq-agent system.

### Tool Calls

- **bash_write-file**

---

## Step 5: User Turn



---

## Step 6: LLM Turn

### Response



### Tool Calls

- **bash_read-file**

---

## Step 7: User Turn



---

## Step 8: LLM Turn

### Response

Document the general flow of sqq-agent

I've analyzed the `sqq-agent` bash script and created comprehensive documentation at `docs/sqq-flow.md`. The document covers:

**Architecture Overview**
- Configuration variables (`SQQ_BIN`, `QUEUE_DB`, `PROJECT_MAP`, `AGENT_MAP`)
- ASCII diagram showing the complete data flow from GitHub issues → task files → queue → worktree execution

**Command Flows**
1. `init` - Sets up directories and SQLite queue database
2. `add` - Manually enqueues tasks with interactive instruction editing
3. `from-github` - Imports GitHub issues with specific labels into the queue
4. `process` - Runs the main processing loop with retry logic
5. `worktree_exec` - Core engine with 5 phases: worktree setup → navigation → environment prep → agent execution → commit/PR

**Key Design Patterns**
- Git worktrees provide isolated execution environments per task
- Sequential task file naming (`NNNN-<branch>.md`)
- Agent sessions captured in both JSON and Markdown formats
- Automatic PR creation via GitHub CLI

**Integration Points**
- `sqq` for queue management
- `gh` for GitHub operations  
- `agents-exe` for AI agent execution
- `git worktree` for directory isolation
