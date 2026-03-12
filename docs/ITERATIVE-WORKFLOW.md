# Iterative Development Workflow

> How to handle multi-step, recursive, or iterative invocations on this codebase.

## Invocation Patterns

### Pattern 1: Fresh Start (No Previous State)

**When:** First time working on this codebase, or no knowledge base.

**Steps:**
1. Initialize knowledge base tables
2. Scan all source files → `code_index`
3. Read existing docs → `docs_index`
4. Identify documentation gaps
5. Create initial documentation plan

```sql
-- Initialize if empty
CREATE TABLE IF NOT EXISTS project_overview (...);
CREATE TABLE IF NOT EXISTS code_index (...);
CREATE TABLE IF NOT EXISTS docs_index (...);
CREATE TABLE IF NOT EXISTS commits_seen (...);

-- Populate code_index
-- (scan all .hs files with bash_list-files and bash_grep-files)
```

### Pattern 2: Continue Previous Work

**When:** Knowledge base exists, continuing documentation task.

**Steps:**
1. Query what was completed
2. Check for new commits since last work
3. Review current docs_index state
4. Pick up where left off

```sql
-- What was documented?
SELECT module_name, last_updated 
FROM code_index 
WHERE documented_in IS NOT NULL
ORDER BY last_updated DESC;

-- What's missing?
SELECT filepath, module_name 
FROM code_index 
WHERE documented_in IS NULL;

-- Any new commits?
SELECT * FROM commits_seen 
ORDER BY commit_date DESC LIMIT 10;
```

### Pattern 3: Handle New Code Changes

**When:** Code has changed since last documentation.

**Steps:**
1. Check commits_seen vs git log
2. Read new commit details
3. Identify affected modules
4. Update documentation
5. Record commits as processed

```bash
# Get recent commits
bash_get_git_log

# Read specific commit
bash_get_git_commit abc123

# Find affected files
bash_grep-files "changed_pattern"
```

### Pattern 4: Deep Dive on Subsystem

**When:** Need to document one specific subsystem thoroughly.

**Steps:**
1. Identify all modules in subsystem
2. Read each module source
3. Extract types and functions
4. Create/update doc file
5. Link all modules in code_index

```sql
-- Find subsystem modules
SELECT * FROM code_index 
WHERE module_name LIKE 'System.Agents.Subsystem.%';
```

## Chunking Strategy

Large documentation tasks should be chunked:

### By Subsystem

```
Chunk 1: Core (Base, Runtime, AgentTree)
Chunk 2: Tools (Bash, MCP, OpenAPI, IO)
Chunk 3: Sessions (Types, Loop, Store)
Chunk 4: Interfaces (CLI, TUI, MCP Server)
Chunk 5: Utilities (FileLoader, ExportImport)
```

### By Document

```
Iteration 1: README.md (overview)
Iteration 2: architecture.md (core)
Iteration 3: tools.md (tools)
Iteration 4: cli-commands.md (CLI)
Iteration 5: Remaining docs
```

### State Tracking Between Iterations

Always update the knowledge base at the end of each iteration:

```sql
-- Record what's been documented
UPDATE code_index 
SET documented_in = 'docs/architecture.md',
    last_updated = datetime('now')
WHERE module_name IN ('System.Agents.Base', 'System.Agents.Runtime');

-- If new doc created
INSERT INTO docs_index (doc_path, title, description, related_modules)
VALUES ('docs/new-feature.md', 'New Feature', 'Description', 'Module1, Module2');
```

## Recursive Documentation

### When Documenting Recursion Features

The codebase has recursion (agents calling agents). When documenting:

1. **Acknowledge the recursion** - Note that this is a recursive feature
2. **Show depth tracking** - Document `ctxMaxDepth`, `CallStackEntry`
3. **Explain limits** - Document how infinite loops are prevented
4. **Cross-reference** - Link to related recursive patterns

```markdown
## Agent Recursion

Agents can call other agents, creating a call stack:

```haskell
data ToolExecutionContext = ToolExecutionContext
    { ctxCallStack :: [CallStackEntry]  -- Tracks recursion
    , ctxMaxDepth :: Maybe Int          -- Prevents infinite loops
    }
```

See also: [Tool Context](tools.md#recursion-control)
```

### Self-Referential Documentation

This document itself is recursive - it documents how to document. When updating:
- Keep patterns general enough to apply to future changes
- Don't make assumptions about specific future tasks
- Focus on process, not content

## Knowledge Base Consistency Checks

Between iterations, verify:

```sql
-- Check 1: All modules have valid doc references
SELECT module_name, documented_in 
FROM code_index 
WHERE documented_in IS NOT NULL 
  AND documented_in NOT IN (SELECT doc_path FROM docs_index);

-- Check 2: All docs reference valid modules
SELECT di.doc_path, di.related_modules
FROM docs_index di
LEFT JOIN code_index ci ON di.related_modules LIKE '%' || ci.module_name || '%'
WHERE ci.module_name IS NULL;

-- Check 3: No duplicate doc entries
SELECT doc_path, COUNT(*) 
FROM docs_index 
GROUP BY doc_path 
HAVING COUNT(*) > 1;

-- Check 4: Timestamps are recent
SELECT module_name, last_updated
FROM code_index
WHERE last_updated < datetime('now', '-7 days')
  AND documented_in IS NOT NULL;
```

## Decision Tree

```
Invoked on codebase
       │
       ▼
Knowledge base exists?
       │
   ┌───┴───┐
   │       │
  Yes     No
   │       │
   ▼       ▼
Check     Initialize
state     tables
   │       │
   └───┬───┘
       ▼
Task specified?
       │
   ┌───┴───┐
   │       │
  Yes     No
   │       │
   ▼       ▼
Do task   Identify
          gaps
       │
       ▼
Multiple subtasks?
       │
   ┌───┴───┐
   │       │
  Yes     No
   │       │
   ▼       ▼
Chunk    Complete
work     task
   │
   ▼
Update KB
   │
   ▼
Done (or next iteration)
```

## Handoff Notes

When work will continue in next invocation, include:

1. **What's done**: List completed documentation
2. **What's next**: Specific next steps
3. **Blockers**: Any issues to resolve
4. **Context**: Relevant state from knowledge base

Example handoff:

```markdown
## Handoff Summary

Completed:
- docs/architecture.md (core types, runtime, agent tree)
- docs/tools.md (bash tools, partial MCP)

Next:
- Complete MCP section in tools.md
- Document OpenAPI tools
- Create mcp.md for protocol details

Blockers: None

Context:
- 12 of 79 modules documented
- Runtime and AgentTree fully covered
- Tools subsystem 60% complete
```

## Common Iteration Sequences

### Sequence: Full Documentation Pass

```
Iteration 1: README + architecture
Iteration 2: Tools system
Iteration 3: Sessions + TUI
Iteration 4: CLI + Export/Import
Iteration 5: File loading + cleanup
Iteration 6: Review + cross-references
```

### Sequence: New Feature Documentation

```
Iteration 1: Understand feature (read code)
Iteration 2: Document core types
Iteration 3: Document API/functions
Iteration 4: Add examples
Iteration 5: Update related docs
Iteration 6: Add to README/index
```

### Sequence: Maintenance Update

```
Iteration 1: Check new commits
Iteration 2: Identify affected docs
Iteration 3: Update affected sections
Iteration 4: Verify consistency
Iteration 5: Record commits processed
```

## Tool Usage by Phase

### Discovery Phase
- `bash_list-files` - Find all source files
- `bash_grep-files` - Search for patterns
- `bash_get_git_log` - See recent changes

### Analysis Phase
- `bash_get_git_commit` - Read specific changes
- Read source files with grep results
- Query `code_index` for relationships

### Documentation Phase
- `bash_write-file` - Create/update docs
- Update `docs_index` with new entries
- Update `code_index` with document links

### Verification Phase
- Query for consistency
- Check cross-references
- Verify all modules covered

## Anti-Patterns to Avoid

1. **Don't** document without updating knowledge base
2. **Don't** create docs without adding to `docs_index`
3. **Don't** leave `code_index` modules unlinked
4. **Don't** assume previous state - always query
5. **Don't** document too much in one iteration (fatigue/errors)
6. **Don't** forget the summary line (first line of response)

## Success Metrics

Good iterative documentation:
- Each iteration has clear scope
- Knowledge base stays consistent
- No lost work between invocations
- Progress is measurable (X of Y modules done)
- Final docs are comprehensive and cross-referenced

## Quick Reference Card

```
STARTING:
  SELECT * FROM project_overview;
  SELECT * FROM docs_index;
  SELECT * FROM code_index WHERE documented_in IS NULL;

DURING:
  Read source → Extract types → Write doc → Update KB

ENDING:
  UPDATE code_index SET documented_in = 'docs/X.md' ...
  INSERT INTO docs_index ... (if new doc)
  SELECT consistency checks

HANDOFF:
  List done, next steps, blockers, context
  Include summary line as first line
```

---

Use this workflow for multi-step documentation tasks.

