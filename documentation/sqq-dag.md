# Analysis of Issue Dependencies and DAG-Aware Agent Processing

## Overview

This document analyzes the current interaction between `sqq-agent.sh` and `create-issue.sh`, focusing on how issue dependencies are handled and the resulting impact on parallel development and git branch management.

## Current State Analysis

### 1. Issue Creation (`create-issue.sh`)
- **Mechanism:** The script takes a `dependencies` argument (e.g., `#42,#43`).
- **Labeling Logic:**
    - No dependencies: Labeled `agents/to-be-taken`.
    - Has dependencies: Labeled `agents/wait`.
- **Limitation:** The labeling is static at creation time. There is no automated process to monitor dependencies and "unlock" `agents/wait` issues.

### 2. Task Fetching (`sqq-agent.sh from_github`)
- **Mechanism:** Queries GitHub for issues with the `agents/to-be-taken` label.
- **Limitation:** It is "blind" to any issue labeled `agents/wait`. This requires manual intervention (human or another agent) to re-label issues once their dependencies are met.

### 3. Worktree Execution (`sqq-agent.sh worktree_exec`)
- **Branching Strategy:** Always executes `git worktree add "$name" "origin/main"`.
- **The "Context Loss" Problem:**
    - If Issue B depends on Issue A, Issue B *must* have the code changes from Issue A to proceed correctly (e.g., using a new Type or API defined in A).
    - If Issue B is started before Issue A is merged into `main`, Issue B will be missing critical context.
    - Currently, `sqq-agent.sh` has no awareness of which branch a dependency might be residing in.

### 4. The "Unfinished Work" Problem
- Users reported that merging multiple parallel PRs can lead to `main` having "unfinished work" if dependencies aren't respected.
- Without a strict DAG (Directed Acyclic Graph) enforcement, agents might implement redundant or conflicting logic because they are both branching from an older state of `main`.

---

## Proposed DAG-Aware Model

To resolve these issues, we propose a transition from a "label-polling" model to a "DAG-aware" model.

### 1. Automated Issue Promotion
We need a "Manager" process (or an update to `from_github`) that:
1. Lists all issues with `agents/wait`.
2. Parses the dependency IDs from the issue description or metadata.
3. Checks the status of these dependencies via `gh issue view` or `gh pr view`.
4. **Promotion Rule:** If all dependencies are "Closed" (or their associated PRs are "Merged"), the label is updated to `agents/to-be-taken`.

### 2. Branch Chaining (Optional but Powerful)
Instead of always branching from `main`, the system could support "Branch Chaining":
- If Issue B depends on Issue A, and Issue A's work is in branch `gh-A`, Issue B could branch from `origin/gh-A` instead of `origin/main`.
- **Pros:** Allows pipelined development without waiting for merges.
- **Cons:** Requires complex rebasing if Issue A is updated after Issue B starts.

### 3. Strict Sequential Processing (Recommended)
Given the user requirement that "the processing of the second issue should wait for the first issue to be finalized", a strict sequential approach is safer:
- **Rule:** An issue is only promoted to `to-be-taken` when all its dependencies are **merged into `main`**.
- This ensures `origin/main` always contains the necessary context for the next task.
- This prevents "unfinished work" from polluting `main` because each task starts from a "finalized" baseline.

## Implementation Recommendations

### Short-term Fixes:
1. **Update `sqq-agent.sh`:** Add a `promote_issues` command that uses `gh issue list --label agents/wait` and checks dependency status.
2. **Standardize Dependency Metadata:** Ensure `create-issue.sh` stores dependencies in a machine-readable format (e.g., a specific line in the body like `Depends-on: #123, #124`).

### Long-term Architecture:
- **Centralized DAG State:** The `sqq` queue database could be extended to store task dependencies directly, allowing the scheduler to respect the DAG without polling GitHub repeatedly.
- **Feature Branch Isolation:** Maintain the current practice of using `gh-<number>` branches, but ensure the `BASE_BRANCH` is dynamically determined if "Branch Chaining" is desired.

## Summary of Benefits
- **Zero Manual Intervention:** Issues flow from `wait` -> `to-be-taken` -> `taken` -> `completed` automatically.
- **Consistency:** Agents always work on top of the latest "finalized" code they depend on.
- **Clean Main:** Prevents the "unfinished work" syndrome by ensuring a logical ordering of merges.
