#!/usr/bin/env bash
set -Eeuo pipefail

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Path to the 'sqq' binary and queue database
SELF="bash ./sqq-agent.sh"
SQQ_BIN="${SQQ_BIN:-/home/lucas/.cabal/bin/sqq}"
QUEUE_DB="tasks/queue.sql"
TASK_DIR="tasks"
SESSIONS_DIR="tasks-sessions"
BASE_BRANCH="main"

# GitHub username for filtering issues (security: only process issues from trusted author)
GITHUB_USERNAME="${GITHUB_USERNAME:-lucasdicioccio}"

# Mapping: Label -> Relative path from worktree root
declare -A PROJECT_MAP=(
    ["root"]="."
    ["architect"]="."
)

# Mapping: Label -> Agent configuration file path
declare -A AGENT_MAP=(
    ["default"]="tasks-agents/kimi-agent-oneshot.json"
    ["architect"]="tasks-agents/kimi-architect.json"
)

# ==============================================================================
# UTILITIES
# ==============================================================================

usage() {
    cat <<EOF
Usage: $0 <command> [args]

Commands:
  init                     Initialize the task queue
  add <label> <branch>     Manually enqueue a task (opens editor for instructions)
  from_github              Fetch tasks from GitHub issues (label: "agents/to-be-taken")
  process                  Start processing the queue
  clean [--do-it] [--force]
                           Clean worktrees with completed sessions (preview mode by default)
  worktree_exec <label> <name> <instruction_file>
                           Internal: The worker command that sets up worktree and runs agent
EOF
    exit 1
}

get_project_dir() {
    local label="$1"
    echo "${PROJECT_MAP[$label]:-}"
}

get_agent_config() {
    local label="$1"
    echo "${AGENT_MAP[$label]:-${AGENT_MAP[default]}}"
}

# ==============================================================================
# CORE COMMANDS
# ==============================================================================

cmd_init() {
    mkdir -p "$TASK_DIR" "$SESSIONS_DIR"
    "$SQQ_BIN" init --queue "$QUEUE_DB"
    echo "Queue initialized at $QUEUE_DB"
}

cmd_add() {
    [[ $# -ne 2 ]] && usage
    local label="$1" branch="$2"
    
    [[ -z "$(get_project_dir "$label")" ]] && { echo "Error: Invalid label '$label'"; exit 1; }

    mkdir -p "$TASK_DIR"
    local taskfile=$(find_or_create_taskfile "$branch")
    
    ${EDITOR:-vim} "$taskfile"

    "$SQQ_BIN" enqueue --queue "$QUEUE_DB" --jobs <(echo "$SELF worktree_exec \"$label\" \"$branch\" \"$taskfile\"")
    echo "Enqueued task: $branch ($label)"
}

cmd_from_github() {
    mkdir -p "$TASK_DIR"
    # Security: Only fetch issues from the trusted author to prevent arbitrary code execution
    # Reverse the list so oldest issues are processed first (gh lists from newest to oldest)
    local issues=$(gh issue list --label "agents/to-be-taken" --author "$GITHUB_USERNAME" --json number,labels | jq 'reverse')
    local count=$(echo "$issues" | jq 'length')

    [[ "$count" -eq 0 ]] && { echo "No tasks found in GitHub."; return 0; }

    for (( i=0; i<count; i++ )); do
        local number=$(echo "$issues" | jq -r ".[$i].number")
        local labels=$(echo "$issues" | jq -r ".[$i].labels[].name")
        
        # Find matching project label
        local label=""
        for l in "${!PROJECT_MAP[@]}"; do
            if echo "$labels" | grep -qE "^$l$"; then label="$l"; break; fi
        done

        [[ -z "$label" ]] && { echo "Skipping issue #$number: No valid project label found."; continue; }

        local branch="gh-$number"
        local taskfile=$(find_or_create_taskfile "$branch")

        # Populate from GitHub if new
        if [[ ! -s "$taskfile" ]]; then
            gh issue view "$number" --json title,body,comments \
              --jq '"# " + .title + "\n\n" + .body + "\n\n" + (.comments | map("## @" + .author.login + "\n" + .body) | join("\n\n"))' \
              > "$taskfile"
            echo -e "\n---\nPlease mention issue #$number in the commit message." >> "$taskfile"
        fi

        "$SQQ_BIN" enqueue --queue "$QUEUE_DB" --jobs <(echo "$SELF worktree_exec \"$label\" \"$branch\" \"$taskfile\"")
        gh issue edit "$number" --remove-label "agents/to-be-taken" --add-label "agents/taken"
        echo "Enqueued GitHub issue #$number as $label task."
    done
}

cmd_process() {
    echo "Starting queue processor..."
    until "$SQQ_BIN" process --queue "$QUEUE_DB" --action Exec; do
        echo "Job failed. Retrying in 1 minute..."
        sleep 60
    done
}

# ==============================================================================
# CLEAN COMMAND
# ==============================================================================

cmd_clean() {
    local do_it=false
    local force_flag=""
    local repo_root=$(git rev-parse --show-toplevel)

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --do-it) do_it=true ;;
            --force) force_flag="--force" ;;
            *) echo "Unknown option: $1"; usage ;;
        esac
        shift
    done

    # List all worktrees and identify those with completed sessions
    local worktrees_to_clean=()
    local branches_to_delete=()

    # Parse git worktree list output
    while IFS= read -r line; do
        # Skip empty lines
        [[ -z "$line" ]] && continue

        # Extract worktree path (it is the second word in porcelain output)
        local worktree_path=$(echo "$line" | cut -d' ' -f2-)
        local worktree_name=$(basename "$worktree_path")

        # Skip if this is the main repository worktree (same as repo root)
        [[ "$worktree_path" == "$repo_root" ]] && continue

        # Check if there's a session file indicating successful completion
        local session_md="${repo_root}/${SESSIONS_DIR}/${worktree_name}.session.md"
        
        if [[ -f "$session_md" ]]; then
            worktrees_to_clean+=("$worktree_path")
            branches_to_delete+=("$worktree_name")
        fi
    done < <(git worktree list --porcelain 2>/dev/null | grep -E '^worktree')

    # If no worktrees to clean
    if [[ ${#worktrees_to_clean[@]} -eq 0 ]]; then
        echo "No worktrees with completed sessions found."
        return 0
    fi

    # Preview or execute
    if [[ "$do_it" == false ]]; then
        echo "=== PREVIEW MODE (use --do-it to execute) ==="
        echo ""
        echo "Worktrees to remove:"
        for wt in "${worktrees_to_clean[@]}"; do
            echo "  - $wt"
        done
        echo ""
        echo "Branches to delete:"
        for branch in "${branches_to_delete[@]}"; do
            echo "  - $branch"
        done
        echo ""
        echo "Run with --do-it to execute these operations."
        return 0
    fi

    # Execute cleanup
    echo "=== EXECUTING CLEANUP ==="
    
    # Remove worktrees
    for wt in "${worktrees_to_clean[@]}"; do
        local wt_name=$(basename "$wt")
        echo "Removing worktree: $wt_name"
        if [[ -n "$force_flag" ]]; then
            git worktree remove --force "$wt_name" || echo "Warning: Failed to remove worktree $wt_name"
        else
            git worktree remove "$wt_name" || echo "Warning: Failed to remove worktree $wt_name (use --force to force removal)"
        fi
    done

    # Prune worktree metadata
    git worktree prune

    # Delete branches (only if worktree was successfully removed)
    for branch in "${branches_to_delete[@]}"; do
        # Check if the branch still exists before trying to delete
        if git show-ref --verify --quiet "refs/heads/$branch" 2>/dev/null; then
            echo "Deleting branch: $branch"
            git branch -D "$branch" || echo "Warning: Failed to delete branch $branch"
        elif git show-ref --verify --quiet "refs/remotes/origin/$branch" 2>/dev/null; then
            echo "Deleting remote branch: origin/$branch"
            git push origin --delete "$branch" 2>/dev/null || echo "Warning: Failed to delete remote branch $branch (may need manual cleanup)"
        else
            echo "Branch $branch already deleted or does not exist"
        fi
    done

    echo ""
    echo "Cleanup completed."
}

# ==============================================================================
# WORKTREE EXECUTION ENGINE
# ==============================================================================

cmd_worktree_exec() {
    set -x
    local label="$1" name="$2" instruction_rel="$3"
    local orig_cwd="$PWD"
    local instruction_abs="${orig_cwd}/${instruction_rel}"
    local repo_root=$(git rev-parse --show-toplevel)

    # 1. Setup Worktree
    cd "$repo_root"
    [[ -d "$name" ]] && git worktree remove --force "$name"
    git worktree prune
    git fetch origin "$BASE_BRANCH"
    git worktree add "$name" "origin/$BASE_BRANCH"

    # 2. Navigate to Project
    local rel_path=$(get_project_dir "$label")
    local project_dir="$repo_root/$name/$rel_path"
    cd "$project_dir"

    # 3. Environment Preparation
    if [[ -x "./git-agent-task.sh" ]]; then
        ./git-agent-task.sh prepare
    else
        echo "Warning: git-agent-task.sh not found. Skipping preparation."
    fi


    # 4. Agent Execution
    local session_file="${SESSIONS_DIR}/${name}.session.json"
    local session_md="${SESSIONS_DIR}/${name}.session.md"
    local agent_config="$(get_agent_config "$label")"
    local commit_message="Update via automation ($name)"

    agents-exe --agent-file "$agent_config" check

    if [[ -f "$instruction_abs" ]]; then
        mkdir -p "$(dirname "$session_file")"
        echo "Running Agent: $label"
        commit_message=$(agents-exe --agent-file "$agent_config" run \
            --session-file "$session_file" -f "$instruction_abs" 2>&1)
        
        agents-exe session-print "$session_file" > "$session_md" || true
    fi

    # 5. Commit and PR
    git checkout -b "$name"
    if [[ -n "$(git status --porcelain)" ]]; then
        git add -A
        git commit --no-verify -m "$commit_message"
        git push -u origin "$name"

        local pr_title=$(echo "$commit_message" | head -n1)
        gh pr create --base "$BASE_BRANCH" --head "$name" --title "$pr_title" --body "$commit_message"
        
        # Optional Preview
        if [[ -x "./git-agent-task.sh" ]] && [[ "$name" != gh-* ]]; then
            ./git-agent-task.sh preview || true
        fi
    fi
}

# ==============================================================================
# HELPERS
# ==============================================================================

find_or_create_taskfile() {
    local branch="$1"
    local existing=$(ls "$TASK_DIR"/[0-9][0-9][0-9][0-9]-"${branch}".md 2>/dev/null | head -n1)
    
    if [[ -n "$existing" ]]; then
        echo "$existing"
    else
        local max_num=$(ls "$TASK_DIR"/[0-9][0-9][0-9][0-9]-*.md 2>/dev/null | grep -oE '^[0-9]+' | sort -n | tail -n1 || echo 0)
        local next_num=$((10#$max_num + 1))
        local new_file=$(printf "%s/%04d-%s.md" "$TASK_DIR" "$next_num" "$branch")
        touch "$new_file"
        echo "$new_file"
    fi
}

# ==============================================================================
# MAIN
# ==============================================================================

[[ $# -lt 1 ]] && usage
COMMAND="$1"; shift

case "$COMMAND" in
    init|add|from_github|process|worktree_exec|clean) "cmd_$COMMAND" "$@" ;;
    *) usage ;;
esac

