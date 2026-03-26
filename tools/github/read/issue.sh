#!/usr/bin/env bash
# Required tools: gh (GitHub CLI)
# This script lists GitHub issues or reads a specific issue using the 'gh' CLI.

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "read_github_issue",
    "description": "List or read GitHub issues using the 'gh' CLI",
    "args": [
        {
            "name": "action",
            "description": "Either 'list' to list issues or 'view' to read a specific issue",
            "type": "string",
            "backing_type": "string",
            "arity": "single",
            "mode": "positional"
        },
        {
            "name": "id",
            "description": "Issue number (required when action is 'view', optional filter when action is 'list')",
            "type": "string",
            "backing_type": "string",
            "arity": "single",
            "mode": "positional"
        }
    ]
}
EOF
    exit 0
fi

if [[ "$1" == "run" ]]; then
    action="$2"
    id="$3"

    case "${action}" in
        list)
            # List issues - optionally filter by ID (label, assignee, etc. could be extended)
            echo "---"
            if [[ -n "${id}" ]]; then
                # If ID provided with list, use it as a limit (most recent N issues)
                gh issue list --limit "${id}"
            else
                gh issue list
            fi
            ;;
        view)
            # View specific issue - ID is required
            if [[ -z "${id}" ]]; then
                echo "Error: issue ID is required when action is 'view'" >&2
                exit 1
            fi
            echo "---"
            gh issue view "${id}"
            ;;
        *)
            echo "Error: action must be either 'list' or 'view'" >&2
            exit 1
            ;;
    esac
    exit 0
fi

