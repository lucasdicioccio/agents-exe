#!/usr/bin/env bash
# tmux-record.sh - Record the content of the currently-running tmux window
# Required tools: bash, tmux

# Handle the 'describe' command to advertise tool interface
if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "tmux-record",
    "description": "Record the content of the currently-running tmux window using tmux capture-pane",
    "args": [
        {
            "name": "start_line",
            "description": "Line number to start capturing from (negative for relative to end, default captures all history)",
            "type": "integer",
            "backing_type": "integer",
            "arity": "optional",
            "mode": "positional"
        }
    ]
}
EOF
    exit 0
fi

# Handle the 'run' command
if [[ "$1" == "run" ]]; then
    # Check if running inside tmux
    if [[ -z "$TMUX" ]]; then
        echo "Error: Not running inside a tmux session (TMUX environment variable not set)"
        exit 1
    fi

    # Determine start line argument
    # $2 contains the optional start_line argument
    local_start_line="${2:--}"

    # Capture pane content
    echo "----"
    tmux capture-pane -p -S "$local_start_line"
    exit_code=$?

    if [[ $exit_code -ne 0 ]]; then
        echo "Error: tmux capture-pane command failed (exit code: $exit_code)"
        exit 1
    fi

    exit 0
fi

# If no valid command provided
echo "Usage: $0 {describe|run} [start_line]"
exit 1

