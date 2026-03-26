#!/usr/bin/env bash
# tmux-record.sh - Record the content of the currently-running tmux window
# Required tools: bash, tmux

# Handle the 'describe' command to advertise tool interface
if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "tmux-record",
    "description": "Record the content of the currently-running tmux window using tmux capture-pane",
    "args": [ ]
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

    # Capture pane content
    echo "----"
    tmux capture-pane
    tmux save-buffer /dev/stdout
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

