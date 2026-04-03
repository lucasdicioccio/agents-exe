#!/usr/bin/env bash

# Required tools: bash, dirname, mkdir

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "write-file",
    "description": "Write content to a file. Optionally compiles or tests the code after writing. Use 'none' for the check action when making multiple edits that are not yet final, to avoid redundant compilation.",
    "args": [
        {
            "name": "filepath",
            "description": "The path to the file to write",
            "type": "string",
            "backing_type": "string",
            "arity": "single",
            "mode": "positional"
        },
        {
            "name": "content",
            "description": "The content to write to the file",
            "type": "string",
            "backing_type": "string",
            "arity": "single",
            "mode": "positional"
        },
        {
            "name": "check_action",
            "description": "Action to perform after writing: 'none' (skip), 'compile' (default), or 'test'. Use 'none' when making multiple non-final edits to avoid redundant compilation.",
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
    if [[ -z "$2" ]]; then
        echo "Error: filepath argument is required"
        exit 1
    fi
    if [[ -z "$3" ]]; then
        echo "Error: content argument is required"
        exit 1
    fi

    filepath="$2"
    content="$3"
    # Default to 'compile' if not provided
    check_action="${4:-compile}"

    mkdir -p "$(dirname "$filepath")"
    echo "$content" > "$filepath"
    echo "----"
    echo "File written"

    # Optionally compile or test after writing
    if [[ "$check_action" != "none" ]]; then
        echo "----"
        # Locate the check-code.sh tool relative to this script
        script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
        check_code_tool="${script_dir}/../read/check-code.sh"

        if [[ -x "$check_code_tool" ]]; then
            "$check_code_tool" run "$check_action"
        else
            echo "Warning: check-code tool not found at $check_code_tool"
        fi
    fi

    exit 0
fi

