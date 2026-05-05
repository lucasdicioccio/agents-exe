#!/usr/bin/env bash

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "read-file",
    "description": "Read the contents of a file. Returns a special marker '(empty file)' when the file is empty.",
    "args": [
        {
            "name": "filepath",
            "description": "The path to the file to read",
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
    if [[ ! -f "$2" ]]; then
        echo "Error: file not found: $2"
        exit 1
    fi
    # Output content or special marker for empty files
    if [[ -s "$2" ]]; then
        cat "$2"
    else
        echo "(empty file)"
    fi
    exit 0
fi

