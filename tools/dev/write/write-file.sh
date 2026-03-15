#!/usr/bin/env bash

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "write-file",
    "description": "Write content to a file",
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
    mkdir -p "$(dirname "$2")"
    echo "$3" > "$2"
    echo "----"
    echo "File written"
    exit 0
fi

