#!/usr/bin/env bash

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "grep-files",
    "description": "Search for a pattern in files",
    "args": [
        {
            "name": "pattern",
            "description": "The pattern to search for",
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
        echo "Error: pattern argument is required"
        exit 1
    fi
    echo "----"
    grep -r "$2" . 2>/dev/null || echo "No matches found for pattern: $2"
    exit 0
fi

