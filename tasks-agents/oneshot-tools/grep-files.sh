#!/usr/bin/env bash

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "grep-files",
    "description": "Search for a pattern in files using 'git grep'",
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
    echo "---"
    pattern="$2"
    git grep -I "${pattern}" -- . ":!tasks-sessions/*"
    exit 0
fi

