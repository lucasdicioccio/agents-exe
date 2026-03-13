#!/usr/bin/env bash

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "list-files",
    "description": "List all files in the project",
    "args": []
}
EOF
    exit 0
fi

if [[ "$1" == "run" ]]; then
    echo "----"
    find ./app/ -name '*.hs'
    find ./src/ -name '*.hs'
    find ./test/ -name '*.hs'
    find ./agq/ -name '*.hs'
    find ./ -name '*.cabal'
    find ./app/ -name '*.md'
    find ./src/ -name '*.md'
    find ./test/ -name '*.md'
    find ./agq/ -name '*.md'
    exit 0
fi

