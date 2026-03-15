#!/bin/bash
set -euo pipefail

# Code Analysis Script
# Usage: analyze-changes.sh <file_path>

if [ "$#" -eq 1 ] && [ "$1" == "describe" ]; then
    echo '{
        "slug": "analyze-changes",
        "description": "Analyze code changes and provide metrics",
        "args": [
            {"name": "file_path", "description": "Path to file to analyze", "type": "string", "required": true}
        ]
    }'
    exit 0
fi

if [ "$#" -lt 1 ]; then
    echo "Usage: $0 <file_path>" >&2
    exit 1
fi

file_path="$1"

if [ ! -f "$file_path" ]; then
    echo "Error: File not found: $file_path" >&2
    exit 1
fi

# Basic analysis
lines=$(wc -l < "$file_path")
echo "Analysis of: $file_path"
echo "Lines: $lines"
echo "File type: ${file_path##*.}"

# Try to detect language and show relevant info
case "${file_path##*.}" in
    hs)
        echo "Language: Haskell"
        grep -c "^import " "$file_path" 2>/dev/null && echo "Imports: $(grep -c "^import " "$file_path")" || true
        ;;
    py)
        echo "Language: Python"
        grep -c "^import\|^from " "$file_path" 2>/dev/null && echo "Imports: $(grep -c "^import\|^from " "$file_path")" || true
        ;;
    js|ts)
        echo "Language: JavaScript/TypeScript"
        ;;
    *)
        echo "Language: Unknown"
        ;;
esac

