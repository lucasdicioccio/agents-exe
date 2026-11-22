#!/usr/bin/env bash

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "grep-files",
    "description": "Search for a pattern in tracked files using 'git grep'. Output is 'path:line:text'; long lines are cut to 300 characters and at most 200 matches are shown. The pattern is a single basic regex (use \\| for alternatives), not a boolean query.",
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
    max_lines=200
    max_cols=300
    # Generated docs (docs/), data blobs (resources/) and session dumps are
    # mostly huge single-line files: one match there can flood the context.
    matches=$(git grep -n -I -e "${pattern}" -- . \
        ":!manual-sessions/*" ":!tasks-sessions/*" \
        ":!docs/*" ":!resources/*")
    if [[ -z "${matches}" ]]; then
        echo "(no matches)"
        exit 0
    fi
    total=$(printf '%s\n' "${matches}" | wc -l)
    printf '%s\n' "${matches}" | head -n "${max_lines}" | cut -c "1-${max_cols}"
    if (( total > max_lines )); then
        echo "... (${total} matches, showing the first ${max_lines}; refine the pattern)"
    fi
    exit 0
fi

