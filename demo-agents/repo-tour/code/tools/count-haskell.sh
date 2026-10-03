#!/usr/bin/env bash
# Counts the tracked Haskell files of the git repository in the current
# directory, and their lines, per top-level directory.
set -euo pipefail

if [ "${1:-}" == "describe" ]; then
    cat <<'JSON'
{
  "slug": "count_haskell",
  "description": "Counts the tracked Haskell (.hs) files and their lines in the git repository of the current directory, per top-level directory, with a total.",
  "args": [],
  "empty-result": { "tag": "AddMessage", "contents": "no Haskell files are tracked here" }
}
JSON
    exit 0
fi

git ls-files '*.hs' | sort | awk -F/ '
  { n = 0; while ((getline line < $0) > 0) n++; close($0)
    if (!($1 in files)) order[++dirs] = $1
    files[$1]++; lines[$1] += n; tf++; tl += n }
  END { for (i = 1; i <= dirs; i++) printf "%s: %d files, %d lines\n", order[i], files[order[i]], lines[order[i]]
        printf "total: %d files, %d lines\n", tf, tl }'
