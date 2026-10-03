#!/usr/bin/env bash
# Lists the markdown pages under documentation/ of the current directory
# with their first heading.
set -euo pipefail

if [ "${1:-}" == "describe" ]; then
    cat <<'JSON'
{
  "slug": "list_docs",
  "description": "Lists the markdown pages in the documentation/ directory of the current directory, each with its title.",
  "args": [],
  "empty-result": { "tag": "AddMessage", "contents": "no documentation/ directory with markdown pages here" }
}
JSON
    exit 0
fi

for f in documentation/*.md; do
    [ -e "$f" ] || continue
    title=$(grep -m1 '^# ' "$f" | sed 's/^# //' || true)
    echo "$f: ${title:-(untitled)}"
done
