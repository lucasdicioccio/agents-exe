#!/usr/bin/env bash
# Prints the size of the git history of the current directory and its latest
# commits.
set -euo pipefail

if [ "${1:-}" == "describe" ]; then
    cat <<'JSON'
{
  "slug": "recent_commits",
  "description": "Prints how many commits the git repository of the current directory has, and the date and subject of the eight most recent non-merge commits.",
  "args": [],
  "empty-result": { "tag": "AddMessage", "contents": "no commits found" }
}
JSON
    exit 0
fi

echo "commits in history: $(git rev-list --count HEAD)"
echo "latest non-merge commits:"
git log --no-merges -n 8 --date=short --format='%ad %s'
