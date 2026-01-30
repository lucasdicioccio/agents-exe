#!/bin/bash
#
case $1 in
  describe)
    cat <<- EOD
{ "slug": "read_git_history"
, "description": "get a short description of the git history"
, "args":
  [
  ]
}
EOD
  ;;
  run)
    git log --oneline
  ;;
esac
