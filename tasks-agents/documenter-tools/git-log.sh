#!/bin/bash

set -x

case $1 in
  describe)
    cat <<- EOD
{ "slug": "get_git_log"
, "description": "read a online log of the git history"
, "args":
  [
  ]
}
EOD
  ;;

  run)
    echo "---"
    git log --oneline | head -n 20
  ;;
esac

