#!/bin/bash
#
case $1 in
  describe)
    cat <<- EOD
{ "slug": "get_uncommitted_changes"
, "description": "ask git diff for the local uncommitted change, use to make sure your edits went through"
, "args":
  [
  ]
}
EOD
  ;;
  run)
    git diff
  ;;
esac
