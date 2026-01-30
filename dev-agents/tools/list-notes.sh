#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "list_notes"
, "description": "list notes"
, "args":
  [
  ]
}
EOD
  ;;

  run)
    find ./notes -type f
  ;;
esac



