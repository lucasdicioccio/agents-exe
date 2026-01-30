#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "list_hs_files"
, "description": "list haskell files"
, "args":
  [
  ]
}
EOD
  ;;

  run)
    find ./src/ -name '*.hs'
  ;;
esac


