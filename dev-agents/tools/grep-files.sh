#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "grep_hs_files"
, "description": "grep haskell files"
, "args":
  [{ "name": "pattern"
    , "description": "the pattern to grep"
    , "type": "pattern"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    pattern="$2"
    git grep "${pattern}" .
  ;;
esac
