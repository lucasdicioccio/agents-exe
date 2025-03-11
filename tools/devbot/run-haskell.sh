#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "run-haskell"
, "description": "compile and runs a previously saved haskell file"
, "args":
  [ { "name": "filename"
    , "description": "the filename (no path allowed)"
    , "type": "filename"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    file=$2
    path="file-${file}"
    runhaskell "${path}" 2>&1
  ;;
esac
