#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "run-python3"
, "description": "executes a python3 script, no virtual env is provided"
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
    python3 "${path}" 2>&1
  ;;
esac

