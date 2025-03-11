#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "read-memory"
, "description": "reads a whole memory bank"
, "args":
  [ { "name": "memory-bank"
    , "description": "the name of the memory bank"
    , "type": "txt"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    memo=$2
    path="memo-${memo}"
    if [ -f "${path}" ]; then
      cat "${path}"
    else
      echo "error: memory bank was not found"
    fi
  ;;
esac

