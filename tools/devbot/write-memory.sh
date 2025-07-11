#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "write-memory"
, "description": "store a line of text to a memory bank and says 'done'"
, "args":
  [ { "name": "memory-bank"
    , "description": "the name of the memory bank"
    , "type": "txt"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  , { "name": "line"
    , "description": "the line of text"
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
    line=$3
    echo "${line}" >> "memo-${memo}"
    echo "done"
  ;;
esac
