#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "write_note"
, "description": "writes a note file"
, "args":
  [{ "name": "filename"
    , "description": "the filename (inspired by the content) to write"
    , "type": "string"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ,{ "name": "contents"
    , "description": "the contents to write"
    , "type": "text"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "stdin"
    }
  ]
}
EOD
  ;;

  run)
    mkdir -p "./notes"
    path="./notes/$2"
    cat /dev/stdin > ${path}
    echo "ok"
  ;;
esac

