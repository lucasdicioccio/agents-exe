#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "read_note"
, "description": "read a note file"
, "args":
  [{ "name": "file"
    , "description": "the file to read (without ./notes/ prefix)"
    , "type": "file-path"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    path="./notes/$2"
    cat "${path}"
  ;;
esac



