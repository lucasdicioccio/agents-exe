#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "read_hs_file"
, "description": "read a haskell file"
, "args":
  [{ "name": "file"
    , "description": "the file to read"
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
    path="./$2"
    cat "${path}"
  ;;
esac


