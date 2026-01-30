#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "write_hs_file"
, "description": "writes a haskell file"
, "args":
  [{ "name": "file"
    , "description": "the file to write"
    , "type": "file-path"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ,{ "name": "contents"
    , "description": "the contents to write"
    , "type": "hs-code"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "stdin"
    }
  ]
}
EOD
  ;;

  run)
    path="./$2"
    cat /dev/stdin > ${path}
    echo "ok"
  ;;
esac

