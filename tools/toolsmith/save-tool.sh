#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "save-file"
, "description": "saves a file"
, "args":
  [ { "name": "tooldir"
    , "description": "the tool directory"
    , "type": "dirname"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  , { "name": "filename"
    , "description": "the filename (no path allowed)"
    , "type": "filename"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  , { "name": "contents"
    , "description": "the file contents"
    , "type": "string"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    tooldir=$2
    toolname=$3
    content=$4
    path="${tooldir}/${toolname}"
    dir="./tools/${tooldir}"
    mkdir -p "${dir}"
    echo "${content}" > "./tools/${path}"
    echo "content has been stored ${path}"
  ;;
esac

