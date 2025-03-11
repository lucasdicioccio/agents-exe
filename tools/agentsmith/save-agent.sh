#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "save-file"
, "description": "saves a file"
, "args":
  [ { "name": "filename"
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
    path=$2
    content=$3
    echo "${content}" > "./agents/${path}"
    echo "content has been stored in file ${path}"
    dir="$(echo "${content}" | jq -r ".contents.toolDirectory")"
    mkdir "${dir}"
    echo "creating tool dir: ${dir}"
  ;;
esac


