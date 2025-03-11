#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "get_example_tool"
, "description": "fetch the bash script of an agent"
, "args":
  [ { "name": "tool-name"
    , "description": "the tool name to get (from the listing)"
    , "type": "toolname"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    toolname=$2
    path="./tools/${toolname}"
    if [ -f "${path}" ]; then
      cat "${path}"
    else
      echo "error: tool was not found"
    fi
  ;;
esac

