#!/bin/bash

set -x

case $1 in
  describe)
    cat <<- EOD
{ "slug": "generage_graphviz"
, "description": "generate a graphviz image from a dot script, the resulting png is stored under '<path>[.png]'"
, "args":
  [{ "name": "dot-path"
    , "description": "the filepath to the dot file"
    , "type": "filepath"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  , { "name": "engine"
    , "description": "the graphviz engine to use among 'dot' or 'neato'"
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
    echo "---"
    path="$2"
    engine="${3:-dot}"
    if [[ "$engine" != "dot" && "$engine" != "neato" ]]; then
      echo "Error: Invalid engine '$engine'. Must be 'dot' or 'neato'." >&2
      exit 1
    fi
    "${engine}" -Tpng "${path}" > "${path}.png"
  ;;
esac
