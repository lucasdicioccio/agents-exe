#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "dig"
, "description": "resolves DNS hostnames"
, "args":
  [ { "name": "hostname"
    , "description": "the target hostname to resolve"
    , "type": "hostname"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    dig $2
  ;;
esac
