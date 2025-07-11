#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "curl"
, "description": "performs http and https calls, limited to simple HTTP-GET, returns headers then body"
, "args":
  [ { "name": "url"
    , "description": "the target url to GET"
    , "type": "url"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    curl -D - $2
  ;;
esac
