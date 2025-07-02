#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "whois"
, "description": "performs WHOIS lookups for IPs"
, "args":
  [ { "name": "target"
    , "description": "the target hostname or ipv4 in string format"
    , "type": "hostname|ip4"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    whois $2
  ;;
esac

