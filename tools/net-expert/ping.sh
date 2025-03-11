#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "ping"
, "description": "performs ICMP pings to a given IPv4 or DNS-based host"
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
    ping -c 3 $2
  ;;
esac
