#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "nmap"
, "description": "scans a network range with nmap -sS"
, "args":
  [ { "name": "range"
    , "description": "the ip range to scan as per nmap, for instance 192.168.1.1-254"
    , "type": "ip-range-specific"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    nmap -sP $2
  ;;
esac
