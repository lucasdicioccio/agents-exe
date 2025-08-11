#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "get-date"
, "description": "get current date"
, "args":
  [ ]
}
EOD
  ;;

  run)
    date
  ;;
esac


