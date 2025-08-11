#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "list_known_tools"
, "description": "lists example tools"
, "args":
  [ ]
}
EOD
  ;;

  run)
    find ./tools -name "*.sh" | sed 's/^.\/tools\///'
  ;;
esac

