#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "list_known_agent_definitions"
, "description": "list example agent definitions"
, "args":
  [ ]
}
EOD
  ;;

  run)
    find ./tools -name "*.json" | sed 's/^.\/tools\///'
  ;;
esac


