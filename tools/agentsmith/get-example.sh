#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "get_example_agent_definition"
, "description": "fetch the json definition of an agent"
, "args":
  [ { "name": "agent-definition-name"
    , "description": "the agent definition name to get (from the listing)"
    , "type": "agent_definitionname"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    agent_definitionname=$2
    path="./agents/${agent_definitionname}"
    if [ -f "${path}" ]; then
      cat "${path}"
    else
      echo "error: agent_definition was not found"
    fi
  ;;
esac


