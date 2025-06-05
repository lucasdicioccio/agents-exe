#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "recent-memory"
, "description": "fetches recent memory of actions for a given agent"
, "args":
  [ { "name": "agent_slug"
    , "description": "the agent slug"
    , "type": "name"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    slug=$2
    curl "http://localhost:3379/agent_conversations?parent_conversation_id=is.null&limit=1&order=created_at.desc&agent_slug=eq.${slug}&pending_query->>type=eq.done" | jq '.[] | {"agent_slug": .agent_slug, "prompt": .llm_history[].prompt, "result": .llm_history[].msg}'
  ;;
esac


