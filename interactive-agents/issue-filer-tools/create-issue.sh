#!/bin/bash
# Required tools: gh (GitHub CLI)
# This script creates GitHub issues with appropriate labels based on dependencies.
# Issues with no dependencies are labeled 'agents/to-be-taken' (ready to work on).
# Issues with dependencies are labeled 'agents/wait' (must wait for dependencies).

case $1 in
  describe)
    cat <<- EOD
{ "slug": "create_github_issue"
, "description": "creates a github issue using the 'gh' CLI"
, "args":
  [{ "name": "title"
    , "description": "the title of the issue"
    , "type": "string"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ,{ "name": "needs_arbitrage"
    , "description": "Set to 'true' if this is the Issue needs arbitrage."
    , "type": "string"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ,{ "name": "body"
    , "description": "the detailed description/body of the issue"
    , "type": "text"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "stdin"
    }
  ]
}
EOD
  ;;

  run)
    title="$2"
    needs_arbitrage="$3"
    scope="root"

    case "${scope}" in
      root)
        ;;
      *)
        echo "Error: scope must be one of 'root'" >&2
        exit 1
        ;;
    esac

    # Determine the agent label based on arbitrage
    # FIX: not equal the string 'true'
    if [[ "${needs_arbitrage}" != "true" ]]; then
      agent_label="agq/to-be-taken"
    else
      agent_label=""
    fi

    labels="${scope}"
    if [[ -n "${agent_label}" ]]; then
      labels="${labels},${agent_label}"
    fi

    # Use --body-file - to read the body from stdin
    # Prepend metadata to the body
    (
        echo "---"
        cat
    ) | gh issue create --title "${title}" --body-file - --label "${labels}"
  ;;
esac

