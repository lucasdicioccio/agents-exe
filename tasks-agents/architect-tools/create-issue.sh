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
  ,{ "name": "dependencies"
    , "description": "comma-separated list of issue dependencies (e.g., '#42,#43'). If empty, the issue will be labeled 'agents/to-be-taken'. If non-empty, it will be labeled 'agents/wait'."
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
    dependencies="$3"
    scope="root"

    case "${scope}" in
      root)
        ;;
      *)
        echo "Error: scope must be one of 'root'" >&2
        exit 1
        ;;
    esac

    # Determine the agent label based on dependencies
    # If dependencies is empty or whitespace-only -> to-be-taken (ready to work)
    # If dependencies has content -> wait (must wait for dependencies)
    if [[ -z "${dependencies}" ]] || [[ "${dependencies}" =~ ^[[:space:]]*$ ]]; then
      agent_label="agents/to-be-taken"
    else
      agent_label="agents/wait"
    fi

    labels="${agent_label},${scope}"

    # Use --body-file - to read the body from stdin
    gh issue create --title "${title}" --body-file - --label "${labels}"
  ;;
esac

