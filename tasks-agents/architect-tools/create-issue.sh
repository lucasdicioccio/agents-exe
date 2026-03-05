#!/bin/bash

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
    scope="root"
    
    case "${scope}" in
      root)
        ;;
      *)
        echo "Error: scope must be one of 'root'" >&2
        exit 1
        ;;
    esac

    labels="agents/to-be-taken,${scope}"
    
    # Use --body-file - to read the body from stdin
    gh issue create --title "${title}" --body-file - --label "${labels}"
  ;;
esac

