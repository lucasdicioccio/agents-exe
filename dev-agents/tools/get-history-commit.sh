#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "get_git_commit"
, "description": "read a git commit by it's short name or long sha"
, "args":
  [{ "name": "git-ref"
    , "description": "the commit ref (short name or long sha) to read"
    , "type": "git-ref"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    sha="$2"
    git show "${sha}" .
  ;;
esac

