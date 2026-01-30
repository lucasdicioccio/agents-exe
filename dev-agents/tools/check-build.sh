#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "build_hs_code"
, "description": "builds (typecheck) the haskell code"
, "args":
  [
  ]
}
EOD
  ;;

  run)
    cabal build
    echo "done"
  ;;
esac



