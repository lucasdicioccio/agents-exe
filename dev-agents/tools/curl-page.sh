#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "curl_web_page"
, "description": "reads a web page"
, "args":
  [{ "name": "url"
    , "description": "the URL to get"
    , "type": "url"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    url="$2"
    curl "${url}" | pandoc -f html -t commonmark
  ;;
esac


