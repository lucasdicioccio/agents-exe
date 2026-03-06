#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "notify-send"
, "description": "notify the user with some message on the desktop"
, "args":
  [ { "name": "message"
    , "description": "the message to put in the notification"
    , "type": "message"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    msg=$2
    notify-send "${msg}"
    echo "done"
  ;;
esac

