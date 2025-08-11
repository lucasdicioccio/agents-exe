#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "ask-user-yes-no"
, "description": "asks a yes/no question to the user"
, "args":
  [ { "name": "message"
    , "description": "the question message to ask the user"
    , "type": "message"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  # todo: use xdialog instead
  run)
    msg=$2
    userChoice=$(notify-send --action=YES=yes --action=NO=no "${msg}")
    if [[ -z "${userChoice}" ]];
    then
      echo "no answer given"
    else
      echo "${userChoice}"
    fi;
  ;;
esac


