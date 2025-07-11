#!/bin/bash


if [ "$1" == "describe" ]; then
  echo '{ "slug": "news-list", "description": "maintains a list of news sites", "args": [ { "name": "action", "description": "action to perform: add or list", "type": "string", "backing_type": "string", "arity": "single", "mode": "positional" }, { "name": "site", "description": "the URL of the news site to add (if action is 'add')", "type": "url", "backing_type": "string", "arity": "single", "mode": "positional" } ] }'
elif [ "$1" == "run" ]; then
  ACTION=$2
  SITE=$3

  if [ "$ACTION" == "add" ]; then
    if [ -z "$SITE" ]; then
      echo "Please provide a site URL."
      exit 1
    fi
    echo "$SITE" >> news_sites.txt
    echo "Added: $SITE"
  elif [ "$ACTION" == "list" ]; then
    echo "List of news sites:";
    cat news_sites.txt;
  else
    echo "Invalid action. Use 'add' or 'list'."
    exit 1
  fi
else
  echo "Invalid command. Use 'describe' or 'run'."
  exit 1
fi

