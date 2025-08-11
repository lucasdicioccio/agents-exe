#!/bin/bash

# Fetches news from provided websites

if [[ "$1" == "describe" ]]; then
  echo '{ "slug": "fetch-news",
          "description": "fetches news articles from specified websites",
          "args": [
            { "name": "urls",
              "description": "A comma-separated list of URLs to fetch news from",
              "type": "url",
              "backing_type": "string",
              "arity": "single",
              "mode": "positional"
            }
          ]
        }'
  exit 0
fi

if [[ "$1" == "run" ]]; then
  IFS=',' read -ra ADDR <<< "$2"
  for url in "${ADDR[@]}"; do
    echo "Fetching news from: $url"
    # Replace with actual news fetching logic here
    # For now, we can simulate this with curl or wget
    curl -s "$url" | pandoc --from html --to markdown | grep -v '^:::' | grep -v 'data:image'
  done
fi

