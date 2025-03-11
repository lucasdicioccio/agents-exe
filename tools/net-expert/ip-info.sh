#!/bin/bash

case $1 in
  describe)
    cat <<- EOD
{ "slug": "bin-ip"
, "description": "runs /bin/ip to get network info about something"
, "args":
  [ { "name": "ip-or-link"
    , "description": "whether to show ip or link configurations"
    , "type": "command-arg"
    , "backing_type": "string"
    , "arity": "single"
    , "mode": "positional"
    }
  ]
}
EOD
  ;;

  run)
    ip $2 show
  ;;
esac
#!/bin/bash
