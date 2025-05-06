#!/bin/bash

function config () {
        salmon-migrator config migrate \
                --db agents_log \
                --db-owner agents_log_owner \
                --db-passfile ./passes/db.passfile \
                --db-extra-user agents_log_inserter \
                --db-extra-user agents_log_anon
}

cfg=$(config)

case $1 in
config) echo "${cfg}" ;;
pretty) echo "${cfg}" | jq . ;;
dot) echo "${cfg}" | salmon-migrator run dag ;;
run) echo "${cfg}" | sudo "$(which salmon-migrator)" run up;;
*) echo "config|pretty|dot|run"
esac

