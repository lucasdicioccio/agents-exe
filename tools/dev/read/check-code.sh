#!/usr/bin/env bash

if [[ "$1" == "describe" ]]; then
    cat << 'EOF'
{
    "slug": "check-code",
    "description": "Check Haskell code by compiling or testing",
    "args": [
        {
            "name": "action",
            "description": "Either 'compile' to compile the code or 'test' to run tests",
            "type": "string",
            "backing_type": "string",
            "arity": "single",
            "mode": "positional"
        }
    ]
}
EOF
    exit 0
fi

if [[ "$1" == "run" ]]; then
    if [[ -z "$2" ]]; then
        echo "Error: action argument is required (compile|test)"
        exit 1
    fi
    
    action="$2"
    echo "----"
    
    if [[ "$action" == "compile" ]]; then
        echo "Compiling Haskell code..."
        echo "Using cabal"
        cabal build 2>&1
    elif [[ "$action" == "test" ]]; then
        echo "Running Haskell tests..."
        timeout 1h cabal test 2>&1
        if [[ $? -eq 124 ]]; then
            echo "NOTE: tests aborted for timeout (1hour)"
        fi
    else
        echo "Error: action must be either 'compile' or 'test'"
        exit 1
    fi
    
    exit 0
fi

