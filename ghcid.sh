#!/usr/bin/env bash
if which nix; then
    nix-shell --run "ghcid --command='cabal v2-repl'"
    #nix-shell --run "ghcid --command='cabal v2-repl test' --test=Test.main"
    exit 0
elif which stack; then
    ghcid '--command=stack ghci niobiumcoconut'
    exit 0
fi
