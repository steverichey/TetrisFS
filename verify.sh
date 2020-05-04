#!/usr/bin/env sh

set +euxr

echo "Verifying all F* files..."
fstar.exe *.fst
