#!/bin/bash
# Build and copy to bin/ for darwin-arm64 (same as build_and_copy.sh on M1/M2).
esy b
mkdir -p bin
cp _build/default/src/bin/bin.exe bin/graphql-ppx-darwin-arm64.exe
chmod +x bin/graphql-ppx-darwin-arm64.exe
