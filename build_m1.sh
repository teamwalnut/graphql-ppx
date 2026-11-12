#!/bin/bash
esy b
mkdir -p bin
cp _build/default/src/bin/bin.exe binaries/darwin-arm64/bin.exe
