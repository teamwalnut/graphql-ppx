#!/bin/sh
file="$1"
shift 1
refmt --parse re --print binary $file | graphql_ppx -schema ../graphql_schema.json $@ /dev/stdin /dev/stdout | refmt --parse binary --print re
