#! /bin/bash

set -e
set -u

MUSL_X86_64="https://musl.cc/x86_64-linux-musl-cross.tgz"
MUSL_ARM64="https://musl.cc/aarch64-linux-musl-cross.tgz"

if [[ "$(uname)" == 'Linux' ]]; then
  curl -o $cur__target_dir/musl.x86_64.tgz $MUSL_X86_64
  curl -o $cur__target_dir/musl.arm64.tgz $MUSL_ARM64

  cd $cur__install
  tar xvp --strip-components=1 -f $cur__target_dir/musl.x86_64.tgz
  tar xvp --strip-components=1 -f $cur__target_dir/musl.arm64.tgz
elif [[ "$(uname)" == 'Darwin' ]]; then
## TODO: properly handle that in the future
  echo "noop"
else
  echo "host not supported"
  exit 1
fi
