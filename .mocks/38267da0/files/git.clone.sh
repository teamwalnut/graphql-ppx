#! /bin/sh

set -e
set -u

## TODO: make a proper default branch
BRANCH="4.10+ios"
if [ "$ESY_TOOLCHAIN_SYSTEM" == "android" ]; then
  BRANCH="4.10+android"
fi
if [ "$ESY_TOOLCHAIN_SYSTEM" == "ios" ]; then
  BRANCH="4.10+ios"
fi
if [ "$ESY_TOOLCHAIN_SYSTEM" == "macos" ]; then
  BRANCH="4.10+ios"
fi

git clone \
  --single-branch --depth 1 \
  --branch $BRANCH \
  --recursive \
  https://github.com/EduardoRFS/ocaml.git
