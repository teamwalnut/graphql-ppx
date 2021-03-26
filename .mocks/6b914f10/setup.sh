#! /bin/sh

set -e
set -u

ARCH="x86_64"
TOOLCHAIN="linux.musl.x86_64"
BASE_TRIPLE="$ARCH-linux-musl"
TARGET_TRIPLE="$ARCH-unknown-linux-musl"

gen_tools () {
  TOOL_NAME="$1"
  HOST="$(which $TOOL_NAME)"
  TARGET="$(which $BASE_TRIPLE-$TOOL_NAME || which $TOOL_NAME)"

  sysroot-gen-tools "$TOOLCHAIN" "$TARGET_TRIPLE" "$TOOL_NAME" "$TARGET" "$HOST"
}

gen_tools ar
gen_tools as
gen_tools gcc
gen_tools g++
gen_tools ld
gen_tools nm
gen_tools objdump
gen_tools ranlib
gen_tools strip

cat > $cur__install/toolchain.cmake <<EOF
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR $ARCH)

set(CMAKE_AR $TARGET_TRIPLE-ar)
set(CMAKE_RANLIB $TARGET_TRIPLE-ranlib)
set(CMAKE_C_COMPILER $TARGET_TRIPLE-gcc)
set(CMAKE_CXX_COMPILER $TARGET_TRIPLE-g++)
EOF
