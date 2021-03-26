#! /bin/bash

set -e
set -u
set -x

TASKS="$(nproc || echo '8')"

## Parameters
OCAML_HOST=$(dirname $(which ocamlrun))/../
ROOTDIR=$PWD

## Advanced
TARGET_BINDIR="$OCAML_HOST/bin"

CAMLRUN="$OCAML_HOST/bin/ocamlrun"
CAMLLEX="$OCAML_HOST/bin/ocamllex"
CAMLYACC="$OCAML_HOST/bin/ocamlyacc"
CAMLDOC="$OCAML_HOST/bin/ocamldoc"
CAMLDEP="$OCAML_HOST/bin/ocamlc -depend"

HOST_MAKEFILE_CONFIG="$OCAML_HOST/lib/ocaml/Makefile.config"
HOST_CAMLC="$OCAML_HOST/bin/ocamlc"
HOST_CAMLOPT="$OCAML_HOST/bin/ocamlopt"

TARGET_CAMLC="$ROOTDIR/ocamlc.opt"
TARGET_CAMLOPT="$ROOTDIR/ocamlopt.opt"

HOST_STATIC_LIBS="-I $OCAML_HOST/lib/ocaml"
DYNAMIC_LIBS="-I $OCAML_HOST/lib/ocaml/stublibs"

TARGET_STATIC_LIBS="-I $ROOTDIR/stdlib -I $ROOTDIR/otherlibs/unix"

write_wrapper () {
  TARGET="$1"
  RUN="$2"

  ARGS='for ARG in "$@"; do NEW_ARGS="$NEW_ARGS \"$ARG\""; done'

  if [[ "$(uname)" != 'Linux' ]]; then
    ARGS='
for ARG in "$@"
do
  if (
    [ "$ARG" != "-function-sections" ]
  ); then
    NEW_ARGS="$NEW_ARGS \"$ARG\""
  fi
done'
  fi

cat > $TARGET <<EOF
#! /bin/sh

NEW_ARGS=""
$ARGS

eval "$RUN \$NEW_ARGS"  
EOF

  chmod +x $TARGET
}

make_caml () {
  CAMLC_FLAGS="-nostdlib $STATIC_LIBS $DYNAMIC_LIBS"
  CAMLOPT_FLAGS="-nostdlib $STATIC_LIBS"

  CAMLC="$CAMLC $CAMLC_FLAGS"
  CAMLOPT="$CAMLOPT $CAMLOPT_FLAGS"

  write_wrapper "$ROOTDIR/ocamlc.wrapper" "$CAMLC"
  write_wrapper "$ROOTDIR/ocamlopt.wrapper" "$CAMLOPT"

  CAMLC="$ROOTDIR/ocamlc.wrapper"
  CAMLOPT="$ROOTDIR/ocamlopt.wrapper"

  make -j$TASKS \
    TARGET_BINDIR="$TARGET_BINDIR" \
    \
    CAMLRUN="$CAMLRUN" \
    CAMLC="$CAMLC" \
    CAMLOPT="$CAMLOPT" \
    CAMLLEX="$CAMLLEX" \
    CAMLYACC="$CAMLYACC" \
    \
    COMPILER="" \
    OPTCOMPILER="" \
    CAMLDEP="$CAMLDEP" \
    \
    OCAMLRUN="$CAMLRUN" \
    OCAMLC="$CAMLC" \
    OCAMLOPT="$CAMLOPT" \
    OCAMLLEX="$CAMLLEX" \
    OCAMLYACC="$CAMLYACC" \
    \
    OCAMLDOC_RUN="$CAMLDOC" \
    $@
}

get_host_variable () {
  cat $HOST_MAKEFILE_CONFIG | grep "$1=" | awk -F '=' '{print $2}'
}

make_host () {
  STATIC_LIBS="$HOST_STATIC_LIBS"
  CAMLC=$HOST_CAMLC
  CAMLOPT=$HOST_CAMLOPT

  NATDYNLINK=$(get_host_variable "NATDYNLINK")
  NATDYNLINKOPTS=$(get_host_variable "NATDYNLINKOPTS")

  make_caml \
    NATDYNLINK="$NATDYNLINK" \
    NATDYNLINKOPTS="$NATDYNLINKOPTS" \
    $@
}

make_target () {
  STATIC_LIBS="$TARGET_STATIC_LIBS"
  CAMLC=$TARGET_CAMLC
  CAMLOPT=$TARGET_CAMLOPT
  make_caml $@
}

## missing ocamldoc, ocamltest, ocamldoc.opt, ocamltest.opt, so copy them?
## mostly extracted from opt.opt on ocaml/Makefile, but no coldstart

## TODO: that clearly should be something else
if [ "$ESY_TOOLCHAIN_SYSTEM" == "linux" ] || [ "$ESY_TOOLCHAIN_SYSTEM" == "freebsd" ]; then
  patch -p1 < static.patch
  echo 'MKEXE_OPT=$(MKEXE) -static' >> Makefile.config
fi

make_host runtime coreall
make_host ocaml
make_host opt-core
make_host ocamlc.opt
make_host ocamlopt.opt
make_host otherlibraries ocamldebugger
make_host ocamllex.opt ocamltoolsopt ocamltoolsopt.opt

rm $(find . | grep -e '\.cm.$')
make_target -C stdlib all allopt
make_target ocaml ocamlc ocamlopt
make_target otherlibraries otherlibrariesopt ocamltoolsopt
make_target \
  driver/main.cmx \
  driver/optmain.cmx \
  compilerlibs/ocamlcommon.cmxa \
  compilerlibs/ocamlbytecomp.cmxa \
  compilerlibs/ocamloptcomp.cmxa

## Install

cp $CAMLRUN runtime/ocamlrun
make_host install
make_host -C debugger install
