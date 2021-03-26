#! /bin/sh

set -e
set -u

TOOLCHAIN=$1
TRIPLE=$2
NAME=$3

TARGET=$4
HOST=$5

FILENAME="$TRIPLE-$NAME"
FILE="$cur__bin/$FILENAME"

touch $FILE
chmod +x $FILE

ln -s $FILE "$cur__bin/$NAME"
chmod +x "$cur__bin/$NAME"

cat > $FILE <<EOF
#! $(which bash)

set -e
set -u
if [ "\${OCAMLFIND_TOOLCHAIN:='native'}" == "$TOOLCHAIN" ] || \
   [ "\$(basename \$0)" == "$FILENAME" ] || \
   [ "default.$TOOLCHAIN" == "\$(basename \${INSIDE_DUNE:='nope'})" ]
then
    eval $TARGET "\\\$@"
else
    eval $HOST "\\\$@"
fi
EOF
