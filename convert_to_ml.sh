#!/bin/bash
echo FILE: $1
FILE=$1
NO_EXT="${FILE%.*}"
esy x refmt --parse re -p ml $FILE > $NO_EXT.mlx && rm $FILE &&mv $NO_EXT.mlx $NO_EXT.ml
