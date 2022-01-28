#!/bin/zsh

setopt extendedglob

function snapshotGeneratePath {
  echo "$(dirname $1)/expected/$2/generate/$(basename $1).txt"
}

function snapshotCompilePath {
  echo "$(dirname $1)/expected/$2/compile/$(basename $1).txt"
}

function snapshotErrorPath {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

taskCount=0
function maybeWait {
  let taskCount+=1
  # spawn in batch of 20 processes
  [[ $((taskCount % 20)) = 0 ]] && printf "." && wait
}

case $(uname) in
  Darwin) platform=darwin;;
  Linux) platform=linux;;
  Windows) platform=win32;;
  *)
    echo "Unknown OS: $uname"
    exit 1
esac

refmt_path="./node_modules/rescript/${platform}/refmt.exe"
ppx_path="./_build/default/src/bucklescript_bin/bin.exe"
bsb_path="./node_modules/rescript/${platform}/bsc.exe"
configs=('records' 'template' 'apollo' 'native')

rm -rf tests_bucklescript/operations/expected/
rm -rf tests_bucklescript/operations/error/expected/

for config in $configs; do
  case $config in
    "records" ) opts="" ;;
    "template") opts="-template-tag-location=gql" ;;
    "apollo"  ) opts="-apollo-mode" ;;
    "native"  ) opts="-native" ;;
  esac

  mkdir -p tests_bucklescript/operations/expected/$config/generate
  mkdir -p tests_bucklescript/operations/expected/$config/compile
  mkdir -p tests_bucklescript/operations/errors/expected

  for file in tests_bucklescript/operations/*.re; do
    $refmt_path --parse=re --print=binary $file | $ppx_path -schema=graphql_schema.json $opts /dev/stdin /dev/stdout | $refmt_path --parse=binary &> $(snapshotGeneratePath $file $config) & maybeWait

    if [[ $config != "native" ]]; then
      $bsb_path -I ./utilities -w -30 -ppx "$ppx_path -schema=graphql_schema.json $opts" $file &> $(snapshotCompilePath $file $config) & maybeWait
    fi
  done
  for file in tests_bucklescript/operations/errors/*.re; do
    $bsb_path -I ./utilities -w -30 -ppx "$ppx_path -schema=graphql_schema.json $opts" $file 2> $(snapshotErrorPath $file $config) 1> /dev/null & maybeWait
  done
done

wait

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified tests_bucklescript/operations/**/expected)
printf "\033[2K\r"
if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests! Did you break a test?\n${diff}\n${reset}"
  exit 1
fi
