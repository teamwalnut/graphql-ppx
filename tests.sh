#!/bin/bash
# setopt extendedglob

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
ppx_path="./_build/default/src/bin/bin.exe"
bsc_path="./node_modules/rescript/${platform}/bsc.exe"
declare -a configs=('records' 'template' 'apollo' 'native' 'records_schema')

rm -rf snapshot_tests/operations/expected/
rm -rf snapshot_tests/operations/error/expected/


for config in "${configs[@]}"; do
  case $config in
    "records" ) opts="" ;;
    "template") opts="-template-tag-location=gql -schema=graphql_schema.json" ;;
    "apollo"  ) opts="-apollo-mode -schema=graphql_schema.json" ;;
    "native"  ) opts="-native -schema=graphql_schema.json" ;;
    "records_schema" ) opts="-schema=schema.graphql" ;;
    "uncurried" ) opts="-uncurried" ;;
  esac

  mkdir -p snapshot_tests/operations/expected/$config/generate
  mkdir -p snapshot_tests/operations/expected/$config/compile
  mkdir -p snapshot_tests/operations/errors/expected

  for file in snapshot_tests/operations/*.res; do
    $bsc_path -ppx $ppx_path -bs-no-builtin-ppx -reprint-source $file &> $(snapshotGeneratePath $file $config) & maybeWait

    if [[ $config != "native" ]]; then
      $bsc_path -I ./utilities -w -30 -ppx "$ppx_path $opts" $file &> $(snapshotCompilePath $file $config) & maybeWait
    fi
  done
done
for file in snapshot_tests/operations/errors/*.res; do
  $bsc_path -I ./utilities -w -30 -ppx "$ppx_path -schema=graphql_schema.json" $file 2> $(snapshotErrorPath $file $config) 1> /dev/null & maybeWait
done

wait

rm snapshot_tests/operations/*.cm*
rm snapshot_tests/operations/errors/*.cm*

warningYellow='\033[0;33m'
successGreen='\033[0;32m'
reset='\033[0m'

diff=$(git ls-files --modified snapshot_tests/operations/*expected/*.txt)
printf "\033[2K\r"

if [[ $diff = "" ]]; then
  printf "${successGreen}✅ No unstaged tests difference.${reset}\n"
else
  printf "${warningYellow}⚠️ There are unstaged differences in tests! Did you break a test?\n${diff}\n${reset}"
  exit 1
fi
