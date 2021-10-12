#!/usr/bin/env bash

# first build cabal
cabal build --verbose=silent

function test() {
  SUITE=$1
  NAME=$2
  DIR="test/$SUITE"
  cabal run --verbose=silent flecha -- "$DIR/$NAME.flecha" > "$DIR/$NAME.tmp"
  pushd "$DIR" &> /dev/null|| exit 0
  jq . "$NAME.tmp" > "$NAME.test"
  jq . "$NAME.json" > "$NAME.expected"
  TEST_COMMAND="diff '$NAME.expected' '$NAME.test' > '$NAME.diff'"
  if eval "$TEST_COMMAND"; then
    echo "OK $SUITE::$NAME"
  else
    echo "FAIL $SUITE::$NAME"
    cat "$NAME.diff"
  fi
  rm "$NAME.tmp" "$NAME.test" "$NAME.expected" "$NAME.diff"
  popd &> /dev/null || exit 0
}

test diwy "test00"

# test foones "test00"
