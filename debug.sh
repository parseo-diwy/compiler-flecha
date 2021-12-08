#!/usr/bin/env bash

TEST_NUMBER="${1:-01}"
TEST_FOONES="${2:-false}"
TEST_NAME="test/mamarracho/test$TEST_NUMBER"

# build
cabal build --verbose=silent

# run
cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --ast > "$TEST_NAME.ast"
cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --mam > "$TEST_NAME.mam"
./bin/mamarracho "$TEST_NAME.mam" > "$TEST_NAME.output"

# dump
cat "$TEST_NAME.flecha"
echo "----- ----- ----- ----- -----"
cat "$TEST_NAME.ast"
echo "----- ----- ----- ----- -----"
cat "$TEST_NAME.mam"
echo "----- ----- ----- ----- -----"
cat "$TEST_NAME.output"

if [[ $TEST_FOONES == "--foones" ]]; then
  ./bin/flecha_foones "$TEST_NAME.flecha" > "$TEST_NAME.foones.mam"
  echo "----- ----- ----- ----- -----"
  echo "foones res:"
  ./bin/mam_foones "$TEST_NAME.foones.mam"
  rm -f "$TEST_NAME.foones.mam"
fi

# clean
rm -f "$TEST_NAME.ast"
rm -f "$TEST_NAME.mam"
rm -f "$TEST_NAME.output"
