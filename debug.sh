#!/usr/bin/env bash

TEST_NUMBER="${1:-01}"
TEST_FOONES="${2:-false}"
TEST_NAME="test/mamarracho/test$TEST_NUMBER"

# build
cabal build --verbose=silent

# run
# cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --ast > "$TEST_NAME.ast"
cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --mam > "$TEST_NAME.mam"
./bin/mamarracho "$TEST_NAME.mam" > "$TEST_NAME.output"

# dump
cat "$TEST_NAME.flecha"

echo "-----  diwy -----"
cat "$TEST_NAME.output"
echo "----- /diwy -----"

if [[ $TEST_FOONES == "--foones" ]]; then
  ./bin/flecha_foones "$TEST_NAME.flecha" > "$TEST_NAME.foones.mam"
  echo "-----  foones -----"
  ./bin/mam_foones "$TEST_NAME.foones.mam"
  echo "----- /foones -----"
  rm -f "$TEST_NAME.foones.mam"
fi

# clean
rm -f "$TEST_NAME.ast"
rm -f "$TEST_NAME.mam"
rm -f "$TEST_NAME.output"
