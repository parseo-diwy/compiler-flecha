#!/usr/bin/env bash

TEST_NUMBER="${1:-01}"
TEST_NAME="test/mamarracho/test$TEST_NUMBER"

# run
cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --mam > "$TEST_NAME.mam"
./mamarracho/mam "$TEST_NAME.mam" > "$TEST_NAME.output"

cat "$TEST_NAME.flecha"
echo ----- ----- -----
cat "$TEST_NAME.mam"
echo ----- ----- -----
cat "$TEST_NAME.output"

# clean
rm -f "$TEST_NAME.mam"
rm -f "$TEST_NAME.output"
