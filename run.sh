#!/usr/bin/env bash

MAM_ARG="${2:---mam}"
TEST_NUMBER="${1:-01}"
TEST_NAME="test/mamarracho/test$TEST_NUMBER"

# run
cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" "$MAM_ARG" > "$TEST_NAME.mam"
./mamarracho/mam "$TEST_NAME.mam" > "$TEST_NAME.output"

cat "$TEST_NAME.flecha"
echo ----- ----- -----
cat "$TEST_NAME.mam"
echo ----- ----- -----
cat "$TEST_NAME.output"

# clean
rm -f "$TEST_NAME.mam"
rm -f "$TEST_NAME.output"
