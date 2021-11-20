#!/usr/bin/env bash

TEST_NUMBER="${1:-01}"
TEST_NAME="test/mamarracho/test$TEST_NUMBER"

# build
cabal build --verbose=silent

# run
cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --ast > "$TEST_NAME.ast"
cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --mam > "$TEST_NAME.mam"
./mamarracho/mam "$TEST_NAME.mam" > "$TEST_NAME.output"

# print
cat "$TEST_NAME.flecha"
echo ----- ----- -----
cat "$TEST_NAME.ast"
echo ----- ----- -----
cat "$TEST_NAME.mam"
echo ----- ----- -----
cat "$TEST_NAME.output"

# clean
rm -f "$TEST_NAME.ast"
rm -f "$TEST_NAME.mam"
rm -f "$TEST_NAME.output"
