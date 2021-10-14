#!/usr/bin/env bats

setup() {
  load 'test_helper/bats-support/load'
  load 'test_helper/bats-assert/load'

  cabal build --verbose=silent
}

teardown() {
    rm -f test/diwy/*.expected
    rm -f test/diwy/*.output
    rm -f test/foones/*.expected
    rm -f test/foones/*.output
}

function run_test() {
  TEST_NAME=$1
  jq . "$TEST_NAME.json" > "$TEST_NAME.expected"
  cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" | jq . > "$TEST_NAME.output"
  run diff "$TEST_NAME.expected" "$TEST_NAME.output"

  assert_success
  assert_output ""
}

@test "diwy::test00" {
  run_test "test/diwy/test00"
}

@test "foones::test00" {
  run_test "test/foones/test00"
}

