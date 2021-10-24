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

# Diwy Test Suite

@test "diwy::test00" {
  run_test "test/diwy/test00"
}

# # Foones Test Suite

@test "foones::test00" {
  run_test "test/foones/test00"
}

@test "foones::test01" {
  run_test "test/foones/test01"
}

@test "foones::test02" {
  run_test "test/foones/test02"
}

@test "foones::test03" {
  run_test "test/foones/test03"
}

@test "foones::test04" {
  run_test "test/foones/test04"
}

@test "foones::test05" {
  run_test "test/foones/test05"
}

@test "foones::test06" {
  run_test "test/foones/test06"
}

@test "foones::test07" {
  run_test "test/foones/test07"
}

@test "foones::test08" {
  run_test "test/foones/test08"
}

@test "foones::test09" {
  run_test "test/foones/test09"
}

@test "foones::test10" {
  run_test "test/foones/test10"
}

@test "foones::test11" {
  run_test "test/foones/test11"
}

@test "foones::test12" {
  run_test "test/foones/test12"
}

@test "foones::test13" {
  run_test "test/foones/test13"
}

@test "foones::test14" {
  run_test "test/foones/test14"
}

@test "foones::test15" {
  run_test "test/foones/test15"
}

@test "foones::test16" {
  run_test "test/foones/test16"
}

@test "foones::test17" {
  run_test "test/foones/test17"
}

@test "foones::test18" {
  run_test "test/foones/test18"
}
