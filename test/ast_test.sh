#!/usr/bin/env bats

setup() {
  load 'test_helper/bats-support/load'
  load 'test_helper/bats-assert/load'

  cabal build --verbose=silent
}

teardown() {
    rm -f test/ast/*.expected
    rm -f test/ast/*.output
}

function run_test() {
  TEST_NAME=$1
  jq . "$TEST_NAME.json" > "$TEST_NAME.expected"
  cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --json | jq . > "$TEST_NAME.output"
  run diff "$TEST_NAME.expected" "$TEST_NAME.output"

  assert_success
  assert_output ""
}

# AST Test Suite

@test "ast::test00" {
  run_test "test/ast/test00"
}

@test "ast::test01" {
  run_test "test/ast/test01"
}

@test "ast::test02" {
  run_test "test/ast/test02"
}

@test "ast::test03" {
  run_test "test/ast/test03"
}

@test "ast::test04" {
  run_test "test/ast/test04"
}

@test "ast::test05" {
  run_test "test/ast/test05"
}

@test "ast::test06" {
  run_test "test/ast/test06"
}

@test "ast::test07" {
  run_test "test/ast/test07"
}

@test "ast::test08" {
  run_test "test/ast/test08"
}

@test "ast::test09" {
  run_test "test/ast/test09"
}

@test "ast::test10" {
  run_test "test/ast/test10"
}

@test "ast::test11" {
  run_test "test/ast/test11"
}

@test "ast::test12" {
  run_test "test/ast/test12"
}

@test "ast::test13" {
  run_test "test/ast/test13"
}

@test "ast::test14" {
  run_test "test/ast/test14"
}

@test "ast::test15" {
  run_test "test/ast/test15"
}

@test "ast::test16" {
  run_test "test/ast/test16"
}

@test "ast::test17" {
  run_test "test/ast/test17"
}

@test "ast::test18" {
  run_test "test/ast/test18"
}

@test "ast::test20" {
  run_test "test/ast/test20"
}
