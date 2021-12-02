#!/usr/bin/env bats

setup() {
  load 'test_helper/bats-support/load'
  load 'test_helper/bats-assert/load'

  ## build flecha
  cabal build --verbose=silent
}

teardown() {
    rm -f test/ast/*.expected
    rm -f test/ast/*.output
    rm -f test/mamarracho/*.mam
    rm -f test/mamarracho/*.output
}

function ast_test() {
  TEST_NAME=$1
  jq . "$TEST_NAME.json" > "$TEST_NAME.expected"
  cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --json | jq . > "$TEST_NAME.output"
  run diff "$TEST_NAME.expected" "$TEST_NAME.output"

  assert_success
  assert_output ""
}

# AST Test Suite

@test "ast::test00" {
  ast_test "test/ast/test00"
}

@test "ast::test01" {
  ast_test "test/ast/test01"
}

@test "ast::test02" {
  ast_test "test/ast/test02"
}

@test "ast::test03" {
  ast_test "test/ast/test03"
}

@test "ast::test04" {
  ast_test "test/ast/test04"
}

@test "ast::test05" {
  ast_test "test/ast/test05"
}

@test "ast::test06" {
  ast_test "test/ast/test06"
}

@test "ast::test07" {
  ast_test "test/ast/test07"
}

@test "ast::test08" {
  ast_test "test/ast/test08"
}

@test "ast::test09" {
  ast_test "test/ast/test09"
}

@test "ast::test10" {
  ast_test "test/ast/test10"
}

@test "ast::test11" {
  ast_test "test/ast/test11"
}

@test "ast::test12" {
  ast_test "test/ast/test12"
}

@test "ast::test13" {
  ast_test "test/ast/test13"
}

@test "ast::test14" {
  ast_test "test/ast/test14"
}

@test "ast::test15" {
  ast_test "test/ast/test15"
}

@test "ast::test16" {
  ast_test "test/ast/test16"
}

@test "ast::test17" {
  ast_test "test/ast/test17"
}

@test "ast::test18" {
  ast_test "test/ast/test18"
}

@test "ast::test20" {
  ast_test "test/ast/test20"
}
