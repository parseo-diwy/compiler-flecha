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

function mam_test() {
  TEST_NAME=$1
  cabal run --verbose=silent flecha -- "$TEST_NAME.flecha" --mam > "$TEST_NAME.mam"
  ./bin/mamarracho "$TEST_NAME.mam" > "$TEST_NAME.output"
  run diff "$TEST_NAME.expected" "$TEST_NAME.output"

  assert_success
  assert_output ""
}

# Mamarracho Test Suite

@test "mam::test01" {
  mam_test "test/mamarracho/test01"
}

@test "mam::test02" {
  mam_test "test/mamarracho/test02"
}

@test "mam::test03" {
  mam_test "test/mamarracho/test03"
}

@test "mam::test04" {
  mam_test "test/mamarracho/test04"
}

@test "mam::test05" {
  mam_test "test/mamarracho/test05"
}

@test "mam::test06" {
  mam_test "test/mamarracho/test06"
}

@test "mam::test07" {
  mam_test "test/mamarracho/test07"
}

@test "mam::test08" {
  mam_test "test/mamarracho/test08"
}

@test "mam::test09" {
  skip
  mam_test "test/mamarracho/test09"
}

@test "mam::test10" {
  skip
  mam_test "test/mamarracho/test10"
}

@test "mam::test11" {
  skip
  mam_test "test/mamarracho/test11"
}

@test "mam::test12" {
  skip
  mam_test "test/mamarracho/test12"
}

@test "mam::test13" {
  skip
  mam_test "test/mamarracho/test13"
}

@test "mam::test14" {
  skip
  mam_test "test/mamarracho/test14"
}

@test "mam::test15" {
  skip
  mam_test "test/mamarracho/test15"
}

@test "mam::test16" {
  skip
  mam_test "test/mamarracho/test16"
}

@test "mam::test17" {
  skip
  mam_test "test/mamarracho/test17"
}

@test "mam::test18" {
  skip
  mam_test "test/mamarracho/test18"
}

@test "mam::test19" {
  skip
  mam_test "test/mamarracho/test19"
}

@test "mam::test20" {
  skip
  mam_test "test/mamarracho/test20"
}

@test "mam::test21" {
  skip
  mam_test "test/mamarracho/test21"
}

@test "mam::test22" {
  skip
  mam_test "test/mamarracho/test22"
}

@test "mam::test23" {
  skip
  mam_test "test/mamarracho/test23"
}

@test "mam::test24" {
  skip
  mam_test "test/mamarracho/test24"
}

@test "mam::test25" {
  skip
  mam_test "test/mamarracho/test25"
}

@test "mam::test26" {
  skip
  mam_test "test/mamarracho/test26"
}

@test "mam::test27" {
  skip
  mam_test "test/mamarracho/test27"
}

@test "mam::test28" {
  skip
  mam_test "test/mamarracho/test28"
}

@test "mam::test29" {
  skip
  mam_test "test/mamarracho/test29"
}

@test "mam::test30" {
  skip
  mam_test "test/mamarracho/test30"
}

@test "mam::test31" {
  skip
  mam_test "test/mamarracho/test31"
}

@test "mam::test43" {
  mam_test "test/mamarracho/test43"
}

@test "mam::testNumOp" {
  mam_test "test/mamarracho/testNumOp"
}
