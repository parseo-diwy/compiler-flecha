module Main (main) where

import Test.HUnit ( assertEqual, Test(TestCase, TestList), runTestTT, test, Counts (errors, failures) )
import System.Exit (exitSuccess, exitFailure)
import Lexer ( lexer )

tests :: Test
tests = test [test1]

test1 :: Test
test1 = TestCase $ assertEqual "Nombre del test" 1 1

test2 :: Test
test2 = TestCase $ assertEqual "Comment ignore" [] (lexer "-- comentario")

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure

