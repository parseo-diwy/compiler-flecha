module Main (main) where

import Test.HUnit (assertEqual, Test(..), runTestTT, test, Counts(..))
import System.Exit (exitSuccess, exitFailure)
import Lexer ( lexer )

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure

assertEq :: (Eq a, Show a) => String -> (a, a) -> Test
assertEq name (expected, got) = TestCase $ assertEqual name expected got

tests :: Test
tests = test [
  run "Demo",
  run "Parser :: Ignore comment"
  ]

run :: String -> Test
run name = case name of
  "Demo"
    -> assertEq name (1, 1)
  
  "Parser :: Ignore comment"
    -> assertEq name ([], lexer "-- comentario")

