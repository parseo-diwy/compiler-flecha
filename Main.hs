module Main (main) where

import Ast (Program)
import Json (toJsonProgram)
import Lexer (lexer)
import Parser (flecha)
import System.Environment (getArgs)
import Mamarracho (compile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filename:options) -> do
      file <- readFile filename
      printResult file options
    _ ->
      error "Invalid arguments.\nUsage: cabal run flecha -- example.flecha"

printResult :: String -> [String] -> IO ()
printResult file ("--ast":_)   = printAST  $ parse file
printResult file ("--json":_)  = printJSON $ parse file
printResult file _             = printMAM  $ parse file

parse :: String -> Program
parse = flecha . lexer

printAST :: Program -> IO ()
printAST = print

printJSON :: Program -> IO ()
printJSON = putStrLn . toJsonProgram

printMAM :: Program -> IO ()
printMAM = putStrLn . compile
