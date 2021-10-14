module Main (main) where

import Json (toJsonProgram)
import Lexer (lexer)
import Parser (flecha)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parse >>= evaluate

parse :: [FilePath] -> IO String
parse [] = getContents
parse fs = concat `fmap` mapM readFile fs

evaluate :: String -> IO ()
evaluate = putStrLn . toJsonProgram . flecha . lexer
