module Main (main) where

import Lexer (lexer)
import Parser (flecha)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= parse >>= print . flecha . lexer

evaluate = print . flecha . lexer

parse [] = getContents
parse fs = concat `fmap` mapM readFile fs
