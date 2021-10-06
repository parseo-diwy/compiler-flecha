module Lexer where

import Data.Char

parseError :: [Token] -> a
parseError _ = error "Parse error"
data Exp  
    = Let String Exp Exp
    | Exp1 Exp1
    deriving Show

data Exp1 
    = Plus Exp1 Term 
    | Minus Exp1 Term 
    | Term Term
    deriving Show

data Term 
    = Times Term Factor 
    | Div Term Factor 
    | Factor Factor
    deriving Show

data Factor 
    = Int Int 
    | Var String 
    | Brack Exp
    deriving Show

data Token
    = TokenLet
    | TokenIn
    | TokenInt Int
    | TokenVar String
    | TokenEq
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenDiv
    | TokenOB
    | TokenCB
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
-- lexer (cs) = exitWith (ExitFailure 1)

lexNum cs = TokenInt (read num) : lexer rest
                where (num,rest) = span isDigit cs

lexVar cs =
    case span isAlpha cs of
        ("let",rest) -> TokenLet : lexer rest
        ("in",rest)  -> TokenIn : lexer rest
        (var,rest)   -> TokenVar var : lexer rest
