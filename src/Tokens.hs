module Tokens where 


data Token = TokenDef 
           | TokenLowerId String
           | TokenUpperId String
           | TokenInt Integer
           | TokenChar Char
           | TokenString String
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenElif
           | TokenCase
           | TokenLet
           | TokenIn
           | TokenDefEq 
           | TokenSemicolon
           | TokenLParen
           | TokenRParen
           | TokenLambda
           | TokenPipe 
           | TokenArrow 
           | TokenAnd
           | TokenOr
           | TokenNot
           | TokenEq
           | TokenNe
           | TokenGe 
           | TokenLe 
           | TokenGt 
           | TokenLt 
           | TokenPlus 
           | TokenMinus 
           | TokenTimes
           | TokenDiv
           | TokenMod 
           deriving (Show)