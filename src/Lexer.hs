module Lexer (lexer) where

import Tokens ( Token(..) )
import Data.Char ( isDigit, isAlpha, isSpace, isLower, isUpper )
import qualified Data.Map as Map

lexer :: String -> [Token]
lexer [] = []
lexer input@(c:cs)  | isSpace c                 = lexer cs
                    | isDigit c                 = lexNumber input
                    | isLower c && isAlpha c    = lexTokenOrLowerId input
                    | isUpper c && isAlpha c    = lexUpperId input
                    | isStartingSymbol c        = lexSymbol input
                    | '"' == c                  = lexString input
                    | '\'' == c                 = lexChar input
                    | '-' == c                  = lexer $ removeComment cs
                    | otherwise                 = error $ "Lexical error: unexpected character: `" ++ [c::Char] ++ "`"

removeComment :: String -> String
removeComment ('-':cs) = ignoreToEndLine cs
removeComment cs = cs

ignoreToEndLine :: String -> String
ignoreToEndLine "" = ""
ignoreToEndLine ('\n':cs) = cs
ignoreToEndLine (_:cs) = cs

lexChar :: String -> [Token]
lexChar ('\'':'\\':'\'':'\'':rest) = TokenChar '\''  : lexer rest
lexChar ('\'':'\\':'\\':'\'':rest) = TokenChar '\\'  : lexer rest
lexChar ('\'':'\\':'\"':'\'':rest) = TokenChar '\"'  : lexer rest
lexChar ('\'':'\\':'t':'\'':rest)  = TokenChar '\t'  : lexer rest
lexChar ('\'':'\\':'n':'\'':rest)  = TokenChar '\n'  : lexer rest
lexChar ('\'':'\\':'r':'\'':rest)  = TokenChar '\r'  : lexer rest
lexChar ('\'': c : '\'':rest)      = TokenChar c     : lexer rest
lexChar _ = error "Lexical error: expected a char"

lexString :: String -> [Token]
lexString input = let (string, rest) = spanString (tail input) "" False
                    in TokenString (reverse string) : lexer rest

spanString :: String -> String -> Bool -> (String, String)
spanString ('"':cs)  string False = (string, cs)
spanString ('\\':cs) string False = spanString cs string        True
spanString (c:cs)    string False = spanString cs (c:string)    False
spanString ('t':cs)  string True  = spanString cs ('\t':string) False
spanString ('n':cs)  string True  = spanString cs ('\n':string) False
spanString ('r':cs)  string True  = spanString cs ('\r':string) False
spanString ('\\':cs) string True  = spanString cs ('\\':string) False
spanString ('\'':cs) string True  = spanString cs ('\'':string) False
spanString ('"':cs)  string True  = spanString cs ('"':string)  False
spanString (c:_)     _      True  = error $ "Lexical error: invalid escaped character `" ++ [c::Char] ++ "`"
spanString _         _      _     = error "Lexical error: unexpected input sequence"

lexNumber :: String -> [Token]
lexNumber cs = let (numStr, rest) = span isDigit cs
                in TokenNumber (read numStr::Integer) : lexer rest

lexSymbol :: String -> [Token]
lexSymbol [] = []
lexSymbol ('-':'-':cs)  = lexComment cs
lexSymbol ('=':'=':cs)  = TokenEq        : lexer cs
lexSymbol ('-':'>':cs)  = TokenArrow     : lexer cs
lexSymbol ('&':'&':cs)  = TokenAnd       : lexer cs
lexSymbol ('|':'|':cs)  = TokenOr        : lexer cs
lexSymbol ('!':'=':cs)  = TokenNe        : lexer cs
lexSymbol ('>':'=':cs)  = TokenGe        : lexer cs
lexSymbol ('<':'=':cs)  = TokenLe        : lexer cs
lexSymbol ('=':cs)      = TokenDefEq     : lexer cs
lexSymbol (';':cs)      = TokenSemicolon : lexer cs
lexSymbol ('(':cs)      = TokenLParen    : lexer cs
lexSymbol (')':cs)      = TokenRParen    : lexer cs
lexSymbol ('\\':cs)     = TokenLambda    : lexer cs
lexSymbol ('|':cs)      = TokenPipe      : lexer cs
lexSymbol ('!':cs)      = TokenNot       : lexer cs
lexSymbol ('>':cs)      = TokenGt        : lexer cs
lexSymbol ('<':cs)      = TokenLt        : lexer cs
lexSymbol ('+':cs)      = TokenPlus      : lexer cs
lexSymbol ('-':cs)      = TokenMinus     : lexer cs
lexSymbol ('*':cs)      = TokenTimes     : lexer cs
lexSymbol ('/':cs)      = TokenDiv       : lexer cs
lexSymbol ('%':cs)      = TokenMod       : lexer cs
lexSymbol _             = error "Add 'otherwise' for 'Non-exhaustive Pattern matching' linting error. Add case if is a valid one."

lexComment :: String -> [Token]
lexComment []        = []
lexComment ('\n':cs) = lexer cs
lexComment ('\r':cs) = lexer cs
lexComment (_:cs)    = lexComment cs

lexTokenOrLowerId :: String -> [Token]
lexTokenOrLowerId input = let (word, rest) = spanId input
                            in case lookupKeywordToken word of
                                Just kwToken -> kwToken : lexer rest
                                Nothing      -> TokenLowerId word : lexer rest

lexUpperId :: String -> [Token]
lexUpperId input = let (word, rest) = spanId input
                    in TokenUpperId word : lexer rest

isSpaceOrSymbol :: Char -> Bool
isSpaceOrSymbol c = isSpace c || isStartingSymbol c

spanId :: [Char] -> ([Char], [Char])
spanId = break isSpaceOrSymbol

isStartingSymbol :: Char -> Bool
isStartingSymbol c = c `elem` startingSymbolsList

startingSymbolsList :: [Char]
startingSymbolsList = head <$> Map.keys symbolTokenMap  -- "!%&()*+-/;<=>\\|" -- 

--  keyword-token Mappings
lookupKeywordToken :: String -> Maybe Token
lookupKeywordToken word = Map.lookup word keywordTokenMap

keywordTokenMap :: Map.Map String Token
keywordTokenMap = Map.fromList [
        ("def",     TokenDef),
        ("if",      TokenIf),
        ("then",    TokenThen),
        ("else",    TokenElse),
        ("elif",    TokenElif),
        ("case",    TokenCase),
        ("let",     TokenLet),
        ("in",      TokenIn)
    ]

--  symbol-token Mappings
symbolTokenMap :: Map.Map String Token
symbolTokenMap = Map.fromList [
        ("=",   TokenDefEq),
        (";",   TokenSemicolon),
        ("(",   TokenLParen),
        (")",   TokenRParen),
        ("\\",  TokenLambda),
        ("|",   TokenPipe),
        ("->",  TokenArrow),
        ("&&",  TokenAnd),
        ("||",  TokenOr),
        ("!",   TokenNot),
        ("==",  TokenEq),
        ("!=",  TokenNe),
        (">=",  TokenGe),
        ("<=",  TokenLe),
        (">",   TokenGt),
        ("<",   TokenLt),
        ("+",   TokenPlus),
        ("-",   TokenMinus),
        ("*",   TokenTimes),
        ("/",   TokenDiv),
        ("%",   TokenMod)
    ]
