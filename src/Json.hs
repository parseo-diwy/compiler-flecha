module Json where

import Ast ( Factor(..), Term(..), Exp1(..), Exp(..) )
import Data.Char (ord)
-- Exp
-- Exp1
-- Term
-- Factor

-- Exp1 (Plus (Term (Factor (Int 1))) (Factor (Int 2)))
jsonProgram :: Exp -> String
jsonProgram p = "[" ++ jsonExpr p ++ "]"

jsonExpr :: Exp -> String
jsonExpr (Let id e1 e2) = "[\"ExprLet\", " ++ id ++ ", " ++ jsonExpr e1 ++ ", " ++ jsonExpr e2 ++ "]"
jsonExpr (Exp1 e1) = jsonExpr1 e1

jsonExpr1 :: Exp1 -> String
jsonExpr1 (Plus e t) = "[\"ExprApply\", [\"ExprApply\", [\"ExprVar\", \"ADD\"], " ++ jsonExpr1 e ++ "], " ++ jsonTerm t ++"]"
jsonExpr1 (Minus e t) = "[\"ExprApply\", [\"ExprApply\", [\"ExprVar\", \"SUB\"], " ++ jsonExpr1 e ++ "], " ++ jsonTerm t ++"]"
jsonExpr1 (Term t) = jsonTerm t

jsonTerm :: Term -> String
jsonTerm (Times t f) = "[\"ExprApply\", [\"ExprApply\", [\"ExprVar\", \"MUL\"], " ++ jsonTerm t ++ "], " ++ jsonFactor f ++"]"
jsonTerm (Div t f) = "[\"ExprApply\", [\"ExprApply\", [\"ExprVar\", \"DIV\"], " ++ jsonTerm t ++ "], " ++ jsonFactor f ++"]"
jsonTerm (Factor f) = jsonFactor f

jsonFactor :: Factor -> String
jsonFactor (Int n) = "[\"ExprNumber\", " ++ show n ++ "]"
jsonFactor (Var s) = jsonString s
jsonFactor (Brack e) = ""

jsonString :: String -> String
jsonString "" = "[\"ExprConstructor\", \"Nil\"]"
jsonString (c:cs) = "[\"ExprApply\", [\"ExprApply\", [\"ExprConstructor\", \"Cons\"], [\"ExprChar\", " ++ ascii c ++ "]]," ++ jsonString cs ++ "]"

ascii :: Char -> String
ascii = show . ord
