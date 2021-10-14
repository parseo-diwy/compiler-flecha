module Json (toJsonProgram) where

import Ast ( Definition(..), Expr(..), Program )
import Data.Char (ord)
import Data.List (intercalate)

toJsonProgram :: Program -> String
toJsonProgram ds = "[" ++ intercalate "," (map toJsonDef ds) ++ "]"

toJsonDef :: Definition -> String
toJsonDef (Def _id _exp) = def _id (toJsonExp _exp)

toJsonExp :: Expr -> String
toJsonExp (ExprVar _id) = exprVar _id
toJsonExp (ExprConstructor _id) = exprConstructor _id
toJsonExp (ExprNumber n) = exprNumber n
toJsonExp (ExprChar c) = exprChar c
toJsonExp (ExprCase _exp _bs) = "TODO CASE"
toJsonExp (ExprLet _id _exp1 _exp2) = exprLet _id (toJsonExp _exp1) (toJsonExp _exp2)
toJsonExp (ExprLambda _id _exp) = exprLambda _id (toJsonExp _exp)
toJsonExp (ExprApply _exp1 _exp2) = exprApply (toJsonExp _exp1) (toJsonExp _exp2)

-- Formatters

type StrID = String
type StrExp = String

quote :: String -> String
quote x = "\"" ++ x ++ "\""

def :: StrID -> StrExp -> String
def i e = "[\"Def\", " ++ quote i ++ ", " ++ e ++ "]"

exprVar :: StrID -> String
exprVar i = "[\"ExprVar\", " ++ quote i ++ "]"

exprConstructor :: StrID -> String
exprConstructor i = "[\"ExprConstructor\", " ++ quote i ++ "]"

exprNumber :: Integer -> String
exprNumber n = "[\"ExprNumber\", " ++ show n ++ "]"

exprChar :: Char -> String
exprChar c = "[\"ExprChar\", " ++ ascii c ++ "]"

exprLet :: StrID -> StrExp -> StrExp -> String
exprLet i e1 e2 = "[\"ExprLet\", " ++ quote i ++ ", " ++ e1 ++ ", " ++ e2 ++ "]"

exprLambda :: StrID -> StrExp -> String
exprLambda i e = "[\"ExprLambda\", " ++ quote i ++ ", " ++ e ++ "]"

exprApply :: StrExp -> StrExp -> String
exprApply e1 e2 = "[\"ExprApply\", " ++ e1 ++ ", " ++ e2 ++ "]"

ascii :: Char -> String
ascii = show . ord
