module Json (jsonProgram) where

import Ast ( Factor(..), Term(..), Exp1(..), Exp(..) )
import Data.Char (ord)

jsonProgram :: Exp -> String
jsonProgram p = "[" ++ jsonExpr p ++ "]"

jsonExpr :: Exp -> String
jsonExpr (Let _id exp1 exp2) = exprLet _id (jsonExpr exp1) (jsonExpr exp2)
jsonExpr (Exp1 exp1) = jsonExpr1 exp1

jsonExpr1 :: Exp1 -> String
jsonExpr1 (Plus _exp term) = jsonApplyTerm "ADD" _exp term
jsonExpr1 (Minus _exp term) = jsonApplyTerm "SUB" _exp term
jsonExpr1 (Term term) = jsonTerm term

jsonApplyTerm :: String -> Exp1 -> Term -> String
jsonApplyTerm _id _exp term = let op = exprApply (exprVar _id) (jsonExpr1 _exp) in exprApply op (jsonTerm term)

jsonTerm :: Term -> String
jsonTerm (Times term factor) = jsonApplyFactor "MUL" term factor
jsonTerm (Div term factor) = jsonApplyFactor "DIV" term factor
jsonTerm (Factor factor) = jsonFactor factor

jsonApplyFactor :: String -> Term -> Factor -> String
jsonApplyFactor _id term factor =
  let op = exprApply (exprVar _id) (jsonTerm term)
    in exprApply op (jsonFactor factor)

jsonFactor :: Factor -> String
jsonFactor (Int n) = exprNumber n
jsonFactor (Var s) = jsonString s
jsonFactor (Brack _exp) = jsonExpr _exp

jsonString :: String -> String
jsonString "" = exprConstructor "\"Nil\""
jsonString (c:cs) =
  let consExpr = exprApply (exprConstructor "\"Cons\"") (exprChar c)
    in exprApply consExpr (jsonString cs)

-- Json Printers

type ID = String
type ExprString = String

exprVar :: ID -> String
exprVar _id = "[\"ExprVar\", " ++ show _id ++ "]"

exprConstructor :: ID -> String
exprConstructor _id = "[\"ExprConstructor\", " ++ show _id ++ "]"

exprNumber :: Integer -> String
exprNumber n = "[\"ExprNumber\", " ++ show n ++ "]"

exprChar :: Char -> String
exprChar c = "[\"ExprChar\", " ++ ascii c ++ "]"

exprLet :: ID -> ExprString -> ExprString -> String
exprLet _id exp1 exp2 = "[\"ExprLet\", " ++ _id ++ ", " ++ exp1 ++ ", " ++ exp2 ++ "]"

exprLambda :: ID -> ExprString -> String
exprLambda _id _exp = "[\"ExprLambda\", " ++ _id ++ ", " ++ _exp ++ "]"

exprApply :: ExprString -> ExprString -> String
exprApply exp1 exp2 = "[\"ExprApply\", " ++ exp1 ++ ", " ++ exp2 ++ "]"

ascii :: Char -> String
ascii = show . ord
