module Json (jsonProgram) where

import Ast ( Factor(..), Term(..), Exp1(..), Exp(..) )
import Data.Char (ord)

jsonProgram :: Exp -> String
jsonProgram p = "[" ++ jsonExpr p ++ "]"

jsonExpr :: Exp -> String
jsonExpr (Let id exp1 exp2) = exprLet id (jsonExpr exp1) (jsonExpr exp2)
jsonExpr (Exp1 exp1) = jsonExpr1 exp1

jsonExpr1 :: Exp1 -> String
jsonExpr1 (Plus exp term) = jsonApplyTerm "ADD" exp term
jsonExpr1 (Minus exp term) = jsonApplyTerm "SUB" exp term
jsonExpr1 (Term term) = jsonTerm term

jsonApplyTerm :: String -> Exp1 -> Term -> String
jsonApplyTerm id exp term = let op = exprApply (exprVar id) (jsonExpr1 exp) in exprApply op (jsonTerm term)

jsonTerm :: Term -> String
jsonTerm (Times term factor) = jsonApplyFactor "MUL" term factor
jsonTerm (Div term factor) = jsonApplyFactor "DIV" term factor
jsonTerm (Factor factor) = jsonFactor factor

jsonApplyFactor :: String -> Term -> Factor -> String
jsonApplyFactor id term factor =
  let op = exprApply (exprVar id) (jsonTerm term)
    in exprApply op (jsonFactor factor)

jsonFactor :: Factor -> String
jsonFactor (Int n) = exprNumber n
jsonFactor (Var s) = jsonString s
jsonFactor (Brack exp) = jsonExpr exp

jsonString :: String -> String
jsonString "" = exprConstructor "\"Nil\""
jsonString (c:cs) =
  let consExpr = exprApply (exprConstructor "\"Cons\"") (exprChar c)
    in exprApply consExpr (jsonString cs)

-- Json Printers

type ID = String
type ExprString = String

exprVar :: ID -> String
exprVar id = "[\"ExprVar\", " ++ show id ++ "]"

exprConstructor :: ID -> String
exprConstructor id = "[\"ExprConstructor\", " ++ show id ++ "]"

exprNumber :: Integer -> String
exprNumber n = "[\"ExprNumber\", " ++ show n ++ "]"

exprChar :: Char -> String
exprChar c = "[\"ExprChar\", " ++ ascii c ++ "]"

exprLet :: ID -> ExprString -> ExprString -> String
exprLet id exp1 exp2 = "[\"ExprLet\", " ++ id ++ ", " ++ exp1 ++ ", " ++ exp2 ++ "]"

exprLambda :: ID -> ExprString -> String
exprLambda id exp = "[\"ExprLambda\", " ++ id ++ ", " ++ exp ++ "]"

exprApply :: ExprString -> ExprString -> String
exprApply exp1 exp2 = "[\"ExprApply\", " ++ exp1 ++ ", " ++ exp2 ++ "]"

ascii :: Char -> String
ascii = show . ord
