module Mamarracho (compile) where

import Constants
import Types
import State

import Ast (Program, Expr(..), Definition(..))
import Control.Monad.State (execState, MonadState(put, get))
import Data.Char (ord)
import Data.Function (on)
import Data.List (intercalate, sortBy, (\\), union)
import Data.Text (unpack, replace, pack)

-- Compilation

compile :: Program -> MamCode
compile prog = showCode $ getCode $ execState (compile' prog) initState

compile' :: Program -> Mam ()
compile' []            = return ()
compile' (def:program) = do
  compileDef def
  compile' program

compileDef :: Definition -> Mam ()
compileDef (Def _id e) = do
  let greg = Global $ "G_" ++ _id
  compileExpr e greg
  extendEnv (_id, BRegister greg)
  return ()

compileExpr :: Expr -> Reg -> Mam ()
compileExpr (ExprVar _id)            reg = compileVariable _id reg
compileExpr (ExprNumber n)           reg = compilePrimitiveValue tagNumber n reg
compileExpr (ExprChar c)             reg = compilePrimitiveValue tagChar (ord c) reg
compileExpr (ExprApply e1 e2)        reg = do
  case e1 of
    (ExprApply (ExprVar "ADD") e1') -> compileArithmeticOperation Add e1' e2 reg
    (ExprApply (ExprVar "SUB") e1') -> compileArithmeticOperation Sub e1' e2 reg
    (ExprApply (ExprVar "MUL") e1') -> compileArithmeticOperation Mul e1' e2 reg
    (ExprApply (ExprVar "DIV") e1') -> compileArithmeticOperation Div e1' e2 reg
    _                               -> compileApplication e1 e2 reg
compileExpr (ExprConstructor _id) reg = do
  case tagOf _id of
    TTrue  tag -> compileBoolean tag reg
    TFalse tag -> compileBoolean tag reg
    _ -> error $ "ExprConstructor " ++ _id ++ " NOT implemented"
compileExpr (ExprLet _id e1 e2) reg = do
  temp <- localReg
  compileExpr e1 temp
  env' <- getEnv
  pushEnv env'
  extendEnv (_id, BRegister temp)
  compileExpr e2 reg
  _ <- popEnv
  return ()
compileExpr (ExprLambda _id e) reg = compileLambda _id e reg
compileExpr e _ = error $ "Expression NOT implemented: " ++ show e

compileVariable :: ID -> Reg -> Mam ()
compileVariable _id reg = do
  case varType _id of
    TVar     -> compileVarValue _id reg
    TOper    -> compileOperation _id reg
    TPrinter -> compilePrimitivePrint _id reg

compilePrimitivePrint :: String -> Reg -> Mam ()
compilePrimitivePrint _id reg = do
  let printer = if _id == "unsafePrintChar" then PrintChar else Print
  lreg <- localReg
  addCode [
    Load (lreg, reg, 1),
    printer lreg
    ]

compileVarValue :: ID -> Reg -> Mam ()
compileVarValue _id reg = do
  res <- lookupEnv _id
  case res of
    Just (BRegister r) -> addCode [MovReg (reg, r)]
    Just (BEnclosed n) -> addCode [Load (reg, Local "fun", n + 2)]
    _ -> addCode [Comment $ "Var " ++ _id ++ " not found in env"]

compilePrimitiveValue :: TagType -> Int -> Reg -> Mam ()
compilePrimitiveValue tag val reg = do
  temp <- tempReg
  addCode [
    Alloc  (reg, 2),
    MovInt (temp, tag),
    Store  (reg, 0, temp),
    MovInt (temp, val),
    Store  (reg, 1, temp)
    ]

compileArithmeticOperation :: NumOp -> Expr -> Expr -> Reg -> Mam ()
compileArithmeticOperation mamOp e1 e2 reg = do
  r1 <- localReg
  r2 <- localReg
  temp <- tempReg
  compileExpr e1 r1
  compileExpr e2 r2
  addCode [
    Load   (r1, r1, 1),
    Load   (r2, r2, 1),
    mamOp  (r1, r1, r2),
    MovInt (temp, tagNumber),
    Alloc  (reg, 2),
    Store  (reg, 0, temp),
    Store  (reg, 1, r1)
    ]

compileOperation :: ID -> Reg -> Mam ()
compileOperation x _ = error $ "compileOperation " ++ x ++ " not implemented"

compileApplication :: Expr -> Expr -> Reg -> Mam ()
compileApplication e1@(ExprVar x) e2 reg = do
  case varType x of
    TVar     -> compileApplicationFunction e1 e2 reg
    TOper    -> compileExpr e2 reg >> compileOperation x reg
    TPrinter -> compileExpr e2 reg >> compilePrimitivePrint x reg
compileApplication e1 e2 reg = compileApplicationFunction e1 e2 reg

compileApplicationFunction :: Expr -> Expr -> Reg -> Mam ()
compileApplicationFunction e1 e2 reg = do
  r1 <- localReg
  r2 <- localReg
  r3 <- localReg
  compileExpr e1 r1
  compileExpr e2 r2
  addCode [
    MovReg (Global "fun", r1),
    MovReg (Global "arg", r2),
    Load   (r3, Global "fun", 1),
    ICall  r3,
    MovReg (reg, Global "res")
    ]

compileBoolean :: I64 -> Reg -> Mam ()
compileBoolean tag reg = do
  temp <- tempReg
  addCode [
    Alloc (reg, 1),
    MovInt (temp, tag),
    Store (reg, 0, temp)
    ]

compileLambda :: ID -> Expr -> Reg -> Mam ()
compileLambda _id e reg = do
  env' <- getEnv
  pushEnv env'
  compileLambdaExpr _id e reg
  _ <- popEnv
  return ()

compileLambdaExpr :: ID -> Expr -> Reg -> Mam ()
compileLambdaExpr _id e reg = do
  env' <- getEnv
  label <- routineLabel
  let freeVars = extractFreeVars env' e \\ [_id]
  compileLexicalClosure _id freeVars label reg
  bindClosureEnv freeVars
  extendEnv (_id, BRegister $ Local "arg")
  compileRoutine e label

compileLexicalClosure :: ID -> [ID] -> Label -> Reg -> Mam ()
compileLexicalClosure _id freeVars label reg = do
  temp <- tempReg
  let len = length freeVars
  addCode [
    Alloc (reg, 2 + len),
    MovInt (temp, 3),
    Store (reg, 0, temp),
    MovLabel (temp, label),
    Store (reg, 1, temp)
    ]
  compileLexicalClosureVars freeVars temp reg 0

compileLexicalClosureVars :: [ID] -> Reg -> Reg -> Int -> Mam ()
compileLexicalClosureVars     []    _   _ _ = return ()
compileLexicalClosureVars (x:xs) treg reg n = do
  compileVariable x treg
  addCode [Store (reg, n + 2, treg)]
  compileLexicalClosureVars xs treg reg (n+1)

compileRoutine :: Expr -> Label -> Mam ()
compileRoutine e label = do
  lreg <- localReg
  prevStack <- getStack
  switchToRoutineStack label
  addCode [
  --rtn_i:
    Comment $ fl e,
    MovReg (Local "fun", Global "fun"),
    MovReg (Local "arg", Global "arg")
    ]
  compileExpr e lreg
  addCode [
    MovReg (Global "res", lreg),
    Return
    ]
  switchStack prevStack

-- helpers

getCode :: MamState -> [Instruction]
getCode mam = [Jump "start"]
           ++ unfoldRoutines (routines mam)
           ++ [ILabel "start"]
           ++ code mam

unfoldRoutines :: [CodeRoutine] -> [Instruction]
unfoldRoutines = unfoldRoutines' [] . sortBy (compare `on` fstNum)

fstNum :: (Label, [Instruction]) -> Int
fstNum (label, _) = read $ unpack $ replace (pack "rtn") (pack "") (pack label) :: Int

unfoldRoutines' :: [Instruction] -> [CodeRoutine] -> [Instruction]
unfoldRoutines' ins [] = ins
unfoldRoutines' ins ((label, rout):routines') =
  unfoldRoutines' (ins ++ [ILabel label] ++ rout) routines'

showCode :: Show a => [a] -> String
showCode code' = intercalate "\n" (map show code') ++ "\n"

varType :: String -> VarType
varType x
  | isPrimitivePrinter   x = TPrinter
  | isPrimitiveOperation x = TOper
  | otherwise              = TVar

isPrimitive :: String -> Bool
isPrimitive _id = isPrimitivePrinter _id || isPrimitiveOperation _id

isPrimitivePrinter :: String -> Bool
isPrimitivePrinter _id = _id `elem` ["unsafePrintInt", "unsafePrintChar"]

isPrimitiveOperation :: String -> Bool
isPrimitiveOperation _id = _id `elem` ["ADD", "SUB", "MUL", "DIV", "MOD"]

tagOf :: ID -> Tag
tagOf _id =
  case _id of
    "Int"     -> TInt     1
    "Char"    -> TChar    2
    "Closure" -> TClosure 3
    "True"    -> TTrue    4
    "False"   -> TFalse   5
    "Nil"     -> TNil     6
    "Cons"    -> TCons    7
    _ -> error $ "Invalid constructor " ++ _id

bindClosureEnv :: [ID] -> Mam ()
bindClosureEnv = bindClosureEnv' 0

bindClosureEnv' :: Int -> [ID] -> Mam ()
bindClosureEnv' _     [] = return ()
bindClosureEnv' n (x:xs) = extendEnv (x, BEnclosed n) >> bindClosureEnv' (n+1) xs

extractFreeVars :: Env -> Expr -> [ID]
extractFreeVars env' (ExprVar _id) = if isPrimitive _id then [] else
  case lookup _id env' of
    Nothing                     -> [_id]
    Just (BRegister (Local  _)) -> [_id]
    Just (BEnclosed          _) -> [_id]
    Just (BRegister (Global _)) -> []
extractFreeVars env' (ExprLet _id e1 e2) = extractFreeVars env' e1 `union` (extractFreeVars env' e2 \\ [_id])
extractFreeVars env' (ExprLambda _id  e) = extractFreeVars env' e \\ [_id]
extractFreeVars env' (ExprApply   e1 e2) = extractFreeVars env' e1 `union` extractFreeVars env' e2
extractFreeVars   _                    _ = []

tempReg :: Mam Reg
tempReg = return $ Local "temp"

routineLabel :: Mam Label
routineLabel = do
  mam <- get
  let n = nextRtn mam
  put $ mam { nextRtn = n + 1 }
  return $ "rtn" ++ show n

localReg :: Mam Reg
localReg = do
  mam <- get
  let n = nextReg mam
  put $ mam { nextReg = n + 1 }
  return $ Local $ "r" ++ show n

fl :: Expr -> String
fl (ExprVar x) = x
fl (ExprConstructor x) = x
fl (ExprNumber n) = show n
fl (ExprChar c) = show c
-- fl (ExprCase e cases) = ""
fl (ExprLet "_" e1 e2) = fl e1 ++ "; " ++ fl e2
fl (ExprLet x e1 e2) = "let " ++ x ++ " = " ++ fl e1 ++ " in " ++ fl e2
fl (ExprLambda x e) = "\\" ++ x ++ " -> " ++ fl e
fl (ExprApply (ExprVar x) (ExprVar y)) = x ++ " " ++ y
fl (ExprApply (ExprVar x) e2) = x ++ " (" ++ fl e2 ++ ")"
fl (ExprApply e1 e2) = "(" ++ fl e1 ++ ") (" ++ fl e2 ++ ")"
fl e = show e ++ " not defined"