module Mamarracho (compile) where

import Constants
import Types
import State

import Ast (Program, Expr(..), Definition(..))
import Control.Monad.State (execState, MonadState(put, get))
import Data.Char (ord)
import Data.List (intercalate)

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
    (ExprLambda _id e1')            -> compileLambda _id e1' (Just e2) reg
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
  pushEnv ((_id, BRegister temp) : env')
  compileExpr e2 reg
  _ <- popEnv
  return ()
compileExpr (ExprLambda _id e) reg = compileLambda _id e Nothing reg
compileExpr e _ = error $ "Expression NOT implemented: " ++ show e

compileVariable :: ID -> Reg -> Mam ()
compileVariable _id reg = do
  case typeOfPrim _id of
    PrimPrint -> compilePrimitivePrint _id reg
    PrimOp    -> compilePrimitiveOperation _id reg
    PrimVar   -> compileVarValue _id reg

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
    _ -> error $ "Var " ++ _id ++ " is not defined en env"


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

compilePrimitiveOperation :: ID -> Reg -> Mam ()
compilePrimitiveOperation _id _ = error $ "compilePrimitiveOperation " ++ _id ++ " not implemented"

compileApplication :: Expr -> Expr -> Reg -> Mam ()
compileApplication (ExprVar _id) e2 reg = do
  case typeOfPrim _id of
    PrimPrint -> do
      compileExpr e2 reg
      compilePrimitivePrint _id reg
    PrimOp    -> do
      compileExpr e2 reg
      compilePrimitiveOperation _id reg
    PrimVar   -> do
      r1 <- lookupEnvRegister _id
      r2 <- localReg
      r3 <- localReg
      compileExpr e2 r2
      addCode [
        MovReg (Global "fun", r1),
        MovReg (Global "arg", r2),
        Load   (r3, Global "fun", 1),
        ICall  r3,
        MovReg (reg, Global "res")
        ]
compileApplication e1 e2 reg = do
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
-- compileApplication e1 e2 reg = error $ "error on compileApplication " ++ show (e1, e2, reg)

compileBoolean :: I64 -> Reg -> Mam ()
compileBoolean tag reg = do
  temp <- tempReg
  addCode [
    Alloc (reg, 1),
    MovInt (temp, tag),
    Store (reg, 0, temp)
    ]

compileLambda :: ID -> Expr -> Maybe Expr -> Reg -> Mam ()
compileLambda _id e1 me2 reg = do
  r1 <- localReg
  env' <- getEnv
  pushEnv env'
  compileLambdaExpr _id e1 r1 reg
  case me2 of
    Just e2 -> compileLambdaApply _id e2  r1 reg
    Nothing -> return ()
  _ <- popEnv
  return ()

compileLambdaExpr :: ID -> Expr -> Reg -> Reg -> Mam ()
compileLambdaExpr _id e lreg greg = do
  extendEnv (_id, BRegister $ Local "arg")
  bindClosureEnv $ extractFreeVars _id e
  label <- routineLabel
  compileRoutine e label
  compileLexicalClosure label lreg greg

compileLambdaApply :: ID -> Expr -> Reg -> Reg -> Mam ()
compileLambdaApply _id e2 lreg greg = do
  r1 <- localReg
  r2 <- localReg
  compileExpr e2 r1
  addCode [
    MovReg (Global "fun", lreg),
    MovReg (Global "arg", r1),
    Load (r2, Global "fun", 1),
    ICall r2,
    MovReg (greg, Global "res")
    ]

compileLexicalClosure :: Label -> Reg -> Reg -> Mam ()
compileLexicalClosure label lreg greg = do
  temp <- tempReg
  varsIns <- compileLexicalClosureVars lreg greg
  isRoutine <- isRoutineStack
  let retreg = if isRoutine then Local "arg" else greg
  let len = length varsIns
  addCode [
    Comment "<compileLexicalClosure>",
    Alloc (greg, 2 + len),
    MovInt (temp, 3),
    Store (greg, 0, temp),
    MovLabel (temp, label),
    Store (greg, tagNumber, temp),
    MovReg (lreg, retreg),
    Comment "</compileLexicalClosure>"
    ]
  addCode varsIns

compileLexicalClosureVars :: Reg -> Reg -> Mam [Instruction]
compileLexicalClosureVars lreg greg = do
  env' <- getEnv
  compileLexicalClosureVars' env' lreg greg

compileLexicalClosureVars' :: Env -> Reg -> Reg -> Mam [Instruction]
compileLexicalClosureVars' [] _ _ = return []
compileLexicalClosureVars' ((_, BRegister _):binds) lreg greg = compileLexicalClosureVars' binds lreg greg
compileLexicalClosureVars' ((_, BEnclosed n):binds) lreg greg = do
  let ins = [Store (greg, n + 2, lreg)]
  rest <- compileLexicalClosureVars' binds lreg greg
  return $ ins ++ rest


compileRoutine :: Expr -> Label -> Mam ()
compileRoutine e label = do
  lreg <- localReg
  prevStack <- getStack
  switchToRoutineStack label
  -- rtn:
  addCode [
    Comment $ show e,
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
unfoldRoutines = unfoldRoutines' []

unfoldRoutines' :: [Instruction] -> [CodeRoutine] -> [Instruction]
unfoldRoutines' ins [] = ins
unfoldRoutines' ins ((label, rout):routines') =
  unfoldRoutines' (ins ++ [ILabel label] ++ rout) routines'

showCode :: Show a => [a] -> String
showCode = intercalate "\n" . map show

typeOfPrim :: String -> PrimType
typeOfPrim _id
  | isPrimitivePrinter _id = PrimPrint
  | isPrimitiveOperation _id = PrimOp
  | otherwise = PrimVar

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
bindClosureEnv' _ [] = return ()
bindClosureEnv' n (x:xs) = extendEnv (x, BEnclosed n) >> bindClosureEnv' (n+1) xs

extractFreeVars :: ID -> Expr -> [ID]
extractFreeVars = extractFreeVars' []

extractFreeVars' :: [ID] -> ID -> Expr -> [ID]
extractFreeVars' vars x (ExprVar       _id) | x == _id        = vars
                                            | isPrimitive _id = vars
                                            | _id `elem` vars = vars
                                            | otherwise       = vars ++ [_id]
extractFreeVars' vars x (ExprCase    e   _) = extractFreeVars' vars x e
extractFreeVars' vars x (ExprLambda  _id e) = extractFreeVars' vars x e
extractFreeVars' vars x (ExprApply   e1 e2) = extractFreeVars' (extractFreeVars' vars x e1) x e2
extractFreeVars' vars x (ExprLet _id e1 e2) = extractFreeVars' (extractFreeVars' vars x e1) x e2
extractFreeVars' vars _ _                   = vars

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
