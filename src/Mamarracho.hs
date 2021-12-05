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
  code' <- compileExpr e greg
  extendCode code'
  extendEnv (_id, BRegister greg)
  return ()

compileExpr :: Expr -> Reg -> Mam [Instruction]
compileExpr (ExprVar _id)            reg = compileVariable _id reg
compileExpr (ExprNumber n)           reg = compilePrimitiveValue tagNumber n reg
compileExpr (ExprChar c)             reg = compilePrimitiveValue tagChar (ord c) reg
compileExpr (ExprApply e1 e2)        reg = do
  case e1 of
    (ExprLambda _id e1') -> do
      ins1 <- compileLambdaExpr _id e1 reg
      ins2 <- compileLambdaApply e1' e2 reg
      return $ ins1 ++ ins2
    (ExprApply (ExprVar cons) e1') -> do
      compileOperation cons e1' e2 reg
    _ -> do
      ins1 <- compileExpr e2 reg
      ins2 <- compileExpr e1 reg
      return $ ins1 ++ ins2
compileExpr (ExprConstructor _id) reg = do
  case tagOf _id of
    TTrue  tag -> compileBoolean tag reg
    TFalse tag -> compileBoolean tag reg
    _ -> error $ "ExprConstructor " ++ _id ++ " NOT implemented"
compileExpr (ExprLet _id e1 e2) reg = do
  temp <- localReg
  ins1 <- compileExpr e1 temp
  env' <- getEnv
  pushEnv ((_id, BRegister temp) : env')
  ins2 <- compileExpr e2 reg
  _ <- popEnv
  return $ ins1 ++ ins2
compileExpr (ExprLambda _id e) reg = compileLambdaExpr _id e reg
compileExpr e _ = error $ "Expression NOT implemented: " ++ show e

compileVariable :: ID -> Reg -> Mam [Instruction]
compileVariable _id reg = do
  case typeOfPrim _id of
    PrimPrint -> compilePrimitivePrint (_id, reg)
    PrimOp    -> compilePrimitiveOperation (_id, reg)
    PrimVar   -> compileVarValue (_id, reg)

compilePrimitivePrint :: (String, Reg) -> Mam [Instruction]
compilePrimitivePrint (_id, reg) = do
  let printer = if _id == "unsafePrintChar" then PrintChar else Print
  lreg <- localReg
  return [
    Load (lreg, reg, 1),
    printer lreg
    ]

compileVarValue :: (ID, Reg) -> Mam [Instruction]
compileVarValue (_id, reg) = do
  greg <- lookupEnvRegister _id
  return [MovReg (reg, greg)]

compilePrimitiveValue :: TagType -> Int -> Reg -> Mam [Instruction]
compilePrimitiveValue tag val reg = do
  temp <- tempReg
  return [
    Alloc  (reg, 2),
    MovInt (temp, tag),
    Store  (reg, 0, temp),
    MovInt (temp, val),
    Store  (reg, 1, temp)
    ]

compileOperation :: ID -> Expr -> Expr -> Reg -> Mam [Instruction]
compileOperation "ADD" = compileArithmeticOperation Add
compileOperation "SUB" = compileArithmeticOperation Sub
compileOperation "MUL" = compileArithmeticOperation Mul
compileOperation "DIV" = compileArithmeticOperation Div
compileOperation _id = error $ "CompileOperation: Invalid Constructor '" ++ show _id ++ "'"

compileArithmeticOperation :: NumOp -> Expr -> Expr -> Reg -> Mam [Instruction]
compileArithmeticOperation mamOp e1 e2 reg = do
  r1 <- localReg
  r2 <- localReg
  temp <- tempReg
  ins1 <- compileExpr e1 r1
  ins2 <- compileExpr e2 r2
  return $ ins1 ++ ins2 ++ [
    Load   (r1, r1, 1),
    Load   (r2, r2, 1),
    mamOp  (r1, r1, r2),
    MovInt (temp, tagNumber),
    Alloc  (reg, 2),
    Store  (reg, 0, temp),
    Store  (reg, 1, r1)
    ]

compilePrimitiveOperation :: (ID, Reg) -> Mam [Instruction]
compilePrimitiveOperation (_id, _) = error $ "compilePrimitiveOperation " ++ _id ++ " not implemented"

compileBoolean :: I64 -> Reg -> Mam [Instruction]
compileBoolean tag reg = do
  temp <- tempReg
  return [
    Alloc (reg, 1),
    MovInt (temp, tag),
    Store (reg, 0, temp)
    ]

compileLambdaExpr :: ID -> Expr -> Reg -> Mam [Instruction]
compileLambdaExpr _id e reg = do
  -- let arg = (_id, BRegister $ Local "arg")
  let freeVars = extractFreeVars _id e
  bindClosure freeVars
  label <- routineLabel
  compileRoutine e label
  compileLexicalClosure label reg

compileLambdaApply :: Expr -> Expr -> Reg -> Mam [Instruction]
compileLambdaApply e1 e2 reg = do
  r1 <- localReg
  r2 <- localReg
  r3 <- localReg
  ins1 <- compileExpr e1 r1
  ins2 <- compileExpr e2 r2
  return $
    ins1 ++
    ins2 ++ [
    Load (r3, r1, tagClosure),
    MovReg (Global "fun", r1),
    MovReg (Global "arg", r2),
    ICall r3,
    MovReg (reg, Global "res")
    ]

compileLexicalClosure :: Label -> Reg -> Mam [Instruction]
compileLexicalClosure label reg = do
  temp <- tempReg
  lreg <- localReg
  varsIns <- compileLexicalClosureVars reg temp
  let len = length varsIns
  return $ [
    Alloc (lreg, 2 + len),
    MovInt (temp, len),
    Store (lreg, tagClosure, temp),
    MovLabel (temp, label),
    Store (lreg, tagNumber, temp)
    ] ++ varsIns

compileLexicalClosureVars :: Reg -> Reg -> Mam [Instruction]
compileLexicalClosureVars reg temp = do
  env' <- getEnv
  compileLexicalClosureVars' env' reg temp

compileLexicalClosureVars' :: Env -> Reg -> Reg -> Mam [Instruction]
compileLexicalClosureVars' [] _ _ = return []
compileLexicalClosureVars' ((_, BRegister _):binds) reg temp = compileLexicalClosureVars' binds reg temp
compileLexicalClosureVars' ((_, BEnclosed n):binds) reg temp = do
  let ins = [Store (reg, n - 1, temp)]
  rest <- compileLexicalClosureVars' binds reg temp
  return $ ins ++ rest


compileRoutine :: Expr -> Label -> Mam ()
compileRoutine e label = do
  let [localRes, globalRes] = [Local "res", Global "res"]
  let [localFun, globalFun] = [Local "fun", Global "fun"]
  let [localArg, globalArg] = [Local "arg", Global "arg"]
  exprIns <- compileExpr e localRes
  mam <- get
  put $ mam { routines = routines mam ++ [
    ILabel label,
    MovReg (localFun, globalFun),
    MovReg (localArg, globalArg)
    ] ++ exprIns ++ [
    MovReg (globalRes, localRes),
    Return
    ]
  }

-- helpers

getCode :: MamState -> [Instruction]
getCode mam = [Jump "start"]
           ++ routines mam
           ++ [ILabel "start"]
           ++ code mam

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

bindClosure :: [ID] -> Mam ()
bindClosure = bindClosure' 1

bindClosure' :: Int -> [ID] -> Mam ()
bindClosure' _ [] = return ()
bindClosure' n (x:xs) = do
  pushEnv [(x, BEnclosed n)]
  bindClosure' (n+1) xs

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
