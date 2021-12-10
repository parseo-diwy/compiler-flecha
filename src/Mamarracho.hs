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
  debug $ "def " ++ _id
  compileExpr e greg
  extendEnv (_id, BRegister greg)
  return ()

compileExpr :: Expr -> Reg -> Mam ()
compileExpr (ExprVar _id)            reg = compileVariable _id reg
compileExpr (ExprNumber n)           reg = compilePrimitiveValue tagNumber n reg
compileExpr (ExprChar c)             reg = compilePrimitiveValue tagChar (ord c) reg
compileExpr (ExprApply e1 e2)        reg = do
  case e1 of
    (ExprLambda _id e1') -> compileLambda _id e1' (Just e2) reg
    (ExprApply (ExprVar cons) e1') -> do
      compileOperation cons e1' e2 reg
    _ -> do
      compileApplication e1 e2 reg
      return ()
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
  debug _id
  addCode [
    Load (lreg, reg, 1),
    printer lreg
    ]

compileVarValue :: ID -> Reg -> Mam ()
compileVarValue _id reg = do
  greg <- lookupEnvRegister _id
  debug $ _id ++ "=" ++ show greg
  addCode [MovReg (reg, greg)]

compilePrimitiveValue :: TagType -> Int -> Reg -> Mam ()
compilePrimitiveValue tag val reg = do
  temp <- tempReg
  debug $ show reg ++ "=" ++ "[" ++ show tag ++ "," ++ show val ++ "]"
  addCode [
    Alloc  (reg, 2),
    MovInt (temp, tag),
    Store  (reg, 0, temp),
    MovInt (temp, val),
    Store  (reg, 1, temp)
    ]

compileOperation :: ID -> Expr -> Expr -> Reg -> Mam ()
compileOperation "ADD" e1 e2 reg = compileArithmeticOperation Add e1 e2 reg
compileOperation "SUB" e1 e2 reg = compileArithmeticOperation Sub e1 e2 reg
compileOperation "MUL" e1 e2 reg = compileArithmeticOperation Mul e1 e2 reg
compileOperation "DIV" e1 e2 reg = compileArithmeticOperation Div e1 e2 reg
-- compileOperation _id   _   _   _ = error $ "CompileOperation: Invalid Constructor '" ++ show _id ++ "'"
compileOperation _id   _   _   _ = do
  n <- lookupEnvRegister _id
  addCode [Comment $ show n]
  -- case n of
  --   Just _ -> addCode [Comment "tu vieja"]
  --   _ -> do
  --     env' <- getStackEnv
  --     error $
  --       "CompileOperation: Invalid Constructor '" ++ show _id ++ "'"
  --       ++ "\n"
  --       ++ "StackEnv: "
  --       ++ show env'

compileArithmeticOperation :: NumOp -> Expr -> Expr -> Reg -> Mam ()
compileArithmeticOperation mamOp e1 e2 reg = do
  r1 <- localReg
  r2 <- localReg
  temp <- tempReg
  compileExpr e1 r1
  compileExpr e2 r2
  debug $ show r1 ++ " " ++ nameOf (mamOp (r1, r1, r2)) ++ " " ++ show r2
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
compilePrimitiveOperation _id _ = error $
  "compilePrimitiveOperation " ++ _id ++ " not implemented"

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
      debug $ "(" ++ show r1 ++ ") (" ++ show r2 ++ ")"
      addCode [
        MovReg (Global "fun", r1),
        MovReg (Global "arg", r2),
        Load   (r3, Global "fun", 1),
        ICall  r3,
        MovReg (reg, Global "res")
        ]
compileApplication e1 e2 reg = error $ "error on compileApplication" ++ show (e1, e2, reg)

compileBoolean :: I64 -> Reg -> Mam ()
compileBoolean tag reg = do
  temp <- tempReg
  debug "true"
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
  debug $ "\\ " ++ _id ++ " -> " ++ show r1
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
  varsIns <- compileLexicalClosureVars greg temp
  let len = length varsIns
  debug $ "LexicalClosureVars (" ++ show len ++") " ++ show lreg ++ " " ++ show greg
  addCode [
    Alloc (greg, 2 + len),
    MovInt (temp, 3),
    Store (greg, 0, temp),
    MovLabel (temp, label),
    Store (greg, tagNumber, temp),
    MovReg (lreg, greg)
    ]
  addCode varsIns

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
  lreg <- localReg
  prevStack <- getStack
  switchToRoutineStack
  -- rtn:
  addCode [
    ILabel label,
    MovReg (Local "fun", Global "fun"),
    MovReg (Local "arg", Global "arg")
    ]
  compileExpr e lreg
  debug $ "return " ++ show lreg
  addCode [
    MovReg (Global "res", lreg),
    Return
    ]
  switchStack prevStack

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

bindClosureEnv :: [ID] -> Mam ()
bindClosureEnv = bindClosureEnv' 1

bindClosureEnv' :: Int -> [ID] -> Mam ()
bindClosureEnv' _ [] = return ()
bindClosureEnv' n (x:xs) = do
  extendEnv (x, BEnclosed n)
  bindClosureEnv' (n+1) xs

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

nameOf :: Instruction -> String
nameOf mamOp = case mamOp of
  Add (_, _ ,_) -> "+"
  Sub (_, _ ,_) -> "-"
  Mul (_, _ ,_) -> "*"
  Div (_, _ ,_) -> "/"
  _ -> "??"