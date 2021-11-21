module Mamarracho (compile) where

import Ast (Program, Expr(..), Definition(..), ID)
import Constants (tagChar, tagNumber, TagType, tagClosure)
import Control.Monad.State (execState, MonadState(put, get), State)
import Data.Char (ord)
import Data.List (intercalate, find)
import MamTypes
import Debug.Trace (traceM, traceShowM)

-- get       :: State s a                        -- Retrieves the state, like Reader.ask
-- put       :: s -> State s ()                  -- Overwrites the existing state
-- runState  :: s -> State s a -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s
data MamState = MamState {
  env     :: StackEnv,
  code    :: [Instruction],
  nextReg :: Int,
  nextRtn :: Int
}

initState :: MamState
initState = MamState {
  env     = [],
  code    = [],
  nextReg = 0,
  nextRtn = 0
}

-- Compilation

type Mam = State MamState

compile :: Program -> MamCode
compile prog = showCode $ getCode $ execState (compile' prog) initState

compile' :: Program -> Mam ()
compile' []            = return ()
compile' (def:program) = do
  compileDef def
  compile' program

compileDef :: Definition -> Mam ()
compileDef (Def _id e) = do
  mam <- get
  reg <-localReg
  _code <- compileExpr e reg
  let greg = Global $ "G_" ++ _id
  put $ mam {
    env  = env  mam ++ [[(_id, BRegister greg)]],
    code = code mam ++ _code ++ [MovReg (greg, reg)]
  }
  return ()

compileExpr :: Expr -> Reg -> Mam [Instruction]
compileExpr (ExprVar _id)            reg = compileVariable (_id, reg)
compileExpr (ExprNumber n)           reg = compilePrimitiveValue (tagNumber,   n, reg)
compileExpr (ExprChar c)             reg = compilePrimitiveValue (tagChar, ord c, reg)
compileExpr (ExprApply e1 e2)        reg = do
  ins2 <- compileExpr e2 reg
  ins1 <- compileExpr e1 reg
  return $ ins2 ++ ins1
compileExpr (ExprConstructor _id) reg = do
  case tagOf _id of
    TTrue  tag -> compileBoolean tag reg
    TFalse tag -> compileBoolean tag reg
    _ -> error $ "ExprConstructor " ++ _id ++ " NOT implemented"
compileExpr (ExprLet _id e1 e2) reg = do
  temp <- localReg
  ins1 <- compileExpr e1 temp
  pushEnv [(_id, BRegister temp)]
  ins2 <- compileExpr e2 reg
  popEnv
  return $ ins1 ++ ins2
compileExpr (ExprLambda _id e) reg = do
  closureVars <- extractFreeVars _id e []
  _ <- traceShowM e
  _ <- traceShowM closureVars
  label  <- routineLabel
  lexIns <- compileLexicalClosure closureVars label reg
  rtnIns <- compileRoutine e label
  return $ lexIns ++ rtnIns

compileExpr e _ = error $ "Expression NOT implemented: " ++ show e

compileVariable :: (ID , Reg) -> Mam [Instruction]
compileVariable (_id, reg) = do
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
  mam <- get
  greg <- findVarReg _id $ env mam
  return [MovReg (reg, greg)]

compilePrimitiveValue :: (TagType, Int, Reg) -> Mam [Instruction]
compilePrimitiveValue (tag, val, reg) = do
  temp <- tempReg
  return [
    Alloc  (reg, 2),
    MovInt (temp, tag),
    Store  (reg, 0, temp),
    MovInt (temp, val),
    Store  (reg, 1, temp)
    ]

compilePrimitiveOperation :: (ID, Reg) -> Mam [Instruction]
compilePrimitiveOperation (_id, _) = return []
-- compilePrimitiveOperation (_id, _) = error "compilePrimitiveOperation NOT implemented yet"

compileBoolean :: I64 -> Reg -> Mam [Instruction]
compileBoolean tag reg = do
  temp <- tempReg
  return [
    Alloc (reg, 1),
    MovInt (temp, tag),
    Store (reg, 0, temp)
    ]

compileLexicalClosure :: [ID] -> Label -> Reg -> Mam [Instruction]
compileLexicalClosure closureVars label reg = do
  temp <- tempReg
  let len = length closureVars
  traceM $ "closureVars = " ++ show closureVars
  varsIns <- compileLexicalClosureVars closureVars 2 reg temp
  return $ [
    Alloc (reg, 2 + len),
    MovInt (temp, len),
    Store (reg, tagClosure, temp),
    MovLabel (temp, label),
    Store (reg, tagNumber, temp)] ++ varsIns

compileLexicalClosureVars :: [ID] -> Int -> Reg -> Reg -> Mam [Instruction]
compileLexicalClosureVars [] _ _ _ = return []
compileLexicalClosureVars (_id:_ids) n reg temp = do
  mam <- get
  regVar <- findVarReg _id (env mam)
  let ins = [MovReg (temp, regVar), Store (reg, n, temp)]
  rest <- compileLexicalClosureVars _ids (n+1) reg temp
  return $ ins ++ rest


compileRoutine :: Expr -> Label -> Mam [Instruction]
compileRoutine e label = do
  let resLocReg  = Local  "res"
  let resGlobReg = Global "res"
  let funLocReg  = Local  "fun"
  let funGlobReg = Global "fun"
  let argLocReg  = Local  "arg"
  let argGlobReg = Global "arg"
  exprIns <- compileExpr e resLocReg
  return $ [
    ILabel label,
    MovReg (funLocReg, funGlobReg),
    MovReg (argLocReg, argGlobReg)
    ] ++ exprIns ++ [
    MovReg (resGlobReg, resLocReg),
    Return
    ]

-- helpers

getCode :: MamState -> [Instruction]
getCode MamState { code = c } = c

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

pushEnv :: [(ID, Binding)] -> Mam ()
pushEnv env' = do
  mam <- get
  put $ mam { env = env' : env mam }

popEnv :: Mam ()
popEnv = do
  mam <- get
  put $ mam { env = tail $ env mam }

findVarReg :: ID -> StackEnv -> Mam Reg
findVarReg _id [] = return $ Local "y"
-- findVarReg _id [] = error $ "'"++ _id ++"' is not defined"
findVarReg _id (env':envs) = do
  let bind = find (\b -> _id == fst b) env'
  case bind of
    Just (_, BRegister greg) -> return greg
    _ -> findVarReg _id envs

extractFreeVars :: ID -> Expr -> [ID] -> Mam [ID]
extractFreeVars x (ExprVar _id) vars
  | isPrimitive _id || x == _id = return vars
  | otherwise = return $ vars ++ [_id]
extractFreeVars _ (ExprConstructor _id) vars = return vars
extractFreeVars _ (ExprNumber _)        vars = return vars
extractFreeVars _ (ExprChar _)          vars = return vars
extractFreeVars x (ExprCase e _)    vars = extractFreeVars x e vars
extractFreeVars x (ExprLet _id e1 e2)   vars = do
  vars' <- extractFreeVars x e1 vars
  extractFreeVars x e2 vars'
extractFreeVars x (ExprLambda _id e)    vars = extractFreeVars x e vars
extractFreeVars x (ExprApply e1 e2)     vars = do
  vars' <- extractFreeVars x e1 vars
  extractFreeVars x e2 vars'

tempReg :: Mam Reg
tempReg = return $ Local "temp"

routineLabel :: Mam Label
routineLabel = do
  mam <- get
  let n = nextRtn mam
  put $ mam { nextRtn = n + 1 }
  return $ "rtn_" ++ show n

localReg :: Mam Reg
localReg = do
  mam <- get
  let n = nextReg mam
  put $ mam { nextReg = n + 1 }
  return $ Local $ "r" ++ show n
