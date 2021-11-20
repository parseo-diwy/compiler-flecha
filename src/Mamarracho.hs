module Mamarracho (compile) where

import Ast (Program, Expr(..), Definition(..), ID)
import Constants (tagChar, tagNumber, TagType)
import Control.Monad.State (execState, MonadState(put, get), State)
import Data.Char (ord)
import Data.List (intercalate, find)
import MamTypes (Instruction(..), Binding(..), Reg(..), MamCode, Tag(..), I64)

-- get       :: State s a                        -- Retrieves the state, like Reader.ask
-- put       :: s -> State s ()                  -- Overwrites the existing state
-- runState  :: s -> State s a -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s
data MamState = MamState {
  env     :: [[(ID, Binding)]],
  code    :: [Instruction],
  nextReg :: Int
}

initState :: MamState
initState = MamState {
  env     = [],
  code    = [],
  nextReg = 0
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
  let greg = Global _id
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
  let temp = Local "temp"
  ins1 <- compileExpr e1 temp
  pushEnv [(_id, BRegister temp)]
  ins2 <- compileExpr e2 reg
  popEnv
  return $ ins1 ++ ins2
compileExpr e _ = error $ "Expression NOT implemented: " ++ show e

compileVariable :: (ID , Reg) -> Mam [Instruction]
compileVariable (_id, reg) = do
  if isPrimitivePrinter _id
    then compilePrimitivePrint (_id, reg)
    else compileVarValue (_id, reg)

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
  let [env'] = env mam
  let greg = findVarReg _id env'
  return [MovReg (reg, greg)]

compilePrimitiveValue :: (TagType, Int, Reg) -> Mam [Instruction]
compilePrimitiveValue (tag, val, reg) = do
  let temp = Local "t"
  return [
    Alloc  (reg, 2),
    MovInt (temp, tag),
    Store  (reg, 0, temp),
    MovInt (temp, val),
    Store  (reg, 1, temp)
    ]

compileBoolean :: I64 -> Reg -> Mam [Instruction]
compileBoolean tag reg = do
  let temp = Local "t"
  return [
    Alloc (reg, 1),
    MovInt (temp, tag),
    Store (reg, 0, temp)
    ]

-- helpers

getCode :: MamState -> [Instruction]
getCode MamState { code = c } = c

showCode :: Show a => [a] -> [Char]
showCode = intercalate "\n" . map show

isPrimitivePrinter :: String -> Bool
isPrimitivePrinter _id = _id `elem` ["unsafePrintInt", "unsafePrintChar"]

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

findVarReg :: ID -> [(ID, Binding)] -> Reg
findVarReg _id _env =
  let bind = find (\b -> _id == fst b) _env
   in case bind of
    Just (_, BRegister greg) -> greg
    _ -> error $ "'"++ _id ++"' is not defined"

localReg :: Mam Reg
localReg = do
  mam <- get
  let n = nextReg mam
  put $ mam { nextReg = n + 1 }
  return $ Local $ "r" ++ show n
