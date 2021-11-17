module Mamarracho (compile) where

import Ast (Program, Expr(..), Definition(..), ID)
import Constants (tagChar, tagNumber, tagTrue, TagType)
import Control.Monad.State (evalState, MonadState(put, get), State)
import Data.Char (ord)
import Data.List (intercalate)
import MamTypes (Instruction(..), Binding(..), Reg(..), MamCode)

-- get       :: State s a                        -- Retrieves the state, like Reader.ask
-- put       :: s -> State s ()                  -- Overwrites the existing state
-- runState  :: s -> State s a -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s
data MamState = MamState {
  nextReg :: Int,
  bindings :: [Binding],
  instructions :: [Instruction]
}

initState :: MamState
initState = MamState {
  nextReg = 0,
  bindings = [],
  instructions = []
}

-- Compilation

compile :: Program -> MamCode
compile prog = showMam $ getAST $ evalState (compile' prog) initState

compile' :: Program -> State MamState MamState
compile' [] = do get
compile' (def:program) = do
  compileDef def >>= put
  compile' program

compileDef :: Definition -> State MamState MamState
compileDef (Def _id e) = do
  mam <- get
  reg <-localReg
  ins <- compileExpr e reg
  let greg = Global _id
  return $ mam {
    bindings = bindings mam ++ [BRegister greg],
    instructions = instructions mam ++ ins ++ [MovReg (greg, reg)]
  }

compileExpr :: Expr -> Reg -> State MamState [Instruction]
compileExpr (ExprVar _id)            reg = compileVariable (_id, reg)
compileExpr (ExprNumber n)           reg = compilePrimitiveValue (tagNumber,   n, reg)
compileExpr (ExprChar c)             reg = compilePrimitiveValue (tagChar, ord c, reg)
compileExpr (ExprApply e1 e2)        reg = do
  ins2 <- compileExpr e2 reg
  ins1 <- compileExpr e1 reg
  return $ ins2 ++ ins1
compileExpr (ExprConstructor "True") reg = do
  let temp = Local "t"
  return [
    Alloc (reg, 1),
    MovInt (temp, tagTrue),
    Store (reg, 0, temp)
    ]
compileExpr e _ = error $ "Expression NOT implemented: " ++ show e

compileVariable :: (ID , Reg) -> State MamState [Instruction]
compileVariable (_id, reg) = do
  if isPrimitivePrinter _id
    then compilePrimitivePrint (_id, reg)
    else compileVarValue (_id, reg)

compilePrimitivePrint :: (String, Reg) -> State MamState [Instruction]
compilePrimitivePrint (_id, reg) = do
  let printer = if _id == "unsafePrintChar" then PrintChar else Print
  lreg <- localReg
  return [
    Load (lreg, reg, 1),
    printer lreg
    ]

compileVarValue :: (ID, Reg) -> State MamState [Instruction]
compileVarValue (_id, reg) = do
  let greg = Global _id
  return [
    MovReg (reg, greg)
    ]

compilePrimitiveValue :: (TagType, Int, Reg) -> State MamState [Instruction]
compilePrimitiveValue (tag, val, reg) = do
  let temp = Local "t"
  return [
    Alloc  (reg, 2),
    MovInt (temp, tag),
    Store  (reg, 0, temp),
    MovInt (temp, val),
    Store  (reg, 1, temp)
    ]

-- helpers

getAST :: MamState -> [Instruction]
getAST MamState { instructions = ins } = ins

showMam :: Show a => [a] -> [Char]
showMam = intercalate "\n" . map show

isPrimitivePrinter :: String -> Bool
isPrimitivePrinter _id = _id `elem` ["unsafePrintInt", "unsafePrintChar"]

localReg :: State MamState Reg
localReg = do
  mam <- get
  let n = nextReg mam
  put $ mam { nextReg = n + 1 }
  return $ Local $ "r" ++ show n
