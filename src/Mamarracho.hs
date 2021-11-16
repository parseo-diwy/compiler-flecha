module Mamarracho (compile) where

import Ast (Program, Expr (..), Definition(..))
import Constants (tagNumber, tagChar)
import Control.Monad.State (evalState, MonadState(put, get), State)
import Data.Char (ord)
import Data.List (intercalate)
import MamTypes (Instruction (..), MamCode, Reg (..))

-- get       :: State s a                        -- Retrieves the state, like Reader.ask
-- put       :: s -> State s ()                  -- Overwrites the existing state
-- runState  :: s -> State s a -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s
data MamState = MamState {
  nextReg :: Int,
  instructions :: [Instruction]
}

initState :: MamState
initState = MamState {
  nextReg = 0,
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
  return $ mam {
    instructions = instructions mam ++ ins ++ [MovReg (Global "main", reg)]
  }

compileExpr :: Expr -> Reg -> State MamState [Instruction]
compileExpr (ExprVar "unsafePrintInt")  reg = compilePrintPrimitive Print reg
compileExpr (ExprVar "unsafePrintChar") reg = compilePrintPrimitive PrintChar reg
compileExpr (ExprNumber n) reg = do
  let temp = Local "t"
  return [
    Alloc  (reg, 2),
    MovInt (temp, tagNumber),
    Store  (reg, 0, temp),
    MovInt (temp, n),
    Store  (reg, 1, temp)
    ]
compileExpr (ExprChar c)  reg = do
  let temp = Local "t"
  return [
    Alloc  (reg, 2),
    MovInt (temp, tagChar),
    Store  (reg, 0, temp),
    MovInt (temp, ord c),
    Store  (reg, 1, temp)
    ]
compileExpr (ExprApply e1 e2) reg = do
  ins2 <- compileExpr e2 reg
  ins1 <- compileExpr e1 reg
  return $ ins2 ++ ins1
compileExpr e _ = error $ "Expression NOT implemented: " ++ show e

compilePrintPrimitive :: (Reg -> Instruction) -> Reg -> State MamState [Instruction]
compilePrintPrimitive cons reg = do
  lreg <- localReg
  return [
    Load (lreg, reg, 1),
    cons lreg
    ]

-- helpers

getAST :: MamState -> [Instruction]
getAST MamState { instructions = ins } = ins

showMam :: Show a => [a] -> [Char]
showMam = intercalate "\n" . map show

localReg :: State MamState Reg
localReg = do
  mam <- get
  let n = nextReg mam
  put $ mam { nextReg = n + 1 }
  return $ Local $ "r" ++ show n
