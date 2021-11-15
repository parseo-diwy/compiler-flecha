module MamarrachoState (compile2) where

import Ast (Program, Expr (..), Definition(..))
import Constants (tagNumber, tagChar)
import Data.Char (ord)
-- import Data.List (intercalate)
-- import Environment (Env, emptyEnv, extendEnv, lookupEnv)
import MamTypes (Instruction (..), MamCode, Reg (..))

import Control.Monad.State
import Data.List (intercalate)

-- get :: State s a                        -- Retrieves the state, like Reader.ask
-- put :: s -> State s ()                  -- Overwrites the existing state
-- runState :: s -> State s a -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s
data MAMState = MAMState {
  nextReg :: Int,
  instructions :: [Instruction]
}

initState :: MAMState
initState = MAMState {
  nextReg = 0,
  instructions = []
}

-- Compilation

compile2 :: Program -> MamCode
compile2 prog = showMam $ getAST $ evalState (compile2' prog) initState

compile2' :: Program -> State MAMState MAMState
compile2' [] = do get
compile2' (def:program) = do
  compileDef def >>= put
  compile2' program

compileDef :: Definition -> State MAMState MAMState
compileDef (Def _id e) = do
  mam <- get
  reg <-localReg
  ins <- compileExpr e reg
  return $ mam {
    instructions = instructions mam ++ ins ++ [MovReg (Global "main", reg)]
  }

compileExpr :: Expr -> Reg -> State MAMState [Instruction]
compileExpr (ExprVar "unsafePrintChar") reg = do
  lreg <- localReg
  return [
    Load      (lreg, reg, 1),
    PrintChar lreg
    ]
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
compileExpr e _ = error $ "NOT implemented " ++ show e

-- helpers

getAST :: MAMState -> [Instruction]
getAST MAMState { instructions = ins } = ins

showMam :: Show a => [a] -> [Char]
showMam = intercalate "\n" . map show

localReg :: State MAMState Reg
localReg = do
  mam <- get
  let n = nextReg mam
  put $ mam { nextReg = n + 1 }
  return $ Local $ "r" ++ show n
