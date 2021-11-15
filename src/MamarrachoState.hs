module MamarrachoState where

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
  definitions :: Program,
  instructions :: [Instruction]
}

startingState :: MAMState
startingState = MAMState { nextReg = 0, definitions = [], instructions = [] }

compile2 :: Program -> MamCode
compile2 prog =
  let a = evalState (compile2' prog) startingState
      ins = extract a
   in intercalate "\n" $ map show ins

extract :: MAMState -> [Instruction]
extract MAMState {instructions = ins} = ins

compile2' :: Program -> State MAMState MAMState
compile2' [] = do get
compile2' (def:program) = do
  mam <- compileDef def
  put mam
  compile2' program

compileDef :: Definition -> State MAMState MAMState
compileDef (Def _id e) = do
  mam <- get
  let n = nextReg mam
  let lreg = Local ("r" ++ show n)
  put $ mam { nextReg = n + 1 }
  ins <- compileExpr e lreg
  let i = instructions mam
  return $ mam { instructions = i ++ ins ++ [MovReg (Global "main") lreg] }

compileExpr :: Expr -> Reg -> State MAMState [Instruction]
compileExpr (ExprVar "unsafePrintChar") reg = do
  mam <- get
  let n = nextReg mam
  let lreg = Local ("r" ++ show n)
  return [Load lreg reg 1, PrintChar lreg]
compileExpr (ExprNumber n) reg = do
  let lreg = Local "t"
  return [ Alloc reg 2,
          MovInt lreg tagNumber,
          Store reg 0 lreg,
          MovInt lreg n,
          Store reg 1 lreg
        ]
compileExpr (ExprChar c)  reg = do
  let lreg = Local "t"
  return [ Alloc reg 2,
          MovInt lreg tagChar, 
          Store reg 0 lreg,
          MovInt lreg (ord c),
          Store reg 1 lreg
        ]
compileExpr (ExprApply e1 e2) reg = do
  ins2 <-  compileExpr e2 reg
  ins1 <- compileExpr e1 reg
  return $ ins2 ++ ins1
compileExpr e _ = error $ "NOT implemented " ++ show e
