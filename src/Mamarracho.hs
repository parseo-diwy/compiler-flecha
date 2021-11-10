module Mamarracho (compile, MamCode) where

import Ast (Definition(..), Expr(..), Program)
import Data.Char (ord)
import Environment (Env, emptyEnv)
import MamDumper (dumpMam)
import MamTypes (MamCode, Binding, Reg(..), Instruction(..))

compile :: Program -> MamCode
compile = dumpMam . compile' emptyEnv

compile' :: Env Binding -> Program -> [Instruction]
compile' _ [] = []
compile' env (def:prog) =
  let (env', mam) = compileDef env def
   in mam ++ compile' env' prog

compileDef :: Env Binding -> Definition -> (Env Binding, [Instruction])
compileDef env (Def _id e) =
  let reg = Local "r0" in
  let (env', ins) = compileExpr env e reg
   in (env', ins ++ [MovReg (Global "main") reg])

compileExpr :: Env Binding -> Expr -> Reg -> (Env Binding, [Instruction])

compileExpr env (ExprVar "unsafePrintChar") reg = let lreg = Local "r1" in (env, [
    Load lreg reg 1,
    PrintChar lreg
  ])

compileExpr env (ExprVar _id)         reg = error "not implemented"
compileExpr env (ExprConstructor _id) reg = error "not implemented"
compileExpr env (ExprNumber n)        reg = error "not implemented"

compileExpr env (ExprChar c)          reg = let lreg = Local "t" in (env, [
    Alloc reg 2,
    MovInt lreg 2,
    Store reg 0 lreg,
    MovInt lreg (ord c),
    Store reg 1 lreg
  ])

compileExpr env (ExprCase e cases)    reg = error "not implemented"
compileExpr env (ExprLet _id e1 e2 )  reg = error "not implemented"
compileExpr env (ExprLambda _id e)    reg = error "not implemented"

compileExpr env (ExprApply e1 e2)     reg =
  let (env2, ins2) = compileExpr env  e2 reg
      (env1, ins1) = compileExpr env2 e1 reg
   in (env1, ins2 ++ ins1)

