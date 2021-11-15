module Mamarracho (compile, MamCode) where

import Ast (Definition (..), Expr (..), Program)
import Constants (tagNumber, tagChar)
import Data.Char (ord)
import Data.List (intercalate)
import Environment (Env, emptyEnv, extendEnv, lookupEnv)
import MamTypes (Binding (..), Instruction (..), MamCode, Reg (..))

compile :: Program -> MamCode
compile = intercalate "\n" . map show . compile' startingEnv

compile' :: Env Binding -> Program -> [Instruction]
compile' _ [] = []
compile' env (def : prog) =
  let (env', mam) = compileDef env def
   in mam ++ compile' env' prog

compileDef :: Env Binding -> Definition -> (Env Binding, [Instruction])
compileDef env (Def _id e) =
  let (env1, reg) = freshLocalReg env
   in let (env2, ins) = compileExpr env1 e reg
       in (env2, ins ++ [MovReg (Global "main", reg)])

compileExpr :: Env Binding -> Expr -> Reg -> (Env Binding, [Instruction])
compileExpr env (ExprVar "unsafePrintChar") reg = compilePrintPrimitive PrintChar env reg
compileExpr env (ExprVar "unsafePrintInt") reg = compilePrintPrimitive Print env reg
compileExpr env (ExprVar _id)         reg = error "not implemented"
compileExpr env (ExprConstructor _id) reg = error "not implemented"
compileExpr env (ExprNumber n)        reg = 
  let lreg = Local "t"
   in (env,
        [ Alloc (reg, 2),
          MovInt (lreg, tagNumber),
          Store (reg, 0, lreg),
          MovInt (lreg, n),
          Store (reg, 1, lreg)
        ]
      )
compileExpr env (ExprChar c)          reg =
  let lreg = Local "t"
   in (env,
        [ Alloc (reg, 2),
          MovInt (lreg, tagChar), 
          Store (reg, 0, lreg),
          MovInt (lreg, ord c),
          Store (reg, 1, lreg)
        ]
      )
compileExpr env (ExprCase e cases)  reg = error "not implemented"
compileExpr env (ExprLet _id e1 e2) reg = error "not implemented"
compileExpr env (ExprLambda _id e)  reg = error "not implemented"
compileExpr env (ExprApply e1 e2)   reg =
  let (env2, ins2) = compileExpr env e2 reg
      (env1, ins1) = compileExpr env2 e1 reg
   in (env1, ins2 ++ ins1)

--

_localRegName :: String
_localRegName = "next_local_reg"

startingEnv :: Env Binding
startingEnv = extendEnv emptyEnv _localRegName (BEnclosed 0)

freshLocalReg :: Env Binding -> (Env Binding, Reg)
freshLocalReg env =
  let regName = _localRegName
      bind = lookupEnv env regName
   in case bind of
        (BEnclosed n) ->
          let next = BEnclosed $ n + 1
              reg = Local $ "r" ++ show n
              env' = extendEnv env regName next
           in (env', reg)
        _ -> error "invalid lookupEnv result in freshLocalReg"


compilePrintPrimitive :: (Reg -> Instruction) ->  Env Binding -> Reg ->  (Env Binding, [Instruction])
compilePrintPrimitive cons env reg =   
  let (env', lreg) = freshLocalReg env
   in (env',
        [ Load (lreg, reg, 1),
          cons lreg
        ]
      )
