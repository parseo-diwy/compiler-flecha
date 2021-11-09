module Mamarracho where

import Ast (Program, Definition)
import Environment (Env)

type Reg = String
type MamCode = String

data Binding = BRegister Reg
             | BEnclosed Int

compile :: Env Binding -> Program -> MamCode
compile _ [] = ""
compile env (def:prog) = 
  let (env', mam) = compileDef env def
   in mam ++ "\n" ++ compile env' prog

compileDef :: Env Binding -> Definition -> (Env Binding, MamCode)
compileDef env _ = (env, "NOT IMPLEMENTED")
