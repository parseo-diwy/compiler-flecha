module MamTypes where

type I64 = Int  -- entero con signo de 64 bits
type U64 = Int  -- entero sin signo de 64 bits
type Label = String
type MamCode = String

data Reg = Global String
         | Local String

instance Show Reg where
  show (Global str) = "@G_" ++ str
  show (Local str) = "$" ++ str

data Binding = BRegister Reg
             | BEnclosed Int

data Val = VInt I64
         | VPtr U64
         | VLoc U64

data Instruction = MovReg Reg Reg
                 | MovInt Reg I64
                 | MovLabel Reg Label
                 | Alloc Reg U64
                 | Load Reg Reg U64
                 | Store Reg U64 Reg
                 | Print Reg
                 | PrintChar Reg
                 | Jump Label
                 | JumpEq Reg Reg Label
                 | JumpLt Reg Reg Label
                 | Add Reg Reg Reg
                 | Sub Reg Reg Reg
                 | Mul Reg Reg Reg
                 | Div Reg Reg Reg
                 | Mod Reg Reg Reg
                 | Call Label
