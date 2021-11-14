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

instance Show Instruction where
  show (Add reg1 reg2 reg3)     = error "not implemented"
  show (Alloc reg n)            = "alloc(" ++ show reg ++ ", " ++ show n ++ ")"
  show (Call label )            = error "not implemented"
  show (Div reg1 reg2 reg3)     = error "not implemented"
  show (Jump label)             = error "not implemented"
  show (JumpEq reg1 reg2 label) = error "not implemented"
  show (JumpLt reg1 reg2 label) = error "not implemented"
  show (Load reg1 reg2 n)       = "load(" ++ show reg1 ++ ", " ++ show reg2 ++ ", " ++ show n ++ ")"
  show (Mod reg1 reg2 reg3)     = error "not implemented"
  show (MovInt reg n)           = "mov_int(" ++ show reg ++ ", " ++ show n ++ ")"
  show (MovLabel reg label)     = error "not implemented"
  show (MovReg reg1 reg2)       = "mov_reg(" ++ show reg1 ++ ", " ++ show reg2 ++ ")"
  show (Mul reg1 reg2 reg3)     = error "not implemented"
  show (Print reg)              = "print(" ++ show reg ++ ")"
  show (PrintChar reg)          = "print_char(" ++ show reg ++ ")"
  show (Store reg1 n reg2)      = "store(" ++ show reg1 ++ ", " ++ show n ++ ", " ++ show reg2 ++ ")"
  show (Sub reg1 reg2 reg3)     = error "not implemented"
