module MamTypes where

type I64 = Int  -- entero con signo de 64 bits
type U64 = Int  -- entero sin signo de 64 bits
type Label   = String
type MamCode = String

data Reg = Global String
         | Local  String

instance Show Reg where
  show (Global str) = "@G_" ++ str
  show (Local  str) = "$" ++ str

data Binding = BRegister Reg
             | BEnclosed Int

data Val = VInt I64
         | VPtr U64
         | VLoc U64

data Instruction = MovReg    (Reg, Reg)
                 | MovInt    (Reg, I64)
                 | MovLabel  (Reg, Label)
                 | Alloc     (Reg, U64)
                 | Load      (Reg, Reg, U64)
                 | Store     (Reg, U64, Reg)
                 | Print     Reg
                 | PrintChar Reg
                 | Jump      Label
                 | JumpEq    (Reg, Reg, Label)
                 | JumpLt    (Reg, Reg, Label)
                 | Add       (Reg, Reg, Reg)
                 | Sub       (Reg, Reg, Reg)
                 | Mul       (Reg, Reg, Reg)
                 | Div       (Reg, Reg, Reg)
                 | Mod       (Reg, Reg, Reg)
                 | Call      Label
                 | ICall     Reg
                 | Return

instance Show Instruction where
  show (MovReg    args)  = "mov_reg"    ++ show args
  show (MovInt    args)  = "mov_int"    ++ show args
  show (MovLabel  args)  = "mov_label"  ++ show args
  show (Alloc     args)  = "alloc"      ++ show args
  show (Load      args)  = "load"       ++ show args
  show (Store     args)  = "store"      ++ show args
  show (Print     reg)   = "print"      ++ arg1 reg
  show (PrintChar reg)   = "print_char" ++ arg1 reg
  show (Jump      label) = "jump"       ++ arg1 label
  show (JumpEq    args)  = "jump_eq"    ++ show args
  show (JumpLt    args)  = "jump_lt"    ++ show args
  show (Add       args)  = "add"        ++ show args
  show (Sub       args)  = "sub"        ++ show args
  show (Mul       args)  = "mul"        ++ show args
  show (Div       args)  = "div"        ++ show args
  show (Mod       args)  = "mod"        ++ show args
  show (Call      label) = "call"       ++ arg1 label
  show (ICall     reg)   = "icall"      ++ arg1 reg
  show Return            = "return()"

--

arg1 :: Show a => a -> String
arg1 a = "(" ++ show a ++ ")"
