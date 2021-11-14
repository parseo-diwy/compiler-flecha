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

data Instruction = MovReg    Reg Reg
                 | MovInt    Reg I64
                 | MovLabel  Reg Label
                 | Alloc     Reg U64
                 | Load      Reg Reg U64
                 | Store     Reg U64 Reg
                 | Print     Reg
                 | PrintChar Reg
                 | Jump      Label
                 | JumpEq    Reg Reg Label
                 | JumpLt    Reg Reg Label
                 | Add       Reg Reg Reg
                 | Sub       Reg Reg Reg
                 | Mul       Reg Reg Reg
                 | Div       Reg Reg Reg
                 | Mod       Reg Reg Reg
                 | Call      Label
                 | ICall     Reg
                 | Return

instance Show Instruction where
  show (MovReg    reg1 reg2)       = "mov_reg"    ++ show (reg1, reg2)
  show (MovInt    reg n)           = "mov_int"    ++ show (reg, n)
  show (MovLabel  reg label)       = "mov_label"  ++ show (reg, label)
  show (Alloc     reg n)           = "alloc"      ++ show (reg, n)
  show (Load      reg1 reg2 n)     = "load"       ++ show (reg1, reg2, n)
  show (Store     reg1 n reg2)     = "store"      ++ show (reg1, n, reg2)
  show (Print     reg)             = "print"      ++ arg1 reg
  show (PrintChar reg)             = "print_char" ++ arg1 reg
  show (Jump      label)           = "jump"       ++ arg1 label
  show (JumpEq    reg1 reg2 label) = "jump_eq"    ++ show (reg1, reg2, label)
  show (JumpLt    reg1 reg2 label) = "jump_lt"    ++ show (reg1, reg2, label)
  show (Add       reg1 reg2 reg3)  = "add"        ++ show (reg1, reg2, reg3)
  show (Sub       reg1 reg2 reg3)  = "sub"        ++ show (reg1, reg2, reg3)
  show (Mul       reg1 reg2 reg3)  = "mul"        ++ show (reg1, reg2, reg3)
  show (Div       reg1 reg2 reg3)  = "div"        ++ show (reg1, reg2, reg3)
  show (Mod       reg1 reg2 reg3)  = "mod"        ++ show (reg1, reg2, reg3)
  show (Call      label)           = "call"       ++ arg1 label
  show (ICall     reg)             = "icall"      ++ arg1 reg
  show Return                      = "return()"

--

arg1 :: Show a => a -> String
arg1 a = "(" ++ show a ++ ")"
