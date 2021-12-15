module Types where

type ID = String

type I64 = Int  -- entero con signo de 64 bits
type U64 = Int  -- entero sin signo de 64 bits
type Label   = String
type MamCode = String

type Env = [(ID, Binding)]
type StackEnv = [Env]

type NumOp = (Reg, Reg, Reg) -> Instruction

data VarType = TPrinter | TOper | TVar

data Reg = Global String
         | Local  String

instance Show Reg where
  show (Global str) = "@" ++ str
  show (Local  str) = "$" ++ str

data Binding = BRegister Reg
             | BEnclosed Int

instance Show Binding where
  show (BRegister reg) = show reg
  show (BEnclosed   n) = "<" ++ show n ++ ">"

data Val = VInt I64
         | VPtr U64
         | VLoc U64

data Tag = TInt     I64
         | TTrue    I64
         | TFalse   I64
         | TChar    I64
         | TClosure I64
         | TNil     I64
         | TCons    I64

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
                 | ILabel    Label
                 | Comment   String
                 | Return

instance Show Instruction where
  show (MovReg     args) = "mov_reg"    ++ show args
  show (MovInt     args) = "mov_int"    ++ show args
  show (MovLabel   args) = "mov_label"  ++ argsLabel args
  show (Alloc      args) = "alloc"      ++ show args
  show (Load       args) = "load"       ++ show args
  show (Store      args) = "store"      ++ show args
  show (Print       reg) = "print"      ++ argR reg
  show (PrintChar   reg) = "print_char" ++ argR reg
  show (Jump      label) = "jump"       ++ argL label
  show (JumpEq     args) = "jump_eq"    ++ show args
  show (JumpLt     args) = "jump_lt"    ++ show args
  show (Add        args) = "add"        ++ show args
  show (Sub        args) = "sub"        ++ show args
  show (Mul        args) = "mul"        ++ show args
  show (Div        args) = "div"        ++ show args
  show (Mod        args) = "mod"        ++ show args
  show (Call      label) = "call"       ++ argL label
  show (ICall       reg) = "icall"      ++ argR reg
  show (ILabel    label) = label ++ ":"
  show (Comment    text) = "% " ++ text
  show Return            = "return()"

--

argL :: Label -> String
argL label = "(" ++ label ++ ")"

argR :: Reg -> String
argR reg = "(" ++ show reg ++ ")"

argsLabel :: (Reg, Label) -> String
argsLabel (reg, label) = "("  ++ show reg ++ "," ++ label ++ ")"
