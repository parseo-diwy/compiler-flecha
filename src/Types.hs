module Types where

type ID = String

type I64 = Int  -- entero con signo de 64 bits
type U64 = Int  -- entero sin signo de 64 bits
type Label   = String
type MamCode = String

type Env = [(ID, Binding)]
type StackEnv = [Env]

type NumOp = (Reg, Reg, Reg) -> Instruction

data PrimType = PrimPrint | PrimOp | PrimVar

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
  show ins@(MovReg    args)  = "mov_reg"    ++ show args      ++ comment ins
  show ins@(MovInt    args)  = "mov_int"    ++ show args      ++ comment ins
  show ins@(MovLabel  args)  = "mov_label"  ++ argsLabel args ++ comment ins
  show ins@(Alloc     args)  = "alloc"      ++ show args      ++ comment ins
  show ins@(Load      args)  = "load"       ++ show args      ++ comment ins
  show ins@(Store     args)  = "store"      ++ show args      ++ comment ins
  show ins@(Print     reg)   = "print"      ++ argR reg       ++ comment ins
  show ins@(PrintChar reg)   = "print_char" ++ argR reg       ++ comment ins
  show ins@(Jump      label) = "jump"       ++ argL label     ++ comment ins
  show ins@(JumpEq    args)  = "jump_eq"    ++ show args      ++ comment ins
  show ins@(JumpLt    args)  = "jump_lt"    ++ show args      ++ comment ins
  show ins@(Add       args)  = "add"        ++ show args      ++ comment ins
  show ins@(Sub       args)  = "sub"        ++ show args      ++ comment ins
  show ins@(Mul       args)  = "mul"        ++ show args      ++ comment ins
  show ins@(Div       args)  = "div"        ++ show args      ++ comment ins
  show ins@(Mod       args)  = "mod"        ++ show args      ++ comment ins
  show ins@(Call      label) = "call"       ++ argL label     ++ comment ins
  show ins@(ICall     reg)   = "icall"      ++ argR reg       ++ comment ins
  show ins@(ILabel    label) = label ++ ":"                   ++ comment ins
  show ins@(Comment   text)  = "% " ++ text                    ++ comment ins
  show ins@Return            = "return()"                     ++ comment ins

comment :: Instruction -> String
comment (MovReg    (r1, r2)    ) = " % " ++ show r1 ++ " := " ++ show r2
comment (MovInt    (r, n)      ) = " % " ++ show r ++ " := " ++ show n
comment (MovLabel  (r, l)      ) = " % " ++ show r ++ " := VLoc(p), p := mem(" ++ l ++ ")"
comment (Alloc     (r, n)      ) = " % " ++ show r ++ " := arr[" ++ show n ++ "]"
comment (Load      (r1, r2, i) ) = " % " ++ show r1 ++ " := " ++ show r2 ++ "[" ++ show i ++ "]"
comment (Store     (r1, i, r2) ) = " % " ++ show r1 ++ "[" ++ show i ++ "] := " ++ show r2
comment (Print     r           ) = " % print " ++ show r
comment (PrintChar r           ) = " % print_char " ++ show r
comment (Jump      _           ) = ""
comment (JumpEq    (r1, r2, _) ) = " % jump_if " ++ show r1 ++ " := " ++ show r2
comment (JumpLt    (r1, r2, _) ) = " % jump_if " ++ show r1 ++ " < " ++ show r2
comment (Add       (r1, r2, r3)) = " % " ++ show r1 ++ " := " ++ show r2 ++ " + " ++ show r3
comment (Sub       (r1, r2, r3)) = " % " ++ show r1 ++ " := " ++ show r2 ++ " - " ++ show r3
comment (Mul       (r1, r2, r3)) = " % " ++ show r1 ++ " := " ++ show r2 ++ " * " ++ show r3
comment (Div       (r1, r2, r3)) = " % " ++ show r1 ++ " := " ++ show r2 ++ " / " ++ show r3
comment (Mod       (r1, r2, r3)) = " % " ++ show r1 ++ " := " ++ show r2 ++ " % " ++ show r3
comment (Call      l           ) = " % save ret dir & env & jump_to" ++ show l
comment (ICall     r           ) = " % save ret dir & env & jump_to" ++ show r
comment (ILabel    _           ) = ""
comment (Comment   _           ) = ""
comment Return                   = ""

--

argL :: Label -> String
argL label = "(" ++ label ++ ")"

argR :: Reg -> String
argR reg = "(" ++ show reg ++ ")"

argsLabel :: (Reg, Label) -> String
argsLabel (reg, label) = "("  ++ show reg ++ "," ++ label ++ ")"
