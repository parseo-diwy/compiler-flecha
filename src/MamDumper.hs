module MamDumper where

import MamTypes (MamCode, Instruction (..))

dumpMam :: [Instruction] -> MamCode
dumpMam [] = ""
dumpMam (i:ins) = dumpMamInstruction i ++ "\n" ++ dumpMam ins

dumpMamInstruction :: Instruction -> MamCode
dumpMamInstruction (Add reg1 reg2 reg3)     = error "not implemented"
dumpMamInstruction (Alloc reg n)            = "alloc(" ++ show reg ++ ", " ++ show n ++ ")"
dumpMamInstruction (Call label )            = error "not implemented"
dumpMamInstruction (Div reg1 reg2 reg3)     = error "not implemented"
dumpMamInstruction (Jump label)             = error "not implemented"
dumpMamInstruction (JumpEq reg1 reg2 label) = error "not implemented"
dumpMamInstruction (JumpLt reg1 reg2 label) = error "not implemented"
dumpMamInstruction (Load reg1 reg2 n)       = "load(" ++ show reg1 ++ ", " ++ show reg2 ++ ", " ++ show n ++ ")"
dumpMamInstruction (Mod reg1 reg2 reg3)     = error "not implemented"
dumpMamInstruction (MovInt reg n)           = "mov_int(" ++ show reg ++ ", " ++ show n ++ ")"
dumpMamInstruction (MovLabel reg label)     = error "not implemented"
dumpMamInstruction (MovReg reg1 reg2)       = "mov_reg(" ++ show reg1 ++ ", " ++ show reg2 ++ ")"
dumpMamInstruction (Mul reg1 reg2 reg3)     = error "not implemented"
dumpMamInstruction (Print reg)              = "print(" ++ show reg ++ ")"
dumpMamInstruction (PrintChar reg)          = "print_char(" ++ show reg ++ ")"
dumpMamInstruction (Store reg1 n reg2)      = "store(" ++ show reg1 ++ ", " ++ show n ++ ", " ++ show reg2 ++ ")"
dumpMamInstruction (Sub reg1 reg2 reg3)     = error "not implemented"
