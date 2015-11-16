

module AbsM where

-- Haskell module generated by the BNF converter




data Line = Cmd [String] Instruction
  deriving (Eq, Ord, Show, Read)

data Instruction
    = Nop
    | Hlt
    | Lim String Integer
    | Mov String String
    | Add String String String
    | Mul String String String
    | Neg String String
    | Prn String
    | Jmp String
    | Beq String String String
    | Blt String String String
    | Lor String String String
    | Bne String String String
    | Bge String String String
    | AddI String String Integer
    | MulI String String Integer
    | NegI String Integer
    | BeqI String Integer String
    | BltI String Integer String
  deriving (Eq, Ord, Show, Read)

