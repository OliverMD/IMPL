

module AbsIntExp where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data IntExp
    = EAdd IntExp IntExp
    | EMul IntExp IntExp
    | ENeg IntExp
    | EInt Integer
    | EVar Ident
  deriving (Eq, Ord, Show, Read)

