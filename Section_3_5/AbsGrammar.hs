

module AbsGrammar where

-- Haskell module generated by the BNF converter




data A = AAdd A A | AMul A A | ANum Integer | ABrk A
  deriving (Eq, Ord, Show, Read)

data B
    = BAdd B B | B01 B | BMul B B | B12 B | BNum Integer | BBrk B
  deriving (Eq, Ord, Show, Read)

