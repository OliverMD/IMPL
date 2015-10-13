module SkelIntExp where

-- Haskell module generated by the BNF converter

import AbsIntExp
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transIntExp :: IntExp -> Result
transIntExp x = case x of
  EAdd intexp1 intexp2 -> failure x
  EMul intexp1 intexp2 -> failure x
  ENeg intexp -> failure x
  EInt integer -> failure x
  EVar ident -> failure x
