module Parlex where

import LexIntExp
import ParIntExp
import AbsIntExp
import ErrM

handleResult :: Err a -> a
handleResult (Ok x) = x
handleResult (Bad s) = error s

pl = handleResult . pIntExp . tokens

simpleIntExp :: IntExp -> IntExp
simpleIntExp (EAdd x (EInt 0)) = simpleIntExp x
simpleIntExp (EAdd (EInt 0) x) = simpleIntExp x
simpleIntExp (EAdd (EInt x) (EInt y)) = EInt (x + y)
simpleIntExp (EAdd x y) = EAdd (simpleIntExp x) (simpleIntExp y)
simpleIntExp (EMul x (EInt 1)) = simpleIntExp x
simpleIntExp (EMul (EInt 1) x) = simpleIntExp x
simpleIntExp (EMul _ (EInt 0)) = EInt 0
simpleIntExp (EMul (EInt 0) _) = EInt 0
simpleIntExp (EMul (EInt x) (EInt y)) = EInt (x * y)
simpleIntExp (EMul x y) = EMul (simpleIntExp x) (simpleIntExp y)
simpleIntExp (ENeg (ENeg x)) = simpleIntExp x
simpleIntExp (ENeg x) = ENeg (simpleIntExp x)
simpleIntExp (ENeg (EInt x)) = EInt (-x)
simpleIntExp (EVar s) = EVar s
simpleIntExp (EInt x) = EInt x
