module Parlex where

import LexIntExp
import ParIntExp
import AbsIntExp
import ErrM
import PrintIntExp

handleResult :: Err a -> a
handleResult (Ok x) = x
handleResult (Bad s) = error s

pl = simpleIntExp . handleResult . pIntExp . tokens

simpleIntExp :: IntExp -> IntExp
simpleIntExp (EVar s) = EVar s
simpleIntExp (EInt x) = EInt x

simpleIntExp (EAdd x (EInt 0)) = simpleIntExp x
simpleIntExp (EAdd (EInt 0) x) = simpleIntExp x
simpleIntExp (EAdd (EInt x) (EInt y)) = EInt (x + y)
simpleIntExp (EAdd x y) = f (simpleIntExp x) (simpleIntExp y)
                          where f (EInt w) (EAdd (EVar s) (EInt q)) = EAdd (EInt (w+q)) (EVar s)
                                f (EAdd (EVar s) (EInt q)) (EInt w) = EAdd (EVar s) (EInt (w+q))
                                f q w  = EAdd q w
simpleIntExp (EMul x (EInt 1)) = simpleIntExp x
simpleIntExp (EMul (EInt 1) x) = simpleIntExp x
simpleIntExp (EMul _ (EInt 0)) = EInt 0
simpleIntExp (EMul (EInt 0) _) = EInt 0
simpleIntExp (EMul (EInt x) (EInt y)) = EInt (x * y)
simpleIntExp (EMul x y) = EMul (simpleIntExp x) (simpleIntExp y)

simpleIntExp (ENeg (ENeg x)) = simpleIntExp x
simpleIntExp (ENeg (EInt x)) = EInt (-x)
simpleIntExp (ENeg x) = ENeg (simpleIntExp x)

ie2scheme :: IntExp -> String
ie2scheme (EInt x) = show x
ie2scheme (EVar s) = s
ie2scheme (EAdd x y) = "(+ " ++ (ie2scheme x) ++ " " ++ (ie2scheme y) ++ ")"
ie2scheme (EMul x y) = "(* " ++ (ie2scheme x) ++ " " ++ (ie2scheme y) ++ ")"
ie2scheme (ENeg x) = "(- " ++ (ie2scheme x) ++ ")"
