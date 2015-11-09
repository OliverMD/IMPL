module Simplify where
import AbsRules

simplify :: Stmt -> Stmt
simplify (STerm (SSkip) p) = simplify p
simplify (STerm p (SSkip)) = simplify p
simplify (SIF (BExp_TRUE) t s) = simplify t
simplify (SIF (BExp_FALSE) t s) = simplify s
