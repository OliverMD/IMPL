module Compiler where

import AbsRules
import Simplifier

compile :: Stmt -> Code
comStm :: Stmt -> Code
comIntExp :: IntExp -> (String, Code)
comBExp :: BExp -> (String, Code)
jumpT :: BExp -> String -> Code
jumpF :: BExp -> String -> Code

comStm (SWhile g p) = 
