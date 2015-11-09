module Compiler where

import AbsRules
import AbsM
import ErrM
import Simplifier
import Fresh

type Code = [Line]

compile :: Stmt -> Code
comStm :: Stmt -> Code
comIntExp :: IntExp -> (String, Code)
comBExp :: BExp -> (String, Code)
jumpT :: BExp -> String -> Code
jumpF :: BExp -> String -> Code

comStm (SWhile g p) = [Cmd [top] Nop]
                      ++ c
                      ++ d
                      ++ [Cmd [] (Jmp top)]
                      ++ [Cmd [end] Nop]
                      where
                        top = fresh ()
                        end = fresh ()
                        c = jumpF g end
                        d = comStm p

jumpT (BExp_FALSE) l = []
jumpT (BExp_TRUE) l = [Cmd [] (Jmp l)]
jumpT (BNot g) l = jumpF g l
jumpT (BAnd g h) l = c
                     ++ d
                     [Cmd [end] Nop]
                     where end = fresh ()
                           c = jumpF g end
                           d = jumpT g l
jumpT (BExpPIdent (PIdent (_, v))) = []
