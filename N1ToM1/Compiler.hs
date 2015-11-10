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

comIntExp (IntExpPIdent (PIdent (_, v))) = (res, c)
               where res = fresh ()
                     c = [Cmd [] (Mov res v)]
comIntExp (IntExpInteger n) = (res, c)
                              where res = fresh ()
                                    c = [Cmd [] (Lim res n)]
comIntExp (IAdd a b) = (res, c)
                       where res = fresh ()
                             c = d
                                 ++ e
                                 ++ [Cmd [] (Add res v u)]
                                 where (v, d) = comIntExp a
                                       (u, e) = comIntExp b

comIntExp (IMul a b) = (res, c)
                       where res = fresh ()
                             c = d
                                 ++ e
                                 ++ [Cmd [] (Mul res v u)]
                                 where (v, d) = comIntExp a
                                       (u, e) = comIntExp b

comIntExp (INeg a b) = (res, c)
                       where res = fresh ()
                             c = d
                                 ++ e
                                 ++ [Cmd [] (Add res v u)]
                                 where (v, d) = comIntExp a
                                       (u, e) = comIntExp (IMul b (IntExpInteger -1))


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

comStm (SSkip) = []
comStm (SIF g p h) = c
                     ++ d
                     ++ [Cmd [] (Jmp end)]
                     ++ [Cmd [els] Nop]
                     ++ e
                     ++ [Cmd [end] Nop]
                     where
                       els = fresh ()
                       end = fresh ()
                       c = jumpF g els
                       d = comStm p
                       e = comStm h
comStm (SParen p) = comStm p
comStm (SBlock i t p) = comStm p
comStm (STerm p k) = c
                     ++ d
                     where c = comStm p
                           d = comStm k

comStm (SPnt exp) = c
                    ++ [Cmd [] (Prn v)]
                    where (v, c) = comIntExp exp

comStm (SIAss (PIdent (_,v)) (IAdd a b)) = c
                                           ++ d
                                           ++ [Cmd [] (Add v t u)]
                                           where
                                             (t, c) = comIntExp a
                                             (u, d) = comIntExp b
comStm (SIAss (PIdent (_,v)) (IAdd a (IntExpInteger n))) = c
                                                           ++ [Cmd [] (AddI v u n)]
                                                           where (u, c) = comIntExp a

comStm (SIAss (PIdent (_,v)) (IAdd (IntExpInteger n) a)) = c
                                                           ++ [Cmd [] (AddI vu n)]
                                                           where (u, c) = comIntExp a

comStm (SIAss (PIdent (_,v)) (IMul a (IntExpInteger n))) = c
                                                           ++ [Cmd [] (MulI v u n)]
                                                           where (u, c) = comIntExp a

comStm (SIAss (PIdent (_,v)) (IMul (IntExpInteger n) a)) = c
                                                           ++ [Cmd [] (MulI vu n)]
                                                           where (u, c) = comIntExp a

comStm (SIAss (PIdent (_,v)) (IMul a b)) = c
                                           ++ d
                                           ++ [Cmd [] (Mul v t u)]
                                           where
                                             (t, c) = comIntExp a
                                             (u, d) = comIntExp b

jumpT (BExp_FALSE) l = []
jumpT (BExp_TRUE) l = [Cmd [] (Jmp l)]
jumpT (BNot g) l = jumpF g l
jumpT (BAnd g h) l = c
                     ++ d
                     [Cmd [end] Nop]
                     where end = fresh ()
                           c = jumpF g end
                           d = jumpT g l
jumpT (BExpPIdent (PIdent (_, v))) l = [Cmd [] (Beqi v 0 end )]
                                       ++ [Cmd [] (Jmp l)]
                                       ++ [Cmd [end] Nop]
                                       where end = fresh ()
jumpT (BExp1 g h) l = c
                      ++ d
                      where c = jumpT g l
                            d = jumpT h l

jumpT (BExp2 g h) l = c
                      ++ d
                      ++ [Cmd [] (Beq v w l)]
                      where (v, c)  = comBExp g
                            (w, d) = comBExp h

jumpF (BExp_FALSE) l = [Cmd [] (Jmp l)]
jumpF (BExp_TRUE) l = []
jumpF (BNot g) l = jumpT g l
jumpF (BAnd g h) l = c
                     ++ d
                     where
                       c = jumpF g l
                       d = jumpF h l
jumpF (BExpPIdent (PIdent (_,v))) l = [Cmd [] (Beqi v 0 l)]
jumpF (BExp1 g h) l = jumpT (BAnd g h) l
jumpF (BExp2 g h) l = c
                      ++ d
                      ++ [Cmd [] (Bne v w l)]
                      where
                        (v, c) = comBexp g
                        (w, d) = comBexp h
