module Optimisations where

import Compiler
import AbsM
import SemM
import ErrM
import ParRules
import Data.List (union, intersect)

type CFGraph = [(LineNum, [LineNum])]

eliminateNop :: [Line] -> [Line]
eliminateNop [] = [] --Should never get [...,Nop] as Hlt must be last instruction!
eliminateNop ((Cmd x Nop):(Cmd y z):xs) = (Cmd (x++y) z):(eliminateNop xs)
eliminateNop (x:xs) = x:(eliminateNop xs)


cfg :: [Line] -> CFGraph
cfg x = dfg 0 x x

dfg :: Int -> [Line] -> [Line] -> CFGraph
dfg x [] _ = []
dfg x ((Cmd _ (Beq _ _ l)):xs) all = (Just x, [Just (x + 1), findLineNum all l]):(dfg (x+1) xs all)
dfg x ((Cmd _ Hlt):xs) all = (Just x, []):[]
dfg x ((Cmd _ (Blt _ _ l)):xs) all = (Just x, [Just (x + 1), findLineNum all l]):(dfg (x+1) xs all)
dfg x ((Cmd _ (Bne _ _ l)):xs) all = (Just x, [Just (x + 1), findLineNum all l]):(dfg (x+1) xs all)
dfg x ((Cmd _ (Bge _ _ l)):xs) all = (Just x, [Just (x + 1), findLineNum all l]):(dfg (x+1) xs all)
dfg x ((Cmd _ (BeqI _ _ l)):xs) all = (Just x, [Just (x + 1), findLineNum all l]):(dfg (x+1) xs all)
dfg x ((Cmd _ (BltI _ _ l)):xs) all = (Just x, [Just (x + 1), findLineNum all l]):(dfg (x+1) xs all)
dfg x ((Cmd _ (Jmp l)):xs) all = (Just x, [findLineNum all l]):(dfg (x+1) xs all)
--Catch all for instrutions that have a single immediate successor.
dfg x ((Cmd _ _):xs) all = (Just x, [Just (x + 1)]):(dfg (x+1)xs all)


--Can use find :: (a->Bool) -> [a] -> Maybe a
searchCFG :: Int -> LineNum -> Bool
searchCFG x (Just y) = x == y
searchCFG _ Nothing = False


--Merge sort like algorithm
--Split in to n CFGraphs and merge these togther

merge :: CFGraph -> CFGraph -> CFGraph
merge l@((Just x, v):xs) r@((Just y, u):ys) | x < y = (Just x, v):(merge xs r)
                                            | y < x = (Just y, u):(merge l ys)
                                            | otherwise = (Just x, v `union`  u):(merge xs ys)
merge [] x = x
merge x [] = x

opposite :: CFGraph -> CFGraph
opposite ((Just x, to):xs) = merge (opposite xs) [(y, [Just x]) | y <- to]
opposite [] = []

def, use :: [Line] -> Int -> [String]
def p n = defl (p!!n)
          where defl (Cmd _ (Lim l _)) = [l]
                defl (Cmd _ (Mov l _)) = [l]
                defl (Cmd _ (Add l _ _)) = [l]
                defl (Cmd _ (Mul l _ _)) = [l]
                defl (Cmd _ (Neg l _)) = [l]
                defl (Cmd _ (Lor l _ _)) = [l]
                defl (Cmd _ (AddI l _ _)) = [l]
                defl (Cmd _ (MulI l _ _)) = [l]
                defl (Cmd _ (NegI l _)) = [l]             
                defl _ = []
use p n = usel (p!!n)
          where usel (Cmd _ (Mov _ l)) = [l]
                usel (Cmd _ (Add _ l v)) = [l,v]
                usel (Cmd _ (Mul _ l v)) = [l,v]
                usel (Cmd _ (Neg _ l)) = [l]
                usel (Cmd _ (Prn l)) = [l]
                usel (Cmd _ (Beq _ l v)) = [l,v]
                usel (Cmd _ (Blt _ l v)) = [l,v]
                usel (Cmd _ (Lor _ l v)) = [l,v]
                usel (Cmd _ (Bne _ l v)) = [l,v]
                usel (Cmd _ (Bge _ l v)) = [l,v]
                usel (Cmd _ (AddI _ l _)) = [l]
                usel (Cmd _ (MulI _ l _)) = [l]
                usel (Cmd _ (BeqI _ _ l)) = [l]
                usel (Cmd _ (BltI _ _ l)) = [l]

gen, kill :: [Line] -> Int -> [Int] 
gen p n | length (def p n) == 0 = [n]
        | otherwise = []
kill p n = killp p 0
           where killp (_:xs) m = if length ((def p n) `intersect` (def p m)) > 0 then m:(killp xs (m+1)) else (killp xs (m+1))
                 killp [] _ = []
