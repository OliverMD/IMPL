module SemM where

import AbsM
import ParM
import ErrM

type State = [(String,Integer)] --List of register values
type LineNum = Maybe Int

update :: (String, Integer) -> State -> State
update (a,b) [] = [(a,b)]
update (a,b) ((x,y):xs) | x==a = (a,b):xs
                        | otherwise = (x,y):(update (a,b) xs)

find :: String -> State -> Integer
find str [] = error "Register not found"
find str ((a, b):xs) | str == a = b
                     | otherwise = find str xs

findLineNum :: [Line] -> String -> LineNum
findLineNum = fln 0
              where fln p lines lab = case lines of
                      [] -> Nothing
                      (Cmd labs _):xs -> if lab `elem` labs then Just p
                                         else  fln (p+1) xs lab

run :: [Line] -> (LineNum, State, String) -> (LineNum, State, String)
run lines (Nothing, state, o) = (Nothing, state, o)
run lines (Just p, state, o) = case lines !! p of
                           (Cmd _ Hlt) -> (Nothing, state, o)
                           (Cmd _ Nop) -> run lines (Just (p + 1), state, o)
                           (Cmd _ (Lim r n)) -> run lines (Just (p + 1), update (r,n) state, o)
                           (Cmd _ (Mov r q)) -> run lines (Just (p + 1), update (r, find q state) state, o)
                           (Cmd _ (Add res x0 x1)) -> run lines (Just (p + 1), update (res, (find x0 state) + (find x1 state)) state, o)
                           (Cmd _ (Mul res x0 x1)) -> run lines (Just (p + 1), update (res, (find x0 state) * (find x1 state)) state, o)
                           (Cmd _ (Neg res x)) -> run lines (Just (p + 1), update (res, (find x state) * (-1)) state, o)
                           (Cmd _ (Prn r)) -> run lines (Just (p + 1), state, o ++ show (find r state))
                           (Cmd _ (Jmp lab)) -> run lines (findLineNum lines lab, state, o)
                           (Cmd _ (Beq r q l)) -> run lines (if (find r state) == (find q state) then (findLineNum lines l, state, o) else (Just (p + 1), state, o))
                           (Cmd _ (Blt r q l)) -> run lines (if (find r state) <  (find q state) then (findLineNum lines l, state, o) else (Just (p + 1), state, o))
                           (Cmd _ (Lor r x0 x1)) -> run lines (Just (p+1), update (r, if (find x0 state) || (find x1 state) then 1 else 0) state, o)
                           (Cmd _ (Bne r q l)) -> run lines (if (find r state) != (find q state) then (findLineNum lines l, state, o) else (Just (p + 1), state, o))
                           (Cmd _ (Bge r q l)) -> run lines (if (find r state) > (find q state) then (findLineNum lines l, state, o) else (Just (p + 1), state, o))
                           (Cmd _ (AddI r x0 x1)) -> run lines (Just (p+1), update (r, x0 + x1), o)
                           (Cmd _ (MulI r x0 x1)) -> run lines (Just (p+1), update (r, x0*x1), o)
                           (Cmd _ (NegI r x)) -> run lines (Just (p+1), update (r, (-1) * x), o)

getLines :: [Err Line] -> [Line]
getLines [] = []
getLines ((Ok l):xs) = l:(getLines xs)
