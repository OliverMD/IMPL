module Answers_2 where

{-
1.4: :load module loads a file of definitions
     :reload reloads the current set of definitions
     :quit quits the interpreter
-}

sum3, medianInt :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

medianInt x y z
  | x < y = if z < x then x else (if z < y then z else y)
  | y < x = if z < y then y else (if z < x then z else x)

allzero, somezero :: [Int] -> Bool
allzero [] = True
allzero (x:xs) = (x == 0) && allzero xs

somezero [] = False
somezero (x:xs) = (x == 0) || somezero xs

alli, somei :: Int -> [Int] -> Bool
alli _ [] = True
alli v (x:xs) = (x == v) && (alli v xs)

somei _ [] = False
somei v (x:xs) = (x == v) || (somei v xs)

allv, somev :: Eq t => t -> [t] -> Bool
allv _ [] = True
allv v (x:xs) = (x == v) && allv v xs

somev _ [] = False
somev v (x:xs) = (x == v) || somev v xs

middle :: String -> Char
middle str = str !! (div  (length str) 2)

repeatstring :: String -> String
repeatstring str = str++" "++str

