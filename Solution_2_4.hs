module Solution_2_4 where

zerofy :: [Int] -> [Int]
zerofy [] = []
zerofy (x:xs) = 0:(zerofy xs)

copies :: Char -> Int -> String
copies c n
  | n <= 0 = []
  | otherwise = c:(copies c (n-1))

stutter :: String -> String
stutter [] = []
stutter (x:xs) = x:x:(stutter xs)

mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + (mysum xs)

myproduct :: Num a => [a] -> a
myproduct [] = 0
myproduct (x:xs) = x * (myproduct xs)

mysum', myproduct' ::[Int] -> Int
mysum' = foldr (+) 0
myproduct' = foldr (*) 1
