module Solution_2_5 where
import Data.Maybe

data Period = AM | PM deriving(Eq, Ord, Show)
data TimeOfDay = TimeOfDay Int Int

validTimeOfDay :: TimeOfDay -> Bool
validTimeOfDay (TimeOfDay hrs mins)
  | hrs > 23 = False
  | mins > 59 = False
  | otherwise = True

tod2period :: TimeOfDay -> Period
tod2period (TimeOfDay hrs mins)
  | hrs < 12 = AM
  | otherwise = PM

data IntExp = Const Int | Var String
                        | Plus IntExp IntExp
                        | Times IntExp IntExp
                        | Minus IntExp
                          deriving Eq

instance Show IntExp where
  show (Const n) = show n
  show (Var s) = s
  show (Plus a b) = show a ++ " + " ++ show b
  show (Times a b) = show a ++ " * " ++ show b
  show (Minus a) = "-" ++ show a

valIntExp :: IntExp -> Int
valIntExp (Const x) = x
valIntExp (Plus a b) = (valIntExp a) + (valIntExp b)
valIntExp (Times a b) = (valIntExp a) * (valIntExp b)
valIntExp (Minus a) = -(valIntExp a)

simpleIntExp1 :: IntExp -> IntExp
simpleIntExp1 (Plus x (Const 0)) =  simpleIntExp1 x
simpleIntExp1 (Plus (Const 0) x) =  simpleIntExp1 x
simpleIntExp1 (Plus (Const x) (Const y)) = Const (x + y)
simpleIntExp1 (Plus x y) = Plus (simpleIntExp1 x) (simpleIntExp1 y)
simpleIntExp1 (Times x (Const 1)) = simpleIntExp1 x
simpleIntExp1 (Times (Const 1) x) = simpleIntExp1 x
simpleIntExp1 (Times _ (Const 0)) = (Const 0)
simpleIntExp1 (Times (Const 0) _) = (Const 0)
simpleIntExp1 (Times (Const x) (Const y)) = Const (x * y)
simpleIntExp1 (Times x y) = Times (simpleIntExp1 x) (simpleIntExp1 y)
simpleIntExp1 (Minus(Minus a)) = simpleIntExp1 a
simpleIntExp1 (Minus a) = Minus (simpleIntExp1 a)
simpleIntExp1 (Minus (Const a)) = Const (-a)
simpleIntExp1 (Var s) = Var s
simpleIntExp1 (Const x) = Const x

exp1, exp2 ::  IntExp
exp1 = Times (Const 1) (Plus (Times (Var "x")  (Const 1))  (Times (Const 1) (Var "y" )))
exp2 = Plus (Var "x")  (Plus (Const 5) (Const 7))

type Enviroment = String -> Int
envIntExp :: Enviroment -> IntExp -> Int
envIntExp env (Var y) = env y
envIntExp _ (Const x) = x
envIntExp env (Plus x y) = (envIntExp env x) + (envIntExp env y)
envIntExp env (Times x y) = (envIntExp env x) * (envIntExp env y)
envIntExp env (Minus x) = -(envIntExp env x)


map', map'' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter', filter'' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) | p x = x : filter' p xs
                 | otherwise = filter' p xs


map'' _ [] = []
map'' f (x:xs) = nf where
  nf = f x : map'' f xs

filter'' _ [] = []
filter'' p (x:xs) | p x = a
                  |otherwise = b
                  where
                  a = x : filter'' p xs
                  b = filter'' p xs
overwrite :: Eq a => (a->b) -> a -> b -> a->b
overwrite f x y z | z == x = y
                  | otherwise = f z

droparg :: Eq a => (a->b) -> a -> (a->b)
droparg f x z | x == z = undefined
              | otherwise = f z

mx :: Maybe Int ->  Int -> Maybe Int
mx (Just b) a | a > b = Just a
              |otherwise = Just b
mx (Nothing) a  = Just a

maxlt :: [Int] -> Maybe Int
maxlt xs = foldl mx Nothing  xs

reportmax :: [Int] -> String
reportmax lst = case (max) of
                (Nothing) ->  "The maximum number in [] is not defined"
                (Just a) -> "The maximum number in " ++ show lst ++ " is " ++ show a
             where max = maxlt lst

reportmax' :: [Int] -> String
reportmax' lst = maybe "The maximum number in [] is not defined" (\a -> "The maximum number in " ++ show lst ++ " is " ++ show a) (maxlt lst)
