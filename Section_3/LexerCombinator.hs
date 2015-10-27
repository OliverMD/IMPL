module LexerCombinator where
import Data.Maybe

type RegExp a = [a] -> (Maybe [a], [a])


chop :: (Eq a, Show a) => RegExp a -> RegExp a -> [a] -> [[a]]
chop w r = rrw
           where rrw s | s' == [] = [s]
                       | isJust m = (fromJust m) :(rrw t)
                       | otherwise = error ("\n\tlexing error: " ++ show (take 8 t))
                                     where
                                       (_,s') = w s
                                       (m, t) = r s

symb :: Eq a => a -> RegExp a
symb symbol (x:xs) | symbol == x = (Just [x], xs)
              | otherwise = (Nothing,xs)

anybut :: Eq a => a -> RegExp a
anybut a (x:xs) | a == x = (Nothing, xs)
                | otherwise = (Just [x], xs)

null :: RegExp a
null (_:xs) = (Nothing, xs)

infixl 5 /|/
infixl 6 /./

(/|/), (/./) :: RegExp a -> RegExp a -> RegExp a
(r /|/ s) t | isJust m = split
            | otherwise = s t
            where split@(m,_) = r t

(r /./ s) t | isJust m = (n, v) --Does s need to operate on u?
            | otherwise = (Nothing, u)
            where (m, u) = r t
                  (n, v) = s u

star :: (Eq a) => RegExp a -> RegExp a
star r all@(x:xs) | (fst (r all)) == Nothing = (Just [], all)
                  | otherwise = (Just (q ++ (fromJust a)), b)
                  where (Just q, w) = r all
                        (a, b) = r w
