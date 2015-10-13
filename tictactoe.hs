module Tictactoe where
import Data.Maybe
import Data.List (and)
import Control.Monad.IO.Class (liftIO)

data Player = X | O deriving(Show, Eq)

class TicTacToe t where
  emptyGame :: t
  playerMove :: Player -> (Int, Int) -> t -> Maybe t
  hasWon :: t -> Maybe Player


newtype TicRel = TicRel [(Int, Int, Player)]

instance Show TicRel where
  show (TicRel xs) = show xs

instance Eq TicRel where
  (==) (TicRel xs) (TicRel ys) = xs == ys

instance TicTacToe TicRel where
  emptyGame = TicRel []
  playerMove player (x,y) (TicRel xs) | (x, y, X) `elem` xs = Nothing
                                      | (x, y, O) `elem` xs = Nothing
                                      | x > 2 || y > 2 || x < 0 || y < 0 = Nothing
                                      | otherwise = Just (TicRel ((x, y, player):xs))
  hasWon (TicRel xs) | and $ map (`elem`xs) [(0,0,O), (0,1,O), (0,2,O)] = Just O
                     | and $ map (`elem`xs) [(1,0,O), (1,1,O), (1,2,O)] = Just O
                     | and $ map (`elem`xs) [(2,0,O), (2,1,O), (2,2,O)] = Just O
                     | and $ map (`elem`xs) [(0,0,O), (1,0,O), (2,0,O)] = Just O
                     | and $ map (`elem`xs) [(0,1,O), (1,1,O), (2,1,O)] = Just O
                     | and $ map (`elem`xs) [(0,2,O), (1,2,O), (2,2,O)] = Just O
                     | and $ map (`elem`xs) [(0,0,O), (1,1,O), (2,2,O)] = Just O
                     | and $ map (`elem`xs) [(0,2,O), (1,1,O), (2,0,O)] = Just O
                     | and $ map (`elem`xs) [(0,0,X), (0,1,X), (0,2,X)] = Just X
                     | and $ map (`elem`xs) [(1,0,X), (1,1,X), (1,2,X)] = Just X
                     | and $ map (`elem`xs) [(2,0,X), (2,1,X), (2,2,X)] = Just X
                     | and $ map (`elem`xs) [(0,0,X), (1,0,X), (2,0,X)] = Just X
                     | and $ map (`elem`xs) [(0,1,X), (1,1,X), (2,1,X)] = Just X
                     | and $ map (`elem`xs) [(0,2,X), (1,2,X), (2,2,X)] = Just X
                     | and $ map (`elem`xs) [(0,0,X), (1,1,X), (2,2,X)] = Just X
                     | and $ map (`elem`xs) [(0,2,X), (1,1,X), (2,0,X)] = Just X
                     | otherwise = Nothing


readMoveString :: String -> Maybe (Int, Int)
readMoveString str = do
  (x, y) <- listToMaybe $ reads str
  return (x, read y)

main = do
  putStrLn "Tic Tac Toe"
  go (TicRel [])
  where go board  = do
          newboard <- player O board
          putStrLn (show newboard)
          if (hasWon newboard) == Nothing then do
            newerboard <- player X newboard
            putStrLn (show newerboard)
            if (hasWon newerboard) == Nothing then go newerboard
              else putStrLn ("Gae Over " ++ show (fromJust(hasWon newerboard)))
           else putStrLn ("Gae Over " ++ show (fromJust(hasWon newboard)))
          where player p b = do
                putStrLn ("Player " ++ show p)
                moveStr <- getLine
                if readMoveString moveStr == Nothing then player p b
                  else (if (playerMove p (fromJust (readMoveString moveStr)) b) == Nothing then player p b
                                else return (fromJust (playerMove p (fromJust (readMoveString moveStr))  b)))
