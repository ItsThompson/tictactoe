module Helper
    where

import Data.Time.Clock
import Board

-- src: randomIO taken from week-6-IO
randomIO :: IO Int
randomIO = do
  t <- getCurrentTime
  (return . floor . (*1000) . toRational . utctDayTime) t

subSet :: [Int] -> [Int] -> Bool
subSet [] [] = True
subSet _ [] = False
subSet [] _ = True
subSet (x:xs) (y:ys)
    | x == y = subSet xs ys
    | otherwise = subSet (x:xs) ys


getCrosses :: Grid -> [Int]
getCrosses board = getCrosses' board 0
    where
        getCrosses' :: Grid -> Int -> [Int]
        getCrosses' (Square x End) i
            | X == x = [i]
            | otherwise = [] 
        getCrosses' (Square x xs) i
            | X == x = i : getCrosses' xs (i+1)
            | otherwise = getCrosses' xs (i+1)

