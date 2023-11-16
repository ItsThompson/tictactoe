module Helper
    where

import Data.Time.Clock
import Board

-- src: randomIO taken from week-6-IO
randomIO :: IO Int
randomIO = do
  t <- getCurrentTime
  (return . floor . (*1000) . toRational . utctDayTime) t

-- Checks if first list is a subset of second list
subSet :: [Int] -> [Int] -> Bool
subSet [] [] = True
subSet _ [] = False
subSet [] _ = True
subSet (x:xs) (y:ys)
    | x == y = subSet xs ys
    | otherwise = subSet (x:xs) ys


-- Generate a list of integers where grid space is a cross
getSpace :: Grid -> Space -> [Int]
getSpace board space = getSpace' board space 0
    where
        getSpace' :: Grid -> Space -> Int -> [Int]
        getSpace' (Square x End) space i
            | space == x = [i]
            | otherwise = [] 
        getSpace' (Square x xs) space i
            | space == x = i : getSpace' xs space (i+1)
            | otherwise = getSpace' xs space (i+1)


-- Get Space by Index
getItem :: Grid -> Int -> Space
getItem board i = getItem' board i 0
    where 
        getItem' :: Grid -> Int -> Int -> Space
        getItem' (Square x End) i j
            | i /= j = error "Out of Bounds" 
            | otherwise = x
        getItem' (Square x xs) i j
            | i /= j = getItem' xs i (j+1)
            | otherwise = x

-- Generate a list of integers where grid space is empty
emptyGrids :: Grid -> [Int] 
emptyGrids board = emptyGrids' board 0 
    where
        emptyGrids' :: Grid -> Int -> [Int] 
        emptyGrids' (Square x End) i  
            | x == Empty = [i] --show i ++ ": "
            | otherwise = [] -- ": "
        emptyGrids' (Square x xs) i 
            | x == Empty = i: emptyGrids' xs (i+1) -- show i ++ " " ++ emptyGrids' xs (i+1)
            | otherwise = emptyGrids' xs (i+1)

-- Prompt generation 
listToString :: [Int] -> String
listToString [] = ": "
listToString (x:xs) = show x ++ " " ++ listToString xs 


-- NOTE: Testing Function (Turns a List of Spaces into a Grid)
listToGrid :: [Space] -> Grid
listToGrid list = listToGrid' list 0
    where
        listToGrid' :: [Space] -> Int -> Grid
        listToGrid' (x:xs) i
            | i < 8 = Square x (listToGrid' xs (i+1))
            | otherwise = Square x End
