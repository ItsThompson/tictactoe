module Move 
    where

import Board

-- TODO: Minimax Algorithm
computerMove :: Grid -> Grid
computerMove board = board
-- We are always maximizing Os

-- Set grid space by index
move :: Grid -> Space -> Int -> Grid
move board space i = move' board i 0
    where
        move' :: Grid -> Int -> Int -> Grid
        move' (Square x End) i j
            | i /= j = Square x End
            | otherwise = Square space End
        move' (Square x xs) i j
            | i /= j = Square x (move' xs i (j+1))
            | otherwise = Square space (move' xs i (j+1))
