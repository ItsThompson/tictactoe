module Move 
    where

import Board

move :: Grid -> Int -> Grid
move board i = move' board i 0
    where
        move' :: Grid -> Int -> Int -> Grid
        move' (Square x End) i j
            | i /= j = Square x End
            | otherwise = Square X End

        move' (Square x xs) i j
            | i /= j = Square x (move' xs i (j+1))
            | otherwise = Square X (move' xs i (j+1))

-- TODO: Minimax Algorithm
computerMove :: Grid -> Grid
computerMove board = board
