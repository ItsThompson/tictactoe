module GameState
    where

import Board
import Helper

gameLoop :: Grid -> (Bool, Maybe Space)
gameLoop board 
    | checkWin (getSpace board X) = (True, Just X)
    | checkWin (getSpace board O) = (True, Just O)
    | otherwise = (False, Nothing)


checkWin :: [Int] -> Bool
checkWin [] = False
checkWin x
    | winByRow x || winByColumn x || winByCross x = True
    | otherwise = False

winByRow :: [Int] -> Bool
winByRow x = subSet [0,1,2] x || subSet [3,4,5] x || subSet [6,7,8] x

winByColumn :: [Int] -> Bool
winByColumn x = subSet [0,3,6] x || subSet [1,4,7] x || subSet [2,5,8] x

winByCross :: [Int] -> Bool
winByCross x = subSet [0,4,8] x || subSet [2,4,6] x
