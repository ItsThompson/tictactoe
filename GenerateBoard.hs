module GenerateBoard
    where

import Board 

-- board = Square Empty ( Square Empty ( Square Empty ( Square Empty ( Square Empty( Square Empty( Square Empty( Square Empty( Square Empty End))))))))
generateEmptyBoard :: Grid
generateEmptyBoard = generateEmptyBoard' 0

generateEmptyBoard' :: Int -> Grid
generateEmptyBoard' i
    | i < 8 = Square Empty (generateEmptyBoard' (i+1))
    | otherwise = Square Empty End

generateBoard:: Grid -> String
generateBoard board = generateBoard' board 0
    where
        generateBoard' :: Grid -> Int -> String
        generateBoard' (Square x End) i = generateGridSquare x i
        generateBoard' (Square x xs) i = generateGridSquare x i ++ generateBoard' xs (i+1)

generateGridSquare :: Space -> Int -> String
generateGridSquare x i
    | (i + 1) == 1 = "\n " ++ show x ++ " |" -- First Line
    | (i + 1) `mod` 3 == 0 && (i+1) /= 9 = " " ++ show x ++ " \n" ++ "---+---+---\n" -- Generate horizontal lines
    | (i+1) == 9 = " " ++ show x ++ " \n" -- Last Line
    | otherwise = " " ++ show x ++ " |" 


