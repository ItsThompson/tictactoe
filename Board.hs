module Board
    where

subscripts = ["₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈"]

data Space = Empty | X | O
    deriving Eq

instance Show Space where
    show = display
        where
        display Empty = " "
        display X = "X"
        display O = "O"


data Grid = End | Square Space Grid
   deriving Eq

instance Show Grid where
    show = generateBoard 
        where
        generateBoard:: Grid -> String
        generateBoard board = generateBoard' board 0
            where
                generateBoard' :: Grid -> Int -> String
                generateBoard' (Square x End) i = generateGridSquare x i
                generateBoard' (Square x xs) i = generateGridSquare x i ++ generateBoard' xs (i+1)

        generateGridSquare :: Space -> Int -> String
        generateGridSquare x i
            | (i + 1) == 1 = "\n " ++ showPosition x i ++ " |" -- First Line
            | (i + 1) `mod` 3 == 0 && (i+1) /= 9 = " " ++ showPosition x i ++ " \n" ++ "---+---+---\n" -- Generate horizontal lines
            | (i+1) == 9 = " " ++ showPosition x i ++ " \n" -- Last Line
            | otherwise = " " ++ showPosition x i ++ " |" 
                where
                    showPosition :: Space -> Int -> String
                    showPosition space i 
                        | space /= Empty = show space 
                        | otherwise = getSubscriptString subscripts 0 i
                    getSubscriptString :: [String] -> Int -> Int -> String
                    getSubscriptString (x:xs) i target
                        | i == target = x
                        | otherwise = getSubscriptString xs (i+1) target

generateEmptyBoard :: Grid
generateEmptyBoard = generateEmptyBoard' 0
    where
        generateEmptyBoard' :: Int -> Grid
        generateEmptyBoard' i
            | i < 8 = Square Empty (generateEmptyBoard' (i+1))
            | otherwise = Square Empty End

