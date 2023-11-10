import Board 
import GenerateBoard
import GameState
import Helper
import Text.Read (readMaybe)

tictactoe :: IO ()
tictactoe = 
    do
        let board = generateEmptyBoard
        tictactoe' board

tictactoe' :: Grid -> IO ()
tictactoe' board = 
    do
        x <- getInput board
        let playerBoard = move board x
        let computerBoard = computerMove playerBoard
        -- let z = getCrosses computerBoard 
        -- putStrLn (printList z)
        if gameLoop computerBoard 
        then
            do
                putStrLn(generateBoard computerBoard)
                putStrLn "End"
                return ()
        else 
            do 
                tictactoe' computerBoard


-- printList :: [Int] -> String 
-- printList [] = ""
-- printList (x:xs) = show x ++ " " ++ printList xs

getInput :: Grid -> IO Int
getInput board = 
    do
        putStrLn(generateBoard board)
        let opts = options (emptyGrids board)
        putStr("Choose your move from " ++ opts)
        moveNumber <- intInput opts
        if getItem board moveNumber == Empty
            then return moveNumber
        else 
            do 
                putStrLn "Space is already taken, choose again!"
                getInput board

intInput :: String -> IO Int
intInput opts = 
    do
        i <- getLine
        let int = readMaybe i :: Maybe Int
        maybe (intInput' opts) return int

intInput' :: String -> IO Int
intInput' opts = 
    do
        putStr ("Invalid input! Please choose again from " ++ opts)
        i <- getLine
        let int = readMaybe i :: Maybe Int
        maybe (intInput' opts) return int

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


options :: [Int] -> String
options [] = ": "
options (x:xs) = show x ++ " " ++ options xs 

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
