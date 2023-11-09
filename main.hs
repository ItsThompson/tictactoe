import GameDataTypes 
import Data.Time.Clock

-- src: randomIO taken from week-6-IO
randomIO :: IO Int
randomIO = do
  t <- getCurrentTime
  (return . floor . (*1000) . toRational . utctDayTime) t

tictactoe :: IO ()
tictactoe = 
    do
        let board = Square Empty ( Square Empty ( Square Empty ( Square Empty ( Square Empty( Square Empty( Square Empty( Square Empty( Square Empty End))))))))
        tictactoe' board

tictactoe' :: Grid -> IO ()
tictactoe' board = 
    do
        x <- getinput board
        let boardwithplayermove = move board x
        let newboard = computermove boardwithplayermove
        --
        let z = getcrosses newboard 
        putStrLn (printlist z)
        --
        if gameloop newboard 
        then
            do
                putStrLn(generateboard newboard)
                putStrLn "End"
                return ()
        else 
            do 
                tictactoe' newboard


printlist :: [Int] -> String 
printlist [] = ""
printlist (x:xs) = show x ++ " " ++ printlist xs

getinput :: Grid -> IO Int
getinput board = 
    do
        putStrLn(generateboard board)
        putStr("Choose your move from " ++ options (emptygrids board))
        i :: Int <- readLn :: IO Int
        if getitem board i == Empty
            then return i
        else 
            do 
                putStrLn "Space is already taken, choose again!"
                getinput board


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

computermove :: Grid -> Grid
computermove board = board

-- Get Space by Index

getitem :: Grid -> Int -> Space
getitem board i = getitem' board i 0
    where 
        getitem' :: Grid -> Int -> Int -> Space
        getitem' (Square x End) i j
            | i /= j = error "Out of Bounds" 
            | otherwise = x
        getitem' (Square x xs) i j
            | i /= j = getitem' xs i (j+1)
            | otherwise = x

gameloop :: Grid -> Bool
gameloop = checkwin . getcrosses


checkwin :: [Int] -> Bool
checkwin [] = False
checkwin x
    | winbyrow x || winbycolumn x || winbycross x = True
    | otherwise = False

winbyrow :: [Int] -> Bool
winbyrow x = subset [0,1,2] x || subset [3,4,5] x || subset [6,7,8] x

-- C = 036,147,258
winbycolumn :: [Int] -> Bool
winbycolumn x = subset [0,3,6] x || subset [1,4,7] x || subset [2,5,8] x

winbycross :: [Int] -> Bool
winbycross x = subset [0,4,8] x || subset [2,4,6] x

subset :: [Int] -> [Int] -> Bool
subset [] [] = True
subset _ [] = False
subset [] _ = True
subset (x:xs) (y:ys)
    | x == y = subset xs ys
    | otherwise = subset (x:xs) ys

getcrosses :: Grid -> [Int]
getcrosses board = getcrosses' board 0
    where
        getcrosses' :: Grid -> Int -> [Int]
        getcrosses' (Square x End) i
            | X == x = [i]
            | otherwise = [] 
        getcrosses' (Square x xs) i
            | X == x = i : getcrosses' xs (i+1)
            | otherwise = getcrosses' xs (i+1)


options :: [Int] -> String
options [] = ": "
options (x:xs) = show x ++ " " ++ options xs 

emptygrids :: Grid -> [Int] 
emptygrids board = emptygrids' board 0 
    where
        emptygrids' :: Grid -> Int -> [Int] 
        emptygrids' (Square x End) i  
            | x == Empty = [i] --show i ++ ": "
            | otherwise = [] -- ": "
        emptygrids' (Square x xs) i 
            | x == Empty = i: emptygrids' xs (i+1) -- show i ++ " " ++ emptygrids' xs (i+1)
            | otherwise = emptygrids' xs (i+1)

-- Generate Board
generateboard:: Grid -> String
generateboard board = generateboard' board 0
    where
        generateboard' :: Grid -> Int -> String
        generateboard' (Square x End) i = generategridsquare x i
        generateboard' (Square x xs) i = generategridsquare x i ++ generateboard' xs (i+1)

generategridsquare :: Space -> Int -> String
generategridsquare x i
    | (i + 1) == 1 = "\n " ++ show x ++ " |" -- First Line
    | (i + 1) `mod` 3 == 0 && (i+1) /= 9 = " " ++ show x ++ " \n" ++ "---+---+---\n" -- Generate horizontal lines
    | (i+1) == 9 = " " ++ show x ++ " \n" -- Last Line
    | otherwise = " " ++ show x ++ " |" 


