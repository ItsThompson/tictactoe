import Board 
import Data.Maybe (fromJust, isNothing)
import GameState 
import Helper (emptyGrids, getItem, listToString)
import MiniMax (generateOptimalMove)
import Move 
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
        let playerBoard = move board X x
        let computerBoard = generateOptimalMove playerBoard
        let gameState = checkGameState computerBoard
        if fst gameState 
        then
            do
                let winner = (fromJust . snd) gameState
                print computerBoard
                if winner == X then putStrLn "IMPOSSIBLE! You won!"
                    else putStrLn "You lost, better luck next time!"
                return ()
        else if (isNothing . snd) gameState
            then 
                do
                    tictactoe' computerBoard
            else
                do 
                    print computerBoard
                    putStrLn "Draw! Close game!"

-- Input
getInput :: Grid -> IO Int
getInput board = 
    do
        print board 
        let opts = listToString (emptyGrids board)
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
