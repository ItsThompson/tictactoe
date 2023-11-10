import Board 
import GenerateBoard
import GameState
import Helper
import Move 
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

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
        let win = gameLoop computerBoard
        if fst win
        then
            do
                let winner = (fromJust . snd) win
                putStrLn(generateBoard computerBoard)
                if winner == X then putStrLn "Game Ended! You Win"
                    else putStrLn "Game Ended! You Lose"
                return ()
        else 
            do 
                tictactoe' computerBoard

-- Input
getInput :: Grid -> IO Int
getInput board = 
    do
        putStrLn(generateBoard board)
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
