module GameDataTypes 
    where

data Space = Empty | X | O
    deriving Eq
data Grid = End | Square Space Grid
   deriving Show 

instance Show Space where
    show = display
        where
        display Empty = " "
        display X = "X"
        display O = "O"
