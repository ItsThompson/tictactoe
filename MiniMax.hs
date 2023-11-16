module MiniMax
    where

import Board
import Data.Maybe (fromJust, isNothing)
import GameState 
import Move

--                  Leaf Board Score Depth
data DecisionTree = Leaf Grid Int Int 
--                    Node Board   [Child]        Score    Depth
                    | Node Grid [DecisionTree] (Maybe Int) Int
    deriving (Show, Eq)


generateOptimalMove :: Grid -> Grid
generateOptimalMove board = optimalBoard decisionTree
    where
        decisionTree = (evaluateTree . generateDecisionTree) board

        optimalBoard :: DecisionTree -> Grid
        optimalBoard (Leaf board _ _) = board
        optimalBoard (Node _ (x:xs) _ _) = chooseBoard (x:xs) (getGrid x, getScore x) 

        chooseBoard :: [DecisionTree] -> (Grid, Int) -> Grid
        chooseBoard [] (grid,_) = grid
        chooseBoard (x:xs) (grid, score) 
            | getScore x > score = chooseBoard xs (getGrid x, getScore x) -- Score of current item is higher
            | otherwise = chooseBoard xs (grid, score)-- Score of current item is lower or equal

        getGrid :: DecisionTree -> Grid 
        getGrid (Node board _ _ _) = board 
        getGrid (Leaf board _ _) = board 

        getScore :: DecisionTree -> Int
        getScore (Node _ _ score _) = fromJust score
        getScore (Leaf _ score _) = score

-- Generates Decision Tree From Grid
generateDecisionTree :: Grid -> DecisionTree
generateDecisionTree = generateTree . generateRootNode
    where
        generateRootNode :: Grid -> DecisionTree
        generateRootNode board 
            | (fst . checkGameState) board = Leaf board (evaluateLeaf board) 0 -- X or O won
            | checkGameState board == (False, Just Empty) = Leaf board 0 0 -- Draw
            | otherwise = Node board (deepen board 0) Nothing 0 

        generateTree :: DecisionTree -> DecisionTree
        generateTree node 
            | checkCompleteTree node = node 
            | otherwise = generateTree newTree
                where
                    newTree = generateLayer node 

                    checkCompleteTree :: DecisionTree -> Bool
                    checkCompleteTree (Node _ [] _ _) = False
                    checkCompleteTree (Node _ tree _ _) = all checkCompleteTree tree
                    checkCompleteTree Leaf {} = True 

                    generateLayer :: DecisionTree -> DecisionTree
                    generateLayer (Node board tree score depth) = Node board (iterativeDeepening tree) score depth

                    iterativeDeepening :: [DecisionTree] -> [DecisionTree]
                    iterativeDeepening = map hydrateDecisionTree

                    hydrateDecisionTree :: DecisionTree -> DecisionTree
                    hydrateDecisionTree (Leaf board score depth) = Leaf board score depth
                    hydrateDecisionTree (Node board [] score depth)= Node board (deepen board depth) score depth
                    hydrateDecisionTree (Node board tree score depth) = Node board (iterativeDeepening tree) score depth 

        deepen :: Grid -> Int -> [DecisionTree]
        deepen board depth = newBoardNode (generatePossibleBoards board depth) (depth+1)
            where
                -- Generates list of DecisionTree elements from a list of Grid elements
                newBoardNode :: [Grid] -> Int -> [DecisionTree]
                newBoardNode [] _ = []
                newBoardNode (x:xs) depth
                    | (fst . checkGameState) x = Leaf x (evaluateLeaf x) depth : newBoardNode xs depth-- X or O won
                    | checkGameState x == (False, Just Empty) = Leaf x 0 depth : newBoardNode xs depth-- Draw
                    | otherwise = Node x [] Nothing depth : newBoardNode xs depth

                -- Generate Board for current depth
                generatePossibleBoards :: Grid -> Int -> [Grid]
                generatePossibleBoards board depth = possibleBoards board (getSpace depth) (getEmptySpaces board) 
                    where
                        getSpace :: Int -> Space
                        getSpace depth
                            | even depth = O
                            | otherwise = X

                        -- Get indexes of places where empty
                        getEmptySpaces :: Grid -> [Int]
                        getEmptySpaces board = getEmptySpaces' board 0
                            where
                                getEmptySpaces' :: Grid -> Int -> [Int]
                                getEmptySpaces' (Square x End) i 
                                    | x /= Empty =[]
                                    | otherwise = [i]
                                getEmptySpaces' (Square x xs) i
                                    | x /= Empty = getEmptySpaces' xs (i+1)
                                    | otherwise = i : getEmptySpaces' xs (i+1)
                        
                        -- Generate possibleBoards 
                        possibleBoards :: Grid -> Space -> [Int] -> [Grid]
                        possibleBoards _ _ [] = []
                        possibleBoards board space (x:xs) = move board space x : possibleBoards board space xs
            
        -- Only run for leaf
        evaluateLeaf :: Grid -> Int
        evaluateLeaf board
            | fst state && (fromJust . snd) state == X = -1
            | fst state && (fromJust . snd) state == O = 1
            | otherwise = 0
            where
                state = checkGameState board



-- Evaluates each node of the tree
evaluateTree :: DecisionTree -> DecisionTree
evaluateTree node 
    | completedEvaluation node = node 
    | otherwise = (evaluateTree . depthFirstEval) node 
        where

            completedEvaluation :: DecisionTree -> Bool
            completedEvaluation (Node _ tree Nothing _) = False
            completedEvaluation (Node _ tree score _) = True 
            completedEvaluation Leaf {} = True 

            depthFirstEval :: DecisionTree -> DecisionTree
            depthFirstEval (Node board (x:xs) score depth)
                | length (x:xs) == 1 = Node board (x:xs) (Just (getLeafScore x)) depth -- Only one child -> Child is leaf 
                | isNothing score && allChildrenEvaluated (x:xs) = Node board (x:xs) (Just (calculateScore (x:xs) depth)) depth  -- All children have been evaluated 
                | otherwise = Node board (iterateOverChildren (x:xs)) score depth -- Children have not been evaluated -> Dig Deeper (isNothing score)
                    where
                        allChildrenEvaluated :: [DecisionTree] -> Bool
                        allChildrenEvaluated = all completedEvaluation

                        iterateOverChildren :: [DecisionTree] -> [DecisionTree]
                        iterateOverChildren [] = []
                        iterateOverChildren (x:xs)
                            | completedEvaluation x = x : iterateOverChildren xs
                            | otherwise = depthFirstEval x : iterateOverChildren xs
                        
                        getLeafScore :: DecisionTree -> Int
                        getLeafScore (Leaf _ score _) = score

                        calculateScore :: [DecisionTree] -> Int -> Int
                        calculateScore decisionTrees depth
                            | even depth = (maximum . getChildrenScores) decisionTrees
                            | otherwise = (minimum . getChildrenScores) decisionTrees
                                where
                                    getChildrenScores :: [DecisionTree] -> [Int]
                                    getChildrenScores [] = []
                                    getChildrenScores ((Leaf _ score _):xs) = score : getChildrenScores xs
                                    getChildrenScores ((Node _ _ score _):xs) = fromJust score : getChildrenScores xs

