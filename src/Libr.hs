
module Libr where

import Text.Read
import Data.List

data Player = X | O | Empty deriving (Show, Eq)
type Point = (Int, Int)
type Rule = (Board -> Bool)
data Move = Move Player Point deriving (Show)
newtype ValidMove = ValidMove Move
type Board = [[Player]]

getEmptyBoard :: Int -> Board
getEmptyBoard size = replicate size row
                     where row = replicate size Empty

makeMove :: String -> Player -> Board -> Either String Board
makeMove input currPlayer board = do
     (x,y) <- parseInput (words input)
     move <- getMove (x,y) currPlayer board
     setFigure move board

setFigure :: Move -> Board -> Either String Board
setFigure move board = do
    let field = getField move board
    case field of 
        Empty -> Right $ updateBoard move board
        _     -> Left "The field is occupied!"
        
updateBoard :: Move -> Board -> Board
updateBoard (Move p (x, y)) board =  let row = board !! y
                                         updatedRow = update row x p
                                     in  update board y updatedRow

parseInput :: [String] -> Either String Point
parseInput []  = Left "Input cannot be empty!"
parseInput [x,y] = do
    row <- parseInt x
    col <- parseInt y
    return (row - 1, col - 1)
parseInput _ = Left "Could not parse!"

parseInt :: String -> Either String Int
parseInt x = case readMaybe x of
    Nothing -> Left $ "Could not read character: " ++ x
    Just l -> Right l

getMove :: Point -> Player -> Board -> Either String Move
getMove (x,y) player board
    | validMove (x,y) board = Right $ Move player (x,y)
    | otherwise             = Left "The move is not valid!"


printBoard :: Board -> String
printBoard = foldl (\acc x -> acc ++ "\\n" ++ show x) ""

getField :: Move -> Board -> Player
getField (Move _ (x,y)) board = let row = board !! y
                                in  row !! x
   
validMove :: Point -> Board -> Bool
validMove (x, y) board
    | (x >= size) || (y >= size) = False
    | (y < 0) || (x < 0)       = False
    | otherwise                = True
    where size = length board

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither (Just x) _ = Right x
maybeToEither Nothing e  = Left e

update :: [a] -> Int -> a -> [a]
update [] _ _ = []
update (_:xs) 0 element = element:xs
update (x:xs) index element = x:update xs (index - 1) element 

isWin :: Board -> Bool
isWin board = or $ fmap ($ filteredBoard) rules
    where filteredBoard = map (filter (/= Empty)) board

rules :: [Rule]
rules = [rowWin, columnWin, diagonalWin]

diagonalWin :: Rule
diagonalWin board = let indexRows     = zip board [0..]
                        indexRevRows  = zip (reverse board) [0..]
                        diagonal      = fmap (uncurry (!!)) indexRows
                        otherDiagonal = fmap (uncurry (!!)) indexRevRows
                    in  or $ fmap allSame [diagonal, otherDiagonal]

rowWin :: Rule
rowWin board = or $ fmap allSame board

columnWin :: Rule
columnWin board = rowWin $ transpose board 


allSame :: Eq a => [a] -> Bool
allSame [] = False
allSame (x:xs) = foldl (\_ y -> x == y) False xs