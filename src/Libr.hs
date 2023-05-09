
module Libr where

import Text.Read

data Player = X | O | Empty deriving (Show, Eq)
type Point = (Int, Int)
data Move = Move Player Int Int deriving (Show)
type Board = [[Player]]

getEmptyBoard :: Int -> Board
getEmptyBoard size = replicate size row
                     where row = replicate size Empty

-- makeMove :: String -> Board -> Either String Board
-- makeMove input board = do
--     moveInput <- parseInput input

move :: Move -> Board -> 


parseInput :: String -> Player -> Either String Move
parseInput [x,' ',y] player = do
    row <- parseInt x
    col <- parseInt y
    return $ Move player row col
parseInput [] _ = Left "Input cannot be empty!"
parseInput _ _= Left "Could not parse!"

parseInt :: Char -> Either String Int
parseInt x = case readMaybe [x] of
    Nothing -> Left $ "Could not read character: " ++ [x]
    Just l -> Right l


isFieldEmpty :: Point -> Board -> Maybe Bool
isFieldEmpty point board = do
    player <- getField point board
    return $ player /= Empty


getField :: Point -> Board -> Maybe Player
getField (x,y) board 
    | validMove (x,y) board = let row = board !! y
                              in Just $ row !! x
    | otherwise             = Nothing

validMove :: Point -> Board -> Bool
validMove (x, y) board
    | (x > size) || (y > size) = False
    | (y < 0) || (x < 0)       = False
    | otherwise                = True
    where size = length board

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither (Just x) _ = Right x
maybeToEither Nothing e  = Left e