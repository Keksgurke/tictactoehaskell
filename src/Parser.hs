module Parser
  ( getMove
  , getPoint
  ) where

import           Text.Read (readMaybe)
import           Types     (Board, Move (..), Player, Point)

getPoint :: [String] -> Board -> Either String Point
getPoint [x] board = do
  int <- parseInt x
  pointByIndex int board
getPoint [x, y] _ = pointByCoordinate x y
getPoint [] _ = Left "The input was empty!"
getPoint _ _ = Left "Could not parse!"

pointByCoordinate :: String -> String -> Either String Point
pointByCoordinate x y = do
  row <- parseInt x
  col <- parseInt y
  return (row - 1, col - 1)

pointByIndex :: Int -> Board -> Either String Point
pointByIndex input board

  | input <= 0 || input >= size = Left "Index is out of bounds"
  | otherwise =
    let y   = index `div` length board
        x   = index `mod` length board
     in Right (x, y)
  where
    size = length $ concat board
    index = input - 1

parseInt :: String -> Either String Int
parseInt x =
  case readMaybe x :: Maybe Int of
    Nothing -> Left $ "Could not read character: " ++ x
    Just l  -> Right l

getMove :: Point -> Player -> Board -> Either String Move
getMove (x, y) player board

  | validMove (x, y) board = Right $ Move player (x, y)
  | otherwise              = Left "The move is not valid!"

validMove :: Point -> Board -> Bool
validMove (x, y) board

  | (x >= size) || (y >= size) = False
  | (y < 0) || (x < 0)         = False
  | otherwise                  = True
  where
    size = length board
