module Parser
  ( parseInput
  , getMove
  ) where

import           Text.Read (readMaybe)
import           Types     (Board, Move (..), Player, Point)

parseInput :: String -> Either String Point
parseInput [] = Left "Input cannot be empty!"
parseInput i = do
  let splittedInput = words i
  case splittedInput of
    [] -> Left "Input cannot be empty!"
    [x, y] -> do
      row <- parseInt x
      col <- parseInt y
      return (row - 1, col - 1)
    _ -> Left "Could not parse!"

parseInt :: String -> Either String Int
parseInt x =
  case readMaybe x of
    Nothing -> Left $ "Could not read character: " ++ x
    Just l  -> Right l

getMove :: Point -> Player -> Board -> Either String Move
getMove (x, y) player board
  | validMove (x, y) board = Right $ Move player (x, y)
  | otherwise = Left "The move is not valid!"

validMove :: Point -> Board -> Bool
validMove (x, y) board
  | (x >= size) || (y >= size) = False
  | (y < 0) || (x < 0) = False
  | otherwise = True
  where
    size = length board
