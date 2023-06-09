module Board where

import           Data.Maybe (isJust)
import           Types      (Board, Move (..), Player)

defaultBoard :: Board
defaultBoard =
  let row = replicate 3 Nothing
   in replicate 3 row

getEmptyBoard :: Int -> Maybe Board
getEmptyBoard size
  | size >= 0 =
    let row = replicate size Nothing
     in Just $ replicate size row
  | otherwise = Nothing

setFigure :: Move -> Board -> Either String Board
setFigure move board = do
  let field = getField move board
  case field of
    Nothing -> Right $ updateBoard move board
    _       -> Left "The field is occupied!"

updateBoard :: Move -> Board -> Board
updateBoard (Move p (x, y)) board =
  let row = board !! y
      updatedRow = update row x (Just p)
   in update board y updatedRow

getField :: Move -> Board -> Maybe Player
getField (Move _ (x, y)) board =
  let row = board !! y
   in row !! x

isFull :: Board -> Bool
isFull board = all isJust $ concat board

update :: [a] -> Int -> a -> [a]
update [] _ _               = []
update (_:xs) 0 element     = element : xs
update (x:xs) index element = x : update xs (index - 1) element
