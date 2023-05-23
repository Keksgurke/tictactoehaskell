module MinMax where

import           Board
import           Rules
import           Types

bestMove :: Player -> Board -> Move
bestMove player board = undefined

moveScore :: Player -> Board -> Int
moveScore player board =
  case boardState board of
    Win -> if player == X then -1 else 1
    Full -> 0
    Incomplete ->
      let allMoves = allPossibleMoves player board
          allBoards = (`updateBoard` board) <$> allMoves
          allScores = fmap (moveScore (nextPlayer player)) allBoards 
       in case player of 
        X -> maximum allScores
        O -> minimum allScores

