module MinMax where

import           Rules
import Board
import           Types

bestMove :: Player -> Board -> Move
bestMove player board = undefined

moveScore :: Player -> Board -> Int
moveScore player board =
  case boardState board player of
    Win wonPlayer
      | wonPlayer == player -> 1
    Win wonPlayer -> -1
    Full -> 0
    Incomplete ->
      let allMoves = allPossibleMoves player board
          allBoards = (`updateBoard` board) <$> allMoves
       in fmap $ moveScore (nextPlayer player) allBoards

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X
