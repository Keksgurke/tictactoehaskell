module MinMax where

import           Board
import           Data.Foldable
import           Data.Function
import           Rules
import           Types

computerTurn :: Player -> Board -> Board
computerTurn player board =
  let move = bestMove player board
   in updateBoard move board

bestMove :: Player -> Board -> Move
bestMove player board =
  let allMoves = allPossibleMoves player board
      scores = fmap (`moveScore` board) allMoves
      movesWithScore = zip scores allMoves
   in case player of
        X -> snd $ maximumBy (compare `on` fst) movesWithScore
        O -> snd $ minimumBy (compare `on` fst) movesWithScore

moveScore :: Move -> Board -> Int
moveScore move@(Move player point) board =
  case boardState $ updateBoard move board of
    Win
      | player == X -> 1
      | otherwise -> -1
    Full -> 0
    Incomplete ->
      let otherPlayer = nextPlayer player
          newBoard = updateBoard move board
          allMoves = allPossibleMoves otherPlayer newBoard
          allScores = fmap (`moveScore` newBoard) allMoves
       in case otherPlayer of
            X -> maximum allScores
            O -> minimum allScores
