module Game
  ( getNextState
  ) where

import           Board  (setFigure)
import MinMax
import           Parser (getMove, getPoint)
import           Rules  (boardState, nextPlayer)
import           Types  (BoardState (..), GameState (..), Player (..),
                         World (..))

getNextState :: String -> GameState -> Either String GameState
getNextState input (Running (World player board)) = do
  point <- getPoint (words input) board
  move <- getMove point player board
  newBoard <- setFigure move board
  let cBoard = computerTurn (nextPlayer player) newBoard
  case boardState cBoard of
    Win        -> return $ GameOver player cBoard
    Full       -> return $ Draw newBoard
    Incomplete -> return $ Running (World player cBoard)
getNextState _ _ = Left "There is no next state"


