module Game
  ( getNextState
  ) where

import           Board  (setFigure)
import           Parser (getMove, parseInput)
import           Rules  (boardState)
import           Types  (BoardState (..), GameState (..), Player (..),
                         World (..))

getNextState :: String -> GameState -> Either String GameState
getNextState input (Running (World player board)) = do
  point <- parseInput input
  move <- getMove point player board
  newBoard <- setFigure move board
  case boardState newBoard of
    Win        -> return $ GameOver player newBoard
    Full       -> return $ Draw newBoard
    Incomplete -> return $ Running (World (nextPlayer player) newBoard)
getNextState _ _ = Left "There is no next state"

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X
