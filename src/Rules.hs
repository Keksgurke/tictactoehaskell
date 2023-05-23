module Rules
  ( boardState
  , nextPlayer
  ) where


import           Board      (isFull)
import           Data.List  (transpose)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Types      (Board, BoardState (..), Rule, Player (..))

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

boardState :: Board -> BoardState
boardState board
  | isWin board = Win
  | isFull board = Full
  | otherwise = Incomplete

isWin :: Rule
isWin board = any ($ board) rules

rules :: [Rule]
rules = [rowWin, columnWin, diagonalWin]

diagonalWin :: Rule
diagonalWin board =
  let indexRows = zip board [0 ..]
      indexRevRows = zip (reverse board) [0 ..]
      diagonal = traverse (uncurry (!!)) indexRows
      otherDiagonal = traverse (uncurry (!!)) indexRevRows
   in or $ fmap (allSame . fromMaybe []) [diagonal, otherDiagonal]

rowWin :: Rule
rowWin board = any allSame filteredBoard
  where
    filteredBoard = mapMaybe sequenceA board

columnWin :: Rule
columnWin board = rowWin $ transpose board

allSame :: Eq a => [a] -> Bool
allSame []     = False
allSame (x:xs) = all (== x) xs
