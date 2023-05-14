module Main where
import Data.Either
import Libr

data World = World {currentPlayer :: Player, currentBoard :: Board }

main :: IO ()
main = do
    gameLoop $ World X (getEmptyBoard 3)

gameLoop :: World -> IO ()
gameLoop (World player board) = do
    print board
    putStrLn (show player ++ " Turn:")
    input <- getLine
    let newBoard = makeMove input player board
    case newBoard of 
        Left a ->  do
            putStrLn a
            gameLoop (World player board)
        Right b -> gameLoop (World (nextPlayer player) b)


nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer _ = X

