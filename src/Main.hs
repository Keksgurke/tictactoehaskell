module Main where
--import Data.Either
import Libr

data GameState = Running World | GameOver Player
data World = World {currentPlayer :: Player, currentBoard :: Board }

main :: IO ()
main = do
    gameLoop $ Running $ World X (getEmptyBoard 3)

gameLoop :: GameState -> IO ()
gameLoop (GameOver winner) = putStrLn $ show winner ++ " won!!!"
gameLoop current@(Running (World player board)) = do
    print (prettyPrint board)
    putStrLn (show player ++ " Turn:")
    input <- getLine
    let newBoard = makeMove input player board
    case newBoard of 
        Left a ->  do
            putStrLn a
            gameLoop current
        Right b -> gameLoop $ if isWin b then GameOver player
                              else Running $ World (nextPlayer player) b


prettyPrint :: Board -> String
prettyPrint = unlines . map (unwords . map show) 

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer _ = X

