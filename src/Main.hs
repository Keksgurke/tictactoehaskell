module Main where
import Data.Either
import Libr

data World = World {getCurrentPlayer :: Player}

main :: IO ()
main = do
    gameLoop $ World X

gameLoop :: World -> IO ()
gameLoop (World player) = do
    putStrLn (show player ++ " Turn:")
    input <- getLine
    if isLeft $ parseInput input player then 
        gameLoop (World player) 
        else 
            gameLoop (World (nextPlayer player))

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer _ = X

