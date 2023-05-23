module Main where

import           Board          --(defaultBoard, getEmptyBoard)
import           Game           --(getNextState)
import           System.Process --(system)
import           Types          --(Board, GameState (..), Player (X),
                                -- World (World))

startingState :: Int -> GameState
startingState size =
  case getEmptyBoard size of
    Just board -> Running $ World X board
    Nothing    -> Running $ World X defaultBoard

main :: IO ()
main = do
  gameLoop $ startingState 4

gameLoop :: GameState -> IO ()
gameLoop (Draw board) = do
  clearScreen
  printBoard board
  putStrLn "It's a draw!!!"
gameLoop (GameOver winner board) = do
  clearScreen
  printBoard board
  putStrLn $ show winner ++ " won!!!"
gameLoop current@(Running (World player board)) = do
  clearScreen
  printBoard board
  putStrLn ""
  putStrLn (show player ++ " plays:")
  input <- getLine
  putStrLn ""
  let nextState = getNextState input current
  case nextState of
    Left err -> do
      putStrLn err
      _ <- getLine
      gameLoop current
    Right state -> gameLoop state

printBoard :: Board -> IO ()
printBoard board = putStr $ prettyPrint board
  where
    prettyPrint =
      let rowPrint = fmap (maybe "-" show)
       in unlines . fmap (unwords . rowPrint)

clearScreen :: IO ()
clearScreen = do
  _ <- system "cls"
  return ()
