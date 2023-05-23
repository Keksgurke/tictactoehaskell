module Types where

data GameState
  = Running World
  | GameOver Player Board
  | Draw Board

data BoardState
  = Win Player
  | Full
  | Incomplete

data World =
  World Player Board

type Rule = (Board -> Bool)

data Player
  = X
  | O
  deriving (Show, Eq)

type Point = (Int, Int)

data Move =
  Move Player Point
  deriving (Show)

type Board = [[Maybe Player]]
