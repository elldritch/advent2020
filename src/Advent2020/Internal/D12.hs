module Advent2020.Internal.D12
  ( Instruction (..),
    Action (..),
    parse,
  )
where

import Advent2020.Internal (parseWith, parseWithPrettyErrors, readInt)
import Control.Monad.Combinators (someTill)
import Relude
import Text.Megaparsec (chunk, eof)
import Text.Megaparsec.Char (char, digitChar, newline)

data Instruction = Instruction
  { action :: Action,
    value :: Int
  }
  deriving (Show, Eq)

data Action
  = MoveNorth
  | MoveSouth
  | MoveEast
  | MoveWest
  | TurnLeft
  | TurnRight
  | MoveForward
  deriving (Show, Eq)

parse :: Text -> Either Text [Instruction]
parse = parseWithPrettyErrors $ (moveP <|> turnP) `someTill` eof
  where
    moveP = do
      action <-
        (char 'N' >> return MoveNorth)
          <|> (char 'S' >> return MoveSouth)
          <|> (char 'E' >> return MoveEast)
          <|> (char 'W' >> return MoveWest)
          <|> (char 'F' >> return MoveForward)
      value <- parseWith readInt $ digitChar `someTill` newline
      return Instruction {..}
    turnP = do
      action <- (char 'L' >> return TurnLeft) <|> (char 'R' >> return TurnRight)
      value <- parseWith readInt (chunk "90" <|> chunk "180" <|> chunk "270")
      _ <- newline
      return Instruction {..}

data Orientation
  = North
  | South
  | East
  | West

data Ship = Ship
  { orientation :: Orientation,
    position :: (Int, Int)
  }

step :: Ship -> Instruction -> Ship
step s@Ship {position = (x, y), ..} Instruction {..} = case action of
  MoveNorth -> s {position = move North value}
  MoveSouth -> s {position = move South value}
  MoveEast -> s {position = move East value}
  MoveWest -> s {position = move West value}
  TurnLeft -> undefined
  TurnRight -> undefined
  MoveForward -> s {position = move orientation value}
  where
    move :: Orientation -> Int -> (Int, Int)
    move North v = (x, y + v)
    move South v = (x, y - v)
    move East v = (x + v, y)
    move West v = (x - v, y)

