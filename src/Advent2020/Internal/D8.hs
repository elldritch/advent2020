module Advent2020.Internal.D8
  ( Program,
    Instruction (..),
    Operation (..),
    parse,
    Machine (..),
    step,
  )
where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt)
import Relude
import Text.Megaparsec (chunk, eof, someTill)
import Text.Megaparsec.Char (char, digitChar, newline)

type Program = [Instruction]

data Operation
  = NoOp
  | Accumulate
  | Jump
  deriving (Show, Eq)

data Instruction = Instruction
  { operation :: Operation,
    argument :: Int
  }
  deriving (Show, Eq)

parse :: Text -> Either Text Program
parse = parseWithPrettyErrors $ parseInstruction `someTill` eof
  where
    parseOp :: Parser Operation
    parseOp =
      (chunk "nop" >> return NoOp)
        <|> (chunk "acc" >> return Accumulate)
        <|> (chunk "jmp" >> return Jump)

    parseInstruction :: Parser Instruction
    parseInstruction = do
      operation <- parseOp
      _ <- char ' '
      sign <-
        (void (char '+') >> return 1)
          <|> (void (char '-') >> return (-1))
      value <- parseWith readInt $ digitChar `someTill` newline
      let argument = sign * value
      return Instruction {..}

data Machine = Machine
  { accumulator :: Int,
    programCounter :: Int
  }

step :: Program -> Machine -> Either Text Machine
step program m@Machine {..} = do
  Instruction {..} <- getInstruction
  return $ case operation of
    NoOp -> m {programCounter = programCounter + 1}
    Accumulate -> m {programCounter = programCounter + 1, accumulator = accumulator + argument}
    Jump -> m {programCounter = programCounter + argument}
  where
    getInstruction = maybeToRight "program counter out-of-bounds" $ program !!? programCounter
