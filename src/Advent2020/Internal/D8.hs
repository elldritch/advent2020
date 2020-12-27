module Advent2020.Internal.D8
  ( Program,
    Instruction (..),
    Operation (..),
    parse,
    Machine (..),
    Status (..),
    initial,
    step,
    runUntilFixed,
  )
where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt)
import Data.Set (insert, member)
import GHC.Show (Show (..))
import Relude hiding (show)
import Text.Megaparsec (chunk, eof, someTill)
import Text.Megaparsec.Char (char, digitChar, newline)

type Program = [Instruction]

data Operation
  = NoOp
  | Accumulate
  | Jump
  deriving (Eq)

instance Show Operation where
  show op = case op of
    NoOp -> "nop"
    Accumulate -> "acc"
    Jump -> "jmp"

data Instruction = Instruction
  { operation :: Operation,
    argument :: Int
  }
  deriving (Eq)

instance Show Instruction where
  show Instruction {..} = show operation ++ " " ++ (if argument >= 0 then "+" else "") ++ show argument

parse :: Text -> Either Text Program
parse = parseWithPrettyErrors $ parseInstruction `someTill` eof
  where
    parseOp :: Parser Operation
    parseOp =
      (NoOp <$ chunk "nop")
        <|> (Accumulate <$ chunk "acc")
        <|> (Jump <$ chunk "jmp")

    parseInstruction :: Parser Instruction
    parseInstruction = do
      operation <- parseOp
      _ <- char ' '
      sign <- (1 <$ char '+') <|> (-1 <$ char '-')
      value <- parseWith readInt $ digitChar `someTill` newline
      let argument = sign * value
      return Instruction {..}

data Status = Running | Terminated deriving (Show, Eq)

data Machine = Machine
  { accumulator :: Int,
    programCounter :: Int,
    status :: Status
  }
  deriving (Show, Eq)

initial :: Machine
initial = Machine {accumulator = 0, programCounter = 0, status = Running}

step :: Program -> Machine -> Either Text Machine
step program m@Machine {..} = do
  Instruction {..} <- getInstruction
  let newPC = case operation of
        NoOp -> programCounter + 1
        Accumulate -> programCounter + 1
        Jump -> programCounter + argument
  let newStatus = if newPC == length program then Terminated else Running
  let m' = m {programCounter = newPC, status = newStatus}
  return $ case operation of
    NoOp -> m'
    Accumulate -> m' {accumulator = accumulator + argument}
    Jump -> m'
  where
    getInstruction = maybeToRight "program counter out-of-bounds" $ program !!? programCounter

runUntilFixed :: Program -> Either Text (Int, Machine)
runUntilFixed program = do
  let machines = iterate (>>= step program) $ Right initial
  takeUntilLoop mempty machines
  where
    takeUntilLoop :: Set Int -> [Either Text Machine] -> Either Text (Int, Machine)
    takeUntilLoop seen (e : es) = do
      ex@Machine {..} <- e
      let result = Right (1 + length seen, ex)
      case status of
        Running ->
          if member programCounter seen
            then result
            else takeUntilLoop (insert programCounter seen) es
        Terminated -> result
    takeUntilLoop _ [] = Left "impossible: iteration of program steps produced finite list"
