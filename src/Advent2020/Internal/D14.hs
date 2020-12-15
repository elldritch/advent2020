module Advent2020.Internal.D14
  ( Program,
    Instruction (..),
    Mask (..),
    parse,
    Machine (..),
    step,
    execute,
  )
where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt)
import Data.Bits (clearBit, setBit, shift, (.&.), (.|.))
import Data.Map (insert)
import Relude
import Text.Megaparsec (chunk, count, eof, someTill)
import Text.Megaparsec.Char (char, digitChar, newline)

type Program = [Instruction]

data Mask = Mask
  { orMask :: Integer,
    andMask :: Integer
  }
  deriving (Show, Eq)

initialMask :: Mask
initialMask =
  Mask
    { orMask = 0,
      andMask = 0b111111111111111111111111111111111111
    }

data Instruction
  = SetMask Mask
  | SetMemory {address :: Integer, value :: Integer}
  deriving (Show, Eq)

parse :: Text -> Either Text Program
parse = parseWithPrettyErrors $ instructionP `someTill` eof
  where
    instructionP :: Parser Instruction
    instructionP = setMaskP <|> setMemoryP

    setMaskP :: Parser Instruction
    setMaskP = do
      _ <- chunk "mask = "
      bits <- count 36 $ (char 'X' >> return Nothing) <|> (char '1' >> return (Just 1)) <|> (char '0' >> return (Just 0))
      _ <- newline
      return $ SetMask $ bitsToMask bits

    bitsToMask :: [Maybe Int] -> Mask
    bitsToMask bs = foldl' f Mask {orMask = 0, andMask = 0} bs
      where
        shiftMask :: Mask -> Mask
        shiftMask Mask {..} =
          Mask
            { orMask = orMask `shift` 1,
              andMask = (andMask `shift` 1) .|. 1
            }

        f :: Mask -> Maybe Int -> Mask
        f m bit =
          let m'@Mask {..} = shiftMask m
           in case bit of
                Just 1 -> m' {orMask = setBit orMask 0}
                Just 0 -> m' {andMask = clearBit andMask 0}
                Nothing -> m'
                _ -> error "impossible: invalid input to bitsToMask"

    setMemoryP :: Parser Instruction
    setMemoryP = do
      _ <- chunk "mem["
      address <- toInteger <$> parseWith readInt (digitChar `someTill` char ']')
      _ <- chunk " = "
      value <- toInteger <$> parseWith readInt (digitChar `someTill` newline)
      return SetMemory {..}

data Machine = Machine
  { memory :: Map Integer Integer,
    mask :: Mask
  }
  deriving (Show, Eq)

initialMachine :: Machine
initialMachine = Machine {memory = mempty, mask = initialMask}

step :: Machine -> Instruction -> Machine
step m (SetMask mask) = m {mask}
step m@Machine {mask = Mask {..}, ..} SetMemory {..} = m {memory = insert address v memory}
  where
    v = (value .|. orMask) .&. andMask

execute :: Program -> Machine
execute [] = initialMachine
execute ps = foldl' step initialMachine ps
