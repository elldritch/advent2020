module Advent2020.Internal.D14
  ( Program,
    Instruction (..),
    MaskBit (..),
    parse,
    Machine (..),
    execute,
    BitMask (..),
    step,
    DecodeMask,
    step',
  )
where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt')
import Data.Bits (clearBit, setBit, shift, (.&.), (.|.))
import Relude
import Relude.Extra.Map
import Text.Megaparsec (chunk, count, eof, someTill)
import Text.Megaparsec.Char (char, digitChar, newline)

-- Programs are sequences of instructions.
type Program = [Instruction]

data MaskBit
  = One
  | Zero
  | X
  deriving (Show, Eq)

data Instruction
  = SetMask [MaskBit]
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
      bits <- count 36 $ (X <$ char 'X') <|> (One <$ char '1') <|> (Zero <$ char '0')
      _ <- newline
      return $ SetMask bits

    setMemoryP :: Parser Instruction
    setMemoryP = do
      _ <- chunk "mem["
      address <- parseWith readInt' (digitChar `someTill` char ']')
      _ <- chunk " = "
      value <- parseWith readInt' (digitChar `someTill` newline)
      return SetMemory {..}

-- Machines are executions of programs.
data Machine m = Machine
  { memory :: Map Integer Integer,
    mask :: m
  }
  deriving (Show, Eq)

initialMachine :: Machine m
initialMachine = Machine {memory = mempty, mask = error "program must set mask as first instruction"}

step :: Machine BitMask -> Instruction -> Machine BitMask
step m (SetMask mask) = m {mask = bitsToBitMask mask}
step m@Machine {..} SetMemory {..} = m {memory = insert address (bitmasked mask value) memory}

execute :: (Machine m -> Instruction -> Machine m) -> Program -> Machine m
execute _ [] = initialMachine
execute f ps = foldl' f initialMachine ps

-- BitMask: masking in part 1.
data BitMask = BitMask
  { orMask :: Integer,
    andMask :: Integer
  }
  deriving (Show, Eq)

bitmasked :: BitMask -> Integer -> Integer
bitmasked BitMask {..} v = (v .|. orMask) .&. andMask

bitsToBitMask :: [MaskBit] -> BitMask
bitsToBitMask = foldl' f BitMask {orMask = 0, andMask = 0}
  where
    shiftMask :: BitMask -> BitMask
    shiftMask BitMask {..} =
      BitMask
        { orMask = orMask `shift` 1,
          andMask = (andMask `shift` 1) .|. 1
        }

    f :: BitMask -> MaskBit -> BitMask
    f m bit =
      let m'@BitMask {..} = shiftMask m
       in case bit of
            One -> m' {orMask = setBit orMask 0}
            Zero -> m' {andMask = clearBit andMask 0}
            X -> m'

-- DecodeMask: masking in part 2.
type DecodeMask = [MaskBit]

data AddressBit
  = AOne
  | AZero
  deriving (Show, Eq)

type Address = [AddressBit]

decoded :: DecodeMask -> Integer -> [Integer]
decoded mask addr = bitsToInts bss
  where
    decoded' :: DecodeMask -> Address -> [[AddressBit]]
    decoded' (m : ms) (a : as) = case m of
      Zero -> [a] : decoded' ms as
      One -> [AOne] : decoded' ms as
      X -> [AOne, AZero] : decoded' ms as
    decoded' [] [] = []
    decoded' _ _ = error "decoded': decode mask and address are not the same length"

    addr' :: Address
    addr' = intToBits addr

    bss :: [[AddressBit]]
    bss = decoded' mask addr'

intToBits :: Integer -> Address
intToBits = reverse . padLeadingZeros . intToBits'
  where
    padLeadingZeros :: [AddressBit] -> Address
    padLeadingZeros bs = take 36 $ bs <> repeat AZero

    intToBits' :: Integer -> [AddressBit]
    intToBits' 1 = [AOne]
    intToBits' 0 = [AZero]
    intToBits' n = case n `mod` 2 of
      0 -> AZero : intToBits' (n `div` 2)
      1 -> AOne : intToBits' (n `div` 2)
      r -> error $ "inToBits: n % 2 == " <> show r

bitsToInts :: [[AddressBit]] -> [Integer]
bitsToInts [bs] = do
  b <- bs
  return $ case b of
    AOne -> 1
    AZero -> 0
bitsToInts (bs : bss) = do
  b <- bs
  i <- bitsToInts bss
  return $
    i + case b of
      AZero -> 0
      AOne -> 2 ^ length bss
bitsToInts [] = []

step' :: Machine DecodeMask -> Instruction -> Machine DecodeMask
step' m (SetMask mask) = m {mask}
step' m@Machine {..} SetMemory {..} = m {memory = foldl' (\mem addr -> insert addr value mem) memory $ decoded mask address}
