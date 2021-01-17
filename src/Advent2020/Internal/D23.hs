module Advent2020.D23
  ( Cups (..),
    parse,
  )
where

import Advent2020.Internal (parseWith, parseWithPrettyErrors, readInt, rotate)
import Relude hiding (some)
import Text.Megaparsec (eof)
import Text.Megaparsec.Char (digitChar, newline)
import Control.Monad.Combinators.NonEmpty (some)
import qualified Relude.Unsafe as Unsafe
import Data.List (elemIndex)

data Cups = Cups
  { cups :: [Int],
    current :: Int
  }

parse :: Text -> Either Text Cups
parse = parseWithPrettyErrors $ do
  ns <- parseWith readInt' $ some digitChar
  _ <- newline
  eof
  return Cups {cups = toList ns, current = head ns}
  where
    readInt' :: NonEmpty Char -> Either Text (NonEmpty Int)
    readInt' (x :| xs) = do
      xs' <- readInt'' xs
      x' <- readInt (show x :: String)
      return $ x' :| xs'
    readInt'' :: [Char] -> Either Text [Int]
    readInt'' (x : xs) = do
      xs' <- readInt'' xs
      x' <- readInt (show x :: String)
      return $ x' : xs'
    readInt'' [] = return []

step :: Cups -> Cups
step Cups {..} = undefined
  where
    m = foldr max 0 cups
    i = Unsafe.fromJust $ elemIndex current cups
    j = i + 1
    pickedUp = take 3 $ rotate j cups
    remaining = drop 3 $ rotate j cups
    destination = Unsafe.fromJust $ find (`elem` remaining) $ iterate (\x -> let y = x - 1 in if y < 0 then m else y) current
