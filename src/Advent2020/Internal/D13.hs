module Advent2020.Internal.D13
  ( Schedule (..),
    parse,
    earliestBusAfter,
    chinese',
  )
where

import Advent2020.Internal (integralP, parseWithPrettyErrors)
import Math.NumberTheory.Moduli.Chinese (chinese)
import Relude
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

data Schedule = Schedule
  { earliestDeparture :: Int,
    buses :: [Maybe Int]
  }
  deriving (Show, Eq)

parse :: Text -> Either Text Schedule
parse = parseWithPrettyErrors $ do
  earliestDeparture <- integralP
  _ <- newline
  buses <- (busP <|> noBusP) `sepBy1` char ','
  _ <- newline >> eof
  return Schedule {..}
  where
    busP = Just <$> integralP
    noBusP = Nothing <$ char 'x'

earliestBusAfter :: Int -> NonEmpty Int -> (Int, Int)
earliestBusAfter ready buses = foldr (min' snd) (head firstDepartureAfter) firstDepartureAfter
  where
    min' :: (Ord b) => (a -> b) -> a -> a -> a
    min' f a b = if f a < f b then a else b

    busDepartures :: NonEmpty (Int, [Int])
    busDepartures = (\busID -> (busID, iterate (+ busID) 0)) <$> buses

    firstDepartureAfter :: NonEmpty (Int, Int)
    firstDepartureAfter = second (Unsafe.fromJust . find (> ready)) <$> busDepartures

chinese' :: NonEmpty (Integer, Integer) -> Maybe (Integer, Integer)
chinese' ((n, m) :| []) = Just (n, m)
chinese' (x :| xt) = foldlM f x xt
  where
    f (nAcc, mAcc) (n, m) = do
      n' <- chinese (nAcc, mAcc) (n, m)
      return (n', mAcc * m)
