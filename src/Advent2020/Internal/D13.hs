module Advent2020.Internal.D13
  ( Schedule (..),
    parse,
    earliestBusAfter,
  )
where

import Advent2020.Internal (min', parseWith, parseWithPrettyErrors, readInt)
import Relude
import Relude.Unsafe (fromJust)
import Text.Megaparsec (eof, someTill)
import Text.Megaparsec.Char (char, digitChar, newline)

data Schedule = Schedule
  { earliestDeparture :: Int,
    buses :: [Maybe Int]
  }
  deriving (Show, Eq)

parse :: Text -> Either Text Schedule
parse = parseWithPrettyErrors $ do
  earliestDeparture <- parseWith readInt $ digitChar `someTill` newline
  buses <- ((Just <$> parseWith readInt (digitChar `someTill` separator)) <|> (char 'x' >> separator >> return Nothing)) `someTill` eof
  return Schedule {..}
  where
    separator = char ',' <|> newline

earliestBusAfter :: Int -> NonEmpty Int -> (Int, Int)
earliestBusAfter ready buses = foldr (min' snd) (head firstDepartureAfter) firstDepartureAfter
  where
    busDepartures :: NonEmpty (Int, [Int])
    busDepartures = (\busID -> (busID, iterate (+ busID) 0)) <$> buses

    firstDepartureAfter :: NonEmpty (Int, Int)
    firstDepartureAfter = second (fromJust . find (> ready)) <$> busDepartures
