module Advent2020.Internal.D2
  ( Password (..),
    parse,
  )
where

import Advent2020.Internal (Parser, parseWith, parseWithPrettyErrors, readInt)
import Relude hiding (some)
import Text.Megaparsec (chunk, eof, hidden, label, someTill, (<?>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, spaceChar)

data Password = Password
  { a :: Int,
    b :: Int,
    letter :: Char,
    password :: String
  }
  deriving (Show, Eq)

parse :: Text -> Either Text [Password]
parse = parseWithPrettyErrors $ parser `someTill` hidden eof

parser :: Parser Password
parser = do
  a <- parseWith readInt $ label "left number" $ digitChar `someTill` char '-'
  b <- parseWith readInt $ label "right number" $ digitChar `someTill` spaceChar
  letter <- letterChar <?> "letter"
  _ <- chunk ": "
  password <- letterChar `someTill` newline <?> "password"
  return Password {..}
