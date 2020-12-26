module Advent2020.Internal.D2
  ( Password (..),
    parse,
  )
where

import Advent2020.Internal (Parser, integralP, parseWithPrettyErrors, symbol, wordP)
import Relude
import Text.Megaparsec (eof, hidden, someTill, (<?>))
import Text.Megaparsec.Char (letterChar, newline)

data Password = Password
  { a :: Int,
    b :: Int,
    letter :: Char,
    password :: Text
  }
  deriving (Show, Eq)

parse :: Text -> Either Text [Password]
parse = parseWithPrettyErrors $ parser `someTill` hidden eof

parser :: Parser Password
parser = do
  a <- integralP <?> "left number"
  _ <- symbol "-"
  b <- integralP <?> "right number"
  letter <- letterChar <?> "letter"
  _ <- symbol ":"
  password <- wordP <?> "password"
  _ <- newline
  return Password {..}
