module Advent2020.Internal.D2 (parse, Password (..)) where

import Advent2020.Internal (Parser, parseWithPrettyErrors)
import Relude hiding (some)
import Text.Megaparsec (chunk, some, someTill, (<?>))
import Text.Megaparsec.Char (char, letterChar, newline, numberChar, spaceChar)

data Password = Password
  { a :: Int,
    b :: Int,
    letter :: Char,
    password :: String
  }
  deriving (Show, Eq)

parse :: Text -> Either Text [Password]
parse = parseWithPrettyErrors $ some parser

parser :: Parser Password
parser = do
  a' <- numberChar `someTill` char '-' <?> "a"
  a <- case readEither a' of
    Right num -> return num
    Left e -> fail $ toString e
  b' <- numberChar `someTill` spaceChar <?> "b"
  b <- case readEither b' of
    Right num -> return num
    Left e -> fail $ toString e
  letter <- letterChar <?> "letter"
  _ <- chunk ": "
  password <- letterChar `someTill` newline <?> "password"
  return Password {..}
