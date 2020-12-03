module Advent2020.Internal.D2 (parse, Password (..), errorBundlePretty) where

import Relude hiding (max, min, some)
import Text.Megaparsec (ParseErrorBundle, Parsec, chunk, errorBundlePretty, manyTill, runParser, some, (<?>))
import Text.Megaparsec.Char (char, letterChar, newline, numberChar, spaceChar)

data Password = Password
  { a :: Int,
    b :: Int,
    letter :: Char,
    password :: String
  }
  deriving (Show, Eq)

parse :: Text -> Either (ParseErrorBundle Text Void) [Password]
parse = runParser (some parser) ""

type Parser = Parsec Void Text

parser :: Parser Password
parser = do
  a' <- numberChar `manyTill` char '-' <?> "a"
  a <- case readEither a' of
    Right num -> return num
    Left e -> fail $ toString e
  b' <- numberChar `manyTill` spaceChar <?> "b"
  b <- case readEither b' of
    Right num -> return num
    Left e -> fail $ toString e
  letter <- letterChar <?> "letter"
  _ <- chunk ": "
  password <- letterChar `manyTill` newline <?> "password"
  return Password {..}
