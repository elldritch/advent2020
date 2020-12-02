module Advent2020.D2 (run, part1, part2, parse, Password (..), errorBundlePretty) where

import Relude hiding (max, min, some)
import Relude.Unsafe (read, (!!))
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
  let a :: Int = read a'
  b' <- numberChar `manyTill` spaceChar <?> "b"
  let b :: Int = read b'
  letter <- letterChar <?> "letter"
  _ <- chunk ": "
  password <- letterChar `manyTill` newline <?> "password"
  return Password {..}

run :: Text -> (Password -> Bool) -> Int
run contents isValid = sum $ map fromEnum valids
  where
    passwords = case parse contents of
      Right ps -> ps
      Left err -> error $ toText $ errorBundlePretty err
    valids = map isValid passwords

part1 :: Password -> Bool
part1 Password {..} = count >= a && count <= b
  where
    countLetter :: Char -> String -> Int
    countLetter l s = foldr (\c n -> if c == l then n + 1 else n) 0 s

    count = countLetter letter $ toString password

part2 :: Password -> Bool
part2 Password {..} = letterAt a `xor` letterAt b
  where
    letterAt i = password !! (i - 1) == letter
