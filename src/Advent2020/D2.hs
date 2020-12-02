module Advent2020.D2 (parse, Password(..), errorBundlePretty, isValid, run) where

import Relude hiding (max, min, some)
import Relude.Unsafe (read)
import Text.Megaparsec ((<?>), errorBundlePretty, ParseErrorBundle, Parsec, chunk, manyTill, runParser, some)
import Text.Megaparsec.Char (spaceChar, char, letterChar, newline, numberChar)

data Password = Password
  { min :: Int,
    max :: Int,
    letter :: Char,
    password :: Text
  }
  deriving (Show, Eq)

parse :: Text -> Either (ParseErrorBundle Text Void) [Password]
parse = runParser (some parser) ""

type Parser = Parsec Void Text

parser :: Parser Password
parser = do
  min' <- numberChar `manyTill` char '-' <?> "min"
  let min :: Int = read min'
  max' <- numberChar `manyTill` spaceChar <?> "max"
  let max :: Int = read max'
  letter <- letterChar <?> "letter"
  _ <- chunk ": "
  password <- toText <$> letterChar `manyTill` newline <?> "password"
  return Password {..}

isValid :: Password -> Bool
isValid Password{..} = count >= min && count <= max
  where
    countLetter :: Char -> String -> Int
    countLetter l s = foldr (\c n -> if c == l then n + 1 else n) 0 s

    count = countLetter letter $ toString password

run :: Text -> Int
run contents = sum $ map fromEnum valids
  where
    passwords = case parse contents of
      Right ps -> ps
      Left err -> error $ toText $ errorBundlePretty err
    valids = map isValid passwords
