module Advent2020.D4 (parse, Passport (..), part1) where

import Advent2020.Internal (Parser, parseWithPrettyErrors)
import Data.Map (lookup)
import Relude hiding (some)
import Text.Megaparsec (count, eof, some, someTill)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, newline, spaceChar)

data Passport = Passport
  { birthYear :: Maybe Text,
    issueYear :: Maybe Text,
    expirationYear :: Maybe Text,
    height :: Maybe Text,
    hairColor :: Maybe Text,
    eyeColor :: Maybe Text,
    passportID :: Maybe Text,
    countryID :: Maybe Text
  }
  deriving (Show, Eq)

parse :: Text -> Either Text [Passport]
parse = parseWithPrettyErrors parser

parser :: Parser [Passport]
parser = some passportParser
  where
    passportParser :: Parser Passport
    passportParser = do
      fields <- fieldParser `someTill` (void newline <|> eof)
      let fieldMap :: Map Text Text = fromList fields
      return
        Passport
          { birthYear = lookup "byr" fieldMap,
            issueYear = lookup "iyr" fieldMap,
            expirationYear = lookup "eyr" fieldMap,
            height = lookup "hgt" fieldMap,
            hairColor = lookup "hcl" fieldMap,
            eyeColor = lookup "ecl" fieldMap,
            passportID = lookup "pid" fieldMap,
            countryID = lookup "cid" fieldMap
          }

    fieldParser :: Parser (Text, Text)
    fieldParser = do
      key <- toText <$> count 3 letterChar
      void $ char ':'
      field <- toText <$> (char '#' <|> alphaNumChar) `someTill` spaceChar
      return (key, field)

part1 :: Text -> Either Text Int
part1 contents = do
  ps <- parse contents
  return $ length $ filter isValid ps
  where
    isValid :: Passport -> Bool
    isValid Passport {..} =
      isJust birthYear
        && isJust issueYear
        && isJust expirationYear
        && isJust height
        && isJust hairColor
        && isJust eyeColor
        && isJust passportID
