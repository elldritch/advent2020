{-# LANGUAGE RankNTypes #-}

module Advent2020.D4 (parse, Passport (..), part1, HexCode, Color (..), Length (..), run, part2) where

import Advent2020.Internal (Parser, parseWith, parseWith', parseWithPrettyErrors, readInt)
import Data.Map (lookup)
import Relude
import Text.Megaparsec (count, eof, hidden, someTill, someTill_)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, hexDigitChar, letterChar, newline, spaceChar)

data Passport = Passport
  { birthYear :: Maybe (Either Text Int),
    issueYear :: Maybe (Either Text Int),
    expirationYear :: Maybe (Either Text Int),
    height :: Maybe (Either Text Length),
    hairColor :: Maybe (Either Text HexCode),
    eyeColor :: Maybe (Either Text Color),
    passportID :: Maybe (Either Text Int),
    countryID :: Maybe Text
  }
  deriving (Show, Eq)

type HexCode = String

data Color
  = Amber
  | Blue
  | Brown
  | Grey
  | Green
  | Hazel
  | Other
  deriving (Show, Eq)

data Length
  = Inches Int
  | Centimeters Int
  deriving (Show, Eq)

parse :: Text -> Either Text [Passport]
parse = parseWithPrettyErrors parser

parser :: Parser [Passport]
parser = passportParser `someTill` hidden eof
  where
    passportParser :: Parser Passport
    passportParser = do
      fields <- fieldParser `someTill` (void newline <|> eof)
      let fieldMap :: Map Text Text = fromList fields
      return $ makePassport fieldMap

    fieldParser :: Parser (Text, Text)
    fieldParser = do
      key <- toText <$> count 3 letterChar
      void $ char ':'
      field <- toText <$> (char '#' <|> alphaNumChar) `someTill` spaceChar
      return (key, field)

    makePassport :: Map Text Text -> Passport
    makePassport m =
      Passport
        { birthYear = validateBirthYear <$> lookup "byr" m,
          issueYear = validateIssueYear <$> lookup "iyr" m,
          expirationYear = validateExpirationYear <$> lookup "eyr" m,
          height = validateHeight <$> lookup "hgt" m,
          hairColor = validateHairColor <$> lookup "hcl" m,
          eyeColor = validateEyeColor <$> lookup "ecl" m,
          passportID = validatePassportID <$> lookup "pid" m,
          countryID = lookup "cid" m
        }

    validateBirthYear :: Text -> Either Text Int
    validateBirthYear y = do
      year <- readInt y
      if year >= 1920 && year <= 2002
        then return year
        else fail $ "invalid birth year: " <> show year

    validateIssueYear :: Text -> Either Text Int
    validateIssueYear y = do
      year <- readInt y
      if year >= 2010 && year <= 2020
        then return year
        else fail $ "invalid issue year: " <> show year

    validateExpirationYear :: Text -> Either Text Int
    validateExpirationYear y = do
      year <- readInt y
      if year >= 2020 && year <= 2030
        then return year
        else fail $ "invalid expiration year: " <> show year

    validateHeight :: Text -> Either Text Length
    validateHeight height = parseWithPrettyErrors heightParser height
      where
        heightParser :: Parser Length
        heightParser = do
          (digits, firstLetter) <- digitChar `someTill_` letterChar
          h <- parseWith' readInt digits
          lastLetter <- letterChar
          _ <- eof
          let units = firstLetter : [lastLetter]
          result <- case units of
            "in" -> return $ Inches h
            "cm" -> return $ Centimeters h
            _ -> fail $ "invalid units in height: " <> show units
          case result of
            Centimeters cm ->
              if cm >= 150 && cm <= 193
                then return result
                else fail $ "invalid cm: " <> show cm
            Inches i ->
              if i >= 59 && i <= 76
                then return result
                else fail $ "invalid inches: " <> show i

    validateHairColor :: Text -> Either Text HexCode
    validateHairColor hairColor = parseWithPrettyErrors hairColorParser hairColor
      where
        hairColorParser = do
          _ <- char '#'
          s <- count 6 hexDigitChar
          _ <- eof
          return s

    validateEyeColor :: Text -> Either Text Color
    validateEyeColor s = case s of
      "amb" -> return Amber
      "blu" -> return Blue
      "brn" -> return Brown
      "gry" -> return Grey
      "grn" -> return Green
      "hzl" -> return Hazel
      "oth" -> return Other
      _ -> fail $ "invalid eye color: " <> show s

    validatePassportID :: Text -> Either Text Int
    validatePassportID passportID = parseWithPrettyErrors passportIDParser passportID
      where
        passportIDParser = do
          pid <- parseWith readInt $ count 9 digitChar
          _ <- eof
          return pid

run :: Text -> (Passport -> Bool) -> Either Text Int
run contents runner = do
  ps <- parse contents
  return $ length $ filter runner ps

checkPassport :: (forall a. Maybe (Either Text a) -> Bool) -> Passport -> Bool
checkPassport f Passport {..} =
  f birthYear
    && f issueYear
    && f expirationYear
    && f height
    && f hairColor
    && f eyeColor
    && f passportID

part1 :: Passport -> Bool
part1 = checkPassport isJust

part2 :: Passport -> Bool
part2 = checkPassport isOK
  where
    isOK (Just (Right _)) = True
    isOK _ = False
