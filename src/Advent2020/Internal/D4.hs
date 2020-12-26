module Advent2020.Internal.D4
  ( Passport (..),
    HexCode,
    Color (..),
    Length (..),
    parse,
  )
where

import Advent2020.Internal (Parser, parseWith, parseWith', parseWithPrettyErrors, readInt)
import Data.Either.Extra (mapLeft)
import Data.List.Extra (replace, trim)
import Relude
import Relude.Extra.Map
import Text.Megaparsec (ParseErrorBundle (..), count, eof, hidden, parseErrorTextPretty, runParser, someTill, someTill_)
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

validateNumber :: (MonadFail m) => (Int, Int) -> String -> Int -> m Int
validateNumber (low, high) name n
  | n < low = fail $ "invalid " <> name <> " (too low): " <> show n
  | n > high = fail $ "invalid " <> name <> " (too high): " <> show n
  | otherwise = return n

validateBirthYear :: Text -> Either Text Int
validateBirthYear birthYear = do
  year <- readInt birthYear
  validateNumber (1920, 2002) "birth year" year

validateIssueYear :: Text -> Either Text Int
validateIssueYear issueYear = do
  year <- readInt issueYear
  validateNumber (2010, 2020) "issue year" year

validateExpirationYear :: Text -> Either Text Int
validateExpirationYear expirationYear = do
  year <- readInt expirationYear
  validateNumber (2020, 2030) "expiration year" year

-- Print `fail` messages, but not surrounding formatting.
parseWithSimpleError :: Text -> Parser t -> Text -> Either Text t
parseWithSimpleError name p contents = mapLeft errorBundleSimple $ runParser p "" contents
  where
    errorBundleSimple :: ParseErrorBundle Text Void -> Text
    errorBundleSimple (ParseErrorBundle (x :| _) _) =
      "could not parse " <> name <> ": " <> toText (replace "\n" ", " $ trim $ parseErrorTextPretty x)

validateHeight :: Text -> Either Text Length
validateHeight = parseWithSimpleError "height" heightParser
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
        Centimeters cm -> do
          n <- validateNumber (150, 193) "cm" cm
          return $ Centimeters n
        Inches i -> do
          n <- validateNumber (59, 75) "inches" i
          return $ Inches n

validateHairColor :: Text -> Either Text HexCode
validateHairColor = parseWithSimpleError "hair color" hairColorParser
  where
    hairColorParser = do
      _ <- char '#'
      s <- count 6 hexDigitChar
      _ <- eof
      return s

validateEyeColor :: Text -> Either Text Color
validateEyeColor eyeColor = case eyeColor of
  "amb" -> return Amber
  "blu" -> return Blue
  "brn" -> return Brown
  "gry" -> return Grey
  "grn" -> return Green
  "hzl" -> return Hazel
  "oth" -> return Other
  _ -> fail $ "invalid eye color: " <> show eyeColor

validatePassportID :: Text -> Either Text Int
validatePassportID = parseWithSimpleError "passport ID" passportIDParser
  where
    passportIDParser = do
      pid <- parseWith readInt $ count 9 digitChar
      _ <- eof
      return pid
