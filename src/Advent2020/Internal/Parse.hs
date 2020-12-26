module Advent2020.Internal.Parse
  ( Parser,
    parseWithPrettyErrors,
    parseWith,
    parseWith',
    parseNumbers,
    readInt,
    readInt',
    lexeme,
    symbol,
    integralP,
    wordP,
  )
where

import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.Either.Extra (mapLeft)
import Relude
import Text.Megaparsec (MonadParsec (eof, hidden), Parsec, errorBundlePretty, runParser, someTill, (<?>))
import Text.Megaparsec.Char (digitChar, hspace, letterChar, newline)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parsers on 'Text' without custom error components. See the documentation
-- for "Text.Megaparsec".
type Parser = Parsec Void Text

-- | Runs a parser, pretty-printing error messages to 'Text'.
parseWithPrettyErrors :: Parser t -> Text -> Either Text t
parseWithPrettyErrors parser contents = mapLeft (toText . errorBundlePretty) $ runParser parser "" contents

-- | Transform and validate a parsed value. If the validation fails, the parser
-- fails as well.
parseWith :: (s -> Either Text t) -> Parser s -> Parser t
parseWith f p = p >>= parseWith' f

-- | Transform and validate a parsed value. If the validation fails, the parser
-- fails as well.
parseWith' :: (s -> Either Text t) -> s -> Parser t
parseWith' f v = case f v of
  Right y -> return y
  Left e -> fail $ toString e

-- | Parse newline-delimited numbers.
parseNumbers :: Text -> Either Text (NonEmpty Int)
parseNumbers = parseWithPrettyErrors $ parseWith readInt numberLineParser `NonEmpty.someTill` hidden eof
  where
    numberLineParser = digitChar `someTill` newline <?> "number"

-- | 'readEither' specialized to '@Int@'s, with clearer error message.
readInt :: (ToString s) => s -> Either Text Int
readInt s = mapLeft (const $ toText $ "could not parse as Int: " ++ show s') $ readEither s'
  where
    s' = toString s

-- | 'readEither' specialized to '@Integer@'s, with clearer error message.
readInt' :: (ToString s) => s -> Either Text Integer
readInt' s = mapLeft (const $ toText $ "could not parse as Integer: " ++ show s') $ readEither s'
  where
    s' = toString s

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

integralP :: (Integral i) => Parser i
integralP = L.signed hspace $ lexeme L.decimal

wordP :: Parser Text
wordP = toText <$> lexeme (some letterChar)
