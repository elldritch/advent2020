module Advent2020.Internal.Parse
  ( Parser,
    parseWithPrettyErrors,
    parseWith,
    parseNumbers,
    readInt,
    symbol,
    integralP,
    wordP,
  )
where

import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.Either.Extra (mapLeft)
import Relude
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser)
import Text.Megaparsec.Char (hspace, letterChar, newline)
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
parseWith f p = do
  v <- p
  case f v of
    Right y -> return y
    Left e -> fail $ toString e

-- | Parse newline-delimited numbers.
parseNumbers :: Text -> Either Text (NonEmpty Int)
parseNumbers = parseWithPrettyErrors $ integralP `NonEmpty.sepEndBy1` newline <* eof

-- | 'readEither' specialized to @'Int'@s, with clearer error message.
readInt :: (ToString s) => s -> Either Text Int
readInt s = mapLeft (const $ toText $ "could not parse as Int: " ++ show s') $ readEither s'
  where
    s' = toString s

-- | Runs a parse, consuming all horizontal whitespace afterwards.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

-- | Parse a literal string of text, consuming all horizontal whitespace afterwards.
symbol :: Text -> Parser Text
symbol = L.symbol hspace

-- | Parse a signed integer, consuming all horizontal whitespace afterwards.
integralP :: (Integral i) => Parser i
integralP = L.signed hspace $ lexeme L.decimal

-- | Parse a contiguous run of one or more letters, consuming all horizontal whitespace afterwards.
wordP :: Parser Text
wordP = toText <$> lexeme (some letterChar)
