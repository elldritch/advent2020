module Advent2020.Internal
  ( readInt,
    map',
    mapE,
    mapE',
    label,
    gather,
    gather',
    Parser,
    parseWithPrettyErrors,
    parseWith,
  )
where

import Data.Either.Combinators (mapLeft)
import Relude
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)

-- | readEither specialized to integers, with clearer error message.
readInt :: (ToString s) => s -> Either Text Int
readInt s = mapLeft (const $ toText $ "could not parse as integer: " ++ show s') $ readEither s'
  where
    s' = toString s

-- | map with index.
map' :: (Int -> a -> b) -> [a] -> [b]
map' f xs = uncurry f <$> zip [0 ..] xs

-- | map to Eithers with error messages indicating indexes of elements that
-- | mapped to a Left.
mapE :: (a -> Either Text b) -> [a] -> [Either Text b]
mapE f = map' f'
  where
    f' i x = mapLeft (\e -> "error while processing element at index " <> show i <> ": " <> e) $ f x

-- | mapE, handling error message concatenation.
mapE' :: (a -> Either Text b) -> [a] -> Either Text [b]
mapE' f xs = gather' $ mapE f xs

-- | Like Megaparsec's label, but for nice error messages with Either Text.
label :: Text -> Either Text t -> Either Text t
label l = mapLeft (\err -> "error while " <> l <> ": " <> err)

-- | Takes a list of successes or failures. On any failure, produces a list of
-- | failures. Otherwise, produces a list of successes.
gather :: [Either e a] -> Either [e] [a]
gather = foldr foldEither (Right [])
  where
    foldEither :: Either e a -> Either [e] [a] -> Either [e] [a]
    foldEither e acc = case acc of
      Right rs -> case e of
        Right r -> Right (r : rs)
        Left l -> Left [l]
      Left ls -> case e of
        Right _ -> Left ls
        Left l -> Left (l : ls)

-- | gather specialized to Either Text, with error message concatenation.
gather' :: [Either Text a] -> Either Text [a]
gather' = mapLeft gatherErrors . gather
  where
    gatherErrors :: [Text] -> Text
    gatherErrors es = mconcat $ intersperse ", " es

-- | Parsers on Text without custom error components.
type Parser = Parsec Void Text

-- | Runs a parser, pretty-printing error messages to Text.
parseWithPrettyErrors :: Parser t -> Text -> Either Text t
parseWithPrettyErrors parser contents = mapLeft (toText . errorBundlePretty) $ runParser parser "" contents

-- | Transform and validate a parsed value.
parseWith :: (s -> Either Text t) -> Parser s -> Parser t
parseWith f p = do
  x <- p
  case f x of
    Right y -> return y
    Left e -> fail $ toString e
