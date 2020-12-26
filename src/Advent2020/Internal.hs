module Advent2020.Internal
  ( traceWith,
    tracePrefix,
    readInt,
    readInt',
    map',
    mapE,
    mapE',
    label,
    gather,
    gather',
    Parser,
    parseWithPrettyErrors,
    parseWith,
    parseWith',
    parseNumbers,
    runNumbers,
    runNumbers',
    simpleRun,
    simpleRun',
    setAt,
    pairs,
    windows,
    smallest,
    largest,
    min',
    fixed,
    unsafeNonEmpty,
  )
where

import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.Either.Extra (mapLeft)
import Relude
import Relude.Unsafe (fromJust)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, hidden, runParser, someTill, (<?>))
import Text.Megaparsec.Char (digitChar, newline)

-- | 'trace' a message derived from the traced value.
{-# WARNING traceWith "'traceWith' remains in code" #-}
traceWith :: (a -> Text) -> a -> a
traceWith f x = trace (toString $ f x) x

-- | 'trace' a message derived from the traced value.
{-# WARNING tracePrefix "'tracePrefix' remains in code" #-}
tracePrefix :: (Show a) => Text -> a -> a
tracePrefix msg x = trace (toString $ msg <> ": " <> show x) x

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

-- | 'map' over a list while providing an index with each element.
map' :: (Int -> a -> b) -> [a] -> [b]
map' f xs = uncurry f <$> zip [0 ..] xs

-- | 'map' to 'Either's with error messages indicating the indexes of elements
-- that mapped to a 'Left'.
mapE :: (a -> Either Text b) -> [a] -> [Either Text b]
mapE f = map' f'
  where
    f' i x = mapLeft (\e -> "error while processing element at index " <> show i <> ": " <> e) $ f x

-- | 'mapE', with pretty error message concatenation.
mapE' :: (a -> Either Text b) -> [a] -> Either Text [b]
mapE' f xs = gather' $ mapE f xs

-- | Like Megaparsec's 'Text.Megaparsec.label', but for nice error messages with
-- @'Either' 'Text'@.
label :: Text -> Either Text t -> Either Text t
label name = mapLeft (\err -> "error while " <> name <> ": " <> err)

-- | Takes a list of successes or failures. On any failure, produces a list of
-- failures. Otherwise, produces a list of successes.
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

-- | 'gather' specialized to @'Either' 'Text'@, with pretty error message
-- concatenation.
gather' :: [Either Text a] -> Either Text [a]
gather' = mapLeft gatherErrors . gather
  where
    gatherErrors :: [Text] -> Text
    gatherErrors es = mconcat $ intersperse ", " es

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

-- | Runner for questions that take number list inputs.
runNumbers :: (NonEmpty Int -> Either Text Int) -> Text -> Either Text Int
runNumbers runner contents = do
  xs <- parseNumbers contents
  runner xs

-- | Like 'runNumbers', but for '[@Int@]' rather than '@NonEmpty@ @Int@'.
runNumbers' :: ([Int] -> Either Text Int) -> Text -> Either Text Int
runNumbers' r = runNumbers (r . toList)

-- | Runner that binds directly to runner.
simpleRun :: (Text -> Either Text input) -> (input -> Either Text output) -> Text -> Either Text output
simpleRun parser runner contents = parser contents >>= runner

-- | Like 'simpleRun', but specifies a function to transform the output of a runner.
simpleRun' :: (Text -> Either Text input) -> (runnerOutput -> Either Text output) -> (input -> runnerOutput) -> Text -> Either Text output
simpleRun' parser runTo runner contents = parser contents >>= runTo . runner

-- | Set a value at an index in a list.
setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_ : xs) = a : xs
    go n (x : xs) = x : go (n -1) xs
    go _ [] = []

-- | Pairs of a list.
pairs :: [a] -> [(a, a)]
pairs (x : xs) = ((x,) <$> xs) ++ pairs xs
pairs [] = []

-- | Fixed-size sliding windows of a list.
windows :: Int -> [a] -> [[a]]
windows n xs = filter (\l -> length l == n) $ take n <$> tails xs

-- | Returns the smallest element of the list.
smallest :: (Ord a) => NonEmpty a -> a
smallest xs = foldr min (head xs) xs

-- | Returns the largest element of the list.
largest :: (Ord a) => NonEmpty a -> a
largest xs = foldr max (head xs) xs

-- | 'min', but project the value to compare on.
min' :: (Ord b) => (a -> b) -> a -> a -> a
min' f a b = if f a < f b then a else b

-- | Find the fixed point of a function given an initial argument.
fixed :: (Eq a) => (a -> a) -> a -> a
fixed f a
  | a == a' = a
  | otherwise = fixed f a'
  where
    a' = f a

-- | Transform a list into its nonempty equivalent. Only use when safe by construction.
unsafeNonEmpty :: [a] -> NonEmpty a
unsafeNonEmpty = fromJust . nonEmpty
