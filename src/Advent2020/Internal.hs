module Advent2020.Internal
  ( readInt,
    gather,
    gather',
    Parser,
    parseWithPrettyErrors,
  )
where

import Data.Either.Combinators (mapLeft)
import Relude
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)

readInt :: (ToString s) => s -> Either Text Int
readInt s = mapLeft (const $ toText $ "could not parse as integer: " ++ show s') $ readEither s'
  where
    s' = toString s

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

gather' :: [Either Text a] -> Either Text [a]
gather' = mapLeft gatherErrors . gather
  where
    gatherErrors :: [Text] -> Text
    gatherErrors es = mconcat $ intersperse ", " es

type Parser = Parsec Void Text

parseWithPrettyErrors :: Parser t -> Text -> Either Text t
parseWithPrettyErrors parser contents = mapLeft (toText . errorBundlePretty) $ runParser parser "" contents
