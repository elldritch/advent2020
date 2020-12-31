module Advent2020.Internal.D19
  ( Rules,
    Rule (..),
    SubRule,
    Message,
    parse,
    parseRules,
    match,
    updateRules,
  )
where

import Advent2020.Internal (Parser, integralP, parseWithPrettyErrors, symbol, wordP)
import qualified Data.Text as Text
import Relude
import Relude.Extra.Map
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec (between, eof, sepBy1, someTill)
import Text.Megaparsec.Char (char, letterChar, newline)

type Rules = Map RuleID Rule

type RuleID = Int

data Rule
  = Literal Char
  | SubRules [SubRule]
  deriving (Show, Eq)

type SubRule = [RuleID]

type Message = Text

parse :: Text -> Either Text (Rules, [Message])
parse = parseWithPrettyErrors $ do
  rules <- fromList <$> ruleP `someTill` newline
  messages <- (wordP <* newline) `someTill` eof
  return (rules, messages)

parseRules :: Text -> Either Text Rules
parseRules = parseWithPrettyErrors $ fromList <$> ruleP `someTill` eof

ruleP :: Parser (RuleID, Rule)
ruleP = do
  ruleID <- integralP
  _ <- symbol ":"
  rule <- literalP <|> (SubRules <$> subrulesP)
  _ <- newline
  return (ruleID, rule)
  where
    literalP :: Parser Rule
    literalP = Literal <$> between (char '"') (char '"') letterChar

    subrulesP :: Parser [SubRule]
    subrulesP = some integralP `sepBy1` symbol "|"

match :: Rules -> Message -> Bool
match rules message = elem "" $ match' message 0
  where
    rule k = Unsafe.fromJust $ lookup k rules

    match' :: Message -> RuleID -> [Message]
    match' "" _ = empty
    match' m rid = case rule rid of
      Literal c -> if Text.head m == c then return $ Text.tail m else empty
      SubRules subrules -> subrules >>= foldlM match' m

updateRules :: Rules -> Rules
updateRules = updateRules' k1 v1 . updateRules' k2 v2
  where
    k1 = 11
    v1 = [[42, 31], [42, 11, 31]]

    k2 = 8
    v2 = [[42], [42, 8]]

    updateRules' :: Int -> [[Int]] -> Rules -> Rules
    updateRules' k v rs = alter (const $ Just $ SubRules v) k rs
