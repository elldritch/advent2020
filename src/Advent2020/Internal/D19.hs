module Advent2020.Internal.D19
  ( Rules,
    Rule (..),
    SubRule,
    Message,
    parse,
    parseRules,
    match,
  )
where

import Advent2020.Internal (Parser, integralP, parseWithPrettyErrors, symbol, wordP)
import qualified Data.Text as Text
import Relude
import Relude.Extra.Map
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec (between, eof, someTill)
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
  messages <- messageP `someTill` eof
  return (rules, messages)
  where
    messageP :: Parser Message
    messageP = do
      message <- wordP
      _ <- newline
      return message

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
    subrulesP = do
      subRuleIDs <- some integralP
      subRules <- optional $ symbol "|" >> subrulesP
      return $ subRuleIDs : fromMaybe [] subRules

match :: Rules -> Message -> Bool
match rules message = [""] == match' message 0
  where
    rule k = Unsafe.fromJust $ lookup k rules

    match' :: Message -> RuleID -> [Message]
    match' "" _ = empty
    match' m rid = case rule rid of
      Literal c -> if Text.head m == c then return $ Text.tail m else empty
      SubRules subrules -> do
        subrule <- subrules
        foldlM match' m subrule
