module Advent2020.Internal.D18 (Expr (..), parse, parse', eval) where

import Advent2020.Internal (Parser, integralP, parseWithPrettyErrors, symbol)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Relude
import Text.Megaparsec (between, eof, sepEndBy1)
import Text.Megaparsec.Char (newline)

data Expr
  = Add Expr Expr
  | Multiply Expr Expr
  | Operand Integer
  deriving (Show, Eq)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

parse_ :: [[Operator Parser Expr]] -> Text -> Either Text [Expr]
parse_ operatorTable = parseWithPrettyErrors $ lineP `sepEndBy1` newline <* eof
  where
    lineP :: Parser Expr
    lineP = exprP

    exprP :: Parser Expr
    exprP = makeExprParser termP operatorTable

    termP :: Parser Expr
    termP = parens exprP <|> numberP

    numberP :: Parser Expr
    numberP = Operand <$> integralP

parse :: Text -> Either Text [Expr]
parse = parse_ [[binary "+" Add, binary "*" Multiply]]

parse' :: Text -> Either Text [Expr]
parse' = parse_ [[binary "+" Add], [binary "*" Multiply]]

eval :: Expr -> Integer
eval expr = case expr of
  Add e1 e2 -> eval e1 + eval e2
  Multiply e1 e2 -> eval e1 * eval e2
  Operand i -> i
