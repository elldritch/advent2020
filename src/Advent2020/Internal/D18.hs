module Advent2020.Internal.D18 (Expr (..), parse, eval) where

import Advent2020.Internal (Parser, parseWithPrettyErrors)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Relude
import Text.Megaparsec (between, eof, someTill)
import Text.Megaparsec.Char (hspace, newline)
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Add Expr Expr
  | Multiply Expr Expr
  | Operand Integer
  deriving (Show, Eq)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

parse :: Text -> Either Text [Expr]
parse = parseWithPrettyErrors $ lineP `someTill` eof
  where
    lineP :: Parser Expr
    lineP = do
      expr <- exprP
      _ <- newline
      return expr

    exprP :: Parser Expr
    exprP = makeExprParser termP operators

    termP :: Parser Expr
    termP = parens exprP <|> numberP

    numberP :: Parser Expr
    numberP = Operand <$> lexeme L.decimal

    operators :: [[Operator Parser Expr]]
    operators =
      [ [binary "+" Add, binary "*" Multiply]
      ]

eval :: Expr -> Integer
eval expr = case expr of
  Add e1 e2 -> eval e1 + eval e2
  Multiply e1 e2 -> eval e1 * eval e2
  Operand i -> i
