{- HLINT ignore "Use <$>" -}
module Parser
where

import Control.Monad.Combinators.Expr
import Data.Char qualified as C
import Lang
import Pre hiding (many, some)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pname :: Parser Text
pname = lexeme $ takeWhile1P (Just "name") C.isLetter

pint :: Parser Int
pint = lexeme L.decimal

pbool :: Parser Bool
pbool = True <$ symbol "true" <|> False <$ symbol "false"

pterm :: Parser Exp
pterm =
  asum
    [ InputInt <$ symbol "input"
    , symbol "(" *> pexp <* symbol ")"
    , pure Let
        <*> (symbol "let" *> pname <* symbol "=")
        <*> (pexp <* symbol "in")
        <*> pexp
    , pure Print
        <*> (symbol "print" *> pexp <* symbol "in")
        <*> pexp
    , Atom . Name <$> pname
    , Atom . LitInt <$> pint
    , Atom . LitBool <$> pbool
    ]

pexp :: Parser Exp
pexp =
  makeExprParser
    pterm
    [
      [ Prefix (UnaryOp USub <$ symbol "-")
      , Prefix (UnaryOp Not <$ symbol "not")
      ]
    ,
      [ InfixL (BinOp Add <$ symbol "+")
      , InfixL (BinOp Sub <$ symbol "-")
      ]
    ,
      [ InfixN (CmpOp Eq <$ symbol "==")
      , InfixN (CmpOp Neq <$ symbol "/=")
      , InfixN (CmpOp Neq <$ symbol "<>")
      , InfixN (CmpOp Lt <$ symbol "<")
      , InfixN (CmpOp Le <$ symbol "<=")
      , InfixN (flip (CmpOp Lt) <$ symbol ">")
      , InfixN (flip (CmpOp Le) <$ symbol ">=")
      ]
    ,
      [ InfixL (BinOp And <$ symbol "&&")
      , InfixL (BinOp Or <$ symbol "||")
      ]
    ]


parseL :: Parser L
parseL = Module <$> pexp
