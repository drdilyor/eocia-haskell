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
    , Atom . Name <$> pname
    , Atom . LitInt <$> pint
    , Atom . LitBool <$> pbool
    , symbol "(" *> pexp <* symbol ")"
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
      [ InfixN (BinOp Eq <$ symbol "==")
      , InfixN (BinOp Neq <$ symbol "/=")
      , InfixN (BinOp Neq <$ symbol "<>")
      , InfixN (BinOp Lt <$ symbol "<")
      , InfixN (BinOp Le <$ symbol "<=")
      , InfixN (flip (BinOp Lt) <$ symbol ">")
      , InfixN (flip (BinOp Le) <$ symbol ">=")
      ]
    ,
      [ InfixL (BinOp And <$ symbol "&&")
      , InfixL (BinOp Or <$ symbol "||")
      ]
    ]

pstmt :: Parser Stmt
pstmt =
  asum
    [ pure Let
        <*> (symbol "let" *> pname <* symbol "=")
        <*> (pexp <* symbol "in")
        <*> pstmt
    , pure Print
        <*> (symbol "print" *> pexp <* symbol "in")
        <*> pstmt
    , Expr <$> pexp
    ]

parseL :: Parser L
parseL = Module <$> pstmt
