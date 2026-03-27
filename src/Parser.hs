{- HLINT ignore "Use <$>" -}
module Parser (Parser, parseL)
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
pname = lexeme $ takeWhile1P (Just "alpha") C.isLetter

pint :: Parser Int
pint = lexeme L.decimal

pterm :: Parser Exp
pterm =
  asum
    [ InputInt <$ symbol "input"
    , Atom . Name <$> pname
    , Atom . Lit <$> pint
    ]

pexp :: Parser Exp
pexp =
  makeExprParser
    pterm
    [
      [ Prefix (UnaryOp USub <$ symbol "-")
      ]
    ,
      [ InfixL (BinOp Add <$ symbol "+")
      , InfixL (BinOp Sub <$ symbol "-")
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
