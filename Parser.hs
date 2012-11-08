module Parser where

import Control.Applicative hiding (many)
import ItLang

import Text.Parsec
import qualified Text.Parsec.Token as P

lexer = P.makeTokenPaser javaDef

parens = P.parens lexer
braces = P.braces lexer
identifier = P.identifier lexer
symbol = P.symbol lexer
semi = P.semi lexer
semiSep = P.semiSep lexer

parseNat = toNat <$> natural

toNat 0 = Z
toNat n = S (toNat (n-1))

parseProg = braces (semiSep parseStmt)

parseStmt =
      Assign <$> identifier <*> (symbol '=' *> parseExp)
  <|> Block <$> parseProg
  <|> If <$> parseBExp <*>