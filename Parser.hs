module Parser where

import           Control.Applicative hiding (many, (<|>))
import           ItLang

import           Text.Parsec
import           Text.Parsec.Language
import qualified Text.Parsec.Token as P

lexer = P.makeTokenParser javaStyle

parens = P.parens lexer
braces = P.braces lexer
identifier = P.identifier lexer
natural = P.natural lexer
reserved = P.reserved lexer
symbol = P.symbol lexer
semi = P.semi lexer
semiSep = P.semiSep lexer

parseNat = toNat <$> natural

toNat 0 = Z
toNat n = S (toNat (n-1))

parseProg = braces (semiSep parseStmt)

parseStmt =
      Assign <$> identifier <*> (symbol "=" *> parseExp)
  <|> Block <$> parseProg
  <|> If <$> parseBExp
         <*> (reserved "then" *> parseStmt)
         <*> (reserved "else" *> parseStmt <* reserved "endif")
  <|> Repeat <$> (reserved "repeat" *> parseExp)
             <*> (reserved "times" *> parseStmt <* reserved "done")

-- parseExp =
--       Lit <$> parseNat
--   <|> V   <$> identifier
--   <|> Plus

parseExp =
  Lit <$> parseNat
  <|> V <$> identifier
  <|> Plus <$> parseExp <*> (symbol "+" *> parseExp)
  <|> Minus <$> parseExp <*> (symbol "-" *> parseExp)
  <|> Times <$> parseExp <*> (symbol "*" *> parseExp)

parseBExp = undefined