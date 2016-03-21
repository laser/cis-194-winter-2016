module Homework.Week09.Assignment (
  zeroOrMore,
  oneOrMore,
  spaces,
  ident,
  parseSExpr,
  Ident(..),
  Atom(..),
  SExpr(..)
) where

import Control.Applicative
import Data.Char

import Homework.Week09.AParser

-- #1
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []


oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) `fmap` p <*> zeroOrMore p

-- #2
-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

-- #3
type Ident = String

data Atom = N Integer
          | I Ident
  deriving (Eq, Show)

data SExpr = A Atom
           | Comb [SExpr]
  deriving (Eq, Show)

parseAtom :: Parser Atom
parseAtom = spaces *> (N <$> posInt <|> I <$> ident) <* spaces

parseComb :: Parser SExpr
parseComb = spaces *> (Comb <$> (char '(' *> oneOrMore parseSExpr <* char ')')) <* spaces


parseSExpr :: Parser SExpr
parseSExpr = (A <$> parseAtom)
              <|> parseComb
