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

import Homework.Week09.AParser
import Data.Char

-- #1
zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p = undefined
zeroOrMore p =  (:) <$> p  <*> (zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p  <*> zeroOrMore p

-- #2
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
parseAtom = I <$> ident <|> N <$> posInt

parseComb :: Parser [SExpr]
parseComb = char '(' *> oneOrMore parseSExpr <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> parseAtom) <|> (Comb <$> parseComb)) <* spaces