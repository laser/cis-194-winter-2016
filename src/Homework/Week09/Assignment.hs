module Homework.Week09.Assignment (
  Atom(..),
  SExpr(..),
  zeroOrMore,
  oneOrMore,
  spaces,
  ident,
  parseSExpr
) where


import Homework.Week09.AParser

import Control.Applicative
import Data.Char           (isAlpha, isAlphaNum, isSpace)

-- #1
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- #2
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser Ident
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- #3
type Ident = String

data Atom = N Integer | I Ident
  deriving (Eq, Show)

data SExpr = A Atom | Comb [SExpr]
  deriving (Eq, Show)

parseAtom :: Parser Atom
parseAtom =  N <$> posInt
         <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *> inner <* spaces
  where inner =  A    <$> parseAtom
             <|> Comb <$> (char '(' *> zeroOrMore parseSExpr <* char ')')
