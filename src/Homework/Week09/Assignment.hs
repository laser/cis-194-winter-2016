module Homework.Week09.Assignment (
  Atom(..),
  SExpr(..),
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase,
  zeroOrMore,
  oneOrMore,
  spaces,
  ident,
  parseSExpr
) where

import Homework.Week09.AParser
import Control.Applicative

-- #1A
first :: (a -> b) -> (a,c) -> (b,c)
first = undefined

instance Functor Parser where
  fmap = undefined

-- #2A
instance Applicative Parser where
  pure = undefined
  _ <*> _ = undefined

-- #3A
abParser :: Parser (Char, Char)
abParser = undefined

abParser_ :: Parser ()
abParser_ = undefined

intPair :: Parser [Integer]
intPair = undefined

-- #4A
instance Alternative Parser where
  empty = undefined
  _ <|> _ = undefined

-- #5A
intOrUppercase :: Parser ()
intOrUppercase = undefined

-- #1B
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = undefined

oneOrMore :: Parser a -> Parser [a]
oneOrMore = undefined

-- #2B
spaces :: Parser String
spaces = undefined

ident :: Parser Ident
ident = undefined

-- #3B
type Ident = String

data Atom = N Integer | I Ident
  deriving (Eq, Show)

data SExpr = A Atom | Comb [SExpr]
  deriving (Eq, Show)

parseSExpr :: Parser SExpr
parseSExpr = undefined
