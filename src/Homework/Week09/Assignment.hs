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
import Data.Char (isAlpha, isAlphaNum)

import Homework.Week09.AParser

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Hint: To parse one or more occurrences of p, run p once
-- and then parse zero or more occurrences of p.

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some
{-oneOrMore p = (:) <$> p <*> zeroOrMore p-}

-- To parse zero or more occurrences of p, try parsing one
-- or more; if that fails, return the empty list.

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many
{-zeroOrMore p = oneOrMore p <|> pure []-}

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- First, spaces should parse a consecutive list of zero or
-- more whitespace characters.

spaces :: Parser String
spaces = many $ (char ' ' <|> char '\n' <|> char '\t' <|> char '\r')

-- Next, ident should parse an identifier, which for our
-- purposes will be an alphabetic character (use isAlpha)
-- followed by zero or more alphanumeric characters (use
-- isAlphaNum).

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> many (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer
          | I Ident
  deriving (Show, Eq)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show, Eq)

-- Textually, S-expressions can optionally begin and end
-- with any number of spaces; after throwing away leading
-- and trailing spaces they consist of either an atom, or
-- an open parenthesis followed by one or more S-expressions
-- followed by a close parenthesis.
--
-- For example, the following are all valid S-expressions:
--  5
--  foo3
--  (bar (foo) 3 5 874)
--  (((lambda x (lambda y (plus x y))) 3) 5)
--  (   lots  of   (  spaces   in  )  this ( one ) )

parseAtom :: Parser Atom
parseAtom = (pure (N) <*> posInt) <|> (pure (I) <*> ident)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> parse <* spaces
  where parse = pure (A) <*> parseAtom <|> (pure (Comb) <*> (char '(' *> some parseSExpr <* char ')'))

sumParse :: Parser Integer
sumParse = spaces *> (hit <|> miss) <* spaces
  where
    hit  = posInt
    miss = foldl1 (+) <$> (char '(' *> some sumParse <* char ')')