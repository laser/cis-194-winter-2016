{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import qualified Control.Arrow       as Arr (first)
import           Control.Applicative
import           Control.Monad              (void)
import           Data.Char                  (isAlpha, isAlphaNum, isSpace, isUpper)

-- #1A
first :: (a -> b) -> (a, c) -> (b, c)
first = Arr.first

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (first f) . g

-- #2A
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)

  (Parser a) <*> (Parser b) = Parser $ \str -> do
    (f, remainingA) <- a str
    (x, remainingB) <- b remainingA
    return (f x, remainingB)

-- #3A
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = collect <$> posInt <* char ' ' <*> posInt
  where collect a b = [a, b]

-- #4A
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \str -> a str <|> b str

-- #5A
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)

-- #1B
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- #2B
spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser Ident
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- #3B
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
