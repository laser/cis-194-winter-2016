module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Data.Char (isUpper)

-- #1
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f p = Parser $ \xs -> (first f) <$> (runParser p xs)

-- #2
instance Applicative Parser where
  pure x    = Parser $ \xs -> Just (x, xs)
  p1 <*> p2 = Parser $ \xs -> maybe Nothing (\(g, ys) -> runParser (g <$> p2) ys) (runParser p1 xs)

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> (char ' ') <*> posInt

-- #4
instance Alternative Parser where
  empty     = Parser $ const Nothing
  p1 <|> p2 = Parser $ \xs -> (runParser p1 xs) <|> (runParser p2 xs)

-- #5
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> (satisfy isUpper))
