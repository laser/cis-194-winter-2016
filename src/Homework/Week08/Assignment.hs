module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Data.Char

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap f p = Parser $ \s -> first f <$> runParser p s

-- #2

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  p1 <*> p2 = Parser $ \s -> do
    (f, s1) <- runParser p1 s
    (x, s2) <- runParser p2 s1
    return (f x, s2)

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = Parser $ \s -> do
  (x1, s1) <- runParser posInt s
  (_, s2) <- runParser (char ' ') s1
  (x2, s3) <- runParser posInt s2
  return ([x1,x2], s3)

-- #4
instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

-- #5
intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper
