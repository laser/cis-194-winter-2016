module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative

-- #1
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f p = Parser (fp f p)
    where fp f p = \ xs -> (first f) <$> (runParser p xs)

-- #2
instance Applicative Parser where
  pure a = Parser (\ xs -> Just (a, xs))
  ff <*> fa = Parser f
    where f xs = maybe Nothing (\ (h, ys) -> runParser (h <$> fa) ys) (runParser ff xs)

-- #3
abParser :: Parser (Char, Char)
abParser = f <$> (char 'a') <*> (char 'b')
  where f x y = (x, y)

abParser_ :: Parser ()
abParser_ = (\ _ -> ()) <$> abParser

intPair :: Parser [Integer]
intPair = undefined

-- #4
instance Alternative Parser where
  empty = undefined
  _ <|> _ = undefined

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
