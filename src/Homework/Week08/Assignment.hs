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
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap fab (Parser fa) = Parser (\x -> fmap (first fab) (fa x))

-- #2
instance Applicative Parser where
  pure = \x -> Parser (\y -> Just(x, y))
  p1 <*> p2 = Parser $ \x -> case (runParser p1 x) of
                               Just(f, rem) -> fmap (first f) (runParser p2 rem)
                               Nothing -> Nothing


  --Parser (\x -> fmap (fb) (fa x))

-- #3
abParser :: Parser (Char, Char)
abParser = undefined

abParser_ :: Parser ()
abParser_ = undefined

intPair :: Parser [Integer]
intPair = undefined

-- #4
instance Alternative Parser where
  empty = undefined
  _ <|> _ = undefined

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
