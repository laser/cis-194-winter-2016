module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Control.Monad (join)


first :: (a -> b) -> (a , c) -> (b , c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f p = Parser f'
     where f' = fmap (first f) . runParser p

-- #2
instance Applicative Parser where
  pure = undefined
  _ <*> _ = undefined

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
