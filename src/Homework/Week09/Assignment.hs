module Homework.Week09.Assignment (
  first,
  abParser,
  abParser_,
  intOrUppercase
) where

import Homework.Week09.AParser

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first = undefined

-- #2
--instance Applicative Parser where
--  pure = ???
--  _ <*> _ = ???

-- #3
abParser :: Parser (Char, Char)
abParser = undefined

abParser_ :: Parser ()
abParser_ = undefined

-- #4
--instance Alternative Parser where
--  empty = ???
--  _ <|> _ = ???

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
