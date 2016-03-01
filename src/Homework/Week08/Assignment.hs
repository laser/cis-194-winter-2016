module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Data.Maybe
import Homework.Week08.AParser
import Control.Applicative

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)

instance Functor Parser where
  fmap fn (Parser run)  = Parser f
    where
      f = fmap (first fn) . run

-- #2
instance Applicative Parser  where
  pure a = Parser $ \s -> Just (a,s)
  (Parser fab )  <*> (Parser fa) = Parser $ (\ s -> apply $ fa s  )
       where apply (Just (a,rest)) = fmap (\ (fn,r) -> (fn a, r)) $ fab rest
             apply _  = Nothing

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
