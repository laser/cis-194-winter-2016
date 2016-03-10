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
  pure = \a -> Parser (\b -> Just(a, b))
  (Parser fab) <*> (Parser fa) = Parser f
        where f a = case (fab a) of
                        Nothing -> Nothing
                        Just(a', rem) -> fmap (first a') (fa rem)


  --Parser (\x -> fmap (fb) (fa x))

-- #3

abParser :: Parser (Char, Char)
abParser = (fmap (\a b -> (a,b)) (char 'a')) <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (fmap (\a b -> ()) (char 'a')) <*> (char 'b')

intPair :: Parser [Integer]
intPair = undefined

-- #4
instance Alternative Parser where
  empty = undefined
  _ <|> _ = undefined

-- #5
intOrUppercase :: Parser ()
intOrUppercase = undefined
