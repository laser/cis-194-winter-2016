module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Data.Functor
import Data.Char

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) =  (f a, c)

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s -> fmap (first f) (g s)

-- #2
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (Parser f) <*> (Parser g) = Parser $ \s ->
    case f s of
      Nothing -> Nothing
      Just (f', s') -> fmap (first f') (g s')

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = (\a b -> [a, b]) <$> posInt <*> (char ' ' *> posInt)

-- #4
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser f) <|> (Parser g) = Parser $ \s -> f s <|> g s

-- #5
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
